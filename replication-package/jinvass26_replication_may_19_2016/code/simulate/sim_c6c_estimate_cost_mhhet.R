################################################################################
## sim_c6c_estimate_cost_mhhet.R — Estimate model_cost_mhhet on simulated data
##
## Produces bootstrap CSV for Table C.3 and result_as_init.Rda for CTF C.4.
## Requires: sim_c6_estimate.R must have run first (sev/price params in data_list)
################################################################################

if (!exists("SIM_DATA_DIR")) { USE_SIMULATED_DATA <- TRUE; source("code/config.R") }
source("code/simulate/functions/sim_helpers.R")

cat("\n================================================================\n")
cat("sim_c6c_estimate_cost_mhhet.R: Estimating model_cost_mhhet\n")
cat("================================================================\n\n")

## ---- Configuration ---------------------------------------------------------

if (!exists("MAX_ITER")) MAX_ITER <- if (TESTING) 500 else 50000
INIT_ALPHA      <- EST_INIT_ALPHA
INIT_ALPHA_WARM <- 1e-4
TOL_OBJ     <- EST_TOL_OBJ
REFRESH     <- 100

STAN_MODEL_BASE <- "data/estimates/model_output"
NUM_THREADS     <- min(parallel::detectCores(), EST_NUM_THREADS_CAP)

if (!requireNamespace("cmdstanr", quietly = TRUE))
  stop("cmdstanr is required for estimation.")
Sys.setenv(STAN_THREADS = "TRUE")

## ---- Helpers ---------------------------------------------------------------

build_init <- function(model_name) {
  sim_csv <- file.path(MODEL_OUT_DIR, model_name,
                       "results", "bootstrap-result-id-0.csv")
  if (file.exists(sim_csv)) {
    cat("  Using simulated output as init from:", sim_csv, "\n")
    params <- parse_bootstrap_csv(sim_csv)$params
    params[["gq_smry"]] <- NULL
    return(list(init = list(params), init_alpha = INIT_ALPHA))
  }

  real_csv <- file.path("data/estimates/model_output", model_name,
                        "results", "bootstrap-result-id-0.csv")
  if (file.exists(real_csv)) {
    cat("  Using real-data MLE as init from:", real_csv, "\n")
    params <- parse_bootstrap_csv(real_csv)$params
    params[["gq_smry"]] <- NULL
    return(list(init = list(params), init_alpha = INIT_ALPHA_WARM))
  }

  cat("  No prior output found — using init = 0.1\n")
  list(init = 0.1, init_alpha = INIT_ALPHA)
}

## ---- Load data_list --------------------------------------------------------

dl_rds <- file.path(SIM_CACHE_DIR, "data_list_IL.rds")
if (!file.exists(dl_rds)) stop("data_list_IL.rds not found. Run sim_c6_estimate.R first.")
sim_data_list <- readRDS(dl_rds)

## ---- Build stan_data for model_cost_mhhet ----------------------------------
## Same as model_main fields plus cost-model-specific ones

stan_data_fields <- c(
  # Dimensions
  "I", "N_clm", "M", "N_sev_pareto_below_limit", "N_sev_pareto_at_limit",
  "N_sev_lnorm", "I_tm_R", "I_tm", "M_clm_record", "M_renw_active",
  "D_R_nb_scheme", "D_R_renw_scheme", "D_R_tm_scheme",
  "N_choice", "N_choice_regimes",
  "D_choice_R_tm", "D_choice_R_nb", "D_choice_R_renw",
  # Mappings
  "N_clm_to_I", "I_tm_R_ordered_to_N_clm", "R_tm_scheme_cutoffs",
  "n_choice_regime_cutoffs", "N_by_choice_regime", "Js",
  "N_choice_to_I",
  "choice_to_R_tm_ranges_n0", "choice_to_R_tm_ranges_n1", "choice_to_R_tm_ranges_R",
  "choice_to_R_nb_ranges_n0", "choice_to_R_nb_ranges_n1", "choice_to_R_nb_ranges_R",
  "choice_to_R_renw_ranges_n0", "choice_to_R_renw_ranges_n1", "choice_to_R_renw_ranges_R",
  # Covariates
  "X_clm", "X_mh_1", "X_mh_2", "X_mh_3",
  "X_rc_clm",  # mhhet-specific: risk class for claim obs
  "X_sev_pareto_below_limit", "X_sev_pareto_at_limit", "X_sev_lnorm",
  "X_rc_R_tm_ordered",
  "X_choice", "X_rc_choice", "zip_income",
  "X_renw_active",
  # Outcomes
  "clm_count_severe_N_clm", "clm_count_N_clm",
  "clm_sev_pareto_below_limit", "clm_sev_pareto_at_limit", "clm_sev_lnorm",
  "log_tm_score_R_ordered",
  "d_t_new", "d_t1_new",
  # Pricing
  "prices_nb_1", "prices_oo_nb_1", "prices_nb_tm_1", "prices_oo_nb_tm_1",
  "prices_nb_2", "prices_oo_nb_2", "prices_nb_tm_2", "prices_oo_nb_tm_2",
  "prices_renw_1", "prices_renw_oo_1", "prices_renw_2", "prices_renw_oo_2",
  "limits_1", "limits_2",
  "clm_surcharge_ftr", "tm_optin_disc_ftr_2",
  # Weights
  "sampling_enum_clm", "sampling_enum_tm_R", "sampling_enum_choice",
  "bootstrap_weights_clm", "bootstrap_weights_tm_R", "bootstrap_weights_choice",
  # Pricing model params (fixed data)
  "theta_0_R_nb_mean", "theta_1_R_nb_mean", "theta_0_R_nb_sd",
  "theta_0_R_renw_mean", "theta_1_R_renw_mean", "theta_0_R_renw_sd",
  "theta_0_R_tm_mean", "theta_1_R_tm_mean", "theta_0_R_tm_sd",
  # Severity params
  "pareto_alpha_choice_base",
  # Settings
  "dollar_norm", "grainsize", "X_mh_mean",
  "d_tm_mh_rational_ind", "d_by_block",
  "discount_factor",
  "score_sd_lb", "score_sd_ub",
  # Renewal features
  "renw_cnt_choice", "renw_cnt_choice0", "renw_cnt_choice1", "renw_cnt_choice59"
)

stan_data <- list()
missing_fields <- c()
for (f in stan_data_fields) {
  if (f %in% names(sim_data_list) && !is.null(sim_data_list[[f]])) {
    stan_data[[f]] <- sim_data_list[[f]]
  } else {
    missing_fields <- c(missing_fields, f)
  }
}

# X_rc_clm: risk class mapped to claim observations (may not exist in data_list)
if (is.null(stan_data$X_rc_clm) && "X_rc" %in% names(sim_data_list)) {
  stan_data$X_rc_clm <- sim_data_list$X_rc[sim_data_list$N_clm_to_I]
  missing_fields <- setdiff(missing_fields, "X_rc_clm")
}

# M_clm_record: if not in data_list, set to M
if (is.null(stan_data$M_clm_record)) {
  stan_data$M_clm_record <- sim_data_list$M
  missing_fields <- setdiff(missing_fields, "M_clm_record")
}

# zip_income: if not in data_list, use zeros
if (is.null(stan_data$zip_income)) {
  stan_data$zip_income <- rep(0, sim_data_list$N_choice)
  missing_fields <- setdiff(missing_fields, "zip_income")
}

# Cost model: skip demand likelihood, only estimate cost/claims
stan_data$run_demand <- 0L
stan_data$run_demand_blocks <- rep(0L, sim_data_list$N_choice_regimes)

# Regularization and prior params from model_main estimates
main_csv <- file.path(MODEL_OUT_DIR, "model_main", "results", "bootstrap-result-id-0.csv")
if (file.exists(main_csv)) {
  main_params <- parse_bootstrap_csv(main_csv)$params
  stan_data$eps_sd <- main_params$eps_sd
  stan_data$eps_ra_sd <- if (!is.null(main_params$eps_ra_sd)) main_params$eps_ra_sd else 0.1
  stan_data$eps_eta_sd <- if (!is.null(main_params$eps_eta_sd)) main_params$eps_eta_sd else 0.1
} else {
  stan_data$eps_sd <- 1.0
  stan_data$eps_ra_sd <- 0.1
  stan_data$eps_eta_sd <- 0.1
}

# reg_factor_score: regularization for TM score coefficients
stan_data$reg_factor_score <- rep(0.01, sim_data_list$D_R_tm_scheme)

if (length(missing_fields) > 0) {
  cat("  WARNING: Missing fields:", paste(missing_fields, collapse = ", "), "\n")
}

cat("  Stan data:", length(stan_data), "fields\n\n")

## ---- Compile and estimate --------------------------------------------------

mn <- "model_cost_mhhet"

# Warm-start from cached output when available
existing_csv <- file.path(MODEL_OUT_DIR, mn, "results", "bootstrap-result-id-0.csv")
cache_warm_start <- USE_CACHE && file.exists(existing_csv)
if (cache_warm_start) cat("  Warm-start from cached output (reduced iterations)\n")
iter_use <- if (cache_warm_start) 100 else MAX_ITER

stan_path <- file.path(STAN_MODEL_BASE, mn, paste0(mn, ".stan"))
if (!file.exists(stan_path)) stop(stan_path, " not found")

cat("Compiling", mn, "...\n")
model <- cmdstanr::cmdstan_model(
  stan_path,
  stanc_options = list("O1"),
  force_recompile = FALSE,
  cpp_options = list(stan_threads = TRUE)
)

cat("Running L-BFGS optimization (", iter_use, " iter, ",
    NUM_THREADS, " threads)...\n", sep = "")
cat("Start:", format(Sys.time()), "\n\n")
init <- build_init(mn)

fit <- tryCatch({
  model$optimize(
    data = stan_data,
    init = init$init,
    algorithm = "lbfgs",
    iter = iter_use,
    threads = NUM_THREADS,
    init_alpha = if (cache_warm_start) 1e-8 else init$init_alpha,
    tol_obj = TOL_OBJ,
    tol_rel_grad = if (cache_warm_start) 1e9 else 1e7,
    refresh = REFRESH
  )
}, error = function(e) {
  cat("\nOptimization error:", conditionMessage(e), "\n")
  NULL
})

cat("\nEnd:", format(Sys.time()), "\n\n")

## ---- Save outputs ----------------------------------------------------------

if (!is.null(fit)) {
  # Save bootstrap CSV
  out_dir <- file.path(MODEL_OUT_DIR, mn, "results")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  csv_files <- fit$output_files()
  if (length(csv_files) > 0 && file.exists(csv_files[1])) {
    file.copy(csv_files[1], file.path(out_dir, "bootstrap-result-id-0.csv"), overwrite = TRUE)
    cat("  -> Saved", file.path(out_dir, "bootstrap-result-id-0.csv"), "\n")
  }

  # Save log file
  model_dir <- file.path(MODEL_OUT_DIR, mn)
  ts <- format(Sys.time(), "%Y%m%d%H%M")
  log_suffix <- paste0(ts, "-1-sim")
  log_file_obj <- list(
    output_filename = paste0(mn, "-", log_suffix, ".csv"),
    model_filename = paste0(mn, "-", log_suffix, ".stan"),
    bootstrap_id = 0L
  )
  save(log_file_obj, file = file.path(model_dir, paste0("log_", mn, "-", log_suffix, ".Rda")))
  file.copy(stan_path, file.path(model_dir, paste0(mn, "-", log_suffix, ".stan")), overwrite = TRUE)

  # Save result_as_init.Rda for CTF mhhet (load_estimation_bootstrap_data.R reads this)
  mle_params <- parse_bootstrap_csv(file.path(out_dir, "bootstrap-result-id-0.csv"))$params
  result_as_init <- list(
    theta_mh_llambda = mle_params$theta_mh_llambda,
    theta_mh_llambda_rc = mle_params$theta_mh_llambda_rc
  )
  saveRDS(result_as_init, file.path(model_dir, paste0("result_as_init-", log_suffix, ".Rda")))
  cat("  -> Saved result_as_init for CTF mhhet\n")

  mle <- tryCatch(cbind.data.frame(fit$lp(), t(fit$mle())), error = function(e) NULL)
  if (!is.null(mle)) {
    cat("  Log-likelihood at MLE:", round(as.numeric(mle[["lp__"]]), 2), "\n")
  }
} else {
  cat("  model_cost_mhhet estimation FAILED\n")
}

## ============================================================================
## 2. Estimate model_main_mhhet (joint demand model with mhhet cost params)
## ============================================================================

cat("\n================================================================\n")
cat("Estimating model_main_mhhet\n")
cat("================================================================\n\n")

mn2 <- "model_main_mhhet"
stan_path2 <- file.path(STAN_MODEL_BASE, mn2, paste0(mn2, ".stan"))

existing_csv2 <- file.path(MODEL_OUT_DIR, mn2, "results", "bootstrap-result-id-0.csv")
cache_warm_start <- USE_CACHE && file.exists(existing_csv2)
if (cache_warm_start) cat("  Warm-start from cached output (reduced iterations)\n")
iter_use <- if (cache_warm_start) 100 else MAX_ITER

if (!file.exists(stan_path2)) {
  cat("  SKIP:", stan_path2, "not found\n")
} else if (is.null(fit)) {
  cat("  SKIP: model_cost_mhhet failed, cannot proceed\n")
} else {
  # Build stan_data for model_main_mhhet: same as model_main + X_rc_clm + theta_mh_llambda_rc
  main_stan_data_fields <- c(
    "I", "N_clm", "M", "N_sev_pareto_below_limit", "N_sev_pareto_at_limit",
    "N_sev_lnorm", "I_tm_R", "I_tm", "M_renw_active",
    "D_R_nb_scheme", "D_R_renw_scheme", "D_R_tm_scheme",
    "N_clm_to_I", "I_tm_R_ordered_to_N_clm", "R_tm_scheme_cutoffs",
    "N_choice", "N_choice_regimes", "n_choice_regime_cutoffs",
    "N_by_choice_regime", "Js",
    "N_choice_to_I", "N_choice_to_I_choice", "I_tm_to_N",
    "D_choice_R_tm", "D_choice_R_nb", "D_choice_R_renw",
    "choice_to_R_tm_ranges_n0", "choice_to_R_tm_ranges_n1", "choice_to_R_tm_ranges_R",
    "choice_to_R_nb_ranges_n0", "choice_to_R_nb_ranges_n1", "choice_to_R_nb_ranges_R",
    "choice_to_R_renw_ranges_n0", "choice_to_R_renw_ranges_n1", "choice_to_R_renw_ranges_R",
    "X_clm", "X_mh_1", "X_mh_2", "X_mh_3",
    "X_choice", "X_rc_choice", "income_choice",
    "X_renw_active", "X_mh_mean",
    "clm_count_severe_N_clm", "clm_count_N_clm",
    "log_tm_score_R_ordered",
    "d_t_new",
    "prices_nb_1", "prices_oo_nb_1", "prices_nb_tm_1", "prices_oo_nb_tm_1",
    "prices_nb_2", "prices_oo_nb_2", "prices_nb_tm_2", "prices_oo_nb_tm_2",
    "prices_renw_1", "prices_renw_oo_1", "prices_renw_2", "prices_renw_oo_2",
    "limits_1", "limits_2",
    "d_t1_new", "clm_surcharge_ftr", "tm_optin_disc_ftr_2",
    "sampling_enum_clm", "sampling_enum_tm_R",
    "estimation_weights_choice",
    "bootstrap_weights_clm", "bootstrap_weights_tm_R", "bootstrap_weights_choice",
    "theta_0_R_nb_mean", "theta_1_R_nb_mean", "theta_0_R_nb_sd",
    "theta_0_R_renw_mean", "theta_1_R_renw_mean", "theta_0_R_renw_sd",
    "theta_0_R_tm_mean", "theta_1_R_tm_mean", "theta_0_R_tm_sd",
    "pareto_alpha_choice_base", "theta_mh_llambda",
    "sev_minor_mean_choice", "sev_minor_sd",
    "dollar_norm", "grainsize", "eps_prior",
    "score_sd_lb", "score_sd_ub", "risk_aversion_ub", "sigma_logit_lb",
    "discount_factor",
    "risk_target_scale_factor", "risk_moment_scale_factor",
    "score_moment_scale_factor", "choice_moment_scale_factor",
    "select_moment_scale_factor",
    "run_estimation", "run_demand_blocks", "d_by_block", "d_tm_mh_rational_ind",
    "prices_kc_nb_1", "prices_kc_nb_tm_1", "prices_kc_nb_2",
    "prices_kc_nb_tm_2", "prices_kc_renw_1", "prices_kc_renw_2",
    "clm_count_I_tm_R_ordered", "clm_count_N_choice"
  )

  stan_data2 <- list()
  for (f in main_stan_data_fields) {
    if (f %in% names(sim_data_list) && !is.null(sim_data_list[[f]]))
      stan_data2[[f]] <- sim_data_list[[f]]
  }
  stan_data2$run_estimation <- 1L
  stan_data2$run_demand_blocks <- stan_data2$run_demand_blocks %||% 1L

  # mhhet-specific: X_rc_clm and theta_mh_llambda_rc from cost model
  if ("X_rc" %in% names(sim_data_list)) {
    stan_data2$X_rc_clm <- sim_data_list$X_rc[sim_data_list$N_clm_to_I]
  }
  stan_data2$theta_mh_llambda_rc <- mle_params$theta_mh_llambda_rc
  # Override theta_mh_llambda with mhhet cost estimates
  stan_data2$theta_mh_llambda <- mle_params$theta_mh_llambda

  cat("  Stan data:", length(stan_data2), "fields\n")
  cat("  theta_mh_llambda_rc:", round(as.numeric(stan_data2$theta_mh_llambda_rc), 4), "\n\n")

  cat("Compiling", mn2, "...\n")
  model2 <- cmdstanr::cmdstan_model(
    stan_path2,
    stanc_options = list("O1"),
    force_recompile = FALSE,
    cpp_options = list(stan_threads = TRUE)
  )

  cat("Running L-BFGS optimization (", iter_use, " iter, ",
      NUM_THREADS, " threads)...\n", sep = "")
  cat("Start:", format(Sys.time()), "\n\n")
  init2 <- build_init(mn2)

  fit2 <- tryCatch({
    model2$optimize(
      data = stan_data2,
      init = init2$init,
      algorithm = "lbfgs",
      iter = iter_use,
      threads = NUM_THREADS,
      init_alpha = if (cache_warm_start) 1e-8 else init2$init_alpha,
      tol_obj = TOL_OBJ,
      tol_rel_grad = if (cache_warm_start) 1e9 else 1e7,
      refresh = REFRESH
    )
  }, error = function(e) {
    cat("\nOptimization error:", conditionMessage(e), "\n")
    NULL
  })

  cat("\nEnd:", format(Sys.time()), "\n\n")

  if (!is.null(fit2)) {
    # Save bootstrap CSV
    out_dir2 <- file.path(MODEL_OUT_DIR, mn2, "results")
    dir.create(out_dir2, recursive = TRUE, showWarnings = FALSE)
    csv_files2 <- fit2$output_files()
    if (length(csv_files2) > 0 && file.exists(csv_files2[1])) {
      file.copy(csv_files2[1], file.path(out_dir2, "bootstrap-result-id-0.csv"), overwrite = TRUE)
      cat("  -> Saved", file.path(out_dir2, "bootstrap-result-id-0.csv"), "\n")
    }

    # Save log file and est_config
    model_dir2 <- file.path(MODEL_OUT_DIR, mn2)
    ts2 <- format(Sys.time(), "%Y%m%d%H%M")
    log_suffix2 <- paste0(ts2, "-1-sim")
    log_file_obj <- list(
      output_filename = paste0(mn2, "-", log_suffix2, ".csv"),
      model_filename = paste0(mn2, "-", log_suffix2, ".stan"),
      bootstrap_id = 0L
    )
    save(log_file_obj, file = file.path(model_dir2, paste0("log_", mn2, "-", log_suffix2, ".Rda")))
    file.copy(stan_path2, file.path(model_dir2, paste0(mn2, "-", log_suffix2, ".stan")), overwrite = TRUE)

    saveRDS(list(
      d_tm_mh_rational_ind = 1L,
      d_by_block = sim_data_list$d_by_block %||% 6L,
      sigma_logit_lb = sim_data_list$sigma_logit_lb %||% 0.025
    ), file.path(model_dir2, paste0("est_config_", mn2, "-", log_suffix2, ".rds")))

    mle2 <- tryCatch(cbind.data.frame(fit2$lp(), t(fit2$mle())), error = function(e) NULL)
    if (!is.null(mle2)) {
      cat("  Log-likelihood at MLE:", round(as.numeric(mle2[["lp__"]]), 2), "\n")
    }
  } else {
    cat("  model_main_mhhet estimation FAILED\n")
  }
}

cat("\n================================================================\n")
cat("sim_c6c_estimate_cost_mhhet.R complete\n")
cat("================================================================\n")
