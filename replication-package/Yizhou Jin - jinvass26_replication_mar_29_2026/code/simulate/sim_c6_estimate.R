################################################################################
## sim_estimate.R — Estimate model_sev, model_price, model_main on simulated data
##
## Estimates three structural models via CmdStan L-BFGS optimization:
##   1. model_sev   — severity (Pareto + lognormal)
##   2. model_price — pricing factors
##   3. model_main  — joint demand/cost/MH (uses sev/price params as fixed data)
##
## Inputs:
##   data/simulated/data_list_IL.rds (or .json)
##   data/estimates/model_output/model_*/model_*.stan
##
## Outputs (under MODEL_OUT_DIR = output/simulated/estimates/model_output/):
##   model_sev/results/bootstrap-result-id-0.csv
##   model_price/results/bootstrap-result-id-0.csv
##   model_main/results/bootstrap-result-id-0.csv
##
## Requires: cmdstanr + CmdStan
################################################################################

if (!exists("SIM_DATA_DIR")) { USE_SIMULATED_DATA <- TRUE; source("code/config.R") }
source("code/simulate/functions/sim_helpers.R")

cat("================================================================\n")
cat("sim_estimate.R: Estimating models on simulated data\n")
cat("================================================================\n\n")

## ---- Configuration ----------------------------------------------------------

if (!exists("MAX_ITER")) MAX_ITER <- if (TESTING) 500 else 50000
INIT_ALPHA      <- EST_INIT_ALPHA
INIT_ALPHA_WARM <- 1e-4  # larger step size for warm-start from real-data init
TOL_OBJ     <- EST_TOL_OBJ
REFRESH     <- 100

STAN_MODEL_BASE <- "data/estimates/model_output"  # where .stan files live
NUM_THREADS     <- min(parallel::detectCores(), EST_NUM_THREADS_CAP)

## ---- Check CmdStan ----------------------------------------------------------

if (!requireNamespace("cmdstanr", quietly = TRUE))
  stop("cmdstanr is required for estimation.")

tryCatch({
  cmdstanr::cmdstan_path()
  cat("CmdStan:", cmdstanr::cmdstan_path(), "\n")
  cat("Version:", cmdstanr::cmdstan_version(), "\n\n")
}, error = function(e) {
  stop("CmdStan not found. Install with cmdstanr::install_cmdstan()")
})

Sys.setenv(STAN_THREADS = "TRUE")

## ---- Helper: build init from prior output -----------------------------------
## Cascade: (1) simulated output, (2) real-data output (hyperparams only), (3) 0.1

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
    # Keep leps (nu values) — simulated data uses these exact values
    return(list(init = list(params), init_alpha = INIT_ALPHA_WARM))
  }

  cat("  No prior output found — using init = 0.1\n")
  list(init = 0.1, init_alpha = INIT_ALPHA)
}

## ---- Load simulated data_list -----------------------------------------------

dl_rds <- file.path(SIM_CACHE_DIR, "data_list_IL.rds")
dl_json <- file.path(SIM_CACHE_DIR, "data_list_IL.json")

if (file.exists(dl_rds)) {
  cat("Loading", dl_rds, "\n")
  sim_data_list <- readRDS(dl_rds)
} else if (file.exists(dl_json)) {
  cat("Loading", dl_json, "\n")
  sim_data_list <- read_data_list(dl_json)
} else {
  stop("No data_list found. Run sim_generate_data_list.R first.")
}

cat("  N_clm:", sim_data_list$N_clm,
    "  N_choice:", sim_data_list$N_choice,
    "  I:", sim_data_list$I, "\n\n")

## ---- Helper: save CmdStan output to replication structure -------------------

save_model_output <- function(fit, model_name, stan_path) {
  out_dir <- file.path(MODEL_OUT_DIR, model_name, "results")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  csv_files <- fit$output_files()
  if (length(csv_files) > 0 && file.exists(csv_files[1])) {
    file.copy(csv_files[1],
              file.path(out_dir, "bootstrap-result-id-0.csv"),
              overwrite = TRUE)
    cat("  -> Saved", file.path(out_dir, "bootstrap-result-id-0.csv"), "\n")
  }

  # Save metadata log file (needed by extraction pipeline)
  ts <- format(Sys.time(), "%Y%m%d%H%M")
  log_suffix <- paste0(ts, "-1-sim")
  model_dir <- file.path(MODEL_OUT_DIR, model_name)

  log_file_obj <- list(
    output_filename = paste0(model_name, "-", log_suffix, ".csv"),
    model_filename = paste0(model_name, "-", log_suffix, ".stan"),
    bootstrap_id = 0L
  )
  save(log_file_obj, file = file.path(model_dir, paste0("log_", model_name, "-", log_suffix, ".Rda")))

  # Save est_config (needed by model_main downstream)
  if (model_name == "model_main") {
    saveRDS(list(
      d_tm_mh_rational_ind = 1L,
      d_by_block = sim_data_list$d_by_block %||% 6L,
      sigma_logit_lb = sim_data_list$sigma_logit_lb %||% 0.025
    ), file.path(model_dir, paste0("est_config_", model_name, "-", log_suffix, ".rds")))
  }

  # Copy Stan model file
  file.copy(stan_path, file.path(model_dir,
            paste0(model_name, "-", log_suffix, ".stan")),
            overwrite = TRUE)
}

## ---- Helper: extract MLE params from fit ------------------------------------

extract_mle_params <- function(fit) {
  tryCatch({
    cbind.data.frame(fit$lp(), t(fit$mle()))
  }, error = function(e) {
    cat("  WARNING: Could not extract MLE:", conditionMessage(e), "\n")
    NULL
  })
}

## ============================================================================
## 1. Estimate model_sev
## ============================================================================

cat("================================================================\n")
cat("1. Estimating model_sev\n")
cat("================================================================\n\n")

sev_stan_path <- file.path(STAN_MODEL_BASE, "model_sev", "model_sev.stan")
if (!file.exists(sev_stan_path)) stop("model_sev.stan not found at ", sev_stan_path)

cat("Compiling model_sev...\n")
sev_model <- cmdstanr::cmdstan_model(
  sev_stan_path,
  stanc_options = list("O1"),
  force_recompile = FALSE,
  cpp_options = list(stan_threads = TRUE)
)

# Build severity-specific data
sev_data <- list()
sev_fields <- c("M",
                 "N_sev_pareto_below_limit", "N_sev_pareto_at_limit", "N_sev_lnorm",
                 "X_sev_pareto_below_limit", "X_sev_pareto_at_limit", "X_sev_lnorm",
                 "clm_sev_pareto_below_limit", "clm_sev_pareto_at_limit", "clm_sev_lnorm",
                 "dollar_norm")
for (f in sev_fields) {
  if (f %in% names(sim_data_list)) sev_data[[f]] <- sim_data_list[[f]]
}
# Bootstrap weights: all 1s for point estimation
sev_data$bootstrap_weights_sev_pareto_below_limit <- rep(1, sim_data_list$N_sev_pareto_below_limit)
sev_data$bootstrap_weights_sev_pareto_at_limit <- rep(1, sim_data_list$N_sev_pareto_at_limit)
sev_data$bootstrap_weights_sev_lnorm <- rep(1, sim_data_list$N_sev_lnorm)
# Regularization
sev_data$reg_factor_sev_major <- 0.01
sev_data$reg_factor_sev_minor <- 0.01

cat("Running L-BFGS optimization for model_sev...\n")
cat("Start:", format(Sys.time()), "\n")
sev_init <- build_init("model_sev")

sev_fit <- tryCatch({
  sev_model$optimize(
    data = sev_data,
    init = sev_init$init,
    algorithm = "lbfgs",
    iter = MAX_ITER,
    threads = NUM_THREADS,
    init_alpha = sev_init$init_alpha,
    tol_obj = TOL_OBJ,
    refresh = REFRESH
  )
}, error = function(e) {
  cat("  ERROR:", conditionMessage(e), "\n")
  NULL
})

cat("End:", format(Sys.time()), "\n")

sev_params <- NULL
if (!is.null(sev_fit)) {
  save_model_output(sev_fit, "model_sev", sev_stan_path)
  sev_mle <- extract_mle_params(sev_fit)
  if (!is.null(sev_mle)) {
    cat("\n  model_sev recovery:\n")
    if ("theta_0_pareto_alpha_severe" %in% colnames(sev_mle))
      cat("    theta_0_pareto: ", round(sev_mle$theta_0_pareto_alpha_severe, 4), "\n")
    if ("theta_0_sev_minor_mean" %in% colnames(sev_mle))
      cat("    theta_0_minor:  ", round(sev_mle$theta_0_sev_minor_mean, 4), "\n")
    if ("sev_minor_sd" %in% colnames(sev_mle))
      cat("    sev_minor_sd:   ", round(sev_mle$sev_minor_sd, 4), "\n")
    sev_params <- sev_mle
  }
} else {
  cat("  model_sev estimation FAILED\n")
}

## ============================================================================
## 2. Estimate model_price
## ============================================================================

cat("\n================================================================\n")
cat("2. Estimating model_price\n")
cat("================================================================\n\n")

price_stan_path <- file.path(STAN_MODEL_BASE, "model_price", "model_price.stan")
if (!file.exists(price_stan_path)) stop("model_price.stan not found at ", price_stan_path)

cat("Compiling model_price...\n")
price_model <- cmdstanr::cmdstan_model(
  price_stan_path,
  stanc_options = list("O1"),
  force_recompile = FALSE,
  cpp_options = list(stan_threads = TRUE)
)

# Build pricing-specific data
price_data <- list()
price_fields <- c("I", "N_clm", "M",
                   "N_R", "N_R_nb", "N_R_ordered_to_N_clm",
                   "R_nb_scheme_cutoffs", "R_renw_scheme_cutoffs",
                   "X_rc_R_ordered", "X_rc_R_tm_ordered",
                   "p_R_ftr_wo_clm_R_ordered", "p_R_ftr_wo_clm_w_tm_R_ordered",
                   "I_tm_R", "I_tm", "D_R_nb_scheme", "D_R_renw_scheme", "D_R_tm_scheme",
                   "I_tm_R_ordered_to_N_clm",
                   "R_tm_scheme_cutoffs",
                   "log_tm_score_R_ordered",
                   "dollar_norm")
for (f in price_fields) {
  if (f %in% names(sim_data_list)) price_data[[f]] <- sim_data_list[[f]]
}
# Bootstrap weights
if (!is.null(sim_data_list$N_R))
  price_data$bootstrap_weights_R <- rep(1, sim_data_list$N_R)
if (!is.null(sim_data_list$I_tm_R))
  price_data$bootstrap_weights_tm_R <- rep(1, sim_data_list$I_tm_R)

cat("Running L-BFGS optimization for model_price...\n")
cat("Start:", format(Sys.time()), "\n")
price_init <- build_init("model_price")

price_fit <- tryCatch({
  price_model$optimize(
    data = price_data,
    init = price_init$init,
    algorithm = "lbfgs",
    iter = MAX_ITER,
    threads = NUM_THREADS,
    init_alpha = price_init$init_alpha,
    tol_obj = TOL_OBJ,
    refresh = REFRESH
  )
}, error = function(e) {
  cat("  ERROR:", conditionMessage(e), "\n")
  NULL
})

cat("End:", format(Sys.time()), "\n")

price_params <- NULL
if (!is.null(price_fit)) {
  save_model_output(price_fit, "model_price", price_stan_path)
  price_params <- extract_mle_params(price_fit)
  if (!is.null(price_params)) {
    cat("\n  model_price estimated successfully\n")
  }
} else {
  cat("  model_price estimation FAILED\n")
}

## ============================================================================
## 3. Estimate model_main
## ============================================================================

cat("\n================================================================\n")
cat("3. Estimating model_main\n")
cat("================================================================\n\n")

# Load sev/price params from estimated CSVs into data_list
if (!is.null(sev_fit) || !is.null(price_fit)) {
  sim_data_list <- load_sev_price_params(sim_data_list, MODEL_OUT_DIR, bootstrap_id = 0)
  saveRDS(sim_data_list, file.path(SIM_CACHE_DIR, "data_list_IL.rds"))
  cat("  Updated data_list with sev/price params and saved to disk\n\n")
}

main_stan_path <- file.path(STAN_MODEL_BASE, "model_main", "model_main.stan")
if (!file.exists(main_stan_path)) stop("model_main.stan not found at ", main_stan_path)

cat("Compiling model_main...\n")
main_model <- cmdstanr::cmdstan_model(
  main_stan_path,
  stanc_options = list("O1"),
  force_recompile = FALSE,
  cpp_options = list(stan_threads = TRUE)
)

# Build main model stan_data from sim_data_list (sev/price params now included)
stan_data_fields <- c(
  # Dimensions
  "I", "N_clm", "M", "N_sev_pareto_below_limit", "N_sev_pareto_at_limit",
  "N_sev_lnorm", "I_tm_R", "I_tm", "M_renw_active",
  "D_R_nb_scheme", "D_R_renw_scheme", "D_R_tm_scheme",
  "N_clm_to_I", "I_tm_R_ordered_to_N_clm", "R_tm_scheme_cutoffs",
  # Choice dimensions
  "N_choice", "N_choice_regimes", "n_choice_regime_cutoffs",
  "N_by_choice_regime", "Js",
  "N_choice_to_I", "N_choice_to_I_choice", "I_tm_to_N",
  "D_choice_R_tm", "D_choice_R_nb", "D_choice_R_renw",
  "choice_to_R_tm_ranges_n0", "choice_to_R_tm_ranges_n1", "choice_to_R_tm_ranges_R",
  "choice_to_R_nb_ranges_n0", "choice_to_R_nb_ranges_n1", "choice_to_R_nb_ranges_R",
  "choice_to_R_renw_ranges_n0", "choice_to_R_renw_ranges_n1", "choice_to_R_renw_ranges_R",
  # Covariates
  "X_clm", "X_mh_1", "X_mh_2", "X_mh_3",
  "X_choice", "X_rc_choice", "income_choice",
  "X_renw_active", "X_mh_mean",
  # Observed outcomes
  "clm_count_severe_N_clm", "clm_count_N_clm",
  "log_tm_score_R_ordered",
  "d_t_new",
  # Pricing environment
  "prices_nb_1", "prices_oo_nb_1", "prices_nb_tm_1", "prices_oo_nb_tm_1",
  "prices_nb_2", "prices_oo_nb_2", "prices_nb_tm_2", "prices_oo_nb_tm_2",
  "prices_renw_1", "prices_renw_oo_1", "prices_renw_2", "prices_renw_oo_2",
  "limits_1", "limits_2",
  "d_t1_new", "clm_surcharge_ftr", "tm_optin_disc_ftr_2",
  # Weights
  "sampling_enum_clm", "sampling_enum_tm_R",
  "estimation_weights_choice",
  "bootstrap_weights_clm", "bootstrap_weights_tm_R", "bootstrap_weights_choice",
  # Pre-estimated pricing model parameters (fixed data)
  "theta_0_R_nb_mean", "theta_1_R_nb_mean", "theta_0_R_nb_sd",
  "theta_0_R_renw_mean", "theta_1_R_renw_mean", "theta_0_R_renw_sd",
  "theta_0_R_tm_mean", "theta_1_R_tm_mean", "theta_0_R_tm_sd",
  # Pre-estimated cost/severity parameters (fixed data)
  "pareto_alpha_choice_base", "theta_mh_llambda",
  "sev_minor_mean_choice", "sev_minor_sd",
  # Estimation settings
  "dollar_norm", "grainsize", "eps_prior",
  "score_sd_lb", "score_sd_ub", "risk_aversion_ub", "sigma_logit_lb",
  "discount_factor",
  "risk_target_scale_factor", "risk_moment_scale_factor",
  "score_moment_scale_factor", "choice_moment_scale_factor",
  "select_moment_scale_factor",
  # Toggles
  "run_estimation", "run_demand_blocks", "d_by_block", "d_tm_mh_rational_ind",
  # Key competitor pricing
  "prices_kc_nb_1", "prices_kc_nb_tm_1", "prices_kc_nb_2",
  "prices_kc_nb_tm_2", "prices_kc_renw_1", "prices_kc_renw_2",
  # Moment matching targets
  "clm_count_I_tm_R_ordered", "clm_count_N_choice"
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

if (length(missing_fields) > 0) {
  cat("  WARNING: Missing fields:", paste(missing_fields, collapse = ", "), "\n")
}

# Ensure estimation is on
stan_data$run_estimation <- 1L
stan_data$run_demand_blocks <- stan_data$run_demand_blocks %||% 1L

cat("  Stan data:", length(stan_data), "of", length(stan_data_fields), "fields\n\n")

# Run optimization
cat("Running L-BFGS optimization (", MAX_ITER, " iter, ",
    NUM_THREADS, " threads)...\n", sep = "")
cat("Start:", format(Sys.time()), "\n\n")
main_init <- build_init("model_main")

main_fit <- tryCatch({
  main_model$optimize(
    data = stan_data,
    init = main_init$init,
    algorithm = "lbfgs",
    iter = MAX_ITER,
    threads = NUM_THREADS,
    init_alpha = main_init$init_alpha,
    tol_obj = TOL_OBJ,
    refresh = REFRESH
  )
}, error = function(e) {
  cat("\nOptimization error:", conditionMessage(e), "\n")
  NULL
})

cat("\nEnd:", format(Sys.time()), "\n\n")

if (!is.null(main_fit)) {
  save_model_output(main_fit, "model_main", main_stan_path)
  main_mle <- extract_mle_params(main_fit)
  if (!is.null(main_mle)) {
    lp <- as.numeric(main_mle[["lp__"]])
    cat("  Log-likelihood at MLE:", round(lp, 2), "\n")
  }
} else {
  cat("  model_main estimation FAILED\n")
}

cat("\n================================================================\n")
cat("sim_estimate.R complete\n")
cat("================================================================\n")
