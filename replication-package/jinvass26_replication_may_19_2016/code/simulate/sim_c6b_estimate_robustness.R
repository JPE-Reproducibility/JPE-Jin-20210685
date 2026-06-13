################################################################################
## sim_c6b_estimate_robustness.R — Estimate 2p and 4p robustness models
##
## Estimates model_main_2p and model_main_4p using the same data_list and
## sev/price params as model_main. Produces bootstrap CSVs for Tables A7/A8.
##
## Requires: sim_c6_estimate.R must have run first (sev/price params in data_list)
################################################################################

if (!exists("SIM_DATA_DIR")) { USE_SIMULATED_DATA <- TRUE; source("code/config.R") }
source("code/simulate/functions/sim_helpers.R")

cat("\n================================================================\n")
cat("sim_c6b_estimate_robustness.R: Estimating 2p and 4p models\n")
cat("================================================================\n\n")

## ---- Configuration (same as sim_c6) ----------------------------------------

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

## ---- Helpers (same as sim_c6) ----------------------------------------------

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

  ts <- format(Sys.time(), "%Y%m%d%H%M")
  log_suffix <- paste0(ts, "-1-sim")
  model_dir <- file.path(MODEL_OUT_DIR, model_name)

  log_file_obj <- list(
    output_filename = paste0(model_name, "-", log_suffix, ".csv"),
    model_filename = paste0(model_name, "-", log_suffix, ".stan"),
    bootstrap_id = 0L
  )
  save(log_file_obj, file = file.path(model_dir, paste0("log_", model_name, "-", log_suffix, ".Rda")))

  file.copy(stan_path, file.path(model_dir,
            paste0(model_name, "-", log_suffix, ".stan")),
            overwrite = TRUE)
}

## ---- Load data_list (already has sev/price params from sim_c6) -------------

dl_rds <- file.path(SIM_CACHE_DIR, "data_list_IL.rds")
if (!file.exists(dl_rds)) stop("data_list_IL.rds not found. Run sim_c6_estimate.R first.")
sim_data_list <- readRDS(dl_rds)

## ---- Build stan_data (same fields as model_main) ---------------------------

stan_data_fields <- c(
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

stan_data <- list()
for (f in stan_data_fields) {
  if (f %in% names(sim_data_list) && !is.null(sim_data_list[[f]]))
    stan_data[[f]] <- sim_data_list[[f]]
}
stan_data$run_estimation <- 1L
stan_data$run_demand_blocks <- stan_data$run_demand_blocks %||% 1L

## ---- Estimate 2p and 4p models --------------------------------------------

for (mn in c("model_main_2p", "model_main_4p")) {
  cat("\n================================================================\n")
  cat("Estimating", mn, "\n")
  cat("================================================================\n\n")

  # If cached output exists, use it as warm-start init with reduced iterations
  # (converges in 1-2 iters but ensures GQ block runs and output is fresh)
  existing_csv <- file.path(MODEL_OUT_DIR, mn, "results", "bootstrap-result-id-0.csv")
  cache_warm_start <- USE_CACHE && file.exists(existing_csv)
  if (cache_warm_start) {
    cat("  Warm-start from cached output (reduced iterations)\n")
  }

  stan_path <- file.path(STAN_MODEL_BASE, mn, paste0(mn, ".stan"))
  if (!file.exists(stan_path)) {
    cat("  SKIP:", stan_path, "not found\n")
    next
  }

  cat("Compiling", mn, "...\n")
  model <- cmdstanr::cmdstan_model(
    stan_path,
    stanc_options = list("O1"),
    force_recompile = FALSE,
    cpp_options = list(stan_threads = TRUE)
  )

  iter_use <- if (cache_warm_start) 100 else MAX_ITER
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

  if (!is.null(fit)) {
    save_model_output(fit, mn, stan_path)
    mle <- tryCatch(cbind.data.frame(fit$lp(), t(fit$mle())), error = function(e) NULL)
    if (!is.null(mle)) {
      cat("  Log-likelihood at MLE:", round(as.numeric(mle[["lp__"]]), 2), "\n")
    }
  } else {
    cat(" ", mn, "estimation FAILED\n")
  }
}

cat("\n================================================================\n")
cat("sim_c6b_estimate_robustness.R complete\n")
cat("================================================================\n")
