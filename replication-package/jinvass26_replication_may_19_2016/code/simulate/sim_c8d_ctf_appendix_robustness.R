################################################################################
## sim_c8d_ctf_appendix_robustness.R — CTF for Tables A.9, A.10, A.13-A.15
##
## All use model_main estimates with different CTF configurations:
##   A.9:  50% lower monitoring cost
##   A.10: 50% higher monitoring cost
##   A.13: 2X monitoring pricing constraints (relaxed)
##   A.14: Concentrated market (min + median only)
##   A.15: Median-pricing composite competitor
##
## Requires: model_main estimated.
################################################################################

cat("=============================================================\n")
cat("CTF APPENDIX ROBUSTNESS: Tables A.9, A.10, A.13-A.15\n")
cat("Started at:", format(Sys.time()), "\n")
cat("=============================================================\n\n")

if (!exists("SIM_DATA_DIR")) { USE_SIMULATED_DATA <- TRUE; source("code/config.R") }

if (!file.exists("code/config.R"))
  stop("Must be run from the repository root directory.")

## ---- Robustness specifications ----------------------------------------------

robustness_specs <- list(
  list(
    label = "A.9: 50% lower monitoring cost",
    csv_name = "tab_a9.csv", rds_name = "ctf_tab_a9_low_cost.rds",
    tm_resource_cost = TM_RESOURCE_COST_BASE * 0.5,
    oo_constrained_toggle = TRUE,
    ctf_oo_option = "all_min_flex_direct",
    ctf_subdir = "ctf_low_cost"
  ),
  list(
    label = "A.10: 50% higher monitoring cost",
    csv_name = "tab_a10.csv", rds_name = "ctf_tab_a10_high_cost.rds",
    tm_resource_cost = TM_RESOURCE_COST_BASE * 1.5,
    oo_constrained_toggle = TRUE,
    ctf_oo_option = "all_min_flex_direct",
    ctf_subdir = "ctf_high_cost"
  ),
  list(
    label = "A.13: 2X pricing constraints (relaxed)",
    csv_name = "tab_a13.csv", rds_name = "ctf_tab_a13_unconst.rds",
    tm_resource_cost = TM_RESOURCE_COST_BASE,
    oo_constrained_toggle = FALSE,
    ctf_oo_option = "all_min_flex_direct",
    ctf_subdir = "ctf_unconst"
  ),
  list(
    label = "A.14: Concentrated market (min + median)",
    csv_name = "tab_a14.csv", rds_name = "ctf_tab_a14_minmed.rds",
    tm_resource_cost = TM_RESOURCE_COST_BASE,
    oo_constrained_toggle = TRUE,
    ctf_oo_option = "min_and_median_min_flex_direct",
    ctf_subdir = "ctf_minmed"
  ),
  list(
    label = "A.15: Median-pricing focal competitor",
    csv_name = "tab_a15.csv", rds_name = "ctf_tab_a15_medflex.rds",
    tm_resource_cost = TM_RESOURCE_COST_BASE,
    oo_constrained_toggle = TRUE,
    ctf_oo_option = "all_median_flex_direct",
    ctf_subdir = "ctf_medflex"
  )
)

## ---- Dispatch: run each spec in a separate R process to avoid fork corruption

if (!exists("CTF_APPENDIX_SPEC")) {
  # Dispatcher mode: launch each spec as a subprocess
  for (idx in seq_along(robustness_specs)) {
    spec <- robustness_specs[[idx]]
    cat("\n--- Dispatching", spec$label, "as subprocess ---\n")
    preamble <- paste0(
      "USE_SIMULATED_DATA <- TRUE; ",
      "TESTING <- ", TESTING, "; ",
      "USE_CACHE <- ", USE_CACHE, "; ",
      "CTF_APPENDIX_SPEC <- ", idx, "L; ",
      "source('code/config.R'); ",
      "source('code/simulate/sim_c8d_ctf_appendix_robustness.R')"
    )
    rc <- system(paste0("Rscript -e \"", preamble, "\""))
    if (rc != 0) {
      cat("  WARNING:", spec$label, "failed with exit code", rc, "\n")
    }
  }
  cat("\n=============================================================\n")
  cat("sim_c8d_ctf_appendix_robustness.R complete (all dispatched)\n")
  cat("=============================================================\n")
} else {

## ---- Single-spec mode: run the spec indicated by CTF_APPENDIX_SPEC ----------

spec <- robustness_specs[[CTF_APPENDIX_SPEC]]

  cat("\n=============================================================\n")
  cat("Running CTF:", spec$label, "->", spec$csv_name, "\n")
  cat("=============================================================\n\n")

  ## ---- Configuration -------------------------------------------------------

  estimation_type <- "opt"
  data_states <- "IL"
  bootstrap_id <- 0
  model_name <- "model_main"

  dl_profile <- jsonlite::fromJSON(file.path(SIM_DATA_DIR, "data_profile_data_list.json"))
  OPT_IN_FTR <- dl_profile$estimation$sim_params$opt_in_ftr

  USE_SIMULATED_ESTIMATES <- TRUE

  ## ---- Load estimation data ------------------------------------------------

  cat("Step 1: Loading estimation data and Stan model...\n")
  library(cmdstanr)
  source("code/simulate/functions/sim_helpers.R")

  sim_model_dir <- file.path(MODEL_OUT_DIR, model_name)
  results_dir <- sim_model_dir
  outdir <- sim_model_dir
  bootpath <- file.path(sim_model_dir, "results")
  bootstrap_results_dir <- bootpath

  sim_logs <- list.files(sim_model_dir, pattern = "^log_.*\\.Rda$")
  if (length(sim_logs) > 0) {
    log_file_suffix <- sub(".*-([0-9]+-[0-9]+-.*)\\.Rda$", "\\1", sim_logs[length(sim_logs)])
  } else {
    stop("No log file found in ", sim_model_dir)
  }

  source("code/simulate/functions/estimation/extract_model_estimates_cmdstan.R")
  cat("  Extraction complete.\n\n")

  ## ---- CTF Configuration ---------------------------------------------------

  cat("Step 2: Configuring CTF options...\n")

  ctf_oo_option <- spec$ctf_oo_option
  oo_constrained_toggle <- spec$oo_constrained_toggle
  profit_horizon <- welfare_horizon
  tm_resource_cost <- spec$tm_resource_cost

  mkt_structure <- paste0(
    ifelse(grepl("min_and_median", ctf_oo_option), "3f",
           ifelse(grepl("all", ctf_oo_option), "all", "2f")),
    ifelse(grepl("direct", ctf_oo_option), "_d", "")
  )
  flex_oo_name <- ifelse(grepl("min_flex", ctf_oo_option), "min",
                         ifelse(grepl("median_flex", ctf_oo_option), "median", "NA"))

  bootstrap_ctf_dir <- file.path(MODEL_OUT_DIR, model_name, spec$ctf_subdir)
  dir.create(bootstrap_ctf_dir, recursive = TRUE, showWarnings = FALSE)

  k_block_ctf <- K_BLOCK_CTF

  ctf_config_preds <- paste0("_constrained_", ifelse(oo_constrained_toggle, "T", "F"))
  ctf_config_full <- ctf_config_preds

  cat("  ctf_oo_option:", ctf_oo_option, "\n")
  cat("  oo_constrained:", oo_constrained_toggle, "\n")
  cat("  tm_resource_cost:", tm_resource_cost, "\n")
  cat("  mkt_structure:", mkt_structure, "\n")
  cat("  Output dir:", bootstrap_ctf_dir, "\n\n")

  ## ---- Clear CTF cache if requested ----------------------------------------

  if (USE_SIMULATED_DATA && !USE_CACHE) {
    CTF_CLEAR_CACHE <- TRUE
  } else if (!exists("CTF_CLEAR_CACHE")) {
    CTF_CLEAR_CACHE <- FALSE
  }
  source("code/simulate/functions/ctf/ctf_clear_cache.R")

  ## ---- CTF Preload ----------------------------------------------------------

  cat("Step 3: Running CTF preload...\n")
  source("code/simulate/functions/ctf/ctf_preload.R")
  cat("  N_part:", N_part, ", J:", J, ", J_oo:", J_oo, "\n\n")

  ## ---- Calibration ----------------------------------------------------------

  cat("Step 4: Running calibration...\n")
  library(doParallel)
  library(foreach)
  num_cores <- parallel::detectCores() - 1
  registerDoParallel(cores = num_cores)

  source("code/simulate/functions/ctf/ctf_calibration.R")
  cat("  brand_value_calibrated:", round(brand_value_calibrated, 4), "\n")
  cat("  cost_factors_calibrated:", round(cost_factors_calibrated, 4), "\n\n")

  ## ---- Regime Calculation ---------------------------------------------------

  cat("Step 5: Setting up regime calculation...\n")

  metrics <- c(
    "consumer_welfare", "firm_profit", "competitor_profit", "industry_profit",
    "total_surplus", "coverage", "firm_market_share", "first_period_firm_choice_prob",
    "renewal_firm_choice_prob", "monitoring_market_share",
    "unmonitored_surcharge", "opt_in_discount", "rent_sharing_factor",
    "risk_surcharge_factor", "competitor_surcharge",
    "competitor_rent_sharing_factor", "competitor_risk_surcharge_factor"
  )
  varnames <- c(
    "welfare", "pi", "pi_oo", "pi_all", "surplus", "cov", "share_own",
    "cp_own", "cp_own_second_period", "cp_tm",
    "unmonitored_surcharge", "opt_in_discount", "rent_sharing_factor",
    "risk_surcharge_factor", "competitor_surcharge",
    "competitor_rent_sharing_factor", "competitor_risk_surcharge_factor"
  )
  regimes <- c("no_monitoring", "current_regime", "partial_equi",
               "optimal_pricing", "data_sharing", "discount_floor")

  calculate_regime_vars <- function(unmonitored_surcharge, opt_in_discount,
                                    rent_sharing_factor, risk_surcharge_factor,
                                    competitor_surcharge,
                                    competitor_rent_sharing_factor,
                                    competitor_risk_surcharge_factor,
                                    suffix) {
    source("code/simulate/functions/ctf/get_ctf_util_profit.R")
    out <- get_ctf_util_profit(
      k0 = k0_calibrated * (1 + unmonitored_surcharge / 100),
      k1 = 1 - opt_in_discount / 100,
      k2 = rent_sharing_factor / 100,
      k2s = risk_surcharge_factor / 100,
      k3 = k3_calibrated * (1 + competitor_surcharge / 100),
      k4 = competitor_rent_sharing_factor / 100,
      k4s = competitor_risk_surcharge_factor / 100,
      brand_value = brand_value_calibrated,
      cost_factors = cost_factors_calibrated,
      oo_tm_response_ind = !is.na(competitor_rent_sharing_factor),
      ctf_has_tm = !is.na(opt_in_discount)
    )

    cp <- out$cp_first_period
    cp_t2 <- out$cp_second_period

    if (!is.na(opt_in_discount)) {
      cp_tm <- cp[, (J + J_oo + 1):(J + J_oo + J)]
      cp_tm_var <- get_samp_wgt_avg(rowSums(cp_tm), sampling_weight) * 100
    } else {
      cp_tm <- 0
      cp_tm_var <- 0
    }

    cp_own <- cp[, 1:J]
    cp_oo <- cp[, (J + 1):(J + J_oo)]
    cp_cov_oo <- cp_own * 0
    for (d in seq_len(J)) {
      cp_cov_oo[, d] <- rowSums(cp_oo[, limits_oo_base == limits_base[d], drop = FALSE])
    }
    cov_var <- get_samp_wgt_avg(rowSums(sweep(cp_own + cp_cov_oo + cp_tm, 2, limits_2, "*")), sampling_weight) * dollar_norm
    cp_own_var <- get_samp_wgt_avg(rowSums(cp_own), sampling_weight) * 100
    share_own_var <- get_samp_wgt_avg(rowSums(cp * out$revs), sampling_weight) /
      get_samp_wgt_avg(rowSums(cp * (out$revs + out$revs_oo)), sampling_weight) * 100
    cp_own_second_period_var <- get_samp_wgt_avg(rowSums(cp_t2[, 1:J]), sampling_weight) * 100

    pi_var <- get_samp_wgt_avg(rowSums(cp * out$profits), sampling_weight) * dollar_norm
    pi_oo_var <- get_samp_wgt_avg(rowSums(cp * out$profits_oo_flex), sampling_weight) * dollar_norm
    pi_all_var <- pi_var + pi_oo_var

    welfare_var <- get_samp_wgt_avg(
      ctf_model$functions$get_welfare(cbind(out$utils_combined), rep(tmp_sigma, N_part), 1),
      sampling_weight
    ) * dollar_norm
    surplus_var <- pi_all_var + welfare_var

    assign(paste0("welfare_", suffix), welfare_var, envir = .GlobalEnv)
    assign(paste0("pi_", suffix), pi_var, envir = .GlobalEnv)
    assign(paste0("pi_oo_", suffix), pi_oo_var, envir = .GlobalEnv)
    assign(paste0("pi_all_", suffix), pi_all_var, envir = .GlobalEnv)
    assign(paste0("surplus_", suffix), surplus_var, envir = .GlobalEnv)
    assign(paste0("cov_", suffix), cov_var, envir = .GlobalEnv)
    assign(paste0("cp_own_", suffix), cp_own_var, envir = .GlobalEnv)
    assign(paste0("cp_own_second_period_", suffix), cp_own_second_period_var, envir = .GlobalEnv)
    assign(paste0("share_own_", suffix), share_own_var, envir = .GlobalEnv)
    assign(paste0("cp_tm_", suffix), cp_tm_var, envir = .GlobalEnv)
    assign(paste0("unmonitored_surcharge_", suffix), unmonitored_surcharge, envir = .GlobalEnv)
    assign(paste0("opt_in_discount_", suffix), opt_in_discount, envir = .GlobalEnv)
    assign(paste0("rent_sharing_factor_", suffix), rent_sharing_factor, envir = .GlobalEnv)
    assign(paste0("risk_surcharge_factor_", suffix), risk_surcharge_factor, envir = .GlobalEnv)
    assign(paste0("competitor_surcharge_", suffix), competitor_surcharge, envir = .GlobalEnv)
    assign(paste0("competitor_rent_sharing_factor_", suffix), competitor_rent_sharing_factor, envir = .GlobalEnv)
    assign(paste0("competitor_risk_surcharge_factor_", suffix), competitor_risk_surcharge_factor, envir = .GlobalEnv)

    cat("  ", suffix, ": pi =", round(pi_var, 2), ", welfare =", round(welfare_var, 2), "\n")
  }

  ## ---- Compute Regimes ------------------------------------------------------

  cat("\nStep 6: Computing pricing regimes...\n")

  cat("  Computing current_regime...\n")
  calculate_regime_vars(0, (1 - OPT_IN_FTR) * 100, 100, 100, 0, NA, NA, "current")

  cat("  Computing no_monitoring...\n")
  calculate_regime_vars(0, NA, NA, NA, 0, NA, NA, "no_monitoring")

  ## ---- Equilibrium Grid Search ----------------------------------------------

  cat("\nStep 7: Running equilibrium grid search...\n")
  cat("  Started at:", format(Sys.time()), "\n")

  source("code/simulate/functions/ctf/ctf_equi_k_save_grid.R")
  cat("  Grid search completed at:", format(Sys.time()), "\n")

  ## ---- Compute Remaining Regimes -------------------------------------------

  cat("\nStep 8: Computing equilibrium regimes...\n")

  cat("  Computing partial_equi...\n")
  calculate_regime_vars(
    round(k0_part_opt / k0_calibrated - 1, 2) * 100,
    (1 - k1_part_opt) * 100, k2_part_opt * 100, k2s_part_opt * 100,
    0, NA, NA, "part_opt"
  )

  cat("  Computing optimal_pricing...\n")
  calculate_regime_vars(
    round(k0_opt / k0_calibrated - 1, 2) * 100,
    (1 - k1_opt) * 100, k2_opt * 100, k2s_opt * 100,
    round(k3_opt / k3_calibrated - 1, 2) * 100, NA, NA, "opt"
  )

  cat("  Computing data_sharing...\n")
  calculate_regime_vars(
    round(k0_ds / k0_calibrated - 1, 2) * 100,
    (1 - k1_ds) * 100, k2_ds * 100, k2s_ds * 100,
    round(k3_ds / k3_calibrated - 1, 2) * 100,
    k4_ds * 100, k4s_ds * 100, "ds"
  )

  cat("  Computing discount_floor...\n")
  calculate_regime_vars(
    round(k0_ds_df / k0_calibrated - 1, 2) * 100,
    (1 - k1_ds_df) * 100, k2_ds_df * 100, k2s_ds_df * 100,
    round(k3_ds_df / k3_calibrated - 1, 2) * 100,
    k4_ds_df * 100, k4s_ds_df * 100, "ds_df"
  )

  ## ---- Build results table --------------------------------------------------

  k_list <- list(
    no_monitoring = list(k0 = k0_calibrated, k1 = NA, k2 = NA, k2s = NA, k3 = k3_calibrated, k4 = NA, k4s = NA),
    current_regime = list(k0 = k0_calibrated, k1 = OPT_IN_FTR, k2 = 1, k2s = 1, k3 = k3_calibrated, k4 = NA, k4s = NA),
    partial_equi = list(k0 = k0_part_opt, k1 = k1_part_opt, k2 = k2_part_opt, k2s = k2s_part_opt, k3 = k3_calibrated, k4 = NA, k4s = NA),
    optimal_pricing = list(k0 = k0_opt, k1 = k1_opt, k2 = k2_opt, k2s = k2s_opt, k3 = k3_opt, k4 = NA, k4s = NA),
    data_sharing = list(k0 = k0_ds, k1 = k1_ds, k2 = k2_ds, k2s = k2s_ds, k3 = k3_ds, k4 = k4_ds, k4s = k4s_ds),
    discount_floor = list(k0 = k0_ds_df, k1 = k1_ds_df, k2 = k2_ds_df, k2s = k2s_ds_df, k3 = k3_ds_df, k4 = k4_ds_df, k4s = k4s_ds_df)
  )

  suffixes <- c("no_monitoring", "current", "part_opt", "opt", "ds", "ds_df")
  df_ctf_results <- data.frame(matrix(NA, nrow = length(regimes), ncol = length(varnames)))
  colnames(df_ctf_results) <- metrics
  for (i in seq_along(suffixes)) {
    for (j in seq_along(varnames)) {
      val <- get0(paste0(varnames[j], "_", suffixes[i]))
      if (!is.null(val)) df_ctf_results[i, j] <- val
    }
  }

  # Raw differences from no-monitoring baseline (matches sim_c8 / tab_7 convention;
  # downstream c3_get_fitctf_exhibits.R expects absolute differences).
  baseline <- df_ctf_results[1, ]
  df_ctf_results_delta <- df_ctf_results
  for (j in 1:ncol(df_ctf_results)) {
    if (is.numeric(baseline[[j]]) && !is.na(baseline[[j]]))
      df_ctf_results_delta[, j] <- df_ctf_results[, j] - baseline[[j]]
  }

  ## ---- Save results ---------------------------------------------------------

  dir.create(CTF_DIR, recursive = TRUE, showWarnings = FALSE)

  level_df <- df_ctf_results
  level_df$scenario <- regimes
  level_df$type <- "level"
  level_df$brand_value_1 <- brand_value_calibrated[1]
  level_df$brand_value_2 <- brand_value_calibrated[2]
  level_df$brand_value_3 <- if (length(brand_value_calibrated) >= 3) brand_value_calibrated[3] else NA
  level_df$cost_factor_1 <- cost_factors_calibrated[1]
  level_df$cost_factor_2 <- cost_factors_calibrated[2]
  level_df$cost_factor_3 <- cost_factors_calibrated[3]

  delta_df <- df_ctf_results_delta
  delta_df$scenario <- regimes
  delta_df$type <- "delta"
  delta_df$brand_value_1 <- brand_value_calibrated[1]
  delta_df$brand_value_2 <- brand_value_calibrated[2]
  delta_df$brand_value_3 <- if (length(brand_value_calibrated) >= 3) brand_value_calibrated[3] else NA
  delta_df$cost_factor_1 <- cost_factors_calibrated[1]
  delta_df$cost_factor_2 <- cost_factors_calibrated[2]
  delta_df$cost_factor_3 <- cost_factors_calibrated[3]

  csv_df <- rbind(level_df, delta_df)
  csv_path <- file.path(CTF_DIR, spec$csv_name)
  write.csv(csv_df, csv_path, row.names = FALSE)
  cat("  Saved:", csv_path, "\n")

  final_results <- list(
    df_ctf_results = df_ctf_results,
    df_ctf_results_delta = df_ctf_results_delta,
    brand_value_calibrated = brand_value_calibrated,
    cost_factors_calibrated = cost_factors_calibrated,
    k_list = k_list
  )
  saveRDS(final_results, file.path(CTF_DIR, spec$rds_name))

  cat("\n", spec$label, "complete.\n")
  print(df_ctf_results[, c("firm_profit", "consumer_welfare", "total_surplus")])

  tryCatch(stopImplicitCluster(), error = function(e) NULL)

cat("\n=============================================================\n")
cat("sim_c8d_ctf_appendix_robustness.R: spec", CTF_APPENDIX_SPEC, "complete\n")
cat("=============================================================\n")
} # end if/else dispatcher
