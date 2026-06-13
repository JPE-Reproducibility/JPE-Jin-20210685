################################################################################
## sim_c8c_ctf_mhhet_learning.R — CTF for MH-het (Tab C.4) and Learning (Tab C.5)
##
## Both use model_main demand estimates. Differences:
##   - mhhet: swaps in theta_mh_llambda + theta_mh_llambda_rc from model_cost_mhhet
##   - learning: applies LEARNING_EFFECT_MULT (0.9) to renewal-period lambda
##
## Requires: model_main estimated, model_cost_mhhet estimated (for C.4 only)
################################################################################

cat("=============================================================\n")
cat("CTF ROBUSTNESS: MH-het and Learning\n")
cat("Started at:", format(Sys.time()), "\n")
cat("=============================================================\n\n")

if (!exists("SIM_DATA_DIR")) { USE_SIMULATED_DATA <- TRUE; source("code/config.R") }

if (!file.exists("code/config.R"))
  stop("Must be run from the repository root directory.")

## ---- Loop over robustness specs -------------------------------------------

robustness_specs <- list(
  list(config_suffix = "mhhet",   csv_name = "tab_c4.csv", rds_name = "ctf_tab_c4_mhhet.rds"),
  list(config_suffix = "learning", csv_name = "tab_c5.csv", rds_name = "ctf_tab_c5_learning.rds")
)

## ---- Dispatch: run each spec in a separate R process to avoid fork corruption

if (!exists("CTF_MHHET_SPEC")) {
  for (idx in seq_along(robustness_specs)) {
    spec <- robustness_specs[[idx]]
    cat("\n--- Dispatching", spec$config_suffix, "as subprocess ---\n")
    preamble <- paste0(
      "USE_SIMULATED_DATA <- TRUE; ",
      "TESTING <- ", TESTING, "; ",
      "USE_CACHE <- ", USE_CACHE, "; ",
      "CTF_MHHET_SPEC <- ", idx, "L; ",
      "source('code/config.R'); ",
      "source('code/simulate/sim_c8c_ctf_mhhet_learning.R')"
    )
    rc <- system(paste0("Rscript -e \"", preamble, "\""))
    if (rc != 0) cat("  WARNING:", spec$config_suffix, "failed with exit code", rc, "\n")
  }
  cat("\n=============================================================\n")
  cat("sim_c8c_ctf_mhhet_learning.R complete (all dispatched)\n")
  cat("=============================================================\n")
} else {

spec <- robustness_specs[[CTF_MHHET_SPEC]]

{
  cat("\n=============================================================\n")
  cat("Running CTF for:", spec$config_suffix, "->", spec$csv_name, "\n")
  cat("=============================================================\n\n")

  ## ---- Configuration (use model_main estimates) ----------------------------

  estimation_type <- "opt"
  data_states <- "IL"
  bootstrap_id <- 0
  # mhhet CTF uses model_main_mhhet estimates; learning uses plain model_main
  model_name <- if (spec$config_suffix == "mhhet") "model_main_mhhet" else "model_main"

  dl_profile <- jsonlite::fromJSON(file.path(SIM_DATA_DIR, "data_profile_data_list.json"))
  OPT_IN_FTR <- dl_profile$estimation$sim_params$opt_in_ftr

  USE_SIMULATED_ESTIMATES <- TRUE

  ## ---- Load estimation data and parameters ---------------------------------

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

  cat("  Using simulated model dir:", sim_model_dir, "\n")
  cat("  Log file suffix:", log_file_suffix, "\n")

  # Ensure X_rc_clm exists in data_list (needed by mhhet branch in load_estimation_bootstrap_data.R)
  dl_rds <- file.path(SIM_CACHE_DIR, "data_list_IL.rds")
  if (file.exists(dl_rds)) {
    dl_tmp <- readRDS(dl_rds)
    if (!"X_rc_clm" %in% names(dl_tmp) && "X_rc" %in% names(dl_tmp)) {
      dl_tmp$X_rc_clm <- dl_tmp$X_rc[dl_tmp$N_clm_to_I]
      saveRDS(dl_tmp, dl_rds)
      cat("  Added X_rc_clm to data_list\n")
    }
    rm(dl_tmp)
  }

  # Extract model estimates (sets model_config from model_name)
  # For mhhet: model_name = "model_main_mhhet" → model_config = "mhhet"
  #   → extract_model_estimates_cmdstan.R + load_estimation_bootstrap_data.R
  #     auto-load theta_mh_llambda_rc from COST_MHHET_MODEL_DIR
  # For learning: model_name = "model_main" → model_config = ""
  #   → we override model_config to "learning" after extraction
  source("code/simulate/functions/estimation/extract_model_estimates_cmdstan.R")

  if (spec$config_suffix == "learning") {
    model_config <- "learning"
    cat("  Overriding model_config to:", model_config, "\n")
  }
  cat("  model_config:", model_config, "\n")
  cat("  welfare_horizon:", welfare_horizon, "\n")
  cat("  Extraction complete.\n\n")

  ## ---- CTF Configuration ---------------------------------------------------

  cat("Step 2: Configuring CTF options...\n")

  ctf_oo_option <- "all_min_flex_direct"
  oo_constrained_toggle <- TRUE
  profit_horizon <- welfare_horizon
  tm_resource_cost <- TM_RESOURCE_COST_BASE

  mkt_structure <- paste0(
    ifelse(grepl("min_and_median", ctf_oo_option), "3f",
           ifelse(grepl("all", ctf_oo_option), "all", "2f")),
    ifelse(grepl("direct", ctf_oo_option), "_d", "")
  )
  flex_oo_name <- ifelse(grepl("min_flex", ctf_oo_option), "min",
                         ifelse(grepl("median_flex", ctf_oo_option), "median", "NA"))

  # Use model_main's ctf dir for intermediates (separate from main run)
  bootstrap_ctf_dir <- file.path(MODEL_OUT_DIR, model_name, paste0("ctf_", spec$config_suffix))
  dir.create(bootstrap_ctf_dir, recursive = TRUE, showWarnings = FALSE)

  k_block_ctf <- K_BLOCK_CTF

  ctf_config_preds <- paste0("_constrained_", ifelse(oo_constrained_toggle, "T", "F"))
  ctf_config_full <- ctf_config_preds

  cat("  profit_horizon:", profit_horizon, "\n")
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

  cat("Step 4: Running brand and cost calibration...\n")
  library(doParallel)
  library(foreach)
  num_cores <- parallel::detectCores() - 1
  registerDoParallel(cores = num_cores)

  source("code/simulate/functions/ctf/ctf_calibration.R")
  cat("  brand_value_calibrated:", round(brand_value_calibrated, 4), "\n")
  cat("  cost_factors_calibrated:", round(cost_factors_calibrated, 4), "\n\n")

  ## ---- Regime Calculation ---------------------------------------------------

  cat("Step 5: Setting up regime calculation...\n")
  source("code/simulate/functions/sim_helpers.R")

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

  cat("\n", spec$config_suffix, "CTF complete.\n")
  print(df_ctf_results[, c("firm_profit", "consumer_welfare", "total_surplus")])

  tryCatch(stopImplicitCluster(), error = function(e) NULL)
}

cat("\n=============================================================\n")
cat("sim_c8c_ctf_mhhet_learning.R: spec", CTF_MHHET_SPEC, "complete\n")
cat("=============================================================\n")
} # end if/else dispatcher
