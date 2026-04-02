#!/usr/bin/env Rscript
################################################################################
## sim_ctf_run.R — Run CTF computation from scratch (no pre-saved files)
##
## This script runs the complete CTF pipeline without using any cached results.
## It computes calibration, all pricing regimes, and equilibrium grid search.
##
## Runtime: ~3-7 hours
################################################################################

cat("=============================================================\n")
cat("CTF REPLICATION FROM SCRATCH\n")
cat("Started at:", format(Sys.time()), "\n")
cat("=============================================================\n\n")

# Load config
if (!exists("SIM_DATA_DIR")) { USE_SIMULATED_DATA <- TRUE; source("code/config.R") }

# Verify we're in the repo root
if (!file.exists("code/config.R")) {
  stop("Must be run from the repository root directory.")
}

## ---- Configuration ----------------------------------------------------------

estimation_type <- "opt"
data_states <- "IL"
bootstrap_id <- 0

model_name <- "model_main"

## CTF constants: IL_OWN_MKT_SHARE, IL_OO_MKT_SHARES, IL_DIRECT_SHARES,
## TM_RESOURCE_COST_BASE now come from config.R
dl_profile <- jsonlite::fromJSON(file.path(SIM_DATA_DIR, "data_profile_data_list.json"))
OPT_IN_FTR <- dl_profile$estimation$sim_params$opt_in_ftr

USE_SIMULATED_ESTIMATES <- TRUE

cat("Model:", model_name, "\n")
cat("Using simulated estimates:", USE_SIMULATED_ESTIMATES, "\n")

## ---- Load estimation data and parameters -----------------------------------

cat("Step 1: Loading estimation data and Stan model...\n")

# Load cmdstanr
library(cmdstanr)

# Load simulation helpers (needed for matrix_to_lists function)
source("code/simulate/functions/sim_helpers.R")

# Override extraction paths to use simulated model output
if (USE_SIMULATED_ESTIMATES) {
  sim_model_dir <- file.path(MODEL_OUT_DIR, model_name)
  results_dir <- sim_model_dir
  outdir <- sim_model_dir
  bootpath <- file.path(sim_model_dir, "bootstrap")
  bootstrap_results_dir <- file.path(bootpath, "results")

  # Discover simulated log_file_suffix from log file
  sim_logs <- list.files(sim_model_dir, pattern = "^log_.*\\.Rda$")
  if (length(sim_logs) > 0) {
    log_file_suffix <- sub(".*-([0-9]+-[0-9]+-.*)\\.Rda$", "\\1", sim_logs[length(sim_logs)])
  } else {
    stop("No log file found in ", sim_model_dir)
  }

  # MODEL_SEV_DIR and MODEL_PRICE_DIR are already set correctly by config.R
  # (both derive from MODEL_OUT_DIR which points to simulated output)

  cat("  Using simulated model dir:", sim_model_dir, "\n")
  cat("  Log file suffix:", log_file_suffix, "\n")
} else {
  log_file_suffix <- LOG_FILE_SUFFIXES[[model_name]]
}

cat("Log file suffix:", log_file_suffix, "\n\n")

# Source the extraction pipeline (this loads data, compiles Stan, computes latent params)
source("code/simulate/functions/estimation/extract_model_estimates_cmdstan.R")

cat("  pareto_alpha_choice_base: mean =", round(mean(pareto_alpha_choice_base), 4), "\n")
cat("  Extraction complete.\n\n")

## ---- CTF Configuration ------------------------------------------------------

cat("Step 2: Configuring CTF options...\n")

ctf_oo_option <- "all_min_flex_direct"
oo_constrained_toggle <- TRUE
profit_horizon <- welfare_horizon
tm_resource_cost <- TM_RESOURCE_COST_BASE

# Directory setup
mkt_structure <- paste0(
  ifelse(grepl("min_and_median", ctf_oo_option), "3f",
         ifelse(grepl("all", ctf_oo_option), "all", "2f")),
  ifelse(grepl("direct", ctf_oo_option), "_d", "")
)
flex_oo_name <- ifelse(grepl("min_flex", ctf_oo_option), "min",
                       ifelse(grepl("median_flex", ctf_oo_option), "median", "NA"))

if (USE_SIMULATED_DATA) {
  bootstrap_ctf_dir <- file.path(MODEL_OUT_DIR, model_name, "ctf")
} else {
  bootpath <- file.path(MODEL_OUT_DIR, model_name, "bootstrap")
  ctf_cache_name <- "ctf_real_data_real_param"
  bootstrap_ctf_dir <- file.path(bootpath, ctf_cache_name, mkt_structure, flex_oo_name)
}
dir.create(bootstrap_ctf_dir, recursive = TRUE, showWarnings = FALSE)

k_block_ctf <- K_BLOCK_CTF

# Configuration strings for cache filenames
ctf_config_preds <- paste0(
  "_constrained_", ifelse(oo_constrained_toggle, "T", "F")
)
ctf_config_full <- ctf_config_preds

cat("  ctf_oo_option:", ctf_oo_option, "\n")
cat("  profit_horizon:", profit_horizon, "\n")
cat("  Output dir:", bootstrap_ctf_dir, "\n\n")

## ---- Clear CTF cache if requested ------------------------------------------

# Rerun CTF from scratch in simulated mode, unless using cache
if (USE_SIMULATED_DATA && !USE_CACHE) {
  CTF_CLEAR_CACHE <- TRUE
} else if (!exists("CTF_CLEAR_CACHE")) {
  CTF_CLEAR_CACHE <- FALSE
}
source("code/simulate/functions/ctf/ctf_clear_cache.R")

## ---- CTF Preload ------------------------------------------------------------

cat("Step 3: Running CTF preload...\n")
source("code/simulate/functions/ctf/ctf_preload.R")
cat("  N_part:", N_part, ", J:", J, ", J_oo:", J_oo, "\n")
cat("  target_market_share:", target_market_share, "\n\n")

## ---- Run Original Calibration -----------------------------------------------

cat("Step 4: Running brand and cost calibration...\n")

# Register fork-based parallel backend once for all CTF work
library(doParallel)
library(foreach)
num_cores <- parallel::detectCores() - 1
cat("  Registering", num_cores, "parallel cores\n")
registerDoParallel(cores = num_cores)

# Source the original calibration script which handles both brand and cost calibration
# with proper grid search methods
source('code/simulate/functions/ctf/ctf_calibration.R')

cat("  Calibration complete.\n")
cat("  brand_value_calibrated:", round(brand_value_calibrated, 4), "\n")
cat("  cost_factors_calibrated:", round(cost_factors_calibrated, 4), "\n")
cat("  k0_calibrated:", k0_calibrated, ", k3_calibrated:", k3_calibrated, "\n\n")

## ---- Define Regime Calculation Function -------------------------------------

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
  flexible_oo_index <- out$flexible_oo_index

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

  # Assign to global environment
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

## ---- Compute Regimes --------------------------------------------------------

cat("\nStep 6: Computing pricing regimes...\n")

cat("  Computing current_regime...\n")
calculate_regime_vars(
  unmonitored_surcharge = 0,
  opt_in_discount = (1 - OPT_IN_FTR) * 100,
  rent_sharing_factor = 1 * 100,
  risk_surcharge_factor = 1 * 100,
  competitor_surcharge = 0,
  competitor_rent_sharing_factor = NA,
  competitor_risk_surcharge_factor = NA,
  suffix = "current"
)

cat("  Computing no_monitoring...\n")
calculate_regime_vars(
  unmonitored_surcharge = 0,
  opt_in_discount = NA,
  rent_sharing_factor = NA,
  risk_surcharge_factor = NA,
  competitor_surcharge = 0,
  competitor_rent_sharing_factor = NA,
  competitor_risk_surcharge_factor = NA,
  suffix = "no_tm"
)

## ---- Grid Search for Equilibria ---------------------------------------------

cat("\nStep 7: Running equilibrium grid search...\n")
cat("  Started at:", format(Sys.time()), "\n")
cat("  (This is the slow part - may take several hours)\n")

source("code/simulate/functions/ctf/ctf_equi_k_save_grid.R")

cat("  Grid search completed at:", format(Sys.time()), "\n")

## ---- Compute Remaining Regimes ----------------------------------------------

cat("\nStep 8: Computing equilibrium regimes...\n")

cat("  Computing partial_equi...\n")
calculate_regime_vars(
  unmonitored_surcharge = round(k0_part_opt / k0_calibrated - 1, 2) * 100,
  opt_in_discount = (1 - k1_part_opt) * 100,
  rent_sharing_factor = k2_part_opt * 100,
  risk_surcharge_factor = k2s_part_opt * 100,
  competitor_surcharge = 0,
  competitor_rent_sharing_factor = NA,
  competitor_risk_surcharge_factor = NA,
  suffix = "part_opt"
)

cat("  Computing optimal_pricing...\n")
calculate_regime_vars(
  unmonitored_surcharge = round(k0_opt / k0_calibrated - 1, 2) * 100,
  opt_in_discount = (1 - k1_opt) * 100,
  rent_sharing_factor = k2_opt * 100,
  risk_surcharge_factor = k2s_opt * 100,
  competitor_surcharge = round(k3_opt / k3_calibrated - 1, 2) * 100,
  competitor_rent_sharing_factor = NA,
  competitor_risk_surcharge_factor = NA,
  suffix = "opt"
)

cat("  Computing data_sharing...\n")
calculate_regime_vars(
  unmonitored_surcharge = round(k0_ds / k0_calibrated - 1, 2) * 100,
  opt_in_discount = (1 - k1_ds) * 100,
  rent_sharing_factor = k2_ds * 100,
  risk_surcharge_factor = k2s_ds * 100,
  competitor_surcharge = round(k3_ds / k3_calibrated - 1, 2) * 100,
  competitor_rent_sharing_factor = k4_ds * 100,
  competitor_risk_surcharge_factor = k4s_ds * 100,
  suffix = "ds"
)

cat("  Computing discount_floor...\n")
calculate_regime_vars(
  unmonitored_surcharge = round(k0_ds_df / k0_calibrated - 1, 2) * 100,
  opt_in_discount = (1 - k1_ds_df) * 100,
  rent_sharing_factor = k2_ds_df * 100,
  risk_surcharge_factor = k2s_ds_df * 100,
  competitor_surcharge = round(k3_ds_df / k3_calibrated - 1, 2) * 100,
  competitor_rent_sharing_factor = k4_ds_df * 100,
  competitor_risk_surcharge_factor = k4s_ds_df * 100,
  suffix = "ds_df"
)

## ---- Assemble Results -------------------------------------------------------

cat("\nStep 9: Assembling results...\n")

df_ctf_results <- data.frame(matrix(0, nrow = length(regimes), ncol = length(metrics)))
names(df_ctf_results) <- metrics
row.names(df_ctf_results) <- regimes

no_tm_vars <- mget(paste0(varnames, "_no_tm"))
current_vars <- mget(paste0(varnames, "_current"))
part_opt_vars <- mget(paste0(varnames, "_part_opt"))
opt_vars <- mget(paste0(varnames, "_opt"))
data_share_vars <- mget(paste0(varnames, "_ds"))
disc_floor_vars <- mget(paste0(varnames, "_ds_df"))

for (i in seq_along(metrics)) {
  metric <- metrics[i]
  df_ctf_results[[metric]][1] <- no_tm_vars[[i]]
  df_ctf_results[[metric]][2] <- current_vars[[i]]
  df_ctf_results[[metric]][3] <- part_opt_vars[[i]]
  df_ctf_results[[metric]][4] <- opt_vars[[i]]
  df_ctf_results[[metric]][5] <- data_share_vars[[i]]
  df_ctf_results[[metric]][6] <- disc_floor_vars[[i]]
}

## ---- Save Results -----------------------------------------------------------

cat("\nStep 10: Saving results...\n")

# Compute deltas relative to no-monitoring baseline (required by c2_ctf_main.R)
df_ctf_results_delta <- df_ctf_results
for (col in names(df_ctf_results_delta)) {
  df_ctf_results_delta[[col]] <- df_ctf_results[[col]] - df_ctf_results[[col]][1]
}

k_vars <- ls(pattern = "^(k|.*calibrated.*)")
k_list <- mget(k_vars)
final_results <- c(
  list(
    df_ctf_results = df_ctf_results,
    df_ctf_results_delta = df_ctf_results_delta,
    brand_value_calibrated = brand_value_calibrated,
    cost_factors_calibrated = cost_factors_calibrated
  ),
  k_list
)

# Save to CTF output directory (intermediate)
result_filename <- file.path(bootstrap_ctf_dir, paste0("ctf_results-id-", bootstrap_id, "_simulated.rds"))
saveRDS(final_results, result_filename)
cat("  Saved:", result_filename, "\n")

# Save to CTF_DIR (where c2_ctf_main.R reads in simulated mode)
dir.create(CTF_DIR, recursive = TRUE, showWarnings = FALSE)
sim_ctf_path <- file.path(CTF_DIR, "ctf_tab7_main.rds")
saveRDS(final_results, sim_ctf_path)
cat("  Saved:", sim_ctf_path, "\n")

# Save tab_7.csv for c3_get_fitctf_exhibits.R
# Format: same as data/precomputed/ctf/tab_7.csv
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

tab_7_csv <- rbind(level_df, delta_df)
tab_7_path <- file.path(CTF_DIR, "tab_7.csv")
write.csv(tab_7_csv, tab_7_path, row.names = FALSE)
cat("  Saved:", tab_7_path, "\n")

## ---- Print Results ----------------------------------------------------------

cat("\n=============================================================\n")
cat("RESULTS SUMMARY\n")
cat("=============================================================\n\n")

print(df_ctf_results[, c("firm_profit", "consumer_welfare", "total_surplus")])

stopImplicitCluster()
cat("\nCompleted at:", format(Sys.time()), "\n")
