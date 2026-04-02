################################################################################
## sim_generate_rf_data.R — Generate simulated RF datasets from data_profile_rf.json
##
## Creates synthetic CSV files for reduced-form analysis scripts to run without
## proprietary data access.
##
## Reads: data/simulated/data_profile_rf.json
## Writes: data/simulated/ — 4 files:
##   data_rf.csv        — denormalized panel (panel + choice_panel + exps + renw + df_mh + ubi)
##   data_rf_rrev.csv   — rate revision IV lookup (IL only)
##   data_rf_viol.csv   — violation lags (3-state, appendix only)
##   ubi_vers_dates.csv — state × year × month monitoring version lookup
##
## Usage: Rscript code/simulate/simulate_data/sim_generate_rf_data.R
################################################################################

if (!exists("SIM_DATA_DIR")) { USE_SIMULATED_DATA <- TRUE; source("code/config.R") }
source("code/functions/helper.R")

set.seed(42)
library(dplyr)
library(tidyr)

## ---- Config ----------------------------------------------------------------
N_POLICIES <- 80000    # Match real data scale (~80K policies)
MAX_RENW   <- 5        # Max renewal count
SIM_DIR    <- SIM_CACHE_DIR

## ---- DGP parameters (causal structure) -------------------------------------
## These embed the paper's empirical findings into the simulated data so that
## reduced-form analyses reproduce qualitatively correct patterns.
## Values now read from data_profile_rf.json dgp_params (populated below)

## ---- Load RF data profile (single JSON with moments + panel stats) ----------
profile_path <- file.path(SIM_DATA_DIR, "data_profile_rf.json")
if (!file.exists(profile_path))
  stop("data_profile_rf.json not found at ", profile_path,
       "\n  This file must be provided in the replication package under data/simulated/.")
if (!requireNamespace("jsonlite", quietly = TRUE)) install.packages("jsonlite")
profile <- jsonlite::fromJSON(profile_path, simplifyVector = TRUE)
cat("Loaded RF data profile from", profile_path, "\n")
mom <- profile$moments  # scalar moments from real data
dgp <- profile$dgp_params
stopifnot("dgp_params section missing from data_profile_rf.json" = !is.null(dgp))
RISK_SD       <- dgp$risk_sd
SELECTION_STR <- dgp$selection_str

## ---- Parse RF estimates from benchmark Tab 2 --------------------------------
## Reads real-data MH regression coefficients from the canonical JSON export.
## Col 3 = ind_XY_ctrl (balanced indicator with X+Y controls).
## Values are formatted to string then re-parsed to match the bit-identical
## floating-point path used by the main repo's LaTeX regex extraction
## (e.g., as.numeric("29.97")/100 rather than round(0.2997119, 4)).
parse_tab2 <- function(json_path = "data/estimates/regression_output/tab_2_regression.json") {
  result <- list(mh_pct = NA, mon_coef = NA)
  if (!file.exists(json_path)) return(result)

  reg <- jsonlite::fromJSON(json_path)

  # implied_mh_pct[3] → format as "XX.XX%" then parse back, matching LaTeX path
  if (!is.null(reg$implied_mh_pct) && length(reg$implied_mh_pct) >= 3)
    result$mh_pct <- as.numeric(sprintf("%.2f", reg$implied_mh_pct[3] * 100)) / 100

  # Col 3 monitoring coefficient → format as "X.XXX" then parse back, matching LaTeX path
  col3 <- reg$models$ind_XY_ctrl$coefficients
  if (!is.null(col3$ubi_ind$estimate))
    result$mon_coef <- as.numeric(sprintf("%.3f", col3$ubi_ind$estimate))

  result
}

tab2 <- parse_tab2()

## ---- Set DGP parameters from profile moments --------------------------------
if (is.null(mom$ubi_optin_rate)) stop("FALLBACK: mom$ubi_optin_rate is NULL")
if (is.null(mom$ubi_fin_rate))   stop("FALLBACK: mom$ubi_fin_rate is NULL")
if (is.null(mom$claim_rate))     stop("FALLBACK: mom$claim_rate is NULL")
UBI_OPTIN_RATE  <- mom$ubi_optin_rate
UBI_FIN_RATE    <- mom$ubi_fin_rate
CLAIM_RATE_BASE <- log(mom$claim_rate)

# MH effect: from implied MH percentage in Tab 2 Col 3
if (is.na(tab2$mh_pct))   stop("FALLBACK: tab2$mh_pct is NA — parse_tab2 failed")
MH_EFFECT <- log(1 - tab2$mh_pct)
# e.g., log(1 - 0.2997) ≈ -0.356

## Create output directories
dir.create(SIM_DIR, recursive = TRUE, showWarnings = FALSE)

## ---- Panel stats from profile (already loaded above) -----------------------

## ---- Helper: generate column from profile stats ----------------------------
gen_col <- function(stat, n) {
  if (is.null(stat)) return(rep(NA_real_, n))

  cls <- stat$class
  if (cls %in% c("numeric", "integer")) {
    if (isTRUE(stat$is_binary)) {
      if (is.null(stat$prop_1)) stop("FALLBACK: gen_col prop_1 is NULL for binary column")
      p <- stat$prop_1
      x <- rbinom(n, 1, p)
    } else {
      if (is.null(stat$mean)) stop("FALLBACK: gen_col mean is NULL for numeric column")
      if (is.null(stat$sd) || is.na(stat$sd) || stat$sd == 0) stop("FALLBACK: gen_col sd is NULL/NA/0 for numeric column")
      mu <- stat$mean
      s  <- stat$sd
      x <- rnorm(n, mu, s)
      # Clamp to observed range
      if (!is.null(stat$min)) x <- pmax(x, stat$min)
      if (!is.null(stat$max)) x <- pmin(x, stat$max)
    }
    if (cls == "integer") x <- as.integer(round(x))
    # Add NAs
    if (!is.null(stat$prop_na) && stat$prop_na > 0) {
      na_idx <- sample(n, round(n * stat$prop_na))
      x[na_idx] <- NA
    }
    return(x)
  }

  if (cls %in% c("factor", "character")) {
    if (!is.null(stat$top5) && length(stat$top5) > 0 && !is.null(names(stat$top5))) {
      lvls <- names(stat$top5)
      probs <- as.numeric(stat$top5) / sum(as.numeric(stat$top5))
      x <- sample(lvls, n, replace = TRUE, prob = probs)
    } else if (!is.null(stat$levels) && length(stat$levels) > 0) {
      x <- sample(stat$levels, n, replace = TRUE)
    } else {
      stop("FALLBACK: gen_col factor/character has no top5 or levels")
    }
    if (cls == "factor") x <- factor(x)
    return(x)
  }

  if (cls == "logical") {
    if (is.null(stat$prop_true)) stop("FALLBACK: gen_col prop_true is NULL for logical column")
    p <- stat$prop_true
    return(as.logical(rbinom(n, 1, p)))
  }

  return(rep(NA, n))
}

## ---- 1. Generate base panel ------------------------------------------------
cat("Generating panel_us...\n")

if (is.null(profile$states)) stop("FALLBACK: profile$states is NULL")
states <- profile$states
# Assign states proportionally (IL gets more)
state_probs <- rep(1/length(states), length(states))
il_idx <- which(states == "IL")
if (length(il_idx) > 0) state_probs[il_idx] <- 0.3
state_probs <- state_probs / sum(state_probs)

pol_ids <- sprintf("SIM_%05d", 1:N_POLICIES)
pol_states <- sample(states, N_POLICIES, replace = TRUE, prob = state_probs)
## Derive max_renw distribution from real survival probabilities
if (is.null(mom$p_see_renw)) stop("FALLBACK: mom$p_see_renw is NULL")
p_see <- mom$p_see_renw
p_surv <- c(1.0, p_see[1], p_see[2], p_see[3], p_see[4], p_see[5])
## P(max_renw = k) = p_surv[k+1] - p_surv[k+2], with last bucket absorbing remainder
p_drop <- -diff(c(p_surv, 0))
pol_max_renw <- sample(0:MAX_RENW, N_POLICIES, replace = TRUE, prob = p_drop)

## --- Latent risk per driver (shared across all downstream data) ---
## This drives claims, selection into monitoring, and score-claim correlation.
drv_log_lambda <- rnorm(N_POLICIES, CLAIM_RATE_BASE, RISK_SD)
drv_risk_z <- (drv_log_lambda - mean(drv_log_lambda)) / sd(drv_log_lambda)

## Advantageous selection: lower-risk drivers more likely to opt into monitoring
drv_optin_prob <- plogis(-SELECTION_STR * drv_risk_z +
                          qlogis(UBI_OPTIN_RATE))  # shift so mean ≈ UBI_OPTIN_RATE
drv_ubi_optin <- rbinom(N_POLICIES, 1, drv_optin_prob)
drv_ubi_fin   <- drv_ubi_optin * rbinom(N_POLICIES, 1, UBI_FIN_RATE)

# Expand to panel
panel_rows <- do.call(rbind, lapply(1:N_POLICIES, function(i) {
  data.frame(
    POL_ID_CHAR = pol_ids[i],
    RENW_CNT = 0:pol_max_renw[i],
    last_renewal_seen = pol_max_renw[i],
    state_alpha = factor(pol_states[i]),
    stringsAsFactors = FALSE
  )
}))
N_ROWS <- nrow(panel_rows)

# State code mapping
state_map <- setNames(1:length(states), states)
panel_rows$ST_CD <- state_map[as.character(panel_rows$state_alpha)]

# Generate covariates from profile
panel_stats <- profile$panel$stats
covariate_cols <- setdiff(names(panel_stats),
                          c("POL_ID_CHAR", "RENW_CNT", "last_renewal_seen",
                            "state_alpha", "ST_CD"))

for (col in covariate_cols) {
  panel_rows[[col]] <- gen_col(panel_stats[[col]], N_ROWS)
}

# Ensure x_drvr_lic_yr exists (years of license, typically 3-30; needed by c2_mh_viol.R filter)
if (!"x_drvr_lic_yr" %in% colnames(panel_rows)) {
  panel_rows$x_drvr_lic_yr <- sample(3:30, N_ROWS, replace = TRUE,
                                      prob = c(rep(0.02, 5), rep(0.04, 10), rep(0.02, 13)))
}

# Fix date_pol_eff to be proper dates
base_date <- as.Date(dgp$base_date)
panel_rows$date_pol_eff <- factor(as.character(
  base_date + panel_rows$RENW_CNT * 182 +
    sample(-dgp$date_jitter_range:dgp$date_jitter_range, N_ROWS, replace = TRUE)
))

# Ensure key relationships
panel_rows$prem_expr <- abs(panel_rows$prem_expr) + SIM_PREMIUM_FLOOR
panel_rows$prem_ern <- panel_rows$prem_expr * runif(N_ROWS, 0.8, 1.0)
panel_rows$prem_bi_ern <- panel_rows$prem_ern * runif(N_ROWS, 0.3, 0.5)
panel_rows$prem_pd_ern <- panel_rows$prem_ern * runif(N_ROWS, 0.1, 0.2)
panel_rows$prem_acq_ern <- panel_rows$prem_ern * runif(N_ROWS, 0.05, 0.1)
panel_rows$prem_opex_ern <- panel_rows$prem_ern * runif(N_ROWS, 0.02, 0.05)
panel_rows$prem_comp_ern <- panel_rows$prem_ern * runif(N_ROWS, 0.05, 0.15)
panel_rows$prem_coll_ern <- panel_rows$prem_ern * runif(N_ROWS, 0.1, 0.2)

# --- Inject latent risk into observable covariates ---
# Moderate injection so controls absorb SOME selection but leave enough
# residual for the persistent advantageous selection gap (Fig 4, t≥1).
drv_idx <- match(panel_rows$POL_ID_CHAR, pol_ids)
risk_z_panel <- drv_risk_z[drv_idx]

# tier_acci_drvr_pt: higher risk → more accident points (primary risk predictor)
# Risk injection at ~30% so controls absorb some selection but leave
# enough residual for the persistent advantageous selection gap (Fig 4, t≥1).
if ("tier_acci_drvr_pt" %in% colnames(panel_rows)) {
  orig <- panel_rows$tier_acci_drvr_pt
  mu <- mean(orig, na.rm = TRUE); s <- sd(orig, na.rm = TRUE)
  panel_rows$tier_acci_drvr_pt <- mu + s * (0.7 * scale(orig)[,1] + 0.3 * risk_z_panel)
  panel_rows$tier_acci_drvr_pt <- pmax(0, panel_rows$tier_acci_drvr_pt)
  # tier_good_ind: 1 for drivers with below-median violation points
  pt_median <- median(panel_rows$tier_acci_drvr_pt, na.rm = TRUE)
  panel_rows$tier_good_ind <- as.integer(panel_rows$tier_acci_drvr_pt <= pt_median)
  # clm_srchg: claim surcharge increases with violation points
  # Scale to match real data range (0.10-0.47) using normalized points
  pt_min <- min(panel_rows$tier_acci_drvr_pt, na.rm = TRUE)
  pt_max <- max(panel_rows$tier_acci_drvr_pt, na.rm = TRUE)
  pt_norm <- (panel_rows$tier_acci_drvr_pt - pt_min) / (pt_max - pt_min)
  panel_rows$clm_srchg <- 0.10 + 0.35 * pt_norm + rnorm(N_ROWS, 0, 0.015)
  panel_rows$clm_srchg <- pmax(0.08, pmin(0.50, panel_rows$clm_srchg))
}

# x_drvr_age_rated: younger drivers → higher risk
if ("x_drvr_age_rated" %in% colnames(panel_rows)) {
  orig <- panel_rows$x_drvr_age_rated
  mu <- mean(orig, na.rm = TRUE); s <- sd(orig, na.rm = TRUE)
  panel_rows$x_drvr_age_rated <- mu + s * (0.75 * scale(orig)[,1] - 0.25 * risk_z_panel)
  panel_rows$x_drvr_age_rated <- pmax(16, pmin(85, panel_rows$x_drvr_age_rated))
}

# x_cred_score: lower credit → higher risk
if ("x_cred_score" %in% colnames(panel_rows)) {
  orig <- panel_rows$x_cred_score
  mu <- mean(orig, na.rm = TRUE); s <- sd(orig, na.rm = TRUE)
  panel_rows$x_cred_score <- mu + s * (0.75 * scale(orig)[,1] - 0.25 * risk_z_panel)
  panel_rows$x_cred_score <- pmax(0, panel_rows$x_cred_score)
}

# x_loc_zipcd_agi: lower income → higher risk
if ("x_loc_zipcd_agi" %in% colnames(panel_rows)) {
  orig <- panel_rows$x_loc_zipcd_agi
  mu <- mean(orig, na.rm = TRUE); s <- sd(orig, na.rm = TRUE)
  panel_rows$x_loc_zipcd_agi <- pmax(1000, mu + s * (0.75 * scale(orig)[,1] - 0.25 * risk_z_panel))
}

# x_drvr_yr_edu: more education → lower risk
if ("x_drvr_yr_edu" %in% colnames(panel_rows)) {
  orig <- panel_rows$x_drvr_yr_edu
  mu <- mean(orig, na.rm = TRUE); s <- sd(orig, na.rm = TRUE)
  panel_rows$x_drvr_yr_edu <- mu + s * (0.75 * scale(orig)[,1] - 0.25 * risk_z_panel)
  panel_rows$x_drvr_yr_edu <- pmax(8, pmin(20, panel_rows$x_drvr_yr_edu))
}

# Claims driven by latent risk
lambda <- exp(drv_log_lambda[drv_idx])
panel_rows$clm_cnt <- rpois(N_ROWS, lambda)
panel_rows$clm_cnt_liab <- rbinom(N_ROWS, panel_rows$clm_cnt, dgp$claim_type_probs$liability)
panel_rows$clm_cnt_coll <- rbinom(N_ROWS, pmax(panel_rows$clm_cnt - panel_rows$clm_cnt_liab, 0L), dgp$claim_type_probs$collision)
panel_rows$clm_cnt_comp <- pmax(panel_rows$clm_cnt - panel_rows$clm_cnt_liab - panel_rows$clm_cnt_coll, 0L)
panel_rows$claim <- ifelse(panel_rows$clm_cnt > 0,
                           rlnorm(N_ROWS, dgp$severity_lognormal$collision$meanlog,
                                  dgp$severity_lognormal$collision$sdlog) * panel_rows$clm_cnt, 0)
panel_rows$claim_liab <- ifelse(panel_rows$clm_cnt_liab > 0,
                                rlnorm(N_ROWS, dgp$severity_lognormal$liability$meanlog,
                                       dgp$severity_lognormal$liability$sdlog) * panel_rows$clm_cnt_liab, 0)

# Coverage — ensure some rows have NA for COV_BI_LIM (PIP-only states) to create
# multi-level factors for st_cov_bi_ind, st_cov_pip_ind, etc.
panel_rows$COV_BI_LIM_D_NBR <- sample(dgp$coverage_distribution$values,
                                       N_ROWS, replace = TRUE,
                                       prob = dgp$coverage_distribution$probs)
panel_rows$COV_BI_LIM_CHAR <- factor(
  ifelse(panel_rows$COV_BI_LIM_D_NBR <= 100000,
         paste0(panel_rows$COV_BI_LIM_D_NBR/1000, "/", panel_rows$COV_BI_LIM_D_NBR*2/1000),
         as.character(panel_rows$COV_BI_LIM_D_NBR))
)
# ~10% of rows are PIP-only (no BI coverage), ensuring multi-level factors
pip_only_idx <- sample(N_ROWS, round(0.10 * N_ROWS))
panel_rows$COV_BI_LIM_D_NBR[pip_only_idx] <- NA
panel_rows$COV_BI_LIM_CHAR[pip_only_idx] <- NA
panel_rows$cov_ind_fullcov <- as.numeric(!is.na(panel_rows$COV_BI_LIM_D_NBR) & panel_rows$COV_BI_LIM_D_NBR >= 100000)
panel_rows$cov_mm_liab_ind <- as.integer(!is.na(panel_rows$COV_BI_LIM_D_NBR) & panel_rows$COV_BI_LIM_D_NBR <= 50000)

# UBI fields
panel_rows$ubi_rate_dvc_vers <- sample(
  c(dgp$ubi_device_distribution$values, NA),
  N_ROWS, replace = TRUE,
  prob = c(dgp$ubi_device_distribution$probs, dgp$ubi_device_distribution$na_prob)
)

# WRT_EXPS_CNT lives in panel_exps (not panel) to avoid join conflicts in c0_sum_stat

# PIP coverage — ensure PIP-only rows always have PIP values for multi-level factors
panel_rows$COV_PIP <- sample(c("10,000", "25,000", "50,000", "100,000", NA),
                              N_ROWS, replace = TRUE, prob = c(0.2, 0.3, 0.2, 0.1, 0.2))
# PIP-only rows (no BI) must have PIP coverage
panel_rows$COV_PIP[pip_only_idx] <- sample(c("10,000", "25,000", "50,000", "100,000"),
                                            length(pip_only_idx), replace = TRUE)
# ~2% of rows have BOTH BI and PIP missing (ensures cov_pip_missing has two levels)
both_missing_idx <- sample(setdiff(seq_len(N_ROWS), pip_only_idx), round(0.02 * N_ROWS))
panel_rows$COV_BI_LIM_D_NBR[both_missing_idx] <- NA
panel_rows$COV_BI_LIM_CHAR[both_missing_idx] <- NA
panel_rows$COV_PIP[both_missing_idx] <- NA

# Deductibles
panel_rows$COV_COLL_DED_NBR_50 <- sample(c(250, 500, 1000, NA), N_ROWS, replace = TRUE,
                                           prob = c(0.2, 0.4, 0.2, 0.2))
panel_rows$COV_COMP_DED_NBR_50 <- sample(c(100, 250, 500, NA), N_ROWS, replace = TRUE,
                                           prob = c(0.3, 0.4, 0.1, 0.2))

# Premium components needed by c5_selection_figures.R
panel_rows$prem_liab_ern <- panel_rows$prem_bi_ern + panel_rows$prem_pd_ern
panel_rows$prem_fee_ern  <- panel_rows$prem_ern * runif(N_ROWS, 0.01, 0.03)

# prem_bi_wrt
panel_rows$prem_bi_wrt <- panel_rows$prem_bi_ern * runif(N_ROWS, 0.9, 1.1)

# Derived columns not in profile but needed by getX("estimation"/"reg")
# x_drvr_lic_oos_ind: 1 if license state differs from policy state (~10% out-of-state)
panel_rows$x_drvr_lic_oos_ind <- rbinom(N_ROWS, 1, 0.10)
# x_loc_zipcd_lg_inc: log zipcode income (used by getX("reg"))
panel_rows$x_loc_zipcd_lg_inc <- rnorm(N_ROWS, 10.5, 0.8)

# Columns needed by panel_renw_clean() invariant_X_next_panel join (from panel)
# Some of these may already exist via gen_col from profile; add if missing
if (!"x_loc_grg_adrs_verify_ind" %in% colnames(panel_rows))
  panel_rows$x_loc_grg_adrs_verify_ind <- rbinom(N_ROWS, 1, 0.5)
if (!"x_loc_popltn_dens_grp" %in% colnames(panel_rows))
  panel_rows$x_loc_popltn_dens_grp <- sample(0:9, N_ROWS, replace = TRUE)
if (!"x_hh_hlth_ins_ind" %in% colnames(panel_rows))
  panel_rows$x_hh_hlth_ins_ind <- rbinom(N_ROWS, 1, 0.5)
if (!"tier_pop_cont_ins_ind" %in% colnames(panel_rows))
  panel_rows$tier_pop_cont_ins_ind <- rbinom(N_ROWS, 1, 0.7)
if (!"tier_pop_no_ind" %in% colnames(panel_rows))
  panel_rows$tier_pop_no_ind <- rbinom(N_ROWS, 1, 0.1)
if (!"tier_pop_not_over_min_ind" %in% colnames(panel_rows))
  panel_rows$tier_pop_not_over_min_ind <- rbinom(N_ROWS, 1, 0.3)
if (!"tier_pop_limit" %in% colnames(panel_rows))
  panel_rows$tier_pop_limit <- sample(c(0, 1, 2, 3), N_ROWS, replace = TRUE, prob = c(0.4, 0.3, 0.2, 0.1))
if (!"tier_pop_prog_lim" %in% colnames(panel_rows))
  panel_rows$tier_pop_prog_lim <- sample(c(0, 1, 2), N_ROWS, replace = TRUE, prob = c(0.5, 0.3, 0.2))
if (!"tier_pop_prir_liab_lim" %in% colnames(panel_rows))
  panel_rows$tier_pop_prir_liab_lim <- runif(N_ROWS, 0, 5)
if (!"tier_pop_prir_liab_lim_missing" %in% colnames(panel_rows))
  panel_rows$tier_pop_prir_liab_lim_missing <- rbinom(N_ROWS, 1, 0.2)

# Coverage factor variables needed by c3_demand_elast.R
cov_bi_levels <- c("lev_40", "lev_50", "lev_100", "lev_150", "lev_300")
panel_rows$cov_lim_ftr_bi <- factor(
  ifelse(is.na(panel_rows$COV_BI_LIM_D_NBR), NA,
         paste0("lev_", panel_rows$COV_BI_LIM_D_NBR/1000)),
  levels = cov_bi_levels
)
panel_rows$cov_lim_ftr_coll <- factor(
  sample(c("ded_250", "ded_500", "ded_1000", NA), N_ROWS, replace = TRUE, prob = c(0.3, 0.4, 0.2, 0.1))
)
panel_rows$cov_lim_ftr_comp <- factor(
  sample(c("ded_100", "ded_250", "ded_500", NA), N_ROWS, replace = TRUE, prob = c(0.3, 0.4, 0.2, 0.1))
)
panel_rows$cov_lim_ftr_pd <- factor(
  sample(c("lev_25", "lev_50", "lev_100"), N_ROWS, replace = TRUE, prob = c(0.3, 0.4, 0.3))
)

panel <- panel_rows

panel_il <- panel %>% filter(state_alpha == "IL")
cat("  Panel:", N_ROWS, "rows\n")

## ---- 2. Generate df_mh (with causal MH + selection structure) ---------------
cat("Generating df_mh_us...\n")

# df_mh: one row per policy at RENW_CNT=0, with future claim columns
df_mh_base <- panel %>%
  filter(RENW_CNT == 0) %>%
  select(POL_ID_CHAR, last_renewal_seen, state_alpha, ST_CD)

n_mh <- nrow(df_mh_base)

# Map driver-level latent variables to df_mh rows
mh_drv_idx <- match(df_mh_base$POL_ID_CHAR, pol_ids)

# Monitoring assignment with ADVERSE SELECTION (from driver-level draws)
df_mh_base$ubi_fin_ind   <- drv_ubi_fin[mh_drv_idx]
df_mh_base$ubi_start_ind <- pmax(df_mh_base$ubi_fin_ind,
                                drv_ubi_optin[mh_drv_idx])
df_mh_base$ubi_end_due_to_claim <- rbinom(n_mh, 1, 0.02) * df_mh_base$ubi_fin_ind
# days_connect_raw: monitoring duration in days (real data moments: mean~128, sd~41)
# Make it correlate with risk so selection patterns are preserved
mh_risk_z <- drv_risk_z[mh_drv_idx]
df_mh_base$days_connect_raw <- ifelse(df_mh_base$ubi_fin_ind > 0,
                                       pmax(10, pmin(182, rnorm(n_mh, 128, 41) + 10 * mh_risk_z)), NA)

# ubi_factor_0/1 as monitoring duration fractions (mirrors get_mh_df.R:205-206)
# Pre-renewal1 days ≈ most of monitoring; post-renewal1 days ≈ small remainder
days_raw <- df_mh_base$days_connect_raw
days_pre  <- ifelse(is.na(days_raw), NA, pmin(days_raw, 182))
days_post <- ifelse(is.na(days_raw), NA, pmax(days_raw - 182, 0))
df_mh_base$ubi_factor_0 <- ifelse(is.na(days_pre), 0, days_pre / 182)
df_mh_base$ubi_factor_1 <- ifelse(is.na(days_post), 0, pmin(days_post / 182, 1))

# --- Claims with MORAL HAZARD at t=0 and SELECTION ---
# Each driver's claim rate is driven by their latent risk
drv_lambda_mh <- drv_log_lambda[mh_drv_idx]

# t=0 (monitoring period): MH effect reduces claims for monitored drivers
# Scale MH effect by treat_int so interaction (post × z) captures differential rebound
treat_int <- df_mh_base$ubi_factor_0 - df_mh_base$ubi_factor_1
treat_int_norm <- treat_int / mean(treat_int[df_mh_base$ubi_fin_ind == 1], na.rm = TRUE)
lambda_t0 <- exp(drv_lambda_mh + MH_EFFECT * df_mh_base$ubi_fin_ind * treat_int_norm)
df_mh_base$clm_acci <- rpois(n_mh, lambda_t0)

# t=1..5 (post-monitoring): NO MH effect — claims return to baseline
# This produces positive DID interaction (monitored claims rise post-monitoring)
for (q in 1:5) {
  lambda_tq <- exp(drv_lambda_mh)  # baseline risk only, no MH
  col_name <- paste0("clm_acci_renw_", q)
  val <- rpois(n_mh, lambda_tq)
  val[df_mh_base$last_renewal_seen < q] <- NA
  df_mh_base[[col_name]] <- val
}

df_mh <- df_mh_base
cat("  df_mh:", n_mh, "rows,", sum(df_mh$ubi_fin_ind), "monitored,",
    round(mean(df_mh$clm_acci[df_mh$ubi_fin_ind==1], na.rm=TRUE), 4), "vs",
    round(mean(df_mh$clm_acci[df_mh$ubi_fin_ind==0], na.rm=TRUE), 4), "claim rate (mon vs unmon)\n")

## ---- 3. Generate panel_viol -----------------------------------------------
cat("Generating panel_viol...\n")

# 3-state subset for violation analysis
panel_viol <- panel %>%
  select(POL_ID_CHAR, RENW_CNT, state_alpha)

n_viol <- nrow(panel_viol)

# Look up driver-level risk and monitoring status for each violation row
drv_idx_viol <- match(panel_viol$POL_ID_CHAR, pol_ids)
viol_ubi_fin <- drv_ubi_fin[drv_idx_viol]
viol_risk_z <- drv_risk_z[drv_idx_viol]
viol_renw <- panel_viol$RENW_CNT

# Violation indicators (lag periods 1-6) — risk-adjusted with monitoring moral hazard
for (v_type in c("AAF", "AAF_MINOR", "DWI", "DWI_MAJ", "SPD", "NAF")) {
  base_prob <- switch(v_type,
    AAF = 0.004, AAF_MINOR = 0.002, DWI = 0.001,
    DWI_MAJ = 0.0004, SPD = 0.01, NAF = 0.006
  )
  for (lag in 1:6) {
    col_name <- paste0("VIOL_", v_type, "_", lag)
    # Risk-adjusted base probability
    p <- plogis(qlogis(base_prob) + 0.3 * viol_risk_z)
    # Monitoring moral hazard: fewer violations during monitoring (renw 1-2),
    # partial rebound after (renw 3+)
    mon_effect <- ifelse(viol_ubi_fin == 1 & viol_renw %in% 1:2, 0.6,
                  ifelse(viol_ubi_fin == 1 & viol_renw >= 3, 0.9, 1.0))
    p <- p * mon_effect
    panel_viol[[col_name]] <- rbinom(n_viol, 1, pmin(p, 1))
  }
}

# DRVR_YR_LIC
panel_viol$DRVR_YR_LIC <- sample(c(1, 2, 3, NA), n_viol, replace = TRUE,
                                   prob = c(0.3, 0.3, 0.2, 0.2))

# panel_viol will be written as data_rf_viol.csv in consolidation step
cat("  panel_viol:", n_viol, "rows\n")

## ---- 4. Generate panel_renw (IL) — renewal responds to price ----------------
cat("Generating panel_renw_IL...\n")

panel_renw_base <- panel_il %>%
  select(POL_ID_CHAR, RENW_CNT, prem_expr, prem_bi_wrt, prem_bi_ern, state_alpha,
         ST_CD, cov_ind_fullcov)

n_renw <- nrow(panel_renw_base)
renw_drv_idx <- match(panel_renw_base$POL_ID_CHAR, pol_ids)

# Rate revision timing — creates the instrument for demand elasticity
# In real data, rate revisions are filed at fixed calendar dates per state.
# Policies have varied effective dates, so some fall just after a revision
# (post window) and others just before the next (pre window).
# This pre/post variation is the instrument for 2SLS.

# Fixed calendar rate revision dates: ~every 3 months over the observation period
# More frequent revisions = more event study groups = stronger first stage
rrev_dates <- as.Date("2015-01-01") + cumsum(c(0, rep(c(80, 100, 85, 95), 5)))
n_rrevs <- length(rrev_dates)

# Policy effective dates spread throughout the observation period
renw_pol_eff_dt <- as.Date("2015-01-01") + panel_renw_base$RENW_CNT * 182 +
                   sample(0:180, n_renw, replace = TRUE)

# Assign each policy to the most recent preceding rate revision
renw_rrev_id <- findInterval(as.numeric(renw_pol_eff_dt), as.numeric(rrev_dates))
renw_rrev_id <- pmax(1, pmin(renw_rrev_id, n_rrevs))
renw_rt_rev_dt <- rrev_dates[renw_rrev_id]

# Premium at renewal: base premium * (1 + rate_revision_shock + idiosyncratic_noise)
# Rate revision shocks are the excluded instrument: affect price but not renewal directly
rrev_shocks <- rnorm(n_rrevs, 0, 0.04)  # ±4% rate revision shocks (instrument)
renw_prem_amt <- panel_renw_base$prem_expr *
  (1 + rrev_shocks[renw_rrev_id] + rnorm(n_renw, 0.02, 0.08))

# Renewal decision depends on price change (creates demand elasticity)
renw_is_mon <- drv_ubi_optin[renw_drv_idx]
renw_is_fin <- drv_ubi_fin[renw_drv_idx]
renw_risk_z <- drv_risk_z[renw_drv_idx]

# Monitoring discount factor for renewal premiums (fig_2b):
# In real data, the insurer's quote already includes the monitoring discount.
# The analysis script divides by tm_disc_ftr to remove it, then re-applies.
# So simulated premiums must also bake in the discount for monitored finishers.
# Monitoring discount reflects noisy DRIVING BEHAVIOR during monitoring,
# not just latent risk. High noise ensures reasonable spread across segments:
#   tm_segment_ftr = 1: disc >= 10% (tm_ftr <= 0.9)
#   tm_segment_ftr = -1: surcharge (tm_ftr > 1.0, disc < 0%)
#   tm_segment_ftr = 0: small disc or unmonitored
# Even low-risk drivers sometimes drive poorly → surcharge; high-risk sometimes
# drive well → discount. This mirrors real monitoring score variability.
# Draw initial discount per driver, then carry forward with small perturbation
# (real data shows discounts persist at ~100% of initial level across renewals)
renw_drv_ids <- panel_renw_base$POL_ID_CHAR
unique_drv <- unique(renw_drv_ids)
drv_base_disc <- 5.0 - 3.0 * drv_risk_z[match(unique_drv, pol_ids)] +
  rnorm(length(unique_drv), 0, 8.0)
drv_base_disc <- pmax(-15, pmin(30, drv_base_disc))
renw_tm_disc <- drv_base_disc[match(renw_drv_ids, unique_drv)] +
  rnorm(n_renw, 0, 1.0)  # small per-renewal perturbation
renw_tm_disc <- pmax(-15, pmin(30, renw_tm_disc))
# Round to integer percentages — real UBI discounts are discrete tiers,
# and factor(tm_ftr) in c3_demand_elast.R needs manageable # of levels
renw_tm_disc <- round(renw_tm_disc)
renw_tm_disc_ftr <- 1 - renw_tm_disc / 100
renw_prem_amt <- ifelse(renw_is_fin == 1,
                        renw_prem_amt * renw_tm_disc_ftr,
                        renw_prem_amt)

# Compute approximate log price change
lg_prem_chg <- log(renw_prem_amt) - log(panel_renw_base$prem_expr)

# Decompose into undiscounted price change + discount factor
# This matches what c3_demand_elast.R constructs as prem_renw_all = prem_renw_amt / tm_ftr
lg_tm_ftr <- ifelse(renw_is_fin == 1, log(renw_tm_disc_ftr), 0)
lg_prem_increase <- lg_prem_chg - lg_tm_ftr  # undiscounted log price change

## Demand elasticities by monitoring segment (from benchmark fig_5 IV estimates):
##   monitored w/ discount:  IV ≈ -0.38 (least elastic)
##   unmonitored:            IV ≈ -0.60
##   monitored w/o discount: IV ≈ -1.28 (most elastic)
## These are hardcoded because c3_demand_elast.R does not save estimates to file.
# Elasticity by monitoring group (matches tm_segment_ftr classification):
#   tm_segment_ftr = 1:  monitored w/ discount (tm_ftr <= 0.9, large disc >= 10%)
#   tm_segment_ftr = -1: monitored w/o discount (tm_ftr > 1.0, surcharge)
#   tm_segment_ftr = 0:  unmonitored (tm_ftr == 1)
# Align elasticity with the ACTUAL tm_segment_ftr classification used by
# c3_demand_elast.R, not risk_z. The analysis classifies by discount value:
#   tm_segment_ftr =  1: tm_ftr <= 0.9 → disc >= 10 (monitored w/ discount)
#   tm_segment_ftr = -1: tm_ftr > 1.0  → disc < 0   (monitored w/o discount)
#   tm_segment_ftr =  0: unmonitored or small disc   (baseline)
# Using risk_z caused misalignment because noisy discounts scramble the mapping.
renw_elast <- rep(dgp$demand_elasticities$unmonitored, n_renw)  # unmonitored + small disc baseline
renw_elast[renw_is_fin == 1 & renw_tm_disc >= 10] <- dgp$demand_elasticities$monitored_discount
renw_elast[renw_is_fin == 1 & renw_tm_disc < 0] <- dgp$demand_elasticities$monitored_surcharge

# DGP matches regression specification in c3_demand_elast.R:
#   renewed ~ β₁ × lg_prem_increase × tm_segment + β₂ × log(tm_ftr) + controls
# Group-specific elasticity on undiscounted price change,
# UNIFORM discount retention effect (same coefficient for all groups)
gamma_disc <- dgp$renewal_dgp$gamma_disc
p_renew <- plogis(dgp$renewal_dgp$intercept +
                   renw_elast * lg_prem_increase * dgp$renewal_dgp$elast_scale +
                   gamma_disc * lg_tm_ftr + rnorm(n_renw, 0, dgp$renewal_dgp$noise_sd))
renw_acpt <- ifelse(runif(n_renw) < p_renew, "Y", "N")

# Rate revision dates — use raw RENW_QT_ column names so panel_renw_clean() can process them
panel_renw <- panel_renw_base %>%
  mutate(
    RENW_QT_POL_EFF_DT = renw_pol_eff_dt,
    RENW_QT_RATE_REV_ID = renw_rrev_id,
    RENW_QT_ALPHA_ST_CD = "IL",
    RENW_QT_RT_REV_DT = renw_rt_rev_dt,
    RENW_QT_RENW_QT_PREM_AMT = renw_prem_amt,
    RENW_QT_PREV_TOT_PREM_EXPR = prem_expr,
    RENW_QT_RENW_ACPT_IND = renw_acpt,
    RENW_QT_RENW_LPS_IND = ifelse(renw_acpt == "N", sample(c("Y","N"), n_renw, replace=TRUE, prob=c(0.6,0.4)), "N"),
    RENW_SFX_NBR = RENW_CNT + 1,
    RENW_QT_POL_ID_CHAR = POL_ID_CHAR,
    # Premium change columns
    RENW_QT_ORGN_PREM_CHNG_PCT = rnorm(n_renw, 2, 5),
    RENW_QT_RENW_PREM_CHNG_AMT = RENW_QT_ORGN_PREM_CHNG_PCT / 100 * prem_expr,
    RENW_QT_ORGN_PREM_CHNG_AMT = RENW_QT_RENW_PREM_CHNG_AMT,
    RENW_QT_RATE_CHNG_PCT = rnorm(n_renw, 1, 3),
    # Driver demographics (raw RENW_QT_ names for panel_renw_clean)
    RENW_QT_DRVR_SEX_CD = sample(c("M", "F"), n_renw, replace = TRUE),
    RENW_QT_DRVR_RATE_ON_AGE = sample(20:70, n_renw, replace = TRUE),
    RENW_QT_DRVR_MRTL_STAT_CD = sample(c("S", "M", NA), n_renw, replace = TRUE, prob = c(0.4, 0.4, 0.2)),
    RENW_QT_MAX_ED_CD = sample(c("1","2","3","4","5","6","7"), n_renw, replace = TRUE),
    RENW_QT_OWN_HM_IND = sample(c("O", "R"), n_renw, replace = TRUE),
    RENW_QT_HOP_VER_CD = sample(c("Y", "V", "N"), n_renw, replace = TRUE),
    RENW_QT_DRVR_DRVR_LIC_ST_CD = sample(c("IL", "IL", "IL", "OH", "IN"), n_renw, replace = TRUE),
    RENW_QT_DRVR_OCC_CD = sample(c("A","B","C","D","X",NA), n_renw, replace = TRUE),
    RENW_QT_DRVR_DRVR_REL_INS_CD = sample(c("I","S","C",NA), n_renw, replace = TRUE),
    RENW_QT_VEH_VEH_FULL_COV_CD = sample(c("O","1"), n_renw, replace = TRUE),
    RENW_QT_DRVR_EMP_STAT_CD = sample(c("E","U","R","S",NA), n_renw, replace = TRUE),
    RENW_QT_CRED_SCORE_STAT_CD = sample(c("O", "N"), n_renw, replace = TRUE),
    RENW_QT_NEW_CRED_SCORE_NBR = sample(c(0, 200:400, 9999), n_renw, replace = TRUE),
    RENW_QT_CRED_SCORE_CD = sample(c("A","B","C","D","XX"), n_renw, replace = TRUE),
    RENW_QT_VEH_MODL_YR_DT = sample(2000:2020, n_renw, replace = TRUE),
    RENW_QT_HH_STRUCT_IND = sample(c("1","2","3",NA), n_renw, replace = TRUE),
    RENW_QT_HSHLD_AVG_CD = sample(c("A","B","C"), n_renw, replace = TRUE),
    RENW_QT_PERS_USE_OCC_CD = sample(c("C","P","X",NA), n_renw, replace = TRUE),
    RENW_QT_MKT = sample(c("PR","UL","NS","ST","MM"), n_renw, replace = TRUE, prob = c(0.3,0.2,0.2,0.2,0.1)),
    RENW_QT_TENURE_CS_XPND = sample(0:10, n_renw, replace = TRUE),
    RENW_QT_LNGTH_PRIR_INS_CD = sample(c("A","B","C","D","N","X"), n_renw, replace = TRUE),
    RENW_QT_LNGTH_RSD_CD = sample(c("A","B","N","X"), n_renw, replace = TRUE),
    RENW_QT_PRIR_CARY_TYP = sample(c("S","N",NA), n_renw, replace = TRUE),
    RENW_QT_DRVR_DRVR_PNT_CNT = sample(0:10, n_renw, replace = TRUE, prob = c(0.6,0.1,0.1,0.05,0.03,0.02,rep(0.02,5))),
    RENW_QT_DRVR_DRVR_BIPD_PNT_CNT = sample(0:5, n_renw, replace = TRUE, prob = c(0.7,0.1,0.1,0.05,0.03,0.02)),
    RENW_QT_DRVR_DRVR_COLL_PNT_CNT = sample(0:3, n_renw, replace = TRUE, prob = c(0.8,0.1,0.05,0.05)),
    RENW_QT_DRVR_DRVR_COMP_PNT_CNT = sample(0:3, n_renw, replace = TRUE, prob = c(0.85,0.08,0.04,0.03)),
    RENW_QT_TOT_NAF_VIOL_CNT = sample(c(0,1,2,NA), n_renw, replace = TRUE, prob = c(0.7,0.15,0.05,0.1)),
    RENW_QT_INCPT_AAF_CNT = sample(c(0,1,2,NA), n_renw, replace = TRUE, prob = c(0.8,0.1,0.03,0.07)),
    RENW_QT_NBR_MO_CLN = sample(0:60, n_renw, replace = TRUE),
    RENW_QT_AAF_MO_CLN = sample(c(0:60,NA), n_renw, replace = TRUE),
    RENW_QT_NBR_OF_PAY = sample(1:6, n_renw, replace = TRUE),
    RENW_QT_PREM_WAS_CAP_THIS_TERM = sample(c(0,1,NA), n_renw, replace = TRUE, prob = c(0.7,0.2,0.1)),
    RENW_QT_RT_CAP_PCT = sample(c(0,5,10,NA), n_renw, replace = TRUE),
    RENW_QT_RT_CAP_COMP_CD = sample(c(0,1,NA), n_renw, replace = TRUE),
    RENW_QT_ACQST_EXP_CAP_PCT = sample(c(0,5,NA), n_renw, replace = TRUE),
    RENW_QT_OPS_EXP_RT_STBL_FCT = sample(c(0,1,NA), n_renw, replace = TRUE),
    RENW_QT_CNVRG_RT_STBL_FCT = runif(n_renw, 0.9, 1.1),
    RENW_QT_POL_RT_STBL_FCT = runif(n_renw, 0.9, 1.1),
    RENW_QT_REQ_DWNPMT_PCT = sample(c(0,10,20,NA), n_renw, replace = TRUE),
    RENW_QT_EFT_INCP_IND = sample(c("Y","N",NA), n_renw, replace = TRUE),
    RENW_QT_EFT_IND = sample(c("Y","N",NA), n_renw, replace = TRUE),
    RENW_QT_DED_SAV_BANK_IND = sample(c("Y","N",NA), n_renw, replace = TRUE),
    RENW_QT_DED_SAV_AMT = sample(c(0,100,500,NA), n_renw, replace = TRUE),
    RENW_QT_VEH_VIN_NBR = paste0("VIN", sprintf("%05d", sample(1:99999, n_renw, replace = TRUE))),
    RENW_QT_PPRO_IND = sample(c("Y","N"), n_renw, replace = TRUE),
    RENW_QT_INIT_QT_EFF_DT = RENW_QT_POL_EFF_DT - sample(30:60, n_renw, replace = TRUE),
    RENW_QT_LAST_CRED_ORDR_DT = RENW_QT_POL_EFF_DT - sample(60:180, n_renw, replace = TRUE),
    RENW_QT_CO_CD = sample(c("01","02","03"), n_renw, replace = TRUE),
    RENW_QT_LST_PM_TRANS_DT = RENW_QT_POL_EFF_DT + sample(0:10, n_renw, replace = TRUE),
    RENW_QT_SCND_CNVRG_EVNT_DT = RENW_QT_POL_EFF_DT + sample(0:30, n_renw, replace = TRUE),
    RENW_QT_SCORE_MTHD_CD = sample(c("A","B","C"), n_renw, replace = TRUE),
    RENW_QT_VEH_GRG_PSTL_CD = sprintf("%05d", sample(60000:62999, n_renw, replace = TRUE)),
    GRG_PSTL_CD = RENW_QT_VEH_GRG_PSTL_CD,
    RENW_QT_RWRT_RSN_CD = sample(c("N",NA), n_renw, replace = TRUE),
    RENW_QT_POL_TERM_NBR = sample(1:10, n_renw, replace = TRUE),
    RENW_QT_QT_ISS_DT = RENW_QT_POL_EFF_DT - sample(20:40, n_renw, replace = TRUE),
    RENW_QT_QT_CHNG_DT = RENW_QT_POL_EFF_DT - sample(5:15, n_renw, replace = TRUE),
    RENW_QT_PREV_POL_EXPR_DT = RENW_QT_POL_EFF_DT - 182,
    RENW_QT_POL_EXPR_DT = RENW_QT_POL_EFF_DT + 182,
    RENW_QT_CUST_SINCE_DT = RENW_QT_POL_EFF_DT - sample(365:3650, n_renw, replace = TRUE),
    RENW_QT_COH_INCP_DT = RENW_QT_POL_EFF_DT - sample(365:1825, n_renw, replace = TRUE),
    RENW_QT_QT_EFF_DT = RENW_QT_POL_EFF_DT - sample(0:5, n_renw, replace = TRUE),
    RENW_QT_RENW_ACPT_DT = RENW_QT_POL_EFF_DT - sample(0:10, n_renw, replace = TRUE),
    RENW_QT_UPLD_FR_MKT = runif(n_renw, 0.8, 1.2),
    RENW_QT_UPLD_UND_MKT = runif(n_renw, 0.8, 1.2),
    REQ_DWNPMT_PCT = sample(c(0,10,20,NA), n_renw, replace = TRUE),
    PRIOR_CARRIER_MIX_GROUP = sample(c("Standard","Non-Standard","Unknown"), n_renw, replace = TRUE),
    PRIOR_CARRIER_GROUP = sample(c("Group_A","Group_B","Group_C"), n_renw, replace = TRUE),
    PRIOR_CARRIER_NAME = sample(c("Carrier1","Carrier2","Carrier3"), n_renw, replace = TRUE)
  )

# Save renewal-level discounts for consistency with panel_ubi_renw (Fix for Fig 5).
# UBI_DISC_FINAL must match the discount already baked into renw_prem_amt,
# otherwise prem_renw_amt / tm_disc_ftr introduces multiplicative noise.
renw_disc_lookup <- data.frame(
  POL_ID_CHAR = panel_renw_base$POL_ID_CHAR,
  RENW_CNT = panel_renw_base$RENW_CNT,
  renw_tm_disc = renw_tm_disc,
  stringsAsFactors = FALSE
)

# panel_renw will be split into data_rf (demographics) + data_rf_rrev (IV) in consolidation step
cat("  panel_renw:", n_renw, "rows\n")

## ---- 6. Generate panel_ubi_renw (IL) — score correlates with risk -----------
cat("Generating panel_ubi_renw_IL...\n")

# Only for policies that had monitoring (opt-in or finisher)
monitored_pols <- df_mh %>%
  filter(ubi_start_ind > 0 & state_alpha == "IL") %>%
  pull(POL_ID_CHAR)

panel_ubi_base <- panel_il %>%
  filter(POL_ID_CHAR %in% monitored_pols)

n_ubi <- nrow(panel_ubi_base)
if (n_ubi > 0) {
  ubi_drv_idx <- match(panel_ubi_base$POL_ID_CHAR, pol_ids)
  ubi_risk_z <- drv_risk_z[ubi_drv_idx]

  # UBI_VALUE_MAX (monitoring score) correlates with RISK:
  # Higher risk → higher score → higher subsequent claims (Fig 3 pattern)
  # Strong correlation (1.2) ensures strictly monotonic quintile means
  ubi_score_raw <- dgp$score_params$intercept + dgp$score_params$risk_coef * ubi_risk_z +
    rnorm(n_ubi, 0, dgp$score_params$noise_sd)
  ubi_score_raw <- pmax(dgp$score_params$lb, pmin(dgp$score_params$ub, ubi_score_raw))

  # UBI_DISC_FINAL must match the discount already baked into renewal premiums.
  # Look up the same renw_tm_disc values used in panel_renw generation (line ~536).
  # This ensures prem_renw_amt / tm_disc_ftr correctly recovers undiscounted premium in Fig 5.
  ubi_disc_match <- merge(
    panel_ubi_base[, c("POL_ID_CHAR", "RENW_CNT")],
    renw_disc_lookup,
    by = c("POL_ID_CHAR", "RENW_CNT"),
    all.x = TRUE,
    sort = FALSE
  )
  # Reorder to match panel_ubi_base row order (merge may reorder)
  ubi_disc_match <- ubi_disc_match[match(
    paste(panel_ubi_base$POL_ID_CHAR, panel_ubi_base$RENW_CNT),
    paste(ubi_disc_match$POL_ID_CHAR, ubi_disc_match$RENW_CNT)
  ), ]
  # Fall back to independent draw for any unmatched rows
  ubi_disc <- ifelse(!is.na(ubi_disc_match$renw_tm_disc),
                     ubi_disc_match$renw_tm_disc,
                     round(dgp$discount_params$intercept - dgp$discount_params$risk_coef * ubi_risk_z +
                           rnorm(n_ubi, 0, dgp$discount_params$noise_sd)))
  ubi_disc <- pmax(dgp$discount_params$lb, pmin(dgp$discount_params$ub, ubi_disc))

  panel_ubi_renw <- panel_ubi_base %>%
    select(POL_ID_CHAR, RENW_CNT) %>%
    mutate(
      RENW_SFX_NBR = RENW_CNT + 1,
      UBI_ENROLL_IND = 1,
      UBI_FIN_IND = drv_ubi_fin[ubi_drv_idx],
      UBI_VALUE_MAX = ubi_score_raw,
      UBI_DISC_FINAL = ubi_disc,
      UBI_DISC_30DAY = ifelse(rbinom(n_ubi, 1, 0.3) > 0,
                               pmax(0, ubi_disc * runif(n_ubi, 0.3, 0.7)), 0)
    )
} else {
  panel_ubi_renw <- data.frame(
    POL_ID_CHAR = character(0), RENW_CNT = integer(0),
    RENW_SFX_NBR = integer(0), UBI_ENROLL_IND = numeric(0),
    UBI_FIN_IND = numeric(0), UBI_VALUE_MAX = numeric(0),
    UBI_DISC_FINAL = numeric(0), UBI_DISC_30DAY = numeric(0)
  )
}

# panel_ubi_renw will be merged into data_rf in consolidation step
cat("  panel_ubi_renw:", n_ubi, "rows\n")

## ---- 7. Generate panel_exps -----------------------------------------------
cat("Generating panel_exps_us...\n")

panel_exps <- panel %>%
  select(POL_ID_CHAR, RENW_CNT) %>%
  mutate(WRT_EXPS_CNT = sample(1:6, nrow(panel), replace = TRUE,
                                prob = c(0.05, 0.05, 0.05, 0.05, 0.1, 0.7)))

# panel_exps will be merged into data_rf in consolidation step
cat("  panel_exps:", nrow(panel_exps), "rows\n")

## ---- 8. Generate UBI date files -------------------------------------------
cat("Generating UBI date files...\n")

# Stagger UBI introduction dates across states so RD design in
# c5_selection_figures.R has both pre and post periods
n_states <- length(states)
# With base_date=2012-01-01 and 182d/renewal, obs window is ~2012-01 to 2014-07.
# Use tight 7-day stagger so all states (including IL at index 9) have intro dates
# well within the observation window, creating balanced pre/post periods.
ubi_start_dates <- as.Date(dgp$ubi_intro$start_date) + seq(0, by = dgp$ubi_intro$stagger_days, length.out = n_states)
st_ubi_dates <- data.frame(
  ST_CD = state_map[states],
  state_alpha = states,
  ubi_start_date = ubi_start_dates,
  min_ubi_1 = ubi_start_dates,
  min_ubi_3 = ubi_start_dates + 365 * 2.5,
  stringsAsFactors = FALSE
)
# st_ubi_dates will be merged into data_rf in consolidation step

# ubi_vers_st_dt_sum
ubi_vers_st_dt_sum <- expand.grid(
  ST_CD = unique(st_ubi_dates$ST_CD),
  date_yr = 2012:2020,
  date_mon = 1:12,
  stringsAsFactors = FALSE
) %>%
  mutate(
    ubi_vers_st = sample(1:3, n(), replace = TRUE),
    ubi_vers_st_max = 3,
    ubi_vers_st_min = 1
  )
# ubi_vers_st_dt_sum will be written as ubi_vers_dates.csv in consolidation step
cat("  UBI dates generated\n")

## ---- 9. Generate processed files (with score-claim correlation) -------------
cat("Generating processed files...\n")

# choice_panel_us (key processed dataset for RF)
n_choice <- N_ROWS  # include all policies for stable quintile estimates in Fig 3
choice_panel <- panel[1:n_choice, ]
cp_drv_idx <- match(choice_panel$POL_ID_CHAR, pol_ids)

# Driver-level monitoring assignments (preserved for df_mh extraction at RENW_CNT=0)
choice_panel$ubi_fin_ind_drv <- drv_ubi_fin[cp_drv_idx]
choice_panel$ubi_ind_drv <- drv_ubi_optin[cp_drv_idx]

# Time-dependent monitoring: zero out for policies before their state's introduction
# This creates the step-change discontinuity needed for Fig B1a
choice_panel$ubi_ind <- drv_ubi_optin[cp_drv_idx]
choice_panel$ubi_fin_ind <- drv_ubi_fin[cp_drv_idx]
cp_dates <- as.Date(as.character(choice_panel$date_pol_eff))
cp_st <- choice_panel$ST_CD
for (r in seq_len(nrow(st_ubi_dates))) {
  pre_mask <- cp_st == st_ubi_dates$ST_CD[r] & cp_dates < st_ubi_dates$min_ubi_1[r]
  choice_panel$ubi_fin_ind[pre_mask] <- 0L
  choice_panel$ubi_ind[pre_mask] <- 0L
}

choice_panel$ubi_group <- factor(ifelse(choice_panel$ubi_ind == 0, "no_ubi",
                                         ifelse(choice_panel$ubi_fin_ind == 1, "ubi_fin", "ubi_nofin")))

# Columns required by clean_choice_panel()
choice_panel$prem_renw_qt_1_amt <- choice_panel$prem_expr * runif(n_choice, 0.95, 1.05)

# Score-risk correlation: higher risk → higher score value (Fig 3)
# Strong correlation (1.2) ensures strictly monotonic quintile claims
cp_risk_z <- drv_risk_z[cp_drv_idx]
cp_score_val <- 4.5 + 1.0 * cp_risk_z + rnorm(n_choice, 0, 0.8)
cp_score_val <- pmax(0.5, pmin(8.0, cp_score_val))

# Use driver-level (not time-dependent) monitoring indicators for score columns.
# Time-dependent ubi_fin_ind is zeroed for pre-introduction rows (for Fig B1a),
# but scores are a driver attribute — using ubi_fin_ind_drv ensures clean_choice_panel()
# picks up score info even at RENW_CNT=0, which is needed for Fig 3 quintiles.
choice_panel$ubi_sum_val_final_max <- ifelse(choice_panel$ubi_fin_ind_drv == 1, cp_score_val, 0)
choice_panel$UbiValueNbr <- ifelse(choice_panel$ubi_ind_drv == 1, cp_score_val, NA)
choice_panel$UbiScoreNbr <- ifelse(choice_panel$ubi_ind_drv == 1,
                                    pmax(0, pmin(100, 50 + 20 * cp_risk_z + rnorm(n_choice, 0, 5))),
                                    NA)

# Discount inversely correlates with risk
cp_disc <- 5.0 - 4.0 * cp_risk_z + rnorm(n_choice, 0, 2.0)
cp_disc <- pmax(-5, pmin(20, cp_disc))
choice_panel$ubi_sum_disc_final_max <- ifelse(choice_panel$ubi_fin_ind_drv == 1, cp_disc, 0)
choice_panel$ubi_sum_tier_final_max <- ifelse(choice_panel$ubi_fin_ind_drv == 1, sample(1:5, n_choice, replace = TRUE), 0)
choice_panel$ubi_renw_1_tier_final <- ifelse(choice_panel$ubi_fin_ind_drv == 1, sample(1:5, n_choice, replace = TRUE), NA)
# Status columns use TIME-DEPENDENT indicators (not driver-level) because
# clean_choice_panel() re-derives ubi_fin_ind from ubi_renw_1_stat_fin.
# Pre-introduction rows must have NA → ubi_fin_ind=0 for B1a step-change.
choice_panel$ubi_renw_1_stat_fin <- ifelse(choice_panel$ubi_fin_ind == 1, TRUE, NA)
choice_panel$ubi_renw_1_stat_non_fin <- ifelse(choice_panel$ubi_ind == 1 & choice_panel$ubi_fin_ind == 0, TRUE, FALSE)
choice_panel$ubi_renw_1_val_final <- ifelse(choice_panel$ubi_fin_ind_drv == 1, cp_score_val, NA)
choice_panel$ubi_rate_rr_vers <- sample(c(0, 1, 2, 3, NA), n_choice, replace = TRUE, prob = c(0.3, 0.2, 0.2, 0.2, 0.1))
choice_panel$RENW <- choice_panel$RENW_CNT
choice_panel$admin_ubi_id <- seq_len(n_choice)

# Forward-looking claim columns: for each policy at RENW_CNT=0, attach the
# claim count at each future renewal period (clm_cnt_renw_{t}_liab).
# Used by c5_selection_figures.R for Fig A.5/A.6 informativeness regressions.
cp0_ids <- choice_panel$POL_ID_CHAR[choice_panel$RENW_CNT == 0]
for (t_fwd in 1:6) {
  future <- panel[panel$RENW_CNT == t_fwd, c("POL_ID_CHAR", "clm_cnt_liab")]
  colnames(future)[2] <- paste0("clm_cnt_renw_", t_fwd, "_liab")
  choice_panel <- merge(choice_panel, future, by = "POL_ID_CHAR", all.x = TRUE)
}

# choice_panel will be merged into data_rf in consolidation step
cat("  choice_panel_us:", nrow(choice_panel), "rows,",
    sum(choice_panel$ubi_fin_ind[choice_panel$RENW_CNT==0]), "finishers at t=0\n")

# X_mat and Y_mat are generated by c0_sum_stat.R at runtime from panel data
# (with proper column names). No need to pre-generate placeholders here.

## ---- 10. Consolidate and write output files ---------------------------------
cat("Consolidating datasets...\n")

# --- data_rf.csv: denormalized panel ---
# Start from panel, left-join all other panel-level datasets
data_rf <- panel

# Join panel_exps (WRT_EXPS_CNT)
data_rf <- data_rf %>%
  left_join(panel_exps, by = c("POL_ID_CHAR", "RENW_CNT"))

# Note: DRVR_YR_LIC is NOT joined here because x_drvr_lic_yr already exists
# in the base panel (generated at line ~207). Joining would create a duplicate
# that breaks panel_renw_clean()'s rename(x_drvr_lic_yr = DRVR_YR_LIC).

# Join st_ubi_dates on ST_CD
data_rf <- data_rf %>%
  left_join(st_ubi_dates, by = c("ST_CD", "state_alpha"))

# Join choice_panel extra columns (ubi/monitoring columns not already in panel)
cp_extra_cols <- c("POL_ID_CHAR", "RENW_CNT",
                   "ubi_ind", "ubi_fin_ind", "ubi_ind_drv", "ubi_fin_ind_drv", "ubi_group",
                   "prem_renw_qt_1_amt",
                   "ubi_sum_val_final_max", "UbiValueNbr", "UbiScoreNbr",
                   "ubi_sum_disc_final_max", "ubi_sum_tier_final_max",
                   "ubi_renw_1_tier_final", "ubi_renw_1_stat_fin",
                   "ubi_renw_1_stat_non_fin", "ubi_renw_1_val_final",
                   "ubi_rate_rr_vers", "admin_ubi_id", "RENW",
                   paste0("clm_cnt_renw_", 1:6, "_liab"))
cp_extra <- choice_panel[, intersect(cp_extra_cols, colnames(choice_panel))]
data_rf <- data_rf %>%
  left_join(cp_extra, by = c("POL_ID_CHAR", "RENW_CNT"))

# Join panel_ubi_renw columns (IL monitored only)
if (nrow(panel_ubi_renw) > 0) {
  data_rf <- data_rf %>%
    left_join(panel_ubi_renw, by = c("POL_ID_CHAR", "RENW_CNT"),
              suffix = c("", ".ubi_renw"))
  # Drop duplicate RENW_SFX_NBR if present from panel_renw join below
}

# Join panel_renw RENW_QT_* demographics (IL only — NA for other states)
renw_qt_cols <- grep("^RENW_QT_", colnames(panel_renw), value = TRUE)
renw_join_cols <- c("POL_ID_CHAR", "RENW_CNT", renw_qt_cols,
                    "RENW_SFX_NBR", "GRG_PSTL_CD",
                    "PRIOR_CARRIER_MIX_GROUP", "PRIOR_CARRIER_GROUP",
                    "PRIOR_CARRIER_NAME", "REQ_DWNPMT_PCT")
renw_join_cols <- intersect(renw_join_cols, colnames(panel_renw))
data_rf <- data_rf %>%
  left_join(panel_renw[, renw_join_cols], by = c("POL_ID_CHAR", "RENW_CNT"),
            suffix = c("", ".renw"))

# Join df_mh columns (policy-level, RENW_CNT=0 — repeated across renewals)
df_mh_cols <- c("POL_ID_CHAR",
                "ubi_factor_0", "ubi_factor_1", "ubi_start_ind",
                "ubi_end_due_to_claim", "days_connect_raw",
                "clm_acci", paste0("clm_acci_renw_", 1:5),
                "last_renewal_seen")
# ubi_fin_ind already in data_rf from choice_panel; keep df_mh's version as dfmh_ubi_fin_ind
df_mh_join <- df_mh[, intersect(df_mh_cols, colnames(df_mh))]
data_rf <- data_rf %>%
  left_join(df_mh_join, by = "POL_ID_CHAR", suffix = c("", ".dfmh"))

# Clean up any .ubi_renw / .renw / .dfmh suffix duplicates
dup_suffixes <- grep("\\.(ubi_renw|renw|dfmh)$", colnames(data_rf), value = TRUE)
if (length(dup_suffixes) > 0) {
  data_rf <- data_rf[, !colnames(data_rf) %in% dup_suffixes]
}

# Sort so that within each policy, post-introduction rows (with valid UBI status)
# come before pre-introduction rows. This ensures that distinct(POL_ID_CHAR) in
# c4_data_figures.R (Fig 3) picks a row where clean_choice_panel() produces valid
# ubi_groups/quintiles, while B1a still sees the full time series with step-change.
has_ubi_stat <- ifelse(is.na(data_rf$ubi_renw_1_stat_fin), 0L, 1L)
data_rf <- data_rf[order(data_rf$POL_ID_CHAR, -has_ubi_stat, data_rf$RENW_CNT), ]
write.csv(data_rf, file.path(SIM_DIR, "data_rf.csv"), row.names = FALSE)
cat("  data_rf.csv:", nrow(data_rf), "rows,", ncol(data_rf), "cols\n")

# --- data_rf_rrev.csv: rate revision IV lookup (IL only) ---
# Columns needed by panel_renw_clean.R for demand elasticity analysis
rrev_cols <- c("POL_ID_CHAR", "RENW_CNT", "prem_expr", "prem_bi_wrt", "prem_bi_ern",
               "state_alpha", "ST_CD", "cov_ind_fullcov", "RENW_SFX_NBR",
               renw_qt_cols, "GRG_PSTL_CD",
               "PRIOR_CARRIER_MIX_GROUP", "PRIOR_CARRIER_GROUP",
               "PRIOR_CARRIER_NAME", "REQ_DWNPMT_PCT")
rrev_cols <- intersect(rrev_cols, colnames(panel_renw))
data_rf_rrev <- panel_renw[, rrev_cols]
write.csv(data_rf_rrev, file.path(SIM_DIR, "data_rf_rrev.csv"), row.names = FALSE)
cat("  data_rf_rrev.csv:", nrow(data_rf_rrev), "rows,", ncol(data_rf_rrev), "cols\n")

# --- data_rf_viol.csv: violation lags (all states) ---
write.csv(panel_viol, file.path(SIM_DIR, "data_rf_viol.csv"), row.names = FALSE)
cat("  data_rf_viol.csv:", nrow(panel_viol), "rows\n")

# --- ubi_vers_dates.csv: state × year × month lookup ---
write.csv(ubi_vers_st_dt_sum, file.path(SIM_DIR, "ubi_vers_dates.csv"), row.names = FALSE)
cat("  ubi_vers_dates.csv:", nrow(ubi_vers_st_dt_sum), "rows\n")

## ---- Summary ---------------------------------------------------------------
cat("\n========================================\n")
cat("Simulated RF data generation complete.\n")
cat("Output directory:", SIM_DIR, "\n")
cat("Files: data_rf.csv, data_rf_rrev.csv, data_rf_viol.csv, ubi_vers_dates.csv\n")
cat("Total policies:", N_POLICIES, "\n")
cat("Total panel rows:", N_ROWS, "\n")
cat("States:", paste(states, collapse = ", "), "\n")
cat("========================================\n")
cat("\nTo use simulated data, set in config.R:\n")
cat("  USE_SIMULATED_DATA <- TRUE\n")
cat("\nFor structural estimation data_list_IL.json, run separately:\n")
cat("  Rscript code/simulate/simulate_data/sim_generate_data_list.R\n")
