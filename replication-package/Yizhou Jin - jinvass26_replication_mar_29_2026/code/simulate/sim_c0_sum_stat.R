################################################################################
## sim_c0_sum_stat.R — Summary statistics from simulated data
##
## Produces:
##   RF_CSV_DIR/tab_1_panel_a.csv          (Table 1 Panel A: summary stats)
##   RF_CSV_DIR/tab_1_panel_b.csv          (Table 1 Panel B: IL market structure)
##   RF_CSV_DIR/tab_1_meta.csv             (Table 1 metadata: N counts)
##   RF_CSV_DIR/appendix/tab_a1a2_summary.json  (Appendix A.1/A.2: X variables)
##   SIM_PROCESSED_DIR/X_mat_panel_us_prenorm.rds  (X matrix, pre-normalization)
##   SIM_PROCESSED_DIR/X_mat_panel_us_norm.rds     (X matrix, normalized)
##   SIM_PROCESSED_DIR/Y_mat_panel_us.rds          (Y matrix)
################################################################################

if (!exists("TABLES_DIR")) source("code/config.R")
source("code/functions/helper.R")
source("code/simulate/functions/load_sim_data.R")

ipak(c("jsonlite"))

dir.create(RF_CSV_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(RF_CSV_DIR, "appendix"), recursive = TRUE, showWarnings = FALSE)

cat("\n=== sim_c0_sum_stat.R ===\n")

## ---- Load data --------------------------------------------------------------

panel      <- load_sim_data("panel", "us", 1)
panel_exps <- load_sim_data("panel_exps", "us", 1)
df_mh      <- load_sim_data("df_mh", "us", 1)

## ---- Join exposures onto panel ----------------------------------------------

panel <- merge(panel, panel_exps, by = c("POL_ID_CHAR", "RENW_CNT"), all.x = TRUE)

## ---- Compute premium and claim variables ------------------------------------
## Mirrors main repo codes/rf/c0_sum_stat.R lines 41-60

## Ensure premium columns exist
prem_liab_cols <- c("prem_bi_ern", "prem_pd_ern", "prem_acq_ern", "prem_opex_ern",
                    "prem_comp_ern", "prem_coll_ern", "prem_ern")
for (cc in prem_liab_cols) {
  if (!cc %in% colnames(panel)) panel[[cc]] <- 0
}

## Exposure-adjusted premium with earned premium ratio adjustment
panel$pct_month_count <- if ("WRT_EXPS_CNT" %in% colnames(panel)) {
  panel$WRT_EXPS_CNT / SIM_EXPOSURE_DIVISOR
} else {
  rep(1, nrow(panel))
}
panel$prem_all <- panel$prem_expr
panel$prem_ern <- pmax(panel$prem_ern,
                       panel$prem_bi_ern + panel$prem_pd_ern +
                       panel$prem_acq_ern + panel$prem_opex_ern)
panel$prem_ern <- ifelse(panel$prem_ern == 0, panel$prem_all, panel$prem_ern)
panel$prem_bipd_proprietary <- panel$prem_bi_ern + panel$prem_pd_ern
panel$prem_liab_proprietary <- panel$prem_bi_ern + panel$prem_pd_ern +
                               panel$prem_acq_ern + panel$prem_opex_ern
panel$all_ern_ratio_noadj <- round(panel$prem_all / panel$prem_ern, 2)
panel$all_ern_ratio <- ifelse(panel$pct_month_count == 0,
                              panel$all_ern_ratio_noadj,
                              round(panel$all_ern_ratio_noadj * panel$pct_month_count, 2))
panel$pct_month_count_clean <- ifelse(
  panel$pct_month_count == 0,
  panel$all_ern_ratio_noadj,
  ifelse(panel$all_ern_ratio <= 0.6 | panel$all_ern_ratio > 1.2,
         panel$pct_month_count / panel$all_ern_ratio,
         panel$pct_month_count)
)

## Exposure-adjust all premium components
panel$prem_bipd_proprietary <- panel$prem_bipd_proprietary / panel$pct_month_count_clean
panel$prem_liab_proprietary <- panel$prem_liab_proprietary / panel$pct_month_count_clean
panel$risk_class <- panel$prem_bipd_proprietary

## Claims: NA -> 0
panel$clm_cnt       <- na_to_zero(panel$clm_cnt)
panel$clm_cnt_liab  <- na_to_zero(panel$clm_cnt_liab)
panel$claim         <- na_to_zero(panel$claim)
panel$claim_liab    <- na_to_zero(panel$claim_liab)

## Winsorize extreme claims: cap dollar amounts at $600K, claim counts at 3
threshold <- SIM_CLAIM_WINSOR
winzor      <- max(panel$claim[panel$claim < threshold])
winzor_liab <- max(panel$claim_liab[panel$claim_liab < threshold])
panel$claim_clean         <- ifelse(panel$claim > threshold, winzor, panel$claim)
panel$claim_liab_clean    <- ifelse(panel$claim_liab > threshold, winzor_liab, panel$claim_liab)
panel$clm_cnt_clean       <- ifelse(panel$clm_cnt > SIM_CLAIM_COUNT_CAP, SIM_CLAIM_COUNT_CAP, panel$clm_cnt)
panel$clm_cnt_liab_clean  <- ifelse(panel$clm_cnt_liab > SIM_CLAIM_COUNT_CAP, SIM_CLAIM_COUNT_CAP, panel$clm_cnt_liab)

## ---- Helper: compute summary stats for one variable -------------------------

sum_stat_row <- function(x, varname, restrict, winsor_probs, N_all) {
  ## Apply restriction
  ok <- switch(restrict,
    "gt0"    = !is.na(x) & x > 0,
    "gte0"   = !is.na(x) & x >= 0,
    "notna"  = !is.na(x),
    rep(TRUE, length(x))
  )
  x <- x[ok]

  ## Winsorize
  if (length(x) > 0 && !is.null(winsor_probs)) {
    x <- winsorize(x, probs = winsor_probs)
  }

  n_used <- length(x)
  if (n_used == 0) {
    return(data.frame(Variable = varname, Mean = NA, SD = NA,
                      Min = NA, p50 = NA, p75 = NA, Max = NA, .N_ = 0,
                      stringsAsFactors = FALSE))
  }

  data.frame(
    Variable = varname,
    Mean     = round(mean(x, na.rm = TRUE), 2),
    SD       = round(sd(x, na.rm = TRUE), 2),
    Min      = round(min(x, na.rm = TRUE), 2),
    p50      = round(quantile(x, 0.50, na.rm = TRUE, names = FALSE), 2),
    p75      = round(quantile(x, 0.75, na.rm = TRUE, names = FALSE), 2),
    Max      = round(max(x, na.rm = TRUE), 2),
    .N_      = n_used,
    stringsAsFactors = FALSE
  )
}

## ---- Table 1 Panel A: Summary Statistics ------------------------------------

cat("  Computing Table 1 Panel A...\n")

## Tenure: from df_mh (RENW_CNT==0 subset), last_renewal_seen + 1
tenure_vec <- if ("last_renewal_seen" %in% colnames(df_mh)) {
  df_mh$last_renewal_seen + 1
} else {
  rep(NA_real_, nrow(df_mh))
}

## Coverage limit: restrict to >0
cov_vec <- if ("COV_BI_LIM_D_NBR" %in% colnames(panel)) {
  panel$COV_BI_LIM_D_NBR / SIM_COVERAGE_DISPLAY_DIV
} else {
  rep(NA_real_, nrow(panel))
}

## Mandatory minimum indicator
mm_vec <- if ("cov_mm_liab_ind" %in% colnames(panel)) {
  panel$cov_mm_liab_ind * 100
} else {
  rep(NA_real_, nrow(panel))
}

## Full coverage indicator
fc_vec <- if ("cov_ind_fullcov" %in% colnames(panel)) {
  panel$cov_ind_fullcov * 100
} else {
  rep(NA_real_, nrow(panel))
}

N_panel <- nrow(panel)

tab_1a <- rbind(
  sum_stat_row(panel$prem_all,             "Total premium (\\$)",              "gt0",  c(0.001, 0.999), N_panel),
  sum_stat_row(panel$prem_liab_proprietary,"Liability premium (\\$)",          "gt0",  c(0.001, 0.999), N_panel),
  sum_stat_row(panel$risk_class,           "Risk class (\\$)",                 "gt0",  c(0, 0.999),     N_panel),
  sum_stat_row(panel$clm_cnt_clean * 100,        "Total claim count (1e-2)",         "gte0", c(0, 1),         N_panel),
  sum_stat_row(panel$clm_cnt_liab_clean * 100,   "Liability claim count (1e-2)",     "gte0", c(0, 1),         N_panel),
  sum_stat_row(panel$claim_clean / 1000,         "Total claim ($\\$000$)",           "gte0", c(0, 1),         N_panel),
  sum_stat_row(panel$claim_liab_clean / 1000,    "Liability claim ($\\$000$)",       "gte0", c(0, 1),         N_panel),
  sum_stat_row(cov_vec,                    "Liability coverage limit ($\\$000$)","gt0",c(0, 1),         N_panel),
  sum_stat_row(mm_vec,                     "Has mandatory minimum limit (\\%)", "notna",c(0, 1),        N_panel),
  sum_stat_row(fc_vec,                     "Has property coverage (\\%)",       "gte0", c(0, 1),        N_panel),
  sum_stat_row(tenure_vec,                 "Tenure (number of renewals)",       "gte0", c(0, 1),        nrow(df_mh))
)
rownames(tab_1a) <- NULL

cat("    Panel A: ", nrow(tab_1a), "rows\n")

## ---- Table 1 Panel B: IL Market Structure -----------------------------------
## Copied from main repo codes/rf/c0_sum_stat.R — Panel B section

cat("  Computing Table 1 Panel B...\n")

##### Bottom panel by Y (Table 1b) ####
data_list <- readRDS(file.path(SIM_CACHE_DIR, "data_list_IL.rds"))
panel_il <- panel[panel$state_alpha == "IL", ]
panel_il$cov_liab <- ifelse(panel_il$COV_BI_LIM_D_NBR == 300000, 150000, ifelse(panel_il$COV_BI_LIM_D_NBR == 500000, 300000, panel_il$COV_BI_LIM_D_NBR))
cat("    IL observations:", nrow(panel_il), "\n")

cov_idx  <- 3:7
cov_lab  <- c("40", "50", "100", "150", "300")
stopifnot(length(cov_idx) == length(cov_lab))

# Average quotes
data_list$prices_raw[data_list$prices_raw > 10000] <- NA
data_list$prices_oo_full[data_list$prices_oo_full > 10000] <- NA
tmp <- grepl("6_", colnames(data_list$prices_oo_full))
data_list$prices_oo_full[, tmp] <- data_list$prices_oo_full[, tmp]/2

own_avg <- colMeans(as.matrix(data_list$prices_raw[, as.character(cov_idx)]), na.rm = TRUE)
comp_firms <- c(4, 3, 35, 6, 2)
avg_by_firm_cov <- function(fid, covi) {
  coln <- paste0(fid, "_", covi)
  x <- data_list$prices_oo_full[, coln, drop = TRUE]
  mean(x, na.rm = TRUE)
}
comp_avgs <- sapply(comp_firms, function(fid) sapply(cov_idx, function(ci) avg_by_firm_cov(fid, ci)))

# Market share
mkt_raw    <- c(9.7, 12.8, 54.4, 7.7, 7.7, 7.8)
mkt_direct <- c(11.8, 10.4, 22.2, 15.6, 3.1, 3.2)

# Coverage share
want_vals <- c(40000, 50000, 100000, 150000, 300000)
tab_cov <- table(factor(panel_il$cov_liab, levels = want_vals))
cov_share_pct <- round(100 * as.numeric(tab_cov) / sum(tab_cov), 0)

# Claims
claim_var <- if ("claim_liab" %in% names(panel_il)) "claim_liab" else "claim"
panel_il$clm_cnt_liab_clean <- na_to_zero(panel_il$clm_cnt_liab)
agg <- lapply(want_vals, function(v) {
  s <- subset(panel_il, cov_liab == v)
  c(avg_claim = mean(s[[claim_var]], na.rm = TRUE),
    avg_ccount = mean(s[["clm_cnt_liab_clean"]], na.rm = TRUE))
})
avg_claims      <- sapply(agg, `[[`, "avg_claim")
avg_claim_count <- sapply(agg, `[[`, "avg_ccount")

bigI <- formatC(sum(data_list$sampling_enum_choice[1:data_list$I]), format = "d", big.mark = ",")
bigN_b <- formatC(sum(data_list$sampling_enum_choice) * (ncol(data_list$prices_oo_full) + ncol(data_list$prices_raw)), format = "d", big.mark = ",")

# Build panel b CSV data
firm_labels <- c("Monitoring firm", paste0("Competitor ", 1:5))
panel_b_quotes <- data.frame(
  section = "quotes",
  firm = firm_labels,
  cov_40 = c(own_avg[1], comp_avgs[1,]),
  cov_50 = c(own_avg[2], comp_avgs[2,]),
  cov_100 = c(own_avg[3], comp_avgs[3,]),
  cov_150 = c(own_avg[4], comp_avgs[4,]),
  cov_300 = c(own_avg[5], comp_avgs[5,]),
  mkt_raw = mkt_raw,
  mkt_direct = mkt_direct,
  stringsAsFactors = FALSE
)
panel_b_cov <- data.frame(
  section = "coverage_share",
  firm = "at monitoring firm (\\%)",
  cov_40 = cov_share_pct[1], cov_50 = cov_share_pct[2],
  cov_100 = cov_share_pct[3], cov_150 = cov_share_pct[4],
  cov_300 = cov_share_pct[5], mkt_raw = NA, mkt_direct = NA,
  stringsAsFactors = FALSE
)
panel_b_claims <- data.frame(
  section = c("coverage_share", "claim_cnt"),
  firm = c("Average liability claim (\\$)", "Average liability claim count"),
  cov_40 = c(avg_claims[1], avg_claim_count[1]),
  cov_50 = c(avg_claims[2], avg_claim_count[2]),
  cov_100 = c(avg_claims[3], avg_claim_count[3]),
  cov_150 = c(avg_claims[4], avg_claim_count[4]),
  cov_300 = c(avg_claims[5], avg_claim_count[5]),
  mkt_raw = NA, mkt_direct = NA,
  stringsAsFactors = FALSE
)
tab_1b <- rbind(panel_b_quotes, panel_b_cov, panel_b_claims)

cat("    Panel B:", nrow(tab_1b), "rows\n")

## ---- Table 1 Metadata -------------------------------------------------------

cat("  Computing Table 1 metadata...\n")

bigN <- formatC(nrow(panel), format = "d", big.mark = ",")

tab_meta <- data.frame(
  key = c("bigN", "bigI", "bigN_b"),
  value = c(bigN, bigI, bigN_b),
  stringsAsFactors = FALSE
)

cat("    bigN =", bigN, ", bigI =", bigI, ", bigN_b =", bigN_b, "\n")

## ---- Appendix Table A.1/A.2: X Variable Summary ----------------------------

cat("  Computing appendix table A.1/A.2 summary...\n")

getX_res <- getX("estimation")
X_cat  <- getX_res[[1]]
X_cont <- getX_res[[2]]
X_label_mat <- getX_res[[4]]

## Helper: compute panel_a/panel_b for a given data scope
compute_x_summary <- function(df) {

  n_obs <- length(unique(df$POL_ID_CHAR))

  ## Derived binary indicators
  df$age_young_ind    <- as.numeric(df$x_drvr_age_rated < SIM_AGE_YOUNG)
  df$age_adult_ind    <- as.numeric(df$x_drvr_age_rated >= SIM_AGE_ADULT)
  df$age_senior_ind   <- as.numeric(df$x_drvr_age_rated >= SIM_AGE_SENIOR)
  df$edu_college_ind  <- as.numeric(df$x_drvr_yr_edu >= SIM_EDU_COLLEGE)
  df$edu_postgrad_ind <- as.numeric(df$x_drvr_yr_edu >= SIM_EDU_POSTGRAD)
  if ("x_cred_ind" %in% colnames(df)) {
    df$cred_avail_ind <- df$x_cred_ind
  } else {
    df$cred_avail_ind <- rep(NA, nrow(df))
  }
  if ("tier_acci_drvr_pt_2" %in% colnames(df)) {
    df$tier_good_ind <- as.numeric(df$tier_acci_drvr_pt_2 == 0)
  } else {
    df$tier_good_ind <- rep(NA, nrow(df))
  }
  if ("tier_pop_lngth" %in% colnames(df)) {
    df$tier_pop_lngth_ind <- as.numeric(!is.na(df$tier_pop_lngth))
  } else {
    df$tier_pop_lngth_ind <- rep(NA, nrow(df))
  }

  ## Panel A: binary/indicator variables
  binary_vars <- list(
    list(var = "x_drvr_is_female",     name = "Female Ind."),
    list(var = "x_drvr_hm_own_ind",    name = "Homeowner Ind."),
    list(var = "x_drvr_lic_oos_ind",   name = "Out-of-state Ind."),
    list(var = "x_cred_clue_ord_ind",  name = "Credit Report Ind."),
    list(var = "x_veh_ls_pay_ind",     name = "Vehicle on Lease Ind."),
    list(var = "x_veh_abs_ind",        name = "ABS Ind."),
    list(var = "x_veh_sd_ind",         name = "Safe Device Ind."),
    list(var = "x_veh_class_C_ind",    name = "Class C Vehicle Ind."),
    list(var = "x_loc_grg_verify_ind", name = "Garage Verification Ind."),
    list(var = "tier_pop_yes_ind",     name = "Prior Insurance Ind."),
    list(var = "tier_pop_some_ind",    name = "Prior Insurance with Lapse Ind."),
    list(var = "tier_pref_ind",        name = "Preferred Customer Ind."),
    list(var = "tier_acci_dui_cnt",    name = "Record - DUI Count"),
    list(var = "tier_pop_lngth_ind",   name = "Length of Prior Insurance"),
    list(var = "tier_good_ind",        name = "Clean Record Ind."),
    list(var = "age_young_ind",        name = "Age $<$ 25 Ind."),
    list(var = "age_adult_ind",        name = "Age $\\geq$ 21 Ind."),
    list(var = "age_senior_ind",       name = "Age $>$ 60 Ind."),
    list(var = "edu_college_ind",      name = "College Ind."),
    list(var = "edu_postgrad_ind",     name = "Post Grad Ind."),
    list(var = "cred_avail_ind",       name = "Insurance History Avail. Ind.")
  )

  panel_a <- lapply(binary_vars, function(bv) {
    if (bv$var %in% colnames(df)) {
      vals <- df[[bv$var]]
      vals <- vals[!is.na(vals)]
      list(name = bv$name, mean = round(mean(vals), 2))
    } else {
      list(name = bv$name, mean = NA)
    }
  })

  ## Panel B: continuous variables
  ## Derived continuous: calendar month, zipcode income ($'000), log zipcode income, risk class
  if ("date_pol_eff" %in% colnames(df)) {
    df$cal_month <- as.numeric(format(as.Date(df$date_pol_eff), "%m"))
  } else {
    df$cal_month <- rep(NA, nrow(df))
  }
  if ("x_loc_zipcd_agi" %in% colnames(df)) {
    df$zip_inc_k <- df$x_loc_zipcd_agi / 1000
  } else {
    df$zip_inc_k <- rep(NA, nrow(df))
  }
  if ("x_loc_zipcd_lg_inc" %in% colnames(df)) {
    df$lg_zip_inc <- df$x_loc_zipcd_lg_inc
  } else if ("x_loc_zipcd_agi" %in% colnames(df)) {
    df$lg_zip_inc <- log(df$x_loc_zipcd_agi)
  } else {
    df$lg_zip_inc <- rep(NA, nrow(df))
  }

  ## Risk class (prem_bipd / 100), need prem columns
  if ("prem_bi_ern" %in% colnames(df) && "prem_pd_ern" %in% colnames(df)) {
    df$rc <- (df$prem_bi_ern + df$prem_pd_ern) / 100
  } else {
    df$rc <- rep(NA, nrow(df))
  }

  ## Vehicle model year as age (relative to reference)
  if ("x_veh_mdl_yr" %in% colnames(df)) {
    df$veh_mdl_yr_use <- df$x_veh_mdl_yr
  } else {
    df$veh_mdl_yr_use <- rep(NA, nrow(df))
  }

  cont_vars <- list(
    list(var = "x_drvr_yr_edu",          name = "Years of Edu."),
    list(var = "veh_mdl_yr_use",         name = "Vehicle Model Year"),
    list(var = "x_veh_len_own",          name = "Ownership Length Cat."),
    list(var = "tier_acci_tot_aaf_cnt",  name = "Record: At-Fault Accident Count"),
    list(var = "x_cred_score",           name = "Driver Credit Tier"),
    list(var = "x_loc_popltn_dens_pct",  name = "Population Density Percentile"),
    list(var = "tier_acci_drvr_pt",      name = "Record: Accident Points"),
    list(var = "cal_month",              name = "Calendar Month"),
    list(var = "zip_inc_k",              name = "Zipcode Income (\\$'000)"),
    list(var = "lg_zip_inc",             name = "Log Zipcode Income"),
    list(var = "rc",                     name = "Risk Class")
  )

  panel_b <- lapply(cont_vars, function(cv) {
    if (cv$var %in% colnames(df)) {
      vals <- df[[cv$var]]
      vals <- vals[!is.na(vals)]
      if (length(vals) == 0) {
        return(list(name = cv$name, mean = NA, sd = NA, min = NA,
                    p25 = NA, p50 = NA, p75 = NA, max = NA, is_integer = TRUE))
      }
      list(
        name = cv$name,
        mean = round(mean(vals), 2),
        sd   = round(sd(vals), 2),
        min  = round(min(vals), 2),
        p25  = round(quantile(vals, 0.25, names = FALSE), 2),
        p50  = round(quantile(vals, 0.50, names = FALSE), 2),
        p75  = round(quantile(vals, 0.75, names = FALSE), 2),
        max  = round(max(vals), 2),
        is_integer = TRUE
      )
    } else {
      list(name = cv$name, mean = NA, sd = NA, min = NA,
           p25 = NA, p50 = NA, p75 = NA, max = NA, is_integer = TRUE)
    }
  })

  ## Age mean: fraction aged >= 21 (or raw mean if needed for display)
  age_mean <- if ("age_adult_ind" %in% colnames(df)) {
    round(mean(df$age_adult_ind, na.rm = TRUE), 2)
  } else {
    round(mean(df$x_drvr_age_rated, na.rm = TRUE), 2)
  }

  list(n = n_obs, panel_a = panel_a, panel_b = panel_b, age_mean = age_mean)
}

## Use RENW_CNT == 0 for cross-sectional X variable summary (policy-level)
panel_r0 <- panel[panel$RENW_CNT == 0, ]
panel_r0_il <- panel_r0[panel_r0$state_alpha == "IL", ]

## If IL has no data, use full panel
if (nrow(panel_r0_il) == 0) panel_r0_il <- panel_r0

tab_a1a2 <- list(
  us = compute_x_summary(panel_r0),
  il = compute_x_summary(panel_r0_il)
)

cat("    US n =", tab_a1a2$us$n, ", IL n =", tab_a1a2$il$n, "\n")

## ---- X_mat and Y_mat --------------------------------------------------------

cat("  Building X_mat and Y_mat...\n")

## X_mat: estimation covariates + derived indicators
build_x_mat <- function(df) {

  out <- data.frame(
    POL_ID_CHAR = df$POL_ID_CHAR,
    RENW_CNT    = df$RENW_CNT,
    stringsAsFactors = FALSE
  )

  ## Categorical X variables
  for (v in X_cat) {
    if (v %in% colnames(df)) out[[v]] <- df[[v]]
    else out[[v]] <- NA_real_
  }

  ## Derived binary indicators
  out$trend_season    <- if ("date_pol_eff" %in% colnames(df)) {
    as.numeric(format(as.Date(df$date_pol_eff), "%m"))
  } else { NA_real_ }

  out$tier_good_ind    <- if ("tier_acci_drvr_pt_2" %in% colnames(df)) {
    as.numeric(df$tier_acci_drvr_pt_2 == 0)
  } else { NA_real_ }

  out$age_young_ind    <- as.numeric(df$x_drvr_age_rated < SIM_AGE_YOUNG)
  out$age_adult_ind    <- as.numeric(df$x_drvr_age_rated >= SIM_AGE_ADULT)
  out$age_senior_ind   <- as.numeric(df$x_drvr_age_rated >= SIM_AGE_SENIOR)
  out$edu_college_ind  <- as.numeric(df$x_drvr_yr_edu >= SIM_EDU_COLLEGE)
  out$edu_postgrad_ind <- as.numeric(df$x_drvr_yr_edu >= SIM_EDU_POSTGRAD)

  out$tier_pop_lngth_ind <- if ("tier_pop_lngth" %in% colnames(df)) {
    as.numeric(!is.na(df$tier_pop_lngth))
  } else { NA_real_ }

  ## Continuous X variables (pre-normalization)
  for (v in X_cont) {
    if (v %in% colnames(df)) out[[v]] <- df[[v]]
    else out[[v]] <- NA_real_
  }

  ## Additional continuous: zip_inc, lg_zip_inc, rc, tier_acci_drvr_pt_raw
  out$zip_inc <- if ("x_loc_zipcd_agi" %in% colnames(df)) {
    df$x_loc_zipcd_agi / 1000
  } else { NA_real_ }

  out$lg_zip_inc <- if ("x_loc_zipcd_lg_inc" %in% colnames(df)) {
    df$x_loc_zipcd_lg_inc
  } else if ("x_loc_zipcd_agi" %in% colnames(df)) {
    log(df$x_loc_zipcd_agi)
  } else { NA_real_ }

  out$rc <- if ("prem_bi_ern" %in% colnames(df) && "prem_pd_ern" %in% colnames(df)) {
    (df$prem_bi_ern + df$prem_pd_ern) / 100
  } else { NA_real_ }

  out$tier_acci_drvr_pt_raw <- if ("tier_acci_drvr_pt" %in% colnames(df)) {
    df$tier_acci_drvr_pt
  } else { NA_real_ }

  out
}

X_mat_prenorm <- build_x_mat(panel)

## Drop raw columns that are replaced by derived indicators
## (main repo X_mat does not include these after transformation)
raw_to_drop <- c("x_drvr_lic_yr", "x_drvr_age_rated", "x_loc_zipcd_agi")
raw_to_drop <- intersect(raw_to_drop, colnames(X_mat_prenorm))
if (length(raw_to_drop) > 0) {
  X_mat_prenorm[raw_to_drop] <- NULL
}

## Normalized version: continuous variables scaled to [0,1]
X_mat_norm <- X_mat_prenorm

## Identify continuous columns to normalize
cont_cols_to_norm <- c(X_cont, "zip_inc", "lg_zip_inc", "rc", "tier_acci_drvr_pt_raw",
                       "trend_season")
cont_cols_to_norm <- intersect(cont_cols_to_norm, colnames(X_mat_norm))

for (cc in cont_cols_to_norm) {
  vals <- X_mat_norm[[cc]]
  if (all(is.na(vals))) next
  rng <- range(vals, na.rm = TRUE)
  if (rng[2] - rng[1] > 0) {
    X_mat_norm[[cc]] <- (vals - rng[1]) / (rng[2] - rng[1])
  }
}

## tier_pop_lngth: main repo uses factor-level scaling + na_to_zero (lines 208-215)
## Must match: tier_pop_lngth_ind = !is.na(tier_pop_lngth), then NA→0
if ("tier_pop_lngth" %in% colnames(X_mat_norm)) {
  X_mat_norm$tier_pop_lngth <- na_to_zero(X_mat_norm$tier_pop_lngth)
  X_mat_prenorm$tier_pop_lngth <- na_to_zero(X_mat_prenorm$tier_pop_lngth)
}
if ("tier_pop_lngth_ind" %in% colnames(X_mat_norm)) {
  ## Main repo: tier_pop_lngth_ind = as.numeric(!is.na(tier_pop_lngth)) — before na_to_zero
  ## Already set correctly in build_x_mat if defined as !is.na(); just ensure it's correct
  ## (replication had > 0 instead of !is.na — fix was applied in build_x_mat already)
}

cat("    X_mat:", nrow(X_mat_prenorm), "x", ncol(X_mat_prenorm), "\n")

## Y_mat: coverage/choice variables
## Matches main repo c0_sum_stat.R lines 54-136 exactly:
##   - Bin deductibles via findInterval + na_to_zero
##   - Extract cov_lim_bi from COV_BI_LIM_CHAR string
##   - Map cov_pip_char via case_when
##   - Join ubi_vers_st from lookup CSV
##   - Convert to factors with exclude = NULL (makes NA a level, not missing)
build_y_mat <- function(df) {
  ## 1. Derive coverage columns in df (matching main repo panel mutations)
  df <- df %>% mutate(
    cov_ded_coll_ind = !is.na(COV_COLL_DED_NBR_50),
    cov_ded_comp_ind = !is.na(COV_COMP_DED_NBR_50),
    cov_ded_coll = na_to_zero(c(100, 250, 500, 1000, 2000)[findInterval(COV_COLL_DED_NBR_50, c(-Inf, 200, 500, 1000, 2000, Inf))]),
    cov_ded_comp = na_to_zero(c(100, 250, 500, 1000, 2000)[findInterval(COV_COMP_DED_NBR_50, c(-Inf, 200, 500, 1000, 2000, Inf))]),
    cov_lim_bi = as.numeric(str_remove_all(str_extract(COV_BI_LIM_CHAR, "[\\d,]+"), ",")),
    cov_lim_bi_is_csl = case_when(
      str_detect(COV_BI_LIM_CHAR, "/") ~ 0,
      str_detect(COV_BI_LIM_CHAR, "\\d") ~ 1,
      TRUE ~ NA_real_
    ),
    cov_pip_char = case_when(
      str_detect(COV_PIP, "100,000") ~ "100,000",
      str_detect(COV_PIP, "50,000")  ~ "50,000",
      str_detect(COV_PIP, "35,000")  ~ "35,000",
      str_detect(COV_PIP, "30,000")  ~ "30,000",
      str_detect(COV_PIP, "25,000")  ~ "25,000",
      str_detect(COV_PIP, "20,000")  ~ "20,000",
      str_detect(COV_PIP, "15,000")  ~ "15,000",
      str_detect(COV_PIP, "10,00")   ~ "10,000",
      str_detect(COV_PIP, "5,00")    ~ "5,000",
      str_detect(COV_PIP, "2,50")    ~ "2,500",
      str_detect(COV_PIP, "FULL|MEDICAL EXPENSE") ~ "Full Medical",
      !is.na(COV_PIP) ~ "Other",
      is.na(COV_PIP) ~ NA_character_
    ),
    st_cov_bi_ind = !is.na(cov_lim_bi),
    st_cov_pip_ind = is.na(cov_lim_bi),
    cov_pip_missing = ifelse(is.na(cov_lim_bi) & is.na(cov_pip_char), 1, 0)
  )

  ## 2. Join ubi_vers_st from lookup table (matching main repo lines 99-111)
  ubi_vers_csv <- file.path(SIM_DATA_DIR, "ubi_vers_dates.csv")
  if (file.exists(ubi_vers_csv)) {
    ubi_vers_st_dt_sum <- read.csv(ubi_vers_csv, stringsAsFactors = FALSE)
    ## Need date_yr and date_mon for the join
    if (!"date_yr" %in% colnames(df)) {
      df$date_yr <- as.numeric(substr(as.character(df$date_pol_eff), 1, 4))
    }
    if (!"date_mon" %in% colnames(df)) {
      df$date_mon <- as.numeric(substr(as.character(df$date_pol_eff), 6, 7))
    }
    join_cols <- intersect(c("ST_CD", "date_yr", "date_mon"), colnames(ubi_vers_st_dt_sum))
    if (length(join_cols) > 0 && "ubi_vers_st" %in% colnames(ubi_vers_st_dt_sum)) {
      df <- df %>% left_join(ubi_vers_st_dt_sum[c(join_cols, "ubi_vers_st")], by = join_cols)
    } else {
      df$ubi_vers_st <- NA
    }
  } else {
    df$ubi_vers_st <- if ("ubi_rate_dvc_vers" %in% colnames(df)) df$ubi_rate_dvc_vers else NA
  }

  ## Ensure cov_ind_fullcov exists
  if (!"cov_ind_fullcov" %in% colnames(df)) df$cov_ind_fullcov <- NA_real_

  ## 3. Select Y columns (matching main repo lines 114-121)
  Y_data <- df %>%
    dplyr::select(all_of(c(
      "POL_ID_CHAR", "RENW_CNT", "date_pol_eff", "state_alpha", "ST_CD",
      "st_cov_bi_ind", "st_cov_pip_ind", "ubi_vers_st",
      "cov_lim_bi", "cov_lim_bi_is_csl", "cov_pip_char", "cov_pip_missing",
      "cov_ded_coll_ind", "cov_ded_comp_ind", "cov_ded_coll", "cov_ded_comp", "cov_ind_fullcov"
    )))

  ## 4. Convert to factors with exclude = NULL (matching main repo lines 125-136)
  ##    This makes NA a factor level, so lm() does NOT drop rows with NAs
  exclusions <- c("POL_ID_CHAR", "RENW_CNT", "date_pol_eff")

  Y_mat <- Y_data %>%
    mutate(across(where(is.logical), as.numeric)) %>%
    mutate(
      across(
        .cols = where(is.character) & !all_of(exclusions),
        .fns = ~ factor(., exclude = NULL)
      ),
      across(
        .cols = where(~ is.numeric(.) && n_distinct(na.omit(.)) < 20) & !all_of(exclusions),
        .fns = ~ factor(., exclude = NULL)
      )
    )

  Y_mat
}

Y_mat <- build_y_mat(panel)
cat("    Y_mat:", nrow(Y_mat), "x", ncol(Y_mat), "\n")

## ---- Write outputs ----------------------------------------------------------

cat("  Writing outputs...\n")

write.csv(tab_1a, file.path(RF_CSV_DIR, "tab_1_panel_a.csv"), row.names = FALSE)
cat("    ->", file.path(RF_CSV_DIR, "tab_1_panel_a.csv"), "\n")

write.csv(tab_1b, file.path(RF_CSV_DIR, "tab_1_panel_b.csv"), row.names = FALSE)
cat("    ->", file.path(RF_CSV_DIR, "tab_1_panel_b.csv"), "\n")

write.csv(tab_meta, file.path(RF_CSV_DIR, "tab_1_meta.csv"), row.names = FALSE)
cat("    ->", file.path(RF_CSV_DIR, "tab_1_meta.csv"), "\n")

jsonlite::write_json(tab_a1a2, file.path(RF_CSV_DIR, "appendix", "tab_a1a2_summary.json"),
                     pretty = TRUE, auto_unbox = TRUE)
cat("    ->", file.path(RF_CSV_DIR, "appendix", "tab_a1a2_summary.json"), "\n")

saveRDS(X_mat_prenorm, file.path(SIM_PROCESSED_DIR, "X_mat_panel_us_prenorm.rds"))
cat("    ->", file.path(SIM_PROCESSED_DIR, "X_mat_panel_us_prenorm.rds"), "\n")

saveRDS(X_mat_norm, file.path(SIM_PROCESSED_DIR, "X_mat_panel_us_norm.rds"))
cat("    ->", file.path(SIM_PROCESSED_DIR, "X_mat_panel_us_norm.rds"), "\n")

saveRDS(Y_mat, file.path(SIM_PROCESSED_DIR, "Y_mat_panel_us.rds"))
cat("    ->", file.path(SIM_PROCESSED_DIR, "Y_mat_panel_us.rds"), "\n")

## ---- Cleanup ----------------------------------------------------------------

rm(panel, panel_exps, panel_il, df_mh, panel_r0, panel_r0_il)
rm(X_mat_prenorm, X_mat_norm, Y_mat)
if (exists("data_list")) rm(data_list)
gc()

cat("=== sim_c0_sum_stat.R done ===\n")
