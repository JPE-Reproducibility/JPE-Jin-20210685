################################################################################
## sim_c5_selection_figures.R — Selection & informativeness figure data
##
## Produces:
##   RF_CSV_DIR/appendix/fig_a5_a6.csv          — Informativeness of monitoring
##   RF_REG_DIR/appendix/fig_a5_a6_regression.json (backward compat)
##   RF_CSV_DIR/appendix/fig_b1a.csv            — Monitoring adoption event study
##   RF_CSV_DIR/appendix/fig_b1b.csv            — RD at monitoring introduction
##   RF_REG_DIR/appendix/fig_b1b_regression.json (backward compat)
##
## Logic mirrors codes/rf/c5_selection_figures.R from the main repo exactly.
################################################################################

if (!exists("TABLES_DIR")) source("code/config.R")
source("code/functions/helper.R")
source("code/simulate/functions/load_sim_data.R")
source("code/simulate/functions/data_clean/clean_choice_panel.R")

ipak(c("jsonlite", "scales", "lubridate"))

dir.create(file.path(RF_CSV_DIR, "appendix"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(RF_REG_DIR, "appendix"), recursive = TRUE, showWarnings = FALSE)

cat("\n=== sim_c5_selection_figures.R ===\n")

data_states <- "us"

## ============================================================================
## Load choice_panel and clean it
## ============================================================================

choice_panel <- load_sim_data("choice_panel", data_states, 1)
choice_panel <- clean_choice_panel(choice_panel)

## ============================================================================
## Part 1: Figures A.5 and A.6 — Diminishing Informativeness
## ============================================================================
cat("--- appendix/fig_a5_a6 ---\n")

## Load X matrix for observable controls (prenorm version for c5)
xmat_file <- file.path(SIM_PROCESSED_DIR, "X_mat_panel_us_prenorm.rds")
if (file.exists(xmat_file)) {
  X_mat <- readRDS(xmat_file)
  X <- colnames(X_mat)[3:ncol(X_mat)]
} else {
  cat("  [SKIP] Fig A.5/A.6: X_mat_panel_us_prenorm.rds not found\n")
  X <- NULL
}

if (!is.null(X)) {
  X <- X[!grepl("rc", X)]  # rc is endogenous with ubi value
  t_T <- 6  # Maximum renewal periods

  ## Run regressions for each renewal period t = 1..6
  ## Three specs: "w/ ctrl" (full X), "claim ctrl" (claim history only), "w/o ctrl"
  make_empty_tbl <- function() {
    data.frame(
      pool     = factor(1:t_T, levels = 1:t_T, labels = as.character(1:t_T)),
      mfx_ind  = rep(0, t_T),
      se_ind   = rep(0, t_T),
      mfx_val  = rep(0, t_T),
      se_val   = rep(0, t_T),
      group    = as.character(c(0, rep(1, t_T - 1))),
      t        = as.character(1:t_T),
      avg_clm_cnt = rep(0, t_T),
      stringsAsFactors = FALSE
    )
  }

  tbl_ctrl     <- make_empty_tbl()
  tbl_clm_only <- make_empty_tbl()
  tbl_no_ctrl  <- make_empty_tbl()

  cp_work <- choice_panel

  for (t in 1:t_T) {
    clm_col <- paste0("clm_cnt_renw_", t, "_liab")
    if (!clm_col %in% colnames(cp_work)) next
    if (all(is.na(cp_work[[clm_col]]))) next

    cp_work$claim_var <- as.vector(unlist(cp_work[clm_col]))

    if (t == 1) {
      cp_work$claim_ctrl_var <- cp_work$clm_cnt_liab
    } else {
      prev_col <- paste0("clm_cnt_renw_", t - 1, "_liab")
      if (prev_col %in% colnames(cp_work)) {
        cp_work$claim_ctrl_var <- cp_work$claim_ctrl_var +
          as.vector(unlist(cp_work[prev_col]))
      }
    }

    regdata <- cp_work[c("POL_ID_CHAR", "claim_var", "ubi_fin_ind",
                         "ubi_val_fin", "claim_ctrl_var")] %>%
      filter(!is.na(claim_var)) %>%
      mutate(ubi_val_fin = ifelse(is.na(ubi_val_fin), 0, scale(ubi_val_fin)))

    ## Use RENW_CNT == t if available, else fall back to RENW_CNT == 0
    xmat_t <- if (t %in% unique(X_mat$RENW_CNT)) t else 0L
    regdata_X <- regdata %>%
      left_join(mutate(filter(X_mat, RENW_CNT == xmat_t), RENW_CNT = NULL),
                by = "POL_ID_CHAR")

    avg_clm <- mean(filter(regdata, ubi_fin_ind == 1)$claim_var)

    ## Spec 1: no controls
    reg <- lm(claim_var ~ ., data = regdata[c("claim_var", "ubi_fin_ind", "ubi_val_fin")])
    co <- summary(reg)$coefficients
    tbl_no_ctrl$mfx_ind[t] <- co["ubi_fin_ind", "Estimate"]
    tbl_no_ctrl$mfx_val[t] <- co["ubi_val_fin", "Estimate"]
    tbl_no_ctrl$se_ind[t]  <- co["ubi_fin_ind", "Std. Error"]
    tbl_no_ctrl$se_val[t]  <- co["ubi_val_fin", "Std. Error"]
    tbl_no_ctrl$avg_clm_cnt[t] <- avg_clm

    ## Spec 2: claim controls only
    reg <- lm(claim_var ~ ., data = regdata[c("claim_var", "ubi_fin_ind",
                                               "ubi_val_fin", "claim_ctrl_var")])
    co <- summary(reg)$coefficients
    tbl_clm_only$mfx_ind[t] <- co["ubi_fin_ind", "Estimate"]
    tbl_clm_only$mfx_val[t] <- co["ubi_val_fin", "Estimate"]
    tbl_clm_only$se_ind[t]  <- co["ubi_fin_ind", "Std. Error"]
    tbl_clm_only$se_val[t]  <- co["ubi_val_fin", "Std. Error"]
    tbl_clm_only$avg_clm_cnt[t] <- avg_clm

    ## Spec 3: full X controls
    X_avail <- X[X %in% colnames(regdata_X)]
    regdata_lm <- regdata_X[c("claim_var", "ubi_fin_ind",
                               "ubi_val_fin", "claim_ctrl_var", X_avail)]
    regdata_lm <- regdata_lm[complete.cases(regdata_lm), ]
    if (nrow(regdata_lm) > 0) {
      reg <- lm(claim_var ~ ., data = regdata_lm)
      co <- summary(reg)$coefficients
      tbl_ctrl$mfx_ind[t] <- co["ubi_fin_ind", "Estimate"]
      tbl_ctrl$mfx_val[t] <- co["ubi_val_fin", "Estimate"]
      tbl_ctrl$se_ind[t]  <- co["ubi_fin_ind", "Std. Error"]
      tbl_ctrl$se_val[t]  <- co["ubi_val_fin", "Std. Error"]
    }
    tbl_ctrl$avg_clm_cnt[t] <- avg_clm
  }

  df_info <- mutate(tbl_ctrl, group = 0) %>%
    rbind.data.frame(mutate(tbl_no_ctrl, group = 2)) %>%
    rbind.data.frame(mutate(tbl_clm_only, group = 1)) %>%
    mutate(group = factor(group, levels = c(0, 1, 2),
                          labels = c("w/ ctrl", "claim ctrl", "w/o ctrl")))

  ## Normalize to percent of average monitored claim count
  mfx_se_cols <- grepl("mfx|se", colnames(df_info)) & !grepl("avg", colnames(df_info))
  df_info_pct <- df_info
  df_info_pct[mfx_se_cols] <- df_info_pct[mfx_se_cols] /
    do.call("cbind.data.frame",
            replicate(sum(mfx_se_cols), df_info_pct["avg_clm_cnt"], simplify = FALSE))

  ## Save CSV (main output)
  write.csv(df_info_pct[c("pool", "group", "mfx_ind", "se_ind", "mfx_val", "se_val")],
            file.path(RF_CSV_DIR, "appendix", "fig_a5_a6.csv"), row.names = FALSE)
  cat("  [GEN] appendix/fig_a5_a6.csv\n")

  ## Save JSON for backward compatibility
  json_result <- list(
    type        = "informativeness",
    description = "Informativeness of monitoring over time",
    n_periods   = t_T,
    specs       = c("w/ ctrl", "claim ctrl", "w/o ctrl"),
    periods     = list()
  )
  for (spec in c("w/ ctrl", "claim ctrl", "w/o ctrl")) {
    spec_rows <- list()
    src <- switch(spec,
                  "w/ ctrl"    = tbl_ctrl,
                  "claim ctrl" = tbl_clm_only,
                  "w/o ctrl"   = tbl_no_ctrl)
    for (tt in seq_len(t_T)) {
      spec_rows[[tt]] <- list(
        mfx_ind     = src$mfx_ind[tt],
        se_ind      = src$se_ind[tt],
        mfx_val     = src$mfx_val[tt],
        se_val      = src$se_val[tt],
        avg_clm_cnt = src$avg_clm_cnt[tt]
      )
    }
    json_result$periods[[spec]] <- spec_rows
  }
  writeLines(toJSON(json_result, auto_unbox = TRUE, pretty = TRUE, digits = 15),
             file.path(RF_REG_DIR, "appendix", "fig_a5_a6_regression.json"))
  cat("  [GEN] appendix/fig_a5_a6_regression.json\n")
}

## ============================================================================
## Part 2: Figures B.1a and B.1b — Cream Skimming / Event Study
## ============================================================================
cat("--- appendix/fig_b1a & fig_b1b ---\n")

## Load panel_exps for exposure counts
panel_exps <- load_sim_data("panel_exps", data_states, 1)
panel_exps <- panel_exps %>%
  mutate(POL_ID_CHAR = as.character(POL_ID_CHAR),
         dup = duplicated(paste(POL_ID_CHAR, RENW_CNT))) %>%
  filter(!dup) %>%
  select(-dup)

## Load UBI dates
st_ubi_dates <- load_sim_data("st_ubi_dates")
st_ubi_dates$min_ubi_1 <- as.Date(st_ubi_dates$min_ubi_1)
st_ubi_dates$min_ubi_3 <- as.Date(st_ubi_dates$min_ubi_3)
st_ubi_gap_dates <- list()
st_ubi_gap_dates_3 <- list()

## Join UBI dates and filter gap dates
choice_panel <- choice_panel %>% left_join(st_ubi_dates, by = "ST_CD")

for (st_cd in unique(choice_panel$ST_CD)) {
  skip_dates <- c(st_ubi_gap_dates[[as.character(st_cd)]],
                  st_ubi_gap_dates_3[[as.character(st_cd)]])
  if (length(skip_dates) > 0) {
    choice_panel <- choice_panel %>%
      filter(!(ST_CD == st_cd & (as_date_70(date_pol_eff) %in% as_date_70(skip_dates))))
  }
}

## Join panel_exps and compute time variables
choice_panel <- choice_panel %>%
  left_join(filter(panel_exps, RENW_CNT == 0),
            by = c("POL_ID_CHAR", "RENW_CNT")) %>%
  mutate(pct_month_count = WRT_EXPS_CNT / SIM_EXPOSURE_DIVISOR,
         date_pol_eff = as_date_70(date_pol_eff),
         min_ubi_date = as_date_70(min_ubi_1),
         min_ubi_3_date = as_date_70(min_ubi_3),
         min_ubi_date_all = pmin(min_ubi_date, min_ubi_3_date),
         mon_since_ubi_all = floor(as.numeric(date_pol_eff - min_ubi_date_all) / (365 / 12)),
         qtr_since_ubi_all = floor(as.numeric(date_pol_eff - min_ubi_date_all) / (365 / 4)),
         ubi_post_ind = (as.numeric(date_pol_eff) >= as.numeric(min_ubi_date)) * 1,
         prem_liab_all = prem_liab_ern + prem_acq_ern + prem_opex_ern + prem_fee_ern,
         prem_all = prem_liab_all + prem_coll_ern + prem_comp_ern)

scaleFUN <- function(x) sprintf("%.0f", x)

## --------------------------------------------------------------------------
## Figure B.1a: Monthly monitoring finish rate around introduction
## --------------------------------------------------------------------------
num_mon_pre <- -4; num_mon_post <- 12
df_b1a <- choice_panel %>%
  filter(min_ubi_date_all >= as_date_70("2012-01-01") + 30.5 * (-num_mon_pre - 1) &
           min_ubi_date_all <= as_date_70("2016-12-18") - 30.5 * num_mon_post) %>%
  mutate(mon_since_ubi = mon_since_ubi_all)

df_trend <- df_b1a %>%
  group_by(mon_since_ubi) %>%
  summarise(mon_fin_pct = mean(ubi_fin_ind), count = n(), .groups = "drop")

write.csv(df_trend, file.path(RF_CSV_DIR, "appendix", "fig_b1a.csv"), row.names = FALSE)
cat("  [GEN] appendix/fig_b1a.csv (", nrow(df_trend), "months)\n")

## --------------------------------------------------------------------------
## Figure B.1b: RD regression — effect of monitoring intro on unmonitored pool
## --------------------------------------------------------------------------
t_T_pre <- 4; t_T_rd <- 4
df_rd <- choice_panel %>%
  filter(min_ubi_date_all >= as_date_70("2012-01-01") + 365 / 4 * t_T_pre &
           min_ubi_date_all <= as_date_70("2016-12-18") - 365 / 4 * t_T_rd) %>%
  mutate(mon_since_ubi = mon_since_ubi_all,
         qtr_since_ubi = qtr_since_ubi_all)

regdata_main <- df_rd %>%
  filter(!is.na(min_ubi_date_all)) %>%
  mutate(ubi_post_ftr = (qtr_since_ubi >= 0) * 1) %>%
  filter(qtr_since_ubi < t_T_rd & qtr_since_ubi >= -t_T_pre) %>%
  filter(ubi_fin_ind == 0) %>%
  mutate(run_var = qtr_since_ubi,
         claim_var_cnt = clm_cnt_liab + clm_cnt_coll,
         prem_var = prem_liab_all + prem_coll_ern) %>%
  filter(prem_var > 30) %>%
  filter(!is.na(claim_var_cnt))

## Build X controls for RD (estimation-style controls)
X_cat <- c('x_drvr_is_female', 'x_drvr_yr_edu', 'x_drvr_hm_own_ind',
           'x_drvr_lic_oos_ind', 'x_cred_clue_ord_ind', 'x_veh_ls_pay_ind',
           'x_veh_mdl_yr', 'x_veh_abs_ind', 'x_veh_sd_ind', 'x_veh_len_own',
           'x_veh_class_C_ind', 'x_loc_grg_verify_ind',
           'tier_pop_yes_ind', 'tier_pop_some_ind', 'tier_pref_ind',
           'tier_acci_tot_aaf_cnt', 'tier_acci_dui_cnt')
X_cont <- c('x_drvr_age_rated', 'x_cred_score', 'x_loc_popltn_dens_pct',
            'x_loc_zipcd_agi', 'tier_pop_lngth', 'tier_acci_drvr_pt')

## Add trend/season and state FE
regdata_main <- regdata_main %>%
  mutate(tier_acci_drvr_pt = ifelse("tier_acci_drvr_pt_2" %in% names(.),
                                     tier_acci_drvr_pt_2, tier_acci_drvr_pt),
         x_drvr_lic_yr = ifelse("DRVR_YR_LIC" %in% names(.), DRVR_YR_LIC, 0),
         trend_yr = year(date_pol_eff),
         trend_season = month(date_pol_eff),
         risk_class = prem_bi_ern + prem_pd_ern)

## Filter available columns
all_X_needed <- c(X_cat, X_cont, "trend_yr", "trend_season", "risk_class", "ST_CD")
X_avail_rd <- all_X_needed[all_X_needed %in% colnames(regdata_main)]

## Build X matrix with transformations
X_mat_rd <- regdata_main[X_avail_rd]
if ("trend_yr" %in% names(X_mat_rd)) {
  X_mat_rd <- X_mat_rd %>%
    mutate(trend_yr = trend_yr - min(trend_yr),
           trend_yr_sq = trend_yr^2,
           trend_season_sq = trend_season^2,
           trend_season_cb = trend_season^3)
}
if ("x_loc_zipcd_agi" %in% names(X_mat_rd)) {
  X_mat_rd <- X_mat_rd %>%
    mutate(zip_inc = x_loc_zipcd_agi / 1000,
           lg_zip_inc = log(x_loc_zipcd_agi)) %>%
    select(-x_loc_zipcd_agi)
}
if ("x_cred_score" %in% names(X_mat_rd)) {
  X_mat_rd <- X_mat_rd %>%
    mutate(x_cred_ind = ifelse(x_cred_score > CRED_SCORE_THRESHOLD, 0, 1),
           x_cred_score = ifelse(x_cred_score > CRED_SCORE_THRESHOLD, 195, x_cred_score))
}
if ("risk_class" %in% names(X_mat_rd)) {
  X_mat_rd <- X_mat_rd %>%
    mutate(rc = ifelse(risk_class <= 50, 50,
                       ifelse(risk_class >= 2000, 2000, risk_class)),
           lg_rc = log(rc), rc = rc / 100, rc_sq = rc^2, rc_cb = rc^3) %>%
    select(-risk_class)
}
if ("x_drvr_age_rated" %in% names(X_mat_rd)) {
  X_mat_rd <- X_mat_rd %>%
    mutate(age_young_ind = ifelse(x_drvr_age_rated < SIM_AGE_YOUNG, 1, 0),
           age_adult_ind = ifelse(x_drvr_age_rated >= SIM_AGE_ADULT, 1, 0),
           age_senior_ind = ifelse(x_drvr_age_rated >= SIM_AGE_SENIOR, 1, 0),
           age_sq = x_drvr_age_rated^2)
}
if ("x_drvr_yr_edu" %in% names(X_mat_rd)) {
  X_mat_rd <- X_mat_rd %>%
    mutate(edu_college_ind = ifelse(x_drvr_yr_edu >= SIM_EDU_COLLEGE, 1, 0),
           edu_postgrad_ind = ifelse(x_drvr_yr_edu >= SIM_EDU_POSTGRAD, 1, 0))
}
if ("x_veh_mdl_yr" %in% names(X_mat_rd)) {
  X_mat_rd <- X_mat_rd %>%
    mutate(x_veh_mdl_yr = ifelse(x_veh_mdl_yr < 1980, -20, x_veh_mdl_yr - 2000))
}
if ("tier_acci_drvr_pt" %in% names(X_mat_rd)) {
  sd_pt <- sd(X_mat_rd$tier_acci_drvr_pt[X_mat_rd$tier_acci_drvr_pt > 0])
  mean_pt <- mean(X_mat_rd$tier_acci_drvr_pt[X_mat_rd$tier_acci_drvr_pt > 0])
  X_mat_rd <- X_mat_rd %>%
    mutate(tier_good_ind = ifelse(tier_acci_drvr_pt == 0, 1, 0),
           tier_acci_drvr_pt = ifelse(tier_acci_drvr_pt == 0,
                                      -1, (tier_acci_drvr_pt - mean_pt) / sd_pt))
}

## Separate state FE
X_mat_factor <- NULL
if ("ST_CD" %in% names(X_mat_rd) && length(unique(X_mat_rd$ST_CD)) > 1) {
  X_mat_factor <- data.frame(ST_CD = factor(X_mat_rd$ST_CD))
  X_mat_rd <- X_mat_rd %>% select(-ST_CD)
} else {
  X_mat_rd <- X_mat_rd %>% select(-any_of("ST_CD"))
}

## Normalize continuous columns
cont_cols <- intersect(c("x_drvr_age_rated", "x_cred_score",
                         "x_loc_popltn_dens_pct", "tier_pop_lngth",
                         "zip_inc", "lg_zip_inc", "age_sq",
                         "rc", "lg_rc", "rc_sq", "rc_cb"),
                       colnames(X_mat_rd))
if (length(cont_cols) > 0) {
  X_mat_rd[cont_cols] <- scale(X_mat_rd[cont_cols])
}

## Run RD regressions
regdata_main <- regdata_main %>%
  mutate(run_var = qtr_since_ubi + abs(min(qtr_since_ubi)),
         run_var_sq = run_var^2,
         exceed_ind = (qtr_since_ubi >= 0) * 1,
         run_exceed_int = run_var * exceed_ind)

## Helper: drop factor columns with < 2 levels and constant numeric columns
drop_degenerate_cols <- function(df, keep_cols = character(0)) {
  for (col in setdiff(names(df), keep_cols)) {
    vals <- df[[col]]
    if (is.factor(vals) && nlevels(droplevels(vals)) < 2) {
      df[[col]] <- NULL
    } else if (is.numeric(vals) && length(unique(vals[!is.na(vals)])) < 2) {
      df[[col]] <- NULL
    }
  }
  df
}

reg_base_vars <- c("run_var", "exceed_ind", "run_exceed_int")
result_tbl <- matrix(0, 6, 6)

## Premium regressions
regdata_prem <- cbind.data.frame(regdata_main[c("prem_var", reg_base_vars)])
fit_base <- lm(log(prem_var) ~ ., data = regdata_prem)

X_no_trend <- X_mat_rd[!grepl("trend", colnames(X_mat_rd))]
if (!is.null(X_mat_factor)) {
  regdata_prem_x_no_t <- drop_degenerate_cols(
    cbind.data.frame(regdata_main[c("prem_var", reg_base_vars)], X_no_trend, X_mat_factor),
    keep_cols = c("prem_var", reg_base_vars))
  regdata_prem_x <- drop_degenerate_cols(
    cbind.data.frame(regdata_main[c("prem_var", reg_base_vars)], X_mat_rd, X_mat_factor),
    keep_cols = c("prem_var", reg_base_vars))
} else {
  regdata_prem_x_no_t <- drop_degenerate_cols(
    cbind.data.frame(regdata_main[c("prem_var", reg_base_vars)], X_no_trend),
    keep_cols = c("prem_var", reg_base_vars))
  regdata_prem_x <- drop_degenerate_cols(
    cbind.data.frame(regdata_main[c("prem_var", reg_base_vars)], X_mat_rd),
    keep_cols = c("prem_var", reg_base_vars))
}
fit_x_no_t <- lm(log(prem_var) ~ ., data = regdata_prem_x_no_t)
fit_x <- lm(log(prem_var) ~ ., data = regdata_prem_x)

result_tbl[1, ] <- c(1, 1, summary(fit_base)$coefficient["run_exceed_int", ])
result_tbl[2, ] <- c(2, 1, summary(fit_x_no_t)$coefficient["run_exceed_int", ])
result_tbl[3, ] <- c(3, 1, summary(fit_x)$coefficient["run_exceed_int", ])

## Claim regressions
regdata_clm <- cbind.data.frame(regdata_main[c("claim_var_cnt", reg_base_vars)])
fit_clm_base <- lm(claim_var_cnt ~ ., data = regdata_clm)

if (!is.null(X_mat_factor)) {
  regdata_clm_x_no_t <- drop_degenerate_cols(
    cbind.data.frame(regdata_main[c("claim_var_cnt", reg_base_vars)], X_no_trend, X_mat_factor),
    keep_cols = c("claim_var_cnt", reg_base_vars))
  regdata_clm_x <- drop_degenerate_cols(
    cbind.data.frame(regdata_main[c("claim_var_cnt", reg_base_vars)], X_mat_rd, X_mat_factor),
    keep_cols = c("claim_var_cnt", reg_base_vars))
} else {
  regdata_clm_x_no_t <- drop_degenerate_cols(
    cbind.data.frame(regdata_main[c("claim_var_cnt", reg_base_vars)], X_no_trend),
    keep_cols = c("claim_var_cnt", reg_base_vars))
  regdata_clm_x <- drop_degenerate_cols(
    cbind.data.frame(regdata_main[c("claim_var_cnt", reg_base_vars)], X_mat_rd),
    keep_cols = c("claim_var_cnt", reg_base_vars))
}
fit_clm_x_no_t <- lm(claim_var_cnt ~ ., data = regdata_clm_x_no_t)
fit_clm_x <- lm(claim_var_cnt ~ ., data = regdata_clm_x)

result_tbl[4, ] <- c(1, 2, summary(fit_clm_base)$coefficient["run_exceed_int", ])
result_tbl[5, ] <- c(2, 2, summary(fit_clm_x_no_t)$coefficient["run_exceed_int", ])
result_tbl[6, ] <- c(3, 2, summary(fit_clm_x)$coefficient["run_exceed_int", ])

result_tbl <- as.data.frame(result_tbl)
colnames(result_tbl) <- c("ctrl", "reg", "est", "se", "t", "p")

## Normalize claim coefficients to percentage terms
denom <- mean(filter(regdata_clm, regdata_main$run_var ==
                       (abs(min(regdata_main$qtr_since_ubi)) - 1))$claim_var_cnt)
if (is.na(denom) || denom == 0) denom <- mean(regdata_clm$claim_var_cnt)
result_tbl[4:6, 3:4] <- result_tbl[4:6, 3:4] / denom

result_tbl <- result_tbl %>%
  mutate(est_ub = est + 1.96 * se,
         est_lb = est - 1.96 * se,
         ctrl = factor(ctrl, labels = c("Raw", "X w/o Trend", "X Control")),
         reg = factor(reg, labels = c("Premium", "Claim")))

## Save CSV (main output)
write.csv(result_tbl[c("ctrl", "reg", "est", "se", "est_ub", "est_lb")],
          file.path(RF_CSV_DIR, "appendix", "fig_b1b.csv"), row.names = FALSE)
cat("  [GEN] appendix/fig_b1b.csv\n")

## Save JSON for backward compatibility
json_b1b <- list(models = list())
model_names <- c("prem_raw", "prem_x_no_t", "prem_x", "clm_raw", "clm_x_no_t", "clm_x")
fits <- list(fit_base, fit_x_no_t, fit_x, fit_clm_base, fit_clm_x_no_t, fit_clm_x)
for (i in seq_along(model_names)) {
  fit <- fits[[i]]
  co <- summary(fit)$coefficients
  coef_list <- list()
  for (j in seq_len(nrow(co))) {
    nm <- rownames(co)[j]
    coef_list[[nm]] <- list(
      estimate  = unname(co[j, 1]),
      std_error = unname(co[j, 2]),
      t_value   = unname(co[j, 3]),
      p_value   = unname(co[j, 4])
    )
  }
  model_entry <- list(
    n             = nobs(fit),
    r_squared     = summary(fit)$r.squared,
    adj_r_squared = summary(fit)$adj.r.squared,
    coefficients  = coef_list
  )
  ## v1 c1_get_rf_exhibits.R reads dep_var_mean from claim models to normalize
  if (grepl("^clm_", model_names[i])) {
    model_entry$dep_var_mean <- mean(fit$model[[1]])
  }
  json_b1b$models[[model_names[i]]] <- model_entry
}
writeLines(toJSON(json_b1b, auto_unbox = TRUE, pretty = TRUE, digits = 15),
           file.path(RF_REG_DIR, "appendix", "fig_b1b_regression.json"))
cat("  [GEN] appendix/fig_b1b_regression.json\n")

cat("=== sim_c5_selection_figures.R done ===\n\n")
