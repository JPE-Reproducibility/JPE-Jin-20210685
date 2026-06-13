################################################################################
## sim_c2_mh_viol.R — Violation figure (AAF violations by monitoring status)
##
## Produces:
##   RF_CSV_DIR/appendix/fig_c4.csv                (Figure C4 plot data)
##   RF_REG_DIR/appendix/fig_c4_regression.json     (backward compat)
##
## Mirrors: codes/rf/c2_mh_viol.R from the main repo
################################################################################

if (!exists("TABLES_DIR")) source("code/config.R")
source("code/functions/helper.R")
source("code/simulate/functions/load_sim_data.R")

ipak("jsonlite")

## ---- Load data -------------------------------------------------------------

data_states <- "3_state"

panel_viol <- load_sim_data("panel_viol", data_states, 1)

panel_viol_clean <- panel_viol %>% mutate(
  viol_aaf_lag = (na_to_zero(VIOL_AAF_1) + na_to_zero(VIOL_AAF_MINOR_1) > 0),
  viol_aaf_lag2 = (na_to_zero(VIOL_AAF_2) + na_to_zero(VIOL_AAF_MINOR_2) > 0) - viol_aaf_lag,
  viol_dwi_lag = na_to_zero(VIOL_DWI_1) + na_to_zero(VIOL_DWI_MAJ_1),
  viol_spd_lag = na_to_zero(VIOL_SPD_1),
  viol_naf_lag = na_to_zero(VIOL_NAF_1)
)
lag_cols <- colnames(panel_viol_clean)[grepl("lag", colnames(panel_viol_clean))]
panel_viol_clean <- as.data.frame(panel_viol_clean)[c("POL_ID_CHAR", "RENW_CNT", lag_cols)]

panel_viol_clean$dup <- duplicated(panel_viol_clean[c("POL_ID_CHAR", "RENW_CNT")])
panel_viol_clean <- panel_viol_clean %>% filter(!dup)
panel_viol_clean$dup <- NULL

## ---- Build regdata_graph_mh inline (simulated mode) ------------------------
## Follows main repo c2_mh_viol.R lines 40-69

panel_3st <- load_sim_data("panel", data_states, 1)
panel_3st <- panel_3st[!duplicated(panel_3st[c("POL_ID_CHAR", "RENW_CNT")]), ]
panel_3st <- panel_3st[panel_3st$RENW_CNT <= 5, ]
df_mh <- load_sim_data("df_mh", "us", 1)
n_3st <- nrow(panel_3st)
regdata_graph_mh <- panel_3st %>%
  left_join(df_mh %>% dplyr::select(POL_ID_CHAR, ubi_fin_ind), by = "POL_ID_CHAR") %>%
  mutate(
    ubi_ind = as.logical(na_to_zero(ubi_fin_ind)),
    trend_yr = as.numeric(substr(as.character(date_pol_eff), 1, 4)),
    clm_acci = clm_cnt,
    RENW_CNT = factor(RENW_CNT + 1)
  ) %>%
  dplyr::select(-ubi_fin_ind)
## Fill missing X covariates with random data (simulated panel may lack these)
x_vars <- c("x_drvr_is_female", "x_drvr_lic_yr", "x_drvr_hm_own_ind",
            "x_drvr_lic_oos_ind", "x_cred_clue_ord_ind", "x_veh_ls_pay_ind",
            "x_veh_abs_ind", "x_veh_sd_ind", "x_veh_len_own",
            "x_veh_class_C_ind", "x_loc_grg_verify_ind", "tier_pop_yes_ind",
            "tier_pop_some_ind", "tier_acci_tot_aaf_cnt", "tier_acci_dui_cnt",
            "x_cred_ind", "age_young_ind", "age_adult_ind", "age_senior_ind",
            "edu_college_ind", "edu_postgrad_ind", "x_cred_score",
            "x_loc_popltn_dens_pct", "tier_acci_drvr_pt", "zip_inc",
            "x_veh_mdl_yr", "tier_pref_ind")
for (xv in x_vars) {
  if (!xv %in% colnames(regdata_graph_mh))
    regdata_graph_mh[[xv]] <- rnorm(n_3st)
}
rm(panel_3st, df_mh)

## ---- Join violation lags ---------------------------------------------------

regdata_graph_mh <- regdata_graph_mh %>% mutate(RENW_CNT = as.numeric(RENW_CNT)-1) %>%
  left_join(panel_viol_clean)

regdata_graph_mh$dup <- duplicated(regdata_graph_mh[c("POL_ID_CHAR", "RENW_CNT")])
regdata_graph_mh <- regdata_graph_mh %>% filter(!dup)
regdata_graph_mh$dup <- NULL

pol_na_viol <- unique(filter(regdata_graph_mh, is.na(viol_aaf_lag))$POL_ID_CHAR)

## ---- Build regression data: RENW_CNT mapping {1,4,5} -> {1,2,3} -----------

df_lag1 <- regdata_graph_mh %>% filter(RENW_CNT < 1) %>% mutate(year = 0, trend_yr = trend_yr, trend_season = NULL,
                                                                viol_aaf_lag = viol_aaf_lag)

df_reg <- regdata_graph_mh %>% filter(RENW_CNT %in% c(1,4,5)) %>%
  mutate(trend_season = NULL, trend_yr = as.numeric(trend_yr),
         year = ifelse(RENW_CNT<3,RENW_CNT,RENW_CNT-2)) %>%
  rbind.data.frame(df_lag1) %>%
  mutate(renw_cnt = as.factor(year),
         dv = viol_aaf_lag) %>%
  filter(!(POL_ID_CHAR %in% pol_na_viol)) %>%
  left_join(df_lag1 %>% dplyr::select(c("POL_ID_CHAR", "viol_aaf_lag")) %>% rename(viol_aaf_lag_agg = viol_aaf_lag))

## ---- Regression ------------------------------------------------------------

X <- "x_drvr_is_female+x_drvr_lic_yr+x_drvr_hm_own_ind+x_drvr_lic_oos_ind+
      x_cred_clue_ord_ind+x_veh_ls_pay_ind+x_veh_abs_ind+x_veh_sd_ind+x_veh_len_own+
      x_veh_class_C_ind+x_loc_grg_verify_ind+tier_pop_yes_ind+tier_pop_some_ind+
      tier_acci_tot_aaf_cnt+tier_acci_dui_cnt+x_cred_ind+age_young_ind+age_adult_ind+
      age_senior_ind+edu_college_ind+edu_postgrad_ind+x_cred_score+x_loc_popltn_dens_pct+
      tier_acci_drvr_pt+zip_inc+x_veh_mdl_yr+trend_yr"

# Run with agg control (this is the version used in paper)
lm_fit <- lm(as.formula(paste("dv ~ ubi_ind + renw_cnt + ubi_ind*renw_cnt + viol_aaf_lag_agg + ", X, sep = "")),
             # Restrict to renewals with >= 3 years license history
             data = filter(df_reg, RENW_CNT > 0 & x_drvr_lic_yr >= 3))

coefs <- summary(lm_fit)$coefficients[,1]
ses <- summary(lm_fit)$coefficients[,2]
index <- match(c("ubi_indTRUE", "renw_cnt1", "renw_cnt2","renw_cnt3",
                 "ubi_indTRUE:renw_cnt1", "ubi_indTRUE:renw_cnt2","ubi_indTRUE:renw_cnt3"),
               names(coefs))

est <- coefs[index]; se <- ses[index]

## ---- Build vis_tbl (CSV output) --------------------------------------------

vis_tbl <- as.data.frame(matrix(0,8,5))
colnames(vis_tbl) <- c("period", "group", "est", "ub", "lb")
vis_tbl$period <- c(0:3,0:3)
vis_tbl$group <- c(rep("unmonitored", 4),rep("monitored", 4))
est_unmon <- c(0,est[2:4])
est_mon <- c(est[1],(est[1] + est[2:4] + est[5:7]))
vis_tbl$est <- c(est_unmon, est_mon)
vis_tbl$ub <- vis_tbl$est + 1.96*c(c(0,se[2:4]),c(se[1], se[5:7]))
vis_tbl$lb <- vis_tbl$est - 1.96*c(c(0,se[2:4]),c(se[1], se[5:7]))

vis_tbl <- vis_tbl |>
  dplyr::mutate(segment = ifelse(period < 1, "pre", "post"),
                segment_group = paste(group, segment))

## ---- Write CSV output ------------------------------------------------------

dir.create(file.path(RF_CSV_DIR, "appendix"), recursive = TRUE, showWarnings = FALSE)
write.csv(vis_tbl, file.path(RF_CSV_DIR, "appendix", "fig_c4.csv"), row.names = FALSE)
cat("  Saved CSV:", file.path(RF_CSV_DIR, "appendix", "fig_c4.csv"), "\n")

## ---- Write JSON output (backward compatibility) ----------------------------

dir.create(file.path(RF_REG_DIR, "appendix"), recursive = TRUE, showWarnings = FALSE)

sm <- summary(lm_fit)
cf <- coef(sm)
fig_c4_out <- list(
  formula = paste(deparse(formula(lm_fit)), collapse = " "),
  n = nobs(lm_fit),
  r_squared = sm$r.squared,
  adj_r_squared = sm$adj.r.squared,
  residual_se = sm$sigma,
  dep_var_mean = mean(filter(df_reg, RENW_CNT > 0 & x_drvr_lic_yr >= 3)$dv, na.rm = TRUE),
  sample_description = "violation progression, 3-state, experienced drivers"
)
coefficients <- list()
for (i in seq_len(nrow(cf))) {
  nm <- rownames(cf)[i]
  coefficients[[nm]] <- list(
    estimate  = cf[i, "Estimate"],
    std_error = cf[i, "Std. Error"],
    t_value   = cf[i, "t value"],
    p_value   = cf[i, "Pr(>|t|)"]
  )
}
fig_c4_out$coefficients <- coefficients

write_json(fig_c4_out, file.path(RF_REG_DIR, "appendix", "fig_c4_regression.json"),
           pretty = TRUE, auto_unbox = TRUE, digits = 15)
cat("  Saved JSON:", file.path(RF_REG_DIR, "appendix", "fig_c4_regression.json"), "\n")

## ---- Cleanup ---------------------------------------------------------------

rm(panel_viol, panel_viol_clean, regdata_graph_mh, df_lag1, df_reg, lm_fit, vis_tbl, fig_c4_out)
gc()

cat("sim_c2_mh_viol.R completed successfully.\n")
