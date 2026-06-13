################################################################################
## sim_c3_demand_elast.R — Demand elasticity figure (IV vs OLS)
##
## Produces:
##   RF_CSV_DIR/fig_5.csv                (Figure 5 data)
##   RF_REG_DIR/fig_5_regression.json    (backward-compatible JSON)
##   IMAGES_DIR/fig_5.png                (Figure 5: price elasticity by monitoring)
##
## Follows the exact pipeline from codes/rf/c3_demand_elast.R in the main repo.
################################################################################

if (!exists("TABLES_DIR")) source("code/config.R")
source("code/functions/helper.R")
source("code/simulate/functions/load_sim_data.R")
source("code/simulate/functions/data_clean/panel_renw_clean.R")
source("code/simulate/functions/data_clean/get_next_rrev.R")
source("code/functions/getX.R")

ipak(c("jsonlite"))

dir.create(RF_CSV_DIR, recursive = TRUE, showWarnings = FALSE)

cat("\n=== sim_c3_demand_elast.R ===\n")

#### 0. Load and clean data ####
data_states <- "IL"

panel <- load_sim_data("panel", data_states, 1) %>%
  mutate(dup = duplicated(paste(POL_ID_CHAR, RENW_CNT))) %>%
  filter(!dup) %>% mutate(dup = NULL)

panel_renw <- load_sim_data("panel_renw", data_states, 1)
panel_renw_cleaned <- panel_renw_clean(panel_renw, panel)
rm(panel_renw); gc()
panel_renw_cleaned <- panel_renw_cleaned %>%
  mutate(dup = duplicated(paste(POL_ID_CHAR, RENW_CNT))) %>%
  filter(!dup) %>% mutate(dup = NULL)

panel_ubi_renw <- load_sim_data("panel_ubi_renw", data_states, 1) %>%
  mutate(RENW_CNT = RENW_SFX_NBR - 1,
         tm_ind = ((UBI_ENROLL_IND + UBI_FIN_IND) > 0) * 1,
         tm_fin_ind = ((UBI_FIN_IND) > 0) * 1,
         log_tm_score = UBI_VALUE_MAX,
         tm_disc = UBI_DISC_FINAL,
         dup = duplicated(paste(POL_ID_CHAR, RENW_CNT))) %>%
  filter(!dup)

panel_ubi_renw_disc_ftr <- panel_ubi_renw[c('POL_ID_CHAR', 'RENW_CNT', 'tm_disc', 'UBI_DISC_30DAY')] %>%
  mutate(tm_disc = ifelse(!is.na(UBI_DISC_30DAY) & UBI_DISC_30DAY != 0,
                          pmax(UBI_DISC_30DAY, tm_disc), tm_disc),
         tm_disc_ftr = (1 - na_to_zero(tm_disc) / 100),
         UBI_DISC_30DAY = NULL)

#### 1. Process rate revisions ####
panel_renw_cleaned <- panel_renw_cleaned %>%
  mutate(date_rrev = as_date_70(date_rt_rev),
         date_pol_eff = as_date_70(date_pol_eff),
         post_rrev_diff = as.numeric(date_pol_eff - date_rrev),
         rrev_fe = paste(RENW_QT_ALPHA_ST_CD, RENW_QT_RATE_REV_ID, sep = "_")) %>%
  left_join(panel_ubi_renw_disc_ftr[c('POL_ID_CHAR', 'RENW_CNT', 'tm_disc_ftr')]) %>%
  mutate(tm_ftr = ifelse(is.na(tm_disc_ftr), 1, tm_disc_ftr),
         prem_renw_all = prem_renw_amt / tm_ftr)

panel_renw_cleaned <- panel_renw_cleaned %>% ungroup() %>%
  mutate(post_rrev_diff = as.numeric(date_pol_eff - date_rrev)) %>%
  group_by(rrev_fe, date_rrev) %>%
  mutate(new_rrev_date = min(date_pol_eff),
         max_date = max(date_pol_eff)) %>%
  ungroup()

panel_renw_cleaned <- get_next_rrev(panel_renw_cleaned, 'new_rrev_date', 'RENW_QT_ALPHA_ST_CD')

#### 2. Create event study regression data ####
regdata_renw <- panel_renw_cleaned %>%
  mutate(rrev_30_post_ind = (as.numeric(date_pol_eff - new_rrev_date) <= 30),
         rrev_30_pre_ind = (as.numeric(next_rrev_date - date_pol_eff) <= 30),
         pre_and_post_ind = (rrev_30_post_ind & rrev_30_pre_ind),
         renewed_ind = (RENW_QT_RENW_ACPT_IND == 'Y') * 1,
         month = as.numeric(substr(date_pol_eff, 6, 7)),
         year = as.numeric(substr(date_pol_eff, 1, 4)))

temp <- regdata_renw[regdata_renw$pre_and_post_ind, ]
regdata_renw <- regdata_renw %>% filter(!pre_and_post_ind) %>%
  rbind.data.frame(temp %>% mutate(rrev_30_post_ind = F)) %>%
  rbind.data.frame(temp %>% mutate(rrev_30_pre_ind = F)) %>%
  mutate(event_study_date = as_date_70(ifelse(rrev_30_post_ind, as.character(new_rrev_date),
                                               ifelse(rrev_30_pre_ind, as.character(next_rrev_date), NA))),
         event_study_id = factor(paste(ST_CD, event_study_date, sep = "_")))

X <- getX('reg')[[1]]

key_vars <- c("POL_ID_CHAR", "RENW_CNT")
renw_vars <- c("renewed_ind", "rrev_30_pre_ind", "rrev_30_post_ind", "event_study_date", "event_study_id",
               "prem_renw_amt", "prem_renw_chg_amt", "prem_renw_chg_pct", "panel_prem_expr_amt",
               "prem_renw_all", "tm_ftr", "month", "year")
nb_vars <- c("state_alpha", X,
             "cov_lim_ftr_bi", "cov_ind_fullcov", "cov_lim_ftr_coll", "cov_lim_ftr_comp", "cov_lim_ftr_pd",
             "prem_bi_wrt", "prem_bi_ern", "prem_expr")

## Only use columns that exist in each dataset
nb_vars <- intersect(nb_vars, colnames(panel))
renw_vars <- intersect(renw_vars, colnames(regdata_renw))

regdata <- panel[c(key_vars, nb_vars)] %>%
  left_join(regdata_renw[c(key_vars, renw_vars)]) %>%
  mutate(rrev_30_post_ind = rrev_30_post_ind * 1,
         rrev_30_pre_ind = rrev_30_pre_ind * 1,
         renw_cnt_ftr = factor(RENW_CNT),
         month_ftr = factor(month),
         year_ftr = factor(year),
         prem_renw_amt_bi = prem_bi_wrt * (1 + prem_renw_chg_pct / 100),
         tm_ind = (tm_ftr != 1) * 1.0,
         tm_srchg_ind = (tm_ftr > 1.0) * 1.0,
         tm_disc_large = (tm_ftr <= .9) * 1.0,
         tm_segment_ftr = factor(tm_ind * (-tm_srchg_ind + tm_disc_large)),
         lg_prem_increase = log(prem_renw_all) - log(prem_expr))

regdata_new <- filter(regdata, !is.na(renewed_ind) & (rrev_30_pre_ind + rrev_30_post_ind > 0))

cat("  Regression sample:", nrow(regdata_new), "obs\n")

## Only include X vars that actually exist in regdata_new
X_avail <- intersect(X, colnames(regdata_new))

#### 3. First stage: predict log premium increase ####
fs_formula <- paste("lg_prem_increase ~ tm_segment_ftr + factor(tm_ftr) + rrev_30_post_ind*event_study_id",
                    " + ", paste(X_avail, collapse = " + "),
                    "+ renw_cnt_ftr + cov_lim_ftr_bi + cov_ind_fullcov",
                    sep = "")
fsmod <- lm(fs_formula, data = regdata_new)
regdata_new$lg_prem_increase_hat <- NA_real_
regdata_new$lg_prem_increase_hat[as.integer(names(fsmod$fitted.values))] <- fsmod$fitted.values

#### 4. Second stage: IV regression ####
ss_formula <- paste("renewed_ind ~ tm_segment_ftr*lg_prem_increase_hat + log(tm_ftr) + event_study_id",
                    " + ", paste(X_avail, collapse = " + "),
                    "+ renw_cnt_ftr + cov_lim_ftr_bi + cov_ind_fullcov",
                    sep = "")
ssmod_iv <- lm(ss_formula, data = regdata_new)

#### 5. OLS regression ####
ols_formula <- paste("renewed_ind ~ tm_segment_ftr*lg_prem_increase + log(tm_ftr) + event_study_id",
                     " + ", paste(X_avail, collapse = " + "),
                     "+ renw_cnt_ftr + cov_lim_ftr_bi + cov_ind_fullcov",
                     sep = "")
olsmod <- lm(ols_formula, data = regdata_new)

#### 6. Build estimate table and figure ####
coefs_iv <- summary(ssmod_iv)$coefficients[, 1]
ses_iv <- summary(ssmod_iv)$coefficients[, 2]
param_interest_iv <- names(coefs_iv)[grepl("lg_prem_increase_hat", names(coefs_iv))]
index <- match(param_interest_iv, names(coefs_iv))
param_interest <- gsub("_hat", "", param_interest_iv)
est_tbl <- cbind.data.frame(coefs_iv[index], ses_iv[index], "IV", param_interest)
colnames(est_tbl)[1:3] <- c("est", "se", "method")
est_tbl[grepl("tm_segment_ftr", est_tbl$param_interest), "est"] <-
  est_tbl[grepl("tm_segment_ftr", est_tbl$param_interest), "est"] +
  est_tbl[!grepl("tm_segment_ftr", est_tbl$param_interest), "est"]

coefs <- summary(olsmod)$coefficients[, 1]
ses <- summary(olsmod)$coefficients[, 2]
index <- match(param_interest, names(coefs))
est <- coefs[index]; se <- ses[index]
temp <- cbind.data.frame(est, se, "OLS", param_interest)
colnames(temp)[3] <- "method"
temp[grepl("tm_segment_ftr", temp$param_interest), "est"] <-
  temp[grepl("tm_segment_ftr", temp$param_interest), "est"] +
  temp[!grepl("tm_segment_ftr", temp$param_interest), "est"]
est_tbl <- rbind.data.frame(est_tbl, temp)

df_est_tbl <- est_tbl %>%
  mutate(ub = est + 1.96 * se, lb = est - 1.96 * se) %>%
  mutate(param_interest = ifelse(grepl("0", param_interest), "unmonitored",
                                 ifelse(grepl("1", param_interest), "monitored w/ discount",
                                        "monitored w/o discount"))) %>%
  mutate(param_interest = factor(param_interest,
    levels = c("monitored w/ discount", "monitored w/o discount", "unmonitored")))

## CSV output
write.csv(df_est_tbl, file.path(RF_CSV_DIR, "fig_5.csv"), row.names = FALSE)
cat("  Saved CSV:", file.path(RF_CSV_DIR, "fig_5.csv"), "\n")

## JSON output (v1-compatible format for c1_get_rf_exhibits.R JSON derivation)
dir.create(RF_REG_DIR, recursive = TRUE, showWarnings = FALSE)

serialize_lm_coefs <- function(mod) {
  s <- summary(mod)$coefficients
  coefs <- list()
  for (nm in rownames(s)) {
    coefs[[nm]] <- list(estimate = s[nm, 1], std_error = s[nm, 2],
                        t_value = s[nm, 3], p_value = s[nm, 4])
  }
  list(coefficients = coefs)
}

result_json <- list(models = list(
  iv_second_stage = serialize_lm_coefs(ssmod_iv),
  ols             = serialize_lm_coefs(olsmod)
))

out_json <- file.path(RF_REG_DIR, "fig_5_regression.json")
writeLines(toJSON(result_json, auto_unbox = TRUE, pretty = TRUE, digits = 15), out_json)
cat("  [GEN] fig_5_regression.json\n")

## Plot
pd_narrow <- pd
pd_narrow$width <- 0.1

gg <-
  ggplot(data = df_est_tbl, aes(x = param_interest, group = method, color = method)) +
  geom_hline(yintercept = 0, alpha = 0.5, linetype = 2) +
  geom_errorbar(aes(ymin = lb, ymax = ub), width = 0, position = pd_narrow, alpha = 0.4) +
  geom_point(aes(y = est), position = pd_narrow) +
  fte_theme() + theme(legend.position = "right", panel.grid = element_blank()) +
  scale_color_manual(values = c("#E69F00", "#999999", "#56B4E9")) +
  coord_cartesian(ylim = c(-1.5, 0)) +
  xlab("Monitoring Group") +
  ylab("Price Elasticity Estimates")

dir.create(IMAGES_DIR, recursive = TRUE, showWarnings = FALSE)
out_file <- file.path(IMAGES_DIR, "fig_5.png")
ggsave(gg, file = out_file, width = 8, height = 3)
strip_png_dpi(out_file)
cat("Wrote", out_file, "\n")

## ---- Cleanup large objects to free memory ------------------------------------
rm(list = intersect(ls(), c("panel", "panel_renw", "panel_renw_cleaned",
                             "panel_ubi_renw", "regdata", "regdata_new", "regdata_renw",
                             "fsmod", "ssmod_iv", "olsmod")))
gc()

cat("=== sim_c3_demand_elast.R done ===\n\n")
