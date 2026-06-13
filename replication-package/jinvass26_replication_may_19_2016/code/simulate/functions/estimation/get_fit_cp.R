################################################################################
## get_fit_cp.R — Generate model fit figures for the paper
##
## Produces 4 CSVs directly in MODEL_FIT_DIR:
##   fig_b3.csv  (score vs lambda)
##   fig_6b.csv  (lambda density by TM status)
##   fig_b4.csv  (pricing by lambda, all regimes)
##   fig_6a.csv  (pricing by lambda, regime 3)
##
## Sourced by: code/results/sim_model_fit.R (or similar)
## Requires: get_c_latentparams.R, get_cp_pred.R already set up the environment
################################################################################

source('code/simulate/functions/estimation/get_c_latentparams.R')
source('code/simulate/functions/estimation/get_cp_pred.R')

## ---- Data prep (binning for all plots) --------------------------------------

llambda_bin = round(log(lambda_i)*10)/10; llambda_severe_bin = round(log(lambda_severe_i)*20)/20
lambda_bin = round(lambda_i * 100)/100; lambda_severe_bin = round(lambda_severe_i * 2000)/2000;
leps_bin = round(leps*40)/40; rc_bin = round(X_rc_i*20)/20

## ---- Figure B3 (fig_b3.csv) is written by fig_b3_override.R ----------------
##
## get_fit_cp.R is sourced both by sim_c7_model_fit.R (main model) and
## sim_c7b_model_fit_mhhet.R (mhhet model). If fig_b3.csv were written here,
## sim_c7b's later call would clobber sim_c7's version with mhhet-context
## values (different leps/llambda scale). We instead delegate fig_b3.csv
## entirely to fig_b3_override.R, which is sourced once from sim_c7 only —
## see that file's header for full rationale and isolation guarantees.

## ---- Suppress remaining figure-CSV writes when called from MH-het pipeline -
## sim_c7b_model_fit_mhhet.R sources this file to compute log_score_est for
## Tab C.2 score moments — but Fig 6a / Fig 6b / Fig B.4 are MAIN-model figures
## and should not be clobbered by mhhet-context values. The flag below gates
## those writes; mhhet still computes them (cheap, no side-effect), only the
## final write.csv is skipped. fig_b3 is already delegated to the override
## (see comment block above), so it's not in this gate.
.skip_fig_writes <- exists("model_name") && identical(model_name, "model_main_mhhet")

## ---- Figure 6b: Lambda by TM status (fit_lambda_by_tm.png) -----------------

llambda_i = llambda[1:I]
tm_R_ind = integer(I)
for(i in 1:I_tm_R){
  tm_R_ind[I_tm_R_to_I[i]] = 1
}

fig_6b_csv <- data.frame(llambda = llambda_i, monitoring = as.logical(tm_R_ind))
if (!.skip_fig_writes) {
  write.csv(fig_6b_csv, file.path(MODEL_FIT_DIR, "fig_6b.csv"), row.names = FALSE)
}

## ---- Figure 6c: Unobserved risk (leps) by TM status -------------------------

## ---- Pricing matrices (needed for Figures 6a, B4) ---------------------------

p_R_nb_mean = matrix(rep(0, I * D_R_nb_scheme), I, D_R_nb_scheme); colnames(p_R_nb_mean) = seq(D_R_nb_scheme)
p_R_tm_mean = matrix(rep(0, I * D_R_tm_scheme), I, D_R_tm_scheme); colnames(p_R_tm_mean) = paste0("tm", seq(D_R_tm_scheme))
p_R_nb_sd = matrix(rep(0, I * D_R_nb_scheme), I, D_R_nb_scheme); colnames(p_R_nb_sd) = seq(D_R_nb_scheme)
p_R_tm_sd = matrix(rep(0, I * D_R_tm_scheme), I, D_R_tm_scheme); colnames(p_R_tm_sd) = paste0("tm", seq(D_R_tm_scheme))
log_score_mean = matrix(rep(0, I * D_R_tm_scheme), I, D_R_tm_scheme); colnames(log_score_mean) = paste0("tm", seq(D_R_tm_scheme))
log_score_sd = matrix(rep(0, I * D_R_tm_scheme), I, D_R_tm_scheme); colnames(log_score_sd) = paste0("tm", seq(D_R_tm_scheme))

for(r in 1:D_R_nb_scheme){
  p_R_nb_mean[,r] = (theta_0_R_nb_mean[r] + log_X_rc[renw_cnt<1] * theta_1_R_nb_mean[r])
  p_R_nb_sd[,r] = theta_0_R_nb_sd[r]
}
for(r in 1:D_R_tm_scheme){
  if(is.matrix(theta_1_log_score_mean)){
    X_pricing_tm = cbind(leps, llambda[renw_cnt<1] - leps)
    log_score_mean[,r] = (theta_0_log_score_mean[r] + X_pricing_tm %*% theta_1_log_score_mean[r,])
  } else {
    X_pricing_tm = llambda[renw_cnt<1]
    log_score_mean[,r] = (theta_0_log_score_mean[r] + X_pricing_tm * theta_1_log_score_mean[r])
  }
  log_score_sd[,r] = theta_0_log_score_sd[r]
  X_tm_R = cbind(log_score_mean[,r], log_X_rc[renw_cnt<1])
  p_R_tm_mean[,r] = (theta_0_R_tm_mean[r] + X_tm_R %*% theta_1_R_tm_mean[r,])
  p_R_tm_sd[,r] = theta_0_R_tm_sd[r]
}

## ---- Figures 6a + B4: Pricing by lambda -------------------------------------

p5 = prices_raw[,3]; p5_i = p5[1:I];
df_p_nb = cbind.data.frame(llambda_severe_bin, llambda_bin, leps_bin, rc_bin,
                           p5_i * p_R_nb_mean,
                           p5_i * p_R_tm_mean
) %>% group_by(llambda_bin) %>% filter(n() > 10) %>% ungroup() %>%
  gather(rr, vals, -c("llambda_severe_bin", "llambda_bin", "leps_bin", "rc_bin")) %>%
  mutate(rr_type = ifelse(grepl("tm", rr), "tm", "nb"),
         rr_num = as.numeric(gsub(".*?([0-9]+).*", "\\1", rr)),
         rr = rr_num + D_R_nb_scheme*(rr_type=="tm")
  )

if(max(D_R_nb_scheme > 4)){
  df_p_nb = df_p_nb %>% mutate(
    period = ifelse(rr < 5, 1,
                    ifelse(rr < 7 | rr==D_R_nb_scheme+1, 2,
                           ifelse(rr < 12 | rr==D_R_nb_scheme+2, 3, 4))),
    period = factor(period, levels=seq(4), labels=paste("period:", c("pre-tm", "tm1", "tm2", "tm3")))
  )
} else {
  df_p_nb = df_p_nb %>% mutate(
    period = ifelse(rr == 1, 1, ifelse(rr %in% c(2, 4, 5), 2, 3)),
    period = factor(period, levels=seq(3), labels=paste("period:", c("pre-tm", "tm1-tm2", "tm3")))
  )
}

df_p_nb = df_p_nb %>% mutate(rr = factor(rr, levels = seq(D_R_nb_scheme + D_R_tm_scheme),
                                         labels = c(seq(D_R_nb_scheme), paste0("tm", seq(D_R_tm_scheme)))))

# Figure B4 CSV: all pricing regimes
fig_b4_csv <- df_p_nb %>% group_by(llambda_bin, rr, rr_type, period) %>%
  filter(n() > 50) %>%
  summarise(R_mean = mean(vals), se = sd(vals)/sqrt(n()), .groups = "drop")
if (!.skip_fig_writes) {
  write.csv(fig_b4_csv, file.path(MODEL_FIT_DIR, "fig_b4.csv"), row.names = FALSE)
}

# Figure 6a CSV: pricing regime 3 only
fig_6a_csv <- df_p_nb %>% group_by(llambda_bin, rr, rr_type, period) %>%
  filter(n() > 50 & grepl("3", period)) %>%
  summarise(R_mean = mean(vals), se = sd(vals)/sqrt(n()), .groups = "drop")
if (!.skip_fig_writes) {
  write.csv(fig_6a_csv, file.path(MODEL_FIT_DIR, "fig_6a.csv"), row.names = FALSE)
}
