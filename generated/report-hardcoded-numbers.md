## Potentially Hardcoded Numeric Constants


We found the following set of hard coded numbers. This may be completely legitimate (parameter input, thresholds for computations, etc), and is hence only for information.

**/var/folders/5q/yhcyv3z55wvg6lhgc3h22kk00000gq/T/20210685-2/replication-package/jinvass26_replication_may_19_2016/code/config.R**

- Line 29, : SIGMA_LOGIT_LB         <- 0.025 # Lower bound on logit-scale preference heterogeneity
- Line 107, : TM_RESOURCE_COST_BASE  <- 0.035

**/var/folders/5q/yhcyv3z55wvg6lhgc3h22kk00000gq/T/20210685-2/replication-package/jinvass26_replication_may_19_2016/code/simulate/functions/ctf/get_ctf_util_profit.R**

- Line 4, : #   opt_in_factor: 0.967 (3.3% monitoring opt-in discount, set via k1 parameter)

**/var/folders/5q/yhcyv3z55wvg6lhgc3h22kk00000gq/T/20210685-2/replication-package/jinvass26_replication_may_19_2016/paper/text/appendix/sc_robust.tex**

- Line 69, : Compared to claims data, AFA records are delayed when claims involve complex adjustments that spill over into future periods. Moreover, some claims, especially minor ones, may be missing from AFA records due to administrative leniency or incomplete adjustments. Overall, the mean trailing-twelve-month AFA records for a renewal customer is 0.018, compared to the average claim records of 0.05 per six-month period.

**/var/folders/5q/yhcyv3z55wvg6lhgc3h22kk00000gq/T/20210685-2/replication-package/jinvass26_replication_may_19_2016/code/c2_get_param_tables.R**

- Line 379, : q2.5_mat[i, j]  <- quantile(values_finite, 0.025, names = FALSE)
- Line 380, : q97.5_mat[i, j] <- quantile(values_finite, 0.975, names = FALSE)
- Line 1297, : stars <- if (z > 2.576) "$^{***}$"
- Line 1298, : else if (z > 1.960) "$^{**}$"
- Line 1299, : else if (z > 1.645) "$^{*}$"

**/var/folders/5q/yhcyv3z55wvg6lhgc3h22kk00000gq/T/20210685-2/replication-package/jinvass26_replication_may_19_2016/code/simulate/functions/ctf/ctf_calibration.R**

- Line 231, : c(0.5, 0.3), c(0.2, 0.1), c(0.005, seq(0.01,0.05,by=0.02))

**/var/folders/5q/yhcyv3z55wvg6lhgc3h22kk00000gq/T/20210685-2/replication-package/jinvass26_replication_may_19_2016/code/simulate/simulate_data/sim_generate_data_list.R**

- Line 141, : qgrid_probs <- seq(0, 1, 0.001)  # 1001 points matching extract_data_list_profile.R
- Line 304, : # Real data has cor(log(X_rc), leps) ≈ +0.034
- Line 305, : target_cor <- 0.034
- Line 521, : # to steer CTF calibration toward original paper cost_factors (0.084, 0.025, 0.209).
- Line 1052, : dl$p_R_ftr_na <- rbinom(N_choice, 1, 0.087)
- Line 1055, : dl$p_R_ftr_wo_clm_R <- rnorm(N_R, 1.036, 0.1)
- Line 1057, : dl$p_R_ftr_wo_clm_w_tm <- rnorm(N_choice, 1.019, 0.1)
- Line 1058, : dl$p_R_ftr_wo_clm_w_tm_R <- rnorm(I_tm_R, 1.055, 0.1)
- Line 1063, : dl$p_R_ftr_w_tm_R <- rnorm(I_tm_R, 1.058, 0.1)
- Line 1097, : dl$d_t1 <- rnorm(N_choice, 1.458, 1)

**/var/folders/5q/yhcyv3z55wvg6lhgc3h22kk00000gq/T/20210685-2/replication-package/jinvass26_replication_may_19_2016/code/simulate/sim_c6c_estimate_cost_mhhet.R**

- Line 395, : sigma_logit_lb = sim_data_list$sigma_logit_lb %||% 0.025

**/var/folders/5q/yhcyv3z55wvg6lhgc3h22kk00000gq/T/20210685-2/replication-package/jinvass26_replication_may_19_2016/code/simulate/sim_c6_estimate.R**

- Line 131, : sigma_logit_lb = sim_data_list$sigma_logit_lb %||% 0.025

**/var/folders/5q/yhcyv3z55wvg6lhgc3h22kk00000gq/T/20210685-2/replication-package/jinvass26_replication_may_19_2016/code/functions/bootstrap_helpers.R**

- Line 73, : q2.5 = quantile(results, probs = 0.025, na.rm = TRUE),
- Line 74, : q97.5 = quantile(results, probs = 0.975, na.rm = TRUE)

**/var/folders/5q/yhcyv3z55wvg6lhgc3h22kk00000gq/T/20210685-2/replication-package/jinvass26_replication_may_19_2016/code/simulate/functions/estimation/extract_model_estimates_cmdstan.R**

- Line 208, : major_accident_expected_cost = (out_model$functions$m1_accident_oop(pareto_alpha_choice_base, 0.0001/dollar_norm) * lambda_severe ) * dollar_norm

**/var/folders/5q/yhcyv3z55wvg6lhgc3h22kk00000gq/T/20210685-2/replication-package/jinvass26_replication_may_19_2016/code/simulate/sim_c0_sum_stat.R**

- Line 163, : sum_stat_row(panel$prem_all,             "Total premium (\\$)",              "gt0",  c(0.001, 0.999), N_panel),
- Line 164, : sum_stat_row(panel$prem_liab_proprietary,"Liability premium (\\$)",          "gt0",  c(0.001, 0.999), N_panel),
- Line 165, : sum_stat_row(panel$risk_class,           "Risk class (\\$)",                 "gt0",  c(0, 0.999),     N_panel),

**/var/folders/5q/yhcyv3z55wvg6lhgc3h22kk00000gq/T/20210685-2/replication-package/jinvass26_replication_may_19_2016/code/simulate/simulate_data/sim_generate_rf_data.R**

- Line 53, : ## (e.g., as.numeric("29.97")/100 rather than round(0.2997119, 4)).
- Line 85, : # e.g., log(1 - 0.2997) ≈ -0.356
- Line 247, : panel_rows$clm_srchg <- 0.10 + 0.35 * pt_norm + rnorm(N_ROWS, 0, 0.015)
- Line 473, : AAF = 0.004, AAF_MINOR = 0.002, DWI = 0.001,
- Line 474, : DWI_MAJ = 0.0004, SPD = 0.01, NAF = 0.006

**/var/folders/5q/yhcyv3z55wvg6lhgc3h22kk00000gq/T/20210685-2/replication-package/jinvass26_replication_may_19_2016/paper/text/s5_estimates.tex**

- Line 50, : Accidents are rare: the average consumer in our panel faces  0.039  accidents per six-month period in expectation. The distribution is right-skewed:
- Line 51, : the riskiest 5\% of drivers expect 0.113 accidents per period---about three times more than the average.  Among accidents, major ones---defined as those with Pareto losses greater than \$10{,}000---account for  10.88\%.  Combining the accident occurrence models for minor and major accidents with their respective severity models (Equations \ref{eq-claim-severity-model}), the expected cost of fully insuring a consumer is  \$205.23,  of which  \$113.62  is due to major accidents. If a consumer chooses two of the most popular plans, \$40{,}000 or \$50{,}000 coverage limits, their expected out-of-pocket expenditures would be  \$14.26  and  \$10.66, respectively.

**/var/folders/5q/yhcyv3z55wvg6lhgc3h22kk00000gq/T/20210685-2/replication-package/jinvass26_replication_may_19_2016/code/c3_get_fitctf_exhibits.R**

- Line 281, : "Compared to Tab. \\ref{tab:fit_c}, the only change detectable (more than 0.001) are the monitoring score moment predictions. \\par}\n",

**/var/folders/5q/yhcyv3z55wvg6lhgc3h22kk00000gq/T/20210685-2/replication-package/jinvass26_replication_may_19_2016/code/simulate/functions/data_clean/clean_choice_panel.R**

- Line 20, : , score_inaccurate = ifelse(((ST_CD == 42 & ubi_sum_val_final_max %in% c(4.084, 4.436)) | ## PA: stayed with 10% disc
- Line 21, : (ST_CD == 34 & ubi_sum_val_final_max %in% c(3.374, 4.378)) | ## NJ: stayed 100%, 12%/1% disc
- Line 22, : (ST_CD == 48 & ubi_sum_val_final_max %in% c(3.585))), ## TX: stayed with 10% disc
- Line 27, : , ubi_attrit_score_ind = (ST_CD == 42 & ubi_sum_val_final_max == 1.447 & ubi_sum_tier_final_max == 1) ## this is all attrition, corresponding to the 1.447 score /  1.447 = attrit + disc 20%,

**/var/folders/5q/yhcyv3z55wvg6lhgc3h22kk00000gq/T/20210685-2/replication-package/jinvass26_replication_may_19_2016/code/simulate/functions/estimation/get_c_latentparams.R**

- Line 65, : gg_lambda = ggplot(data=as.data.frame(lambda), aes(x=lambda)) + geom_histogram(binwidth = 0.005)

**/var/folders/5q/yhcyv3z55wvg6lhgc3h22kk00000gq/T/20210685-2/replication-package/jinvass26_replication_may_19_2016/paper/text/s3_reduced_form.tex**

- Line 21, : \Cref{tab:mh_results}. We find a large moral hazard effect. Column 3 corresponds to the specification in \Cref{eq:mh_reg}, which shows that the average claim count for monitored consumers is 0.007 or 21.93\% higher after the monitoring period. Adjusting for the average monitoring duration of first-period monitoring finishers of 131 days---only a fraction of the monitoring period---the moral hazard effect would be 29.97\%. This result is stable across specifications and only gets slightly bigger as we add controls; for instance, adding additional variation in monitoring duration (treatment intensity) generates a similar result of 33.08\% (derived by setting $z=1$ and adding up the two interaction coefficients in column 7). We test for parallel trends between the monitored and unmonitored groups by repeating our main specification in balanced panels encompassing subsequent unmonitored periods, which also serves as a placebo check. As columns 9-11 show, no differential claim change across periods can be detected.

