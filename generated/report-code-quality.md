## Code Quality

### R

[ADVISORY] `filter(` call not preceded by a comment within 2 lines — consider adding a comment explaining the criterion. (c1_get_rf_exhibits.R, line 504)
  → gg <- ggplot(filter(df_trend, mon_since_ubi > num_mon_pre &

[ADVISORY] `filter(` call not preceded by a comment within 2 lines — consider adding a comment explaining the criterion. (bootstrap_helpers.R, line 51)
  → filter(parameter == parameter_name) %>%

[ADVISORY] `filter(` call not preceded by a comment within 2 lines — consider adding a comment explaining the criterion. (bootstrap_helpers.R, line 64)
  → filter(parameter == parameter_name) %>%

[ADVISORY] `filter(` call not preceded by a comment within 2 lines — consider adding a comment explaining the criterion. (bootstrap_helpers.R, line 124)
  → filter(parameter %in% params, statistic %in% stats) %>%

[ADVISORY] `filter(` call not preceded by a comment within 2 lines — consider adding a comment explaining the criterion. (fig_b3_override.R, line 87)
  → dplyr::filter(dplyr::n() > 10) |>

[ADVISORY] `filter(` call not preceded by a comment within 2 lines — consider adding a comment explaining the criterion. (fig_b3_override.R, line 93)
  → dplyr::filter(dplyr::n() > 25) |>

[ADVISORY] `filter(` call not preceded by a comment within 2 lines — consider adding a comment explaining the criterion. (sim_c1_mh_regression.R, line 114)
  → df_reg_save <- filter(df_mh_save, last_renewal_seen >= 2)[c(reg_cols, "clm_acci")] %>% left_join(filter(XY_mat, RENW_CNT == 0))

[ADVISORY] `filter(` call not preceded by a comment within 2 lines — consider adding a comment explaining the criterion. (sim_c1_mh_regression.R, line 118)
  → temp <- df_mh[c(reg_cols, claim_varname)] %>% left_join(filter(XY_mat, RENW_CNT == q))

[ADVISORY] `filter(` call not preceded by a comment within 2 lines — consider adding a comment explaining the criterion. (sim_c1_mh_regression.R, line 120)
  → temp <- temp %>% filter(!is.na(clm_acci))

[ADVISORY] `filter(` call not preceded by a comment within 2 lines — consider adding a comment explaining the criterion. (sim_c1_mh_regression.R, line 130)
  → mon_period_mean <- mean((df_reg_save %>% filter(RENW_CNT == 0 & ubi_ind > 0))$clm_acci, na.rm=T)

[ADVISORY] `filter(` call not preceded by a comment within 2 lines — consider adding a comment explaining the criterion. (sim_c1_mh_regression.R, line 131)
  → temp <- df_reg_save %>% filter(RENW_CNT < 1 & ubi_ind > 0) %>% left_join(df_mh_save[c("POL_ID_CHAR", "days_connect_raw")])

[ADVISORY] `filter(` call not preceded by a comment within 2 lines — consider adding a comment explaining the criterion. (sim_c1_mh_regression.R, line 160)
  → df_reg <- df_reg %>% filter(RENW_CNT %in% c(0,1,2))

[ADVISORY] `filter(` call not preceded by a comment within 2 lines — consider adding a comment explaining the criterion. (sim_c1_mh_regression.R, line 162)
  → df_reg_ind <- df_reg %>% filter(!(ubi_start_ind==1 & ubi_ind==0)) %>%

[ADVISORY] `filter(` call not preceded by a comment within 2 lines — consider adding a comment explaining the criterion. (sim_c1_mh_regression.R, line 194)
  → df_reg <- df_reg_save %>% filter(RENW_CNT > 0) %>%

[ADVISORY] `filter(` call not preceded by a comment within 2 lines — consider adding a comment explaining the criterion. (sim_c1_mh_regression.R, line 195)
  → filter(!(ubi_start_ind==1 & ubi_ind==0))

[ADVISORY] `filter(` call not preceded by a comment within 2 lines — consider adding a comment explaining the criterion. (sim_c1_mh_regression.R, line 202)
  → df_reg_pt <- df_reg_pt %>% filter(RENW_CNT %in% c(p,(p+1)) & last_renewal_seen >= (p+1))

[ADVISORY] `filter(` call not preceded by a comment within 2 lines — consider adding a comment explaining the criterion. (sim_c1_mh_regression.R, line 362)
  → df_reg <- df_reg %>% filter(last_renewal_seen > 4)

[ADVISORY] `filter(` call not preceded by a comment within 2 lines — consider adding a comment explaining the criterion. (sim_c1_mh_regression.R, line 437)
  → left_join(filter(XY_mat[c("POL_ID_CHAR", "RENW_CNT", X, Y)], RENW_CNT == 0) %>%

[ADVISORY] `filter(` call not preceded by a comment within 2 lines — consider adding a comment explaining the criterion. (sim_c1_mh_regression.R, line 446)
  → df_reg <- df_reg_save %>% filter(as.numeric(RENW_CNT) <= 5) %>%

[ADVISORY] `filter(` call not preceded by a comment within 2 lines — consider adding a comment explaining the criterion. (sim_c1_mh_regression.R, line 512)
  → df_reg <- df_reg_save %>% filter(RENW_CNT == 0) %>%

[ADVISORY] `filter(` call not preceded by a comment within 2 lines — consider adding a comment explaining the criterion. (sim_c1_mh_regression.R, line 513)
  → filter(!(ubi_start_ind==1 & ubi_fin120_ind==0)) %>%

[ADVISORY] `filter(` call not preceded by a comment within 2 lines — consider adding a comment explaining the criterion. (sim_c2_mh_viol.R, line 34)
  → panel_viol_clean <- panel_viol_clean %>% filter(!dup)

[ADVISORY] `filter(` call not preceded by a comment within 2 lines — consider adding a comment explaining the criterion. (sim_c2_mh_viol.R, line 76)
  → regdata_graph_mh <- regdata_graph_mh %>% filter(!dup)

[ADVISORY] `filter(` call not preceded by a comment within 2 lines — consider adding a comment explaining the criterion. (sim_c2_mh_viol.R, line 79)
  → pol_na_viol <- unique(filter(regdata_graph_mh, is.na(viol_aaf_lag))$POL_ID_CHAR)

[ADVISORY] `filter(` call not preceded by a comment within 2 lines — consider adding a comment explaining the criterion. (sim_c2_mh_viol.R, line 86)
  → df_reg <- regdata_graph_mh %>% filter(RENW_CNT %in% c(1,4,5)) %>%

[ADVISORY] `filter(` call not preceded by a comment within 2 lines — consider adding a comment explaining the criterion. (sim_c2_mh_viol.R, line 92)
  → filter(!(POL_ID_CHAR %in% pol_na_viol)) %>%

[ADVISORY] `filter(` call not preceded by a comment within 2 lines — consider adding a comment explaining the criterion. (sim_c2_mh_viol.R, line 151)
  → dep_var_mean = mean(filter(df_reg, RENW_CNT > 0 & x_drvr_lic_yr >= 3)$dv, na.rm = TRUE),

[ADVISORY] `filter(` call not preceded by a comment within 2 lines — consider adding a comment explaining the criterion. (sim_c3_demand_elast.R, line 30)
  → filter(!dup) %>% mutate(dup = NULL)

[ADVISORY] `filter(` call not preceded by a comment within 2 lines — consider adding a comment explaining the criterion. (sim_c3_demand_elast.R, line 37)
  → filter(!dup) %>% mutate(dup = NULL)

[ADVISORY] `filter(` call not preceded by a comment within 2 lines — consider adding a comment explaining the criterion. (sim_c3_demand_elast.R, line 46)
  → filter(!dup)

[ADVISORY] `filter(` call not preceded by a comment within 2 lines — consider adding a comment explaining the criterion. (sim_c3_demand_elast.R, line 83)
  → regdata_renw <- regdata_renw %>% filter(!pre_and_post_ind) %>%

[ADVISORY] `filter(` call not preceded by a comment within 2 lines — consider adding a comment explaining the criterion. (sim_c3_demand_elast.R, line 118)
  → regdata_new <- filter(regdata, !is.na(renewed_ind) & (rrev_30_pre_ind + rrev_30_post_ind > 0))

[ADVISORY] `filter(` call not preceded by a comment within 2 lines — consider adding a comment explaining the criterion. (sim_c4_data_figures.R, line 42)
  → filter(!dup) %>%

[ADVISORY] `filter(` call not preceded by a comment within 2 lines — consider adding a comment explaining the criterion. (sim_c4_data_figures.R, line 47)
  → filter(!is.na(tm_disc))

[ADVISORY] `filter(` call not preceded by a comment within 2 lines — consider adding a comment explaining the criterion. (sim_c4_data_figures.R, line 56)
  → filter(last_renewal_seen >= 5 & !(mean_0 == 0 & mean_1 != 0)) %>%

[ADVISORY] `filter(` call not preceded by a comment within 2 lines — consider adding a comment explaining the criterion. (sim_c4_data_figures.R, line 58)
  → filter(RENW_CNT <= 5) %>%

[ADVISORY] `filter(` call not preceded by a comment within 2 lines — consider adding a comment explaining the criterion. (sim_c4_data_figures.R, line 82)
  → filter(!duplicated(paste(POL_ID_CHAR, RENW_CNT)))

[ADVISORY] `filter(` call not preceded by a comment within 2 lines — consider adding a comment explaining the criterion. (sim_c4_data_figures.R, line 102)
  → filter(RENW_CNT == 0) %>%

[ADVISORY] `filter(` call not preceded by a comment within 2 lines — consider adding a comment explaining the criterion. (sim_c4_data_figures.R, line 106)
  → filter(RENW_CNT == 0) %>%

[ADVISORY] `filter(` call not preceded by a comment within 2 lines — consider adding a comment explaining the criterion. (sim_c4_data_figures.R, line 125)
  → filter(RENW_CNT == 0) %>%

[ADVISORY] `filter(` call not preceded by a comment within 2 lines — consider adding a comment explaining the criterion. (sim_c4_data_figures.R, line 130)
  → filter(RENW_CNT == 0) %>%

[ADVISORY] `filter(` call not preceded by a comment within 2 lines — consider adding a comment explaining the criterion. (sim_c4_data_figures.R, line 136)
  → filter(RENW_CNT == 0) %>%

[ADVISORY] `filter(` call not preceded by a comment within 2 lines — consider adding a comment explaining the criterion. (sim_c4_data_figures.R, line 156)
  → filter(!p_R_ftr_na)

[ADVISORY] `filter(` call not preceded by a comment within 2 lines — consider adding a comment explaining the criterion. (sim_c4_data_figures.R, line 159)
  → filter(tm_fin_ind == 0) %>%

[ADVISORY] `filter(` call not preceded by a comment within 2 lines — consider adding a comment explaining the criterion. (sim_c4_data_figures.R, line 162)
  → filter(tm_fin_ind == 1) %>%

[ADVISORY] `filter(` call not preceded by a comment within 2 lines — consider adding a comment explaining the criterion. (sim_c4_data_figures.R, line 165)
  → filter(tm_fin_ind == 1) %>%

[ADVISORY] `filter(` call not preceded by a comment within 2 lines — consider adding a comment explaining the criterion. (sim_c4_data_figures.R, line 277)
  → filter(RENW_CNT == 0 & UBI_FIN_IND == 1) %>%

[ADVISORY] `filter(` call not preceded by a comment within 2 lines — consider adding a comment explaining the criterion. (sim_c4_data_figures.R, line 279)
  → filter(!is.na(log_tm_score) & log_tm_score > 0.1)

[ADVISORY] `filter(` call not preceded by a comment within 2 lines — consider adding a comment explaining the criterion. (sim_c4_data_figures.R, line 314)
  → filter(!is.na(clm_acci_renw_1))

[ADVISORY] `filter(` call not preceded by a comment within 2 lines — consider adding a comment explaining the criterion. (sim_c4_data_figures.R, line 337)
  → filter(ubi_fin_ind == 1 & !is.na(ubi_groups) &

[ADVISORY] `filter(` call not preceded by a comment within 2 lines — consider adding a comment explaining the criterion. (sim_c5_selection_figures.R, line 95)
  → filter(!is.na(claim_var)) %>%

[ADVISORY] `filter(` call not preceded by a comment within 2 lines — consider adding a comment explaining the criterion. (sim_c5_selection_figures.R, line 101)
  → left_join(mutate(filter(X_mat, RENW_CNT == xmat_t), RENW_CNT = NULL),

[ADVISORY] `filter(` call not preceded by a comment within 2 lines — consider adding a comment explaining the criterion. (sim_c5_selection_figures.R, line 104)
  → avg_clm <- mean(filter(regdata, ubi_fin_ind == 1)$claim_var)

[ADVISORY] `filter(` call not preceded by a comment within 2 lines — consider adding a comment explaining the criterion. (sim_c5_selection_figures.R, line 199)
  → filter(!dup) %>%

[ADVISORY] `filter(` call not preceded by a comment within 2 lines — consider adding a comment explaining the criterion. (sim_c5_selection_figures.R, line 217)
  → filter(!(ST_CD == st_cd & (as_date_70(date_pol_eff) %in% as_date_70(skip_dates))))

[ADVISORY] `filter(` call not preceded by a comment within 2 lines — consider adding a comment explaining the criterion. (sim_c5_selection_figures.R, line 243)
  → filter(min_ubi_date_all >= as_date_70("2012-01-01") + 30.5 * (-num_mon_pre - 1) &

[ADVISORY] `filter(` call not preceded by a comment within 2 lines — consider adding a comment explaining the criterion. (sim_c5_selection_figures.R, line 259)
  → filter(min_ubi_date_all >= as_date_70("2012-01-01") + 365 / 4 * t_T_pre &

[ADVISORY] `filter(` call not preceded by a comment within 2 lines — consider adding a comment explaining the criterion. (sim_c5_selection_figures.R, line 265)
  → filter(!is.na(min_ubi_date_all)) %>%

[ADVISORY] `filter(` call not preceded by a comment within 2 lines — consider adding a comment explaining the criterion. (sim_c5_selection_figures.R, line 267)
  → filter(qtr_since_ubi < t_T_rd & qtr_since_ubi >= -t_T_pre) %>%

[ADVISORY] `filter(` call not preceded by a comment within 2 lines — consider adding a comment explaining the criterion. (sim_c5_selection_figures.R, line 268)
  → filter(ubi_fin_ind == 0) %>%

[ADVISORY] `filter(` call not preceded by a comment within 2 lines — consider adding a comment explaining the criterion. (sim_c5_selection_figures.R, line 272)
  → filter(prem_var > 30) %>%

[ADVISORY] `filter(` call not preceded by a comment within 2 lines — consider adding a comment explaining the criterion. (sim_c5_selection_figures.R, line 273)
  → filter(!is.na(claim_var_cnt))

[ADVISORY] `filter(` call not preceded by a comment within 2 lines — consider adding a comment explaining the criterion. (ctf_equi_k_save_grid.R, line 170)
  → stage_grid = unique(round(stage_grid,2)) %>% filter(k0  >= hard_coded_lower_limits[1] &

[ADVISORY] `filter(` call not preceded by a comment within 2 lines — consider adding a comment explaining the criterion. (ctf_equi_k_save_grid.R, line 261)
  → stage_grid = unique(round(stage_grid,2)) %>% filter(k0  >= hard_coded_lower_limits[1] &

[ADVISORY] `filter(` call not preceded by a comment within 2 lines — consider adding a comment explaining the criterion. (ctf_equi_k_save_grid.R, line 488)
  → stage_grid = unique(round(stage_grid,2)) %>% filter(k0  >= hard_coded_lower_limits[1] &

[ADVISORY] `filter(` call not preceded by a comment within 2 lines — consider adding a comment explaining the criterion. (ctf_equi_k_save_grid.R, line 563)
  → mid_point <- mid_point %>% filter(pi_oo>0) %>%

[ADVISORY] `filter(` call not preceded by a comment within 2 lines — consider adding a comment explaining the criterion. (ctf_equi_k_save_grid.R, line 567)
  → rank = pmin(rank1, rank2, rank3)) %>% filter(rank < 4)

[ADVISORY] `filter(` call not preceded by a comment within 2 lines — consider adding a comment explaining the criterion. (ctf_equi_k_save_grid.R, line 589)
  → stage_grid = unique(round(stage_grid,2)) %>% filter(k0  >= hard_coded_lower_limits[1] &

[ADVISORY] `filter(` call not preceded by a comment within 2 lines — consider adding a comment explaining the criterion. (ctf_equi_k_save_grid.R, line 818)
  → out_pis_full <- filter(out_pis_full, !is.na(k4))

[ADVISORY] `filter(` call not preceded by a comment within 2 lines — consider adding a comment explaining the criterion. (ctf_equi_k_save_grid.R, line 1069)
  → mid_point <- mid_point %>% filter(pi_oo>0) %>%

[ADVISORY] `filter(` call not preceded by a comment within 2 lines — consider adding a comment explaining the criterion. (ctf_equi_k_save_grid.R, line 1073)
  → rank = pmin(rank1, rank2, rank3)) %>% filter(rank < 3)

[ADVISORY] `filter(` call not preceded by a comment within 2 lines — consider adding a comment explaining the criterion. (ctf_equi_k_save_grid.R, line 1097)
  → stage_grid = unique(round(stage_grid,2)) %>% filter(k0  >= hard_coded_lower_limits[1] &

[ADVISORY] `filter(` call not preceded by a comment within 2 lines — consider adding a comment explaining the criterion. (clean_choice_panel.R, line 14)
  → filter(!premature_dropout_ind) %>%

[ADVISORY] `filter(` call not preceded by a comment within 2 lines — consider adding a comment explaining the criterion. (clean_choice_panel.R, line 47)
  → choice_panel <- choice_panel %>% filter(!dup)

[ADVISORY] `filter(` call not preceded by a comment within 2 lines — consider adding a comment explaining the criterion. (get_next_rrev.R, line 28)
  → { sum_rrev_dates_st <- sum_rrev_dates %>% filter(ST_CD == st)

[ADVISORY] `filter(` call not preceded by a comment within 2 lines — consider adding a comment explaining the criterion. (get_fit_cp.R, line 89)
  → ) %>% group_by(llambda_bin) %>% filter(n() > 10) %>% ungroup() %>%

[ADVISORY] `filter(` call not preceded by a comment within 2 lines — consider adding a comment explaining the criterion. (sim_generate_rf_data.R, line 394)
  → panel_il <- panel %>% filter(state_alpha == "IL")

[ADVISORY] `filter(` call not preceded by a comment within 2 lines — consider adding a comment explaining the criterion. (sim_generate_rf_data.R, line 716)
  → filter(POL_ID_CHAR %in% monitored_pols)

[ADVISORY] `merge()` called without explicit `all=`, `all.x=`, or `all.y=` argument — defaults to inner join, which may silently drop rows. (sim_generate_rf_data.R, line 733)
  → ubi_disc_match <- merge(

