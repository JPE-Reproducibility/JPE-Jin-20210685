data_obj_names = c(
  "N", "I", "J", "M", "D", "D_oo", "N_to_I", "I_to_renw0", "renw_cnt", "X", "X_rc",
  "D_R_nb_scheme", "D_R_renw_scheme", "D_R_tm_scheme",
  "I_tm", "I_tm_to_N", "I_tm_R", "I_tm_R_to_I",
  "demand_choice_index", "d_t1", "tm_ind", "tm_int", "tm_start_ind",
  "tm_not_na_ind", "cov_space_index", "cov_space_options", "cov_space_oo_index", "cov_space_oo_options",
  "mm_index", "limit_csl_ind", "limit_oo_csl_ind", "limits_raw",
  "prices_raw","prices_oo_raw", #"tm_optin_disc", #"clm_surcharge",
  "clm_count", "clm_count_minor", "clm_count_severe",
  "p_R_ftr_wo_clm", "p_R_ftr_wo_clm_w_tm", "p_R_ftr_tm_disc",
  "N_choice", "N_by_choice_regime", "N_choice_to_N", "N_choice_to_I", "N_to_N_choice", "N_choice_to_I_choice",
  "prices_nb_1", "prices_oo_nb_1",
  "prices_nb_2", "prices_oo_nb_2",
  "prices_nb_tm_1", "prices_oo_nb_tm_1",
  "prices_nb_tm_2", "prices_oo_nb_tm_2",
  "prices_renw_1", "prices_renw_oo_1",
  "prices_renw_2", "prices_renw_oo_2",
  "tm_optin_disc_ftr_2",
  "clm_surcharge_ftr",
  "n_choice_regime_cutoffs",
  "limits_1", "limits_2",
  "d_t_new", "d_t1_new",
  "X_rc_choice", "X_clm",
  "N_clm_to_I", "N_clm_to_N", "N_clm", "clm_count_N_clm", "clm_count_severe_N_clm", "N_clm_to_I",
  "N_R", "R_nb_scheme_cutoffs", "R_tm_scheme_cutoffs", "R_renw_scheme_cutoffs",
  "p_R_ftr_wo_clm_R_ordered", "p_R_ftr_wo_clm_w_tm_R_ordered", "log_tm_score_R_ordered",
  "N_R_ordered_to_N_clm", "I_tm_R_ordered_to_N_clm", "X_rc_R_ordered", "X_rc_R_tm_ordered",
  "clm_sev_raw",
  "Js",
  "X_mh_1", "X_mh_2", "X_mh_3",
  "I_with_renw",  "I_with_renw_to_I", "i_w_renw_choice_regime_cutoffs",
  # "d_subsample_size", "d_subsample_w_renw_1", "d_subsample_w_renw_2", "d_subsample_w_renw_4", "d_subsample_block5", "d_subsample_block6",
  "X_mh_mean",
  "zip_income", "renw_cnt_choice", "renw_cnt_choice59", "renw_cnt_choice0", "renw_cnt_choice1", "X_renw_active",
  "choice_to_R_tm_ranges_n0",
  "choice_to_R_tm_ranges_n1",
  "choice_to_R_tm_ranges_R",
  names(data_list)[grepl("oo", names(data_list)) & grepl("full", names(data_list))],
  "dollar_norm", "run_demand_blocks", "d_tm_mh_rational_ind",  "d_by_block", "d_by_block_w_renw"
)

if(!("theta_mh_llambda" %in% meta$stan_variables)){
  data_obj_names = c(data_obj_names, "theta_mh_llambda")
}

if(!("discount_factor" %in% meta$stan_variables)){
  data_obj_names = c(data_obj_names, "discount_factor")
}

if(!("theta_0_R_nb_mean" %in% meta$stan_variables)){
  data_obj_names = c(data_obj_names, "theta_0_R_nb_mean", "theta_1_R_nb_mean",   "theta_0_R_nb_sd",    "theta_0_R_renw_mean", "theta_1_R_renw_mean",
                     "theta_0_R_renw_sd",   "theta_0_R_tm_mean",   "theta_1_R_tm_mean",   "theta_0_R_tm_sd")
}
if(!("theta_0_log_score_sd" %in% meta$stan_variables)){
  data_obj_names = c(data_obj_names, "theta_0_log_score_sd")
}
if(!("theta_0_pareto_alpha_severe" %in% meta$stan_variables)){
  data_obj_names = c(data_obj_names, "theta_0_pareto_alpha_severe", "theta_1_pareto_alpha_severe",
                     "theta_0_sev_minor_mean", "theta_1_sev_minor_mean", "sev_minor_sd")
}

if(!("theta_0_log_score_mean" %in% meta$stan_variables)){
  data_obj_names = c(data_obj_names, "theta_0_log_score_mean", "theta_1_log_score_mean")
}

if(!exists("base_reg_factor_c")){
  data_obj_names = c(data_obj_names, "base_reg_factor_c")
}
if(!exists("base_reg_factor_s")){
  data_obj_names = c(data_obj_names, "base_reg_factor_c")
}
if(!exists("base_reg_factor_d")){
  data_obj_names = c(data_obj_names, "base_reg_factor_c")
}
if(!exists("eps_prior_sd")){
  data_obj_names = c(data_obj_names, "eps_prior_sd")
}
if(!exists("eps_ra_sd")){
  data_obj_names = c(data_obj_names, "eps_ra_sd")
}
# "eps_prior_sd",
# "eps_ra_sd",
# "base_reg_factor_c",
# "base_reg_factor_s",
# "base_reg_factor_d"

if(exists("data_list")){
  for(k in c(data_obj_names)){
    print(k)
    assign(k, data_list[[k]])
  }
} else {
  for(k in c(data_obj_names)){
    print(k)
    assign(k, data_estimation[[k]])
  }
}

##### ADDITIONAL PROCESSING ####

zip_inc_norm = zip_income/dollar_norm

if(min(X_rc) <= 0){
  log_X_rc = log((X_rc + 0.1) * 1/1.1)
} else {
  log_X_rc = log(X_rc)
}

if(min(X_rc_R_ordered) <= 0){
  log_X_rc_R_ordered = log((X_rc_R_ordered + 0.1) * 1/1.1)
} else {
  log_X_rc_R_ordered = log(X_rc_R_ordered)
}

if(min(X_rc_R_tm_ordered) <= 0){
  log_X_rc_R_tm_ordered = log((X_rc_R_tm_ordered + 0.1) * 1/1.1)
} else {
  log_X_rc_R_tm_ordered = log(X_rc_R_tm_ordered)
}

if(min(X_rc_choice) == 0){
  log_X_rc_choice = log(X_rc_choice + 0.1) * 1/1.1;
} else {
  log_X_rc_choice = log(X_rc_choice);
}

I_R = max(R_nb_scheme_cutoffs)
I_R_to_I = N_R_ordered_to_N_clm[1:I_R]
# I_tm_R = max(R_tm_scheme_cutoffs)
# I_tm_R_to_I = N_clm_to_I[I_tm_R_ordered_to_N_clm]

# N_to_N_choice = rep(NA, N)
# for(n in 1:N_choice){
#   N_to_N_choice[N_choice_to_N[n]] = n
# }
N_choice_to_I_tm = rep(NA, N_choice)
for(i in 1:I_tm){
  N_choice_to_I_tm[N_to_N_choice[I_tm_to_N[i]]] = i
}
N_choice_to_I_tm_R_ordered = rep(NA, N_choice)
for(i in 1:I_tm_R){
  N_choice_to_I_tm_R_ordered[N_to_N_choice[N_clm_to_N[I_tm_R_ordered_to_N_clm[i]]]] = i
}


if(is.null(d_by_block)){
  for(k in names(est_config_obj)){
    print(k)
    assign(k, est_config_obj[[k]])
  }
}

