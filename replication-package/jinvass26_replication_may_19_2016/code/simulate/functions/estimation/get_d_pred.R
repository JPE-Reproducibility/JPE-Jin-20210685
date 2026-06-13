clm_count_choice = clm_count[N_choice_to_N]
clm_count_severe_choice = clm_count_severe[N_choice_to_N]

## Get individual-level aggregated/averaged risk measures
num_renw_i_choice = integer(I); lambda_i_choice = integer(I);clm_cnt_i_choice <- integer(I);
# num_renw_i_choice <- num_renw_i_choice + tapply(N_choice_to_I_choice, N_choice_to_I_choice, length)
# lambda_i_choice[N_choice_to_I_choice] <- lambda_i_choice[N_choice_to_I_choice] + lambda_choice[N_choice_to_I_choice]
# clm_cnt_i_choice[N_choice_to_I_choice] <- clm_cnt_i_choice[N_choice_to_I_choice] + clm_count_choice[N_choice_to_I_choice]
for(n in 1:N_choice){
  num_renw_i_choice[N_choice_to_I_choice[n]] = num_renw_i_choice[N_choice_to_I_choice[n]] + 1
  lambda_i_choice[N_choice_to_I_choice[n]] = lambda_i_choice[N_choice_to_I_choice[n]] + lambda_choice[n]
  clm_cnt_i_choice[N_choice_to_I_choice[n]] = clm_cnt_i_choice[N_choice_to_I_choice[n]] + clm_count_choice[n]
}
lambda_i_choice <- lambda_i_choice / num_renw_i_choice
clm_cnt_i_choice <- clm_cnt_i_choice / num_renw_i_choice
lambda_i_choice_n <- lambda_i_choice[N_choice_to_I_choice]
clm_cnt_i_choice_n <- clm_cnt_i_choice[N_choice_to_I_choice]

#### CALCULATE CHOICE LIKELIHOODS ####

fit_sample_size_by_block = d_by_block #d_by_block is the max of subsample size and block size...
fit_sample_size_by_block[3] = 0 #...except for the third block, which was only ~ 1000 obs so did not use
cv_sample_size_by_block = N_by_choice_regime - fit_sample_size_by_block ### CV comes from three sources: block 3, block 4 and 6 obs un-used in fitting

limits = list(limits_1, limits_1,
              limits_2, limits_2,
              limits_1, limits_2)
limits_oo = list(c(limits_1, limits_1), c(limits_1, limits_1),
                 c(limits_2, limits_2), c(limits_2, limits_2),
                 c(limits_1, limits_1), c(limits_2, limits_2))
prices = list(
  prices_nb_1, prices_nb_tm_1, prices_nb_2, prices_nb_tm_2, prices_renw_1, prices_renw_2
)
prices_oo = list(
  # prices_oo_nb_1, prices_oo_nb_tm_1, prices_oo_nb_2, prices_oo_nb_tm_2, prices_renw_oo_1, prices_renw_oo_2
  cbind(data_list$prices_kc_nb_1, data_list$prices_oo_nb_1),
  cbind(data_list$prices_kc_nb_tm_1, data_list$prices_oo_nb_tm_1),
  cbind(data_list$prices_kc_nb_2, data_list$prices_oo_nb_2),
  cbind(data_list$prices_kc_nb_tm_2, data_list$prices_oo_nb_tm_2),
  cbind(data_list$prices_kc_renw_1, data_list$prices_renw_oo_1),
  cbind(data_list$prices_kc_renw_2, data_list$prices_renw_oo_2)
)
tm_discount_ftr = list(NA, rep(1,N_by_choice_regime[2]), NA, tm_optin_disc_ftr_2, NA, NA) #no discount should be coded as 1

firm_discount_factor = 1

utils = list()
log_choice_probs = list()
log_choice_probs_fit = list()
log_choice_probs_cv = list()

log_likelihoods_clm_count_fit = rep(0, 6)
log_likelihoods_clm_count_severe_fit = rep(0, 6)
log_likelihoods_choice_fit = rep(0, 6)
log_likelihoods_clm_count_cv = rep(0, 6)
log_likelihoods_clm_count_severe_cv = rep(0, 6)
log_likelihoods_choice_cv = rep(0, 6)

for(k in c(1,3)){
  n0 = n_choice_regime_cutoffs[k]; n1 = (n_choice_regime_cutoffs[k+1]-1); N_part = (n1-n0+1); # n1 = n0-1+fit_sample_size[k]; N_part = fit_sample_size[k];
  adj = matrix(0, N_part, Js[k]);
  adj[,1] = psi_mm_choice[n0:n1]

  invisible(gc())  # pre-clean heap before Stan exposed function call
  utils[[k]] = out_model$functions$get_util_nb_w_joo(
    prices[[k]][1:N_part,],
    prices_oo[[k]][1:N_part,],
    clm_surcharge_ftr[n0:n1],
    limits[[k]],
    limits_oo[[k]],
    adj,
    lambda_choice[n0:n1],
    lambda_severe_choice[n0:n1],
    pareto_alpha_choice_base[n0:n1],
    R_choice[n0:n1,1],
    R_choice[n0:n1,2],
    R_choice[n0:n1,1],
    R_choice[n0:n1,2],
    zip_inc_norm[n0:n1],
    risk_aversion[n0:n1],
    firm_switch_cost[n0:n1],
    inert_cost[n0:n1],
    discount_factor,
    sigma_logit_renw1
  )

  invisible(gc())  # pre-clean heap before Stan exposed function call
  log_choice_probs[[k]] = ctf_model$functions$get_loglikelihood(
    utils[[k]], rep(sigma_logit_nb, N_part), 1
  )

  n0_fit = n0; n1_fit = n0_fit-1+fit_sample_size_by_block[k]
  n0_cv = n1_fit+1; n1_cv = n0_cv-1+cv_sample_size_by_block[k]; if(n1_cv>n1){print("error")}

  if(fit_sample_size_by_block[k]>0){
    log_choice_probs_fit[[k]] = log_choice_probs[[k]][1:fit_sample_size_by_block[k],]
    for(n in 1:fit_sample_size_by_block[k]){
      log_likelihoods_choice_fit[k] = log_likelihoods_choice_fit[k] + log_choice_probs_fit[[k]][n, d_t_new[n0_fit-1+n]]
    }
    log_likelihoods_clm_count_fit[k] = sum(log(dpois(clm_count_choice[n0_fit:n1_fit], lambda_choice[n0_fit:n1_fit])))
    log_likelihoods_clm_count_severe_fit[k] = sum(log(dpois(clm_count_severe_choice[n0_fit:n1_fit], lambda_severe_choice[n0_fit:n1_fit])))
  }
  if(cv_sample_size_by_block[k]>0){
    log_choice_probs_cv[[k]] = log_choice_probs[[k]][(fit_sample_size_by_block[k]+1):(fit_sample_size_by_block[k]+cv_sample_size_by_block[k]),]
    for(n in 1:cv_sample_size_by_block[k]){
      log_likelihoods_choice_cv[k] = log_likelihoods_choice_cv[k] + log_choice_probs_cv[[k]][n, d_t_new[n0_cv-1+n]]
    }
    log_likelihoods_clm_count_cv[k] = sum(log(dpois(clm_count_choice[n0_cv:n1_cv], lambda_choice[n0_cv:n1_cv])))
    log_likelihoods_clm_count_severe_cv[k] = sum(log(dpois(clm_count_severe_choice[n0_cv:n1_cv], lambda_severe_choice[n0_cv:n1_cv])))
  }
}

for(k in c(2,4)){
  # n0 = n_choice_regime_cutoffs[k]; n1 = n0-1+fit_sample_size[k]; N_part = fit_sample_size[k]; #
  n0 = n_choice_regime_cutoffs[k]; n1 = (n_choice_regime_cutoffs[k+1]-1); N_part = (n1-n0+1);
  k_tm = k/2; n0_tm = N_by_choice_regime[2]*(k_tm-1)+1
  adj = matrix(0, N_part, Js[k]);
  adj[,1] = psi_mm_choice[n0:n1]

  invisible(gc())  # pre-clean heap before Stan exposed function calls
  utils_k_notm = out_model$functions$get_util_nb_w_joo(
      prices[[k]][1:N_part,],
      prices_oo[[k]][1:N_part,],
      clm_surcharge_ftr[n0:n1],
      limits[[k]],
      limits_oo[[k]],
      adj, #matrix(0, N_part, Js[k]),
      lambda_choice[n0:n1],
      lambda_severe_choice[n0:n1],
      pareto_alpha_choice_base[n0:n1],
      R_choice[n0:n1,1],
      R_choice[n0:n1,2],
      R_choice[n0:n1,1],
      R_choice[n0:n1,2],
      zip_inc_norm[n0:n1],
      risk_aversion[n0:n1],
      firm_switch_cost[n0:n1],
      inert_cost[n0:n1],
      discount_factor,
      sigma_logit_renw1
    )
  invisible(gc())  # pre-clean heap before second Stan call
  utils_k_tm = out_model$functions$get_util_nb_w_joo(
      prices[[k]][1:N_part,],
      prices_oo[[k]][1:N_part,],
      clm_surcharge_ftr[n0:n1],
      limits[[k]],
      limits_oo[[k]],
      adj + prices[[k]][1:N_part,]*(1-replicate(Js[k], tm_discount_ftr[[k]])) - replicate(Js[k],xi_choice[n0:n1]), #adj
      lambda_choice_tm[n0:n1],
      lambda_severe_choice_tm[n0:n1],
      pareto_alpha_choice_base[n0:n1],
      R_tm_choice[(n0_tm):(n0_tm-1+N_part),1],
      R_tm_choice[(n0_tm):(n0_tm-1+N_part),2],
      R_choice[n0:n1,1],
      R_choice[n0:n1,2],
      zip_inc_norm[n0:n1],
      risk_aversion[n0:n1],
      firm_switch_cost[n0:n1],
      inert_cost[n0:n1],
      discount_factor,
      sigma_logit_renw1
    )
  utils[[k]] = cbind(utils_k_notm, utils_k_tm)
  rm(utils_k_notm, utils_k_tm)

  invisible(gc())  # pre-clean heap before Stan exposed function call
  if(grepl("nest", model_config)){
    log_choice_probs[[k]] = ctf_model$functions$get_nested_loglikelihood(
      utils[[k]], rep(sigma_logit_nb, N_part), 1, tm_nesting_param, Js[[k]]
    )
  } else {
    log_choice_probs[[k]] = ctf_model$functions$get_loglikelihood(
      utils[[k]], rep(sigma_logit_nb, N_part), 1
    )
  }

  n0_fit = n0; n1_fit = n0_fit-1+fit_sample_size_by_block[k]
  n0_cv = n1_fit+1; n1_cv = n0_cv-1+cv_sample_size_by_block[k]; if(n1_cv>n1){print("error")}
  if(fit_sample_size_by_block[k]>0){
    log_choice_probs_fit[[k]] = log_choice_probs[[k]][1:fit_sample_size_by_block[k],]
    for(n in 1:fit_sample_size_by_block[k]){
      log_likelihoods_choice_fit[k] = log_likelihoods_choice_fit[k] + log_choice_probs_fit[[k]][n, d_t_new[n0_fit-1+n]]
    }
    tmp = lambda_choice[n0_fit:n1_fit] * (d_t_new[n0_fit:n1_fit] < (Js[k]+1)) + lambda_w_mh_choice[n0_fit:n1_fit] * (d_t_new[n0_fit:n1_fit] > Js[k])
    log_likelihoods_clm_count_fit[k] = sum(log(dpois(clm_count_choice[n0_fit:n1_fit], tmp)))
    tmp = lambda_severe_choice[n0_fit:n1_fit] * (d_t_new[n0_fit:n1_fit] < (Js[k]+1)) + lambda_severe_w_mh_choice[n0_fit:n1_fit] * (d_t_new[n0_fit:n1_fit] > Js[k])
    log_likelihoods_clm_count_severe_fit[k] = sum(log(dpois(clm_count_severe_choice[n0_fit:n1_fit], tmp)))
  }
  if(cv_sample_size_by_block[k]>0){
    log_choice_probs_cv[[k]] = log_choice_probs[[k]][(fit_sample_size_by_block[k]+1):(fit_sample_size_by_block[k]+cv_sample_size_by_block[k]),]
    for(n in 1:cv_sample_size_by_block[k]){
      log_likelihoods_choice_cv[k] = log_likelihoods_choice_cv[k] + log_choice_probs_cv[[k]][n, d_t_new[n0_cv-1+n]]
    }
    tmp = lambda_choice[n0_cv:n1_cv] * (d_t_new[n0_cv:n1_cv] < (Js[k]+1)) + lambda_w_mh_choice[n0_cv:n1_cv] * (d_t_new[n0_cv:n1_cv] > Js[k])
    log_likelihoods_clm_count_cv[k] = sum(log(dpois(clm_count_choice[n0_cv:n1_cv], tmp)))
    tmp = lambda_severe_choice[n0_cv:n1_cv] * (d_t_new[n0_cv:n1_cv] < (Js[k]+1)) + lambda_severe_w_mh_choice[n0_cv:n1_cv] * (d_t_new[n0_cv:n1_cv] > Js[k])
    log_likelihoods_clm_count_severe_cv[k] = sum(log(dpois(clm_count_severe_choice[n0_cv:n1_cv], tmp)))
  }

}

for(k in c(5,6)){
  n0 = n_choice_regime_cutoffs[k]; n1 = (n_choice_regime_cutoffs[k+1]-1); N_part = (n1-n0+1);

  tmp = replicate(Js[k], -inert_cost[n0:n1])
  for(n in 1:N_part){
    tmp[n, d_t1_new[n0-I+n-1]] = 0;
  }

  tmp_sigma = sigma_logit[n0:n1]

  adj_oo = replicate(length(limits_oo[[k]]),-firm_switch_cost[n0:n1]) #adj
  if("psi_oo"  %in% meta$stan_variables){
    adj_oo = adj_oo + psi_oo
  }

  invisible(gc())  # pre-clean heap before Stan exposed function call
  utils_k_own = out_model$functions$get_util_renw_w_joo(
      prices[[k]][1:N_part,],
      prices_oo[[k]][1:N_part,],
      clm_surcharge_ftr[n0:n1],
      limits[[k]],
      tmp, #adj
      lambda_choice[n0:n1],
      lambda_severe_choice[n0:n1],
      pareto_alpha_choice_base[n0:n1],
      R_choice[n0:n1,1],
      R_choice[n0:n1,2],
      R_choice_nb[n0:n1,1],
      R_choice_nb[n0:n1,2],
      zip_inc_norm[n0:n1],
      risk_aversion[n0:n1],
      firm_switch_cost[n0:n1],
      inert_cost[n0:n1],
      discount_factor,
      sigma_logit_renw1,
      sigma_logit_renw
    )
  invisible(gc())  # pre-clean heap before Stan exposed function call
  utils_k_oo_raw = out_model$functions$get_util_renw_w_joo(
        prices_oo[[k]][1:N_part,],
        prices[[k]][1:N_part,],
        clm_surcharge_ftr[n0:n1],
        limits_oo[[k]],
        adj_oo,
        lambda_choice[n0:n1],
        lambda_severe_choice[n0:n1],
        pareto_alpha_choice_base[n0:n1],
        R_choice_nb[n0:n1,1],
        R_choice_nb[n0:n1,2],
        R_choice[n0:n1,1],
        R_choice[n0:n1,2],
        zip_inc_norm[n0:n1],
        risk_aversion[n0:n1],
        firm_switch_cost[n0:n1] * 0 + 1000,
        inert_cost[n0:n1],
        discount_factor,
        sigma_logit_renw1,
        sigma_logit_renw
      )
  invisible(gc())  # pre-clean heap before Stan exposed function call
  utils_k_oo = out_model$functions$log_sum_exp_v(
      utils_k_oo_raw / replicate(length(limits_oo[[k]]), tmp_sigma)
    ) * tmp_sigma
  rm(utils_k_oo_raw)
  utils[[k]] = cbind(utils_k_own, utils_k_oo)
  rm(utils_k_own, utils_k_oo)

  invisible(gc())  # pre-clean heap before Stan exposed function call
  log_choice_probs[[k]] = ctf_model$functions$get_loglikelihood(
    utils[[k]], tmp_sigma, 1
  )

  n0_fit = n0; n1_fit = n0_fit-1+fit_sample_size_by_block[k]
  n0_cv = n1_fit+1; n1_cv = n0_cv-1+cv_sample_size_by_block[k]; if(n1_cv>n1){print("error")}
  if(fit_sample_size_by_block[k]>0){
    log_choice_probs_fit[[k]] = log_choice_probs[[k]][1:fit_sample_size_by_block[k],]
    for(n in 1:fit_sample_size_by_block[k]){
      tmp = d_t_new[n0_fit-1+n]
      if(tmp < 1){
        tmp = Js[k] + 1
      }
      log_likelihoods_choice_fit[k] = log_likelihoods_choice_fit[k] + log_choice_probs_fit[[k]][n, tmp]
    }
    log_likelihoods_clm_count_fit[k] = sum(log(dpois(clm_count_choice[n0_fit:n1_fit], lambda_choice[n0_fit:n1_fit])))
    log_likelihoods_clm_count_severe_fit[k] = sum(log(dpois(clm_count_severe_choice[n0_fit:n1_fit], lambda_severe_choice[n0_fit:n1_fit])))
  }
  if(cv_sample_size_by_block[k]>0){
    log_choice_probs_cv[[k]] = log_choice_probs[[k]][(fit_sample_size_by_block[k]+1):(fit_sample_size_by_block[k]+cv_sample_size_by_block[k]),]
    for(n in 1:cv_sample_size_by_block[k]){
      tmp = d_t_new[n0_fit-1+n]
      if(tmp < 1){
        tmp = Js[k] + 1
      }
      log_likelihoods_choice_cv[k] = log_likelihoods_choice_cv[k] + log_choice_probs_cv[[k]][n, tmp]
    }
    log_likelihoods_clm_count_severe_cv[k] = sum(log(dpois(clm_count_severe_choice[n0_cv:n1_cv], lambda_severe_choice[n0_cv:n1_cv])))
    log_likelihoods_clm_count_cv[k] = sum(log(dpois(clm_count_choice[n0_cv:n1_cv], lambda_choice[n0_cv:n1_cv])))
  }
}

perobs_fit_choice = sum(log_likelihoods_choice_fit)/sum(fit_sample_size_by_block)
perobs_cv_choice = sum(log_likelihoods_choice_cv)/sum(cv_sample_size_by_block)
perobs_fit_clm = sum(log_likelihoods_clm_count_fit)/sum(fit_sample_size_by_block)
perobs_cv_clm = sum(log_likelihoods_clm_count_cv)/sum(cv_sample_size_by_block)
perobs_fit_clm_severe = sum(log_likelihoods_clm_count_severe_fit)/sum(fit_sample_size_by_block)
perobs_cv_clm_severe = sum(log_likelihoods_clm_count_severe_cv)/sum(cv_sample_size_by_block)

perobs_fit = perobs_fit_choice + perobs_fit_clm + perobs_fit_clm_severe
perobs_cv = perobs_cv_choice + perobs_cv_clm + perobs_cv_clm_severe
