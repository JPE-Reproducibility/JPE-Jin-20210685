#### 4. Calculate choice likelihood ####
X_choice = X[N_choice_to_N,]

if("theta_0_llambda_choice" %in% meta$stan_variables){
  llambda_choice = llambda[N_choice_to_N]
  llambda_choice = llambda_choice + theta_0_llambda_choice;
  if("theta_0_llambda_severe_choice" %in% meta$stan_variables){
    llambda_severe_choice = llambda_severe[N_choice_to_N] + theta_0_llambda_severe_choice;
  } else {
    # llambda_severe_choice = llambda_severe[N_choice_to_N] + theta_0_llambda_choice;
    llambda_severe_choice = llambda_choice
  }
} else {
  if("prob_distortion_coef" %in% meta$stan_variables){
    lambda_choice = prob_distortion_coef[1] + exp(theta_0_llambda + X_choice %*% theta_1_llambda + leps[N_choice_to_I]) * prob_distortion_coef[2];
    lambda_severe_choice = lambda_choice;
    llambda_choice = log(lambda_choice);
    llambda_severe_choice = log(lambda_severe_choice);
  } else {
    llambda_choice = llambda[N_choice_to_N]
    llambda_severe_choice = llambda_severe[N_choice_to_N]
  }
}
lambda_choice = exp(llambda_choice)
lambda_severe_choice = exp(llambda_severe_choice)

if("lambda_choice_composite_weight" %in% meta$stan_variables){
  lambda_choice = lambda_choice * lambda_choice_composite_weight + lambda_severe_choice * (1-lambda_choice_composite_weight)
  lambda_severe_choice = lambda_choice
  llambda_choice = log(lambda_choice)
  llambda_severe_choice = log(lambda_severe_choice)
}

if("sev_cnt_adjustment" %in% meta$stan_variables){
  llambda_severe_choice = llambda_severe_choice + sev_cnt_adjustment
}
leps_n_choice = leps_n[N_choice_to_N]

if(d_tm_mh_rational_ind > 0){ # model assumes that people uses average monitoring intensity to anticiptate risk reduction
  # llambda_mh_adj = as.numeric(X_mh_mean %*% theta_mh_llambda[3,])
  if(grepl("mhhet", model_config)){
    llambda_mh_adj = rep(0, I);
    for(k in 1:D_R_tm_scheme){
      n0=choice_to_R_tm_ranges_n0[k];
      n1=choice_to_R_tm_ranges_n1[k];
      r = choice_to_R_tm_ranges_R[k];
      llambda_mh_adj[n0:n1] = X_mh_mean %*% data_list$theta_mh_llambda[r,] + data_list$X_rc_clm[n0:n1] * data_list$theta_mh_llambda_rc[r];
    }
  } else {
    mh_adj = rep(0, D_R_tm_scheme);
    for(k in 1:D_R_tm_scheme){
      mh_adj[k] = sum(X_mh_mean %*% theta_mh_llambda[k,]);
    }
    llambda_mh_adj = out_model$functions$get_tm_vectors(I, mh_adj,
                                                        choice_to_R_tm_ranges_n0,
                                                        choice_to_R_tm_ranges_n1,
                                                        choice_to_R_tm_ranges_R);
    llambda_mh_adj[llambda_mh_adj == 0] = mh_adj[1] #these are pre-tm introduction observations
    # we code them as if they have had tm since llambda_mh is by definition counterfactual object in people's expectations
    if("theta_0_llambda_mh_choice" %in% meta$stan_variables){
      llambda_mh_adj = llambda_mh_adj + theta_0_llambda_mh_choice
    }
    if("mh_distortion_coef" %in% meta$stan_variables){
      llambda_mh_adj = llambda_mh_adj * mh_distortion_coef
    }
  }
} else {
  llambda_mh_adj = rep(0, I)
}
llambda_choice_tm = llambda_choice
llambda_choice_tm[1:I] = llambda_choice[1:I] + llambda_mh_adj
lambda_choice_tm = exp(llambda_choice_tm)

llambda_severe_choice_tm = llambda_severe_choice
llambda_severe_choice_tm[1:I] = llambda_severe_choice[1:I] + llambda_mh_adj
lambda_severe_choice_tm = exp(llambda_severe_choice_tm)

# these are calculated using actual monitoring intensity data
lambda_w_mh_choice = lambda_choice
lambda_w_mh_choice[1:I] = lambda_clm_w_mh[N_choice_to_N[1:I]]
lambda_severe_w_mh_choice = lambda_severe_choice
lambda_severe_w_mh_choice[1:I] = lambda_severe_clm_w_mh[N_choice_to_N[1:I]]

sev_minor_mean_choice = theta_0_sev_minor_mean + X_choice %*% theta_1_sev_minor_mean;

if("leps_ra" %in% meta$stan_variables){
  leps_log_risk_aversion = leps_ra[N_choice_to_I]
} else {
  if("leps_risk_aversion" %in% meta$stan_variables){
    leps_log_risk_aversion = out_model$functions$add_eps(I, N_choice_to_I_choice, n_choice_regime_cutoffs, rep(0, N_choice),
                                                         leps_risk_aversion, d_by_block) #d_subsample_size, d_subsample_block5, d_subsample_block6
  } else {
    leps_log_risk_aversion = rep(0, N_choice)
  }
}
log_risk_aversion_observed = theta_0_log_risk_aversion + X_choice %*% theta_1_log_risk_aversion
risk_aversion = exp(log_risk_aversion_observed + leps_log_risk_aversion) #* risk_aversion_ub #upper bound of 2
log_risk_aversion = log(risk_aversion)

if("eps_mm" %in% meta$stan_variables){
  eps_psi_mm_i = out_model$functions$add_eps_I(n_choice_regime_cutoffs, rep(0, I),
                                               eps_mm, d_by_block) #d_subsample_size, d_subsample_block5, d_subsample_block6)
} else {
  eps_psi_mm_i = rep(0, I)
}
if("theta_0_mm" %in% meta$stan_variables){
  psi_mm_i = theta_0_mm + X_choice[1:I,] %*% theta_1_mm + eps_psi_mm_i
} else {
  psi_mm_i = theta_0_inert + X_choice[1:I,] %*% theta_1_inert
}
eps_psi_mm_choice = c(eps_psi_mm_i, rep(0, (N_choice-I)))
psi_mm_choice = c(psi_mm_i, rep(0, (N_choice-I)))

if("leps_eta" %in% meta$stan_variables){
  eps_eta_choice = leps_eta[N_choice_to_I]
} else {
  if("eps_eta" %in% meta$stan_variables){
    eps_eta_choice = out_model$functions$add_eps(I, N_choice_to_I_choice, n_choice_regime_cutoffs, rep(0, N_choice),
                                                 eps_eta, d_by_block)
  } else {
    eps_eta_choice = rep(0, N_choice)
  }
}

if("theta_0_eta" %in% meta$stan_variables){
  firm_switch_cost = theta_0_eta + X_choice %*% theta_1_eta + eps_eta_choice
  if("theta_1_eta_renw" %in% meta$stan_variables){
    firm_switch_cost = firm_switch_cost + theta_1_eta_renw * renw_cnt_choice + theta_1_eta_renw59 * renw_cnt_choice59;
  }
  if("theta_1_renw_active_eta" %in% meta$stan_variables){
    firm_switch_cost = firm_switch_cost + X_renw_active %*% theta_1_renw_active_eta;
  }

  if(grepl("invlogit", model_config)){
    firm_switch_cost = inv.logit(firm_switch_cost)*2
  } else {
    firm_switch_cost = exp(firm_switch_cost)
  }
} else {
  firm_switch_cost = rep(0, N_choice)
}

if("theta_1_inert" %in% meta$stan_variables){
  inert_cost = theta_0_inert + X_choice %*% theta_1_inert
  if("theta_1_inert_renw" %in% meta$stan_variables){
    inert_cost = inert_cost + theta_1_inert_renw * renw_cnt_choice + theta_1_inert_renw59 * renw_cnt_choice59;
  }
  if(grepl("invlogit", model_config)){
    inert_cost = inv.logit(inert_cost)
  }
} else {
  inert_cost = rep(0, N_choice)
}

sigma_logit = rep(theta_0_sigma_logit, N_choice)
if("theta_1_sigma_logit" %in% meta$stan_variables){
  sigma_logit = sigma_logit + X_choice %*% theta_1_sigma_logit #llambda_choice_norm * theta_1_llambda_sigma_logit +
}
if("theta_1_sigma_logit_rc" %in% meta$stan_variables){
  sigma_logit = sigma_logit + theta_1_sigma_logit_rc * log_X_rc_choice
}
if("theta_1_sigma_logit_renw0" %in% meta$stan_variables){
  sigma_logit_nb = sigma_logit_lb + exp(theta_0_sigma_logit + theta_1_sigma_logit_renw0 * 1)
  sigma_logit = sigma_logit + theta_1_sigma_logit_renw0 * renw_cnt_choice0
} else {
  sigma_logit_nb = sigma_logit_lb + exp(theta_0_sigma_logit)
}
if("theta_1_sigma_logit_renw1" %in% meta$stan_variables){
  sigma_logit_renw1 = sigma_logit_lb + exp(theta_0_sigma_logit + theta_1_sigma_logit_renw1 * 1)
  sigma_logit = sigma_logit + theta_1_sigma_logit_renw1 * renw_cnt_choice1
} else {
  sigma_logit_renw1 = sigma_logit_lb + exp(theta_0_sigma_logit)
}
if("theta_1_sigma_logit_renw59" %in% meta$stan_variables){
  sigma_logit_renw59 = sigma_logit_lb + exp(theta_0_sigma_logit + theta_1_sigma_logit_renw59 * 1)
  sigma_logit = sigma_logit + theta_1_sigma_logit_renw59 * renw_cnt_choice59
} else {
  sigma_logit_renw59 =  sigma_logit_lb + exp(theta_0_sigma_logit)
}
sigma_logit_renw = sigma_logit_lb + exp(theta_0_sigma_logit)
sigma_logit = sigma_logit_lb + exp(sigma_logit)

if(!"sigma_logit_tm_factors" %in% meta$stan_variables){
  sigma_logit_tm_factors = rep(1,data_list$D_R_tm_scheme)
}

if("eps_xi" %in% meta$stan_variables){  ## if xi includes private eps
  # eps_xi_choice_i = out_model$functions$add_eps_I(n_choice_regime_cutoffs, rep(0, I),
  #                                                 eps_xi, d_by_block)
  eps_xi_choice_i = eps_xi[N_choice_to_I[1:I]];
} else {
  eps_xi_choice_i = rep(0, I);
}
if(length(theta_0_xi) > 1){ ## if xi function changes with xi rate revision
  xi_choice_i = out_model$functions$get_xi(I, theta_0_xi, X_choice, matrix_to_lists(theta_1_xi),
                                           rep(0,d_by_block[2] + d_by_block[4]),
                                           d_by_block[2], d_by_block[4]);
} else {
  xi_choice_i = theta_0_xi + X_choice[1:I,] %*% theta_1_xi
}

xi_choice_no_eps_i = xi_choice_i
xi_choice_i = xi_choice_i + eps_xi_choice_i
eps_xi_choice = c(xi_choice_i - xi_choice_no_eps_i, rep(0, (N_choice-I)))
xi_choice = c(xi_choice_i, rep(0, (N_choice-I)))

R_choice = rbind(
  out_model$functions$get_R_choice(
    log_X_rc_choice[1:I],
    theta_0_R_nb_mean,
    theta_1_R_nb_mean,
    theta_0_R_nb_sd,
    data_list$choice_to_R_nb_ranges_n0,
    data_list$choice_to_R_nb_ranges_n1,
    data_list$choice_to_R_nb_ranges_R
  ),
  out_model$functions$get_R_choice(
    log_X_rc_choice[(I+1):N_choice],
    theta_0_R_renw_mean,
    theta_1_R_renw_mean,
    theta_0_R_renw_sd,
    data_list$choice_to_R_renw_ranges_n0 - I,
    data_list$choice_to_R_renw_ranges_n1 - I,
    data_list$choice_to_R_renw_ranges_R
  )
)

R_choice_baseline_tm = out_model$functions$choice_vectors_to_tm_vector(
  I_tm, R_choice[,1],
  choice_to_R_tm_ranges_n0,
  choice_to_R_tm_ranges_n1,
  choice_to_R_tm_ranges_R
);
R_choice_nb = R_choice
for(n in (I+1):N_choice){
  R_choice_nb[n] = R_choice[N_choice_to_I_choice[n]];
}

if(is.matrix(theta_1_log_score_mean)){
  theta_1_log_score_mean_tmp = matrix_to_lists(theta_1_log_score_mean)
  X_R_choice = cbind(leps_n_choice, llambda_choice - leps_n_choice)
} else {
  theta_1_log_score_mean_tmp = theta_1_log_score_mean
  X_R_choice = llambda_choice
}

R_tm_choice = out_model$functions$get_R_tm(
  I_tm,
  R_choice_baseline_tm,
  X_R_choice,
  log_X_rc_choice,
  theta_0_R_tm_mean,
  matrix_to_lists(theta_1_R_tm_mean),
  theta_0_R_tm_sd,
  theta_0_log_score_mean,
  theta_1_log_score_mean_tmp,
  theta_0_log_score_sd,
  data_list$choice_to_R_tm_ranges_n0,
  data_list$choice_to_R_tm_ranges_n1,
  data_list$choice_to_R_tm_ranges_R
);


