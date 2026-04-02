num_renw_i_clm = integer(I); lambda_i = integer(I);
clm_cnt_i <- integer(I); clm_severe_cnt_i <- integer(I);
lambda_severe_i = integer(I); X_rc_i = integer(I)
for(n in 1:N_clm){
  num_renw_i_clm[N_clm_to_I[n]] = num_renw_i_clm[N_clm_to_I[n]] + 1
  lambda_i[N_clm_to_I[n]] = lambda_i[N_clm_to_I[n]] + lambda_clm[n]
  lambda_severe_i[N_clm_to_I[n]] = lambda_severe_i[N_clm_to_I[n]] + lambda_severe_clm[n]
  clm_cnt_i[N_clm_to_I[n]] = clm_cnt_i[N_clm_to_I[n]] + clm_count_N_clm[n]
  clm_severe_cnt_i[N_clm_to_I[n]] = clm_severe_cnt_i[N_clm_to_I[n]] + clm_count_severe_N_clm[n]
  X_rc_i[N_clm_to_I[n]] <- X_rc_i[N_clm_to_I[n]] + X_rc[N_clm_to_N[n]]
}
lambda_i <- lambda_i / num_renw_i_clm
clm_cnt_i <- clm_cnt_i / num_renw_i_clm
X_rc_i <- X_rc_i / num_renw_i_clm

#########
p_R_nb_est = c(); p_R_nb_sd = c(); rr_nb = c()
p_R_renw_est = c(); p_R_renw_sd = c(); rr_renw = c()
for(r in 1:(length(R_nb_scheme_cutoffs)-1)){
  # tmp = X_pricing[(R_nb_scheme_cutoffs[r]+1):R_nb_scheme_cutoffs[r+1],]
  # p_R_nb_est = c(p_R_nb_est, theta_0_R_nb_mean[r] + tmp %*% theta_1_R_nb_mean[r,]);
  tmp = log_X_rc_R_ordered[(R_nb_scheme_cutoffs[r]+1):R_nb_scheme_cutoffs[r+1]]
  p_R_nb_est = c(p_R_nb_est, (theta_0_R_nb_mean[r] + tmp * theta_1_R_nb_mean[r]));
  rr_nb = c(rr_nb, rep(r, R_nb_scheme_cutoffs[r+1] - R_nb_scheme_cutoffs[r]))
  p_R_nb_sd = c(p_R_nb_sd, theta_0_R_nb_sd[r])
}
# p_R_nb_est = exp(p_R_nb_est)

for(r in 1:(length(R_renw_scheme_cutoffs)-1)){
  # tmp = X_pricing[(R_renw_scheme_cutoffs[r]+1):R_renw_scheme_cutoffs[r+1],]
  # p_R_renw_est = c(p_R_renw_est, theta_0_R_renw_mean[r] + tmp %*% theta_1_R_renw_mean[r,]);
  tmp = log_X_rc_R_ordered[(R_renw_scheme_cutoffs[r]+1):R_renw_scheme_cutoffs[r+1]]
  p_R_renw_est = c(p_R_renw_est, (theta_0_R_renw_mean[r] + tmp * theta_1_R_renw_mean[r]));
  rr_renw = c(rr_renw, rep(r, (R_renw_scheme_cutoffs[r+1] - R_renw_scheme_cutoffs[r])))
  p_R_renw_sd = c(p_R_renw_sd, theta_0_R_renw_sd[r])
}
# p_R_renw_est = exp(p_R_renw_est)

p_R_tm_est = c(); p_R_tm_sd = c(); rr_tm = c()
log_score_est = c(); log_score_sd = c();
X_pricing_tm = cbind(log_tm_score_R_ordered, log_X_rc_R_tm_ordered)
# tmp = cbind(leps_n_clm, llambda_clm - leps_n_clm)

for(r in 1:(length(R_tm_scheme_cutoffs)-1)){
  tmp = X_pricing_tm[(R_tm_scheme_cutoffs[r]+1):R_tm_scheme_cutoffs[r+1],]
  p_R_tm_est = c(p_R_tm_est, (theta_0_R_tm_mean[r] + tmp %*% theta_1_R_tm_mean[r,]));
  p_R_tm_sd = c(p_R_tm_sd, theta_0_R_tm_sd[r]); # + tmp %*% theta_1_R_tm_sd[r,]
  rr_tm = c(rr_tm, rep(r, nrow(tmp)))
  log_score_sd = c(log_score_sd, theta_0_log_score_sd[r])
}

if(is.matrix(theta_1_log_score_mean)){
  X_R_tm = cbind(leps_n_clm[I_tm_R_ordered_to_N_clm], llambda_clm[I_tm_R_ordered_to_N_clm]) #log_X_rc_R_tm_ordered
  for(r in 1:(length(R_tm_scheme_cutoffs)-1)){
    tmp = X_R_tm[(R_tm_scheme_cutoffs[r]+1):R_tm_scheme_cutoffs[r+1],]
    pred  = theta_0_log_score_mean[r] + tmp %*% theta_1_log_score_mean[r,]
    log_score_est = c(log_score_est, (pred))
  }
} else {
  X_R_tm = llambda_clm[I_tm_R_ordered_to_N_clm]
  for(r in 1:(length(R_tm_scheme_cutoffs)-1)){
    tmp = X_R_tm[(R_tm_scheme_cutoffs[r]+1):R_tm_scheme_cutoffs[r+1]]
    pred = theta_0_log_score_mean[r] + tmp * theta_1_log_score_mean[r]
    log_score_est = c(log_score_est, (pred))
  }
}
