if (use_mle > 0) {
  outdir = mle_out_dir
  if(!exists("results") ||
     !"theta_0_llambda" %in% colnames(df)){
    results = model_fit_opt
    meta = results$metadata()
    df = cbind.data.frame(results$lp(), t(results$mle()))
  }
} else {
  outdir = output_dir
  meta = model_fit_samp$metadata()
  samples = as_tibble(model_fit_samp$draws())
  colnames(samples) = sub('.*\\.', '', colnames(samples))
  # head(samples)
  df = colMeans(samples)
  df_95 = stack(lapply(samples, quantile, prob = 0.95, names = FALSE)) %>% rename(pct95 = values)
  df_05 = stack(lapply(samples, quantile, prob = 0.05, names = FALSE)) %>% rename(pct05 = values)
  df_ci = df_95 %>% left_join(df_05) %>% select(c("ind", "pct05", "pct95")) %>% rename(est = ind)
}

meta$stan_variables <- meta$stan_variables[!grepl("gq",meta$stan_variables)]
for(i in meta$stan_variables){
  if(i != "lp__"){
    if(length(meta$stan_variable_sizes[[i]]) > 1){
      D = meta$stan_variable_sizes[[i]][1]
      M = meta$stan_variable_sizes[[i]][2]
      tmp = matrix(rep(0,D*M),D,M)
      for(d in 1:D){
        col_names = paste0(i,paste0("[",d,",",paste0((1:M), "]")))
        tmp[d,] = as.numeric(df[col_names])
      }
      assign(i, tmp)
    } else {
      if(meta$stan_variable_sizes[[i]] > 1){
        N = meta$stan_variable_sizes[[i]]
        col_names = paste0(i,paste0("[", paste0((1:N), "]")))
        assign(i, as.numeric(df[col_names]))
      } else {
        assign(i, as.numeric(df[i]))
      }
    }
    # print(i)
    # print(get(i))
  }
}

if(!("leps" %in% meta$stan_variables)){
  leps = nu * eps_sd
}

source('code/simulate/functions/estimation/load_model_data.R')

leps_n = leps[N_to_I];
llambda = theta_0_llambda + X %*% theta_1_llambda + leps_n;
if("theta_1_llambda_severe" %in% meta$stan_variables){
    llambda_severe = llambda * theta_1_llambda_severe + theta_0_llambda_severe;
  } else {
    llambda_severe = llambda + theta_0_llambda_severe;
  }

lambda = exp(llambda); lambda_severe = exp(llambda_severe);
eps_n = lambda - exp(llambda - leps_n)

gg_llambda = ggplot(data=as.data.frame(llambda), aes(x=llambda)) + geom_histogram()
gg_lambda = ggplot(data=as.data.frame(lambda), aes(x=lambda)) + geom_histogram(binwidth = 0.005)
gg_llambda_severe = ggplot(data=as.data.frame(llambda_severe), aes(x=llambda_severe)) + geom_histogram()

leps_n_clm = leps[N_clm_to_I];

#from the cost model
theta_mh_llambda_pi <- MH_PARAMS
if(!"theta_mh_llambda" %in% meta$stan_variables){
  theta_mh_llambda <- theta_mh_llambda_pi
}

tmp_i = as.matrix(X_mh_1) %*% theta_mh_llambda[1,] + as.matrix(X_mh_2) %*% theta_mh_llambda[2,] + as.matrix(X_mh_3) %*% theta_mh_llambda[3,]
llambda_clm = llambda[N_clm_to_N]
llambda_severe_clm = llambda_severe[N_clm_to_N]
llambda_clm_w_mh = llambda_clm
llambda_clm_w_mh[1:I] = llambda_clm_w_mh[1:I] + tmp_i
if("theta_1_llambda_severe" %in% meta$stan_variables){
  llambda_severe_clm_w_mh = llambda_clm_w_mh * theta_1_llambda_severe + theta_0_llambda_severe
} else {
  llambda_severe_clm_w_mh = llambda_clm_w_mh + theta_0_llambda_severe
}
lambda_clm = exp(llambda_clm); lambda_severe_clm = exp(llambda_severe_clm);
lambda_clm_w_mh = exp(llambda_clm_w_mh); lambda_severe_clm_w_mh = exp(llambda_severe_clm_w_mh);

