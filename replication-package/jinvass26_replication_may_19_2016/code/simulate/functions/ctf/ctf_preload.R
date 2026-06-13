# CTF Preload: Sets up the counterfactual environment (choice probabilities, market structure, prices).
#
# HARDCODED ASSUMPTIONS (focal-state market):
#   Market shares: own=5.3%, firm_2=7.02%, firm_3=29.78%, firm_4=4.19%, firm_6=4.22%, firm_35=4.26%
#     Source: state Department of Insurance 2016 Top 25 report
#   Direct channel shares: own=60%, firm_2=40%, firm_3=20%, firm_4=100%, others=20%
#   firm_discount_factor default: 1 (no time discounting)
#   N_part: min(N_by_choice_regime[k_block_ctf], nrow(data)) for CTF sample size
#   k_block_ctf default: 4 (4th period block index)

#### Defaults for parameters that must be set before sourcing this file ####
if(!exists("ctf_oo_option")) stop("ctf_oo_option must be set before sourcing ctf_preload.R")
if(!exists("tm_resource_cost")) stop("tm_resource_cost must be set before sourcing ctf_preload.R")
if(!exists("firm_discount_factor")){assign("firm_discount_factor", FIRM_DISCOUNT_FACTOR); print("setting firm_discount_factor to FIRM_DISCOUNT_FACTOR by default")}
profit_horizon <- welfare_horizon  # Always equal; no separate horizon implemented
if(!exists("k_block_ctf")){assign("k_block_ctf", K_BLOCK_CTF); print("setting k_block_ctf to K_BLOCK_CTF by default")}

# Consumers are rational about monitoring's moral hazard effect on their expected risk
lambda_severe_choice_tm_tmp = lambda_severe_choice_tm
lambda_choice_tm_tmp = lambda_choice_tm

llambda_shifter_from_mh_effect = as.numeric(X_mh_mean %*% theta_mh_llambda_pi[3,]) # telematics regime #3
lambda_severe_choice_w_mh = as.vector(exp(llambda_severe_choice + llambda_shifter_from_mh_effect))
lambda_choice_w_mh = as.vector(exp(llambda_choice + llambda_shifter_from_mh_effect))

fit_sample_size = d_by_block

#### Calculate outside option utilities ####
n0 = n_choice_regime_cutoffs[k_block_ctf];
N_part = N_by_choice_regime[k_block_ctf]
# k_block_ctf indexes all 6 choice regimes (1=NB, 2=NB+TM, 3=Renw, 4=Renw+TM, 5=Attr, 6=Attr+TM).
# k_tm indexes the 3 TM-eligible blocks only: blocks 2,4,6 map to k_tm 1,2,3.
k_tm = k_block_ctf/2; n0_tm = N_by_choice_regime[2]*(k_tm-1)+1
n1 = n0 + N_part - 1;
J = Js[k_block_ctf]
tmp_sigma = sigma_logit_nb

sampling_weight = 1/data_list$sampling_enum_choice[n0:n1]
weights_combined = sampling_weight
if(k_tm == 2){
  price_base = prices_nb_tm_2[1:N_part,]
  limits_base = limits_2
  tm_optin_disc_dollar = price_base*(1 - replicate(J, tm_optin_disc_ftr_2[1:N_part]))
  prices_oo_temp = prices_oo_nb_tm_full_2
} else {
  price_base = prices_nb_tm_1[1:N_part,]
  limits_base = limits_1
  tm_optin_disc_dollar = price_base*0
  prices_oo_temp = prices_oo_nb_tm_full_1
}

full_oo_cov_name = colnames(prices_oo_full)
if(k_tm == 2){
  full_oo_cov_name = full_oo_cov_name[!grepl("_3",full_oo_cov_name)]
}
firm_indices = str_extract(full_oo_cov_name[1:5], "^[^_]+")
min_price_firm_index = which.min(colMeans(prices_oo_temp[,grepl("_4", full_oo_cov_name)]))
min_price_firm_cov_indices = grepl(paste0(firm_indices[min_price_firm_index],"_"),full_oo_cov_name)
max_price_firm_index = which.max(colMeans(prices_oo_temp[,grepl("_4", full_oo_cov_name)]))
max_price_firm_cov_indices = grepl(paste0(firm_indices[max_price_firm_index],"_"),full_oo_cov_name)

kc_firm_index = 3
kc_firm_cov_indices = grepl(paste0(firm_indices[kc_firm_index], "_"), full_oo_cov_name)
if(k_tm == 2){
  median_price_firm_index = which.median(colMeans(prices_oo_temp[,grepl("_4", full_oo_cov_name)]))
} else {
  median_price_firm_index = 1
}
median_price_firm_cov_indices = grepl(paste0(firm_indices[median_price_firm_index], "_"), full_oo_cov_name)

#### Market shares (focal state, 2016 — from config.R) ####
own_mkt_share = IL_OWN_MKT_SHARE
mkt_share_oo <- unname(IL_OO_MKT_SHARES)
firm_residual_share = 100 - sum(mkt_share_oo) - own_mkt_share

#### Direct distribution channel shares (from config.R) ####
if(grepl("direct", ctf_oo_option)){
  own_mkt_share <- own_mkt_share * IL_DIRECT_SHARES[["own"]]
  mkt_share_oo <- c(IL_OO_MKT_SHARES[["firm_2"]]  * IL_DIRECT_SHARES[["firm_2"]],
                    IL_OO_MKT_SHARES[["firm_3"]]  * IL_DIRECT_SHARES[["firm_3"]],
                    IL_OO_MKT_SHARES[["firm_4"]]  * IL_DIRECT_SHARES[["firm_4"]],
                    IL_OO_MKT_SHARES[["firm_6"]]  * IL_DIRECT_SHARES[["firm_6"]],
                    IL_OO_MKT_SHARES[["firm_35"]] * IL_DIRECT_SHARES[["firm_35"]],
                    firm_residual_share            * IL_DIRECT_SHARES[["residual"]])
}

if(grepl("min_and_median|all", ctf_oo_option)){
  price_oo_base = prices_oo_temp[1:N_part,(min_price_firm_cov_indices | median_price_firm_cov_indices)]
  if(grepl("min_flex", ctf_oo_option)){
    flexible_oo_index = min_price_firm_cov_indices[min_price_firm_cov_indices | median_price_firm_cov_indices]
  } else {
    if(grepl("median_flex", ctf_oo_option)){
      flexible_oo_index = median_price_firm_cov_indices[min_price_firm_cov_indices | median_price_firm_cov_indices]
    }
  }
}

J_oo = ncol(price_oo_base)
limits_oo_base = as.vector(t(do.call(cbind, replicate(J_oo/J, limits_base, simplify = FALSE))))

if(J_oo>J){
  if(grepl("min_and_median", ctf_oo_option)){
    tot_tmp = sum(own_mkt_share,mkt_share_oo[c(min_price_firm_index, median_price_firm_index)])
  } else {
    if(grepl("kc_and_median", ctf_oo_option)){
      tot_tmp = sum(own_mkt_share,mkt_share_oo[c(kc_firm_index, median_price_firm_index)])
    } else {
      if(grepl("all", ctf_oo_option)){
        tot_tmp = sum(own_mkt_share,mkt_share_oo)
      }
    }
  }

  if(grepl("min_flex", ctf_oo_option)){
    target_market_share = c(own_mkt_share / tot_tmp, mkt_share_oo[min_price_firm_index] / tot_tmp)
  } else {
    if(grepl("median_flex", ctf_oo_option)){
      target_market_share = c(own_mkt_share / tot_tmp, 1 - (own_mkt_share + mkt_share_oo[min_price_firm_index]) / tot_tmp)
    }
  }
}
