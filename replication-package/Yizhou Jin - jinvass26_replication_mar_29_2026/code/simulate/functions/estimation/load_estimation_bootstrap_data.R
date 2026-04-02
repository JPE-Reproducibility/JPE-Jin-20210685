
# Load data_list: cache -> canonical -> JSON (simulated).
if (USE_SIMULATED_DATA) {
  dl_rds  <- file.path(SIM_CACHE_DIR, "data_list_IL.rds")
  dl_json <- file.path(SIM_CACHE_DIR, "data_list_IL.json")
  if (file.exists(dl_rds)) {
    data_list <- readRDS(dl_rds)
  } else if (file.exists(dl_json)) {
    data_list <- read_data_list(dl_json)
  } else {
    stop("Simulated data_list not found.\n",
         "  Run the simulation pipeline first:\n",
         "  Rscript code/simulate/sim_c6_estimate.R")
  }
} else {
  # Real data mode: look in data/estimates or data/ for data_list_IL.rds
  dl_rds <- file.path("data", "data_list_IL.rds")
  if (!file.exists(dl_rds)) dl_rds <- file.path("data", "estimates", "data_list_IL.rds")
  if (!file.exists(dl_rds)) stop("data_list_IL.rds not found")
  data_list <- readRDS(dl_rds)
}

data_list$X_rc_clm = data_list$X_rc[data_list$N_clm_to_N]

data_list$limits_1 <- COVERAGE_LIMITS_1
data_list$limits_2 <- COVERAGE_LIMITS_2

temp <- cbind.data.frame(data_list$zip_income, data_list$d_t_new)
# temp <- temp[1:I,]
seq12 <- 1:(data_list$n_choice_regime_cutoffs[3]-1)
seq34 <- data_list$n_choice_regime_cutoffs[4]:(data_list$n_choice_regime_cutoffs[5]-1)
seq5 <- data_list$n_choice_regime_cutoffs[5]:(data_list$n_choice_regime_cutoffs[6]-1)
seq6 <- data_list$n_choice_regime_cutoffs[6]:(data_list$n_choice_regime_cutoffs[7]-1)
temp[seq12,2][temp[seq12,2] > 5] <- temp[seq12,2][temp[seq12,2] > 5] - 5
temp[seq34,2][temp[seq34,2] > 4] <- temp[seq34,2][temp[seq34,2] > 4] - 4
temp[seq34,2] <- temp[seq34,2] + 1
temp[seq6,2] <- temp[seq6,2] + 1
colnames(temp) <- c("income", "index")
temp$index[temp$index==0] <- temp$index[data_list$N_to_I[temp$index==0]]

dimtable <- cbind.data.frame(c(1,2,3,4,5), data_list$limits_1)
colnames(dimtable) <- c("index", "cov")
temp <- temp %>% left_join(dimtable)

if (!exists("dollar_norm")) dollar_norm <- data_list$dollar_norm
data_list$income_choice <- temp$income/dollar_norm

data_list$I_tm_R_order_to_N_R <- data_list$N_clm_to_N_R[data_list$I_tm_R_ordered_to_N_clm]
data_list$p_R_ftr_wo_clm_baseline_tm_R_ordered <- data_list$p_R_ftr_wo_clm_R[data_list$I_tm_R_order_to_N_R]
data_list$tm_discount_at_lb_tm_R_ordered <- rep(NA, data_list$I_tm_R)
data_list$tm_discount_at_ub_tm_R_ordered <- rep(NA, data_list$I_tm_R)
for(r in 1:data_list$D_R_tm_scheme){
  n0 = data_list$R_tm_scheme_cutoffs[r]+1;
  n1 = data_list$R_tm_scheme_cutoffs[r+1];
  tempRtm = data_list$p_R_ftr_wo_clm_w_tm_R_ordered[n0:n1]
  tempR0 = data_list$p_R_ftr_wo_clm_baseline_tm_R_ordered[n0:n1]
  temptmdisc = tempRtm/tempR0
  ub = TM_DISCOUNT_BOUNDS[[r]]$ub
  lb = TM_DISCOUNT_BOUNDS[[r]]$lb
  data_list$tm_discount_at_lb_tm_R_ordered[n0:n1] = (temptmdisc == lb) * 1
  data_list$tm_discount_at_ub_tm_R_ordered[n0:n1] = (temptmdisc == ub) * 1
}
data_list$theta_mh_llambda <- MH_PARAMS

if(grepl("mhhet", model_config)){
  # Load MH-het cost model init (find the .Rda file by pattern)
  mhhet_files <- list.files(COST_MHHET_MODEL_DIR, pattern = "^result_as_init.*\\.Rda$", full.names = TRUE)
  if (length(mhhet_files) == 0) stop("No result_as_init .Rda found in ", COST_MHHET_MODEL_DIR)
  temp <- readRDS(mhhet_files[1])
  # data_list$theta_mh_llambda_rc <- rep(0, data_list$D_R_tm_scheme)
  data_list$theta_mh_llambda_rc <- temp$theta_mh_llambda_rc
  data_list$theta_mh_llambda <- temp$theta_mh_llambda
}

# summary(data_list$N_choice_to_I_choice[1:I] - seq(I))
# summary(data_list$N_to_N_choice[seq(I)] - seq(I))
full_oo_cov_name_1 = colnames(data_list$prices_oo_full)
full_oo_cov_name_2 = full_oo_cov_name_1[!grepl("_3",full_oo_cov_name_1)]
# get min and median priced firm indices
firm_indices = str_extract(full_oo_cov_name_2[1:5], "^[^_]+")
min_price_firm_index = which.min(colMeans(data_list$prices_oo_nb_tm_full_2[,grepl("_4", full_oo_cov_name_2)]))
# Uses median-priced OO firm: the median better represents the typical alternative a consumer evaluates vs. the cheapest competitor.
median_price_firm_index = which.median(colMeans(data_list$prices_oo_nb_tm_full_2[,grepl("_4", full_oo_cov_name_2)]))  #& !grepl("2_",full_oo_cov_name)[2]

min_price_firm_cov_indices = grepl(paste0(firm_indices[min_price_firm_index],"_"),full_oo_cov_name_1)
median_price_firm_cov_indices = grepl(paste0(firm_indices[median_price_firm_index], "_"), full_oo_cov_name_1)
data_list$prices_oo_nb_1 = data_list$prices_oo_nb_full_1[,median_price_firm_cov_indices]
data_list$prices_oo_nb_tm_1 = data_list$prices_oo_nb_tm_full_1[,median_price_firm_cov_indices]
data_list$prices_renw_oo_1 = data_list$prices_renw_oo_full_1[,median_price_firm_cov_indices]
data_list$prices_kc_nb_1 = data_list$prices_oo_nb_full_1[,min_price_firm_cov_indices]
data_list$prices_kc_nb_tm_1 = data_list$prices_oo_nb_tm_full_1[,min_price_firm_cov_indices]
data_list$prices_kc_renw_1 = data_list$prices_renw_oo_full_1[,min_price_firm_cov_indices]

min_price_firm_cov_indices = grepl(paste0(firm_indices[min_price_firm_index],"_"),full_oo_cov_name_2)
median_price_firm_cov_indices = grepl(paste0(firm_indices[median_price_firm_index], "_"), full_oo_cov_name_2)
data_list$prices_oo_nb_2 = data_list$prices_oo_nb_full_2[,median_price_firm_cov_indices]
data_list$prices_oo_nb_tm_2 = data_list$prices_oo_nb_tm_full_2[,median_price_firm_cov_indices]
data_list$prices_renw_oo_2 = data_list$prices_renw_oo_full_2[,median_price_firm_cov_indices]
data_list$prices_kc_nb_2 = data_list$prices_oo_nb_full_2[,min_price_firm_cov_indices]
data_list$prices_kc_nb_tm_2 = data_list$prices_oo_nb_tm_full_2[,min_price_firm_cov_indices]
data_list$prices_kc_renw_2 = data_list$prices_renw_oo_full_2[,min_price_firm_cov_indices]

data_list$N_to_N_choice = rep(NA, data_list$N)
for(n in 1:data_list$N_choice){
  data_list$N_to_N_choice[data_list$N_choice_to_N[n]] = n
}
data_list$N_choice_to_I_choice = data_list$N_to_N_choice[data_list$N_choice_to_I]

# Load sev/price params from bootstrap CSVs when needed for profit calculations.
# Uses parse_bootstrap_csv() to read CSVs uniformly (same format as model_main).
if (is.null(data_list$pareto_alpha_choice_base) || is.null(data_list$sev_minor_mean_choice)) {
  if (!is.na(model_config) && !model_config %in% c("sev", "price")) {
    if (!exists("parse_bootstrap_csv")) source("code/simulate/functions/sim_helpers.R")
    data_list <- load_sev_price_params(data_list, MODEL_OUT_DIR, bootstrap_id)
  }
}

# Ensure pareto_alpha_choice_base exists as a top-level variable
if (!exists("pareto_alpha_choice_base") && !is.null(data_list$pareto_alpha_choice_base)) {
  pareto_alpha_choice_base <- data_list$pareto_alpha_choice_base
}
