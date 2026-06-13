################################################################################
## sim_c7b_model_fit_mhhet.R — Generate Tab C.2 (MH-het model fit)
##
## Same pipeline as sim_c7_model_fit.R but for model_main_mhhet.
## Produces: MODEL_FIT_DIR/tab_c2.csv, MODEL_FIT_DIR/tab_c2_block2.csv
################################################################################

if (!exists("MODEL_FIT_DIR")) { USE_SIMULATED_DATA <- TRUE; source("code/config.R") }
source("code/simulate/functions/sim_helpers.R")

# Check estimation output exists
mhhet_csv <- file.path(MODEL_OUT_DIR, "model_main_mhhet", "results", "bootstrap-result-id-0.csv")
if (!file.exists(mhhet_csv)) {
  cat("sim_c7b_model_fit_mhhet.R: SKIP — model_main_mhhet not estimated\n")
} else {

cat("sim_c7b_model_fit_mhhet.R: Extracting mhhet model fit CSVs...\n")
dir.create(MODEL_FIT_DIR, recursive = TRUE, showWarnings = FALSE)

## ---- Set up environment for extraction pipeline -----------------------------

model_name <- "model_main_mhhet"
model_config <- "mhhet"
bootstrap_id <- 0
estimation_type <- "opt"
use_mle <- TRUE

library(cmdstanr)
Sys.setenv(STAN_THREADS = "TRUE")

USE_SIMULATED_ESTIMATES <- TRUE
sim_model_dir <- file.path(MODEL_OUT_DIR, model_name)
results_dir <- sim_model_dir
outdir <- sim_model_dir
bootpath <- file.path(sim_model_dir, "results")
bootstrap_results_dir <- bootpath

sim_logs <- list.files(sim_model_dir, pattern = "^log_.*\\.Rda$")
have_stan_artifacts <- length(sim_logs) > 0
if (have_stan_artifacts) {
  log_file_suffix <- sub(".*-([0-9]+-[0-9]+-.*)\\.Rda$", "\\1", sim_logs[length(sim_logs)])
} else {
  cat("sim_c7b_model_fit_mhhet.R: SKIP --- no Stan log file in ", sim_model_dir, "\n",
      "  (model_main_mhhet was cache-restored without running estimation; the\n",
      "   downstream Stan-model recompilation in extract_model_estimates_cmdstan.R\n",
      "   would have nothing to bind to. Tab C.2 will not be regenerated this run.\n",
      "   To enable: run with USE_CACHE=FALSE, or apply the warm-start refactor\n",
      "   so that sim_c6c writes a fresh log_*.Rda alongside the bootstrap CSV.)\n", sep = "")
}

if (have_stan_artifacts) {

# Ensure X_rc_clm exists (needed by mhhet branch in load_estimation_bootstrap_data.R)
dl_rds <- file.path(SIM_CACHE_DIR, "data_list_IL.rds")
if (file.exists(dl_rds)) {
  dl_tmp <- readRDS(dl_rds)
  if (!"X_rc_clm" %in% names(dl_tmp) && "X_rc" %in% names(dl_tmp)) {
    dl_tmp$X_rc_clm <- dl_tmp$X_rc[dl_tmp$N_clm_to_I]
    saveRDS(dl_tmp, dl_rds)
    cat("  Added X_rc_clm to data_list\n")
  }
  rm(dl_tmp)
}

## ---- Run extraction pipeline ------------------------------------------------

cat("  Loading model estimates and compiling Stan...\n")
source("code/simulate/functions/estimation/extract_model_estimates_cmdstan.R")

cat("  Computing cost/pricing predictions...\n")
source("code/simulate/functions/estimation/get_fit_cp.R")

cat("  Computing demand latent parameters...\n")
d_tm_mh_rational_ind <- ifelse(is.null(d_tm_mh_rational_ind), 1, d_tm_mh_rational_ind)
source("code/simulate/functions/estimation/get_d_latentparams.R")

cat("  Computing demand choice probabilities...\n")
source("code/simulate/functions/estimation/get_d_pred.R")

## ---- Build tab_c2.csv (same structure as tab_4) -----------------------------

cat("  Building tab_c2.csv...\n")

risk_m1_d <- get_samp_wgt_avg(data_list$clm_count_N_clm, 1/data_list$sampling_enum_clm)
risk_m1_p <- get_samp_wgt_avg(lambda_clm, 1/data_list$sampling_enum_clm)
risk_m2_d <- get_samp_wgt_avg(data_list$clm_count_N_clm^2, 1/data_list$sampling_enum_clm)
risk_m2_p <- get_samp_wgt_avg(lambda_clm + lambda_clm^2, 1/data_list$sampling_enum_clm)
risk_major_d <- get_samp_wgt_avg(data_list$clm_count_severe_N_clm, 1/data_list$sampling_enum_clm)
risk_major_p <- get_samp_wgt_avg(lambda_severe_clm, 1/data_list$sampling_enum_clm)
risk_N <- sum(data_list$sampling_enum_clm)

score_m1_d <- get_samp_wgt_avg(data_list$log_tm_score_R_ordered, 1/data_list$sampling_enum_tm_R)
score_m1_p <- get_samp_wgt_avg(log_score_est, 1/data_list$sampling_enum_tm_R)
score_m2_d <- get_samp_wgt_avg(data_list$log_tm_score_R_ordered^2, 1/data_list$sampling_enum_tm_R)
score_m2_p <- get_samp_wgt_avg(log_score_est^2, 1/data_list$sampling_enum_tm_R)
score_cov_d <- get_samp_wgt_avg(data_list$log_tm_score_R_ordered * clm_cnt_i[I_tm_R_ordered_to_N_clm],
                                 1/data_list$sampling_enum_tm_R)
score_cov_p <- get_samp_wgt_avg(log_score_est * lambda_i[I_tm_R_ordered_to_N_clm],
                                 1/data_list$sampling_enum_tm_R)
score_N <- sum(data_list$sampling_enum_tm_R)

I <- data_list$I
mask1 <- data_list$N_choice_to_N_R[1:I]
mask2 <- data_list$N_choice_to_N_R[data_list$renw_cnt_choice > 1]
price1_m1_d <- get_samp_wgt_avg(data_list$p_R_ftr_wo_clm_R[mask1], 1/data_list$sampling_enum_choice[mask1])
price1_m1_p <- get_samp_wgt_avg(p_R_nb_est[mask1], 1/data_list$sampling_enum_choice[mask1])
price1_m2_d <- get_samp_wgt_avg(data_list$p_R_ftr_wo_clm_R[mask1]^2, 1/data_list$sampling_enum_choice[mask1])
price1_m2_p <- get_samp_wgt_avg(p_R_nb_est[mask1]^2, 1/data_list$sampling_enum_choice[mask1])
price1_cov_d <- get_samp_wgt_avg(data_list$p_R_ftr_wo_clm_R[mask1] * clm_cnt_i[data_list$N_R_to_N_clm][mask1],
                                  1/data_list$sampling_enum_choice[mask1])
price1_cov_p <- get_samp_wgt_avg(p_R_nb_est[mask1] * lambda[data_list$N_R_to_N_clm][mask1],
                                  1/data_list$sampling_enum_choice[mask1])
price1_N <- sum(data_list$sampling_enum_choice[mask1])

price2_m1_d <- get_samp_wgt_avg(data_list$p_R_ftr_wo_clm_R[mask2], 1/data_list$sampling_enum_choice[mask2])
price2_m1_p <- get_samp_wgt_avg(p_R_renw_est[mask2], 1/data_list$sampling_enum_choice[mask2])
price2_m2_d <- get_samp_wgt_avg(data_list$p_R_ftr_wo_clm_R[mask2]^2, 1/data_list$sampling_enum_choice[mask2])
price2_m2_p <- get_samp_wgt_avg(p_R_renw_est[mask2]^2, 1/data_list$sampling_enum_choice[mask2])
price2_cov_d <- get_samp_wgt_avg(data_list$p_R_ftr_wo_clm_R[mask2] * clm_cnt_i[data_list$N_clm_to_I[data_list$N_R_to_N_clm]][mask2],
                                  1/data_list$sampling_enum_choice[mask2])
price2_cov_p <- get_samp_wgt_avg(p_R_renw_est[mask2] * lambda[data_list$N_R_to_N_clm][mask2],
                                  1/data_list$sampling_enum_choice[mask2])
price2_N <- sum(data_list$sampling_enum_choice[mask2])

tab_c2 <- data.frame(
  panel = c(rep("left", 8), rep("right", 8)),
  section = c(rep("Poisson claim counts", 4), rep("Monitoring score", 4),
              rep("First renewal pricing factor", 4), rep("Latter renewal pricing factor", 4)),
  moment = rep(c("first moment", "major claims", "second moment", "N"), 4),
  data_value = round(c(risk_m1_d, risk_major_d, risk_m2_d, risk_N,
                        score_m1_d, score_m2_d, score_cov_d, score_N,
                        price1_m1_d, price1_m2_d, price1_cov_d, price1_N,
                        price2_m1_d, price2_m2_d, price2_cov_d, price2_N), 6),
  pred_value = round(c(risk_m1_p, risk_major_p, risk_m2_p, NA,
                        score_m1_p, score_m2_p, score_cov_p, NA,
                        price1_m1_p, price1_m2_p, price1_cov_p, NA,
                        price2_m1_p, price2_m2_p, price2_cov_p, NA), 6),
  stringsAsFactors = FALSE
)
score_mask <- tab_c2$section == "Monitoring score"
tab_c2$moment[score_mask] <- c("first moment", "second moment", "covariance with risk", "N")

write.csv(tab_c2, file.path(MODEL_FIT_DIR, "tab_c2.csv"), row.names = FALSE)
cat("    -> tab_c2.csv\n")

## ---- Build tab_c2_block2.csv (mirrors tab_5: Coverage Share, Selection %, TM
##      Share, TM Selection %, Attrition Share, N — but with MH-het estimates) ---

cat("  Building tab_c2_block2.csv...\n")

cov_labels_list <- list(
  paste0(data_list$limits_1 * 2), paste0(data_list$limits_1 * 2),
  paste0(data_list$limits_2 * 2), paste0(data_list$limits_2 * 2),
  paste0(data_list$limits_1 * 2), paste0(data_list$limits_2 * 2)
)

Js <- data_list$Js

demand_rows <- list()
.add_demand <- function(type, label, block, data_val, pred_val, scale_pct = TRUE) {
  if (scale_pct) { data_val <- data_val * 100; pred_val <- pred_val * 100 }
  demand_rows[[length(demand_rows) + 1]] <<- data.frame(
    type = type, label = label, block = as.integer(block),
    data_val = round(data_val, 2), pred_val = round(pred_val, 2),
    stringsAsFactors = FALSE
  )
}

# Coverage share and selection by block
for (k in 1:data_list$N_choice_regimes) {
  n0 <- data_list$n_choice_regime_cutoffs[k]
  n1 <- data_list$n_choice_regime_cutoffs[k + 1] - 1
  sw <- 1 / data_list$sampling_enum_choice[n0:n1]

  for (j in seq_along(cov_labels_list[[k]])) {
    lab <- cov_labels_list[[k]][j]
    if (k %in% c(2, 4)) {
      idx <- c(j, Js[k] + j)
      share_d <- get_samp_wgt_avg(data_list$d_t_new[n0:n1] %in% idx, sw)
      share_p <- get_samp_wgt_avg(rowSums(exp(log_choice_probs[[k]][, idx])), sw)
      risk_d <- get_samp_wgt_avg((data_list$d_t_new[n0:n1] %in% idx) * clm_cnt_i_choice[n0:n1], sw)
      risk_p <- get_samp_wgt_avg(rowSums(exp(log_choice_probs[[k]][, idx])) * lambda_i_choice[n0:n1], sw)
    } else {
      share_d <- get_samp_wgt_avg(data_list$d_t_new[n0:n1] == j, sw)
      share_p <- get_samp_wgt_avg(exp(log_choice_probs[[k]][, j]), sw)
      risk_d <- get_samp_wgt_avg((data_list$d_t_new[n0:n1] == j) * clm_cnt_i_choice_n[n0:n1], sw)
      risk_p <- get_samp_wgt_avg(exp(log_choice_probs[[k]][, j]) * lambda_i_choice_n[n0:n1], sw)
    }
    if (j == 1) { risk0_d <- risk_d; risk0_p <- risk_p }
    .add_demand("Coverage Share", lab, k, share_d, share_p)
    .add_demand("Selection %", lab, k, risk_d / risk0_d, risk_p / risk0_p)
  }
}

# TM share and selection (blocks 2, 4)
for (k in c(2, 4)) {
  n0 <- data_list$n_choice_regime_cutoffs[k]
  n1 <- data_list$n_choice_regime_cutoffs[k + 1] - 1
  sw <- 1 / data_list$sampling_enum_choice[n0:n1]

  tm_share_d <- get_samp_wgt_avg(data_list$d_t_new[n0:n1] > Js[k], sw)
  tm_share_p <- get_samp_wgt_avg(rowSums(exp(log_choice_probs[[k]][, (Js[k] + 1):(Js[k] * 2)])), sw)
  tm_risk_d <- get_samp_wgt_avg((data_list$d_t_new[n0:n1] > Js[k]) * clm_cnt_i_choice[n0:n1], sw)
  tm_risk_p <- get_samp_wgt_avg(rowSums(exp(log_choice_probs[[k]][, (Js[k] + 1):(Js[k] * 2)])) * lambda_i_choice[n0:n1], sw)
  nontm_risk_d <- get_samp_wgt_avg((data_list$d_t_new[n0:n1] < (Js[k] + 1)) * clm_cnt_i_choice[n0:n1], sw)
  nontm_risk_p <- get_samp_wgt_avg(rowSums(exp(log_choice_probs[[k]][, 1:Js[k]])) * lambda_i_choice[n0:n1], sw)

  .add_demand("TM Share", "", k, tm_share_d, tm_share_p)
  .add_demand("TM Selection %", "", k, tm_risk_d / nontm_risk_d, tm_risk_p / nontm_risk_p)
}

# Attrition (blocks 5, 6)
for (k in c(5, 6)) {
  n0 <- data_list$n_choice_regime_cutoffs[k]
  n1 <- data_list$n_choice_regime_cutoffs[k + 1] - 1
  sw <- 1 / data_list$sampling_enum_choice[n0:n1]
  att_d <- data_list$d_t_new[n0:n1] == 0
  att_p <- exp(log_choice_probs[[k]])[, Js[k] + 1]

  .add_demand("Attrition Share", "", k, get_samp_wgt_avg(att_d, sw), get_samp_wgt_avg(att_p, sw))
}

# N row for each block
for (k in 1:data_list$N_choice_regimes) {
  n0 <- data_list$n_choice_regime_cutoffs[k]
  n1 <- data_list$n_choice_regime_cutoffs[k + 1] - 1
  .add_demand("N", "", k, sum(data_list$sampling_enum_choice[n0:n1]), 0, scale_pct = FALSE)
}

# Pivot to wide format matching tab_5.csv schema (consumed by build_tab5_body in c3)
demand_df <- do.call(rbind, demand_rows)
tab_c2b_rows <- list()
for (type_val in unique(demand_df$type)) {
  for (label_val in unique(demand_df$label[demand_df$type == type_val])) {
    sub <- demand_df[demand_df$type == type_val & demand_df$label == label_val, ]
    row <- data.frame(type = type_val, label = label_val, stringsAsFactors = FALSE)
    for (b in 1:6) {
      bsub <- sub[sub$block == b, ]
      if (nrow(bsub) > 0) {
        row[[paste0("block", b, "_data")]] <- bsub$data_val[1]
        row[[paste0("block", b, "_pred")]] <- bsub$pred_val[1]
      } else {
        row[[paste0("block", b, "_data")]] <- NA
        row[[paste0("block", b, "_pred")]] <- NA
      }
    }
    tab_c2b_rows[[length(tab_c2b_rows) + 1]] <- row
  }
}
tab_c2b <- do.call(rbind, tab_c2b_rows)
write.csv(tab_c2b, file.path(MODEL_FIT_DIR, "tab_c2_block2.csv"), row.names = FALSE)
cat("    -> tab_c2_block2.csv\n")

cat("sim_c7b_model_fit_mhhet.R: Done.\n")

} # end have_stan_artifacts guard
} # end mhhet_csv check
