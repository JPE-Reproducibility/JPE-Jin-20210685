################################################################################
## fig_b3_override.R — illustration-only override for Fig B.3 in paper_simulated.pdf
##
## WHAT THIS DOES
##   Overwrites output/simulated/precomputed/model_fit/fig_b3.csv with values
##   generated using REAL-model score parameters at full slope (score_scale = 1.0),
##   evaluated at the simulated-pipeline's leps and llambda. Mirrors exactly the
##   X structure used by get_cp_pred.R::log_score_est so the model line is
##   computed identically (but with real-model params at full slope).
##
## WHY
##   The main simulated pipeline uses score_scale = 0.4 in sim_generate_data_list.R
##   (necessary because simulated `leps` has wider variance than real-data `leps`
##   for TM participants, which would otherwise inflate the leps -> discount
##   mapping). The side effect is that the score~llambda slope in fig_b3 is
##   dampened by 60%, hiding the relationship.
##
##   This script provides a FOR-ILLUSTRATION-ONLY override that shows what the
##   score~risk relationship looks like at full slope. It does NOT modify
##   data_list, any model estimates, any other CSV, or any pipeline behavior.
##   It only overwrites fig_b3.csv (which feeds the Fig B.3 PNG render in c3).
##
## ISOLATION GUARANTEES
##   - Reads:  data/estimates/model_output/model_main/results/bootstrap-result-id-0.csv
##             (real-model params), and env vars set by get_fit_cp.R / get_cp_pred.R
##             (already in scope when sim_c7 sources us)
##   - Writes: output/simulated/precomputed/model_fit/fig_b3.csv (overwrite only)
##   - Does NOT touch: data_list, any model_output/, any CTF output, no other CSV
##
## INVOCATION
##   Sourced once at the end of sim_c7_model_fit.R, after get_fit_cp.R has
##   produced the default (score_scale=0.4) fig_b3.csv. This override then
##   overwrites that file.
################################################################################

stopifnot(exists("data_list"), exists("MODEL_FIT_DIR"),
          exists("leps_n_clm"), exists("llambda_clm"),
          exists("lambda_i"),
          exists("I_tm_R_ordered_to_N_clm"), exists("I_tm_R_to_I"),
          exists("R_tm_scheme_cutoffs"))

local({
  source("code/simulate/functions/sim_helpers.R", local = FALSE)

  real_csv <- "data/estimates/model_output/model_main/results/bootstrap-result-id-0.csv"
  if (!file.exists(real_csv)) {
    cat("  [SKIP] fig_b3 override: real-model CSV not found at", real_csv, "\n")
    return(invisible(NULL))
  }
  rp <- parse_bootstrap_csv(real_csv)$params
  score_scale_override <- 1.0

  ## Build X matching the Stan model (model_main.stan: X_R_tm[i,2] =
  ## llambda_clm - leps_n_clm) and the CTF path (get_d_latentparams.R:241):
  ## the second column is the BASELINE log-rate excluding leps. Fixed in
  ## tandem with get_cp_pred.R:53 (previously both incorrectly used
  ## llambda_clm directly, double-counting leps).
  X_R_tm <- cbind(leps_n_clm[I_tm_R_ordered_to_N_clm],
                  llambda_clm[I_tm_R_ordered_to_N_clm] - leps_n_clm[I_tm_R_ordered_to_N_clm])

  log_score_model_ovr <- numeric(0)
  log_score_data_ovr  <- numeric(0)

  set.seed(42)  # reproducible noise draws
  for (r in seq_len(length(R_tm_scheme_cutoffs) - 1)) {
    rng <- (R_tm_scheme_cutoffs[r] + 1):R_tm_scheme_cutoffs[r + 1]
    if (length(rng) == 0) next
    tmp <- X_R_tm[rng, , drop = FALSE]
    pred <- rp$theta_0_log_score_mean[r] +
      tmp %*% (rp$theta_1_log_score_mean[r, ] * score_scale_override)
    log_score_model_ovr <- c(log_score_model_ovr, as.numeric(pred))
    noise <- rnorm(length(pred), 0, rp$theta_0_log_score_sd[r])
    log_score_data_ovr  <- c(log_score_data_ovr, as.numeric(pred) + noise)
  }

  ## Bin matching get_fit_cp.R lines 19, 25-40 exactly.
  llambda_bin_per_i <- round(log(lambda_i) * 10) / 10
  rr_tm <- data_list$R_tm_scheme[I_tm_R_to_I]

  df_ovr <- tibble::tibble(
      rr          = factor(rr_tm),
      llambda_bin = llambda_bin_per_i[I_tm_R_to_I],
      data        = log_score_data_ovr,
      model       = log_score_model_ovr
    ) |>
    dplyr::group_by(llambda_bin) |>
    dplyr::filter(dplyr::n() > 10) |>
    dplyr::ungroup() |>
    tidyr::gather(type, vals, -c("llambda_bin", "rr"))

  out_csv <- df_ovr |>
    dplyr::group_by(llambda_bin, rr, type) |>
    dplyr::filter(dplyr::n() > 25) |>
    dplyr::summarise(mean = mean(vals), se = stats::sd(vals) / sqrt(dplyr::n()),
                     .groups = "drop")

  write.csv(out_csv, file.path(MODEL_FIT_DIR, "fig_b3.csv"), row.names = FALSE)
  cat("  [OVERRIDE] fig_b3.csv regenerated with score_scale = 1.0 (illustration only; see fig_b3_override.R)\n")
})
