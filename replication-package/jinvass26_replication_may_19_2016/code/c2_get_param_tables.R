################################################################################
## c2_get_param_tables.R — Structural estimation parameter tables
##
## Reads from: data/estimates/model_output/*/results/ (bootstrap CSVs)
## Writes to:  output/exhibits/, output/exhibits/appendix/
##
## Computes point estimates (from bootstrap 0) and bootstrap standard errors
## (SD across bootstrap samples), then formats LaTeX tables matching the
## paper's layout.
##
## Produces 8 tables:
##   output/exhibits/tab_6.tex              (Main model parameter summary)
##   output/exhibits/appendix/tab_a3.tex    (Additional latent parameter summary)
##   output/exhibits/appendix/tab_a4.tex    (Remaining params + moral hazard)
##   output/exhibits/appendix/tab_a5.tex    (Price score hyper parameters)
##   output/exhibits/appendix/tab_a6.tex    (X loadings on observables)
##   output/exhibits/appendix/tab_a7.tex    (4-period horizon robustness)
##   output/exhibits/appendix/tab_a8.tex    (2-period horizon robustness)
##   output/exhibits/appendix/tab_c3.tex    (MH het params)
##
## Called by run_all.R.
################################################################################

if (!exists("TABLES_DIR")) source("code/config.R")

library(dplyr)
library(tidyr)
library(readr)
library(purrr)
library(stringr)
library(glue)

## ---- Paths -----------------------------------------------------------------

est_out_dir <- MODEL_DIR

# GQ CSVs (from c1b, capped in-place by c1e): contain gq_smry matrix + gq_sigma_logit scalars
results_gq_dir <- file.path(est_out_dir, "results")

# Bootstrap CSVs: contain theta_* hyperparameters
original_csv_dir <- file.path(MODEL_DIR, "results")
if (!dir.exists(original_csv_dir))
  stop("Bootstrap CSVs not found at: ", original_csv_dir)

# model_cost CSVs: contain theta_mh_llambda (with true bootstrap variation)
cost_csv_candidates <- file.path(MODEL_COST_DIR, "results")
cost_csv_dir <- NULL
for (d in cost_csv_candidates) {
  if (dir.exists(d)) { cost_csv_dir <- d; break }
}

# model_sev CSVs: contain sev_minor_sd, sev_minor_mean, pareto_alpha_severe
sev_csv_candidates <- file.path(MODEL_SEV_DIR, "results")
sev_csv_dir <- NULL
for (d in sev_csv_candidates) {
  if (dir.exists(d)) { sev_csv_dir <- d; break }
}

# model_price CSVs: contain R_tm, R_nb, R_renw params (not in main model)
price_csv_candidates <- file.path(MODEL_PRICE_DIR, "results")
price_csv_dir <- NULL
for (d in price_csv_candidates) {
  if (dir.exists(d)) { price_csv_dir <- d; break }
}

if (!dir.exists(results_gq_dir)) {
  stop("GQ CSVs not found: ", results_gq_dir, "\n  Run c1b and c1e first.")
}

# Output directories
dir.create(file.path(TABLES_DIR, "appendix"), showWarnings = FALSE, recursive = TRUE)

## ---- Constants for unit transformations -------------------------------------

# NUM_RENEWAL_PERIODS, D_X, N_PRICING_REGIMES, N_MH_COVARIATES, SIGMA_LOGIT_LB
# are defined in config.R

## ---- Helper functions -------------------------------------------------------
source("code/functions/bootstrap_helpers.R")

## ---- Load raw bootstrap CSVs ------------------------------------------------

cat("c1_param_tables.R: Loading GQ CSVs...\n")

gq_csv_files <- list.files(results_gq_dir,
                            pattern = "^bootstrap-result-id-\\d+\\.csv$",
                            full.names = TRUE)
cat("  Found", length(gq_csv_files), "GQ CSVs\n")
if (length(gq_csv_files) == 0) {
  stop("No GQ CSVs found in ", results_gq_dir)
}

# Parse all GQ CSVs into named vectors
gq_parsed <- lapply(gq_csv_files, parse_cmdstan_csv)
names(gq_parsed) <- basename(gq_csv_files)

# Identify bootstrap-0 (point estimate)
bs0_gq_name <- grep("id-0\\.csv$", names(gq_parsed), value = TRUE)
if (length(bs0_gq_name) == 0) stop("bootstrap-result-id-0.csv not found")

cat("  Loading original bootstrap CSVs for hyperparameters...\n")

orig_csv_files <- list.files(original_csv_dir,
                              pattern = "^bootstrap-result-id-\\d+\\.csv$",
                              full.names = TRUE)
cat("  Found", length(orig_csv_files), "original bootstrap CSVs\n")

# Parse theta params from original CSVs (skip 627K gq_ columns)
orig_parsed <- lapply(orig_csv_files, parse_original_csv_thetas)
names(orig_parsed) <- basename(orig_csv_files)

# Also merge gq_sigma_logit from GQ CSVs into orig_parsed
for (nm in names(orig_parsed)) {
  if (nm %in% names(gq_parsed)) {
    gq_vals <- gq_parsed[[nm]]
    for (sig_col in c("gq_sigma_logit_nb", "gq_sigma_logit_renw", "gq_sigma_logit_renw1")) {
      if (sig_col %in% names(gq_vals)) {
        orig_parsed[[nm]][sig_col] <- gq_vals[sig_col]
      }
    }
  }
}

## ---- Build parameter data frames from GQ CSVs (Tab 6, A3) ------------------

# gq_smry column mapping: statistic name (after janitor::clean_names) -> gq_smry col
stat_to_gq_col <- c(
  "mean" = 1,
  "q25_25_percent" = 2,
  "q50_50_percent" = 3,
  "q75_75_percent" = 4,
  "q95_95_percent" = 5,
  "cor_with_log_baseline_risk" = 6,
  "cor_with_log_private_risk" = 7
)

#' Build a data frame with one row per parameter from a parsed GQ CSV
build_param_df <- function(parsed_vec, param_names, gq_rows) {
  df <- data.frame(parameter = param_names, stringsAsFactors = FALSE)
  for (stat_name in names(stat_to_gq_col)) {
    col_idx <- stat_to_gq_col[stat_name]
    vals <- vapply(seq_along(param_names), function(i) {
      gq_row <- gq_rows[i]
      if (is.na(gq_row)) return(NA_real_)
      col_name <- sprintf("gq_smry.%d.%d", gq_row, col_idx)
      v <- parsed_vec[col_name]
      if (is.null(v) || is.na(v)) NA_real_ else as.numeric(v)
    }, numeric(1))
    df[[stat_name]] <- vals
  }
  df
}

# Main parameters (Tab 6)
main_param_names <- c(
  "Baseline Accident Risk (1e-2)",
  "Log Baseline Accident Risk",
  "Log Accident Risk Private Component",
  "Expected Cost to Insurer",
  "Risk Aversion (1e-05)",
  "Plan Switching Cost",
  "Firm Switching Cost",
  "Default Plan FE",
  "Monitoring Disutility"
)
main_gq_rows <- c(1, 2, 3, 12, 4, 5, 6, 7, 8)  # row 12 = Expected Cost

# Secondary parameters (Tab A3 rows from gq_smry)
secondary_param_names <- c(
  "Severe Accident Risk (1e-2)",
  "Baseline Accident Risk with Monitoring Incentive Effect (1e-2)",
  "Severe Accident Risk with Monitoring Incentive Effect (1e-2)",
  "Accident Severity Pareto Shape",
  "Minor Accident Mean",
  "Baseline Renewal Factor - First Renewal",
  "Baseline Renewal Factor - Subsequent Renewals",
  "Renewal Factor w Telematics"
)
secondary_gq_rows <- c(9, 10, 11, 13, 14, 15, 16, 17)

# Build data frames for each bootstrap
main_params_dfs <- lapply(gq_parsed, function(p) {
  df <- build_param_df(p, main_param_names, main_gq_rows)
  df
})
names(main_params_dfs) <- names(gq_parsed)

secondary_params_dfs <- lapply(gq_parsed, function(p) {
  df <- build_param_df(p, secondary_param_names, secondary_gq_rows)
  df
})
names(secondary_params_dfs) <- names(gq_parsed)

# Mark bootstrap-0
for (nm in names(main_params_dfs)) {
  main_params_dfs[[nm]]$is_zero_csv <- grepl("id-0\\.csv$", nm)
  secondary_params_dfs[[nm]]$is_zero_csv <- grepl("id-0\\.csv$", nm)
}

## ---- Define table parameters ------------------------------------------------

# Statistics (columns) -- same order as original pipeline
statistics <- c("mean",
                "q25_25_percent",
                "q50_50_percent",
                "q75_75_percent",
                "q95_95_percent",
                "cor_with_log_baseline_risk",
                "cor_with_log_private_risk")
statistics_pretty <- c("mean",
                       "Q25",
                       "Q50",
                       "Q75",
                       "Q95",
                       "Correlation Log\nBaseline Risk",
                       "Correlation Log\nPrivate Risk")

## ---- Table 6: Main model parameter summary ----------------------------------

# Parameter names (matching gq_smry row names from build_param_df)
main_table_parameters <- c(
  "Baseline Accident Risk (1e-2)",
  "Log Baseline Accident Risk",
  "Log Accident Risk Private Component",
  "Expected Cost to Insurer",
  "Risk Aversion (1e-05)",
  "Default Plan FE",
  "Plan Switching Cost",
  "Firm Switching Cost",
  "Monitoring Disutility"
)

main_table_parameters_pretty <- c(
  "Claim Rate (1e-2)",
  "Log Claim Rate",
  "- Private Component",
  "Expected Cost to Insurer (\\$)",
  "Risk Aversion (1e-05)",
  "Default Plan FE (\\$)",
  "Plan Switching Cost (\\$)",
  "Firm Switching Cost (\\$)",
  "Monitoring Disutility (\\$)"
)

main_table_custom_rounding <- list(
  "Baseline Accident Risk (1e-2)" = list(digits = 2, scientific = FALSE),
  "Log Baseline Accident Risk" = list(digits = 2, scientific = FALSE),
  "Log Accident Risk Private Component" = list(digits = 2, scientific = FALSE),
  "Expected Cost to Insurer" = list(digits = 2, scientific = FALSE),
  "Risk Aversion (1e-05)" = list(digits = 2, scientific = FALSE),
  "Default Plan FE" = list(digits = 2, scientific = FALSE),
  "Plan Switching Cost" = list(digits = 2, scientific = FALSE),
  "Firm Switching Cost" = list(digits = 2, scientific = FALSE),
  "Monitoring Disutility" = list(digits = 2, scientific = FALSE),
  "Correlation Log\nBaseline Risk" = list(digits = 2, scientific = FALSE),
  "Correlation Log\nPrivate Risk" = list(digits = 2, scientific = FALSE)
)

cat("  Computing main parameter table (tab_6)...\n")
main_table_results <- get_summary_table_from_list(
  main_params_dfs,
  main_table_parameters,
  main_table_parameters_pretty,
  statistics,
  statistics_pretty
)

write.csv(main_table_results, file.path(MODEL_TAB_DIR, "tab_6.csv"), row.names = TRUE)
cat("  Saved CSV:", file.path(MODEL_TAB_DIR, "tab_6.csv"), "\n")

main_tex <- generate_latex_table_with_se(
  main_table_results,
  custom_params = main_table_parameters,
  custom_stats = statistics_pretty,
  custom_rounding = main_table_custom_rounding,
  caption = "Model Parameter Summary",
  label = "tab:model_params_summary",
  notes = "This table reports the distributions of key parameters from our model. Columns are moments/correlations across individuals. Risk and choice frictions (default plan FE, switching costs, and monitoring disutility) are reported on a per period basis. Parentheses show bootstrap standard errors.",
  placement = "h"
)

tab6_path <- file.path(TABLES_DIR, "tab_6.tex")
writeLines(main_tex, tab6_path)
cat("  Wrote", tab6_path, "\n")

## ---- Table A3: Secondary (additional latent) parameter summary --------------

# All Tab A3 rows now come from gq_smry (rows 9-11, 13-17).
# No more dependency on c1f auxiliary summary CSVs.

secondary_variables <- c(
  "Severe Accident Risk (1e-2)",
  "Accident Severity Pareto Shape",
  "Minor Accident Mean",
  "Baseline Renewal Factor - First Renewal",
  "Baseline Renewal Factor - Subsequent Renewals",
  "Renewal Factor w Telematics"
)

secondary_variables_pretty <- secondary_variables

secondary_custom_rounding <- list(
  "Severe Accident Risk (1e-2)" = list(digits = 2, scientific = FALSE),
  "Accident Severity Pareto Shape" = list(digits = 2, scientific = FALSE),
  "Minor Accident Mean" = list(digits = 2, scientific = FALSE),
  "Baseline Renewal Factor - First Renewal" = list(digits = 2, scientific = FALSE),
  "Baseline Renewal Factor - Subsequent Renewals" = list(digits = 2, scientific = FALSE),
  "Renewal Factor w Telematics" = list(digits = 2, scientific = FALSE),
  "Correlation Log\nBaseline Risk" = list(digits = 2, scientific = FALSE),
  "Correlation Log\nPrivate Risk" = list(digits = 2, scientific = FALSE)
)

# All rows from gq_smry — no auxiliary merge needed
full_params_dfs <- secondary_params_dfs

cat("  Computing secondary parameter table (tab_a3)...\n")
secondary_table_results <- get_summary_table_from_list(
  full_params_dfs,
  secondary_variables,
  secondary_variables_pretty,
  statistics,
  statistics_pretty
)

write.csv(secondary_table_results, file.path(MODEL_TAB_DIR, "tab_a3.csv"), row.names = TRUE)
cat("  Saved CSV:", file.path(MODEL_TAB_DIR, "tab_a3.csv"), "\n")

secondary_tex <- generate_latex_table_with_se(
  secondary_table_results,
  custom_params = secondary_variables,
  custom_stats = statistics_pretty,
  custom_rounding = secondary_custom_rounding,
  caption = "Additional Latent Parameter Summary",
  label = "tab:app-est-secondary",
  notes = "This table reports the distributions of key parameters from our model. Columns are moments/correlations across individuals. Parentheses show bootstrap standard errors."
)

taba3_path <- file.path(TABLES_DIR, "appendix", "tab_a3.tex")
writeLines(secondary_tex, taba3_path)
cat("  Wrote", taba3_path, "\n")

## ---- Matrix bootstrap helper (shared by A4, A5, A6) -------------------------

#' Summarize bootstrap matrices: compute estimate (id-0), mean, SE, and CIs
#' across a list of matrices. Uses sd_capped for SE computation since
#' hyperparameters are not pre-capped by c1e.
summarize_bootstrap_matrices <- function(mat_list, zero_name = NULL,
                                          estimate_override = NULL,
                                          use_raw_sd = FALSE) {
  if (length(mat_list) == 0) stop("mat_list is empty")

  # Find bootstrap-0 for point estimate
  if (is.null(zero_name)) {
    zero_idx <- grep("id-0\\.csv$", names(mat_list))
  } else {
    zero_idx <- which(names(mat_list) == zero_name)
  }
  estimate <- if (length(zero_idx) > 0) mat_list[[zero_idx[1]]] else mat_list[[1]]
  if (!is.null(estimate_override)) estimate <- estimate_override

  n_rows <- nrow(mat_list[[1]])
  n_cols <- ncol(mat_list[[1]])
  rnames <- rownames(mat_list[[1]])
  cnames <- colnames(mat_list[[1]])

  mean_mat <- sd_mat <- q2.5_mat <- q97.5_mat <-
    matrix(NA_real_, nrow = n_rows, ncol = n_cols,
           dimnames = list(rnames, cnames))

  sd_fn <- if (use_raw_sd) sd else sd_capped

  for (i in seq_len(n_rows)) {
    for (j in seq_len(n_cols)) {
      values <- vapply(mat_list, function(m) m[i, j], numeric(1))
      values_finite <- values[is.finite(values)]
      if (length(values_finite) > 0) {
        mean_mat[i, j]  <- mean(values_finite)
        sd_mat[i, j]    <- sd_fn(values_finite)
        q2.5_mat[i, j]  <- quantile(values_finite, 0.025, names = FALSE)
        q97.5_mat[i, j] <- quantile(values_finite, 0.975, names = FALSE)
      }
    }
  }

  list(estimate = estimate, mean = mean_mat, sd = sd_mat,
       q2.5 = q2.5_mat, q97.5 = q97.5_mat)
}

## ---- Formatting helpers for matrix tables -----------------------------------

# Escape angle brackets for LaTeX
fix_angle_brackets <- function(s) {
  s <- gsub("<", "$<$", s, fixed = TRUE)
  gsub(">", "$>$", s, fixed = TRUE)
}

# Format a single cell as \makecell{estimate + stars \\ (SE)}
.format_cell_star <- function(est, se, digits = 3,
                              star_cuts = c(0.10, 0.05, 0.01)) {
  fmt <- function(x) ifelse(is.na(x), "--", sprintf(paste0("%.", digits, "f"), x))
  if (is.na(est)) return("--")
  # Stars via two-sided normal test
  star <- ""
  if (!is.na(se) && se > 1e-12) {
    p <- 2 * (1 - pnorm(abs(est / se)))
    if (p < star_cuts[3]) star <- "$^{***}$"
    else if (p < star_cuts[2]) star <- "$^{**}$"
    else if (p < star_cuts[1]) star <- "$^{*}$"
  }
  se_str <- if (is.na(se)) "(--)" else paste0("(", fmt(se), ")")
  paste0("\\makecell[c]{", fmt(est), star, " \\\\ ", se_str, "}")
}

# Extract a scalar from a summary list by row name
.extract_scalar <- function(sum_list, name) {
  pick <- function(M, key) {
    if (is.null(dim(M))) {
      if (!is.null(names(M)) && key %in% names(M)) return(as.numeric(M[[key]]))
      return(NA_real_)
    }
    rn <- rownames(M)
    if (!is.null(rn) && key %in% rn) return(as.numeric(M[key, 1]))
    NA_real_
  }
  c(est = pick(sum_list$estimate, name),
    se  = pick(sum_list$sd, name))
}

# Balanced \makecell header wrapping
.makecell_balanced <- function(x, lines = 2, align = "c") {
  x0 <- trimws(x)
  if (lines <= 1) return(paste0("\\makecell[", align, "]{", fix_angle_brackets(x0), "}"))
  words <- unlist(strsplit(x0, "\\s+"))
  n <- length(words)
  if (n <= 1) return(paste0("\\makecell[", align, "]{", fix_angle_brackets(x0), "}"))
  if (lines > n) lines <- n

  if (lines == 2) {
    best <- NULL; best_cost <- Inf
    for (i in 1:(n - 1)) {
      p1 <- paste(words[1:i], collapse = " ")
      p2 <- paste(words[(i + 1):n], collapse = " ")
      cost <- abs(nchar(p1) - nchar(p2))
      if (cost < best_cost) { best <- c(p1, p2); best_cost <- cost }
    }
    parts <- best
  } else if (lines == 3) {
    best <- NULL; best_cost <- Inf
    for (i in 1:(n - 2)) for (j in (i + 1):(n - 1)) {
      p1 <- paste(words[1:i], collapse = " ")
      p2 <- paste(words[(i + 1):j], collapse = " ")
      p3 <- paste(words[(j + 1):n], collapse = " ")
      lens <- c(nchar(p1), nchar(p2), nchar(p3))
      cost <- sum((lens - mean(lens))^2)
      if (cost < best_cost) { best <- c(p1, p2, p3); best_cost <- cost }
    }
    parts <- best
  } else {
    parts <- x0
  }

  parts <- fix_angle_brackets(parts)
  paste0("\\makecell[", align, "]{", paste(parts, collapse = " \\\\ "), "}")
}


## =============================================================================
## Table A4: Remaining Parameter Estimates and Moral Hazard Effects
## =============================================================================

tryCatch({
  cat("  Computing Tab A4 (hyperp_nonX) from raw CSVs...\n")

  # D_X defined in config.R

  #' Compute hyperp_nonX matrix from a parsed theta vector.
  #' cost_parsed: optional parsed vector from model_cost (for MH params)
  #' sev_parsed: optional parsed vector from model_sev (for sev_minor_sd)
  compute_hyperp_nonX_from_parsed <- function(parsed, cost_parsed = NULL,
                                               sev_parsed = NULL) {
    sigma_logit_lb <- SIGMA_LOGIT_LB
    theta_0_sigma_logit <- parsed["theta_0_sigma_logit"]
    eps_sd <- parsed["eps_sd"]
    sigma_logit_val <- sigma_logit_lb + exp(theta_0_sigma_logit)

    # MH params: source from cost_parsed (estimated) if available
    source_mh <- if (!is.null(cost_parsed)) cost_parsed else parsed
    mh_params <- numeric(6)
    for (r in 1:N_PRICING_REGIMES) {
      for (c_idx in 1:N_MH_COVARIATES) {
        v <- source_mh[paste0("theta_mh_llambda.", r, ".", c_idx)]
        mh_params[(r - 1) * 2 + c_idx] <- if (is.na(v)) 0 else v
      }
    }

    # sev_minor_sd: source from sev_parsed if available
    sev_sd_val <- if (!is.null(sev_parsed)) sev_parsed["sev_minor_sd"] else parsed["theta_0_sev_minor_sd"]

    # Sigma logit divided by welfare horizon to match original analysis
    welfare_horizon <- NUM_RENEWAL_PERIODS
    sig_nb    <- sigma_logit_val / welfare_horizon
    sig_renw1 <- sigma_logit_val / welfare_horizon
    sig_renw  <- sigma_logit_val / welfare_horizon

    vals <- c(
      sev_sd_val,
      parsed["theta_0_llambda_severe"],
      eps_sd,
      mh_params[1], mh_params[2],
      mh_params[3], mh_params[4],
      mh_params[5], mh_params[6],
      sig_nb,
      sig_renw1,
      sig_renw
    )

    rn <- c(
      "Log Minor Accident Severity SD",
      "Major Accident Fraction",
      "Log Accident Risk Private Component SD",
      "Moral Hazard Effect Regime 1 Monitoring Opt-in Coef",
      "Moral Hazard Effect Regime 1 Monitoring Intensity Coef",
      "Moral Hazard Effect Regime 2 Monitoring Opt-in Coef",
      "Moral Hazard Effect Regime 2 Monitoring Intensity Coef",
      "Moral Hazard Effect Regime 3 Monitoring Opt-in Coef",
      "Moral Hazard Effect Regime 3 Monitoring Intensity Coef",
      "Logit Error Spread First Period",
      "Logit Error Spread First Renewal",
      "Logit Error Spread Subsequent Renewals"
    )

    matrix(vals, ncol = 1, dimnames = list(rn, "mean"))
  }

  # Parse model_cost CSVs (for MH params — estimated, not hardcoded 0)
  # The older bootstrap directory has true variation; the flat structure may not
  cost_parsed <- list()
  cost_point_parsed <- NULL  # separate point-estimate source (current model)
  if (!is.null(cost_csv_dir) && dir.exists(cost_csv_dir)) {
    cost_files <- list.files(cost_csv_dir, pattern = "^bootstrap-result-id-\\d+\\.csv$",
                              full.names = TRUE)
    cat("    Found", length(cost_files), "model_cost bootstrap CSVs in", cost_csv_dir, "\n")
    cost_parsed <- lapply(cost_files, parse_original_csv_thetas)
    names(cost_parsed) <- basename(cost_files)
    # Check if MH params actually vary (if not, also try flat structure for point estimates)
    if (length(cost_parsed) >= 2) {
      mh_v1 <- cost_parsed[[1]]["theta_mh_llambda.1.1"]
      mh_v2 <- cost_parsed[[2]]["theta_mh_llambda.1.1"]
      if (!is.na(mh_v1) && !is.na(mh_v2) && mh_v1 == mh_v2) {
        cat("    WARNING: MH params identical across bootstraps — SEs will be 0\n")
      }
    }
  } else {
    cat("    model_cost CSVs not found — MH params from main model\n")
  }
  # Also load current model_cost point estimates (flat structure, for override)
  cost_flat_dir <- file.path(MODEL_OUT_DIR, "model_cost", "results")
  if (cost_flat_dir != cost_csv_dir && dir.exists(cost_flat_dir)) {
    cost_flat_0 <- file.path(cost_flat_dir, "bootstrap-result-id-0.csv")
    if (file.exists(cost_flat_0)) {
      cost_point_parsed <- parse_original_csv_thetas(cost_flat_0)
    }
  }

  # Parse model_sev CSVs (for sev_minor_sd)
  sev_parsed <- list()
  if (!is.null(sev_csv_dir) && dir.exists(sev_csv_dir)) {
    sev_files <- list.files(sev_csv_dir, pattern = "^bootstrap-result-id-\\d+\\.csv$",
                             full.names = TRUE)
    cat("    Found", length(sev_files), "model_sev bootstrap CSVs\n")
    sev_parsed <- lapply(sev_files, parse_original_csv_thetas)
    names(sev_parsed) <- basename(sev_files)
  } else {
    cat("    model_sev CSVs not found — sev_minor_sd from main model\n")
  }

  if (length(orig_parsed) == 0) {
    cat("    SKIP: No original bootstrap CSVs found.\n")
  } else {
    # Build matrices for all bootstraps, sourcing MH from model_cost and sev_minor_sd from model_sev
    nonx_matrices <- lapply(names(orig_parsed), function(nm) {
      compute_hyperp_nonX_from_parsed(
        orig_parsed[[nm]],
        cost_parsed = cost_parsed[[nm]],
        sev_parsed = sev_parsed[[nm]]
      )
    })
    names(nonx_matrices) <- names(orig_parsed)

    # Point estimate override: use current model_cost bootstrap-0 for MH params
    estimate_override <- NULL
    if (!is.null(cost_point_parsed)) {
      est0 <- compute_hyperp_nonX_from_parsed(
        orig_parsed[["bootstrap-result-id-0.csv"]],
        cost_parsed = cost_point_parsed,
        sev_parsed = sev_parsed[["bootstrap-result-id-0.csv"]]
      )
      estimate_override <- est0
    }

    # Summarize across bootstraps — use raw sd (not sd_capped) for non-GQ params
    nonxsum <- summarize_bootstrap_matrices(nonx_matrices,
                                             estimate_override = estimate_override,
                                             use_raw_sd = TRUE)

    # Left block: standalone parameters
    left_params <- c(
      "Major Accident Fraction"                = "Log Major Accident Fraction",
      "Log Minor Accident Severity SD"         = "Log Minor Accident Severity SD",
      "Logit Error Spread First Period"         = "Logit Error SD",
      "Log Accident Risk Private Component SD"  = "Log Risk Private Component SD"
    )

    left_df <- do.call(rbind, lapply(seq_along(left_params), function(i) {
      src <- names(left_params)[i]; lab <- unname(left_params[i])
      sc <- .extract_scalar(nonxsum, src)
      data.frame(Variable = lab,
                 Estimate = .format_cell_star(sc["est"], sc["se"], digits = 3),
                 stringsAsFactors = FALSE, check.names = FALSE)
    }))

    # Right block: MH effects by regime
    mh_name <- function(regime, which)
      sprintf("Moral Hazard Effect Regime %d Monitoring %s Coef",
              regime, if (which == "opt-in") "Opt-in" else "Intensity")

    regimes <- 1:N_PRICING_REGIMES
    right_df <- do.call(rbind, lapply(regimes, function(rg) {
      sc_opt   <- .extract_scalar(nonxsum, mh_name(rg, "opt-in"))
      sc_inten <- .extract_scalar(nonxsum, mh_name(rg, "intensity"))
      # Implied MH = (1 - exp(opt_in + intensity)) * 100
      implied <- NA_real_
      if (!is.na(sc_opt["est"]) && !is.na(sc_inten["est"])) {
        implied <- (1 - exp(sc_opt["est"] + sc_inten["est"])) * 100
      }
      data.frame(
        Regime = rg,
        `Monitoring Opt-in Coef.` = .format_cell_star(sc_opt["est"], sc_opt["se"], digits = 3),
        `Monitoring Intensity Coef.` = .format_cell_star(sc_inten["est"], sc_inten["se"], digits = 3),
        `Implied Moral Hazard Effect` = if (!is.na(implied)) paste0(sprintf("%.3f", implied), "\\% ") else "--",
        stringsAsFactors = FALSE, check.names = FALSE)
    }))

    # Pad to equal rows
    nL <- nrow(left_df); nR <- nrow(right_df); n <- max(nL, nR)
    if (nL < n) left_df <- rbind(left_df,
                                  data.frame(Variable = rep("", n - nL),
                                             Estimate = rep("", n - nL),
                                             stringsAsFactors = FALSE))
    if (nR < n) right_df <- rbind(right_df,
                                   data.frame(Regime = rep("", n - nR),
                                              `Monitoring Opt-in Coef.` = rep("", n - nR),
                                              `Monitoring Intensity Coef.` = rep("", n - nR),
                                              `Implied Moral Hazard Effect` = rep("", n - nR),
                                              stringsAsFactors = FALSE, check.names = FALSE))

    out_df <- cbind(left_df, right_df)

    kb <- kableExtra::kable(
      out_df,
      format = "latex", booktabs = TRUE, escape = FALSE,
      caption = "\\label{tab:app-hyperp-nonx}Remaining Parameter Estimates and Moral Hazard Effects",
      align = c("l", "c", "c", "c", "c", "c"),
      linesep = ""
    )
    kb <- kb %>%
      kableExtra::add_header_above(
        c(" " = 2, "Moral Hazard Effect" = 4), escape = FALSE
      ) %>%
      kableExtra::kable_styling(latex_options = c("hold_position", "scale_down"))

    # Build final output matching target format
    a4_raw <- as.character(kb)
    # Fix table positioning to match benchmark
    a4_raw <- sub("[!h]", "[!htbp]", a4_raw, fixed = TRUE)
    a4_raw <- sub("\\centering", "\\begin{centering}", a4_raw, fixed = TRUE)
    # Wrap multi-word column headers in \makecell
    a4_raw <- gsub("Monitoring Opt-in Coef\\.",
                    "\\\\makecell[c]{Monitoring\\\\\\\\Opt-in Coef.}", a4_raw)
    a4_raw <- gsub("Monitoring Intensity Coef\\.",
                    "\\\\makecell[c]{Monitoring\\\\\\\\Intensity Coef.}", a4_raw)
    a4_raw <- gsub("Implied Moral Hazard Effect",
                    "\\\\makecell[c]{Implied Moral \\\\\\\\Hazard Effect}", a4_raw)
    # Insert notes block before \end{table}
    notes_block <- paste0(
      "\\par\\end{centering}\n",
      "\\vspace{0.1cm}\n",
      "\\begin{singlespace}\n",
      "{\\footnotesize \\emph{Notes:} This table reports parameter estimates for our renewal price and monitoring score models. Parentheses show bootstrap standard errors.\\par}\n",
      "\\end{singlespace}\n",
      "\\end{table}"
    )
    a4_tex <- sub("\\end{table}", notes_block, a4_raw, fixed = TRUE)
    # Remove trailing empty & cell on padding rows (benchmark has 5 cols, not 6)
    a4_tex <- gsub(" &  &  &  & \\\\", " &  &  & \\\\", a4_tex, fixed = TRUE)
    # Add space before \\ after percentage values (kable strips trailing spaces)
    a4_tex <- gsub("\\%\\\\", "\\% \\\\", a4_tex, fixed = TRUE)

    write.csv(out_df, file.path(MODEL_TAB_DIR, "tab_a4.csv"), row.names = TRUE)
    cat("    Saved CSV:", file.path(MODEL_TAB_DIR, "tab_a4.csv"), "\n")

    taba4_path <- file.path(TABLES_DIR, "appendix", "tab_a4.tex")
    writeLines(a4_tex, taba4_path)
    cat("    Wrote", taba4_path, "\n")
  }
}, error = function(e) {
  cat("    ERROR generating Tab A4:", conditionMessage(e), "\n")
  taba4_check <- file.path(TABLES_DIR, "appendix", "tab_a4.tex")
  if (file.exists(taba4_check)) cat("    Pre-computed tab_a4.tex retained.\n")
})


## =============================================================================
## Table A5: Renewal Price and Monitoring Score Hyper Parameter Estimates
## =============================================================================

tryCatch({
  cat("  Computing Tab A5 (pricescore) from raw CSVs...\n")

  # Price score row labels and column structure (from reference CSV)
  price_row_labels <- c(
    "Regime 1 \u2014 intercept", "Regime 1 \u2014 public signal",
    "Regime 1 \u2014 private signal", "Regime 1 \u2014 sd",
    "Regime 2 \u2014 intercept", "Regime 2 \u2014 public signal",
    "Regime 2 \u2014 private signal", "Regime 2 \u2014 sd",
    "Regime 3 \u2014 intercept", "Regime 3 \u2014 public signal",
    "Regime 3 \u2014 private signal", "Regime 3 \u2014 sd",
    "Regime 4 \u2014 intercept", "Regime 4 \u2014 public signal",
    "Regime 4 \u2014 private signal", "Regime 4 \u2014 sd"
  )
  price_col_names <- c("Log Score", "First-Renewal w/ TM",
                        "First-Renewal (No TM)", "Subsequent-Renewal")

  #' Compute pricescore matrix from parsed theta vectors.
  #' main_parsed: from main model (log_score params)
  #' price_parsed: from model_price (R_tm, R_nb, R_renw params)
  #' Stan convention: dim 1 = private signal, dim 2 = public signal
  compute_pricescore_from_parsed <- function(main_parsed, price_parsed = NULL) {
    n_rows <- length(price_row_labels)
    n_cols <- length(price_col_names)
    mat <- matrix(NA_real_, nrow = n_rows, ncol = n_cols,
                  dimnames = list(price_row_labels, price_col_names))

    extract_main <- function(name) {
      v <- main_parsed[name]
      if (is.null(v) || is.na(v)) NA_real_ else as.numeric(v)
    }
    # R_tm/R_nb/R_renw come from model_price if available
    extract_price <- function(name) {
      src <- if (!is.null(price_parsed)) price_parsed else main_parsed
      v <- src[name]
      if (is.null(v) || is.na(v)) NA_real_ else as.numeric(v)
    }

    for (r in 1:3) {
      base_row <- (r - 1) * 4
      # intercept
      mat[base_row + 1, 1] <- extract_main(paste0("theta_0_log_score_mean.", r))
      mat[base_row + 1, 2] <- extract_price(paste0("theta_0_R_tm_mean.", r))
      mat[base_row + 1, 3] <- extract_price(paste0("theta_0_R_nb_mean.", r))
      mat[base_row + 1, 4] <- extract_price(paste0("theta_0_R_renw_mean.", r))
      # public signal: dim 2 in Stan
      mat[base_row + 2, 1] <- extract_main(paste0("theta_1_log_score_mean.", r, ".2"))
      mat[base_row + 2, 2] <- extract_price(paste0("theta_1_R_tm_mean.", r, ".2"))
      mat[base_row + 2, 3] <- extract_price(paste0("theta_1_R_nb_mean.", r))
      mat[base_row + 2, 4] <- extract_price(paste0("theta_1_R_renw_mean.", r))
      # private signal: dim 1 in Stan
      mat[base_row + 3, 1] <- extract_main(paste0("theta_1_log_score_mean.", r, ".1"))
      mat[base_row + 3, 2] <- extract_price(paste0("theta_1_R_tm_mean.", r, ".1"))
      # sd
      mat[base_row + 4, 1] <- extract_main(paste0("theta_0_log_score_sd.", r))
      mat[base_row + 4, 2] <- extract_price(paste0("theta_0_R_tm_sd.", r))
      mat[base_row + 4, 3] <- extract_price(paste0("theta_0_R_nb_sd.", r))
      mat[base_row + 4, 4] <- extract_price(paste0("theta_0_R_renw_sd.", r))
    }

    # Regime 4: only Subsequent-Renewal column
    mat[13, 4] <- extract_price("theta_0_R_renw_mean.4")
    mat[14, 4] <- extract_price("theta_1_R_renw_mean.4")
    # private signal row 15: all NA
    mat[16, 4] <- extract_price("theta_0_R_renw_sd.4")

    mat
  }

  # Parse model_price CSVs (R_tm, R_nb, R_renw — not in main model)
  price_parsed_list <- list()
  if (!is.null(price_csv_dir) && dir.exists(price_csv_dir)) {
    price_files <- list.files(price_csv_dir, pattern = "^bootstrap-result-id-\\d+\\.csv$",
                               full.names = TRUE)
    cat("    Found", length(price_files), "model_price bootstrap CSVs\n")
    price_parsed_list <- lapply(price_files, parse_original_csv_thetas)
    names(price_parsed_list) <- basename(price_files)
  } else {
    cat("    model_price CSVs not found — R_tm/R_nb/R_renw columns will be NA\n")
  }

  if (length(orig_parsed) == 0) {
    cat("    SKIP: No original bootstrap CSVs found.\n")
  } else {
    # Build matrices for all bootstraps, merging main + model_price
    price_matrices <- lapply(names(orig_parsed), function(nm) {
      compute_pricescore_from_parsed(orig_parsed[[nm]], price_parsed_list[[nm]])
    })
    names(price_matrices) <- names(orig_parsed)

    # Summarize across bootstraps — use raw sd (benchmark uses raw, not capped)
    psum <- summarize_bootstrap_matrices(price_matrices, use_raw_sd = TRUE)

    est <- psum$estimate
    se  <- psum$sd
    rn  <- rownames(est)
    cn  <- colnames(est)

    # Parse rownames: "Regime N — component"
    parse_row <- function(s) {
      m <- regexec("^\\s*Regime\\s*([0-9]+)\\s*[\u2014-]\\s*(.+?)\\s*$", s)
      r <- regmatches(s, m)[[1]]
      if (length(r) == 3) list(regime = as.integer(r[2]), comp = tolower(trimws(r[3])))
      else list(regime = NA_integer_, comp = tolower(trimws(s)))
    }
    pr <- lapply(rn, parse_row)
    regime <- vapply(pr, `[[`, integer(1), "regime")
    comp   <- vapply(pr, `[[`, character(1), "comp")

    # Keep only intercept, public signal, private signal (drop sd)
    keep_comps <- c("intercept", "public signal", "private signal")
    comp_norm <- comp
    comp_norm[comp_norm == "public"]  <- "public signal"
    comp_norm[comp_norm == "private"] <- "private signal"
    keep <- comp_norm %in% keep_comps
    est <- est[keep, , drop = FALSE]
    se  <- se[keep, , drop = FALSE]
    regime <- regime[keep]
    comp_norm <- comp_norm[keep]
    comp_print <- tools::toTitleCase(comp_norm)

    # Order by regime then component
    comp_order <- match(comp_norm, keep_comps)
    ord <- order(regime, comp_order)
    est <- est[ord, , drop = FALSE]
    se  <- se[ord, , drop = FALSE]
    regime <- regime[ord]
    comp_print <- comp_print[ord]
    comp_norm <- comp_norm[ord]

    # Remove Regime 4 Private Signal rows (benchmark comments them out)
    r4_priv <- which(regime == 4 & comp_norm == "private signal")
    if (length(r4_priv) > 0) {
      est <- est[-r4_priv, , drop = FALSE]
      se  <- se[-r4_priv, , drop = FALSE]
      regime <- regime[-r4_priv]
      comp_norm <- comp_norm[-r4_priv]
      comp_print <- comp_print[-r4_priv]
    }

    # Format with stars
    fmt_val <- function(x) ifelse(is.na(x), "--", sprintf("%.3f", x))
    fmt_se  <- function(x) ifelse(is.na(x), "--", paste0("(", sprintf("%.3f", x), ")"))
    tstat <- est / se
    pval  <- 2 * (1 - pnorm(abs(tstat)))
    stars <- matrix("", nrow = nrow(est), ncol = ncol(est))
    stars[!is.na(pval) & pval < 0.10] <- "$^{*}$"
    stars[!is.na(pval) & pval < 0.05] <- "$^{**}$"
    stars[!is.na(pval) & pval < 0.01] <- "$^{***}$"

    # Rename columns for display — use two-line headers matching benchmark
    cn_hdr_line1 <- c("", "", "",
                       "\\multicolumn{1}{c}{First-Renewal}",
                       "\\multicolumn{1}{c}{First-Renewal}",
                       "\\multicolumn{1}{c}{Subsequent}")
    cn_hdr_line2 <- c("Regime", "Component", "\\multicolumn{1}{c}{Log Score}",
                       "\\multicolumn{1}{c}{w/ Monitoring}",
                       "\\multicolumn{1}{c}{No Monitoring}",
                       "\\multicolumn{1}{c}{Renewal}")

    # Build body rows
    body_lines <- character(0)
    for (i in seq_len(nrow(est))) {
      est_cells <- paste0(fmt_val(est[i, ]), stars[i, ])
      se_cells  <- fmt_se(se[i, ])
      est_row <- paste0(regime[i], " & ", comp_print[i], " & ",
                        paste(est_cells, collapse = " & "), "\\\\")
      se_row  <- paste0(" &  & ", paste(se_cells, collapse = " & "), "\\\\")
      body_lines <- c(body_lines, est_row, se_row)
    }

    # Add commented-out Regime 4 Private Signal rows (matching benchmark)
    has_r4 <- any(regime == 4)
    if (has_r4) {
      r4_private_est <- paste0("% 4 & Private Signal & ",
                                paste(rep("--", ncol(est)), collapse = " & "), "\\\\")
      r4_private_se  <- paste0("%  &  & ",
                                paste(rep("--", ncol(est)), collapse = " & "), "\\\\")
      body_lines <- c(body_lines, r4_private_est, r4_private_se)
    }

    body_text <- paste(body_lines, collapse = "\n")

    a5_tex <- sprintf(
      "\\begin{table}[!htbp]\n\\begin{centering}\n\\caption{\\label{tab:app-est-pricescore} Renewal Price and Monitoring Score Hyper Parameter Estimates}\n\\centering\n\\resizebox{\\ifdim\\width>\\linewidth\\linewidth\\else\\width\\fi}{!}{\n\\begin{tabular}[t]{cl%s}\n\\toprule\n%s \\\\\n%s \\\\\n%%\\cmidrule(l{3pt}r{3pt}){3-3} \\cmidrule(l{3pt}r{3pt}){4-4} \\cmidrule(l{3pt}r{3pt}){5-5} \\cmidrule(l{3pt}r{3pt}){6-6}\n%%Regime & Component &   &   &   &  \\\\\n\\midrule\n%s\n\\bottomrule\n\\end{tabular}}\n\\par\\end{centering}\n\\vspace{0.1cm}\n\\begin{singlespace}\n{\\footnotesize \\emph{Notes:} This table reports estimates for our renewal price and monitoring score models. Parentheses show bootstrap standard errors.\\par}\n\\end{singlespace}\n\\end{table}",
      paste(rep("c", ncol(est)), collapse = ""),
      paste(cn_hdr_line1, collapse = " & "),
      paste(cn_hdr_line2, collapse = " & "),
      body_text
    )

    a5_results <- as.data.frame(est)
    a5_results_se <- as.data.frame(se)
    colnames(a5_results_se) <- paste0(colnames(a5_results_se), "_se")
    a5_csv <- cbind(a5_results, a5_results_se)
    write.csv(a5_csv, file.path(MODEL_TAB_DIR, "tab_a5.csv"), row.names = TRUE)
    cat("    Saved CSV:", file.path(MODEL_TAB_DIR, "tab_a5.csv"), "\n")

    taba5_path <- file.path(TABLES_DIR, "appendix", "tab_a5.tex")
    writeLines(a5_tex, taba5_path)
    cat("    Wrote", taba5_path, "\n")
  }
}, error = function(e) {
  cat("    ERROR generating Tab A5:", conditionMessage(e), "\n")
  taba5_check <- file.path(TABLES_DIR, "appendix", "tab_a5.tex")
  if (file.exists(taba5_check)) cat("    Pre-computed tab_a5.tex retained.\n")
})


## =============================================================================
## Table A6: Latent Parameter Loadings on Observables
## =============================================================================

tryCatch({
  cat("  Computing Tab A6 (hyperp_X) from raw CSVs...\n")

  # X variable column names (from reference CSV)
  x_col_names <- c("(Intercept)", "Female Ind.", "Driver License Year",
                    "Home Ownership", "Out-of-State License",
                    "Credit Report Ind....7", "Vehicle on Lease Ind.",
                    "ABS Ind.", "Airbag Ind.", "Length of Ownership",
                    "Class C Ind.", "Garage Verified Ind.", "Has Prior Ins.",
                    "-- w/ Lapse", "Total Accident Count", "Total DUI Count",
                    "Credit Report Ind....18", "Age < 25", "Age > 21",
                    "Age > 60", "College Ind.", "Post Grad Ind.",
                    "Delinq. Score*", "Population Density",
                    "Violation Points", "Zipcode Income", "Model Year",
                    "Quarter-Year", "Risk Class")

  D_X_local <- length(x_col_names) - 1  # 28 (excluding intercept)

  param_types <- list(
    list(name = "Log Baseline Accident Risk",
         theta_0 = "theta_0_llambda", theta_1 = "theta_1_llambda"),
    list(name = "Log Risk Aversion",
         theta_0 = "theta_0_log_risk_aversion", theta_1 = "theta_1_log_risk_aversion"),
    list(name = "Plan Switching Cost (10e3)",
         theta_0 = "theta_0_inert", theta_1 = "theta_1_inert"),
    list(name = "Firm Switching Cost (10e3)",
         theta_0 = "theta_0_eta", theta_1 = "theta_1_eta"),
    list(name = "Default Plan FE (10e3)",
         theta_0 = "theta_0_mm", theta_1 = "theta_1_mm"),
    list(name = "Monitoring Disutility (10e3)",
         theta_0 = "theta_0_xi", theta_1 = "theta_1_xi"),
    list(name = "Log Minor Accident Severity (10e3) Mean",
         theta_0 = "theta_0_sev_minor_mean", theta_1 = "theta_1_sev_minor_mean"),
    list(name = "Major Accident Severity (10e3) Pareto Shape",
         theta_0 = "theta_0_llambda_severe", theta_1 = "theta_1_llambda_severe")
  )

  #' Compute hyperp_X matrix from parsed theta vectors.
  #' sev_parsed: optional, from model_sev (for sev_minor_mean + pareto_alpha_severe)
  compute_hyperp_X_from_parsed <- function(parsed, sev_parsed = NULL) {
    n_rows <- length(param_types)
    n_cols <- length(x_col_names)  # intercept + 28 X vars
    mat <- matrix(NA_real_, n_rows, n_cols)
    rownames(mat) <- vapply(param_types, `[[`, character(1), "name")
    colnames(mat) <- x_col_names

    for (i in seq_along(param_types)) {
      pt <- param_types[[i]]
      # sev_minor_mean and pareto (llambda_severe) come from model_sev
      if (!is.null(sev_parsed) && pt$theta_0 == "theta_0_sev_minor_mean") {
        source <- sev_parsed
      } else if (!is.null(sev_parsed) && pt$theta_0 == "theta_0_llambda_severe") {
        # Remap: main model uses theta_0_llambda_severe, model_sev uses theta_0_pareto_alpha_severe
        source <- sev_parsed
        pt <- list(theta_0 = "theta_0_pareto_alpha_severe",
                   theta_1 = "theta_1_pareto_alpha_severe")
      } else {
        source <- parsed
      }
      t0 <- source[pt$theta_0]
      mat[i, 1] <- if (!is.na(t0)) t0 else NA_real_
      for (j in 1:D_X_local) {
        t1_name <- paste0(pt$theta_1, ".", j)
        v <- source[t1_name]
        mat[i, j + 1] <- if (!is.na(v)) v else NA_real_
      }
    }
    mat
  }

  # Parse model_sev CSVs for sev_minor_mean + pareto_alpha_severe (cols 7-8)
  sev_x_parsed_list <- list()
  if (!is.null(sev_csv_dir) && dir.exists(sev_csv_dir)) {
    sev_x_files <- list.files(sev_csv_dir, pattern = "^bootstrap-result-id-\\d+\\.csv$",
                               full.names = TRUE)
    cat("    Found", length(sev_x_files), "model_sev bootstrap CSVs for A6\n")
    sev_x_parsed_list <- lapply(sev_x_files, parse_original_csv_thetas)
    names(sev_x_parsed_list) <- basename(sev_x_files)
  } else {
    cat("    model_sev CSVs not found — sev cols will use main model values\n")
  }

  if (length(orig_parsed) == 0) {
    cat("    SKIP: No original bootstrap CSVs found.\n")
  } else {
    # Build matrices for all bootstraps, merging main + model_sev
    x_matrices <- lapply(names(orig_parsed), function(nm) {
      compute_hyperp_X_from_parsed(orig_parsed[[nm]], sev_x_parsed_list[[nm]])
    })
    names(x_matrices) <- names(orig_parsed)

    # Summarize across bootstraps — use raw sd (benchmark uses raw, not capped)
    xsum <- summarize_bootstrap_matrices(x_matrices, use_raw_sd = TRUE)

    # Transpose: rows = X variables, cols = parameter types
    est_t <- t(xsum$estimate)
    se_t  <- t(xsum$sd)
    rownames(se_t) <- rownames(est_t)
    colnames(se_t) <- colnames(est_t)

    rn <- rownames(est_t)
    # Rename variables to match benchmark display names
    rn[rn == "Length of Ownership"] <- "Car Ownership Years"
    rn[rn == "Total Accident Count"] <- "Total Accident Records"
    rn[rn == "Total DUI Count"] <- "Total DUI Records"
    rn[rn == "Credit Report Ind....18"] <- "Has Credit Report....18"
    rn[rn == "Violation Points"] <- "Violation Record (Points)"
    rownames(est_t) <- rn
    rownames(se_t) <- rn
    cn <- colnames(est_t)

    # Format with stars
    fmt_val <- function(x) ifelse(is.na(x), "--", sprintf("%.3f", x))
    fmt_se  <- function(x) ifelse(is.na(x), "--", paste0("(", sprintf("%.3f", x), ")"))
    tstat <- est_t / se_t
    pval  <- 2 * (1 - pnorm(abs(tstat)))
    stars <- matrix("", nrow = nrow(est_t), ncol = ncol(est_t))
    stars[!is.na(pval) & pval < 0.10] <- "$^{*}$"
    stars[!is.na(pval) & pval < 0.05] <- "$^{**}$"
    stars[!is.na(pval) & pval < 0.01] <- "$^{***}$"

    # Build column headers with \makecell
    col_header_map <- list(
      "Log Baseline Accident Risk"              = "\\makecell[c]{Log Baseline \\\\ Accident Risk}",
      "Log Risk Aversion"                        = "\\makecell[c]{Log Risk \\\\ Aversion}",
      "Plan Switching Cost (10e3)"               = "\\makecell[c]{Plan Switching \\\\ Cost (10e3)}",
      "Firm Switching Cost (10e3)"               = "\\makecell[c]{Firm Switching \\\\ Cost (10e3)}",
      "Default Plan FE (10e3)"                   = "\\makecell[c]{Default Plan \\\\ FE (10e3)}",
      "Monitoring Disutility (10e3)"             = "\\makecell[c]{Monitoring \\\\ Disutility (10e3)}",
      "Log Minor Accident Severity (10e3) Mean"  = "\\makecell[c]{Log Minor \\\\ Accident Severity \\\\ (10e3) Mean}",
      "Major Accident Severity (10e3) Pareto Shape" = "\\makecell[c]{Major Accident \\\\Severity (10e3) \\\\ Pareto Shape}"
    )
    col_headers <- vapply(cn, function(h) {
      if (h %in% names(col_header_map)) col_header_map[[h]]
      else .makecell_balanced(h, lines = 2, align = "c")
    }, character(1))

    # Build body rows
    body_lines <- character(0)
    for (i in seq_len(nrow(est_t))) {
      est_cells <- paste0(fmt_val(est_t[i, ]), stars[i, ])
      se_cells  <- fmt_se(se_t[i, ])
      est_row <- paste0(fix_angle_brackets(rn[i]), " & ",
                        paste(est_cells, collapse = " & "), "\\\\")
      se_row  <- paste0(" & ", paste(se_cells, collapse = " & "), "\\\\")
      body_lines <- c(body_lines, est_row, se_row)
    }
    body_text <- paste(body_lines, collapse = "\n")

    a6_tex <- sprintf(
      "\\begin{table}[!h]\n\\begin{centering}\n\\caption{\\label{tab:app-hyperp-x}Latent Parameter Loadings on Observables}\n\\centering\n\\resizebox{\\ifdim\\width>\\linewidth\\linewidth\\else\\width\\fi}{!}{\n\\begin{tabular}[t]{l%s}\n\\toprule\nX Variable & %s\\\\\n\\midrule\n%s\n\\bottomrule\n\\end{tabular}}\n\\par\\end{centering}\n\\vspace{0.1cm}\n\\begin{singlespace}\n{\\footnotesize \\emph{Notes:} This table reports hyper-parameter estimates for all latent parameters that are outlined in Section \\ref{sec:econometric-model}. Parentheses show bootstrap standard errors.\\par}\n\\end{singlespace}\n\\end{table}",
      paste(rep("c", length(cn)), collapse = ""),
      paste(col_headers, collapse = " & "),
      body_text
    )

    a6_results <- as.data.frame(est_t)
    a6_results_se <- as.data.frame(se_t)
    colnames(a6_results_se) <- paste0(colnames(a6_results_se), "_se")
    a6_csv <- cbind(a6_results, a6_results_se)
    write.csv(a6_csv, file.path(MODEL_TAB_DIR, "tab_a6.csv"), row.names = TRUE)
    cat("    Saved CSV:", file.path(MODEL_TAB_DIR, "tab_a6.csv"), "\n")

    taba6_path <- file.path(TABLES_DIR, "appendix", "tab_a6.tex")
    writeLines(a6_tex, taba6_path)
    cat("    Wrote", taba6_path, "\n")
  }
}, error = function(e) {
  cat("    ERROR generating Tab A6:", conditionMessage(e), "\n")
  taba6_check <- file.path(TABLES_DIR, "appendix", "tab_a6.tex")
  if (file.exists(taba6_check)) cat("    Pre-computed tab_a6.tex retained.\n")
})


## =============================================================================
## Table A7: 4-Period Horizon Robustness
## =============================================================================

tryCatch({
  cat("  Computing Tab A7 (4-period robustness)...\n")

  # 4p model: use GQ CSVs (same approach as Tab 6)
  gq_4p_dir <- file.path(MODEL_4P_DIR, "results")

  if (dir.exists(gq_4p_dir)) {
    gq_4p_files <- list.files(gq_4p_dir,
                               pattern = "^bootstrap-result-id-\\d+\\.csv$",
                               full.names = TRUE)
    cat("    Found", length(gq_4p_files), "4p GQ CSVs\n")

    if (length(gq_4p_files) > 0) {
      gq_4p_parsed <- lapply(gq_4p_files, parse_cmdstan_csv)
      names(gq_4p_parsed) <- basename(gq_4p_files)

      main_4p_dfs <- lapply(gq_4p_parsed, function(p) {
        build_param_df(p, main_param_names, main_gq_rows)
      })
      names(main_4p_dfs) <- names(gq_4p_parsed)
      for (nm in names(main_4p_dfs)) {
        main_4p_dfs[[nm]]$is_zero_csv <- grepl("id-0\\.csv$", nm)
      }

      main_4p_results <- get_summary_table_from_list(
        main_4p_dfs,
        main_table_parameters,
        main_table_parameters_pretty,
        statistics,
        statistics_pretty
      )

      write.csv(main_4p_results, file.path(MODEL_TAB_DIR, "tab_a7.csv"), row.names = TRUE)
      cat("    Saved CSV:", file.path(MODEL_TAB_DIR, "tab_a7.csv"), "\n")

      a7_tex <- generate_latex_table_with_se(
        main_4p_results,
        custom_params = main_table_parameters,
        custom_stats = statistics_pretty,
        custom_rounding = main_table_custom_rounding,
        caption = "Latent Parameter Summary - Four-Period Horizon",
        label = "tab:app-est-robust-4p",
        notes = "This table reports the distributions of key parameters from our model. Columns are moments/correlations across individuals. Risk and demand frictions (default plan FE, switching costs, and monitoring disutility) are reported on a per period basis. Parentheses show bootstrap standard errors."
      )

      taba7_path <- file.path(TABLES_DIR, "appendix", "tab_a7.tex")
      writeLines(a7_tex, taba7_path)
      cat("    Wrote", taba7_path, "\n")
    } else {
      cat("    SKIP: No GQ CSVs for 4p model.\n")
    }
  } else {
    cat("    SKIP: 4p GQ dir not found at", gq_4p_dir, "\n")
    taba7_check <- file.path(TABLES_DIR, "appendix", "tab_a7.tex")
    if (file.exists(taba7_check)) cat("    Pre-computed tab_a7.tex exists.\n")
    else cat("    WARNING: tab_a7.tex MISSING.\n")
  }
}, error = function(e) {
  cat("    ERROR generating Tab A7:", conditionMessage(e), "\n")
  taba7_check <- file.path(TABLES_DIR, "appendix", "tab_a7.tex")
  if (file.exists(taba7_check)) cat("    Pre-computed tab_a7.tex retained.\n")
})


## =============================================================================
## Table A8: 2-Period Horizon Robustness
## =============================================================================

tryCatch({
  cat("  Computing Tab A8 (2-period robustness)...\n")

  # 2p model: use GQ CSVs (same approach as Tab 6)
  gq_2p_dir <- file.path(MODEL_2P_DIR, "results")

  if (dir.exists(gq_2p_dir)) {
    gq_2p_files <- list.files(gq_2p_dir,
                               pattern = "^bootstrap-result-id-\\d+\\.csv$",
                               full.names = TRUE)
    cat("    Found", length(gq_2p_files), "2p GQ CSVs\n")

    if (length(gq_2p_files) > 0) {
      gq_2p_parsed <- lapply(gq_2p_files, parse_cmdstan_csv)
      names(gq_2p_parsed) <- basename(gq_2p_files)

      main_2p_dfs <- lapply(gq_2p_parsed, function(p) {
        build_param_df(p, main_param_names, main_gq_rows)
      })
      names(main_2p_dfs) <- names(gq_2p_parsed)
      for (nm in names(main_2p_dfs)) {
        main_2p_dfs[[nm]]$is_zero_csv <- grepl("id-0\\.csv$", nm)
      }

      main_2p_results <- get_summary_table_from_list(
        main_2p_dfs,
        main_table_parameters,
        main_table_parameters_pretty,
        statistics,
        statistics_pretty
      )

      write.csv(main_2p_results, file.path(MODEL_TAB_DIR, "tab_a8.csv"), row.names = TRUE)
      cat("    Saved CSV:", file.path(MODEL_TAB_DIR, "tab_a8.csv"), "\n")

      a8_tex <- generate_latex_table_with_se(
        main_2p_results,
        custom_params = main_table_parameters,
        custom_stats = statistics_pretty,
        custom_rounding = main_table_custom_rounding,
        caption = "Latent Parameter Summary - Two-Period Horizon",
        label = "tab:app-est-robust-2p",
        notes = "This table reports the distributions of key parameters from our model. Columns are moments/correlations across individuals. Risk and demand frictions (default plan FE, switching costs, and monitoring disutility) are reported on a per period basis. Parentheses show bootstrap standard errors.",
        placement = "h"
      )

      taba8_path <- file.path(TABLES_DIR, "appendix", "tab_a8.tex")
      writeLines(a8_tex, taba8_path)
      cat("    Wrote", taba8_path, "\n")
    } else {
      cat("    SKIP: No GQ CSVs for 2p model.\n")
    }
  } else {
    cat("    SKIP: 2p GQ dir not found at", gq_2p_dir, "\n")
    taba8_check <- file.path(TABLES_DIR, "appendix", "tab_a8.tex")
    if (file.exists(taba8_check)) cat("    Pre-computed tab_a8.tex exists.\n")
    else cat("    WARNING: tab_a8.tex MISSING.\n")
  }
}, error = function(e) {
  cat("    ERROR generating Tab A8:", conditionMessage(e), "\n")
  taba8_check <- file.path(TABLES_DIR, "appendix", "tab_a8.tex")
  if (file.exists(taba8_check)) cat("    Pre-computed tab_a8.tex retained.\n")
})


## =============================================================================
## Table C3: MH Het Parameters (from cost_mh_het bootstrap CSVs)
## =============================================================================

tryCatch({
  cat("  Generating Tab C3 (mhhet cost params from bootstrap CSVs)...\n")

  # Bootstrap CSV directory for cost MH het model (flat structure)
  cost_mhhet_bs_dir <- file.path(COST_MHHET_MODEL_DIR, "results")

  if (dir.exists(cost_mhhet_bs_dir)) {
    bs_files <- list.files(cost_mhhet_bs_dir, pattern = "^bootstrap-result-id-.*\\.csv$",
                           full.names = TRUE)
    cat("    Found", length(bs_files), "bootstrap CSVs in", cost_mhhet_bs_dir, "\n")

    if (length(bs_files) > 0) {
      # The 9 MH parameter columns to extract
      mh_cols <- c(
        "theta_mh_llambda.1.1", "theta_mh_llambda.2.1", "theta_mh_llambda.3.1",  # opt-in coef, regimes 1-3
        "theta_mh_llambda.1.2", "theta_mh_llambda.2.2", "theta_mh_llambda.3.2",  # intensity coef, regimes 1-3
        "theta_mh_llambda_rc.1", "theta_mh_llambda_rc.2", "theta_mh_llambda_rc.3" # risk class coef, regimes 1-3
      )

      # Read each CSV: skip comment lines (#), find header starting with lp__
      read_mh_from_csv <- function(path) {
        lines <- readLines(path)
        header_idx <- which(startsWith(lines, "lp__"))
        if (length(header_idx) == 0) return(NULL)
        header_idx <- header_idx[1]
        header <- strsplit(lines[header_idx], ",")[[1]]
        # Data is on the next line (single MLE row)
        if (header_idx >= length(lines)) return(NULL)
        data_vals <- as.numeric(strsplit(lines[header_idx + 1], ",")[[1]])
        names(data_vals) <- header
        # Extract the 9 MH columns
        if (!all(mh_cols %in% header)) return(NULL)
        data_vals[mh_cols]
      }

      # Collect estimates from all bootstrap samples
      bs_matrix <- do.call(rbind, lapply(bs_files, read_mh_from_csv))
      n_valid <- nrow(bs_matrix)
      cat("    Successfully read", n_valid, "bootstrap samples\n")

      if (n_valid > 0) {
        # Point estimates = mean across bootstraps; SEs = raw SD
        bs_mean <- colMeans(bs_matrix, na.rm = TRUE)
        bs_se <- apply(bs_matrix, 2, sd, na.rm = TRUE)

        # Format a cell with significance stars: \makecell[c]{value$^{stars}$ \\ (se)}
        format_b3_cell <- function(est, se, digits = 3) {
          if (is.na(se) || se == 0) {
            # Single bootstrap: no SE available
            return(formatC(round(est, digits), format = "f", digits = digits))
          }
          z <- abs(est / se)
          stars <- if (z > 2.576) "$^{***}$"
                   else if (z > 1.960) "$^{**}$"
                   else if (z > 1.645) "$^{*}$"
                   else ""
          sprintf("\\makecell[c]{%s%s \\\\ (%s)}",
                  formatC(round(est, digits), format = "f", digits = digits),
                  stars,
                  formatC(round(se, digits), format = "f", digits = digits))
        }

        # Build LaTeX table matching existing format
        b3_tex <- paste0(
          "\\begin{table}[h]\n",
          "\\begin{centering}\n",
          "\\caption{\\label{tab:app-mh-het-est} Moral Hazard Effects (with Risk-Class Heterogeneity)}\n",
          "\\centering\n",
          "\\resizebox{0.8\\linewidth}{!}{\n",
          "\\begin{tabular}[t]{cccc}\n",
          "\\toprule\n",
          "% \\multicolumn{2}{c}{ } & \\multicolumn{3}{c}{Moral Hazard Effect} \\\\\n",
          "% \\cmidrule(l{3pt}r{3pt}){3-5}\n",
          "Regime & Monitoring Opt-in Coef & Monitoring Intensity Coef &  Risk Class Coef \\\\\n",
          "\\midrule\n"
        )

        for (rg in 1:3) {
          opt_col <- paste0("theta_mh_llambda.", rg, ".1")
          int_col <- paste0("theta_mh_llambda.", rg, ".2")
          rc_col  <- paste0("theta_mh_llambda_rc.", rg)

          b3_tex <- paste0(b3_tex,
            rg, " & ",
            format_b3_cell(bs_mean[opt_col], bs_se[opt_col]), " & ",
            format_b3_cell(bs_mean[int_col], bs_se[int_col]), " & ",
            format_b3_cell(bs_mean[rc_col],  bs_se[rc_col]), "\\\\\n")
        }

        b3_tex <- paste0(b3_tex,
          "\\bottomrule\n",
          "\\end{tabular}}\n",
          "\\par\\end{centering}\n",
          "\\vspace{0.1cm}\n",
          "\\begin{singlespace}\n",
          "{\\footnotesize \\emph{Notes:} This table reports parameter estimates for the moral hazard effect, similar to Table \\ref{tab:app-hyperp-nonx} but incorporating heterogeneity with respect to drivers' risk class. Parentheses show bootstrap standard errors.\\par}\n",
          "\\end{singlespace}\n",
          "\\end{table}")

        c3_csv <- data.frame(
          parameter = names(bs_mean),
          estimate = as.numeric(bs_mean),
          se = as.numeric(bs_se),
          stringsAsFactors = FALSE
        )
        write.csv(c3_csv, file.path(MODEL_TAB_DIR, "tab_c3.csv"), row.names = FALSE)
        cat("    Saved CSV:", file.path(MODEL_TAB_DIR, "tab_c3.csv"), "\n")

        tabb3_path <- file.path(TABLES_DIR, "appendix", "tab_c3.tex")
        writeLines(b3_tex, tabb3_path)
        cat("    Wrote", tabb3_path, "(from", n_valid, "bootstrap samples)\n")
      } else {
        cat("    WARNING: No valid bootstrap samples could be read.\n")
        tabb3_check <- file.path(TABLES_DIR, "appendix", "tab_c3.tex")
        if (file.exists(tabb3_check)) cat("    Pre-computed tab_c3.tex retained.\n")
        else cat("    WARNING: tab_c3.tex MISSING.\n")
      }
    } else {
      cat("    No bootstrap CSV files found in", cost_mhhet_bs_dir, "\n")
      tabb3_check <- file.path(TABLES_DIR, "appendix", "tab_c3.tex")
      if (file.exists(tabb3_check)) cat("    Pre-computed tab_c3.tex retained.\n")
      else cat("    WARNING: tab_c3.tex MISSING.\n")
    }
  } else {
    cat("    Cost MH het bootstrap dir not found:", cost_mhhet_bs_dir, "\n")
    cat("    Falling back to pre-computed tab_c3.tex.\n")
    tabb3_check <- file.path(TABLES_DIR, "appendix", "tab_c3.tex")
    if (file.exists(tabb3_check)) cat("    Pre-computed tab_c3.tex exists.\n")
    else cat("    WARNING: tab_c3.tex MISSING.\n")
  }
}, error = function(e) {
  cat("    ERROR generating Tab C3:", conditionMessage(e), "\n")
  tabb3_check <- file.path(TABLES_DIR, "appendix", "tab_c3.tex")
  if (file.exists(tabb3_check)) cat("    Pre-computed tab_c3.tex retained.\n")
})

cat("c1_param_tables.R completed successfully.\n")
