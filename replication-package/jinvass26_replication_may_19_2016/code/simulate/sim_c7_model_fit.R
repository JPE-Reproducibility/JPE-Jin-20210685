################################################################################
## sim_model_fit.R — Extract model fit CSVs from estimated model_main output
##
## Requires: CmdStan, model_main estimation output in MODEL_OUT_DIR
## Produces:
##   MODEL_FIT_DIR/tab_4.csv   (risk, score, pricing moments)
##   MODEL_FIT_DIR/tab_5.csv   (demand/choice shares)
##   MODEL_FIT_DIR/fig_6a.csv  (pricing by lambda, regime 3)
##   MODEL_FIT_DIR/fig_6b.csv  (lambda density by TM status)
##
## Sourced by: code/run_simulated.R (after sim_estimate.R)
################################################################################

if (!exists("MODEL_FIT_DIR")) { USE_SIMULATED_DATA <- TRUE; source("code/config.R") }
source("code/simulate/functions/sim_helpers.R")

source("code/functions/helper.R")

## ---- Fit table formatters (moved from c1_get_rf_exhibits.R) -----------------
## These format model-fit .txt output into publication-ready LaTeX tables.

## ---- Helper: format cost/risk/score tabular to match benchmark style --------
format_cost_tabular <- function(tab_lines) {
  tab_lines <- tab_lines[!grepl("^\\s*%", tab_lines)]  # strip comments

  # Remove nested xtable \begin{tabular} + its \toprule/\midrule
  begin_idx <- grep("\\\\begin\\{tabular\\}", tab_lines)
  end_idx <- grep("\\\\end\\{tabular\\}", tab_lines)

  if (length(begin_idx) >= 2) {
    inner_begin <- begin_idx[2]
    remove_lines <- inner_begin
    for (j in (inner_begin + 1):min(inner_begin + 3, length(tab_lines))) {
      if (grepl("^\\s*\\\\(toprule|midrule)\\s*$", tab_lines[j])) {
        remove_lines <- c(remove_lines, j)
      } else {
        break
      }
    }
    tab_lines <- tab_lines[-remove_lines]
  }

  end_idx <- grep("\\\\end\\{tabular\\}", tab_lines)
  if (length(end_idx) >= 2) {
    tab_lines <- tab_lines[-end_idx[1]]
  }

  bottom_idx <- grep("\\\\bottomrule", tab_lines)
  if (length(bottom_idx) >= 2) {
    tab_lines <- tab_lines[-bottom_idx[1]]
  }

  # Reformat column spec and headers
  tab_lines <- gsub("\\\\begin\\{tabular\\}\\{p\\{5\\.0cm\\}rrp\\{5\\.0cm\\}rr\\}",
                     "\\\\begin{tabular}{\n    @{} >{\\\\raggedright\\\\arraybackslash}p{4.2cm} rr\n        >{\\\\raggedright\\\\arraybackslash}p{5.2cm} rr @{}\n}",
                     tab_lines)
  tab_lines <- gsub("\\\\multicolumn\\{2\\}\\{c\\}\\{Risk \\\\& Score\\}",
                     "\\\\multicolumn{3}{c}{\\\\textbf{Risk \\\\& Score}}", tab_lines)
  tab_lines <- gsub("\\\\multicolumn\\{3\\}\\{c\\}\\{Pricing\\}",
                     "\\\\multicolumn{3}{c}{\\\\textbf{Pricing}}", tab_lines)
  tab_lines <- gsub("\\\\cmidrule\\(lr\\)\\{2-3\\}", "\\\\cmidrule(lr){1-3}", tab_lines)
  tab_lines <- gsub("^\\s*& (\\\\multicolumn)", "    \\1", tab_lines)
  tab_lines <- gsub("\\bPred\\b", "Predicted", tab_lines)
  tab_lines <- gsub("\\\\addlinespace\\[1\\.5ex\\]", "\\\\addlinespace[1ex]", tab_lines)

  # Remove duplicate midrules
  is_midrule <- grepl("^\\s*\\\\midrule\\s*$", tab_lines)
  dup_midrule <- is_midrule & c(FALSE, head(is_midrule, -1))
  tab_lines <- tab_lines[!dup_midrule]

  tab_lines <- gsub("^\\s*\\\\\\\\\\s*$", "    \\\\addlinespace[1ex]", tab_lines)

  # Normalize indentation
  for (i in seq_along(tab_lines)) {
    line <- tab_lines[i]
    if (grepl("^\\s*\\\\begin\\{tabular\\}", line) ||
        grepl("^\\s*\\\\end\\{tabular\\}", line) ||
        grepl("^\\s*@\\{\\}", line) ||
        grepl("^\\s*>\\{", line) ||
        grepl("^\\s*\\}", line) ||
        grepl("^\\s*$", line)) next
    tab_lines[i] <- paste0("    ", trimws(line))
  }
  tab_lines
}

## ---- Helper: format N row values -------------------------------------------
format_n_row <- function(values_str) {
  clean <- sub("\\\\\\\\\\s*$", "", values_str)
  parts <- strsplit(clean, "&")[[1]]
  for (i in seq_along(parts)) {
    val <- trimws(parts[i])
    if (grepl("^[0-9]+\\.00$", val)) {
      num <- as.integer(sub("\\.00$", "", val))
      if (num == 0) {
        parts[i] <- ""
      } else {
        parts[i] <- paste0(" ", format(num, big.mark = ",", scientific = FALSE))
      }
    } else if (val %in% c("0", "0.0")) {
      parts[i] <- ""
    }
  }
  paste0(paste(parts, collapse = " &"), " \\\\")
}

## ---- Helper: format demand/choice tabular to match benchmark style ----------
format_demand_tabular <- function(tabular_str) {
  lines <- strsplit(tabular_str, "\n")[[1]]
  skip_re <- paste(c(
    "^\\s*\\\\begin\\{tabular\\}", "^\\s*\\\\end\\{tabular\\}",
    "^\\s*\\\\toprule", "^\\s*\\\\midrule", "^\\s*\\\\bottomrule",
    "^\\s*\\\\cmidrule", "^\\s*%", "^\\s*$",
    "Block [0-9]", "Type & Label", "^\\s*\\\\multicolumn\\{2\\}\\{c\\}"
  ), collapse = "|")
  data_lines <- trimws(lines[!grepl(skip_re, lines)])
  data_lines <- data_lines[nchar(data_lines) > 0]

  cov_map <- c("40" = "40,000", "50" = "50,000", "100" = "100,000",
               "150" = "150,000", "300" = "300,000",
               "20K" = "40,000", "25K" = "50,000", "50K" = "100,000",
               "75K" = "150,000", "150K" = "300,000")

  n_cells_first <- length(strsplit(data_lines[1], "&")[[1]])
  n_blocks <- (n_cells_first - 2) / 2

  if (n_blocks >= 6) {
    total_cols <- 14
    hdr <- c(
      "\\begin{tabular}{@{} ll *{2}{rr} ZZ *{3}{rr} @{}}",
      "    \\toprule",
      "        \\multicolumn{2}{c}{}",
      "      & \\multicolumn{8}{c}{New customers}",
      "      & \\multicolumn{4}{c}{Renewal customers}",
      "      \\\\",
      "      \\cmidrule(lr){3-10} \\cmidrule(lr){11-14}",
      "      \\multicolumn{2}{c}{}",
      "      & \\multicolumn{2}{c}{Pre-Mtr}",
      "      & \\multicolumn{6}{c}{Post-Mtr}",
      "      \\\\",
      "      \\cmidrule(lr){3-4} \\cmidrule(lr){5-10}",
      "      \\multicolumn{2}{c}{}",
      "      & \\multicolumn{4}{c}{Pre-MM Chg}",
      "      & \\multicolumn{2}{c}{}",
      "      & \\multicolumn{2}{c}{Post-MM Chg}",
      "      & \\multicolumn{2}{c}{Pre-MM Chg}",
      "      & \\multicolumn{2}{c}{Post-MM Chg}",
      "      \\\\",
      "      \\cmidrule(lr){3-6} \\cmidrule(lr){7-10} \\cmidrule(lr){11-12} \\cmidrule(lr){13-14}",
      "    \\multicolumn{2}{c}{} & Data & Pred & Data & Pred & Data & Pred & Data & Pred & Data & Pred & Data & Pred \\\\",
      "    \\midrule"
    )
  } else {
    total_cols <- 12
    hdr <- c(
      "\\begin{tabular}{@{} ll *{5}{rr} @{}}",
      "    \\toprule",
      "        \\multicolumn{2}{c}{}",
      "      & \\multicolumn{6}{c}{New customers}",
      "      & \\multicolumn{4}{c}{Renewal customers}",
      "      \\\\",
      "      \\cmidrule(lr){3-8} \\cmidrule(lr){9-12}",
      "      \\multicolumn{2}{c}{}",
      "      & \\multicolumn{2}{c}{Pre-Mtr}",
      "      & \\multicolumn{4}{c}{Post-Mtr}",
      "      \\\\",
      "      \\cmidrule(lr){3-4} \\cmidrule(lr){5-8}",
      "      \\multicolumn{2}{c}{}",
      "      & \\multicolumn{2}{c}{}",
      "      & \\multicolumn{2}{c}{Pre-MM Chg}",
      "      & \\multicolumn{2}{c}{Post-MM Chg}",
      "      & \\multicolumn{2}{c}{Pre-MM Chg}",
      "      & \\multicolumn{2}{c}{Post-MM Chg}",
      "      \\\\",
      "      \\cmidrule(lr){5-6} \\cmidrule(lr){7-8} \\cmidrule(lr){9-10} \\cmidrule(lr){11-12}",
      "    \\multicolumn{2}{c}{} & Data & Pred & Data & Pred & Data & Pred & Data & Pred & Data & Pred \\\\",
      "    \\midrule"
    )
  }

  body <- character()
  prev_group <- ""

  for (dl in data_lines) {
    cells <- strsplit(dl, "&")[[1]]
    if (length(cells) < 3) next
    type <- trimws(cells[1])
    label <- trimws(cells[2])
    rest <- paste(cells[3:length(cells)], collapse = "&")

    mc_l <- function(txt) paste0("\\multicolumn{", total_cols, "}{l}{\\textit{", txt, "}} \\\\")

    if (type == "Coverage Share") {
      if (prev_group != "cov_share") {
        body <- c(body, mc_l("Coverage Share"))
        prev_group <- "cov_share"
      }
      lbl <- if (label %in% names(cov_map)) cov_map[label] else label
      body <- c(body, paste0("  & ", lbl, " &", rest))

    } else if (grepl("TM Share", type)) {
      if (prev_group != "monitoring") {
        body <- c(body, mc_l("Monitoring Opt-in"))
        prev_group <- "monitoring"
      }
      body <- c(body, paste0("  & Share &", rest))

    } else if (grepl("TM Selection", type)) {
      body <- c(body, paste0("  & Selection &", rest))

    } else if (grepl("Selection", type)) {
      if (prev_group != "cov_sel") {
        body <- c(body, mc_l("Coverage Selection"))
        prev_group <- "cov_sel"
      }
      lbl <- if (label %in% names(cov_map)) cov_map[label] else label
      body <- c(body, paste0("  & ", lbl, " &", rest))

    } else if (grepl("Attrition", type)) {
      body <- c(body, mc_l("Attrition"))
      prev_group <- "attrition"
      body <- c(body, paste0("  & Share &", rest))

    } else if (grepl("\\$N\\$|^N$", type)) {
      body <- c(body, "\\addlinespace[1ex]")
      n_fmt <- format_n_row(rest)
      body <- c(body, paste0(" & $N$ &", n_fmt))
    }
  }

  paste(c(hdr, body, "    \\bottomrule", "\\end{tabular}"), collapse = "\n")
}

## ---- build_fit_tex_from_txt: Parse .txt -> two .tex table strings -----------
##
## Reads a fit_tbl_*.txt file (produced by get_fit_table.R), splits it into
## two blocks (risk/score/pricing and demand/choice), applies formatting,
## and wraps each in a \begin{table}...\end{table} environment.
##
## Returns a list with elements $tab4 and $tab5 (character strings).
build_fit_tex_from_txt <- function(txt_path,
                                    tab4_placement = "[htbp!]",
                                    tab4_caption = "Fit of Claim Risk, Monitoring Score, and Renewal Pricing",
                                    tab4_label = "tab:fit_c",
                                    tab4_notes = paste0(
                                      "This table reports the fit of model predictions to key data moments. ",
                                      "Monitoring scores are only available for eligible new customers that have completed monitoring. ",
                                      "For customers that have left the firm at a renewal, we do not observe claim realization for that period. ",
                                      "For customers that have left the firm during a period, we do not observe the renewal pricing factor ",
                                      "for the following period."
                                    ),
                                    tab5_placement = "[htbp!]",
                                    tab5_caption = "Fit of Choice Shares (Coverage, Monitoring, and Attrition) and Selection",
                                    tab5_label = "tab:fit_d",
                                    tab5_notes = paste0(
                                      "This table reports the fit of model predictions to key data moments. ",
                                      "All quantities except the number of observations are reported in percentage point units. ",
                                      "Coverage selection patterns are reported as the total claim cost of each coverage benchmarked against ",
                                      "(divided by) that of the mandatory minimum plan. ",
                                      "Monitoring selection patterns are reported as the total claim cost of opt-in customers divided by ",
                                      "that of the customers who did not opt-in. ",
                                      "We do not observe claim realization for renewal customers that left the firm. ",
                                      "``Pre-Mtr'' and ``Post-Mtr'' separate new customers to the firm before and after the introduction of monitoring. ",
                                      "``Pre-MM Chg'' and ``Post-MM Chg'' separate customers before and after the State raised mandatory ",
                                      "minimum coverage from \\$40,000 to \\$50,000."
                                    )) {

  fit_tbl_raw <- readLines(txt_path)

  # Split into two blocks at \footnotesize delimiter
  footnotesize_line <- grep("footnotesize", fit_tbl_raw, fixed = TRUE)
  if (length(footnotesize_line) == 0) {
    stop("Could not split fit_tbl into two blocks (no \\footnotesize found) in: ", txt_path)
  }

  block1_lines <- fit_tbl_raw[1:(footnotesize_line[1] - 1)]
  block2_lines <- fit_tbl_raw[footnotesize_line[1]:length(fit_tbl_raw)]
  block1_lines <- block1_lines[nchar(block1_lines) > 0]
  block2_lines <- block2_lines[nchar(block2_lines) > 0]

  # Extract tabular environments
  b1_ts <- grep("\\\\begin\\{tabular\\}", block1_lines)
  b1_te <- grep("\\\\end\\{tabular\\}", block1_lines)
  b2_ts <- grep("\\\\begin\\{tabular\\}", block2_lines)
  b2_te <- grep("\\\\end\\{tabular\\}", block2_lines)

  if (length(b1_ts) == 0 || length(b1_te) == 0 || length(b2_ts) == 0 || length(b2_te) == 0) {
    stop("Could not locate tabular environments in: ", txt_path)
  }

  # Block 1 -> tab4 (risk/score/pricing)
  tabular1 <- paste(block1_lines[b1_ts[1]:b1_te[length(b1_te)]], collapse = "\n")
  tab1_lines <- strsplit(tabular1, "\n")[[1]]
  tab1_lines <- format_cost_tabular(tab1_lines)
  tabular1_fixed <- paste(tab1_lines, collapse = "\n")

  tab4_tex <- paste0(
    "\\begin{table}", tab4_placement, "\n",
    "\\begin{centering}\n",
    "\\caption{", tab4_caption, "}\n",
    "\\label{", tab4_label, "}\n",
    "\\resizebox{\\linewidth}{!}{%\n",
    tabular1_fixed, "\n",
    "}\n",
    "\\par\\end{centering}\n",
    "\\vspace{0.5em}\n",
    "\\begin{singlespace}\n",
    "  {\\footnotesize \\emph{Notes:} ", tab4_notes, " \\par}\n",
    "\\end{singlespace}\n",
    "\\end{table}"
  )

  # Block 2 -> tab5 (demand/choice)
  tabular2 <- paste(block2_lines[b2_ts[1]:b2_te[length(b2_te)]], collapse = "\n")
  tabular2_fixed <- format_demand_tabular(tabular2)

  tab5_tex <- paste0(
    "\\begin{table}", tab5_placement, "\n",
    "\\begin{centering}\n",
    "\\caption{", tab5_caption, "}\n",
    "\\label{", tab5_label, "}\n",
    "\\resizebox{\\linewidth}{!}{%\n",
    tabular2_fixed, "%\n",
    "}\n",
    "\\par\\end{centering}\n",
    "\\vspace{0.5em}\n",
    "\\begin{singlespace}\n",
    "  {\\footnotesize \\emph{Notes:} ", tab5_notes, " \\par}\n",
    "\\end{singlespace}\n",
    "\\end{table}"
  )

  list(tab4 = tab4_tex, tab5 = tab5_tex)
}

cat("sim_model_fit.R: Extracting model fit CSVs...\n")
dir.create(MODEL_FIT_DIR, recursive = TRUE, showWarnings = FALSE)

## ---- Set up environment for extraction pipeline -----------------------------

model_name <- "model_main"
model_config <- ""
bootstrap_id <- 0  # ID 0 = main equal-weight estimate (no bootstrap resampling)
estimation_type <- "opt"
use_mle <- TRUE
log_file_suffix <- "0"

## ---- Run extraction pipeline ------------------------------------------------
## extract_model_estimates_cmdstan.R loads bootstrap CSV, extracts all theta_*
## params, compiles Stan model with expose_functions, loads data_list,
## and computes per-individual latent params (llambda, lambda, etc.)
cat("  Loading model estimates and compiling Stan...\n")
source("code/simulate/functions/estimation/extract_model_estimates_cmdstan.R")

## get_fit_cp.R sources get_c_latentparams.R + get_cp_pred.R, then builds
## fig_6a/6b/b3/b4 CSVs directly in MODEL_FIT_DIR
cat("  Computing cost/pricing predictions and figure CSVs...\n")
source("code/simulate/functions/estimation/get_fit_cp.R")

## Fig B.3 illustration-only override (overwrites fig_b3.csv with a version
## generated at full score slope, score_scale = 1.0; see fig_b3_override.R
## header for full isolation guarantees and rationale).
source("code/simulate/fig_b3_override.R")

## get_d_latentparams.R computes demand-side latent params (risk_aversion,
## sigma_logit, psi_mm, xi, etc.) needed for choice probability calculation
cat("  Computing demand latent parameters...\n")
d_tm_mh_rational_ind <- ifelse(is.null(d_tm_mh_rational_ind), 1, d_tm_mh_rational_ind)
source("code/simulate/functions/estimation/get_d_latentparams.R")

## get_d_pred.R computes log_choice_probs for all 6 choice blocks
## Requires Stan expose_functions (out_model + ctf_model)
cat("  Computing demand choice probabilities...\n")
source("code/simulate/functions/estimation/get_d_pred.R")

## ---- Build tab_4.csv (Risk, Score, Pricing moments) -------------------------

cat("  Building tab_4.csv...\n")

# Risk moments (from claim-level data)
risk_m1_d <- get_samp_wgt_avg(data_list$clm_count_N_clm, 1/data_list$sampling_enum_clm)
risk_m1_p <- get_samp_wgt_avg(lambda_clm, 1/data_list$sampling_enum_clm)
risk_m2_d <- get_samp_wgt_avg(data_list$clm_count_N_clm^2, 1/data_list$sampling_enum_clm)
risk_m2_p <- get_samp_wgt_avg(lambda_clm + lambda_clm^2, 1/data_list$sampling_enum_clm)
risk_major_d <- get_samp_wgt_avg(data_list$clm_count_severe_N_clm, 1/data_list$sampling_enum_clm)
risk_major_p <- get_samp_wgt_avg(lambda_severe_clm, 1/data_list$sampling_enum_clm)
risk_N <- sum(data_list$sampling_enum_clm)

# Score moments (from TM records)
score_m1_d <- get_samp_wgt_avg(data_list$log_tm_score_R_ordered, 1/data_list$sampling_enum_tm_R)
score_m1_p <- get_samp_wgt_avg(log_score_est, 1/data_list$sampling_enum_tm_R)
score_m2_d <- get_samp_wgt_avg(data_list$log_tm_score_R_ordered^2, 1/data_list$sampling_enum_tm_R)
score_m2_p <- get_samp_wgt_avg(log_score_est^2, 1/data_list$sampling_enum_tm_R)
score_cov_d <- get_samp_wgt_avg(data_list$log_tm_score_R_ordered * clm_cnt_i[I_tm_R_ordered_to_N_clm],
                                 1/data_list$sampling_enum_tm_R)
score_cov_p <- get_samp_wgt_avg(log_score_est * lambda_i[I_tm_R_ordered_to_N_clm],
                                 1/data_list$sampling_enum_tm_R)
score_N <- sum(data_list$sampling_enum_tm_R)

# Pricing moments (first renewal = new business, latter = renewal)
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

tab_4 <- data.frame(
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
# Fix score section moment labels to match real-data format
tab_4$moment[tab_4$section == "Monitoring score" & tab_4$moment == "major claims"] <- "second moment"
tab_4$moment[tab_4$section == "Monitoring score" & tab_4$moment == "second moment" &
              tab_4$data_value == round(score_m2_d, 6)] <- "second moment"
# Reorder score rows: first moment, second moment, covariance with risk, N
score_mask <- tab_4$section == "Monitoring score"
tab_4$moment[score_mask] <- c("first moment", "second moment", "covariance with risk", "N")

write.csv(tab_4, file.path(MODEL_FIT_DIR, "tab_4.csv"), row.names = FALSE)
cat("    -> tab_4.csv\n")

## ---- Build tab_5.csv (Demand/Choice shares) ---------------------------------

cat("  Building tab_5.csv...\n")

cov_labels_list <- list(
  paste0(data_list$limits_1 * 2), paste0(data_list$limits_1 * 2),
  paste0(data_list$limits_2 * 2), paste0(data_list$limits_2 * 2),
  paste0(data_list$limits_1 * 2), paste0(data_list$limits_2 * 2)
)

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

# Pivot to wide format matching real-data tab_5.csv
demand_df <- do.call(rbind, demand_rows)

# Build wide with blockN_data, blockN_pred columns
tab_5_rows <- list()
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
    tab_5_rows[[length(tab_5_rows) + 1]] <- row
  }
}
tab_5 <- do.call(rbind, tab_5_rows)

write.csv(tab_5, file.path(MODEL_FIT_DIR, "tab_5.csv"), row.names = FALSE)
cat("    -> tab_5.csv\n")

cat("sim_model_fit.R: Done.\n")
cat("  Output:", MODEL_FIT_DIR, "\n")
