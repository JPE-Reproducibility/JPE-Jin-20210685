################################################################################
## c3_get_fitctf_exhibits.R — Generate model fit + CTF exhibits from CSVs
##
## Produces:
##   Tab 4, 5          (model fit moments and choice shares)
##   Tab C.2            (MH-het model fit)
##   Fig 6a, 6b        (risk rating and selection)
##   Fig B.3, B.4      (score and pricing by regime)
##   Tab 7              (main CTF results)
##   Tab A.9–A.15      (CTF robustness)
##   Tab C.4, C.5      (CTF MH-het and learning)
##
## Reads from: data/precomputed/model_fit/, data/precomputed/ctf/
## Writes to:  output/exhibits/, output/exhibits/appendix/
################################################################################

if (!exists("TABLES_DIR")) source("code/config.R")
source("code/functions/ctf_table_helpers.R")

n_generated <- 0
n_skipped <- 0

cat("c3_get_fitctf_exhibits.R: Generating model fit + CTF exhibits...\n")

################################################################################
## Part 1: Model fit tables (Tab 4, 5, C.2) from CSVs
################################################################################

## Helper: build model-fit tabular body (shared by Tab 4 and Tab C.2)
build_fit_tabular_body <- function(df) {
  sections <- list(
    list(left = "Poisson claim counts", right = "First renewal pricing factor"),
    list(left = "Monitoring score", right = "Latter renewal pricing factor")
  )

  body <- ""
  for (s_idx in seq_along(sections)) {
    sec <- sections[[s_idx]]
    left_rows <- df[df$panel == "left" & df$section == sec$left, ]
    right_rows <- df[df$panel == "right" & df$section == sec$right, ]

    body <- paste0(body,
      "    \\textit{", sec$left, "} & & & \\textit{", sec$right, "} & & \\\\\n")

    for (i in seq_len(nrow(left_rows))) {
      lr <- left_rows[i, ]
      rr <- right_rows[i, ]
      lv <- if (lr$moment == "N") formatC(lr$data_value, format = "d", big.mark = ",") else sprintf("%.3f", lr$data_value)
      lp <- if (lr$moment == "N") "" else sprintf("%.3f", lr$pred_value)
      rv <- if (rr$moment == "N") formatC(rr$data_value, format = "d", big.mark = ",") else sprintf("%.3f", rr$data_value)
      rp <- if (rr$moment == "N") "" else sprintf("%.3f", rr$pred_value)

      if (lr$moment == "N") {
        body <- paste0(body, "    \\addlinespace[1ex]\n")
      }
      lm_fmt <- if (lr$moment == "N") "$N$" else lr$moment
      rm_fmt <- if (rr$moment == "N") "$N$" else rr$moment
      body <- paste0(body,
        "    \\quad ", lm_fmt, " & ", lv, " & ", lp, " & ",
        "\\quad ", rm_fmt, " & ", rv, " & ", rp, " \\\\\n")
    }
    # Between sections: addlinespace; after last section: nothing extra
    if (s_idx < length(sections)) {
      body <- paste0(body, "    \\addlinespace[1ex]\n")
    }
  }
  body
}

tab_4_csv <- file.path(MODEL_FIT_DIR, "tab_4.csv")
if (file.exists(tab_4_csv)) {
  df4 <- read.csv(tab_4_csv, stringsAsFactors = FALSE)

  tab4_body <- build_fit_tabular_body(df4)

  tab4_tex <- paste0(
    "\\begin{table}[htbp!]\n",
    "\\begin{centering}\n",
    "\\caption{Fit of Claim Risk, Monitoring Score, and Renewal Pricing}\n",
    "\\label{tab:fit_c}\n",
    "\\resizebox{\\linewidth}{!}{%\n",
    "\\begin{tabular}{\n",
    "    @{} >{\\raggedright\\arraybackslash}p{4.2cm} rr\n",
    "        >{\\raggedright\\arraybackslash}p{5.2cm} rr @{}\n",
    "}\n",
    "    \\toprule\n",
    "    \\multicolumn{3}{c}{\\textbf{Risk \\& Score}} & \\multicolumn{3}{c}{\\textbf{Pricing}} \\\\\n",
    "    \\cmidrule(lr){1-3} \\cmidrule(lr){4-6}\n",
    "    Moment & Data & Predicted & Moment & Data & Predicted \\\\\n",
    "    \\midrule\n",
    tab4_body,
    "    \\bottomrule\n",
    "\\end{tabular}\n",
    "}\n",
    "\\par\\end{centering}\n",
    "\\vspace{0.5em}\n",
    "\\begin{singlespace}\n",
    "  {\\footnotesize \\emph{Notes:} This table reports the fit of model predictions to key data moments. ",
    "Monitoring scores are only available for eligible new customers that have completed monitoring. ",
    "For customers that have left the firm at a renewal, we do not observe claim realization for that period. ",
    "For customers that have left the firm during a period, we do not observe the renewal pricing factor for the following period. \\par}\n",
    "\\end{singlespace}\n",
    "\\end{table}"
  )

  writeLines(tab4_tex, file.path(TABLES_DIR, "tab_4.tex"))
  cat("  [GEN] tab_4.tex\n"); n_generated <- n_generated + 1
} else {
  cat("  [SKIP] tab_4.tex (no CSV)\n"); n_skipped <- n_skipped + 1
}

## Helper: build Tab 5-style choice-shares body from CSV data frame
build_tab5_body <- function(df5) {
  # Coverage label mapping: numeric labels -> formatted with commas
  coverage_labels <- c("40" = "40,000", "50" = "50,000", "100" = "100,000",
                        "150" = "150,000", "300" = "300,000")

  body <- ""
  current_section <- ""

  for (i in seq_len(nrow(df5))) {
    r <- df5[i, ]
    type_raw <- r$type

    # Determine section header
    section <- ""
    if (type_raw == "Coverage Share") section <- "Coverage Share"
    else if (type_raw == "Selection \\%" || type_raw == "Selection %") section <- "Coverage Selection"
    else if (type_raw == "TM Share") section <- "Monitoring Opt-in"
    else if (type_raw == "TM Selection \\%" || type_raw == "TM Selection %") section <- "Monitoring Opt-in"
    else if (type_raw == "Attrition Share") section <- "Attrition"
    else if (type_raw == "N") section <- ""

    # Emit section header if new section
    if (section != "" && section != current_section) {
      body <- paste0(body, "\\multicolumn{14}{l}{\\textit{", section, "}} \\\\\n")
      current_section <- section
    }

    # Format label column
    label_str <- as.character(r$label)
    if (type_raw %in% c("Coverage Share", "Selection \\%", "Selection %")) {
      # Use formatted coverage label
      label_str <- ifelse(label_str %in% names(coverage_labels),
                          coverage_labels[label_str], label_str)
    } else if (type_raw == "TM Share") {
      label_str <- "Share"
    } else if (type_raw == "TM Selection \\%" || type_raw == "TM Selection %") {
      label_str <- "Selection"
    } else if (type_raw == "Attrition Share") {
      label_str <- "Share"
    } else if (type_raw == "N") {
      label_str <- "$N$"
    }

    # Format values
    vals <- c(r$block1_data, r$block1_pred, r$block2_data, r$block2_pred,
              r$block3_data, r$block3_pred, r$block4_data, r$block4_pred,
              r$block5_data, r$block5_pred, r$block6_data, r$block6_pred)

    if (type_raw == "N") {
      # N row: data values as integers with commas, pred values as empty
      fmt_vals <- character(12)
      for (j in seq_along(vals)) {
        if (is.na(vals[j])) {
          fmt_vals[j] <- ""
        } else if (j %% 2 == 1) {
          # Data column: integer with commas
          fmt_vals[j] <- formatC(as.integer(vals[j]), format = "d", big.mark = ",")
        } else {
          # Pred column: empty
          fmt_vals[j] <- ""
        }
      }
    } else {
      fmt_vals <- sapply(vals, function(v) {
        if (is.na(v)) "" else sprintf("%.2f", v)
      })
    }

    # Add addlinespace before N row
    if (type_raw == "N") {
      body <- paste0(body, "\\addlinespace[1ex]\n")
    }

    body <- paste0(body, "  & ", label_str, " & ",
                   paste(fmt_vals, collapse = " & "), " \\\\\n")
  }
  body
}

tab_5_csv <- file.path(MODEL_FIT_DIR, "tab_5.csv")
if (file.exists(tab_5_csv)) {
  df5 <- read.csv(tab_5_csv, stringsAsFactors = FALSE)

  tab5_body <- build_tab5_body(df5)

  tab5_tex <- paste0(
    "\\begin{table}[htbp!]\n",
    "\\begin{centering}\n",
    "\\caption{Fit of Choice Shares (Coverage, Monitoring, and Attrition) and Selection}\n",
    "\\label{tab:fit_d}\n",
    "\\resizebox{\\linewidth}{!}{%\n",
    "\\begin{tabular}{@{} ll *{2}{rr} ZZ *{3}{rr} @{}}\n",
    "    \\toprule\n",
    "        \\multicolumn{2}{c}{}\n",
    "      & \\multicolumn{8}{c}{New customers}\n",
    "      & \\multicolumn{4}{c}{Renewal customers}\n",
    "      \\\\\n",
    "      \\cmidrule(lr){3-10} \\cmidrule(lr){11-14}\n",
    "      \\multicolumn{2}{c}{}\n",
    "      & \\multicolumn{2}{c}{Pre-Mtr}\n",
    "      & \\multicolumn{6}{c}{Post-Mtr}\n",
    "      \\\\\n",
    "      \\cmidrule(lr){3-4} \\cmidrule(lr){5-10}\n",
    "      \\multicolumn{2}{c}{}\n",
    "      & \\multicolumn{4}{c}{Pre-MM Chg}\n",
    "      & \\multicolumn{2}{c}{}\n",
    "      & \\multicolumn{2}{c}{Post-MM Chg}\n",
    "      & \\multicolumn{2}{c}{Pre-MM Chg}\n",
    "      & \\multicolumn{2}{c}{Post-MM Chg}\n",
    "      \\\\\n",
    "      \\cmidrule(lr){3-6} \\cmidrule(lr){7-10} \\cmidrule(lr){11-12} \\cmidrule(lr){13-14}\n",
    "    \\multicolumn{2}{c}{} & Data & Pred & Data & Pred & Data & Pred & Data & Pred & Data & Pred & Data & Pred \\\\\n",
    "    \\midrule\n",
    tab5_body,
    "    \\bottomrule\n",
    "\\end{tabular}%\n",
    "}\n",
    "\\par\\end{centering}\n",
    "\\vspace{0.5em}\n",
    "\\begin{singlespace}\n",
    "  {\\footnotesize \\emph{Notes:} This table reports the fit of model predictions to key data moments. ",
    "All quantities except the number of observations are reported in percentage point units. ",
    "Coverage selection patterns are reported as the total claim cost of each coverage benchmarked against (divided by) that of the mandatory minimum plan. ",
    "Monitoring selection patterns are reported as the total claim cost of opt-in customers divided by that of the customers who did not opt-in. ",
    "We do not observe claim realization for renewal customers that left the firm. ",
    "``Pre-Mtr'' and ``Post-Mtr'' separate new customers to the firm before and after the introduction of monitoring. ",
    "``Pre-MM Chg'' and ``Post-MM Chg'' separate customers before and after the State raised mandatory minimum coverage from \\$40,000 to \\$50,000. \\par}\n",
    "\\end{singlespace}\n",
    "\\end{table}"
  )

  writeLines(tab5_tex, file.path(TABLES_DIR, "tab_5.tex"))
  cat("  [GEN] tab_5.tex\n"); n_generated <- n_generated + 1
} else {
  cat("  [SKIP] tab_5.tex (no CSV)\n"); n_skipped <- n_skipped + 1
}

# Tab C.2 (MH-het model fit) — same tabular format as Tab 4, different caption/notes
tab_c2_csv <- file.path(MODEL_FIT_DIR, "tab_c2.csv")
if (file.exists(tab_c2_csv)) {
  df_c2 <- read.csv(tab_c2_csv, stringsAsFactors = FALSE)

  c2_body <- build_fit_tabular_body(df_c2)

  c2_tex <- paste0(
    "\\begin{table}[H]\n",
    "\\begin{centering}\n",
    "% ---- First table ----\n",
    "\\caption{Fit of Claim Risk, Monitoring Score, and Renewal Pricing}\n",
    "\\label{tab:fit_c_mh_het}\n",
    "\\resizebox{\\linewidth}{!}{%\n",
    "\\begin{tabular}{\n",
    "    @{} >{\\raggedright\\arraybackslash}p{4.2cm} rr\n",
    "        >{\\raggedright\\arraybackslash}p{5.2cm} rr @{}\n",
    "}\n",
    "    \\toprule\n",
    "    \\multicolumn{3}{c}{\\textbf{Risk \\& Score}} & \\multicolumn{3}{c}{\\textbf{Pricing}} \\\\\n",
    "    \\cmidrule(lr){1-3} \\cmidrule(lr){4-6}\n",
    "    Moment & Data & Predicted & Moment & Data & Predicted \\\\\n",
    "    \\midrule\n",
    c2_body,
    "    \\bottomrule\n",
    "\\end{tabular}\n",
    "}\n",
    "\\par\\end{centering}\n",
    "\\vspace{0.5em}\n",
    "\\begin{singlespace}\n",
    "  {\\footnotesize \\emph{Notes:} This table reports the fit of model (incorporating risk-class-based heterogeneity in the moral hazard effect) predictions to key data moments. ",
    "Compared to Tab. \\ref{tab:fit_c}, the only change detectable (more than 0.001) are the monitoring score moment predictions. \\par}\n",
    "\\end{singlespace}\n",
    "\\end{table}"
  )

  # Append Block 2 (choice shares) if CSV exists
  tab_c2b_csv <- file.path(MODEL_FIT_DIR, "tab_c2_block2.csv")
  if (file.exists(tab_c2b_csv)) {
    df_c2b <- read.csv(tab_c2b_csv, stringsAsFactors = FALSE)
    c2b_body <- build_tab5_body(df_c2b)

    c2b_tex <- paste0(
      "\n\\begin{table}[H]\n",
      "\\begin{centering}\n",
      "\\caption{Fit of Choice Shares (Coverage, Monitoring, and Attrition) and Selection}\n",
      "\\label{tab:fit_d_mh_het}\n",
      "\\resizebox{\\linewidth}{!}{%\n",
      "\\begin{tabular}{@{} ll *{2}{rr} rr *{3}{rr} @{}}\n",
      "    \\toprule\n",
      "    & & \\multicolumn{4}{c}{New customers} & \\multicolumn{2}{c}{} & \\multicolumn{6}{c}{Renewal customers} \\\\\n",
      "    \\cmidrule(lr){3-6} \\cmidrule(lr){9-14}\n",
      "    & & \\multicolumn{2}{c}{Pre-Mtr} & \\multicolumn{2}{c}{Post-Mtr} & \\multicolumn{2}{c}{Pre-MM Chg} & \\multicolumn{2}{c}{Post-MM Chg} & \\multicolumn{2}{c}{Pre-Mtr} & \\multicolumn{2}{c}{Post-Mtr} \\\\\n",
      "    \\cmidrule(lr){3-4} \\cmidrule(lr){5-6} \\cmidrule(lr){7-8} \\cmidrule(lr){9-10} \\cmidrule(lr){11-12} \\cmidrule(lr){13-14}\n",
      "    & & Data & Pred & Data & Pred & Data & Pred & Data & Pred & Data & Pred & Data & Pred \\\\\n",
      "    \\midrule\n",
      c2b_body,
      "    \\bottomrule\n",
      "\\end{tabular}\n",
      "}\n",
      "\\par\\end{centering}\n",
      "\\vspace{0.5em}\n",
      "\\begin{singlespace}\n",
      "  {\\footnotesize \\emph{Notes:} This table reports the fit of model (incorporating risk-class-based heterogeneity in the moral hazard effect) predictions to key data moments. ",
      "Compared to Tab. \\ref{tab:fit_d}, there are slight shifts in coverage and monitoring shares and selection patterns. \\par}\n",
      "\\end{singlespace}\n",
      "\\end{table}"
    )
    c2_tex <- paste0(c2_tex, "\n", c2b_tex)
  }

  writeLines(c2_tex, file.path(APPENDIX_DIR, "tab_c2.tex"))
  cat("  [GEN] appendix/tab_c2.tex\n"); n_generated <- n_generated + 1
} else {
  cat("  [SKIP] appendix/tab_c2.tex (no CSV)\n"); n_skipped <- n_skipped + 1
}

################################################################################
## Part 2: Model fit figures (Fig 6a, 6b, B.3, B.4) from CSVs
################################################################################

source("code/functions/helper.R")

# Fig 6a (pricing by lambda, regime 3)
fig_6a_csv <- file.path(MODEL_FIT_DIR, "fig_6a.csv")
if (file.exists(fig_6a_csv)) {
  df <- read.csv(fig_6a_csv, stringsAsFactors = FALSE)
  gg <- ggplot(df %>% mutate(
    rr = as.character(rr),
    rr = gsub("tm", "", ifelse(grepl("tm", rr), rr, " none"))
  ), aes(x = llambda_bin, y = R_mean, group = rr,
         color = ifelse(rr_type == "nb", "baseline", "monitoring"),
         shape = ifelse(rr_type == "nb", "baseline", "monitoring"))) +
    geom_errorbar(aes(ymin = R_mean - 1.96*se, ymax = R_mean + 1.96*se), width = 0.06, alpha = 0.3) +
    geom_point() + geom_line() +
    fte_theme() +
    theme(panel.grid.minor = element_blank(), legend.position = "right") +
    scale_color_manual(values = c("#999999", "#E69F00")) +
    labs(color = "Pricing Type", shape = "Pricing Type") +
    xlab("Log Poisson Accident Arrival Rate") +
    ylab("First Renewal Price Charged")
  ggsave(gg, file = file.path(IMAGES_DIR, "fig_6a.png"), width = 5, height = 3, dpi = 300)
  cat("  [GEN] fig_6a.png\n"); n_generated <- n_generated + 1
} else {
  cat("  [SKIP] fig_6a.png (no CSV)\n"); n_skipped <- n_skipped + 1
}

# Fig 6b (lambda density by TM status)
fig_6b_csv <- file.path(MODEL_FIT_DIR, "fig_6b.csv")
if (file.exists(fig_6b_csv)) {
  df <- read.csv(fig_6b_csv, stringsAsFactors = FALSE)
  gg <- ggplot(df %>% mutate(monitoring = as.factor(monitoring)),
               aes(x = llambda, group = monitoring, color = monitoring, fill = monitoring)) +
    geom_density(linewidth = 0.2, alpha = 0.3) +
    fte_theme() +
    theme(panel.grid.minor = element_blank(), legend.position = "bottom") +
    scale_color_manual(values = c("#999999", "#E69F00")) +
    scale_fill_manual(values = c("#999999", "#E69F00")) +
    coord_cartesian(xlim = c(-4.5, -1.5)) +
    labs(color = "monitoring", fill = "monitoring") +
    xlab("Log(Poisson Accident Arrival Rate)")
  ggsave(gg, file = file.path(IMAGES_DIR, "fig_6b.png"), width = 6, height = 3.5, dpi = 300)
  cat("  [GEN] fig_6b.png\n"); n_generated <- n_generated + 1
} else {
  cat("  [SKIP] fig_6b.png (no CSV)\n"); n_skipped <- n_skipped + 1
}

# Fig B.3 (score vs lambda by regime)
fig_b3_csv <- file.path(MODEL_FIT_DIR, "fig_b3.csv")
if (file.exists(fig_b3_csv)) {
  df <- read.csv(fig_b3_csv, stringsAsFactors = FALSE)
  df <- df[!is.na(df$rr) & df$rr != "NA", ]
  gg <- ggplot(df, aes(x = llambda_bin, y = mean, group = rr, color = factor(rr))) +
    geom_errorbar(aes(ymin = mean - 1.96*se, ymax = mean + 1.96*se), width = 0.1, alpha = 0.2) +
    geom_point() + geom_line() +
    facet_grid(. ~ type, scales = "free") +
    fte_theme() +
    theme(panel.grid.minor = element_blank(), legend.position = "bottom") +
    scale_color_manual(values = c("#999999", "#E69F00", "#56B4E9")) +
    labs(color = "Monitoring Regime") +
    xlab("Log Poisson Accident Arrival Rate") +
    ylab("Monitoring Score")
  ggsave(gg, file = file.path(APPENDIX_DIR, "fig_b3.png"), width = 10, height = 6, dpi = 300)
  cat("  [GEN] appendix/fig_b3.png\n"); n_generated <- n_generated + 1
} else {
  cat("  [SKIP] appendix/fig_b3.png (no CSV)\n"); n_skipped <- n_skipped + 1
}

# Fig B.4 (pricing by lambda, all regimes)
fig_b4_csv <- file.path(MODEL_FIT_DIR, "fig_b4.csv")
if (file.exists(fig_b4_csv)) {
  df <- read.csv(fig_b4_csv, stringsAsFactors = FALSE)
  gg <- ggplot(df %>% mutate(
    rr = as.character(rr),
    rr_display = gsub("tm", "", ifelse(grepl("tm", rr), rr, " none")),
    period = gsub("period: ", "", period),
    period = ifelse(period == "pre-tm", "Baseline Pricing Regime 1",
                    ifelse(period == "tm1-tm2", "Baseline Pricing Regime 2",
                           "Baseline Pricing Regime 3"))
  ), aes(x = llambda_bin, y = R_mean, group = rr,
         color = rr_display,
         shape = ifelse(rr_type == "nb", "baseline", "monitoring"))) +
    geom_errorbar(aes(ymin = R_mean - 1.96*se, ymax = R_mean + 1.96*se), width = 0.06, alpha = 0.3) +
    geom_point() + geom_line() +
    facet_grid(. ~ period) +
    fte_theme() +
    theme(panel.grid.minor = element_blank(), legend.position = "bottom") +
    scale_color_manual(values = c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")) +
    labs(color = "Monitoring Regimes", shape = "Pricing Type") +
    xlab("Log Poisson Accident Arrival Rate") +
    ylab("First Renewal Price Charged")
  ggsave(gg, file = file.path(APPENDIX_DIR, "fig_b4.png"), width = 10, height = 6, dpi = 300)
  cat("  [GEN] appendix/fig_b4.png\n"); n_generated <- n_generated + 1
} else {
  cat("  [SKIP] appendix/fig_b4.png (no CSV)\n"); n_skipped <- n_skipped + 1
}

################################################################################
## Part 3: CTF tables (Tab 7, A.9–A.15, C.4, C.5) from CSVs
################################################################################

## Adapt generate_ctf_table to read from CSV instead of RDS
generate_ctf_table_from_csv <- function(csv_path, caption, label, is_main = FALSE,
                                        table_placement = NULL, ...) {
  csv_df <- read.csv(csv_path, stringsAsFactors = FALSE)

  # Reconstruct d (level) and dd (delta) from CSV
  d <- csv_df[csv_df$type == "level", ]
  dd <- csv_df[csv_df$type == "delta", ]
  rownames(d) <- d$scenario
  rownames(dd) <- dd$scenario

  # Extract calibration values from first level row
  brand_cal <- c(d$brand_value_1[1], d$brand_value_2[1], d$brand_value_3[1])
  costs_cal <- c(d$cost_factor_1[1], d$cost_factor_2[1], d$cost_factor_3[1])

  # Build a fake rds-like list for generate_ctf_table compatibility
  rds <- list(
    df_ctf_results = d[, !names(d) %in% c("scenario", "type", "brand_value_1", "brand_value_2", "brand_value_3", "cost_factor_1", "cost_factor_2", "cost_factor_3")],
    df_ctf_results_delta = dd[, !names(dd) %in% c("scenario", "type", "brand_value_1", "brand_value_2", "brand_value_3", "cost_factor_1", "cost_factor_2", "cost_factor_3")],
    brand_value_calibrated = brand_cal,
    cost_factors_calibrated = costs_cal
  )

  # Now use the same logic as generate_ctf_table
  d <- rds$df_ctf_results
  dd <- rds$df_ctf_results_delta
  n_scenarios <- nrow(d)

  safe_col <- function(df, col) {
    if (col %in% names(df)) df[[col]] else rep(NA_real_, nrow(df))
  }

  # Surplus Division
  cw_delta <- safe_col(dd, "consumer_welfare")
  if (is_main) {
    cw_vals <- c("-", sapply(cw_delta[-1], fmt_delta))
  } else {
    cw_vals <- c(f2(cw_delta[1]), sapply(cw_delta[-1], fmt_delta))
  }

  firm_profit      <- sapply(safe_col(d, "firm_profit"), f2)
  competitor_profit <- sapply(safe_col(d, "competitor_profit"), f2)
  industry_profit  <- sapply(safe_col(d, "industry_profit"), f2)

  ts_delta <- safe_col(dd, "total_surplus")
  if (is_main) {
    ts_vals <- c("-", sapply(ts_delta[-1], fmt_delta))
  } else {
    ts_vals <- c(f2(ts_delta[1]), sapply(ts_delta[-1], fmt_delta))
  }

  coverage   <- sapply(safe_col(d, "coverage") / COVERAGE_DIVISOR, f2)
  firm_share <- sapply(safe_col(d, "firm_market_share"), f2)
  renew_prob <- sapply(safe_col(d, "renewal_firm_choice_prob"), f2)
  mon_share  <- sapply(safe_col(d, "monitoring_market_share"), f2)

  k0s  <- sapply(1 + safe_col(d, "unmonitored_surcharge") / 100, fmt_kappa)
  k0d  <- sapply((100 - safe_col(d, "opt_in_discount")) / 100, fmt_kappa)
  k1s  <- sapply(safe_col(d, "risk_surcharge_factor") / 100, fmt_kappa)
  k1d  <- sapply(safe_col(d, "rent_sharing_factor") / 100, fmt_kappa)

  kc0s <- sapply(1 + safe_col(d, "competitor_surcharge") / 100, fmt_kappa)
  kc1s <- sapply(safe_col(d, "competitor_risk_surcharge_factor") / 100, fmt_kappa)
  kc1d <- sapply(safe_col(d, "competitor_rent_sharing_factor") / 100, fmt_kappa)

  pricing_header <- if (is_main) "Pricing Levers" else "Pricing"
  comp_pricing_header <- if (is_main) "Competitor Pricing Levers" else "Competitor Pricing"
  mkt_share_label <- if (is_main) "Initial-Period Firm Market Share" else "First-Period Firm Market Share"

  make_row <- function(label_text, vals) {
    paste0(" & ", label_text, " & ", paste(vals, collapse = " & "), "\\\\")
  }

  if (n_scenarios == 6) {
    col_spec <- "\\begin{tabular}{@{} l l *{6}{>{\\raggedleft\\arraybackslash}p{1.7cm}} @{}}"
    header <- paste0(
      "\\toprule\n",
      "\\multicolumn{2}{c}{Metrics$^{1}$} & \\multicolumn{5}{c}{Scenarios$^{2}$} \\\\\n",
      "\\cmidrule(lr){1-2} \\cmidrule(lr){3-8} \n",
      "& & \\multicolumn{1}{c}{No} & \\multicolumn{1}{c}{Current} & \\multicolumn{4}{c}{Counterfactual Equilibria} \\\\\n",
      "[-0.3em]\n\\cmidrule(lr){5-8}\n\\addlinespace[-3pt]\n",
      "& & \\multicolumn{1}{c}{Monitoring} & \\multicolumn{1}{c}{Regime} & ",
      "\\multicolumn{1}{r}{Partial} & \\multicolumn{1}{r}{Full} & ",
      "\\multicolumn{1}{r}{+ Data Port.} & \\multicolumn{1}{r}{+ Disc. Floor} \\\\\n",
      "\\midrule")
    section_span <- 8
  } else {
    col_spec <- "\\begin{tabular}{@{} l l *{5}{>{\\raggedleft\\arraybackslash}p{1.7cm}} @{}}"
    header <- paste0(
      "\\toprule\n",
      "\\multicolumn{2}{c}{Metrics$^{1}$} & \\multicolumn{5}{c}{Scenarios$^{2}$} \\\\\n",
      "\\cmidrule(lr){1-2} \\cmidrule(lr){3-7} \n",
      "& & \\multicolumn{1}{c}{No} & \\multicolumn{1}{c}{Current} & \\multicolumn{3}{c}{Counterfactual Equilibria} \\\\\n",
      "[-0.3em]\n\\cmidrule(lr){5-7}\n\\addlinespace[-3pt]\n",
      "& & \\multicolumn{1}{c}{Monitoring} & \\multicolumn{1}{c}{Regime} & ",
      "\\multicolumn{1}{r}{Partial} & \\multicolumn{1}{r}{Full} & ",
      "\\multicolumn{1}{r}{+ Data Port.} \\\\\n",
      "\\midrule")
    section_span <- 7
  }

  section_hdr <- function(title) {
    paste0("\\addlinespace[0.5em]\n\\multicolumn{", section_span, "}{l}{\\textit{\\textbf{", title, "}}}\\\\")
  }

  body_lines <- c("", "", "",
    section_hdr("Surplus Division"),
    make_row("Consumer Welfare ($\\Delta$\\$ p.c.y.)", cw_vals),
    make_row("Firm Profit (\\$ p.c.y.)", firm_profit),
    make_row("Competitor Profit (\\$ p.c.y.)", competitor_profit),
    make_row("Industry Profit (\\$ p.c.y.)", industry_profit),
    make_row("Total Surplus ($\\Delta$\\$ p.c.y.)", ts_vals),
    section_hdr("Quantity"),
    make_row("Coverage (\\$000 p.c.y.)", coverage),
    make_row(paste0(mkt_share_label, " (\\%)"), firm_share),
    make_row("Renewal Firm Choice Prob (\\%)", renew_prob),
    make_row("Monitoring Market Share (\\%)", mon_share),
    section_hdr(pricing_header),
    make_row("Baseline Factor ($\\kappa_{0s}$)", k0s),
    make_row("Initial-Period Monitoring Factor ($\\kappa_{0d}$)", k0d),
    make_row("Risk Surcharge Factor ($\\kappa_{1s}$)", k1s),
    make_row("Rent Sharing Factor ($\\kappa_{1d}$)", k1d),
    section_hdr(comp_pricing_header),
    make_row("Baseline Factor ($\\kappa^c_{0s}$,\\%)", kc0s),
    make_row("Risk Surcharge Factor ($\\kappa^c_{1s}$)", kc1s),
    make_row("Rent Sharing Factor ($\\kappa^c_{1d}$)", kc1d)
  )

  brand_cal <- if (!is.null(rds$brand_value_calibrated)) rds$brand_value_calibrated else c(0, 0, 0)
  costs_cal <- if (!is.null(rds$cost_factors_calibrated)) rds$cost_factors_calibrated else c(0, 0, 0)

  if (is_main) {
    notes <- paste0(
      "{\\footnotesize \\emph{Notes:} ",
      "This table reports counterfactual simulation results, with the ``Current Regime'' corresponding to model fit on data after the introduction of monitoring. ",
      "The market is modeled as consisting of three players: the monitoring firm, a focal competitor, and a composite passive competitor representing the residual market. ",
      "The focal competitor is defined as the firm offering the lowest average premium for the most popular coverage plan, while the passive competitor corresponds to the median-priced firm. ",
      "Brand effects are calibrated to \\$0, \\$", round(abs(brand_cal[2] - brand_cal[1]) * 1000),
      ", and \\$", round(abs(brand_cal[1]) * 1000),
      " for the three firms, respectively, and per-period per-customer marginal costs are calibrated to \\$",
      round(costs_cal[1] * 1000), ", \\$",
      round(costs_cal[2] * 1000), ", and \\$",
      round(costs_cal[3] * 1000), ", respectively.\n\n",
      "$^{1}$~Units are given in brackets. ``$\\Delta$'' denotes changes relative to the ``No Monitoring'' benchmark; ``p.c.y.'' stands for per capita per year; ``\\$'' indicates dollar terms; and ``\\%'' indicates percentage-point terms. Pricing parameters have a step size of 1 percentage point, or 0.01.\n\n",
      "$^{2}$~Scenario definitions: In ``No Monitoring,'' all customers are mechanically made ineligible for monitoring without changing baseline prices. ",
      "``Partial Equilibrium'' computes the profit-maximizing equilibrium when the focal competitor's pricing is held fixed. ",
      "``Full Equilibrium'' computes the Nash equilibrium in which the focal competitor can only adjust its baseline surcharge. ",
      "``+ Data Port.'' computes a Nash equilibrium where both firms observe monitoring outcomes and can commit to future prices subject to ex-post risk surcharge $\\leq 100\\%$ and rent sharing $\\geq 0$. ",
      "``+~Disc. Floor'' further restricts that rent-sharing $\\geq 100\\%$.\n",
      "\\par }")
    resize_width <- "\\textwidth"
  } else {
    notes <- paste0(
      "{\\footnotesize \\emph{Notes:} \n",
      "Grid search steps for some pricing parameters are coarsened: 0.05 step size for the initial-period monitoring factors, and 0.1 step size for the risk surcharge and rent sharing factors.\n",
      "Brand effects are calibrated to \\$0, \\$", round(abs(brand_cal[2] - brand_cal[1]) * 1000),
      ", and \\$", round(abs(brand_cal[1]) * 1000),
      " for the three firms, respectively, and per-period per-customer marginal costs are calibrated to \\$",
      round(costs_cal[1] * 1000), ", \\$",
      round(costs_cal[2] * 1000), ", and \\$",
      round(costs_cal[3] * 1000), ", respectively.\n",
      "\\par }")
    resize_width <- "0.9\\textwidth"
  }

  # Table placement: use explicit override if provided, else default to [h]
  if (!is.null(table_placement)) {
    table_begin <- paste0("\\begin{table}[", table_placement, "]")
  } else {
    table_begin <- "\\begin{table}[h]"
  }

  lines <- c(table_begin, "\\begin{centering}",
    paste0("\\caption{", caption, "}\\label{", label, "}"), "",
    paste0("\\resizebox{", resize_width, "}{!}{%"),
    col_spec, "", header, body_lines, "", "", "",
    "\\bottomrule", "\\end{tabular}", "}",
    "\\par\\end{centering}", "\\vspace{0.1cm}",
    "\\begin{singlespace}", notes, "\\end{singlespace}", "\\end{table}")

  paste(lines, collapse = "\n")
}

## Generate CTF tables
ctf_specs <- list(
  list(csv = "tab_7.csv",    tex = "tab_7.tex",              caption = "Counterfactual Simulation Results",                                  label = "tab:ctf_main",               is_main = TRUE,  appendix = FALSE),
  list(csv = "tab_a9.csv",   tex = "appendix/tab_a9.tex",    caption = "Counterfactual Simulation Results - 50\\% Lower Monitoring Cost",     label = "tab:ctf_robust_low_cost",     appendix = TRUE),
  list(csv = "tab_a10.csv",  tex = "appendix/tab_a10.tex",   caption = "Counterfactual Simulation Results - 50\\% Higher Monitoring Marginal Cost", label = "tab:ctf_robust_high_cost", appendix = TRUE),
  list(csv = "tab_a11.csv",  tex = "appendix/tab_a11.tex",   caption = "Counterfactual Simulation Results - Two-Period Time Horizon",         label = "tab:ctf_robust_2p",           appendix = TRUE),
  list(csv = "tab_a12.csv",  tex = "appendix/tab_a12.tex",   caption = "Counterfactual Simulation Results - Four-Period Time Horizon",        label = "tab:ctf_robust_4p",           appendix = TRUE),
  list(csv = "tab_a13.csv",  tex = "appendix/tab_a13.tex",   caption = "Counterfactual Simulation Results - 2X Monitoring Pricing Constraints", label = "tab:ctf_robust_unconstraint", appendix = TRUE, placement = "!htbp"),
  list(csv = "tab_a14.csv",  tex = "appendix/tab_a14.tex",   caption = "Counterfactual Simulation Results - Concentrated Market",             label = "tab:ctf_robust_minmed_only",  appendix = TRUE, placement = "!htbp"),
  list(csv = "tab_a15.csv",  tex = "appendix/tab_a15.tex",   caption = "Counterfactual Simulation Results - Median-Pricing Composite Competitor", label = "tab:ctf_robust_med",       appendix = TRUE, placement = "!htbp"),
  list(csv = "tab_c4.csv",   tex = "appendix/tab_c4.tex",    caption = "Counterfactual Simulation Results - Heterogeneous Moral Hazard Effect", label = "tab:ctf_robust_mh_het",    appendix = TRUE, placement = "H"),
  list(csv = "tab_c5.csv",   tex = "appendix/tab_c5.tex",    caption = "Counterfactual Simulation Results - 10\\% Learning Effect",            label = "tab:ctf_robust_learning",     appendix = TRUE, placement = "H")
)

for (spec in ctf_specs) {
  csv_path <- file.path(CTF_DIR, spec$csv)
  if (file.exists(csv_path)) {
    is_main <- if (!is.null(spec$is_main)) spec$is_main else FALSE
    placement <- if (!is.null(spec$placement)) spec$placement else NULL
    tex <- generate_ctf_table_from_csv(csv_path, spec$caption, spec$label,
                                       is_main = is_main, table_placement = placement)
    out_path <- file.path(TABLES_DIR, spec$tex)
    writeLines(tex, out_path)
    cat("  [GEN]", spec$tex, "\n"); n_generated <- n_generated + 1
  } else {
    cat("  [SKIP]", spec$tex, "(no CSV)\n"); n_skipped <- n_skipped + 1
  }
}

################################################################################
cat(sprintf("\nc3_get_fitctf_exhibits.R completed: %d generated, %d skipped.\n", n_generated, n_skipped))
