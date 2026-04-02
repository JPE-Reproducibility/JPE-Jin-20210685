################################################################################
## ctf_table_helpers.R — Shared helpers for generating CTF LaTeX tables
##
## Sourced by: codes/results/c2_ctf_main.R
################################################################################

## Format a numeric value with 2 decimal places
f2 <- function(x) sprintf("%.2f", x)

## Format a delta value with + sign for positive, "-" for NA
fmt_delta <- function(x) ifelse(is.na(x), "-", ifelse(x >= 0, paste0("+", f2(x)), f2(x)))

## Format a kappa factor value, returning "-" for NA
fmt_kappa <- function(x) ifelse(is.na(x), "-", f2(x))

## Generate LaTeX table from CTF .rds file
generate_ctf_table <- function(
    rds_path,
    caption,
    label,
    is_main = FALSE,   # TRUE for tab_5 (different formatting)
    table_placement = "h",
    resize_width = "0.9\\textwidth",
    notes = NULL,
    metadata_comment = NULL,
    override_brand = NULL,  # Use main spec brand values for consistent notes
    override_costs = NULL,  # Use main spec cost values for consistent notes
    coverage_divisor = COVERAGE_DIVISOR
) {
  rds <- readRDS(rds_path)
  d   <- rds$df_ctf_results
  dd  <- rds$df_ctf_results_delta
  n_scenarios <- nrow(d)
  scenarios <- rownames(d)

  # Save intermediate CSV for standalone exhibit generation
  csv_name <- sub("\\.rds$", ".csv", basename(rds_path))
  # Map labeled .rds names to paper table names
  csv_name_map <- c(
    "ctf_tab7_main.rds" = "tab_7.csv",
    "ctf_tab_a9_low_cost.rds" = "tab_a9.csv",
    "ctf_tab_a10_high_cost.rds" = "tab_a10.csv",
    "ctf_tab_a11_2p.rds" = "tab_a11.csv",
    "ctf_tab_a12_4p.rds" = "tab_a12.csv",
    "ctf_tab_a13_unconst.rds" = "tab_a13.csv",
    "ctf_tab_a14_minmed.rds" = "tab_a14.csv",
    "ctf_tab_a15_median.rds" = "tab_a15.csv",
    "ctf_tab_c4_mhhet.rds" = "tab_c4.csv",
    "ctf_tab_c5_learning.rds" = "tab_c5.csv"
  )
  rds_basename <- basename(rds_path)
  if (rds_basename %in% names(csv_name_map)) csv_name <- csv_name_map[rds_basename]

  # Build combined level + delta CSV
  d_level <- d
  d_level$scenario <- rownames(d_level)
  d_level$type <- "level"
  dd_delta <- dd
  dd_delta$scenario <- rownames(dd_delta)
  dd_delta$type <- "delta"
  # Add calibration metadata columns
  brand_cal <- rds$brand_value_calibrated
  costs_cal <- rds$cost_factors_calibrated
  d_level$brand_value_1 <- brand_cal[1]; d_level$brand_value_2 <- brand_cal[2]; d_level$brand_value_3 <- brand_cal[3]
  d_level$cost_factor_1 <- costs_cal[1]; d_level$cost_factor_2 <- costs_cal[2]; d_level$cost_factor_3 <- costs_cal[3]
  dd_delta$brand_value_1 <- NA; dd_delta$brand_value_2 <- NA; dd_delta$brand_value_3 <- NA
  dd_delta$cost_factor_1 <- NA; dd_delta$cost_factor_2 <- NA; dd_delta$cost_factor_3 <- NA
  csv_df <- rbind(d_level, dd_delta)
  csv_path <- file.path(CTF_RESULTS_DIR, csv_name)
  write.csv(csv_df, csv_path, row.names = FALSE)
  cat("    Saved CSV:", csv_path, "\n")

  # Determine column count: 2 (label cols) + n_scenarios (data cols)
  n_cols <- 2 + n_scenarios

  ## ---- Extract and format metrics ----

  # Helper: safely extract a column, returning NA vector if missing
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

  # Quantity
  coverage   <- sapply(safe_col(d, "coverage") / coverage_divisor, f2)
  firm_share <- sapply(safe_col(d, "firm_market_share"), f2)
  renew_prob <- sapply(safe_col(d, "renewal_firm_choice_prob"), f2)
  mon_share  <- sapply(safe_col(d, "monitoring_market_share"), f2)

  # Pricing Levers: convert from integer percentage points to factor notation
  k0s <- sapply(1 + safe_col(d, "unmonitored_surcharge") / 100, fmt_kappa)
  k0d <- sapply((100 - safe_col(d, "opt_in_discount")) / 100, fmt_kappa)
  k1s <- sapply(safe_col(d, "risk_surcharge_factor") / 100, fmt_kappa)
  k1d <- sapply(safe_col(d, "rent_sharing_factor") / 100, fmt_kappa)

  # Competitor Pricing Levers
  kc0s <- sapply(1 + safe_col(d, "competitor_surcharge") / 100, fmt_kappa)
  kc1s <- sapply(safe_col(d, "competitor_risk_surcharge_factor") / 100, fmt_kappa)
  kc1d <- sapply(safe_col(d, "competitor_rent_sharing_factor") / 100, fmt_kappa)

  ## ---- Section header names ----
  pricing_header <- if (is_main) "Pricing Levers" else "Pricing"
  comp_pricing_header <- if (is_main) "Competitor Pricing Levers" else "Competitor Pricing"
  mkt_share_label <- if (is_main) "Initial-Period Firm Market Share" else "First-Period Firm Market Share"

  ## ---- Build LaTeX rows ----

  make_row <- function(label_text, vals) {
    paste0(" & ", label_text, " & ", paste(vals, collapse = " & "), "\\\\")
  }

  # Column header setup
  if (n_scenarios == 6) {
    col_spec <- paste0("\\begin{tabular}{@{} l l *{6}{>{\\raggedleft\\arraybackslash}p{1.7cm}} @{}}")
    header <- paste0(
      "\\toprule\n",
      "\\multicolumn{2}{c}{Metrics$^{1}$} & \\multicolumn{5}{c}{Scenarios$^{2}$} \\\\\n",
      "\\cmidrule(lr){1-2} \\cmidrule(lr){3-8} \n",
      "& & \\multicolumn{1}{c}{No} & \\multicolumn{1}{c}{Current} & \\multicolumn{4}{c}{Counterfactual Equilibria} \\\\\n",
      "[-0.3em]\n",
      "\\cmidrule(lr){5-8}\n",
      "\\addlinespace[-3pt]\n",
      "& & \\multicolumn{1}{c}{Monitoring} & \\multicolumn{1}{c}{Regime} & ",
      "\\multicolumn{1}{r}{Partial} & \\multicolumn{1}{r}{Full} & ",
      "\\multicolumn{1}{r}{+ Data Port.} & \\multicolumn{1}{r}{+ Disc. Floor} \\\\\n",
      "\\midrule"
    )
    section_span <- 8
  } else {
    # 5 scenarios (no discount_floor)
    col_spec <- paste0("\\begin{tabular}{@{} l l *{5}{>{\\raggedleft\\arraybackslash}p{1.7cm}} @{}}")
    header <- paste0(
      "\\toprule\n",
      "\\multicolumn{2}{c}{Metrics$^{1}$} & \\multicolumn{5}{c}{Scenarios$^{2}$} \\\\\n",
      "\\cmidrule(lr){1-2} \\cmidrule(lr){3-7} \n",
      "& & \\multicolumn{1}{c}{No} & \\multicolumn{1}{c}{Current} & \\multicolumn{3}{c}{Counterfactual Equilibria} \\\\\n",
      "[-0.3em]\n",
      "\\cmidrule(lr){5-7}\n",
      "\\addlinespace[-3pt]\n",
      "& & \\multicolumn{1}{c}{Monitoring} & \\multicolumn{1}{c}{Regime} & ",
      "\\multicolumn{1}{r}{Partial} & \\multicolumn{1}{r}{Full} & ",
      "\\multicolumn{1}{r}{+ Data Port.} \\\\\n",
      "\\midrule"
    )
    section_span <- 7
  }

  section_hdr <- function(title) {
    paste0("\\addlinespace[0.5em]\n",
           "\\multicolumn{", section_span, "}{l}{\\textit{\\textbf{", title, "}}}\\\\")
  }

  # Assemble all rows
  body_lines <- c(
    "",
    "",
    "",
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

  ## ---- Build notes ----

  if (is.null(notes)) {
    # Use override calibration values if provided, otherwise use this spec's own
    brand_cal <- if (!is.null(override_brand)) override_brand else rds$brand_value_calibrated
    costs_cal <- if (!is.null(override_costs)) override_costs else rds$cost_factors_calibrated

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
        "\\par }"
      )
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
        "\\par }"
      )
    }
  }

  ## ---- Assemble full LaTeX ----

  table_begin <- if (is_main) "\\begin{table}[h]" else paste0("\\begin{table}[", table_placement, "]")

  lines <- c(
    table_begin,
    "\\begin{centering}",
    paste0("\\caption{", caption, "}\\label{", label, "}"),
    "",
    paste0("\\resizebox{", resize_width, "}{!}{%"),
    col_spec,
    "",
    header,
    body_lines,
    "",
    "",
    "",
    "\\bottomrule",
    "\\end{tabular}",
    "}",
    "",
    "\\par\\end{centering}",
    "\\vspace{0.1cm}",
    "\\begin{singlespace}",
    notes,
    "\\end{singlespace}",
    "\\end{table}"
  )

  # Add metadata comment if provided
  if (!is.null(metadata_comment)) {
    lines <- c(lines, metadata_comment)
  }

  paste(lines, collapse = "\n")
}
