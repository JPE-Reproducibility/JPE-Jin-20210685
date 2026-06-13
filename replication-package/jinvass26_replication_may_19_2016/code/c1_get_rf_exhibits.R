################################################################################
## c1_get_rf_exhibits.R — Reduced-form exhibits from precomputed CSVs/JSONs
##
## Reads from: data/precomputed/rf/, data/estimates/regression_output/
## Writes to:  output/exhibits/, output/exhibits/appendix/
##
## Produces 22 exhibits:
##   Tab 1, 2, 3, A.1, A.2, C.1
##   Fig 2a, 2b, 3, 4, 5, A.3, A.4, A.5, A.6, B.1a, B.1b, B.2a, B.2b,
##       C.1, C.2, C.3, C.4
##
## Called by run_all.R. No data loading — purely formatting and plotting.
################################################################################

if (!exists("TABLES_DIR")) source("code/config.R")
source("code/functions/helper.R")

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(scales)
  library(kableExtra)
  library(stringr)
})

################################################################################
## Main body — runs only when not sourced for functions alone
################################################################################
if (exists(".GENERATE_EXHIBITS_SOURCED_ONLY") && .GENERATE_EXHIBITS_SOURCED_ONLY) {
  # Sourced for function definitions only; skip main body
} else {

stopifnot(exists("RF_CSV_DIR"))

cat("generate_exhibits.R: Regenerating all exhibits from CSVs...\n")
cat("  RF_CSV_DIR =", RF_CSV_DIR, "\n")
cat("  MODEL_TAB_DIR =", MODEL_TAB_DIR, "\n")
cat("  CTF_RESULTS_DIR =", CTF_RESULTS_DIR, "\n")

dir.create(TABLES_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(APPENDIX_DIR, recursive = TRUE, showWarnings = FALSE)

CACHE_DIR <- file.path(RF_CSV_DIR, "cache")
CACHE_DIR_APP <- file.path(RF_CSV_DIR, "appendix", "cache")
dir.create(CACHE_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(CACHE_DIR_APP, recursive = TRUE, showWarnings = FALSE)

n_generated <- 0
n_skipped <- 0

################################################################################
## STEP 0: Derive CSVs from regression JSONs
##
## For exhibits based on regressions, the primary input is a JSON file
## containing the full lm() summary. We derive the CSV plot/table data from
## the JSON coefficients. Non-regression exhibits use pre-computed CSVs directly.
################################################################################

cat("\n--- Deriving CSVs from regression JSONs ---\n")

read_json_coefs <- function(json_path) {
  if (!file.exists(json_path)) return(NULL)
  jsonlite::fromJSON(json_path)
}

## ---- fig_4.csv and fig_c1.csv from regression JSONs -------------------------
for (spec in list(
  list(json = "fig_4_regression.json", csv = "fig_4.csv", bal = FALSE),
  list(json = "appendix/fig_c1_regression.json", csv = "appendix/fig_c1.csv", bal = TRUE)
)) {
  j <- read_json_coefs(file.path(RF_REG_DIR, spec$json))
  if (!is.null(j)) {
    idx_names <- c("ubi_ind", paste0("RENW_CNT", 1:5),
                   paste0("ubi_ind:RENW_CNT", 1:5))
    est <- sapply(idx_names, function(nm) {
      if (!is.null(j$coefficients[[nm]])) j$coefficients[[nm]]$estimate else NA
    })
    se <- sapply(idx_names, function(nm) {
      if (!is.null(j$coefficients[[nm]])) j$coefficients[[nm]]$std_error else NA
    })
    est_unmon <- c(0, est[2:6])
    est_mon <- c(est[1], (est[1] + est[2:6] + est[7:11]))
    vis_tbl <- data.frame(
      period = rep(0:5, 2),
      group = c(rep("opt-in", 6), rep("opt-out", 6)),
      est = c(est_unmon, est_mon),
      ub = c(est_unmon, est_mon) + 1.96 * c(c(0, se[2:6]), c(se[1], se[7:11])),
      lb = c(est_unmon, est_mon) - 1.96 * c(c(0, se[2:6]), c(se[1], se[7:11]))
    )
    cache_path <- if (grepl("appendix", spec$csv)) file.path(CACHE_DIR_APP, basename(spec$csv))
                  else file.path(CACHE_DIR, basename(spec$csv))
    write.csv(vis_tbl, cache_path, row.names = FALSE)
    cat("  [DERIVE]", spec$csv, "from", spec$json, "\n")
  }
}

## ---- fig_c4.csv from regression JSON ----------------------------------------
j_c4 <- read_json_coefs(file.path(RF_REG_DIR, "appendix/fig_c4_regression.json"))
if (!is.null(j_c4)) {
  idx_names <- c("ubi_indTRUE", paste0("renw_cnt", 1:3),
                 paste0("ubi_indTRUE:renw_cnt", 1:3))
  est <- sapply(idx_names, function(nm) {
    if (!is.null(j_c4$coefficients[[nm]])) j_c4$coefficients[[nm]]$estimate else NA
  })
  se <- sapply(idx_names, function(nm) {
    if (!is.null(j_c4$coefficients[[nm]])) j_c4$coefficients[[nm]]$std_error else NA
  })
  est_unmon <- c(0, est[2:4] - est[1])
  est_mon <- c(est[1], (est[1] + est[2:4] + est[5:7]))
  vis_tbl <- data.frame(
    period = rep(0:3, 2),
    group = c(rep("unmonitored", 4), rep("monitored", 4)),
    est = c(est_unmon, est_mon),
    ub = c(est_unmon, est_mon) + 1.96 * c(c(0, se[2:4]), c(se[1], se[5:7])),
    lb = c(est_unmon, est_mon) - 1.96 * c(c(0, se[2:4]), c(se[1], se[5:7]))
  )
  vis_tbl$segment <- ifelse(vis_tbl$period < 1, "pre", "post")
  vis_tbl$segment_group <- paste(vis_tbl$group, vis_tbl$segment)
  write.csv(vis_tbl, file.path(CACHE_DIR_APP, "fig_c4.csv"), row.names = FALSE)
  cat("  [DERIVE] appendix/fig_c4.csv from fig_c4_regression.json\n")
}

## ---- fig_5.csv from combined regression JSON --------------------------------
j5 <- read_json_coefs(file.path(RF_REG_DIR, "fig_5_regression.json"))
if (!is.null(j5) && !is.null(j5$models)) {
  extract_demand <- function(m, method_label) {
    base <- m$coefficients$lg_prem_increase_hat %||% m$coefficients$lg_prem_increase
    int0 <- m$coefficients[["tm_segment_ftr0:lg_prem_increase_hat"]] %||%
            m$coefficients[["tm_segment_ftr0:lg_prem_increase"]]
    int1 <- m$coefficients[["tm_segment_ftr1:lg_prem_increase_hat"]] %||%
            m$coefficients[["tm_segment_ftr1:lg_prem_increase"]]
    data.frame(
      est = c(base$estimate, int0$estimate + base$estimate, int1$estimate + base$estimate),
      se = c(base$std_error, int0$std_error, int1$std_error),
      method = method_label,
      param_interest = c("monitored w/o discount", "unmonitored", "monitored w/ discount")
    )
  }
  df_iv <- extract_demand(j5$models$iv_second_stage, "IV")
  df_ols <- extract_demand(j5$models$ols, "OLS")
  df_est <- rbind(df_iv, df_ols)
  df_est$ub <- df_est$est + 1.96 * df_est$se
  df_est$lb <- df_est$est - 1.96 * df_est$se
  write.csv(df_est, file.path(CACHE_DIR, "fig_5.csv"), row.names = FALSE)
  cat("  [DERIVE] fig_5.csv from fig_5_regression.json\n")
}

## ---- fig_c2.csv and fig_c3.csv from heterogeneity regression JSON -----------
j_het <- read_json_coefs(file.path(RF_REG_DIR, "appendix/fig_c2_c3_regression.json"))
if (!is.null(j_het)) {
  # Extract triple-interaction terms: ubi_ind:post_ind:*_0
  all_names <- names(j_het$coefficients)
  het_names <- all_names[grepl("ubi_ind", all_names) & grepl("post_ind", all_names) & grepl("_0", all_names)]

  est_het <- sapply(het_names, function(nm) j_het$coefficients[[nm]]$estimate)
  se_het <- sapply(het_names, function(nm) j_het$coefficients[[nm]]$std_error)
  dimensions <- gsub("1+$", "", gsub("_0", "", gsub("ubi_ind:post_ind:", "", het_names)))

  df_het <- data.frame(
    est_het = est_het, est_het_lb = est_het - 1.96 * se_het,
    est_het_ub = est_het + 1.96 * se_het,
    stat_sig = (abs(est_het) > 0) & ((est_het - 1.96 * se_het) * (est_het + 1.96 * se_het) > 0),
    dimension = dimensions, row.names = NULL
  )

  # Split into coverage (y) and observables (x) dimensions
  df_y <- df_het[grepl("cov", df_het$dimension), ]
  df_x <- df_het[!grepl("cov", df_het$dimension), ]

  write.csv(df_x, file.path(CACHE_DIR_APP, "fig_c2.csv"), row.names = FALSE)
  write.csv(df_y, file.path(CACHE_DIR_APP, "fig_c3.csv"), row.names = FALSE)
  cat("  [DERIVE] appendix/fig_c2.csv, fig_c3.csv from fig_c2_c3_regression.json\n")
}

## ---- fig_b1b.csv from combined RD regression JSON ---------------------------
j_b1b <- read_json_coefs(file.path(RF_REG_DIR, "appendix/fig_b1b_regression.json"))
if (!is.null(j_b1b) && !is.null(j_b1b$models)) {
  model_map <- list(
    list(name = "prem_raw",    ctrl = "Raw",         reg = "Premium"),
    list(name = "prem_x_no_t", ctrl = "X w/o Trend", reg = "Premium"),
    list(name = "prem_x",      ctrl = "X Control",   reg = "Premium"),
    list(name = "clm_raw",     ctrl = "Raw",         reg = "Claim"),
    list(name = "clm_x_no_t",  ctrl = "X w/o Trend", reg = "Claim"),
    list(name = "clm_x",       ctrl = "X Control",   reg = "Claim")
  )
  b1b_rows <- list()
  for (spec in model_map) {
    m <- j_b1b$models[[spec$name]]
    if (!is.null(m)) {
      rd_coef <- m$coefficients$run_exceed_int
      b1b_rows[[length(b1b_rows) + 1]] <- data.frame(
        ctrl = spec$ctrl, reg = spec$reg,
        est = rd_coef$estimate, se = rd_coef$std_error,
        est_ub = rd_coef$estimate + 1.96 * rd_coef$std_error,
        est_lb = rd_coef$estimate - 1.96 * rd_coef$std_error
      )
    }
  }
  if (length(b1b_rows) == 6) {
    clm_raw_mean <- j_b1b$models$clm_raw$dep_var_mean
    result_tbl <- do.call(rbind, b1b_rows)
    clm_mask <- result_tbl$reg == "Claim"
    result_tbl[clm_mask, c("est", "se", "est_ub", "est_lb")] <-
      result_tbl[clm_mask, c("est", "se", "est_ub", "est_lb")] / clm_raw_mean
    write.csv(result_tbl, file.path(CACHE_DIR_APP, "fig_b1b.csv"), row.names = FALSE)
    cat("  [DERIVE] appendix/fig_b1b.csv from fig_b1b_regression.json\n")
  }
}

## ---- fig_a5_a6.csv from informativeness regression JSON ---------------------
j_a5a6 <- read_json_coefs(file.path(RF_REG_DIR, "appendix/fig_a5_a6_regression.json"))
if (!is.null(j_a5a6) && !is.null(j_a5a6$periods)) {
  p <- j_a5a6$periods  # nested data frame: p[["w/ ctrl"]]$mfx_ind etc.
  spec_names <- c("w/ ctrl", "claim ctrl", "w/o ctrl")
  rows <- list()
  for (g in spec_names) {
    sp <- p[[g]]
    for (t in seq_len(nrow(sp))) {
      avg <- sp$avg_clm_cnt[t]
      rows[[length(rows) + 1]] <- data.frame(
        pool = t, group = g,
        mfx_ind = sp$mfx_ind[t] / avg, se_ind = sp$se_ind[t] / avg,
        mfx_val = sp$mfx_val[t] / avg, se_val = sp$se_val[t] / avg,
        stringsAsFactors = FALSE
      )
    }
  }
  df_info_pct <- do.call(rbind, rows)
  write.csv(df_info_pct, file.path(CACHE_DIR_APP, "fig_a5_a6.csv"), row.names = FALSE)
  cat("  [DERIVE] appendix/fig_a5_a6.csv from fig_a5_a6_regression.json\n")
}

## ---- tab_2.tex from regression JSONs ----------------------------------------
## Tab 2 coefficients come from the fig_4 regression JSON (unbalanced, full controls)
## The key coefficients are: ubi_ind, ubi_ind:post_ind (= ubi_ind:RENW_CNT1-5 combined)
## Tab 2 and Tab 3 are generated from tab_2_regression.json and tab_3_regression.json
## in data/estimates/regression_output/.

cat("--- Done deriving CSVs ---\n\n")

################################################################################
## RF FIGURES
################################################################################

## ---- fig_4.png (MH claim progression, unbalanced) --------------------------

fig_4_csv <- file.path(CACHE_DIR, "fig_4.csv")
if (file.exists(fig_4_csv)) {
  vis_tbl <- read.csv(fig_4_csv, stringsAsFactors = FALSE)

  pd_narrow <- pd
  pd_narrow$width <- 0.1

  # Extract opt-out (monitored) period 0 estimate for annotation box
  est_mon <- vis_tbl$est[vis_tbl$period == 0 & vis_tbl$group == "opt-out"]
  se_mon <- (vis_tbl$ub[vis_tbl$period == 0 & vis_tbl$group == "opt-out"] - est_mon) / 1.96

  gg <- ggplot(data = vis_tbl, aes(x = period, group = group, color = group)) +
    geom_hline(yintercept = 0, alpha = 0.1) +
    geom_hline(yintercept = est_mon, alpha = 0.1) +
    geom_errorbar(aes(ymin = lb, ymax = ub), width = 0.1, position = pd_narrow, alpha = 0.4) +
    geom_point(aes(y = est), position = pd_narrow) +
    geom_line(aes(y = est), position = pd_narrow, size = 0.5, alpha = 0.5) +
    fte_theme() +
    theme(legend.position = "right", legend.title = element_blank(),
          panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank()) +
    scale_color_manual(values = c("#999999", "#E69F00", "#56B4E9")) +
    annotate("rect", xmin = -0.5, xmax = 0.5,
             ymin = est_mon - 3 * se_mon, ymax = est_mon + 3 * se_mon,
             fill = NA, colour = "red", linetype = "dotted") +
    annotate("text", x = 0, y = est_mon - 2.75 * se_mon, colour = "red", label = "monitored") +
    xlab("Period") +
    ylab("Claim Count Differentials w/ Controls")

  ggsave(gg, file = file.path(IMAGES_DIR, "fig_4.png"), width = 10, height = 5, dpi = 300)
  cat("  [GEN] fig_4.png\n"); n_generated <- n_generated + 1
} else {
  cat("  [SKIP] fig_4.png (no CSV)\n"); n_skipped <- n_skipped + 1
}

## ---- fig_5.png (demand elasticity) ------------------------------------------

fig_5_csv <- file.path(CACHE_DIR, "fig_5.csv")
if (file.exists(fig_5_csv)) {
  df_est_tbl <- read.csv(fig_5_csv, stringsAsFactors = FALSE)
  df_est_tbl$param_interest <- factor(df_est_tbl$param_interest,
    levels = c("monitored w/ discount", "monitored w/o discount", "unmonitored"))

  pd_narrow <- pd
  pd_narrow$width <- 0.1

  gg <- ggplot(data = df_est_tbl, aes(x = param_interest, group = method, color = method)) +
    geom_hline(yintercept = 0, alpha = 0.5, linetype = 2) +
    geom_errorbar(aes(ymin = lb, ymax = ub), width = 0, position = pd_narrow, alpha = 0.4) +
    geom_point(aes(y = est), position = pd_narrow) +
    fte_theme() + theme(legend.position = "right", panel.grid = element_blank(),
                        axis.text.x = element_text(size = 8)) +
    scale_color_manual(values = c("#E69F00", "#999999", "#56B4E9")) +
    coord_cartesian(ylim = c(-1.5, 0)) +
    xlab("Monitoring Group") +
    ylab("Price Elasticity Estimates")

  ggsave(gg, file = file.path(IMAGES_DIR, "fig_5.png"), width = 6, height = 3, dpi = 300)
  cat("  [GEN] fig_5.png\n"); n_generated <- n_generated + 1
} else {
  cat("  [SKIP] fig_5.png (no CSV)\n"); n_skipped <- n_skipped + 1
}

## ---- fig_2a.png (monitoring score distribution) -----------------------------

fig_2a_csv <- file.path(RF_CSV_DIR, "fig_2a.csv")
if (file.exists(fig_2a_csv)) {
  tm_score_df <- read.csv(fig_2a_csv, stringsAsFactors = FALSE)

  ## fte_theme with base_size=12 to match benchmark (Proj_Tesla fte.R)
  fte12 <- function() {
    palette <- brewer.pal("Greys", n=9)
    theme_bw(base_size=12) +
      theme(panel.background=element_rect(fill=palette[2], color=palette[2])) +
      theme(plot.background=element_rect(fill=palette[2], color=palette[2])) +
      theme(panel.border=element_rect(color=palette[2])) +
      theme(panel.grid.major=element_line(color=palette[3], linewidth=.25)) +
      theme(panel.grid.minor=element_blank()) +
      theme(axis.ticks=element_blank()) +
      theme(legend.position="none") +
      theme(legend.background = element_rect(fill=palette[2])) +
      theme(legend.text = element_text(size=9,color=palette[7])) +
      theme(plot.title=element_text(color=palette[9], size=14, vjust=1.25)) +
      theme(axis.text.x=element_text(size=10,color=palette[6])) +
      theme(axis.text.y=element_text(size=10,color=palette[6])) +
      theme(axis.title.x=element_text(size=11,color=palette[7], vjust=0)) +
      theme(axis.title.y=element_text(size=11,color=palette[7], vjust=1.25)) +
      theme(plot.margin = unit(c(0.35, 0.2, 0.3, 0.35), "cm"))
  }

  gg <- ggplot(tm_score_df, aes(x = log_tm_score)) +
    geom_histogram(aes(y = after_stat(density)), bins = 40, color = FALSE,
                   fill = "turquoise4", alpha = 0.5) +
    fte12() +
    theme(axis.text.y = element_blank(), axis.title.y = element_blank()) +
    coord_cartesian(xlim = c(0, 8)) +
    xlab("Log(Monitoring Score)")

  ggsave(gg, file = file.path(IMAGES_DIR, "fig_2a.png"), width = 5, height = 3, dpi = 300)
  cat("  [GEN] fig_2a.png\n"); n_generated <- n_generated + 1
} else {
  cat("  [SKIP] fig_2a.png (no CSV)\n"); n_skipped <- n_skipped + 1
}

## ---- fig_2b.png (renewal price density) -------------------------------------

fig_2b_csv <- file.path(RF_CSV_DIR, "fig_2b.csv")
if (file.exists(fig_2b_csv)) {
  df_plot <- read.csv(fig_2b_csv, stringsAsFactors = FALSE)

  gg <- ggplot(data = df_plot, aes(group = Mon, fill = Mon)) +
    geom_density(aes(x = p_R_ftr_bench), alpha = 0.3, colour = NA) +
    geom_vline(xintercept = 1, linetype = 2, alpha = 0.3) +
    fte12() +
    theme(legend.position = "right", legend.title = element_blank()) +
    scale_color_manual(values = cbPalette) +
    xlim(0.5, 1.5) +
    xlab("First Period Renewal Price Change (1x = Unmonitored Pool Mean)") +
    ylab("Density")

  ggsave(gg, file = file.path(IMAGES_DIR, "fig_2b.png"), width = 5, height = 3, dpi = 300)
  cat("  [GEN] fig_2b.png\n"); n_generated <- n_generated + 1
} else {
  cat("  [SKIP] fig_2b.png (no CSV)\n"); n_skipped <- n_skipped + 1
}

## ---- fig_3.png (claim cost by monitoring group) -----------------------------

fig_3_csv <- file.path(RF_CSV_DIR, "fig_3.csv")
if (file.exists(fig_3_csv)) {
  tbl_all <- read.csv(fig_3_csv, stringsAsFactors = FALSE)
  tbl_all$group <- factor(tbl_all$group,
    levels = c("opt-out", "opt-in", "1", "2", "3", "4", "5"))

  gg <- ggplot(tbl_all, aes(x = group, y = avg_clm_pct, color = color_grp)) +
    geom_errorbar(aes(ymin = avg_clm_pct - 1.96 * se_clm_pct,
                      ymax = avg_clm_pct + 1.96 * se_clm_pct), width = 0) +
    geom_point(size = 3) +
    geom_hline(yintercept = 1, alpha = 0.3, linetype = "dotted") +
    fte12() +
    theme(legend.position = "none",
          plot.margin = unit(c(0.35, 0.2, 0.3, 0.5), "cm")) +
    scale_color_manual(values = c("benchmark" = cbPalette[1], "quintile" = cbPalette[2])) +
    scale_y_continuous(labels = scales::percent, breaks = c(0.4, 0.6, 0.8, 1.0, 1.2, 1.4)) +
    xlab("Monitoring Groups / Score Quartiles") +
    ylab("Average Claim Count ( / Opt-Out Pool)")

  ggsave(gg, file = file.path(IMAGES_DIR, "fig_3.png"), width = 5, height = 3, dpi = 300)
  cat("  [GEN] fig_3.png\n"); n_generated <- n_generated + 1
} else {
  cat("  [SKIP] fig_3.png (no CSV)\n"); n_skipped <- n_skipped + 1
}

## ---- appendix/fig_a3.png (monitoring discount persistence) ------------------

fig_a3_csv <- file.path(RF_CSV_DIR, "appendix", "fig_a3.csv")
if (file.exists(fig_a3_csv)) {
  df_tm_disc_panel <- read.csv(fig_a3_csv, stringsAsFactors = FALSE)

  gg <- ggplot(data = df_tm_disc_panel, aes(x = RENW_CNT)) +
    geom_errorbar(aes(ymin = mean - 1.96 * se, ymax = mean + 1.96 * se),
                  width = 0, position = pd, alpha = 0.3) +
    geom_line(aes(y = mean), position = pd) +
    geom_point(aes(y = mean), position = pd) +
    fte_theme() +
    scale_color_manual(values = cbPalette) +
    scale_y_continuous(labels = scales::percent) +
    xlab("Renewal Number") +
    ylab("Change in Monitoring Discount\nafter 1st Period (%)")

  ggsave(gg, file = file.path(IMAGES_DIR, "appendix/fig_a3.png"), width = 5, height = 3, dpi = 300)
  cat("  [GEN] appendix/fig_a3.png\n"); n_generated <- n_generated + 1
} else {
  cat("  [SKIP] appendix/fig_a3.png (no CSV)\n"); n_skipped <- n_skipped + 1
}

## ---- appendix/fig_a4.png (claim surcharge) ----------------------------------

fig_a4_csv <- file.path(RF_CSV_DIR, "appendix", "fig_a4.csv")
if (file.exists(fig_a4_csv)) {
  plot_a4 <- read.csv(fig_a4_csv, stringsAsFactors = FALSE)

  gg <- ggplot(data = plot_a4,
               aes(x = tier_acci_drvr_pt, y = clm_srchg, group = tier_good_ind)) +
    geom_smooth(aes(color = "1", fill = "2"), alpha = 0.2, show.legend = FALSE) +
    fte_theme() +
    theme(axis.text.x = element_blank()) +
    scale_color_manual(values = cbPalette[2:length(cbPalette)]) +
    xlab("Existing Violation/Accident Points") +
    ylab("Claim Surcharge (x)")

  ggsave(gg, file = file.path(IMAGES_DIR, "appendix/fig_a4.png"), width = 5, height = 3, dpi = 300)
  cat("  [GEN] appendix/fig_a4.png\n"); n_generated <- n_generated + 1
} else {
  cat("  [SKIP] appendix/fig_a4.png (no CSV)\n"); n_skipped <- n_skipped + 1
}

## ---- appendix/fig_a5.png (informativeness - participation) ------------------

fig_a5_csv <- file.path(CACHE_DIR_APP, "fig_a5_a6.csv")
if (file.exists(fig_a5_csv)) {
  df_info_pct <- read.csv(fig_a5_csv, stringsAsFactors = FALSE)
  # Filter to periods 1-5 (benchmark excludes period 6)
  df_info_pct <- df_info_pct[df_info_pct$pool <= 5, ]
  if (all(is.na(df_info_pct$mfx_ind)) && all(is.na(df_info_pct$mfx_val))) {
    cat("  [SKIP] appendix/fig_a5.png, fig_a6.png (all NA values)\n"); n_skipped <- n_skipped + 2
  } else {
  df_info_pct$pool <- factor(df_info_pct$pool)
  df_info_pct$group <- factor(df_info_pct$group,
    levels = c("w/ ctrl", "claim ctrl", "w/o ctrl"))

  pd_wide <- position_dodge(0.8)

  gg_a5 <- ggplot(df_info_pct, aes(x = pool, group = group, color = group)) +
    geom_errorbar(aes(ymin = mfx_ind - 1.96 * se_ind,
                      ymax = mfx_ind + 1.96 * se_ind), width = 0, position = pd_wide) +
    geom_point(aes(y = mfx_ind), position = pd_wide) +
    geom_hline(yintercept = 0, alpha = 0.3, linetype = "dotted") +
    fte_theme() +
    theme(legend.position = "right", legend.title = element_blank()) +
    scale_color_manual(values = cbPalette) +
    scale_y_continuous(labels = scales::percent) +
    xlab("Renewal Period (after first semester)") +
    ylab("Estimate - Claim Count on Monitoring Finish Ind.")

  ggsave(gg_a5, file = file.path(IMAGES_DIR, "appendix", "fig_a5.png"), width = 5.5, height = 3, dpi = 300)
  cat("  [GEN] appendix/fig_a5.png\n"); n_generated <- n_generated + 1

  ## ---- appendix/fig_a6.png (informativeness - score) --------------------------

  gg_a6 <- ggplot(df_info_pct, aes(x = pool, group = group, color = group)) +
    geom_errorbar(aes(ymin = mfx_val - 1.96 * se_val,
                      ymax = mfx_val + 1.96 * se_val), width = 0, position = pd_wide) +
    geom_point(aes(y = mfx_val), position = pd_wide) +
    geom_hline(yintercept = 0, alpha = 0.3, linetype = "dotted") +
    fte_theme() +
    theme(legend.position = "right", legend.title = element_blank()) +
    scale_color_manual(values = cbPalette) +
    scale_y_continuous(labels = scales::percent) +
    xlab("Renewal Period (after first semester)") +
    ylab("Estimate - Claim Count on Monitoring Score")

  ggsave(gg_a6, file = file.path(IMAGES_DIR, "appendix", "fig_a6.png"), width = 5, height = 3, dpi = 300)
  cat("  [GEN] appendix/fig_a6.png\n"); n_generated <- n_generated + 1
  }  # end all-NA check
} else {
  cat("  [SKIP] appendix/fig_a5.png, fig_a6.png (no CSV)\n"); n_skipped <- n_skipped + 2
}

## ---- appendix/fig_b1a.png (monitoring intro event study) --------------------

fig_b1a_csv <- file.path(RF_CSV_DIR, "appendix", "fig_b1a.csv")
if (file.exists(fig_b1a_csv)) {
  df_trend <- read.csv(fig_b1a_csv, stringsAsFactors = FALSE)
  num_mon_pre <- -4; num_mon_post <- 12

  scaleFUN <- function(x) sprintf("%.0f", x)

  gg <- ggplot(filter(df_trend, mon_since_ubi > num_mon_pre &
                                 mon_since_ubi < num_mon_post),
               aes(x = mon_since_ubi, y = mon_fin_pct)) +
    geom_point() +
    geom_vline(xintercept = -0.5, alpha = 0.3, linetype = "dotted") +
    fte_theme() +
    theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
    scale_color_manual(values = cbPalette) +
    scale_y_continuous(labels = scales::percent) +
    scale_x_continuous(labels = scaleFUN,
                       limits = c(num_mon_pre + 1, num_mon_post),
                       breaks = c(num_mon_pre + 1, -1, 0, num_mon_post)) +
    xlab("Months Since Introduction") +
    ylab("Monitoring Finish Rate")

  ggsave(gg, file = file.path(IMAGES_DIR, "appendix", "fig_b1a.png"), width = 5, height = 3, dpi = 300)
  cat("  [GEN] appendix/fig_b1a.png\n"); n_generated <- n_generated + 1
} else {
  cat("  [SKIP] appendix/fig_b1a.png (no CSV)\n"); n_skipped <- n_skipped + 1
}

## ---- appendix/fig_b1b.png (RD effect) --------------------------------------

fig_b1b_csv <- file.path(CACHE_DIR_APP, "fig_b1b.csv")
if (file.exists(fig_b1b_csv)) {
  result_tbl <- read.csv(fig_b1b_csv, stringsAsFactors = FALSE)
  result_tbl$ctrl <- factor(result_tbl$ctrl, levels = c("Raw", "X w/o Trend", "X Control"))
  result_tbl$reg <- factor(result_tbl$reg, levels = c("Premium", "Claim"))

  pd_b1b <- position_dodge(0.8)

  gg <- ggplot(result_tbl, aes(x = reg, group = ctrl, color = ctrl)) +
    geom_errorbar(aes(ymin = est_lb, ymax = est_ub), width = 0, position = pd_b1b) +
    geom_point(aes(y = est), position = pd_b1b) +
    geom_hline(yintercept = 0, alpha = 0.3, linetype = "dotted") +
    fte_theme() +
    theme(legend.position = "right", legend.title = element_blank(),
          axis.title.x = element_blank()) +
    scale_color_manual(values = cbPalette) +
    scale_y_continuous(labels = scales::percent) +
    ylab("% Change at Monitoring Introduction")

  ggsave(gg, file = file.path(IMAGES_DIR, "appendix", "fig_b1b.png"), width = 5, height = 3, dpi = 300)
  cat("  [GEN] appendix/fig_b1b.png\n"); n_generated <- n_generated + 1
} else {
  cat("  [SKIP] appendix/fig_b1b.png (no CSV)\n"); n_skipped <- n_skipped + 1
}

## ---- appendix/fig_b2a.png (score density by regime) -------------------------

fig_b2a_csv <- file.path(RF_CSV_DIR, "appendix", "fig_b2a.csv")
if (file.exists(fig_b2a_csv)) {
  df_score <- read.csv(fig_b2a_csv, stringsAsFactors = FALSE)
  df_score$scheme <- factor(df_score$scheme)

  gg <- ggplot(df_score, aes(x = score, colour = scheme)) +
    geom_density() +
    fte_theme() +
    theme(legend.position = "right") +
    xlab("score") +
    ylab("density")

  ggsave(gg, file = file.path(IMAGES_DIR, "appendix/fig_b2a.png"), width = 5, height = 3, dpi = 300)
  cat("  [GEN] appendix/fig_b2a.png\n"); n_generated <- n_generated + 1
} else {
  cat("  [SKIP] appendix/fig_b2a.png (no CSV)\n"); n_skipped <- n_skipped + 1
}

## ---- appendix/fig_b2b.png (score-discount mapping) -------------------------

fig_b2b_csv <- file.path(RF_CSV_DIR, "appendix", "fig_b2b.csv")
if (file.exists(fig_b2b_csv)) {
  gg_df <- read.csv(fig_b2b_csv, stringsAsFactors = FALSE)
  gg_df$scheme <- factor(gg_df$scheme)

  gg <- ggplot(gg_df, aes(x = score, group = scheme, color = scheme)) +
    geom_errorbar(aes(ymin = lb, ymax = ub)) +
    geom_point(aes(y = est)) +
    fte_theme() +
    theme(legend.position = "right") +
    xlab("score") +
    ylab("Monitoring Renewal Factor")

  ggsave(gg, file = file.path(IMAGES_DIR, "appendix/fig_b2b.png"), width = 5, height = 3, dpi = 300)
  cat("  [GEN] appendix/fig_b2b.png\n"); n_generated <- n_generated + 1
} else {
  cat("  [SKIP] appendix/fig_b2b.png (no CSV)\n"); n_skipped <- n_skipped + 1
}

## ---- appendix/fig_c1.png (MH progression, balanced) ------------------------

fig_c1_csv <- file.path(CACHE_DIR_APP, "fig_c1.csv")
if (file.exists(fig_c1_csv)) {
  vis_tbl <- read.csv(fig_c1_csv, stringsAsFactors = FALSE)

  pd_narrow <- pd
  pd_narrow$width <- 0.1

  est_mon <- vis_tbl$est[vis_tbl$period == 0 & vis_tbl$group == "opt-out"]
  se_mon <- (vis_tbl$ub[vis_tbl$period == 0 & vis_tbl$group == "opt-out"] - est_mon) / 1.96

  gg <- ggplot(data = vis_tbl, aes(x = period, group = group, color = group)) +
    geom_hline(yintercept = 0, alpha = 0.1) +
    geom_hline(yintercept = est_mon, alpha = 0.1) +
    geom_errorbar(aes(ymin = lb, ymax = ub), width = 0.1, position = pd_narrow, alpha = 0.4) +
    geom_point(aes(y = est), position = pd_narrow) +
    geom_line(aes(y = est), position = pd_narrow, size = 0.5, alpha = 0.5) +
    fte_theme() +
    theme(legend.position = "right", legend.title = element_blank(),
          panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank()) +
    scale_color_manual(values = c("#999999", "#E69F00", "#56B4E9")) +
    annotate("rect", xmin = -0.5, xmax = 0.5,
             ymin = est_mon - 3 * se_mon, ymax = est_mon + 3 * se_mon,
             fill = NA, colour = "red", linetype = "dotted") +
    annotate("text", x = 0, y = est_mon - 2.75 * se_mon, colour = "red", label = "monitored") +
    xlab("Period") +
    ylab("Claim Count Differentials w/ Controls")

  ggsave(gg, file = file.path(IMAGES_DIR, "appendix", "fig_c1.png"), width = 10, height = 5, dpi = 300)
  cat("  [GEN] appendix/fig_c1.png\n"); n_generated <- n_generated + 1
} else {
  cat("  [SKIP] appendix/fig_c1.png (no CSV)\n"); n_skipped <- n_skipped + 1
}

## ---- appendix/fig_c2.png (MH het across observables) -----------------------

fig_c2_csv <- file.path(CACHE_DIR_APP, "fig_c2.csv")
if (file.exists(fig_c2_csv)) {
  df_graph <- read.csv(fig_c2_csv, stringsAsFactors = FALSE)
  df_graph$dimension <- factor(df_graph$dimension, ordered = TRUE, levels = df_graph$dimension)

  gg <- ggplot(df_graph, aes(x = dimension, y = est_het, color = stat_sig)) +
    geom_errorbar(aes(ymin = est_het_lb, ymax = est_het_ub), width = 0, position = pd, alpha = 0.7) +
    geom_point(position = pd) +
    fte_theme() + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
    scale_color_manual(values = cbPalette) +
    xlab("Dimensions of Heterogeneity") +
    ylab("Moral Hazard Heterogeneity\n(Interaction Term Estimates)")

  ggsave(gg, file = file.path(IMAGES_DIR, "appendix", "fig_c2.png"), width = 10, height = 5, dpi = 300)
  cat("  [GEN] appendix/fig_c2.png\n"); n_generated <- n_generated + 1
} else {
  cat("  [SKIP] appendix/fig_c2.png (no CSV)\n"); n_skipped <- n_skipped + 1
}

## ---- appendix/fig_c3.png (MH het across coverage) --------------------------

fig_c3_csv <- file.path(CACHE_DIR_APP, "fig_c3.csv")
if (file.exists(fig_c3_csv)) {
  df_graph <- read.csv(fig_c3_csv, stringsAsFactors = FALSE)
  df_graph$dimension <- factor(df_graph$dimension, ordered = TRUE, levels = df_graph$dimension)

  gg <- ggplot(df_graph, aes(x = dimension, y = est_het, color = stat_sig)) +
    geom_errorbar(aes(ymin = est_het_lb, ymax = est_het_ub), width = 0, position = pd, alpha = 0.7) +
    geom_point(position = pd) +
    fte_theme() + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
    scale_color_manual(values = cbPalette) +
    xlab("Dimensions of Heterogeneity") +
    ylab("Moral Hazard Heterogeneity\n(Interaction Term Estimates)")

  ggsave(gg, file = file.path(IMAGES_DIR, "appendix", "fig_c3.png"), width = 10, height = 5, dpi = 300)
  cat("  [GEN] appendix/fig_c3.png\n"); n_generated <- n_generated + 1
} else {
  cat("  [SKIP] appendix/fig_c3.png (no CSV)\n"); n_skipped <- n_skipped + 1
}

## ---- appendix/fig_c4.png (AAF violations) -----------------------------------

fig_c4_csv <- file.path(CACHE_DIR_APP, "fig_c4.csv")
if (file.exists(fig_c4_csv)) {
  vis_tbl <- read.csv(fig_c4_csv, stringsAsFactors = FALSE)

  # Ensure period column is numeric (JSON-derived CSVs already have it)
  if ("year" %in% names(vis_tbl) && !"period" %in% names(vis_tbl)) {
    vis_tbl$period <- match(vis_tbl$year, c("pre-period\naggregate", "1", "2", "3")) - 1
  }
  # Ensure segment_group exists (for line grouping)
  if (!"segment_group" %in% names(vis_tbl)) {
    vis_tbl$segment_group <- paste(vis_tbl$group,
                                   ifelse(vis_tbl$period < 1, "pre", "post"))
  }

  pd_narrow <- pd
  pd_narrow$width <- 0.1

  est1 <- vis_tbl$est[vis_tbl$period == 0 & vis_tbl$group == "monitored"]

  gg <- ggplot(data = vis_tbl, aes(x = period, group = segment_group, color = group)) +
    geom_hline(yintercept = 0, alpha = 0.1) +
    geom_hline(yintercept = est1, alpha = 0.1) +
    geom_errorbar(aes(ymin = lb, ymax = ub), width = 0, position = pd_narrow, alpha = 0.4) +
    geom_point(aes(y = est), position = pd_narrow) +
    geom_line(aes(y = est), position = pd_narrow, size = 0.5, alpha = 0.5) +
    fte_theme() + theme(legend.title = element_blank(), legend.position = "right") +
    scale_color_manual(values = c("#E69F00", "#999999", "#56B4E9")) +
    scale_x_continuous(
      breaks = c(0, 1, 2, 3),
      labels = c("pre-period\naggregate", "1", "2", "3")
    ) +
    xlab("Year") +
    ylab("Residual AAF (After Controls)")

  ggsave(gg, file = file.path(IMAGES_DIR, "appendix", "fig_c4.png"), width = 6, height = 3, dpi = 300)
  cat("  [GEN] appendix/fig_c4.png\n"); n_generated <- n_generated + 1
} else {
  cat("  [SKIP] appendix/fig_c4.png (no CSV)\n"); n_skipped <- n_skipped + 1
}

################################################################################
## RF TABLES
################################################################################

## ---- tab_1.tex (summary statistics) -----------------------------------------

tab_1a_csv <- file.path(RF_CSV_DIR, "tab_1_panel_a.csv")
tab_1b_csv <- file.path(RF_CSV_DIR, "tab_1_panel_b.csv")
tab_1_meta_csv <- file.path(RF_CSV_DIR, "tab_1_meta.csv")

if (file.exists(tab_1a_csv) && file.exists(tab_1b_csv) && file.exists(tab_1_meta_csv)) {
  tab_full <- read.csv(tab_1a_csv, stringsAsFactors = FALSE)
  panel_b <- read.csv(tab_1b_csv, stringsAsFactors = FALSE)
  tab_meta <- read.csv(tab_1_meta_csv, stringsAsFactors = FALSE)

  bigN <- tab_meta$value[tab_meta$key == "bigN"]
  bigI <- tab_meta$value[tab_meta$key == "bigI"]
  bigN_b <- tab_meta$value[tab_meta$key == "bigN_b"]

  ## Formatting helpers
  fmt_num <- function(x, digits = 2) formatC(x, format = "f", big.mark = ",", digits = digits)
  fmt_int <- function(x) formatC(x, format = "d", big.mark = ",")
  int_like <- function(v) all(is.finite(v)) && all(abs(v - round(v)) < 1e-12)

  display <- tab_full %>%
    mutate(
      Mean = if (int_like(Mean)) fmt_int(round(Mean)) else fmt_num(Mean, 2),
      SD   = if (int_like(SD))   fmt_int(round(SD))   else fmt_num(SD, 2),
      Min  = if (int_like(Min))  fmt_int(round(Min))  else fmt_num(Min, 2),
      p50  = if (int_like(p50))  fmt_int(round(p50))  else fmt_num(p50, 2),
      p75  = if (int_like(p75))  fmt_int(round(p75))  else fmt_num(p75, 2),
      Max  = if (int_like(Max))  fmt_int(round(Max))  else fmt_num(Max, 2)
    ) %>%
    select(-.N_)

  foot_txt <- paste0(
    "This table reports summary statistics of our main panel data (all 23 states, full unbalanced panel). ",
    "Risk class is the actuarially-fair premium derived by the company's actuarial team for liability coverage. ",
    "$N= ", bigN, "$."
  )

  ## Build panel (a) body
  addlinespace_before <- c("Total claim count", "Total claim", "Liability coverage limit", "Tenure")
  panel_a_body <- ""
  for (i in seq_len(nrow(display))) {
    if (any(sapply(addlinespace_before, function(k) grepl(k, display$Variable[i], fixed = TRUE)))) {
      panel_a_body <- paste0(panel_a_body, "\\addlinespace\n")
    }
    row_vals <- paste(as.character(display[i, ]), collapse = " & ")
    panel_a_body <- paste0(panel_a_body, row_vals, "\\\\\n")
  }

  ## Build panel (b) from CSV
  f2  <- function(x) sprintf("%.2f", x)
  f1  <- function(x) sprintf("%.1f", x)
  fi0 <- function(x) sprintf("%d", as.integer(round(x)))

  row_line <- function(lbl, nums, tail = NULL) {
    paste0(lbl, " & ", paste(nums, collapse = " & "),
           if (length(tail)) paste0("  & ", paste(tail, collapse = " & ")) else "",
           " \\\\")
  }

  quotes <- panel_b[panel_b$section == "quotes", ]
  cov_sh <- panel_b[panel_b$section == "coverage_share" & !grepl("claim", panel_b$firm, ignore.case = TRUE), ]
  avg_cl <- panel_b[panel_b$section == "coverage_share" & grepl("claim.*\\$", panel_b$firm), ]
  avg_cc <- panel_b[panel_b$section == "claim_cnt", ]

  hdr <- paste0(
    "\\resizebox{\\linewidth}{!}{%\n",
    "\\begin{tabular}[t]{lrrrrrrr}\n",
    "\\multicolumn{8}{c}{(b) By Coverage and Firm (focal state)}\\\\\n",
    "\\toprule\n",
    "  & \\multicolumn{5}{c}{Liability coverage (\\$000)} & \\multicolumn{2}{c}{Market Share}\\\\ \n",
    " \\cmidrule(lr){2-6} \\cmidrule(lr){7-8} \n",
    "& 40 & 50 & 100 & 150 & 300 & Raw & Direct \\\\ \n",
    "\\midrule\n",
    "\\multicolumn{8}{l}{Average Quotes (\\$)}\\\\\n"
  )

  lines_quotes <- sapply(seq_len(nrow(quotes)), function(i) {
    row_line(quotes$firm[i],
             f2(c(quotes$cov_40[i], quotes$cov_50[i], quotes$cov_100[i],
                  quotes$cov_150[i], quotes$cov_300[i])),
             c(f1(quotes$mkt_raw[i]), f1(quotes$mkt_direct[i])))
  })

  cov_vals <- fi0(c(cov_sh$cov_40, cov_sh$cov_50, cov_sh$cov_100, cov_sh$cov_150, cov_sh$cov_300))
  lines_covshare <- c(
    "\\addlinespace\n\\multicolumn{8}{l}{Coverage share}\\\\",
    row_line("at monitoring firm (\\%)", cov_vals, c("", ""))
  )

  ## avg_cl and avg_cc already set above from CSV section names
  lines_claims <- c(
    "\\addlinespace\nAverage liability claim (\\$) & ",
    paste(paste0(f2(c(avg_cl$cov_40, avg_cl$cov_50, avg_cl$cov_100, avg_cl$cov_150, avg_cl$cov_300)),
                 collapse = " & "), " &  &  \\\\\n"),
    "Average liability claim count & ",
    paste(paste0(f2(c(avg_cc$cov_40, avg_cc$cov_50, avg_cc$cov_100, avg_cc$cov_150, avg_cc$cov_300)),
                 collapse = " & "), " &  &  \\\\\n"),
    "\\bottomrule\n"
  )

  notes <- paste0("\\end{tabular}\n}\n")

  latex_block_b <- paste0(
    hdr,
    paste0(lines_quotes, collapse = "\n"), "\n",
    paste0(lines_covshare, collapse = "\n"), "\n",
    paste0(lines_claims, collapse = ""),
    notes
  )

  combined_tex <- paste0(
    "\\begin{table}[htbp!]\n\\centering\n\\begin{threeparttable}\n",
    "\\caption{Summary Statistics\\label{tab:sum_stat}}\n\\small\n",
    "\\begin{minipage}{\\textwidth}\n\\centering\n\n",
    "% --- Panel (a) ---\n",
    "% \\resizebox{\\linewidth}{!}{%\n",
    "\\begin{tabular}[t]{lrrrrrr}\n\\\\\n",
    "\\multicolumn{7}{c}{(a) Premium, Coverage and Claims (All States, Per 6-month Period)}\\\\\n",
    "\\toprule\n",
    " & Mean & SD & Min & p50 & p75 & Max\\\\\n",
    "\\midrule\n",
    panel_a_body,
    "\\bottomrule\n\\end{tabular}\n",
    "% }\n\n"
  )

  combined_tex <- paste0(combined_tex,
    "\\vspace{2em}\n\n",
    "% --- Panel (b) ---\n",
    latex_block_b, "\n",
    "\\end{minipage}\n\n",
    "\\begin{tablenotes}\n\\footnotesize\n",
    "\\item (a) \\textit{Note: }", foot_txt, "\n",
    "\\item (b) \\textit{Note: }This table reports the average quotes and claims of the monitoring firm and its top 5 competitors by market share in our focal state. \n",
    "The total number of consumers is ", bigI, ". The total number of quotes is ", bigN_b, ".\n",
    "In the focal state, the mandatory minimum and the most popular coverage changed from \\$40,000 to \\$50,000 during the research window, which is why the former had lower share and slightly lower claims. \n",
    "Competitor 3 experienced significant premium reduction over time, which explains why its \\$40,000 plan is more expensive on average than its \\$50,000 plan. \n",
    "The market share is based on each firm's aggregate liability premiums written over our research window, obtained from the National Association of Insurance Commissioners' annual reports.\n",
    "\\par\n",
    "\\end{tablenotes}\n\n\\end{threeparttable}\n\\end{table}"
  )

  writeLines(combined_tex, file.path(TABLES_DIR, "tab_1.tex"))
  cat("  [GEN] tab_1.tex\n"); n_generated <- n_generated + 1
} else {
  cat("  [SKIP] tab_1.tex (missing CSVs)\n"); n_skipped <- n_skipped + 1
}

## ---- tab_3.tex (selection / first-period claim) from regression JSON --------

j_tab3 <- read_json_coefs(file.path(RF_REG_DIR, "tab_3_regression.json"))
if (!is.null(j_tab3) && !is.null(j_tab3$models)) {
  fmt_coef <- function(x) sprintf("%.3f", x)
  fmt_se <- function(x) sprintf("(%.3f)", x)
  stars <- function(p) ifelse(p < 0.01, "$^{***}$", ifelse(p < 0.05, "$^{**}$", ifelse(p < 0.1, "$^{*}$", "")))

  models <- j_tab3$models
  post_mean <- j_tab3$post_period_mean

  get_cell <- function(m, var) {
    c <- m$coefficients[[var]]
    if (is.null(c)) return(list(est = "", se = ""))
    list(est = paste0(fmt_coef(c$estimate), stars(c$p_value)),
         se = fmt_se(c$std_error))
  }

  ubi <- lapply(models, function(m) get_cell(m, "ubi_ind"))
  intercept <- lapply(models, function(m) get_cell(m, "(Intercept)"))
  ns <- if (!is.null(j_tab3$n_values)) {
    sapply(j_tab3$n_values, function(n) format(n, big.mark = ","))
  } else {
    sapply(models, function(m) if (!is.null(m$n)) format(m$n, big.mark = ",") else "")
  }
  risk_pct <- sapply(models, function(m) {
    ubi_est <- m$coefficients$ubi_ind$estimate
    sprintf("%.2f\\%%", ubi_est / post_mean * 100)
  })

  tex <- paste0(
    "\\begin{table}[ht] \\begin{centering}\n",
    "\t\t\\caption{First-Period Claim Comparison Across Monitoring Groups} \n",
    "\t\t\\label{tab:mon_unmon_t0} \n",
    "\t\t%\\vspace{0.5em}\n",
    "\t\t\\begin{tabular}{@{\\extracolsep{5pt}}lccc} \n",
    "\\\\[-1.8ex]\\hline \n",
    "\\hline \\\\[-1.8ex] \n",
    " & \\multicolumn{3}{c}{Dependent variable: claim count ($C$)} \\\\ \n",
    "\\cline{2-4} \n",
    "\\\\[-1.8ex] & \\multicolumn{1}{c}{(1)} & \\multicolumn{1}{c}{(2)} & \\multicolumn{1}{c}{(3)}\\\\ \n",
    "\\hline \\\\[-1.8ex] \n",
    " monitoring indicator ($m$) & ", paste(sapply(ubi, `[[`, "est"), collapse = " & "), " \\\\ \n",
    "  & ", paste(sapply(ubi, `[[`, "se"), collapse = " & "), " \\\\ \n",
    "  Constant & ", paste(sapply(intercept, `[[`, "est"), collapse = " & "), " \\\\ \n",
    "  & ", paste(sapply(intercept, `[[`, "se"), collapse = " & "), " \\\\ \n",
    " \\hline \\\\[-1.8ex] \n",
    "observables controls ($x$) & No & Yes & Yes \\\\ \n",
    "coverage fixed effects & No & No & Yes \\\\ \n",
    "risk reduction (\\%) & ", paste(risk_pct, collapse = " & "), " \\\\ \n",
    "Observations & ", paste(paste0("\\multicolumn{1}{c}{", ns, "}"), collapse = " & "), " \\\\ \n",
    "\\hline \n",
    "\\hline \\\\[-1.8ex] \n",
    "\\end{tabular}\n",
    "\\par\\end{centering}\n",
    "%\\vspace{0.5cm}\n",
    "\\begin{singlespace}\n",
    "\t{\\footnotesize \\emph{Notes: }This table reports the results of a regression where the dependent variable is first-period claim count, and the independent variables are the monitoring indicator and controls. Mirroring Table \\ref{tab:mh_results} columns (1) to (3), the monitoring group consists of all monitoring finishers. %This variable is consistent with the monitoring indicator in the incentive effect regression (\\ref{eq:mh_reg}) (Table \\ref{tab:mh_results}), so as to facilitate comparison and decomposition. \n",
    "    $^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01 \\par}\n",
    "\\end{singlespace}\n",
    "\\end{table}\n"
  )

  writeLines(tex, file.path(TABLES_DIR, "tab_3.tex"))
  cat("  [GEN] tab_3.tex\n"); n_generated <- n_generated + 1
} else {
  cat("  [SKIP] tab_3.tex (no JSON)\n"); n_skipped <- n_skipped + 1
}

## ---- tab_2.tex and tab_c1.tex (MH DID regression, sidewaystable) ------------

generate_mh_did_table <- function(json_path, dest_path, label, caption_label) {
  j <- read_json_coefs(json_path)
  if (is.null(j) || is.null(j$models)) return(FALSE)

  fmt_coef <- function(x) sprintf("%.3f", x)
  fmt_se <- function(x) sprintf("(%.3f)", x)
  stars <- function(p) {
    if (is.na(p)) return("")
    if (p < 0.01) "***" else if (p < 0.05) "**" else if (p < 0.1) "*" else ""
  }

  get_cell <- function(m, var) {
    c <- m$coefficients[[var]]
    if (is.null(c)) return(list(est = "", se = ""))
    p <- if (!is.null(c$p_value) && length(c$p_value) > 0) c$p_value else NA
    se_str <- if (!is.null(c$std_error) && length(c$std_error) > 0) fmt_se(c$std_error) else ""
    list(est = paste0(fmt_coef(c$estimate), stars(p)), se = se_str)
  }

  # Column order: 1-3 indicator, 4 ind driver FE, 5-7 intensity, 8 int driver FE, 9-11 placebo
  m_order <- c("ind_no_ctrl", "ind_X_ctrl", "ind_XY_ctrl", "ind_driver_fe",
               "int_no_ctrl", "int_X_ctrl", "int_XY_ctrl", "int_driver_fe",
               "placebo_1", "placebo_2", "placebo_3")
  models <- j$models[m_order]

  coef_row <- function(label_tex, var) {
    cells <- character(11)
    se_cells <- character(11)
    for (i in 1:11) {
      cc <- get_cell(models[[i]], var)
      cells[i] <- cc$est
      se_cells[i] <- cc$se
    }
    est_line <- paste0(label_tex, " & ", paste(cells, collapse = " & "), " \\\\")
    se_line <- paste0("& ", paste(se_cells, collapse = " & "), " \\\\")
    paste(est_line, se_line, sep = "\n")
  }

  # N values from JSON (pre-computed from benchmark)
  ns <- if (!is.null(j$n_values)) {
    sapply(j$n_values, function(n) format(n, big.mark = ","))
  } else {
    rep("", 11)
  }

  mh_pct <- j$implied_mh_pct
  pct_cells <- character(11)
  if (!is.null(mh_pct) && length(mh_pct) >= 11) {
    for (i in 1:11) pct_cells[i] <- if (i %in% c(4,8)) "" else sprintf("%.2f\\%%", mh_pct[i] * 100)
  }

  tex <- paste0(
    "\\begin{sidewaystable}[!htbp]\n\n",
    "    \\begin{centering}\n",
    "    \\caption{\\textbf{Estimation Results: Moral Hazard Effect \\label{", caption_label, "}}}\n",
    "    \\vspace{0.5em}\n",
    "    \\begin{adjustbox}{max width=\\textwidth}\n",
    "\\small\n",
    "\\begin{tabular}{llllllllllll}\n",
    "\\hline\n",
    "& \\multicolumn{11}{c}{\\emph{dependent variable}: claim count ($C$)}\\\\\n",
    "\\cmidrule{2-12}\n",
    "\\emph{explanatory variables} & \\multicolumn{1}{c}{(1)} & \\multicolumn{1}{c}{(2)} & \\multicolumn{1}{c}{(3)} & \\multicolumn{1}{c}{(4)} & \\multicolumn{1}{c}{(5)} & \\multicolumn{1}{c}{(6)} & \\multicolumn{1}{c}{(7)} & \\multicolumn{1}{c}{(8)} & \\multicolumn{3}{c}{Parallel Trend/Placebo}\\\\\n",
    "\\midrule\n",
    coef_row("constant", "(Intercept)"), "\n",
    coef_row("monitoring indicator ($m$)", "ubi_ind"), "\n",
    coef_row("post monitoring indicator ($\\mathbf{1}_{post}$)", "post_ind"), "\n",
    coef_row("monitoring duration ($z$)", "treat_int"), "\n",
    coef_row("interaction ($\\mathbf{1}_{post}$ $\\times$ $m$)", "ubi_ind:post_ind"), "\n",
    coef_row("interaction ($\\mathbf{1}_{post}$ $\\times$ $z$)", "post_ind:treat_int"), "\n",
    "\\addlinespace\\hline &  &  &  &  &  &  &  &  &  &  &  \\\\\n",
    "observables controls ($x$) & No & Yes & Yes & No & No & Yes & Yes & No & Yes & Yes & Yes \\\\\n",
    "coverage fixed effects & No & No & Yes & No & No & No & Yes & No & Yes & Yes & Yes \\\\\n",
    "driver fixed effects & No & No & No & Yes & No & No & No & Yes & No & No & No \\\\\n",
    "implied moral hazard effect (\\%) & ", paste(pct_cells, collapse = " & "), " \\\\\n",
    "pre / post periods - ``1st diff'' & $t=0/1-2$ & $t=0/1-2$ & $t=0/1-2$ & $t=0/1-2$ & $t=0/1-2$ & $t=0/1-2$ & $t=0/1-2$ & $t=0/1-2$ & $t=1/t=2$ & $t=2/t=3$ & $t=3/t=4$ \\\\\n",
    "treatment group - ``2nd diff'' & finishers & finishers & finishers & finishers & all monitored & all monitored & all monitored & all monitored & finishers & finishers & finishers \\\\\n",
    "$N$ & ", paste(ns, collapse = " & "), " \\\\\n",
    "\\hline\n",
    "\\end{tabular}\n",
    "\\end{adjustbox}\n",
    "\\par\\end{centering}\n\n",
    "    \\vspace{0.5em}\n\n",
    "\t  {\\footnotesize \\emph{Notes:} This table reports results of \\eqref{mh_reg}. The datasets consists of users that are eligible for monitoring and have stayed throughout the pre / post periods (balanced panel). Relative to Columns (1) to (4) removes all drivers that have started monitoring but have not finished. \n",
    "\t  The estimate on the interaction term ($\\mathbf{1}_{post}$ $\\times$ $m$ or $z$) \n",
    "\t  measures the ``treatment effect'' of monitoring ending on claim count across periods. \n",
    "\t  We first balance our panel data to include all drivers who stay until the end of the third semester ($t=3$). \n",
    "\t  This gives us two renewal semesters ($t\\in\\{1,2\\}$) after the monitoring semester ($t=0$). \n",
    "\t  We control for a full set of observables, including driver and vehicle characteristics and tiers\n",
    "\t  (past records of violations or claims). Continuous observable characteristics are normalized. \n",
    "\t  We report estimates with and without these controls. Columns (3) and (6) are our main specification. \n",
    "\t  Column (3) focuses on monitored drivers who finished within the first period, while Column (6) \n",
    "\t  introduces additional variation in monitoring duration and timing and looks at all monitoring finishers. \n",
    "\t  Columns (1,2,4,5) show robustness of our estimates to observable and coverage fixed-effect controls. \n",
    "\t  The right-most columns are placebo tests for parallel trends among treatment/control groups after monitoring \n",
    "\t  ends. We first try to detect a similar change from $t=1$ to $t=2$. We drop all observations from period 0, \n",
    "\t  and roll the post-period cutoff one period forward, so that $\\text{1}_{post,t}=1\\iff t\\geq2$ (changed from $t\\geq1$). \n",
    "\t  Naturally, we look at the future trends of monitored drivers who finished within the first semester and \n",
    "\t  drop other monitored finishers. We find similar results by repeating this test in subsequent periods. \n",
    "\t  As we need to balance panels, number of drivers drop in these tests. \\par}\n",
    "    \\end{sidewaystable}\n"
  )

  writeLines(tex, dest_path)
  return(TRUE)
}

if (generate_mh_did_table(file.path(RF_REG_DIR, "tab_2_regression.json"),
                           file.path(TABLES_DIR, "tab_2.tex"),
                           "tab_2", "tab:mh_results")) {
  cat("  [GEN] tab_2.tex\n"); n_generated <- n_generated + 1
} else {
  cat("  [SKIP] tab_2.tex (no JSON)\n"); n_skipped <- n_skipped + 1
}

if (generate_mh_did_table(file.path(RF_REG_DIR, "appendix/tab_c1_regression.json"),
                           file.path(APPENDIX_DIR, "tab_c1.tex"),
                           "tab_c1", "tab:mh_results_unbal")) {
  cat("  [GEN] appendix/tab_c1.tex\n"); n_generated <- n_generated + 1
} else {
  cat("  [SKIP] appendix/tab_c1.tex (no JSON)\n"); n_skipped <- n_skipped + 1
}

## ---- appendix/tab_a1.tex (Tab A.1 + A.2: observable characteristics) --------

tab_a1a2_json <- file.path(RF_CSV_DIR, "appendix", "tab_a1a2_summary.json")
if (file.exists(tab_a1a2_json)) {
  j <- jsonlite::fromJSON(tab_a1a2_json)

  # Helper: build one sidewaystable from a scope's data
  build_x_sumstat_table <- function(scope_data, caption, label, notes_purpose, age_mean) {
    pa <- scope_data$panel_a
    pb <- scope_data$panel_b
    n_obs <- scope_data$n

    # Panel A: 3-column layout of "Name (mean)" strings, sorted alphabetically
    binary_strings <- sprintf("%s (%.2f)", pa$name, pa$mean)
    n_bin <- length(binary_strings)
    n_rows_a <- ceiling(n_bin / 3)
    padded <- c(binary_strings, rep("", n_rows_a * 3 - n_bin))
    mat_a <- matrix(padded, nrow = n_rows_a, ncol = 3, byrow = FALSE)
    panel_a_rows <- apply(mat_a, 1, function(r) paste(r, collapse = " & "))
    panel_a_tex <- paste0(
      "\\begin{tabular}{lll}\n",
      "\\multicolumn{3}{c}{\\textbf{Panel A: Binary Indicators (Mean)}} \\\\\n",
      "\\hline\n",
      paste(panel_a_rows, collapse = " \\\\\n"), " \\\\\n",
      "\\hline\n",
      "\\end{tabular}"
    )

    # Panel B: continuous variables with conditional formatting
    panel_b_header <- "Variable & Mean & Std. Dev. & Min & $P_{25}$ & $P_{50}$ & $P_{75}$ & Max"
    panel_b_rows <- vapply(seq_len(nrow(pb)), function(i) {
      r <- pb[i, ]
      is_int <- isTRUE(r$is_integer)
      fmt_ms <- function(x) sprintf("%.2f", x)
      fmt_rest <- if (is_int) function(x) sprintf("%.0f", x) else function(x) sprintf("%.2f", x)
      paste(r$name, "&", fmt_ms(r$mean), "&", fmt_ms(r$sd), "&",
            fmt_rest(r$min), "&", fmt_rest(r$p25), "&", fmt_rest(r$p50), "&",
            fmt_rest(r$p75), "&", fmt_rest(r$max))
    }, character(1))
    panel_b_tex <- paste0(
      "\\begin{tabular}{lccccccc}\n",
      "\\multicolumn{8}{c}{\\textbf{Panel B: Continuous Variables}} \\\\\n",
      "\\hline\n",
      panel_b_header, " \\\\\n",
      "\\hline\n",
      paste(panel_b_rows, collapse = " \\\\\n"), " \\\\\n",
      "\\hline\n",
      "\\end{tabular}"
    )

    age_note <- if (!is.na(age_mean) && age_mean > 0) {
      sprintf(" For instance, drivers' age (mean %.2f) is removed as a result.", age_mean)
    } else ""

    notes_tex <- paste0(
      "\\vspace{0.5em}\n",
      "\\begin{flushleft}\n",
      "\\footnotesize\n",
      "\\textit{Notes:} This table presents summary statistics for the observable characteristics used in our ",
      notes_purpose, ". ",
      "Panel A shows the means for binary indicator variables. ",
      "``Avail.'' is short for ``Available'', and ``Ind.'' for ``Indicator''. ",
      "Panel B provides detailed distributional statistics for continuous variables. ",
      "``Cat.'' represents categorical variables that are coarsened and discretized by segmenting the raw continuous variable. ",
      "Zipcode income is winsorized at the 1st and 99th percentiles. ",
      "This table includes all new customers that had single-driver-single-vehicle policies, with $N = ",
      format(n_obs, big.mark = ","), "$. ",
      "To mitigate multicollinearity, we compute variance inflation factors (VIFs) by regressing drivers' risk class on all other observables and exclude variables with VIFs exceeding 5.",
      age_note, "\n",
      "\\end{flushleft}"
    )

    paste0(
      "\\begin{sidewaystable}[htbp] \n",
      "\\centering\n",
      "\\caption{", caption, "}\n",
      "\\label{", label, "}\n\n",
      panel_a_tex, "\n\n",
      "\\vspace{1em}\n\n",
      panel_b_tex, "\n",
      notes_tex, "\n\n",
      "\\end{sidewaystable}"
    )
  }

  us_tex <- build_x_sumstat_table(j$us,
    "Summary Statistics on Observable Characteristics", "tab:sum_stat_x",
    "reduced-form analysis", j$us$age_mean)
  il_tex <- build_x_sumstat_table(j$il,
    "Summary Statistics on Observable Characteristics (focal state)", "tab:sum_stat_x_il",
    "structural estimation and counterfactual simulations", j$il$age_mean)

  combined <- paste0(us_tex, "\n\n", il_tex, "\n")
  writeLines(combined, file.path(APPENDIX_DIR, "tab_a1.tex"))
  cat("  [GEN] appendix/tab_a1.tex\n"); n_generated <- n_generated + 1
} else {
  cat("  [SKIP] appendix/tab_a1.tex (no JSON)\n"); n_skipped <- n_skipped + 1
}

################################################################################
## SUMMARY
################################################################################

cat(sprintf(
  "\ngenerate_exhibits.R completed: %d generated, %d skipped.\n",
  n_generated, n_skipped
))

################################################################################
## Paper compilation moved to run_all.R (after c3_get_fitctf_exhibits.R)
################################################################################

if (FALSE) { # Disabled — compilation now in run_all.R
cat("\n--- Compiling paper.pdf ---\n")
paper_dir <- "paper"
log_dir <- file.path(paper_dir, "log")
dir.create(log_dir, showWarnings = FALSE)

compile_ok <- tryCatch({
  old_wd <- getwd()
  setwd(paper_dir)

  run_latex <- function(label) {
    system2("pdflatex",
      args = c("-interaction=nonstopmode", "paper.tex"),
      stdout = file.path("log", paste0(label, "_stdout.log")),
      stderr = file.path("log", paste0(label, "_stderr.log")))
  }

  # Pass 1: pdflatex (generates .aux for bibtex)
  cat("  Pass 1: pdflatex...\n")
  run_latex("pass1")

  # Biber: resolve citations (biblatex backend)
  cat("  Pass 2: biber...\n")
  system2("biber", args = "paper",
    stdout = file.path("log", "biber_stdout.log"),
    stderr = file.path("log", "biber_stderr.log"))

  # Pass 3 & 4: pdflatex (resolve references)
  cat("  Pass 3: pdflatex...\n")
  run_latex("pass3")
  cat("  Pass 4: pdflatex...\n")
  run_latex("pass4")

  # Move build artifacts to log/
  artifacts <- list.files(".", pattern = "\\.(aux|bbl|bcf|blg|log|out|run\\.xml|synctex\\.gz|toc|fls|fdb_latexmk)$",
                         full.names = TRUE)
  if (length(artifacts) > 0) file.rename(artifacts, file.path("log", basename(artifacts)))

  if (file.exists("paper.pdf")) {
    cat("  paper.pdf compiled (", round(file.size("paper.pdf") / 1e6, 1), "MB). Build logs in paper/log/\n")
  } else {
    cat("  WARNING: paper.pdf not produced. Check paper/log/\n")
  }

  setwd(old_wd)
  file.exists(file.path(paper_dir, "paper.pdf"))
}, error = function(e) {
  cat("  WARNING: pdflatex not found or failed:", conditionMessage(e), "\n")
  try(setwd(old_wd), silent = TRUE)
  FALSE
})

} # end disabled paper compilation block
} # end main body guard

