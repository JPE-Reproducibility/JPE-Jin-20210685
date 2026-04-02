################################################################################
## sim_c4_data_figures.R — Data description figure CSVs
##
## Produces:
##   RF_CSV_DIR/fig_2a.csv        — Monitoring score distribution
##   RF_CSV_DIR/fig_2b.csv        — Renewal price change density
##   RF_CSV_DIR/fig_3.csv         — Claims by monitoring group & score quintile
##   RF_CSV_DIR/appendix/fig_a3.csv  — Monitoring discount persistence
##   RF_CSV_DIR/appendix/fig_a4.csv  — Claim surcharge by violation points
##   RF_CSV_DIR/appendix/fig_b2a.csv — Score density by pricing regime
##   RF_CSV_DIR/appendix/fig_b2b.csv — Score-discount mapping
##
## Logic mirrors codes/rf/c4_data_figures.R from the main repo exactly.
################################################################################

if (!exists("TABLES_DIR")) source("code/config.R")
source("code/functions/helper.R")
source("code/simulate/functions/load_sim_data.R")

dir.create(file.path(RF_CSV_DIR, "appendix"), recursive = TRUE, showWarnings = FALSE)

cat("\n=== sim_c4_data_figures.R ===\n")

data_states <- "IL"

################################################################################
## Figure A3: Persistence of Monitoring Discount
## (matches main repo c4_data_figures.R lines 43-112)
################################################################################

cat("--- fig_a3 ---\n")

panel_ubi_renw <- load_sim_data("panel_ubi_renw", "IL", 1)

panel_ubi_renw <- panel_ubi_renw %>%
  mutate(
    POL_ID_CHAR = as.character(POL_ID_CHAR),
    RENW_CNT    = RENW_SFX_NBR - 1,
    tm_disc     = UBI_DISC_FINAL,
    dup         = duplicated(paste(POL_ID_CHAR, RENW_CNT))
  ) %>%
  filter(!dup) %>%
  select(-dup)

tm_disc_panel <- panel_ubi_renw %>%
  select(POL_ID_CHAR, RENW_CNT, tm_disc) %>%
  filter(!is.na(tm_disc))

df_tm_disc_panel <- tm_disc_panel %>%
  group_by(POL_ID_CHAR) %>%
  mutate(
    last_renewal_seen = max(RENW_CNT),
    mean_0 = max(tm_disc * as.numeric(RENW_CNT == 0)),
    mean_1 = max(tm_disc * as.numeric(RENW_CNT == 2))
  ) %>%
  filter(last_renewal_seen >= 5 & !(mean_0 == 0 & mean_1 != 0)) %>%
  ungroup() %>%
  filter(RENW_CNT <= 5) %>%
  group_by(RENW_CNT) %>%
  summarise(mean = mean(tm_disc), se = sd(tm_disc) / sqrt(n()), .groups = "drop")

baseline <- as.numeric(df_tm_disc_panel$mean[df_tm_disc_panel$RENW_CNT == 0])
if (length(baseline) > 0 && baseline != 0) {
  df_tm_disc_panel$mean <- df_tm_disc_panel$mean / baseline
  df_tm_disc_panel$se   <- df_tm_disc_panel$se / baseline
}

write.csv(df_tm_disc_panel, file.path(RF_CSV_DIR, "appendix", "fig_a3.csv"), row.names = FALSE)
cat("  [GEN] appendix/fig_a3.csv (", nrow(df_tm_disc_panel), "rows)\n")

################################################################################
## Figure 2b: First Period Renewal Price Change by Monitoring Group (Benchmarked)
## (matches main repo c4_data_figures.R lines 129-275)
################################################################################

cat("--- fig_2b ---\n")

fig_2b_success <- tryCatch({
  panel_il <- load_sim_data("panel", data_states, 1)
  panel_il <- panel_il %>%
    mutate(POL_ID_CHAR = as.character(POL_ID_CHAR)) %>%
    filter(!duplicated(paste(POL_ID_CHAR, RENW_CNT)))

  panel_renw_il <- load_sim_data("panel_renw", data_states, 1)
  source("code/simulate/functions/data_clean/panel_renw_clean.R")
  panel_renw_cleaned <- panel_renw_clean(panel_renw_il, panel_il)
  TRUE
}, error = function(e) {
  cat("  [ERR] fig_2b: error loading/cleaning panel data:", conditionMessage(e), "\n")
  FALSE
})

if (fig_2b_success) {
  panel_ubi_renw_disc_ftr <- panel_ubi_renw %>%
    select(POL_ID_CHAR, RENW_CNT, tm_disc, any_of("UBI_DISC_30DAY")) %>%
    mutate(
      tm_disc_ftr = (1 - na_to_zero(tm_disc) / 100)
    ) %>%
    select(POL_ID_CHAR, RENW_CNT, tm_disc_ftr)

  tm_ind <- panel_il %>%
    filter(RENW_CNT == 0) %>%
    select(POL_ID_CHAR, RENW_CNT) %>%
    left_join(
      panel_ubi_renw %>%
        filter(RENW_CNT == 0) %>%
        mutate(
          tm_ind    = ((UBI_ENROLL_IND + UBI_FIN_IND) > 0) * 1,
          tm_fin_ind = (UBI_FIN_IND > 0) * 1
        ) %>%
        select(POL_ID_CHAR, tm_ind, tm_fin_ind),
      by = "POL_ID_CHAR"
    ) %>%
    mutate(
      tm_ind     = na_to_zero(tm_ind),
      tm_fin_ind = na_to_zero(tm_fin_ind)
    )

  tm_initial_disc_col <- intersect(
    c("tm_initial_disc", "tm_initial_disc_possible"),
    colnames(panel_il)
  )
  if (length(tm_initial_disc_col) > 0) {
    tm_init <- panel_il %>%
      filter(RENW_CNT == 0) %>%
      select(POL_ID_CHAR, all_of(tm_initial_disc_col[1])) %>%
      rename(tm_initial_disc = !!tm_initial_disc_col[1])
  } else {
    tm_init <- panel_il %>%
      filter(RENW_CNT == 0) %>%
      select(POL_ID_CHAR) %>%
      mutate(tm_initial_disc = 0)
  }

  renw_price <- panel_renw_cleaned %>%
    filter(RENW_CNT == 0) %>%
    select(POL_ID_CHAR, RENW_CNT, prem_renw_amt, panel_prem_expr_amt) %>%
    left_join(
      panel_ubi_renw_disc_ftr %>% mutate(RENW_CNT_join = RENW_CNT) %>%
        select(POL_ID_CHAR, RENW_CNT_join, tm_disc_ftr),
      by = c("POL_ID_CHAR", "RENW_CNT" = "RENW_CNT_join")
    ) %>%
    left_join(tm_init, by = "POL_ID_CHAR") %>%
    left_join(tm_ind %>% select(POL_ID_CHAR, tm_fin_ind), by = "POL_ID_CHAR") %>%
    mutate(
      tm_disc_ftr      = ifelse(is.na(tm_disc_ftr), 1, tm_disc_ftr),
      tm_initial_disc  = ifelse(is.na(tm_initial_disc), 0, tm_initial_disc),
      tm_fin_ind       = ifelse(is.na(tm_fin_ind), 0, tm_fin_ind),
      prem_renw_no_tm  = prem_renw_amt / tm_disc_ftr,
      tm_init_ftr      = 1 - tm_initial_disc / 100,
      prem_prev_no_tm  = panel_prem_expr_amt / tm_init_ftr,
      p_R_ftr_wo_tm    = prem_renw_no_tm / prem_prev_no_tm,
      p_R_ftr_w_tm     = p_R_ftr_wo_tm * tm_disc_ftr,
      p_R_ftr_na       = is.na(p_R_ftr_wo_tm) | p_R_ftr_wo_tm > 2 | p_R_ftr_wo_tm < 0.5
    ) %>%
    filter(!p_R_ftr_na)

  df_unmon     <- renw_price %>%
    filter(tm_fin_ind == 0) %>%
    mutate(p_R_ftr_m = p_R_ftr_wo_tm, Mon = "UnMon")
  df_mon       <- renw_price %>%
    filter(tm_fin_ind == 1) %>%
    mutate(p_R_ftr_m = p_R_ftr_w_tm, Mon = "Mon")
  df_mon_predisc <- renw_price %>%
    filter(tm_fin_ind == 1) %>%
    mutate(p_R_ftr_m = p_R_ftr_wo_tm, Mon = "Mon (pre-disc)")

  df_all <- rbind(df_unmon, df_mon, df_mon_predisc)

  bench_mean <- mean(df_unmon$p_R_ftr_m, na.rm = TRUE)

  df_plot <- df_all %>%
    mutate(p_R_ftr_bench = p_R_ftr_m / bench_mean)

  write.csv(df_plot[c("p_R_ftr_bench", "Mon")], file.path(RF_CSV_DIR, "fig_2b.csv"), row.names = FALSE)
  cat("  [GEN] fig_2b.csv (", nrow(df_plot), "obs)\n")
}

################################################################################
## Figure A4: Claim Surcharge by Violation Points
## (matches main repo c4_data_figures.R lines 288-350)
## Uses pre-extracted plot data from PLOT_DIR when available
################################################################################

cat("--- appendix/fig_a4 ---\n")

panel_a4 <- load_sim_data("panel", "IL", 1)
if (all(c("tier_acci_drvr_pt", "tier_good_ind", "clm_srchg") %in% names(panel_a4))) {
  plot_a4 <- panel_a4[, c("tier_acci_drvr_pt", "tier_good_ind", "clm_srchg")]
  plot_a4 <- plot_a4[!is.na(plot_a4$clm_srchg), ]
  write.csv(plot_a4, file.path(RF_CSV_DIR, "appendix", "fig_a4.csv"), row.names = FALSE)
  cat("  [GEN] appendix/fig_a4.csv (", nrow(plot_a4), "rows)\n")
} else {
  cat("  [SKIP] appendix/fig_a4.csv (missing columns in panel)\n")
}

################################################################################
## Figures B2a & B2b: Monitoring Score Distribution and Discount Mapping
## by Monitoring Pricing Regime
## (matches main repo c4_data_figures.R lines 364-458)
################################################################################

cat("--- appendix/fig_b2a & fig_b2b ---\n")

fig_b2_generated <- FALSE

dl_path <- file.path(SIM_CACHE_DIR, "data_list_IL.rds")
if (!file.exists(dl_path)) {
  dl_json <- file.path(SIM_CACHE_DIR, "data_list_IL.json")
  if (file.exists(dl_json)) dl_path <- dl_json
}

if (file.exists(dl_path)) {
  if (grepl("\\.json$", dl_path)) {
    data_list <- jsonlite::fromJSON(dl_path)
  } else {
    data_list <- readRDS(dl_path)
  }

  if ("log_tm_score" %in% names(data_list) &&
      "p_R_ftr_tm_disc" %in% names(data_list) &&
      "ubi_rate_dvc_vers" %in% names(data_list) &&
      "I_to_renw0" %in% names(data_list)) {

    ubi_rr_vers <- floor(data_list$ubi_rate_dvc_vers[data_list$I_to_renw0])
    tmpmask <- data_list$tm_ind > 0 &
      !is.na(data_list$p_R_ftr_na[data_list$I_to_renw0]) &
      (1 - data_list$p_R_ftr_na[data_list$I_to_renw0]) > 0 &
      !is.na(ubi_rr_vers)

    if (sum(tmpmask, na.rm = TRUE) > 100) {
      score    <- round(data_list$log_tm_score[tmpmask], 1)
      discount <- data_list$p_R_ftr_tm_disc[tmpmask]
      scheme   <- factor(ubi_rr_vers[tmpmask])

      df_score <- data.frame(
        score    = score,
        discount = discount,
        scheme   = scheme
      )

      write.csv(df_score[c("score", "scheme")], file.path(RF_CSV_DIR, "appendix", "fig_b2a.csv"), row.names = FALSE)
      cat("  [GEN] appendix/fig_b2a.csv (", nrow(df_score), "obs)\n")

      gg_df <- df_score %>%
        group_by(scheme, score) %>%
        summarise(
          est   = mean(discount),
          count = n(),
          ub    = mean(discount) + 1.96 * sd(discount) / sqrt(n()),
          lb    = mean(discount) - 1.96 * sd(discount) / sqrt(n()),
          .groups = "drop"
        )

      write.csv(gg_df, file.path(RF_CSV_DIR, "appendix", "fig_b2b.csv"), row.names = FALSE)
      cat("  [GEN] appendix/fig_b2b.csv (", nrow(gg_df), "rows)\n")

      fig_b2_generated <- TRUE
    }
  }
}

if (!fig_b2_generated) {
  cat("  [SKIP] fig_b2a/fig_b2b: requires data_list with log_tm_score,\n")
  cat("   p_R_ftr_tm_disc, ubi_rate_dvc_vers, I_to_renw0.\n")
}

################################################################################
## Figure 2a: Monitoring Score Distribution (Histogram)
## (matches main repo c4_data_figures.R lines 477-526)
## UBI_VALUE_MAX is ALREADY the log of the raw monitoring score
################################################################################

cat("--- fig_2a ---\n")

tm_score_df <- panel_ubi_renw %>%
  filter(RENW_CNT == 0 & UBI_FIN_IND == 1) %>%
  mutate(log_tm_score = UBI_VALUE_MAX) %>%
  filter(!is.na(log_tm_score) & log_tm_score > 0.1)

write.csv(tm_score_df["log_tm_score"], file.path(RF_CSV_DIR, "fig_2a.csv"), row.names = FALSE)
cat("  [GEN] fig_2a.csv (", nrow(tm_score_df), "obs)\n")

################################################################################
## Figure 3: Comparison of Subsequent Claim Cost Across Monitoring Groups
## (matches main repo c4_data_figures.R lines 542-676)
## Uses clean_choice_panel() for ubi_groups quintiles
################################################################################

cat("--- fig_3 ---\n")

fig_3_generated <- tryCatch({
  df_mh_fig3 <- load_sim_data("df_mh", "us", 1)
  df_mh_fig3 <- df_mh_fig3 %>%
    mutate(ubi_fin_ind = ifelse(is.na(ubi_fin_ind), 0, ubi_fin_ind))

  choice_panel <- load_sim_data("choice_panel", "us", 1)
  source("code/simulate/functions/data_clean/clean_choice_panel.R")
  choice_panel_clean <- clean_choice_panel(choice_panel)
  rm(choice_panel); gc()

  score_info <- choice_panel_clean %>%
    dplyr::select(POL_ID_CHAR, ubi_fin_ind, ubi_val_fin, ubi_val_fin_quantile, ubi_groups) %>%
    dplyr::distinct(POL_ID_CHAR, .keep_all = TRUE)

  df_mh_fig3 <- df_mh_fig3 %>%
    dplyr::left_join(
      score_info %>% dplyr::select(POL_ID_CHAR, ubi_groups, ubi_val_fin_quantile),
      by = "POL_ID_CHAR"
    )
  rm(choice_panel_clean, score_info); gc()

  df_fig3 <- df_mh_fig3 %>%
    filter(!is.na(clm_acci_renw_1))

  opt_out_avg <- mean(df_fig3$clm_acci_renw_1[df_fig3$ubi_fin_ind == 0], na.rm = TRUE)

  if (opt_out_avg > 0) {
    n_optout <- sum(df_fig3$ubi_fin_ind == 0, na.rm = TRUE)
    tbl_optout <- data.frame(
      group = "opt-out",
      avg_clm = mean(df_fig3$clm_acci_renw_1[df_fig3$ubi_fin_ind == 0], na.rm = TRUE),
      se_clm  = sd(df_fig3$clm_acci_renw_1[df_fig3$ubi_fin_ind == 0], na.rm = TRUE) / sqrt(n_optout),
      color_grp = "benchmark"
    )

    fin_mask <- df_fig3$ubi_fin_ind == 1
    n_optin <- sum(fin_mask, na.rm = TRUE)
    tbl_optin <- data.frame(
      group = "opt-in",
      avg_clm = mean(df_fig3$clm_acci_renw_1[fin_mask], na.rm = TRUE),
      se_clm  = sd(df_fig3$clm_acci_renw_1[fin_mask], na.rm = TRUE) / sqrt(n_optin),
      color_grp = "benchmark"
    )

    df_fin <- df_fig3 %>%
      filter(ubi_fin_ind == 1 & !is.na(ubi_groups) &
               ubi_groups %in% c("1", "2", "3", "4", "5"))

    tbl_quintiles <- df_fin %>%
      group_by(ubi_groups) %>%
      summarise(
        avg_clm = mean(clm_acci_renw_1, na.rm = TRUE),
        se_clm  = sd(clm_acci_renw_1, na.rm = TRUE) / sqrt(n()),
        .groups = "drop"
      ) %>%
      mutate(group = as.character(ubi_groups), color_grp = "quintile") %>%
      dplyr::select(group, avg_clm, se_clm, color_grp)

    tbl_all <- rbind(tbl_optout, tbl_optin, tbl_quintiles)
    tbl_all$avg_clm_pct <- tbl_all$avg_clm / opt_out_avg
    tbl_all$se_clm_pct  <- tbl_all$se_clm / opt_out_avg
    tbl_all$group <- factor(tbl_all$group,
                            levels = c("opt-out", "opt-in", "1", "2", "3", "4", "5"))

    write.csv(tbl_all[c("group", "avg_clm_pct", "se_clm_pct", "color_grp")],
              file.path(RF_CSV_DIR, "fig_3.csv"), row.names = FALSE)
    cat("  [GEN] fig_3.csv (", nrow(tbl_all), "rows)\n")
    TRUE
  } else {
    cat("  [SKIP] fig_3: opt-out average claim count is zero\n")
    FALSE
  }
}, error = function(e) {
  cat("  [ERR] fig_3:", conditionMessage(e), "\n")
  FALSE
})

cat("=== sim_c4_data_figures.R done ===\n\n")
