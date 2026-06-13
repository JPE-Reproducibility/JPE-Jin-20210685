## Appendix: Detailed PII Detection Results

*Generated on 2026-06-13 09:29:44*

This appendix lists all detected instances of potential personally identifiable information (PII) in the project files. Each entry shows the matched PII terms and, for data files, sample values to help verify whether the flagged content is indeed sensitive.

### Data Files

**/replication-package/jinvass26_replication_may_19_2016/LICENSE.txt**

- Variable: `to any person obtaining a copy`
  - Matched terms: son
  - Sample values: 

**/replication-package/jinvass26_replication_may_19_2016/data/precomputed/model_fit/tab_5.csv**

- Variable: `block1_data`
  - Matched terms: block, loc
  - Sample values: 46.9, 14.2, 17.3
- Variable: `block1_pred`
  - Matched terms: block, loc
  - Sample values: 45.3, 19.6, 15.6
- Variable: `block2_data`
  - Matched terms: block, loc
  - Sample values: 45.2, 12.9, 19.9
- Variable: `block2_pred`
  - Matched terms: block, loc
  - Sample values: 47.3, 19.3, 15.7
- Variable: `block3_data`
  - Matched terms: block, loc
  - Sample values: NA, 76.8, 5.2
- Variable: `block3_pred`
  - Matched terms: block, loc
  - Sample values: NA, 60.2, 19
- Variable: `block4_data`
  - Matched terms: block, loc
  - Sample values: NA, 51.8, 25.6
- Variable: `block4_pred`
  - Matched terms: block, loc
  - Sample values: NA, 51.3, 23.7
- Variable: `block5_data`
  - Matched terms: block, loc
  - Sample values: 36.5, 10.7, 16.5
- Variable: `block5_pred`
  - Matched terms: block, loc
  - Sample values: 37.5, 11.5, 16.3
- Variable: `block6_data`
  - Matched terms: block, loc
  - Sample values: NA, 45.1, 20.8
- Variable: `block6_pred`
  - Matched terms: block, loc
  - Sample values: NA, 45, 21

**/replication-package/jinvass26_replication_may_19_2016/data/precomputed/model_fit/tab_c2_block2.csv**

- Variable: `block1_data`
  - Matched terms: block, loc
  - Sample values: 46.9, 14.2, 17.3
- Variable: `block1_pred`
  - Matched terms: block, loc
  - Sample values: 46.4, 19.2, 15.2
- Variable: `block2_data`
  - Matched terms: block, loc
  - Sample values: 45.2, 12.9, 19.9
- Variable: `block2_pred`
  - Matched terms: block, loc
  - Sample values: 47.7, 19, 15.6
- Variable: `block3_data`
  - Matched terms: block, loc
  - Sample values: NA, 76.8, 5.2
- Variable: `block3_pred`
  - Matched terms: block, loc
  - Sample values: NA, 59.1, 19.5
- Variable: `block4_data`
  - Matched terms: block, loc
  - Sample values: NA, 51.8, 25.6
- Variable: `block4_pred`
  - Matched terms: block, loc
  - Sample values: NA, 50.1, 24.2
- Variable: `block5_data`
  - Matched terms: block, loc
  - Sample values: 36.5, 10.7, 16.5
- Variable: `block5_pred`
  - Matched terms: block, loc
  - Sample values: 37.6, 11.5, 16.3
- Variable: `block6_data`
  - Matched terms: block, loc
  - Sample values: NA, 45.1, 20.8
- Variable: `block6_pred`
  - Matched terms: block, loc
  - Sample values: NA, 45, 21

### Code Files

**/replication-package/jinvass26_replication_may_19_2016/code/c1_get_rf_exhibits.R**

- Line 27: lon
  ```
  ## Main body — runs only when not sourced for functions alo
  ```
- Line 54: son
  ```
  ## For exhibits based on regressions, the primary input is a JSON file
  ```
- Line 56: son
  ```
  ## the JSON coefficients. Non-regression exhibits use pre-computed CSVs directly.
  ```
- Line 61: son
  ```
  read_json_coefs <- function(json_path) {
  ```
- Line 62: son
  ```
  if (!file.exists(json_path)) return(NULL)
  ```
- Line 63: son
  ```
  jsonlite::fromJSON(json_path)
  ```
- Line 68: son
  ```
  list(json = "fig_4_regression.json", csv = "fig_4.csv", bal = FALSE),
  ```
- Line 69: son
  ```
  list(json = "appendix/fig_c1_regression.json", csv = "appendix/fig_c1.csv", bal = TRUE)
  ```
- Line 71: son
  ```
  j <- read_json_coefs(file.path(RF_REG_DIR, spec$json))
  ```
- Line 73: name
  ```
  idx_names <- c("ubi_ind", paste0("RENW_CNT", 1:5),
  ```
- Line 75: name
  ```
  est <- sapply(idx_names, function(nm) {
  ```
- Line 78: name
  ```
  se <- sapply(idx_names, function(nm) {
  ```
- Line 90: name
  ```
  cache_path <- if (grepl("appendix", spec$csv)) file.path(CACHE_DIR_APP, basename(spec$csv))
  ```
- Line 91: name
  ```
  else file.path(CACHE_DIR, basename(spec$csv))
  ```
- Line 92: name
  ```
  write.csv(vis_tbl, cache_path, row.names = FALSE)
  ```
- Line 98: son
  ```
  j_c4 <- read_json_coefs(file.path(RF_REG_DIR, "appendix/fig_c4_regression.json"))
  ```
- Line 100: name
  ```
  idx_names <- c("ubi_indTRUE", paste0("renw_cnt", 1:3),
  ```
- Line 102: name
  ```
  est <- sapply(idx_names, function(nm) {
  ```
- Line 105: name
  ```
  se <- sapply(idx_names, function(nm) {
  ```
- Line 119: name
  ```
  write.csv(vis_tbl, file.path(CACHE_DIR_APP, "fig_c4.csv"), row.names = FALSE)
  ```
- Line 124: son
  ```
  j5 <- read_json_coefs(file.path(RF_REG_DIR, "fig_5_regression.json"))
  ```
- Line 139: second
  ```
  df_iv <- extract_demand(j5$models$iv_second_stage, "IV")
  ```
- Line 144: name
  ```
  write.csv(df_est, file.path(CACHE_DIR, "fig_5.csv"), row.names = FALSE)
  ```
- Line 149: son
  ```
  j_het <- read_json_coefs(file.path(RF_REG_DIR, "appendix/fig_c2_c3_regression.json"))
  ```
- Line 152: name
  ```
  all_names <- names(j_het$coefficients)
  ```
- Line 153: name
  ```
  het_names <- all_names[grepl("ubi_ind", all_names) & grepl("post_ind", all_names) & grepl("_0", all_
  ```
- Line 155: name
  ```
  est_het <- sapply(het_names, function(nm) j_het$coefficients[[nm]]$estimate)
  ```
- Line 156: name
  ```
  se_het <- sapply(het_names, function(nm) j_het$coefficients[[nm]]$std_error)
  ```
- Line 157: name
  ```
  dimensions <- gsub("1+$", "", gsub("_0", "", gsub("ubi_ind:post_ind:", "", het_names)))
  ```
- Line 163: name
  ```
  dimension = dimensions, row.names = NULL
  ```
- Line 170: name
  ```
  write.csv(df_x, file.path(CACHE_DIR_APP, "fig_c2.csv"), row.names = FALSE)
  ```
- Line 171: name
  ```
  write.csv(df_y, file.path(CACHE_DIR_APP, "fig_c3.csv"), row.names = FALSE)
  ```
- Line 176: son
  ```
  j_b1b <- read_json_coefs(file.path(RF_REG_DIR, "appendix/fig_b1b_regression.json"))
  ```
- Line 179: name
  ```
  list(name = "prem_raw",    ctrl = "Raw",         reg = "Premium"),
  ```
- Line 180: name
  ```
  list(name = "prem_x_no_t", ctrl = "X w/o Trend", reg = "Premium"),
  ```
- Line 181: name
  ```
  list(name = "prem_x",      ctrl = "X Control",   reg = "Premium"),
  ```
- Line 182: name
  ```
  list(name = "clm_raw",     ctrl = "Raw",         reg = "Claim"),
  ```
- Line 183: name
  ```
  list(name = "clm_x_no_t",  ctrl = "X w/o Trend", reg = "Claim"),
  ```
- Line 184: name
  ```
  list(name = "clm_x",       ctrl = "X Control",   reg = "Claim")
  ```
- Line 188: name
  ```
  m <- j_b1b$models[[spec$name]]
  ```
- Line 205: name
  ```
  write.csv(result_tbl, file.path(CACHE_DIR_APP, "fig_b1b.csv"), row.names = FALSE)
  ```
- Line 211: son
  ```
  j_a5a6 <- read_json_coefs(file.path(RF_REG_DIR, "appendix/fig_a5_a6_regression.json"))
  ```
- Line 214: name
  ```
  spec_names <- c("w/ ctrl", "claim ctrl", "w/o ctrl")
  ```
- Line 216: name
  ```
  for (g in spec_names) {
  ```
- Line 229: name
  ```
  write.csv(df_info_pct, file.path(CACHE_DIR_APP, "fig_a5_a6.csv"), row.names = FALSE)
  ```
- Line 281: city
  ```
  ## ---- fig_5.png (demand elasticity) ------------------------------------------
  ```
- Line 299: coord
  ```
  coord_cartesian(ylim = c(-1.5, 0)) +
  ```
- Line 301: city
  ```
  ylab("Price Elasticity Estimates")
  ```
- Line 341: coord
  ```
  coord_cartesian(xlim = c(0, 8)) +
  ```
- Line 434: lat
  ```
  xlab("Existing Violation/Accident Points") +
  ```
- Line 670: lat
  ```
  ## ---- appendix/fig_c4.png (AAF violations) -----------------------------------
  ```
- Line 676: son
  ```
  # Ensure period column is numeric (JSON-derived CSVs already have it)
  ```
- Line 677: name
  ```
  if ("year" %in% names(vis_tbl) && !"period" %in% names(vis_tbl)) {
  ```
- Line 681: name
  ```
  if (!"segment_group" %in% names(vis_tbl)) {
  ```
- Line 818: block, lat, loc
  ```
  latex_block_b <- paste0(
  ```
- Line 845: block, lat, loc
  ```
  latex_block_b, "\n",
  ```
- Line 866: son
  ```
  j_tab3 <- read_json_coefs(file.path(RF_REG_DIR, "tab_3_regression.json"))
  ```
- Line 896: son
  ```
  "\t\t\\caption{First-Period Claim Comparison Across Monitoring Groups} \n",
  ```
- Line 921: son
  ```
  "\t{\\footnotesize \\emph{Notes: }This table reports the results of a regression where the dependent
  ```
- Line 930: son
  ```
  cat("  [SKIP] tab_3.tex (no JSON)\n"); n_skipped <- n_skipped + 1
  ```
- Line 935: son
  ```
  generate_mh_did_table <- function(json_path, dest_path, label, caption_label) {
  ```
- Line 936: son
  ```
  j <- read_json_coefs(json_path)
  ```
- Line 1018: lat
  ```
  "\t  {\\footnotesize \\emph{Notes:} This table reports results of \\eqref{mh_reg}. The datasets cons
  ```
- Line 1024: lat
  ```
  "\t  (past records of violations or claims). Continuous observable characteristics are normalized. \
  ```
- Line 1042: son
  ```
  if (generate_mh_did_table(file.path(RF_REG_DIR, "tab_2_regression.json"),
  ```
- Line 1047: son
  ```
  cat("  [SKIP] tab_2.tex (no JSON)\n"); n_skipped <- n_skipped + 1
  ```
- Line 1050: son
  ```
  if (generate_mh_did_table(file.path(RF_REG_DIR, "appendix/tab_c1_regression.json"),
  ```
- Line 1055: son
  ```
  cat("  [SKIP] appendix/tab_c1.tex (no JSON)\n"); n_skipped <- n_skipped + 1
  ```
- Line 1060: son
  ```
  tab_a1a2_json <- file.path(RF_CSV_DIR, "appendix", "tab_a1a2_summary.json")
  ```
- Line 1061: son
  ```
  if (file.exists(tab_a1a2_json)) {
  ```
- Line 1062: son
  ```
  j <- jsonlite::fromJSON(tab_a1a2_json)
  ```
- Line 1070: name
  ```
  # Panel A: 3-column layout of "Name (mean)" strings, sorted alphabetically
  ```
- Line 1071: name
  ```
  binary_strings <- sprintf("%s (%.2f)", pa$name, pa$mean)
  ```
- Line 1093: name
  ```
  paste(r$name, "&", fmt_ms(r$mean), "&", fmt_ms(r$sd), "&",
  ```
- Line 1122: zip
  ```
  "Zipcode income is winsorized at the 1st and 99th percentiles. ",
  ```
- Line 1148: lat
  ```
  "structural estimation and counterfactual simulations", j$il$age_mean)
  ```
- Line 1154: son
  ```
  cat("  [SKIP] appendix/tab_a1.tex (no JSON)\n"); n_skipped <- n_skipped + 1
  ```
- Line 1167: lat
  ```
  ## Paper compilation moved to run_all.R (after c3_get_fitctf_exhibits.R)
  ```
- Line 1170: lat
  ```
  if (FALSE) { # Disabled — compilation now in run_all
  ```
- Line 1180: lat
  ```
  run_latex <- function(label) {
  ```
- Line 1181: lat
  ```
  system2("pdflatex",
  ```
- Line 1187: lat
  ```
  # Pass 1: pdflatex (generates .aux for bibtex)
  ```
- Line 1188: lat
  ```
  cat("  Pass 1: pdflatex...\n")
  ```
- Line 1189: lat
  ```
  run_latex("pass1")
  ```
- Line 1191: lat
  ```
  # Biber: resolve citations (biblatex backend)
  ```
- Line 1197: lat
  ```
  # Pass 3 & 4: pdflatex (resolve references)
  ```
- Line 1198: lat
  ```
  cat("  Pass 3: pdflatex...\n")
  ```
- Line 1199: lat
  ```
  run_latex("pass3")
  ```
- Line 1200: lat
  ```
  cat("  Pass 4: pdflatex...\n")
  ```
- Line 1201: lat
  ```
  run_latex("pass4")
  ```
- Line 1204: lat
  ```
  artifacts <- list.files(".", pattern = "\\.(aux|bbl|bcf|blg|log|out|run\\.xml|synctex\\.gz|toc|fls|f
  ```
- Line 1205: name
  ```
  full.names = TRUE)
  ```
- Line 1206: name
  ```
  if (length(artifacts) > 0) file.rename(artifacts, file.path("log", basename(artifacts)))
  ```
- Line 1217: lat
  ```
  cat("  WARNING: pdflatex not found or failed:", conditionMessage(e), "\n")
  ```
- Line 1222: block, lat, loc
  ```
  } # end disabled paper compilation block
  ```

**/replication-package/jinvass26_replication_may_19_2016/code/c2_get_param_tables.R**

- Line 8: lat
  ```
  ## (SD across bootstrap samples), then formats LaTeX tables matching the
  ```
- Line 13: lat
  ```
  ##   output/exhibits/appendix/tab_a3.tex    (Additional latent parameter summary)
  ```
- Line 87: name
  ```
  full.names = TRUE)
  ```
- Line 93: name
  ```
  # Parse all GQ CSVs into named vectors
  ```
- Line 95: name
  ```
  names(gq_parsed) <- basename(gq_csv_files)
  ```
- Line 98: name
  ```
  bs0_gq_name <- grep("id-0\\.csv$", names(gq_parsed), value = TRUE)
  ```
- Line 99: name
  ```
  if (length(bs0_gq_name) == 0) stop("bootstrap-result-id-0.csv not found")
  ```
- Line 105: name
  ```
  full.names = TRUE)
  ```
- Line 110: name
  ```
  names(orig_parsed) <- basename(orig_csv_files)
  ```
- Line 113: name
  ```
  for (nm in names(orig_parsed)) {
  ```
- Line 114: name
  ```
  if (nm %in% names(gq_parsed)) {
  ```
- Line 117: name
  ```
  if (sig_col %in% names(gq_vals)) {
  ```
- Line 126: name
  ```
  # gq_smry column mapping: statistic name (after janitor::clean_names) -> gq_smry col
  ```
- Line 138: name
  ```
  build_param_df <- function(parsed_vec, param_names, gq_rows) {
  ```
- Line 139: name
  ```
  df <- data.frame(parameter = param_names, stringsAsFactors = FALSE)
  ```
- Line 140: name
  ```
  for (stat_name in names(stat_to_gq_col)) {
  ```
- Line 141: name
  ```
  col_idx <- stat_to_gq_col[stat_name]
  ```
- Line 142: lon, name
  ```
  vals <- vapply(seq_along(param_names), function(i) {
  ```
- Line 145: name
  ```
  col_name <- sprintf("gq_smry.%d.%d", gq_row, col_idx)
  ```
- Line 146: name
  ```
  v <- parsed_vec[col_name]
  ```
- Line 149: name
  ```
  df[[stat_name]] <- vals
  ```
- Line 155: name
  ```
  main_param_names <- c(
  ```
- Line 169: name, second
  ```
  secondary_param_names <- c(
  ```
- Line 179: second
  ```
  secondary_gq_rows <- c(9, 10, 11, 13, 14, 15, 16, 17)
  ```
- Line 183: name
  ```
  df <- build_param_df(p, main_param_names, main_gq_rows)
  ```
- Line 186: name
  ```
  names(main_params_dfs) <- names(gq_parsed)
  ```
- Line 188: second
  ```
  secondary_params_dfs <- lapply(gq_parsed, function(p) {
  ```
- Line 189: name, second
  ```
  df <- build_param_df(p, secondary_param_names, secondary_gq_rows)
  ```
- Line 192: name, second
  ```
  names(secondary_params_dfs) <- names(gq_parsed)
  ```
- Line 195: name
  ```
  for (nm in names(main_params_dfs)) {
  ```
- Line 197: second
  ```
  secondary_params_dfs[[nm]]$is_zero_csv <- grepl("id-0\\.csv$", nm)
  ```
- Line 215: lat
  ```
  "Correlation Log\nBaseline Risk",
  ```
- Line 216: lat
  ```
  "Correlation Log\nPrivate Risk")
  ```
- Line 255: lat
  ```
  "Correlation Log\nBaseline Risk" = list(digits = 2, scientific = FALSE),
  ```
- Line 256: lat
  ```
  "Correlation Log\nPrivate Risk" = list(digits = 2, scientific = FALSE)
  ```
- Line 268: name
  ```
  write.csv(main_table_results, file.path(MODEL_TAB_DIR, "tab_6.csv"), row.names = TRUE)
  ```
- Line 271: lat
  ```
  main_tex <- generate_latex_table_with_se(
  ```
- Line 286: lat, second
  ```
  ## ---- Table A3: Secondary (additional latent) parameter summary --------------
  ```
- Line 291: second
  ```
  secondary_variables <- c(
  ```
- Line 300: second
  ```
  secondary_variables_pretty <- secondary_variables
  ```
- Line 302: second
  ```
  secondary_custom_rounding <- list(
  ```
- Line 309: lat
  ```
  "Correlation Log\nBaseline Risk" = list(digits = 2, scientific = FALSE),
  ```
- Line 310: lat
  ```
  "Correlation Log\nPrivate Risk" = list(digits = 2, scientific = FALSE)
  ```
- Line 314: second
  ```
  full_params_dfs <- secondary_params_dfs
  ```
- Line 316: second
  ```
  cat("  Computing secondary parameter table (tab_a3)...\n")
  ```
- Line 317: second
  ```
  secondary_table_results <- get_summary_table_from_list(
  ```
- Line 319: second
  ```
  secondary_variables,
  ```
- Line 320: second
  ```
  secondary_variables_pretty,
  ```
- Line 325: name, second
  ```
  write.csv(secondary_table_results, file.path(MODEL_TAB_DIR, "tab_a3.csv"), row.names = TRUE)
  ```
- Line 328: lat, second
  ```
  secondary_tex <- generate_latex_table_with_se(
  ```
- Line 329: second
  ```
  secondary_table_results,
  ```
- Line 330: second
  ```
  custom_params = secondary_variables,
  ```
- Line 332: second
  ```
  custom_rounding = secondary_custom_rounding,
  ```
- Line 333: lat
  ```
  caption = "Additional Latent Parameter Summary",
  ```
- Line 334: second
  ```
  label = "tab:app-est-secondary",
  ```
- Line 339: second
  ```
  writeLines(secondary_tex, taba3_path)
  ```
- Line 347: name
  ```
  summarize_bootstrap_matrices <- function(mat_list, zero_name = NULL,
  ```
- Line 353: name
  ```
  if (is.null(zero_name)) {
  ```
- Line 354: name
  ```
  zero_idx <- grep("id-0\\.csv$", names(mat_list))
  ```
- Line 356: name
  ```
  zero_idx <- which(names(mat_list) == zero_name)
  ```
- Line 363: name
  ```
  rnames <- rownames(mat_list[[1]])
  ```
- Line 364: lname, name
  ```
  cnames <- colnames(mat_list[[1]])
  ```
- Line 368: name
  ```
  dimnames = list(rnames, cnames))
  ```
- Line 379: name
  ```
  q2.5_mat[i, j]  <- quantile(values_finite, 0.025, names = FALSE)
  ```
- Line 380: name
  ```
  q97.5_mat[i, j] <- quantile(values_finite, 0.975, names = FALSE)
  ```
- Line 391: lat
  ```
  # Escape angle brackets for LaTeX
  ```
- Line 415: name
  ```
  .extract_scalar <- function(sum_list, name) {
  ```
- Line 418: name
  ```
  if (!is.null(names(M)) && key %in% names(M)) return(as.numeric(M[[key]]))
  ```
- Line 421: name
  ```
  rn <- rownames(M)
  ```
- Line 425: name
  ```
  c(est = pick(sum_list$estimate, name),
  ```
- Line 426: name
  ```
  se  = pick(sum_list$sd, name))
  ```
- Line 532: name
  ```
  matrix(vals, ncol = 1, dimnames = list(rn, "mean"))
  ```
- Line 536: lat
  ```
  # The older bootstrap directory has true variation; the flat structure may not
  ```
- Line 541: name
  ```
  full.names = TRUE)
  ```
- Line 544: name
  ```
  names(cost_parsed) <- basename(cost_files)
  ```
- Line 545: lat
  ```
  # Check if MH params actually vary (if not, also try flat structure for point estimates)
  ```
- Line 556: lat
  ```
  # Also load current model_cost point estimates (flat structure, for override)
  ```
- Line 557: lat
  ```
  cost_flat_dir <- file.path(MODEL_OUT_DIR, "model_cost", "results")
  ```
- Line 558: lat
  ```
  if (cost_flat_dir != cost_csv_dir && dir.exists(cost_flat_dir)) {
  ```
- Line 559: lat
  ```
  cost_flat_0 <- file.path(cost_flat_dir, "bootstrap-result-id-0.csv")
  ```
- Line 560: lat
  ```
  if (file.exists(cost_flat_0)) {
  ```
- Line 561: lat
  ```
  cost_point_parsed <- parse_original_csv_thetas(cost_flat_0)
  ```
- Line 569: name
  ```
  full.names = TRUE)
  ```
- Line 572: name
  ```
  names(sev_parsed) <- basename(sev_files)
  ```
- Line 581: name
  ```
  nonx_matrices <- lapply(names(orig_parsed), function(nm) {
  ```
- Line 588: name
  ```
  names(nonx_matrices) <- names(orig_parsed)
  ```
- Line 606: block, loc, lon
  ```
  # Left block: standalone parameters
  ```
- Line 614: lon
  ```
  left_df <- do.call(rbind, lapply(seq_along(left_params), function(i) {
  ```
- Line 615: name
  ```
  src <- names(left_params)[i]; lab <- unname(left_params[i])
  ```
- Line 619: name
  ```
  stringsAsFactors = FALSE, check.names = FALSE)
  ```
- Line 622: block, loc
  ```
  # Right block: MH effects by regime
  ```
- Line 623: name
  ```
  mh_name <- function(regime, which)
  ```
- Line 629: name
  ```
  sc_opt   <- .extract_scalar(nonxsum, mh_name(rg, "opt-in"))
  ```
- Line 630: name
  ```
  sc_inten <- .extract_scalar(nonxsum, mh_name(rg, "intensity"))
  ```
- Line 641: name
  ```
  stringsAsFactors = FALSE, check.names = FALSE)
  ```
- Line 655: name
  ```
  stringsAsFactors = FALSE, check.names = FALSE))
  ```
- Line 661: lat
  ```
  format = "latex", booktabs = TRUE, escape = FALSE,
  ```
- Line 670: lat
  ```
  kableExtra::kable_styling(latex_options = c("hold_position", "scale_down"))
  ```
- Line 684: block, loc
  ```
  # Insert notes block before \end{table}
  ```
- Line 685: block, loc
  ```
  notes_block <- paste0(
  ```
- Line 693: block, loc
  ```
  a4_tex <- sub("\\end{table}", notes_block, a4_raw, fixed = TRUE)
  ```
- Line 699: name
  ```
  write.csv(out_df, file.path(MODEL_TAB_DIR, "tab_a4.csv"), row.names = TRUE)
  ```
- Line 731: name
  ```
  price_col_names <- c("Log Score", "First-Renewal w/ TM",
  ```
- Line 740: name
  ```
  n_cols <- length(price_col_names)
  ```
- Line 742: name
  ```
  dimnames = list(price_row_labels, price_col_names))
  ```
- Line 744: name
  ```
  extract_main <- function(name) {
  ```
- Line 745: name
  ```
  v <- main_parsed[name]
  ```
- Line 749: name
  ```
  extract_price <- function(name) {
  ```
- Line 751: name
  ```
  v <- src[name]
  ```
- Line 790: name
  ```
  full.names = TRUE)
  ```
- Line 793: name
  ```
  names(price_parsed_list) <- basename(price_files)
  ```
- Line 802: name
  ```
  price_matrices <- lapply(names(orig_parsed), function(nm) {
  ```
- Line 805: name
  ```
  names(price_matrices) <- names(orig_parsed)
  ```
- Line 812: name
  ```
  rn  <- rownames(est)
  ```
- Line 813: lname, name
  ```
  cn  <- colnames(est)
  ```
- Line 815: name
  ```
  # Parse rownames: "Regime N — componen
  ```
- Line 867: name
  ```
  # Rename columns for display — use two-line headers matching benchma
  ```
- Line 910: lname, name
  ```
  colnames(a5_results_se) <- paste0(colnames(a5_results_se), "_se")
  ```
- Line 912: name
  ```
  write.csv(a5_csv, file.path(MODEL_TAB_DIR, "tab_a5.csv"), row.names = TRUE)
  ```
- Line 927: lat
  ```
  ## Table A6: Latent Parameter Loadings on Observables
  ```
- Line 934: name
  ```
  x_col_names <- c("(Intercept)", "Female Ind.", "Driver License Year",
  ```
- Line 942: lat
  ```
  "Delinq. Score*", "Population Density",
  ```
- Line 943: lat, zip
  ```
  "Violation Points", "Zipcode Income", "Model Year",
  ```
- Line 946: loc, name
  ```
  D_X_local <- length(x_col_names) - 1  # 28 (excluding intercept)
  ```
- Line 949: name
  ```
  list(name = "Log Baseline Accident Risk",
  ```
- Line 951: name
  ```
  list(name = "Log Risk Aversion",
  ```
- Line 953: name
  ```
  list(name = "Plan Switching Cost (10e3)",
  ```
- Line 955: name
  ```
  list(name = "Firm Switching Cost (10e3)",
  ```
- Line 957: name
  ```
  list(name = "Default Plan FE (10e3)",
  ```
- Line 959: name
  ```
  list(name = "Monitoring Disutility (10e3)",
  ```
- Line 961: name
  ```
  list(name = "Log Minor Accident Severity (10e3) Mean",
  ```
- Line 963: name
  ```
  list(name = "Major Accident Severity (10e3) Pareto Shape",
  ```
- Line 971: name
  ```
  n_cols <- length(x_col_names)  # intercept + 28 X vars
  ```
- Line 973: name
  ```
  rownames(mat) <- vapply(param_types, `[[`, character(1), "name")
  ```
- Line 974: lname, name
  ```
  colnames(mat) <- x_col_names
  ```
- Line 976: lon
  ```
  for (i in seq_along(param_types)) {
  ```
- Line 991: loc
  ```
  for (j in 1:D_X_local) {
  ```
- Line 992: name
  ```
  t1_name <- paste0(pt$theta_1, ".", j)
  ```
- Line 993: name
  ```
  v <- source[t1_name]
  ```
- Line 1004: name
  ```
  full.names = TRUE)
  ```
- Line 1007: name
  ```
  names(sev_x_parsed_list) <- basename(sev_x_files)
  ```
- Line 1016: name
  ```
  x_matrices <- lapply(names(orig_parsed), function(nm) {
  ```
- Line 1019: name
  ```
  names(x_matrices) <- names(orig_parsed)
  ```
- Line 1027: name
  ```
  rownames(se_t) <- rownames(est_t)
  ```
- Line 1028: lname, name
  ```
  colnames(se_t) <- colnames(est_t)
  ```
- Line 1030: name
  ```
  rn <- rownames(est_t)
  ```
- Line 1031: name
  ```
  # Rename variables to match benchmark display names
  ```
- Line 1036: lat
  ```
  rn[rn == "Violation Points"] <- "Violation Record (Points)"
  ```
- Line 1037: name
  ```
  rownames(est_t) <- rn
  ```
- Line 1038: name
  ```
  rownames(se_t) <- rn
  ```
- Line 1039: lname, name
  ```
  cn <- colnames(est_t)
  ```
- Line 1063: name
  ```
  if (h %in% names(col_header_map)) col_header_map[[h]]
  ```
- Line 1080: lat
  ```
  "\\begin{table}[!h]\n\\begin{centering}\n\\caption{\\label{tab:app-hyperp-x}Latent Parameter Loading
  ```
- Line 1088: lname, name
  ```
  colnames(a6_results_se) <- paste0(colnames(a6_results_se), "_se")
  ```
- Line 1090: name
  ```
  write.csv(a6_csv, file.path(MODEL_TAB_DIR, "tab_a6.csv"), row.names = TRUE)
  ```
- Line 1117: name
  ```
  full.names = TRUE)
  ```
- Line 1122: name
  ```
  names(gq_4p_parsed) <- basename(gq_4p_files)
  ```
- Line 1125: name
  ```
  build_param_df(p, main_param_names, main_gq_rows)
  ```
- Line 1127: name
  ```
  names(main_4p_dfs) <- names(gq_4p_parsed)
  ```
- Line 1128: name
  ```
  for (nm in names(main_4p_dfs)) {
  ```
- Line 1140: name
  ```
  write.csv(main_4p_results, file.path(MODEL_TAB_DIR, "tab_a7.csv"), row.names = TRUE)
  ```
- Line 1143: lat
  ```
  a7_tex <- generate_latex_table_with_se(
  ```
- Line 1148: lat
  ```
  caption = "Latent Parameter Summary - Four-Period Horizon",
  ```
- Line 1185: name
  ```
  full.names = TRUE)
  ```
- Line 1190: name
  ```
  names(gq_2p_parsed) <- basename(gq_2p_files)
  ```
- Line 1193: name
  ```
  build_param_df(p, main_param_names, main_gq_rows)
  ```
- Line 1195: name
  ```
  names(main_2p_dfs) <- names(gq_2p_parsed)
  ```
- Line 1196: name
  ```
  for (nm in names(main_2p_dfs)) {
  ```
- Line 1208: name
  ```
  write.csv(main_2p_results, file.path(MODEL_TAB_DIR, "tab_a8.csv"), row.names = TRUE)
  ```
- Line 1211: lat
  ```
  a8_tex <- generate_latex_table_with_se(
  ```
- Line 1216: lat
  ```
  caption = "Latent Parameter Summary - Two-Period Horizon",
  ```
- Line 1248: lat
  ```
  # Bootstrap CSV directory for cost MH het model (flat structure)
  ```
- Line 1253: name
  ```
  full.names = TRUE)
  ```
- Line 1274: name
  ```
  names(data_vals) <- header
  ```
- Line 1307: lat
  ```
  # Build LaTeX table matching existing format
  ```
- Line 1345: name
  ```
  parameter = names(bs_mean),
  ```
- Line 1350: name
  ```
  write.csv(c3_csv, file.path(MODEL_TAB_DIR, "tab_c3.csv"), row.names = FALSE)
  ```

**/replication-package/jinvass26_replication_may_19_2016/code/c3_get_fitctf_exhibits.R**

- Line 32: son
  ```
  list(left = "Poisson claim counts", right = "First renewal pricing factor"),
  ```
- Line 33: lat
  ```
  list(left = "Monitoring score", right = "Latter renewal pricing factor")
  ```
- Line 37: lon
  ```
  for (s_idx in seq_along(sections)) {
  ```
- Line 144: name
  ```
  label_str <- ifelse(label_str %in% names(coverage_labels),
  ```
- Line 157: block, loc
  ```
  vals <- c(r$block1_data, r$block1_pred, r$block2_data, r$block2_pred,
  ```
- Line 158: block, loc
  ```
  r$block3_data, r$block3_pred, r$block4_data, r$block4_pred,
  ```
- Line 159: block, loc
  ```
  r$block5_data, r$block5_pred, r$block6_data, r$block6_pred)
  ```
- Line 164: lon
  ```
  for (j in seq_along(vals)) {
  ```
- Line 286: block, loc
  ```
  # Append Block 2 (choice shares) if CSV exists
  ```
- Line 287: block, loc
  ```
  tab_c2b_csv <- file.path(MODEL_FIT_DIR, "tab_c2_block2.csv")
  ```
- Line 349: son
  ```
  xlab("Log Poisson Accident Arrival Rate") +
  ```
- Line 368: coord
  ```
  coord_cartesian(xlim = c(-4.5, -1.5)) +
  ```
- Line 370: son
  ```
  xlab("Log(Poisson Accident Arrival Rate)")
  ```
- Line 390: son
  ```
  xlab("Log Poisson Accident Arrival Rate") +
  ```
- Line 419: son
  ```
  xlab("Log Poisson Accident Arrival Rate") +
  ```
- Line 439: name
  ```
  rownames(d) <- d$scenario
  ```
- Line 440: name
  ```
  rownames(dd) <- dd$scenario
  ```
- Line 448: name
  ```
  df_ctf_results = d[, !names(d) %in% c("scenario", "type", "brand_value_1", "brand_value_2", "brand_v
  ```
- Line 449: name
  ```
  df_ctf_results_delta = dd[, !names(dd) %in% c("scenario", "type", "brand_value_1", "brand_value_2", 
  ```
- Line 460: name
  ```
  if (col %in% names(df)) df[[col]] else rep(NA_real_, nrow(df))
  ```
- Line 565: lat
  ```
  "This table reports counterfactual simulation results, with the ``Current Regime'' corresponding to 
  ```
- Line 574: lat
  ```
  "$^{1}$~Units are given in brackets. ``$\\Delta$'' denotes changes relative to the ``No Monitoring''
  ```
- Line 615: name
  ```
  # NOTE: CSV filenames here use a sequential index that does NOT match the rendered
  ```
- Line 617: block, loc
  ```
  # concatenated table environments (model fit + choice shares appendix block),
  ```
- Line 621: name
  ```
  # Body-section and Tab A.x numbering match the filenames as expected.
  ```
- Line 623: lat
  ```
  list(csv = "tab_7.csv",    tex = "tab_7.tex",              caption = "Counterfactual Simulation Resu
  ```
- Line 624: lat
  ```
  list(csv = "tab_a9.csv",   tex = "appendix/tab_a9.tex",    caption = "Counterfactual Simulation Resu
  ```
- Line 625: lat
  ```
  list(csv = "tab_a10.csv",  tex = "appendix/tab_a10.tex",   caption = "Counterfactual Simulation Resu
  ```
- Line 626: lat
  ```
  list(csv = "tab_a11.csv",  tex = "appendix/tab_a11.tex",   caption = "Counterfactual Simulation Resu
  ```
- Line 627: lat
  ```
  list(csv = "tab_a12.csv",  tex = "appendix/tab_a12.tex",   caption = "Counterfactual Simulation Resu
  ```
- Line 628: lat
  ```
  list(csv = "tab_a13.csv",  tex = "appendix/tab_a13.tex",   caption = "Counterfactual Simulation Resu
  ```
- Line 629: lat
  ```
  list(csv = "tab_a14.csv",  tex = "appendix/tab_a14.tex",   caption = "Counterfactual Simulation Resu
  ```
- Line 630: lat
  ```
  list(csv = "tab_a15.csv",  tex = "appendix/tab_a15.tex",   caption = "Counterfactual Simulation Resu
  ```
- Line 631: lat
  ```
  list(csv = "tab_c4.csv",   tex = "appendix/tab_c4.tex",    caption = "Counterfactual Simulation Resu
  ```
- Line 632: lat
  ```
  list(csv = "tab_c5.csv",   tex = "appendix/tab_c5.tex",    caption = "Counterfactual Simulation Resu
  ```

**/replication-package/jinvass26_replication_may_19_2016/code/config.R**

- Line 4: lat
  ```
  ## All paths are relative to the repo root. No .env file needed.
  ```
- Line 7: lat
  ```
  if (!exists("USE_SIMULATED_DATA")) USE_SIMULATED_DATA <- FALSE
  ```
- Line 18: block, lat, loc
  ```
  K_BLOCK_CTF        <- 4         # Index of the CTF choice block (latest TM scoring/pricing regime)
  ```
- Line 20: loc
  ```
  LOG_FILE_SUFFIXES  <- list(     # CmdStan run identifiers for locating estimation output
  ```
- Line 34: lat
  ```
  if (!USE_SIMULATED_DATA) {
  ```
- Line 46: lat
  ```
  ## === Simulated-data paths ===
  ```
- Line 47: lat
  ```
  SIM_DATA_DIR      <- "data/simulated"
  ```
- Line 50: lat
  ```
  RF_CSV_DIR    <- "output/simulated/precomputed/rf"
  ```
- Line 51: lat
  ```
  RF_REG_DIR    <- "output/simulated/estimates/regression_output"
  ```
- Line 52: lat
  ```
  MODEL_FIT_DIR <- "output/simulated/precomputed/model_fit"
  ```
- Line 53: lat
  ```
  MODEL_OUT_DIR <- "output/simulated/estimates/model_output"
  ```
- Line 54: lat
  ```
  CTF_DIR       <- "output/simulated/precomputed/ctf"
  ```
- Line 56: lat
  ```
  TABLES_DIR    <- "output/exhibits_simulated"
  ```
- Line 57: lat
  ```
  IMAGES_DIR    <- "output/exhibits_simulated"
  ```
- Line 69: lat
  ```
  # Simulated model_cost is estimated separately for Tab A.4 display only.
  ```
- Line 70: lat
  ```
  source("code/simulate/functions/sim_helpers.R")
  ```
- Line 100: son
  ```
  .sp <- jsonlite::fromJSON(file.path(SIM_DATA_DIR, "data_profile_data_list.json"))$estimation$sim_par
  ```
- Line 136: lat
  ```
  if (USE_SIMULATED_DATA) {
  ```

**/replication-package/jinvass26_replication_may_19_2016/code/functions/bootstrap_helpers.R**

- Line 8: name
  ```
  #' Parse a CmdStan CSV: skip comment lines, return named numeric vector
  ```
- Line 14: name
  ```
  names(values) <- header
  ```
- Line 31: name
  ```
  names(vals) <- header[1:n_keep]
  ```
- Line 39: name
  ```
  q25 <- quantile(x, 0.25, names = FALSE, type = 7)
  ```
- Line 40: name
  ```
  q75 <- quantile(x, 0.75, names = FALSE, type = 7)
  ```
- Line 46: name
  ```
  extract_parameter_statistic <- function(parameter_name, statistic_name, df_list) {
  ```
- Line 49: name
  ```
  if (parameter_name %in% df$parameter) {
  ```
- Line 51: name
  ```
  filter(parameter == parameter_name) %>%
  ```
- Line 52: name
  ```
  pull(!!sym(statistic_name))
  ```
- Line 60: name
  ```
  bs0_df <- df_list[grep("id-0\\.csv$", names(df_list))]
  ```
- Line 64: name
  ```
  filter(parameter == parameter_name) %>%
  ```
- Line 65: name
  ```
  pull(!!sym(statistic_name))
  ```
- Line 86: name
  ```
  param_map <- setNames(parameters_pretty, parameters)
  ```
- Line 87: name
  ```
  stat_map <- setNames(statistics_pretty, statistics)
  ```
- Line 89: name
  ```
  mutate(parameter_pretty = ifelse(parameter %in% names(param_map),
  ```
- Line 94: name
  ```
  statistic = ifelse(statistic %in% names(stat_map),
  ```
- Line 100: lat
  ```
  #' Generate a LaTeX table with estimate + SE rows
  ```
- Line 101: lat
  ```
  generate_latex_table_with_se <- function(
  ```
- Line 107: name
  ```
  use_pretty_param_names = TRUE,
  ```
- Line 121: name
  ```
  { setNames(.$parameter_pretty, .$parameter) }
  ```
- Line 130: name
  ```
  if (!is.null(custom_rounding) && stat %in% names(custom_rounding)) {
  ```
- Line 132: name
  ```
  } else if (!is.null(custom_rounding) && param %in% names(custom_rounding)) {
  ```
- Line 145: lat
  ```
  latex_rows <- vapply(params, function(param) {
  ```
- Line 155: name
  ```
  lbl <- if (use_pretty_param_names && !is.null(pretty_map[[param]]))
  ```
- Line 166: lat
  ```
  body_rows <- paste(latex_rows, collapse = "\n")
  ```

**/replication-package/jinvass26_replication_may_19_2016/code/functions/ctf_table_helpers.R**

- Line 2: lat
  ```
  ## ctf_table_helpers.R — Shared helpers for generating CTF LaTeX tabl
  ```
- Line 34: name
  ```
  scenarios <- rownames(d)
  ```
- Line 36: lon
  ```
  # Save intermediate CSV for standalone exhibit generation
  ```
- Line 37: name
  ```
  csv_name <- sub("\\.rds$", ".csv", basename(rds_path))
  ```
- Line 38: name
  ```
  # Map labeled .rds names to paper table names
  ```
- Line 39: name
  ```
  csv_name_map <- c(
  ```
- Line 51: name
  ```
  rds_basename <- basename(rds_path)
  ```
- Line 52: name
  ```
  if (rds_basename %in% names(csv_name_map)) csv_name <- csv_name_map[rds_basename]
  ```
- Line 56: name
  ```
  d_level$scenario <- rownames(d_level)
  ```
- Line 59: name
  ```
  dd_delta$scenario <- rownames(dd_delta)
  ```
- Line 69: name
  ```
  csv_path <- file.path(CTF_RESULTS_DIR, csv_name)
  ```
- Line 70: name
  ```
  write.csv(csv_df, csv_path, row.names = FALSE)
  ```
- Line 80: name
  ```
  if (col %in% names(df)) df[[col]] else rep(NA_real_, nrow(df))
  ```
- Line 119: name
  ```
  ## ---- Section header names ----
  ```
- Line 124: lat
  ```
  ## ---- Build LaTeX rows ----
  ```
- Line 208: lat
  ```
  "This table reports counterfactual simulation results, with the ``Current Regime'' corresponding to 
  ```
- Line 217: lat
  ```
  "$^{1}$~Units are given in brackets. ``$\\Delta$'' denotes changes relative to the ``No Monitoring''
  ```
- Line 240: lat
  ```
  ## ---- Assemble full LaTeX ----
  ```

**/replication-package/jinvass26_replication_may_19_2016/code/functions/getX.R**

- Line 9: name
  ```
  ## Returns: list of variable name vectors (+ label matrix for "estimation")
  ```
- Line 42: loc
  ```
  'x_loc_grg_verify_ind',
  ```
- Line 43: loc, zip
  ```
  'x_loc_zipcd_ind',
  ```
- Line 44: loc, zip
  ```
  'x_loc_zipcd_lg_inc',
  ```
- Line 54: loc
  ```
  'x_loc_grg_verify_ind',
  ```
- Line 60: loc, zip
  ```
  'x_loc_popltn_dens_pct', 'x_loc_zipcd_agi',
  ```
- Line 78: lat
  ```
  'Population Density Percentile',
  ```
- Line 79: zip
  ```
  'Zipcode AGI',
  ```
- Line 84: lname, name
  ```
  colnames(X_label_mat) <- c("vars", "name")
  ```

**/replication-package/jinvass26_replication_may_19_2016/code/run_all.R**

- Line 9: son
  ```
  ## CSV and JSON files in data/. Output is written to output/exhibits/.
  ```
- Line 20: name
  ```
  model_dirs <- list.dirs(MODEL_OUT_DIR, recursive = FALSE, full.names = TRUE)
  ```
- Line 23: name
  ```
  archives <- list.files(results_dir, pattern = "\\.tar\\.gz$", full.names = TRUE)
  ```
- Line 28: name
  ```
  cat("Extracting", basename(archive), "...\n")
  ```
- Line 52: lat
  ```
  run_latex <- function(label) {
  ```
- Line 53: lat
  ```
  system2("pdflatex",
  ```
- Line 59: lat
  ```
  cat("  Pass 1: pdflatex...\n"); run_latex("pass1")
  ```
- Line 64: lat
  ```
  cat("  Pass 3: pdflatex...\n"); run_latex("pass3")
  ```
- Line 65: lat
  ```
  cat("  Pass 4: pdflatex...\n"); run_latex("pass4")
  ```
- Line 68: lat
  ```
  artifacts <- list.files(".", pattern = "\\.(aux|bbl|bcf|blg|log|out|run\\.xml|synctex\\.gz|toc|fls|f
  ```
- Line 69: name
  ```
  full.names = TRUE)
  ```
- Line 70: name
  ```
  if (length(artifacts) > 0) file.rename(artifacts, file.path("log", basename(artifacts)))
  ```
- Line 80: lat
  ```
  cat("  WARNING: pdflatex not found or failed:", conditionMessage(e), "\n")
  ```

**/replication-package/jinvass26_replication_may_19_2016/code/run_simulated.R**

- Line 2: lat
  ```
  ## run_simulated.R — Simulated-data verification pipeli
  ```
- Line 6: lat
  ```
  ##   Rscript code/run_simulated.R
  ```
- Line 9: lat
  ```
  ## files in data/simulated/. Output is written to output/exhibits_simulated/,
  ```
- Line 10: lat
  ```
  ## and paper/paper_simulated.pdf is compiled.
  ```
- Line 12: lat, son
  ```
  ## Inputs (shipped): data/simulated/data_profile_rf.json,
  ```
- Line 13: lat, son
  ```
  ##                   data/simulated/data_profile_data_list.json
  ```
- Line 18: lat
  ```
  ##   RUN_CTF        <- TRUE   # run counterfactual simulation
  ```
- Line 22: lat
  ```
  USE_SIMULATED_DATA <- TRUE
  ```
- Line 35: name
  ```
  files <- list.files(dir_path, full.names = TRUE, recursive = TRUE)
  ```
- Line 37: name
  ```
  files <- list.files(dir_path, pattern = pattern, full.names = TRUE, recursive = TRUE)
  ```
- Line 58: name
  ```
  for (md in list.dirs(MODEL_OUT_DIR, recursive = FALSE, full.names = TRUE)) {
  ```
- Line 64: son
  ```
  clean_contents(RF_REG_DIR, pattern = "\\.json$")
  ```
- Line 73: lat
  ```
  CACHE_DIR <- "output/simulated/cached"
  ```
- Line 75: name
  ```
  cache_tarballs <- list.files(CACHE_DIR, pattern = "\\.tar\\.gz$", full.names = TRUE)
  ```
- Line 78: name
  ```
  cat("  Extracting cache:", basename(tarball), "\n")
  ```
- Line 79: lat
  ```
  untar(tarball, exdir = "output/simulated")
  ```
- Line 90: lat
  ```
  ## Step 1: Generate simulated RF data (if not already generated)
  ```
- Line 98: lat
  ```
  source("code/simulate/simulate_data/sim_generate_rf_data.R")
  ```
- Line 112: lat
  ```
  stop("Missing simulated data files:\n  ", paste(missing, collapse = "\n  "),
  ```
- Line 117: lat
  ```
  ## Step 2: Generate simulated data_list (if not already generated)
  ```
- Line 125: lat
  ```
  source("code/simulate/simulate_data/sim_generate_data_list.R")
  ```
- Line 131: lat, son
  ```
  ## Step 3: RF analysis (data_rf.csv -> output/simulated/ CSVs + JSONs)
  ```
- Line 136: lat
  ```
  source("code/simulate/sim_c0_sum_stat.R")
  ```
- Line 137: lat
  ```
  source("code/simulate/sim_c1_mh_regression.R")
  ```
- Line 138: lat
  ```
  source("code/simulate/sim_c2_mh_viol.R")
  ```
- Line 139: lat
  ```
  source("code/simulate/sim_c3_demand_elast.R")
  ```
- Line 140: lat
  ```
  source("code/simulate/sim_c4_data_figures.R")
  ```
- Line 141: lat
  ```
  source("code/simulate/sim_c5_selection_figures.R")
  ```
- Line 153: lat
  ```
  "USE_SIMULATED_DATA <- TRUE; ",
  ```
- Line 165: lat
  ```
  run_step("code/simulate/sim_c6_estimate.R", "Step 4a: Structural estimation")
  ```
- Line 166: lat
  ```
  run_step("code/simulate/sim_c7_model_fit.R", "Step 4b: Model fit")
  ```
- Line 167: lat
  ```
  run_step("code/simulate/sim_c6b_estimate_robustness.R", "Step 4c: Robustness estimation (2p/4p)")
  ```
- Line 168: lat
  ```
  run_step("code/simulate/sim_c6c_estimate_cost_mhhet.R", "Step 4d: Cost MH-het estimation")
  ```
- Line 169: lat
  ```
  run_step("code/simulate/sim_c7b_model_fit_mhhet.R", "Step 4e: MH-het model fit (Tab C.2)")
  ```
- Line 175: lat
  ```
  ## Step 5: CTF simulation (requires CmdStan + estimation output)
  ```
- Line 179: lat
  ```
  run_step("code/simulate/sim_c8_ctf_run.R", "Step 5a: CTF simulation (main)")
  ```
- Line 180: lat
  ```
  run_step("code/simulate/sim_c8b_ctf_robustness.R", "Step 5b: CTF robustness (2p/4p)")
  ```
- Line 181: lat
  ```
  run_step("code/simulate/sim_c8c_ctf_mhhet_learning.R", "Step 5c: CTF mhhet + learning")
  ```
- Line 182: lat
  ```
  run_step("code/simulate/sim_c8d_ctf_appendix_robustness.R", "Step 5d: CTF appendix robustness (A.9-A
  ```
- Line 191: lat
  ```
  cat("\n--- Step 6: Generating exhibits (simulated mode) ---\n")
  ```
- Line 193: son
  ```
  cat("  RF JSONs: ", RF_REG_DIR, "\n")
  ```
- Line 196: lat
  ```
  ## Extract bootstrap archives (no-op for simulated unless estimation was run)
  ```
- Line 198: name
  ```
  model_dirs <- list.dirs(MODEL_OUT_DIR, recursive = FALSE, full.names = TRUE)
  ```
- Line 202: name
  ```
  archives <- list.files(results_dir, pattern = "\\.tar\\.gz$", full.names = TRUE)
  ```
- Line 207: name
  ```
  cat("Extracting", basename(archive), "...\n")
  ```
- Line 215: lat
  ```
  ## c2: param tables — requires bootstrap CSVs (may not exist in simulated mod
  ```
- Line 225: lat
  ```
  ## Step 7: Compile paper_simulated.pdf
  ```
- Line 228: lat
  ```
  cat("\n--- Compiling paper_simulated.pdf ---\n")
  ```
- Line 237: lat
  ```
  ## Override \exhibitpath to point to simulated exhibits.
  ```
- Line 241: lat
  ```
  run_latex <- function(label) {
  ```
- Line 243: lat, name
  ```
  "pdflatex -interaction=nonstopmode -jobname=paper_simulated ",
  ```
- Line 244: lat
  ```
  "'\\def\\exhibitpath{../output/exhibits_simulated}\\input{paper.tex}' ",
  ```
- Line 250: lat
  ```
  cat("  Pass 1: pdflatex...\n"); run_latex("pass1")
  ```
- Line 252: lat
  ```
  system2("biber", args = "paper_simulated",
  ```
- Line 255: lat
  ```
  cat("  Pass 3: pdflatex...\n"); run_latex("pass3")
  ```
- Line 256: lat
  ```
  cat("  Pass 4: pdflatex...\n"); run_latex("pass4")
  ```
- Line 259: lat
  ```
  artifacts <- list.files(".", pattern = "^paper_simulated\\.(aux|bbl|bcf|blg|log|out|run\\.xml|syncte
  ```
- Line 260: name
  ```
  full.names = TRUE)
  ```
- Line 261: name
  ```
  if (length(artifacts) > 0) file.rename(artifacts, file.path("log", basename(artifacts)))
  ```
- Line 263: lat
  ```
  if (file.exists("paper_simulated.pdf")) {
  ```
- Line 264: lat
  ```
  cat("  paper_simulated.pdf compiled (",
  ```
- Line 265: lat
  ```
  round(file.size("paper_simulated.pdf") / 1e6, 1), "MB).",
  ```
- Line 268: lat
  ```
  cat("  WARNING: paper_simulated.pdf not produced. Check paper/log/\n")
  ```
- Line 273: lat
  ```
  cat("  WARNING: pdflatex not found or failed:", conditionMessage(e), "\n")
  ```
- Line 277: lat
  ```
  cat("\nDone. Simulated exhibits written to:", TABLES_DIR, "\n")
  ```

**/replication-package/jinvass26_replication_may_19_2016/code/simulate/fig_b3_override.R**

- Line 2: lat
  ```
  ## fig_b3_override.R — illustration-only override for Fig B.3 in paper_simulated.p
  ```
- Line 5: lat
  ```
  ##   Overwrites output/simulated/precomputed/model_fit/fig_b3.csv with values
  ```
- Line 7: lat
  ```
  ##   evaluated at the simulated-pipeline's leps and llambda. Mirrors exactly the
  ```
- Line 12: lat
  ```
  ##   The main simulated pipeline uses score_scale = 0.4 in sim_generate_data_list.R
  ```
- Line 13: lat
  ```
  ##   (necessary because simulated `leps` has wider variance than real-data `leps`
  ```
- Line 14: lat
  ```
  ##   for TM participants, which would otherwise inflate the leps -> discount
  ```
- Line 16: lat
  ```
  ##   dampened by 60%, hiding the relationship.
  ```
- Line 19: lat
  ```
  ##   score~risk relationship looks like at full slope. It does NOT modify
  ```
- Line 23: lat
  ```
  ## ISOLATION GUARANTEES
  ```
- Line 27: lat
  ```
  ##   - Writes: output/simulated/precomputed/model_fit/fig_b3.csv (overwrite only)
  ```
- Line 42: loc
  ```
  local({
  ```
- Line 43: lat, loc
  ```
  source("code/simulate/functions/sim_helpers.R", local = FALSE)
  ```
- Line 54: lat
  ```
  ## llambda_clm - leps_n_clm) and the CTF path (get_d_latentparams.R:241):
  ```
- Line 55: second
  ```
  ## the second column is the BASELINE log-rate excluding leps. Fixed in
  ```
- Line 97: name
  ```
  write.csv(out_csv, file.path(MODEL_FIT_DIR, "fig_b3.csv"), row.names = FALSE)
  ```

**/replication-package/jinvass26_replication_may_19_2016/code/simulate/functions/ctf/ctf_calibration.R**

- Line 5: lat
  ```
  source('code/simulate/functions/ctf/get_ctf_util_profit.R')
  ```
- Line 7: name
  ```
  calibration_save_filename <- file.path(bootstrap_ctf_dir, paste0("ctf_calibration-id-", bootstrap_id
  ```
- Line 8: name
  ```
  if(USE_CACHE && file.exists(calibration_save_filename)){
  ```
- Line 9: name
  ```
  out_calibration <- readRDS(calibration_save_filename)
  ```
- Line 102: name
  ```
  rename(brand_value = value) %>% mutate(brand_value_oo = 0)
  ```
- Line 202: lname, name
  ```
  colnames(stage_scores) = c("score", deltas)
  ```
- Line 302: lname, name
  ```
  if(max(stage_own_results[,colnames(stage_own_results) == "0.01"])<2 |
  ```
- Line 303: lname, name
  ```
  max(stage_oo_flex_results[,colnames(stage_oo_flex_results) == "0.01"])<2){
  ```
- Line 382: name
  ```
  saveRDS(out_calibration, calibration_save_filename)
  ```
- Line 383: name
  ```
  cat("calibration results saved as: ", calibration_save_filename,"\n")
  ```

**/replication-package/jinvass26_replication_may_19_2016/code/simulate/functions/ctf/ctf_clear_cache.R**

- Line 21: name
  ```
  files <- list.files(bootstrap_ctf_dir, pattern = pat, full.names = TRUE)
  ```

**/replication-package/jinvass26_replication_may_19_2016/code/simulate/functions/ctf/ctf_equi_k_find_equilibria.R**

- Line 7: lat
  ```
  # A profile is an equilibrium if neither firm can unilaterally deviate to increase profit.
  ```

**/replication-package/jinvass26_replication_may_19_2016/code/simulate/functions/ctf/ctf_equi_k_save_grid.R**

- Line 10: lat
  ```
  source('code/simulate/functions/ctf/get_ctf_util_profit.R')
  ```
- Line 11: lat
  ```
  source('code/simulate/functions/ctf/ctf_equi_k_find_equilibria.R')
  ```
- Line 12: lat
  ```
  source('code/simulate/functions/ctf/find_cycles.R')
  ```
- Line 14: name
  ```
  part_opt_save_filename <- file.path(bootstrap_ctf_dir, paste0("ctf_opt-part-id-", bootstrap_id, ctf_
  ```
- Line 15: name
  ```
  opt_save_filename <- file.path(bootstrap_ctf_dir, paste0("ctf_opt-id-", bootstrap_id, ctf_config_pre
  ```
- Line 23: block, loc
  ```
  tic(cat("My code block", n, "(", N_stage, "total points)\n"))
  ```
- Line 52: lname, name
  ```
  colnames(out_pis) <- c("pi", "pi_oo", "pi0", "pi0_oo")
  ```
- Line 74: name
  ```
  if(USE_CACHE && file.exists(part_opt_save_filename)){
  ```
- Line 75: name
  ```
  out_opt_part <- readRDS(part_opt_save_filename)
  ```
- Line 97: name
  ```
  full.names = TRUE
  ```
- Line 208: loc
  ```
  equi_hit_minmax_local <-
  ```
- Line 214: loc
  ```
  if(min(equi_hit_minmax_local) < 1){
  ```
- Line 215: loc
  ```
  equi = equi[!equi_hit_minmax_local,]
  ```
- Line 297: loc
  ```
  equi_hit_minmax_local <-
  ```
- Line 313: loc
  ```
  equi_hit_minmax_tmp = c(equi_hit_minmax_tmp, equi_hit_minmax_local)
  ```
- Line 379: name
  ```
  saveRDS(out_opt_part, part_opt_save_filename)
  ```
- Line 385: name
  ```
  if(USE_CACHE && file.exists(opt_save_filename)){
  ```
- Line 386: name
  ```
  out_opt <- readRDS(opt_save_filename)
  ```
- Line 406: name
  ```
  full.names = TRUE
  ```
- Line 627: loc
  ```
  equi_hit_minmax_local <-
  ```
- Line 644: loc
  ```
  equi_hit_minmax_tmp = c(equi_hit_minmax_tmp, equi_hit_minmax_local)
  ```
- Line 734: name
  ```
  saveRDS(out_opt, opt_save_filename)
  ```
- Line 748: name
  ```
  ds_save_filename <- file.path(bootstrap_ctf_dir, paste0("ctf_ds-id-", bootstrap_id, paste0(ctf_confi
  ```
- Line 751: name
  ```
  ds_progress_filename <- file.path(bootstrap_ctf_dir, paste0("ctf_ds_progress-id-", bootstrap_id,
  ```
- Line 753: name
  ```
  if(USE_CACHE && file.exists(ds_progress_filename) && !file.exists(ds_save_filename)){
  ```
- Line 755: name
  ```
  ds_progress <- readRDS(ds_progress_filename)
  ```
- Line 762: name
  ```
  if(USE_CACHE && file.exists(ds_save_filename)){
  ```
- Line 764: name
  ```
  out_ds_df <- readRDS(ds_save_filename)
  ```
- Line 782: name
  ```
  out_ds <- readRDS(ds_save_filename)
  ```
- Line 807: name
  ```
  full.names = TRUE
  ```
- Line 1055: lon
  ```
  hard_coded_lower_limits[7] = hard_coded_upper_limits[7] #no longer need to search for k4s
  ```
- Line 1243: name
  ```
  saveRDS(out_ds_df, ds_save_filename)
  ```
- Line 1282: name
  ```
  saveRDS(out_ds, ds_save_filename)
  ```

**/replication-package/jinvass26_replication_may_19_2016/code/simulate/functions/ctf/ctf_preload.R**

- Line 8: block, loc
  ```
  #   N_part: min(N_by_choice_regime[k_block_ctf], nrow(data)) for CTF sample size
  ```
- Line 9: block, loc
  ```
  #   k_block_ctf default: 4 (4th period block index)
  ```
- Line 16: block, loc
  ```
  if(!exists("k_block_ctf")){assign("k_block_ctf", K_BLOCK_CTF); print("setting k_block_ctf to K_BLOCK
  ```
- Line 26: block, loc
  ```
  fit_sample_size = d_by_block
  ```
- Line 28: lat
  ```
  #### Calculate outside option utilities ####
  ```
- Line 29: block, loc
  ```
  n0 = n_choice_regime_cutoffs[k_block_ctf];
  ```
- Line 30: block, loc
  ```
  N_part = N_by_choice_regime[k_block_ctf]
  ```
- Line 31: block, loc
  ```
  # k_block_ctf indexes all 6 choice regimes (1=NB, 2=NB+TM, 3=Renw, 4=Renw+TM, 5=Attr, 6=Attr+TM).
  ```
- Line 32: block, loc
  ```
  # k_tm indexes the 3 TM-eligible blocks only: blocks 2,4,6 map to k_tm 1,2,3.
  ```
- Line 33: block, loc
  ```
  k_tm = k_block_ctf/2; n0_tm = N_by_choice_regime[2]*(k_tm-1)+1
  ```
- Line 35: block, loc
  ```
  J = Js[k_block_ctf]
  ```
- Line 52: lname, name
  ```
  full_oo_cov_name = colnames(prices_oo_full)
  ```
- Line 54: name
  ```
  full_oo_cov_name = full_oo_cov_name[!grepl("_3",full_oo_cov_name)]
  ```
- Line 56: name
  ```
  firm_indices = str_extract(full_oo_cov_name[1:5], "^[^_]+")
  ```
- Line 57: name
  ```
  min_price_firm_index = which.min(colMeans(prices_oo_temp[,grepl("_4", full_oo_cov_name)]))
  ```
- Line 58: name
  ```
  min_price_firm_cov_indices = grepl(paste0(firm_indices[min_price_firm_index],"_"),full_oo_cov_name)
  ```
- Line 59: name
  ```
  max_price_firm_index = which.max(colMeans(prices_oo_temp[,grepl("_4", full_oo_cov_name)]))
  ```
- Line 60: name
  ```
  max_price_firm_cov_indices = grepl(paste0(firm_indices[max_price_firm_index],"_"),full_oo_cov_name)
  ```
- Line 63: name
  ```
  kc_firm_cov_indices = grepl(paste0(firm_indices[kc_firm_index], "_"), full_oo_cov_name)
  ```
- Line 65: name
  ```
  median_price_firm_index = which.median(colMeans(prices_oo_temp[,grepl("_4", full_oo_cov_name)]))
  ```
- Line 69: name
  ```
  median_price_firm_cov_indices = grepl(paste0(firm_indices[median_price_firm_index], "_"), full_oo_co
  ```
- Line 73: name
  ```
  mkt_share_oo <- unname(IL_OO_MKT_SHARES)
  ```

**/replication-package/jinvass26_replication_may_19_2016/code/simulate/functions/ctf/find_cycles.R**

- Line 4: lat
  ```
  # can cycle. This module detects such cycles by simulating best-response
  ```
- Line 15: lat
  ```
  find_all_cycles_simulation <- function(payoff_data,
  ```
- Line 25: lat
  ```
  message("Calculating best responses...")
  ```
- Line 50: lat
  ```
  message("Detecting cycles via simulation...")
  ```
- Line 151: name
  ```
  expected_cols <- c("cycle_id", "sequence_in_cycle", names(original_data))
  ```
- Line 153: name
  ```
  dimnames = list(NULL, expected_cols))))
  ```
- Line 160: lon
  ```
  for (i in seq_along(cycles_list)) {
  ```
- Line 164: lon
  ```
  for (j in seq_along(current_cycle_states)) {
  ```
- Line 190: name
  ```
  orig_data_cols <- setdiff(names(final_details_df), id_cols)
  ```
- Line 198: name
  ```
  expected_cols <- c("cycle_id", "sequence_in_cycle", names(original_data))
  ```
- Line 200: name
  ```
  dimnames = list(NULL, expected_cols))))
  ```
- Line 223: lat
  ```
  results_all <- find_all_cycles_simulation(analysis_data, start_states_to_check = all_unique_states)
  ```

**/replication-package/jinvass26_replication_may_19_2016/code/simulate/functions/ctf/get_ctf_util_profit.R**

- Line 59: lat, second
  ```
  # Second moments calculations:
  ```
- Line 68: second
  ```
  # Second moment of R
  ```
- Line 77: second
  ```
  # Overall second moment
  ```
- Line 113: lat
  ```
  #### calculate baseline utils and profits #####
  ```
- Line 129: zip
  ```
  zip_inc_norm[n0:n1],
  ```
- Line 152: zip
  ```
  zip_inc_norm[n0:n1],
  ```
- Line 181: zip
  ```
  zip_inc_norm[n0:n1],
  ```
- Line 236: zip
  ```
  zip_inc_norm[n0:n1],
  ```
- Line 247: lat
  ```
  revs_tm, #notice that we are now calculating only the first-period profit
  ```
- Line 299: second
  ```
  utils_second_period = data.frame(matrix(0, N_part, (J+J_oo))) #J*2 second period choices: own plans,
  ```
- Line 300: second
  ```
  cp_second_period = data.frame(matrix(0, N_part, (J+J_oo))) #J*2 second period choices: own plans, oo
  ```
- Line 306: lat, second
  ```
  ## first calculate the baseline renewal price that consumers face in second period
  ```
- Line 311: second
  ```
  ## get second-period profit matrix for each own-firm first-period choice
  ```
- Line 316: second
  ```
  # first do first-to-second period transition (cut of tm)
  ```
- Line 317: lat
  ```
  lambda_first_period = lambda_choice[n0:n1]  ## we have already calculated t0 profit, only need trans
  ```
- Line 318: lat
  ```
  lambda_choice_latter_period = lambda_choice[n0:n1] ## technically their X'es evolves over time, but 
  ```
- Line 319: lat
  ```
  lambda_severe_choice_latter_period = lambda_severe_choice[n0:n1]
  ```
- Line 320: lat
  ```
  lambda_latter_period = lambda[N_choice_to_N][n0:n1]
  ```
- Line 321: lat
  ```
  lambda_severe_latter_period = lambda_severe[N_choice_to_N][n0:n1]
  ```
- Line 332: lat
  ```
  lambda_choice_latter_period = lambda_choice[n0:n1] * ifelse(grepl("learning", model_config), LEARNIN
  ```
- Line 333: lat
  ```
  lambda_severe_choice_latter_period = lambda_severe_choice[n0:n1] * ifelse(grepl("learning", model_co
  ```
- Line 334: lat
  ```
  lambda_latter_period = lambda[N_choice_to_N][n0:n1] * ifelse(grepl("learning", model_config), LEARNI
  ```
- Line 335: lat
  ```
  lambda_severe_latter_period = lambda_severe[N_choice_to_N][n0:n1] * ifelse(grepl("learning", model_c
  ```
- Line 357: second
  ```
  #second period only expenses
  ```
- Line 362: second
  ```
  #second-to-third-period R and R_oo:
  ```
- Line 375: second
  ```
  #second period only expenses
  ```
- Line 381: second
  ```
  #second-to-third-period R and R_oo:
  ```
- Line 395: second
  ```
  #second period only expenses
  ```
- Line 407: second
  ```
  #second-to-third-period R and R_oo:
  ```
- Line 418: lon, second
  ```
  ## there is no longer tm choice in second period, when j has tm in it, you simply get a different pr
  ```
- Line 426: lat
  ```
  lambda_choice_latter_period, # lambda_choice[n0:n1],
  ```
- Line 427: lat
  ```
  lambda_severe_choice_latter_period, # lambda_severe_choice[n0:n1],
  ```
- Line 433: zip
  ```
  zip_inc_norm[n0:n1],
  ```
- Line 450: lat
  ```
  lambda_choice_latter_period, # lambda_choice[n0:n1],
  ```
- Line 451: lat
  ```
  lambda_severe_choice_latter_period, # lambda_severe_choice[n0:n1],
  ```
- Line 457: zip
  ```
  zip_inc_norm[n0:n1],
  ```
- Line 479: lat
  ```
  lambda_choice_latter_period, # lambda_choice[n0:n1],
  ```
- Line 480: lat
  ```
  lambda_severe_choice_latter_period, # lambda_severe_choice[n0:n1],
  ```
- Line 486: zip
  ```
  zip_inc_norm[n0:n1],
  ```
- Line 501: lat
  ```
  ## calculate future period profits (assuming no switch after t2)
  ```
- Line 504: lat
  ```
  lambda_latter_period, #lambda[N_choice_to_N][n0:n1],
  ```
- Line 505: lat
  ```
  lambda_severe_latter_period, #lambda_severe[N_choice_to_N][n0:n1],
  ```
- Line 513: lat
  ```
  lambda_latter_period, #lambda[N_choice_to_N][n0:n1],
  ```
- Line 514: lat
  ```
  lambda_severe_latter_period, #lambda_severe[N_choice_to_N][n0:n1],
  ```
- Line 537: second
  ```
  utils_second_period = utils_second_period + tmp_weight * utils_renw
  ```
- Line 538: second
  ```
  cp_second_period = cp_second_period + tmp_weight * cp_renw
  ```
- Line 559: lat
  ```
  lambda_latter_period, #lambda[N_choice_to_N][n0:n1],
  ```
- Line 560: lat
  ```
  lambda_severe_latter_period, #lambda_severe[N_choice_to_N][n0:n1],
  ```
- Line 566: lat
  ```
  lambda_latter_period, #lambda[N_choice_to_N][n0:n1],
  ```
- Line 567: lat
  ```
  lambda_severe_latter_period, #lambda_severe[N_choice_to_N][n0:n1],
  ```
- Line 580: lat
  ```
  lambda_latter_period, #lambda[N_choice_to_N][n0:n1],
  ```
- Line 581: lat
  ```
  lambda_severe_latter_period, #lambda_severe[N_choice_to_N][n0:n1],
  ```
- Line 587: lat
  ```
  lambda_latter_period, #lambda[N_choice_to_N][n0:n1],
  ```
- Line 588: lat
  ```
  lambda_severe_latter_period, #lambda_severe[N_choice_to_N][n0:n1],
  ```
- Line 596: lat
  ```
  "error: can't calculate welfare horizon > 4"
  ```
- Line 625: second
  ```
  utils_second_period = utils_second_period * 2/welfare_horizon,
  ```
- Line 627: second
  ```
  cp_second_period = cp_second_period,
  ```

**/replication-package/jinvass26_replication_may_19_2016/code/simulate/functions/data_clean/get_next_rrev.R**

- Line 2: lat
  ```
  ## get_next_rrev.R — Calculate next rate revision date per sta
  ```
- Line 4: name
  ```
  ## Usage: get_next_rrev(dataset, rrev_date_var_name, state_cd_var_name)
  ```
- Line 9: name
  ```
  get_next_rrev <- function(dataset, rrev_date_var_name, state_cd_var_name){
  ```
- Line 16: name
  ```
  dataset$ST_CD <- unlist(dataset[state_cd_var_name])
  ```
- Line 18: name
  ```
  temp$rrev_date_base <- unlist(dataset[rrev_date_var_name])
  ```
- Line 38: name
  ```
  dataset$rrev_date_base <- unlist(dataset[rrev_date_var_name])
  ```
- Line 40: name
  ```
  rename( new_rrev_date_rank_asc = rrev_order_asc
  ```

**/replication-package/jinvass26_replication_may_19_2016/code/simulate/functions/data_clean/panel_renw_clean.R**

- Line 15: zip
  ```
  # Med_zip_inc = median(as.vector(panel_renw_input$zipcode_agi), na.rm = T)
  ```
- Line 36: sex
  ```
  , x_drvr_is_female = (RENW_QT_DRVR_SEX_CD == 'F') * 1
  ```
- Line 50: school
  ```
  , x_drvr_voc_edu_ind = ifelse(x_drvr_edu == 3, 1, 0) # vocational school
  ```
- Line 132: second
  ```
  , admin_second_cnvrg_dt = RENW_QT_SCND_CNVRG_EVNT_DT
  ```
- Line 134: loc, location
  ```
  # Location
  ```
- Line 135: zip
  ```
  , admin_grg_zipcode = RENW_QT_VEH_GRG_PSTL_CD
  ```
- Line 136: zip
  ```
  , panel_admin_grg_zipcode_prev_renw = GRG_PSTL_CD
  ```
- Line 141: lname, name
  ```
  cols <- colnames(panel_renw_input)
  ```
- Line 154: name
  ```
  , 'PRIOR_CARRIER_MIX_GROUP', 'PRIOR_CARRIER_GROUP', 'PRIOR_CARRIER_NAME'
  ```
- Line 159: loc
  ```
  'x_loc_grg_verify_ind',
  ```
- Line 160: loc
  ```
  'x_loc_grg_adrs_verify_ind',
  ```
- Line 161: loc, zip
  ```
  'x_loc_zipcd_ind',
  ```
- Line 162: loc, zip
  ```
  'x_loc_zipcd_agi',
  ```
- Line 163: loc, zip
  ```
  'x_loc_zipcd_lg_inc',
  ```
- Line 164: loc
  ```
  'x_loc_popltn_dens_grp',
  ```
- Line 165: loc
  ```
  'x_loc_popltn_dens_pct',
  ```
- Line 177: lname, name
  ```
  invariant_X_cols <- unique(intersect(invariant_X_cols, colnames(panel_input)))
  ```
- Line 185: loc, zip
  ```
  vars_X <- getX('reg')[[1]] #; X <- c(X[X != "x_loc_zipcd_agi"], "lg_inc");
  ```
- Line 186: lname, name
  ```
  print(paste("reg X not in panel_renw:", setdiff(vars_X, colnames(panel_renw_cleaned))))
  ```
- Line 194: loc
  ```
  # # , x_loc_grg_verify_ind = ifelse(CURR_GRG_VLD_SRC %in% c("A", "H", "U"), 0, 1)
  ```
- Line 195: loc
  ```
  # # , x_loc_grg_adrs_verify_ind = ifelse(GRG_ADRS_VLD_CD %in% c("A", "H", "U"), 0, 1)
  ```
- Line 196: loc, zip
  ```
  # # , x_loc_zipcd_ind = !is.na(zipcode_agi)
  ```
- Line 197: loc, zip
  ```
  # # , x_loc_zipcd_agi = ifelse(is.na(zipcode_agi), Med_zip_inc, zipcode_agi)
  ```
- Line 198: loc
  ```
  # # , x_loc_popltn_dens_grp = ifelse(is.na(DNSTY_POPLT_GRP),0,ifelse(is.na(as.numeric(DNSTY_POPLT_GR
  ```
- Line 199: loc
  ```
  # # , x_loc_popltn_dens_pct = as.numeric(DNSTY_POPLT_PCT)
  ```
- Line 244: lat
  ```
  # , pmt_late_pay_cnt = LATE_PAY_CNT
  ```
- Line 263: email
  ```
  # , disc_email_subs_ind_incpt = ifelse(is.na(EMAIL_INCP_SBSCRB), 0, ifelse(EMAIL_INCP_SBSCRB == "Y",
  ```
- Line 264: email
  ```
  # , disc_email_subs_ind = ifelse(is.na(EMAIL_SBSCRB_IND), 0, ifelse(EMAIL_SBSCRB_IND == "Y", 1, 0))
  ```
- Line 270: zip
  ```
  # , admin_zip_extend = EXTND_ZIP_CD
  ```
- Line 271: zip
  ```
  # , admin_zip_extend_last_4_digit = ZIP_CD_EXTN
  ```
- Line 318: social
  ```
  # , tier_fctr_mkt_social_grp_cd = as.factor(ifelse(SCL_GRP_CD %in% c("??", ""), "Unknown", as.charac
  ```
- Line 320: city, second
  ```
  # , tier_fctr_mkt_urban_cd = as.factor(URBN_RSD_CD) # C = Second City/Metro Suburb; R = Rural; S = M
  ```
- Line 321: community, lat
  ```
  # , tier_fctr_mkt_cmnty_cd = as.factor(CMNTY_CD) # community code (Boston, Community within MA vs wi
  ```

**/replication-package/jinvass26_replication_may_19_2016/code/simulate/functions/estimation/extract_model_estimates_cmdstan.R**

- Line 5: name
  ```
  results_dir <- file.path(MODEL_OUT_DIR, model_name)
  ```
- Line 12: name
  ```
  if(grepl("model_joint_final_", model_name)){
  ```
- Line 13: name
  ```
  model_config = gsub("model_joint_final_", "", model_name)
  ```
- Line 16: name
  ```
  } else if(grepl("^model_main", model_name)){
  ```
- Line 17: name
  ```
  model_config = gsub("^model_main_?", "", model_name)
  ```
- Line 25: name
  ```
  log_file_rda = file.path(outdir, paste0("log_", model_name, "-", log_file_suffix, ".Rda"))
  ```
- Line 28: name
  ```
  for(i in c("output_filename","est_config_summary")) {
  ```
- Line 33: name
  ```
  output_filename <- paste0(model_name, "-", log_file_suffix, ".csv")
  ```
- Line 46: name
  ```
  if(is.null(output_filename)|| is.null(outdir)){ # || is.null(data_filename)
  ```
- Line 47: name
  ```
  print("error - must specify 'output_filename' (.csv), 'data_filename' (.Rda), and put them in the sa
  ```
- Line 50: name
  ```
  tmp = file.path(outdir, output_filename)
  ```
- Line 64: name
  ```
  col_names_dot <- strsplit(header, ",")[[1]]
  ```
- Line 68: name
  ```
  col_names <- sapply(col_names_dot, function(nm) {
  ```
- Line 74: name
  ```
  }, USE.NAMES = FALSE)
  ```
- Line 76: name
  ```
  names(values) <- col_names
  ```
- Line 77: name
  ```
  point_est <- as.data.frame(as.list(values), check.names = FALSE)
  ```
- Line 80: name
  ```
  base_names <- sapply(col_names, function(nm) sub("\\[.*", "", nm))
  ```
- Line 81: name
  ```
  unique_vars <- unique(base_names)
  ```
- Line 84: name
  ```
  cols_v <- col_names[base_names == v]
  ```
- Line 124: name
  ```
  col_names = paste0(i,paste0("[",d,",",paste0((1:M), "]")))
  ```
- Line 125: name
  ```
  tmp[d,] = as.numeric(df[col_names])
  ```
- Line 131: name
  ```
  col_names = paste0(i,paste0("[", paste0((1:N), "]")))
  ```
- Line 132: name
  ```
  assign(i, as.numeric(df[col_names]))
  ```
- Line 142: name
  ```
  est_config_path <- paste0(outdir, "/", "est_config_", gsub(".csv", ".rds", output_filename))
  ```
- Line 149: block, loc
  ```
  d_by_block = if (exists("data_list")) data_list$N_by_choice_regime else NULL,
  ```
- Line 154: name
  ```
  for(k in names(est_config_obj)){
  ```
- Line 159: lat
  ```
  source("code/simulate/functions/estimation/load_estimation_bootstrap_data.R")
  ```
- Line 160: lat
  ```
  source('code/simulate/functions/estimation/load_model_data.R')
  ```
- Line 165: name
  ```
  model_filename <- if (exists("log_file_obj") && !is.null(log_file_obj$model_filename)) {
  ```
- Line 166: name
  ```
  log_file_obj$model_filename
  ```
- Line 168: name
  ```
  paste0(model_name, ".stan")
  ```
- Line 170: name
  ```
  model_expose_function_path = file.path(outdir, model_filename)
  ```
- Line 172: name
  ```
  # Try alternative naming: results dir may have model_* name
  ```
- Line 173: name
  ```
  alt_names <- list.files(outdir, pattern = "\\.stan$", full.names = TRUE)
  ```
- Line 174: name
  ```
  if (length(alt_names) >= 1) {
  ```
- Line 175: name
  ```
  model_expose_function_path <- alt_names[1]
  ```
- Line 179: name
  ```
  alt_path <- file.path("data/estimates/model_output", model_name, paste0(model_name, ".stan"))
  ```
- Line 190: lat
  ```
  # this is added functions needed only for counterfactual calculations
  ```
- Line 193: lat
  ```
  ctf_model = cmdstanr::cmdstan_model("code/simulate/functions/estimation/ctf_functions.stan", force_r
  ```
- Line 200: lat
  ```
  source('code/simulate/functions/estimation/get_c_latentparams.R')
  ```
- Line 204: lat
  ```
  source('code/simulate/functions/estimation/get_d_latentparams.R')
  ```

**/replication-package/jinvass26_replication_may_19_2016/code/simulate/functions/estimation/get_c_latentparams.R**

- Line 4: lname, name
  ```
  !"theta_0_llambda" %in% colnames(df)){
  ```
- Line 13: lname, name
  ```
  colnames(samples) = sub('.*\\.', '', colnames(samples))
  ```
- Line 16: name
  ```
  df_95 = stack(lapply(samples, quantile, prob = 0.95, names = FALSE)) %>% rename(pct95 = values)
  ```
- Line 17: name
  ```
  df_05 = stack(lapply(samples, quantile, prob = 0.05, names = FALSE)) %>% rename(pct05 = values)
  ```
- Line 18: name
  ```
  df_ci = df_95 %>% left_join(df_05) %>% select(c("ind", "pct05", "pct95")) %>% rename(est = ind)
  ```
- Line 29: name
  ```
  col_names = paste0(i,paste0("[",d,",",paste0((1:M), "]")))
  ```
- Line 30: name
  ```
  tmp[d,] = as.numeric(df[col_names])
  ```
- Line 36: name
  ```
  col_names = paste0(i,paste0("[", paste0((1:N), "]")))
  ```
- Line 37: name
  ```
  assign(i, as.numeric(df[col_names]))
  ```
- Line 51: lat
  ```
  source('code/simulate/functions/estimation/load_model_data.R')
  ```

**/replication-package/jinvass26_replication_may_19_2016/code/simulate/functions/estimation/get_cp_pred.R**

- Line 54: lat, second
  ```
  # and the CTF-facing path (get_d_latentparams.R:241): the second column is the
  ```

**/replication-package/jinvass26_replication_may_19_2016/code/simulate/functions/estimation/get_d_latentparams.R**

- Line 1: lat
  ```
  #### 4. Calculate choice likelihood ####
  ```
- Line 91: block, loc
  ```
  leps_risk_aversion, d_by_block) #d_subsample_size, d_subsample_block5, d_subsample_block6
  ```
- Line 102: block, loc
  ```
  eps_mm, d_by_block) #d_subsample_size, d_subsample_block5, d_subsample_block6)
  ```
- Line 119: block, loc
  ```
  eps_eta, d_by_block)
  ```
- Line 189: block, loc
  ```
  #                                                 eps_xi, d_by_block)
  ```
- Line 196: block, loc
  ```
  rep(0,d_by_block[2] + d_by_block[4]),
  ```
- Line 197: block, loc
  ```
  d_by_block[2], d_by_block[4]);
  ```

**/replication-package/jinvass26_replication_may_19_2016/code/simulate/functions/estimation/get_d_pred.R**

- Line 19: lat
  ```
  #### CALCULATE CHOICE LIKELIHOODS ####
  ```
- Line 21: block, loc
  ```
  fit_sample_size_by_block = d_by_block #d_by_block is the max of subsample size and block size...
  ```
- Line 22: block, loc
  ```
  fit_sample_size_by_block[3] = 0 #...except for the third block, which was only ~ 1000 obs so did not
  ```
- Line 79: zip
  ```
  zip_inc_norm[n0:n1],
  ```
- Line 92: block, loc
  ```
  n0_fit = n0; n1_fit = n0_fit-1+fit_sample_size_by_block[k]
  ```
- Line 93: block, loc
  ```
  n0_cv = n1_fit+1; n1_cv = n0_cv-1+cv_sample_size_by_block[k]; if(n1_cv>n1){print("error")}
  ```
- Line 95: block, loc
  ```
  if(fit_sample_size_by_block[k]>0){
  ```
- Line 96: block, loc
  ```
  log_choice_probs_fit[[k]] = log_choice_probs[[k]][1:fit_sample_size_by_block[k],]
  ```
- Line 97: block, loc
  ```
  for(n in 1:fit_sample_size_by_block[k]){
  ```
- Line 103: block, loc
  ```
  if(cv_sample_size_by_block[k]>0){
  ```
- Line 104: block, loc
  ```
  log_choice_probs_cv[[k]] = log_choice_probs[[k]][(fit_sample_size_by_block[k]+1):(fit_sample_size_by
  ```
- Line 105: block, loc
  ```
  for(n in 1:cv_sample_size_by_block[k]){
  ```
- Line 135: zip
  ```
  zip_inc_norm[n0:n1],
  ```
- Line 142: second
  ```
  invisible(gc())  # pre-clean heap before second Stan call
  ```
- Line 157: zip
  ```
  zip_inc_norm[n0:n1],
  ```
- Line 178: block, loc
  ```
  n0_fit = n0; n1_fit = n0_fit-1+fit_sample_size_by_block[k]
  ```
- Line 179: block, loc
  ```
  n0_cv = n1_fit+1; n1_cv = n0_cv-1+cv_sample_size_by_block[k]; if(n1_cv>n1){print("error")}
  ```
- Line 180: block, loc
  ```
  if(fit_sample_size_by_block[k]>0){
  ```
- Line 181: block, loc
  ```
  log_choice_probs_fit[[k]] = log_choice_probs[[k]][1:fit_sample_size_by_block[k],]
  ```
- Line 182: block, loc
  ```
  for(n in 1:fit_sample_size_by_block[k]){
  ```
- Line 190: block, loc
  ```
  if(cv_sample_size_by_block[k]>0){
  ```
- Line 191: block, loc
  ```
  log_choice_probs_cv[[k]] = log_choice_probs[[k]][(fit_sample_size_by_block[k]+1):(fit_sample_size_by
  ```
- Line 192: block, loc
  ```
  for(n in 1:cv_sample_size_by_block[k]){
  ```
- Line 232: zip
  ```
  zip_inc_norm[n0:n1],
  ```
- Line 254: zip
  ```
  zip_inc_norm[n0:n1],
  ```
- Line 275: block, loc
  ```
  n0_fit = n0; n1_fit = n0_fit-1+fit_sample_size_by_block[k]
  ```
- Line 276: block, loc
  ```
  n0_cv = n1_fit+1; n1_cv = n0_cv-1+cv_sample_size_by_block[k]; if(n1_cv>n1){print("error")}
  ```
- Line 277: block, loc
  ```
  if(fit_sample_size_by_block[k]>0){
  ```
- Line 278: block, loc
  ```
  log_choice_probs_fit[[k]] = log_choice_probs[[k]][1:fit_sample_size_by_block[k],]
  ```
- Line 279: block, loc
  ```
  for(n in 1:fit_sample_size_by_block[k]){
  ```
- Line 289: block, loc
  ```
  if(cv_sample_size_by_block[k]>0){
  ```
- Line 290: block, loc
  ```
  log_choice_probs_cv[[k]] = log_choice_probs[[k]][(fit_sample_size_by_block[k]+1):(fit_sample_size_by
  ```
- Line 291: block, loc
  ```
  for(n in 1:cv_sample_size_by_block[k]){
  ```
- Line 303: block, loc
  ```
  perobs_fit_choice = sum(log_likelihoods_choice_fit)/sum(fit_sample_size_by_block)
  ```
- Line 304: block, loc
  ```
  perobs_cv_choice = sum(log_likelihoods_choice_cv)/sum(cv_sample_size_by_block)
  ```
- Line 305: block, loc
  ```
  perobs_fit_clm = sum(log_likelihoods_clm_count_fit)/sum(fit_sample_size_by_block)
  ```
- Line 306: block, loc
  ```
  perobs_cv_clm = sum(log_likelihoods_clm_count_cv)/sum(cv_sample_size_by_block)
  ```
- Line 307: block, loc
  ```
  perobs_fit_clm_severe = sum(log_likelihoods_clm_count_severe_fit)/sum(fit_sample_size_by_block)
  ```
- Line 308: block, loc
  ```
  perobs_cv_clm_severe = sum(log_likelihoods_clm_count_severe_cv)/sum(cv_sample_size_by_block)
  ```

**/replication-package/jinvass26_replication_may_19_2016/code/simulate/functions/estimation/get_fit_cp.R**

- Line 11: lat
  ```
  ## Requires: get_c_latentparams.R, get_cp_pred.R already set up the environment
  ```
- Line 14: lat
  ```
  source('code/simulate/functions/estimation/get_c_latentparams.R')
  ```
- Line 15: lat
  ```
  source('code/simulate/functions/estimation/get_cp_pred.R')
  ```
- Line 27: lat
  ```
  ## sim_c7b's later call would clobber sim_c7's version with mhhet-context
  ```
- Line 30: lat
  ```
  ## see that file's header for full rationale and isolation guarantees.
  ```
- Line 38: block, loc
  ```
  ## (see comment block above), so it's not in this gate.
  ```
- Line 39: name
  ```
  .skip_fig_writes <- exists("model_name") && identical(model_name, "model_main_mhhet")
  ```
- Line 51: name
  ```
  write.csv(fig_6b_csv, file.path(MODEL_FIT_DIR, "fig_6b.csv"), row.names = FALSE)
  ```
- Line 58: lname, name
  ```
  p_R_nb_mean = matrix(rep(0, I * D_R_nb_scheme), I, D_R_nb_scheme); colnames(p_R_nb_mean) = seq(D_R_n
  ```
- Line 59: lname, name
  ```
  p_R_tm_mean = matrix(rep(0, I * D_R_tm_scheme), I, D_R_tm_scheme); colnames(p_R_tm_mean) = paste0("t
  ```
- Line 60: lname, name
  ```
  p_R_nb_sd = matrix(rep(0, I * D_R_nb_scheme), I, D_R_nb_scheme); colnames(p_R_nb_sd) = seq(D_R_nb_sc
  ```
- Line 61: lname, name
  ```
  p_R_tm_sd = matrix(rep(0, I * D_R_tm_scheme), I, D_R_tm_scheme); colnames(p_R_tm_sd) = paste0("tm", 
  ```
- Line 62: lname, name
  ```
  log_score_mean = matrix(rep(0, I * D_R_tm_scheme), I, D_R_tm_scheme); colnames(log_score_mean) = pas
  ```
- Line 63: lname, name
  ```
  log_score_sd = matrix(rep(0, I * D_R_tm_scheme), I, D_R_tm_scheme); colnames(log_score_sd) = paste0(
  ```
- Line 118: name
  ```
  write.csv(fig_b4_csv, file.path(MODEL_FIT_DIR, "fig_b4.csv"), row.names = FALSE)
  ```
- Line 126: name
  ```
  write.csv(fig_6a_csv, file.path(MODEL_FIT_DIR, "fig_6a.csv"), row.names = FALSE)
  ```

**/replication-package/jinvass26_replication_may_19_2016/code/simulate/functions/estimation/load_estimation_bootstrap_data.R**

- Line 2: lat, son
  ```
  # Load data_list: cache -> canonical -> JSON (simulated).
  ```
- Line 3: lat
  ```
  if (USE_SIMULATED_DATA) {
  ```
- Line 5: son
  ```
  dl_json <- file.path(SIM_CACHE_DIR, "data_list_IL.json")
  ```
- Line 8: son
  ```
  } else if (file.exists(dl_json)) {
  ```
- Line 9: son
  ```
  data_list <- read_data_list(dl_json)
  ```
- Line 11: lat
  ```
  stop("Simulated data_list not found.\n",
  ```
- Line 12: lat
  ```
  "  Run the simulation pipeline first:\n",
  ```
- Line 13: lat
  ```
  "  Rscript code/simulate/sim_c6_estimate.R")
  ```
- Line 28: zip
  ```
  temp <- cbind.data.frame(data_list$zip_income, data_list$d_t_new)
  ```
- Line 38: lname, name
  ```
  colnames(temp) <- c("income", "index")
  ```
- Line 42: lname, name
  ```
  colnames(dimtable) <- c("index", "cov")
  ```
- Line 67: name
  ```
  mhhet_files <- list.files(COST_MHHET_MODEL_DIR, pattern = "^result_as_init.*\\.Rda$", full.names = T
  ```
- Line 77: lname, name
  ```
  full_oo_cov_name_1 = colnames(data_list$prices_oo_full)
  ```
- Line 78: name
  ```
  full_oo_cov_name_2 = full_oo_cov_name_1[!grepl("_3",full_oo_cov_name_1)]
  ```
- Line 80: name
  ```
  firm_indices = str_extract(full_oo_cov_name_2[1:5], "^[^_]+")
  ```
- Line 81: name
  ```
  min_price_firm_index = which.min(colMeans(data_list$prices_oo_nb_tm_full_2[,grepl("_4", full_oo_cov_
  ```
- Line 83: name
  ```
  median_price_firm_index = which.median(colMeans(data_list$prices_oo_nb_tm_full_2[,grepl("_4", full_o
  ```
- Line 85: name
  ```
  min_price_firm_cov_indices = grepl(paste0(firm_indices[min_price_firm_index],"_"),full_oo_cov_name_1
  ```
- Line 86: name
  ```
  median_price_firm_cov_indices = grepl(paste0(firm_indices[median_price_firm_index], "_"), full_oo_co
  ```
- Line 94: name
  ```
  min_price_firm_cov_indices = grepl(paste0(firm_indices[min_price_firm_index],"_"),full_oo_cov_name_2
  ```
- Line 95: name
  ```
  median_price_firm_cov_indices = grepl(paste0(firm_indices[median_price_firm_index], "_"), full_oo_co
  ```
- Line 113: lat
  ```
  if (!exists("parse_bootstrap_csv")) source("code/simulate/functions/sim_helpers.R")
  ```

**/replication-package/jinvass26_replication_may_19_2016/code/simulate/functions/estimation/load_model_data.R**

- Line 1: name
  ```
  data_obj_names = c(
  ```
- Line 32: block, loc
  ```
  # "d_subsample_size", "d_subsample_w_renw_1", "d_subsample_w_renw_2", "d_subsample_w_renw_4", "d_sub
  ```
- Line 34: zip
  ```
  "zip_income", "renw_cnt_choice", "renw_cnt_choice59", "renw_cnt_choice0", "renw_cnt_choice1", "X_ren
  ```
- Line 38: name
  ```
  names(data_list)[grepl("oo", names(data_list)) & grepl("full", names(data_list))],
  ```
- Line 39: block, loc
  ```
  "dollar_norm", "run_demand_blocks", "d_tm_mh_rational_ind",  "d_by_block", "d_by_block_w_renw"
  ```
- Line 43: name
  ```
  data_obj_names = c(data_obj_names, "theta_mh_llambda")
  ```
- Line 47: name
  ```
  data_obj_names = c(data_obj_names, "discount_factor")
  ```
- Line 51: name
  ```
  data_obj_names = c(data_obj_names, "theta_0_R_nb_mean", "theta_1_R_nb_mean",   "theta_0_R_nb_sd",   
  ```
- Line 55: name
  ```
  data_obj_names = c(data_obj_names, "theta_0_log_score_sd")
  ```
- Line 58: name
  ```
  data_obj_names = c(data_obj_names, "theta_0_pareto_alpha_severe", "theta_1_pareto_alpha_severe",
  ```
- Line 63: name
  ```
  data_obj_names = c(data_obj_names, "theta_0_log_score_mean", "theta_1_log_score_mean")
  ```
- Line 67: name
  ```
  data_obj_names = c(data_obj_names, "base_reg_factor_c")
  ```
- Line 70: name
  ```
  data_obj_names = c(data_obj_names, "base_reg_factor_c")
  ```
- Line 73: name
  ```
  data_obj_names = c(data_obj_names, "base_reg_factor_c")
  ```
- Line 76: name
  ```
  data_obj_names = c(data_obj_names, "eps_prior_sd")
  ```
- Line 79: name
  ```
  data_obj_names = c(data_obj_names, "eps_ra_sd")
  ```
- Line 88: name
  ```
  for(k in c(data_obj_names)){
  ```
- Line 93: name
  ```
  for(k in c(data_obj_names)){
  ```
- Line 101: zip
  ```
  zip_inc_norm = zip_income/dollar_norm
  ```
- Line 146: block, loc
  ```
  if(is.null(d_by_block)){
  ```
- Line 147: name
  ```
  for(k in names(est_config_obj)){
  ```

**/replication-package/jinvass26_replication_may_19_2016/code/simulate/functions/load_sim_data.R**

- Line 4: name
  ```
  ## Reads data_rf.csv once, caches in memory, returns column subsets by panel_name.
  ```
- Line 13: name
  ```
  load_sim_data <- function(panel_name, data_states = "us", dbug_pct = 1) {
  ```
- Line 28: lname, name
  ```
  viol_cols <- grep("^VIOL_", colnames(data_rf), value = TRUE)
  ```
- Line 29: lname, name
  ```
  renw_qt_cols <- grep("^RENW_QT_", colnames(data_rf), value = TRUE)
  ```
- Line 45: name
  ```
  "PRIOR_CARRIER_NAME", "REQ_DWNPMT_PCT")
  ```
- Line 47: name
  ```
  if (panel_name == "panel") {
  ```
- Line 50: lname, name
  ```
  cols <- setdiff(colnames(data_rf), exclude)
  ```
- Line 52: lname, loc, name, zip
  ```
  if ("x_loc_zipcd_agi" %in% colnames(dataset) && !"x_loc_zipcd_lg_inc" %in% colnames(dataset))
  ```
- Line 53: loc, zip
  ```
  dataset$x_loc_zipcd_lg_inc <- log(dataset$x_loc_zipcd_agi)
  ```
- Line 55: name
  ```
  } else if (panel_name == "panel_viol") {
  ```
- Line 61: name
  ```
  } else if (panel_name == "panel_exps") {
  ```
- Line 63: lname, name
  ```
  dataset <- data_rf[, intersect(cols, colnames(data_rf))]
  ```
- Line 65: name
  ```
  } else if (panel_name == "panel_renw") {
  ```
- Line 72: name
  ```
  } else if (panel_name == "panel_ubi_renw") {
  ```
- Line 76: lname, name
  ```
  cols <- intersect(ubi_cols, colnames(data_rf))
  ```
- Line 78: lname, name
  ```
  if ("UBI_ENROLL_IND" %in% colnames(dataset))
  ```
- Line 82: name
  ```
  } else if (panel_name == "choice_panel") {
  ```
- Line 86: lname, name
  ```
  cols <- setdiff(colnames(data_rf), exclude)
  ```
- Line 89: name
  ```
  } else if (panel_name == "df_mh") {
  ```
- Line 96: lname, name
  ```
  if ("ubi_fin_ind_drv" %in% colnames(data_rf))
  ```
- Line 98: lname, name
  ```
  cols <- intersect(df_mh_cols, colnames(data_rf))
  ```
- Line 101: lname, name
  ```
  if ("ubi_fin_ind_drv" %in% colnames(dataset))
  ```
- Line 102: lname, name
  ```
  colnames(dataset)[colnames(dataset) == "ubi_fin_ind_drv"] <- "ubi_fin_ind"
  ```
- Line 104: name
  ```
  } else if (panel_name == "st_ubi_dates") {
  ```
- Line 106: lname, name
  ```
  cols <- intersect(cols, colnames(data_rf))
  ```
- Line 112: name
  ```
  stop("[load_sim_data] Unknown panel_name: ", panel_name)
  ```
- Line 116: lname, name
  ```
  if (data_states != "us" && "state_alpha" %in% colnames(dataset)) {
  ```
- Line 118: son
  ```
  rf_prof <- jsonlite::fromJSON(file.path(SIM_DATA_DIR, "data_profile_rf.json"))
  ```
- Line 132: name
  ```
  cat("[load_sim_data]", panel_name, ":", nrow(dataset), "rows,", ncol(dataset), "cols\n")
  ```

**/replication-package/jinvass26_replication_may_19_2016/code/simulate/functions/sim_helpers.R**

- Line 2: lat
  ```
  ## sim_helpers.R — Shared helper functions for simulation/estimation scrip
  ```
- Line 6: son
  ```
  ##   format_param_comparison()  — Format true-vs-estimated parameter tab
  ```
- Line 7: son
  ```
  ##   print_param_comparison()   — Print formatted comparison tab
  ```
- Line 12: name
  ```
  ##   parse_bootstrap_csv()      — Parse CmdStan bootstrap CSV into named param li
  ```
- Line 20: name
  ```
  if (!requireNamespace("stringr", quietly = TRUE)) {
  ```
- Line 29: lname, name
  ```
  full_oo_cov_name_1 <- colnames(dl$prices_oo_full)
  ```
- Line 30: name
  ```
  full_oo_cov_name_2 <- full_oo_cov_name_1[!grepl("_3", full_oo_cov_name_1)]
  ```
- Line 31: name
  ```
  firm_indices <- stringr::str_extract(full_oo_cov_name_2[1:5], "^[^_]+")
  ```
- Line 34: name
  ```
  colMeans(dl$prices_oo_nb_tm_full_2[, grepl("_4", full_oo_cov_name_2)])
  ```
- Line 37: name
  ```
  colMeans(dl$prices_oo_nb_tm_full_2[, grepl("_4", full_oo_cov_name_2)])
  ```
- Line 40: name
  ```
  min_cov_1 <- grepl(paste0(firm_indices[min_price_firm_index], "_"), full_oo_cov_name_1)
  ```
- Line 41: name
  ```
  med_cov_1 <- grepl(paste0(firm_indices[median_price_firm_index], "_"), full_oo_cov_name_1)
  ```
- Line 50: name
  ```
  min_cov_2 <- grepl(paste0(firm_indices[min_price_firm_index], "_"), full_oo_cov_name_2)
  ```
- Line 51: name
  ```
  med_cov_2 <- grepl(paste0(firm_indices[median_price_firm_index], "_"), full_oo_cov_name_2)
  ```
- Line 63: son
  ```
  ## ---- Parameter comparison ---------------------------------------------------
  ```
- Line 65: son
  ```
  format_param_comparison <- function(fit_params, true_params, scalar_params,
  ```
- Line 67: son
  ```
  comparison <- data.frame(
  ```
- Line 75: name
  ```
  for (pname in names(scalar_params)) {
  ```
- Line 76: name
  ```
  col_name <- scalar_params[[pname]]
  ```
- Line 77: lname, name
  ```
  if (col_name %in% colnames(fit_params) && !is.null(true_params[[pname]])) {
  ```
- Line 78: name
  ```
  est_val  <- as.numeric(fit_params[[col_name]])
  ```
- Line 79: name
  ```
  true_val <- true_params[[pname]]
  ```
- Line 80: son
  ```
  comparison <- rbind(comparison, data.frame(
  ```
- Line 81: name
  ```
  parameter = pname, true_value = round(true_val, 6),
  ```
- Line 91: name
  ```
  col_name <- paste0(ap$name, "[", d, "]")
  ```
- Line 92: lname, name
  ```
  if (col_name %in% colnames(fit_params)) {
  ```
- Line 93: name
  ```
  est_val  <- as.numeric(fit_params[[col_name]])
  ```
- Line 95: son
  ```
  comparison <- rbind(comparison, data.frame(
  ```
- Line 96: name
  ```
  parameter = col_name, true_value = round(true_val, 6),
  ```
- Line 104: son
  ```
  comparison
  ```
- Line 107: son
  ```
  print_param_comparison <- function(comparison) {
  ```
- Line 110: son
  ```
  for (i in seq_len(nrow(comparison))) {
  ```
- Line 112: son
  ```
  comparison$parameter[i], comparison$true_value[i],
  ```
- Line 113: son
  ```
  comparison$estimated[i], comparison$abs_diff[i]))
  ```
- Line 143: name
  ```
  #' Parse a CmdStan bootstrap CSV into a named list of R objects.
  ```
- Line 158: name
  ```
  col_names_dot <- strsplit(header, ",")[[1]]
  ```
- Line 162: name
  ```
  col_names <- sapply(col_names_dot, function(nm) {
  ```
- Line 168: name
  ```
  }, USE.NAMES = FALSE)
  ```
- Line 170: name
  ```
  names(values) <- col_names
  ```
- Line 173: name
  ```
  base_names <- sapply(col_names, function(nm) sub("\\[.*", "", nm))
  ```
- Line 174: name
  ```
  unique_vars <- unique(base_names)
  ```
- Line 177: name
  ```
  cols_v <- col_names[base_names == v]
  ```
- Line 191: name
  ```
  # Extract to named list of R objects
  ```
- Line 240: name
  ```
  for (v in names(sev)) data_list[[v]] <- sev[[v]]
  ```
- Line 264: name
  ```
  for (v in names(price)) data_list[[v]] <- price[[v]]
  ```
- Line 272: son
  ```
  ## ---- JSON data_list reader --------------------------------------------------
  ```
- Line 274: son
  ```
  read_data_list <- function(json_path, cache_dir = NULL) {
  ```
- Line 276: son
  ```
  rds_path <- sub("\\.json$", ".rds", json_path)
  ```
- Line 278: son
  ```
  (!file.exists(json_path) || file.mtime(rds_path) >= file.mtime(json_path))) {
  ```
- Line 282: name, son
  ```
  if (!requireNamespace("jsonlite", quietly = TRUE))
  ```
- Line 283: son
  ```
  stop("jsonlite package required")
  ```
- Line 285: son
  ```
  dl <- jsonlite::fromJSON(json_path, simplifyVector = TRUE)
  ```
- Line 288: name
  ```
  for (nm in names(dl)) {
  ```
- Line 306: name
  ```
  # Restore matrix column names
  ```
- Line 309: lname, name
  ```
  if (!is.null(dl$prices_raw) && is.null(colnames(dl$prices_raw)))
  ```
- Line 310: lname, name
  ```
  colnames(dl$prices_raw) <- as.character(seq_len(ncol(dl$prices_raw)))
  ```
- Line 311: lname, name
  ```
  if (!is.null(dl$prices_oo_full) && is.null(colnames(dl$prices_oo_full)))
  ```
- Line 312: lname, name
  ```
  colnames(dl$prices_oo_full) <- as.vector(outer(OO_FIRMS, oo_covs, paste, sep = "_"))
  ```

**/replication-package/jinvass26_replication_may_19_2016/code/simulate/sim_c0_sum_stat.R**

- Line 8: son
  ```
  ##   RF_CSV_DIR/appendix/tab_a1a2_summary.json  (Appendix A.1/A.2: X variables)
  ```
- Line 16: lat
  ```
  source("code/simulate/functions/load_sim_data.R")
  ```
- Line 18: son
  ```
  ipak(c("jsonlite"))
  ```
- Line 42: lname, name
  ```
  if (!cc %in% colnames(panel)) panel[[cc]] <- 0
  ```
- Line 46: lname, name
  ```
  panel$pct_month_count <- if ("WRT_EXPS_CNT" %in% colnames(panel)) {
  ```
- Line 93: name
  ```
  sum_stat_row <- function(x, varname, restrict, winsor_probs, N_all) {
  ```
- Line 110: name
  ```
  return(data.frame(Variable = varname, Mean = NA, SD = NA,
  ```
- Line 116: name
  ```
  Variable = varname,
  ```
- Line 120: name
  ```
  p50      = round(quantile(x, 0.50, na.rm = TRUE, names = FALSE), 2),
  ```
- Line 121: name
  ```
  p75      = round(quantile(x, 0.75, na.rm = TRUE, names = FALSE), 2),
  ```
- Line 133: lname, name
  ```
  tenure_vec <- if ("last_renewal_seen" %in% colnames(df_mh)) {
  ```
- Line 140: lname, name
  ```
  cov_vec <- if ("COV_BI_LIM_D_NBR" %in% colnames(panel)) {
  ```
- Line 147: lname, name
  ```
  mm_vec <- if ("cov_mm_liab_ind" %in% colnames(panel)) {
  ```
- Line 154: lname, name
  ```
  fc_vec <- if ("cov_ind_fullcov" %in% colnames(panel)) {
  ```
- Line 175: name
  ```
  rownames(tab_1a) <- NULL
  ```
- Line 197: lname, name
  ```
  tmp <- grepl("6_", colnames(data_list$prices_oo_full))
  ```
- Line 219: name
  ```
  claim_var <- if ("claim_liab" %in% names(panel_il)) "claim_liab" else "claim"
  ```
- Line 303: lname, name
  ```
  if ("x_cred_ind" %in% colnames(df)) {
  ```
- Line 308: lname, name
  ```
  if ("tier_acci_drvr_pt_2" %in% colnames(df)) {
  ```
- Line 313: lname, name
  ```
  if ("tier_pop_lngth" %in% colnames(df)) {
  ```
- Line 321: name
  ```
  list(var = "x_drvr_is_female",     name = "Female Ind."),
  ```
- Line 322: name
  ```
  list(var = "x_drvr_hm_own_ind",    name = "Homeowner Ind."),
  ```
- Line 323: name
  ```
  list(var = "x_drvr_lic_oos_ind",   name = "Out-of-state Ind."),
  ```
- Line 324: name
  ```
  list(var = "x_cred_clue_ord_ind",  name = "Credit Report Ind."),
  ```
- Line 325: name
  ```
  list(var = "x_veh_ls_pay_ind",     name = "Vehicle on Lease Ind."),
  ```
- Line 326: name
  ```
  list(var = "x_veh_abs_ind",        name = "ABS Ind."),
  ```
- Line 327: name
  ```
  list(var = "x_veh_sd_ind",         name = "Safe Device Ind."),
  ```
- Line 328: name
  ```
  list(var = "x_veh_class_C_ind",    name = "Class C Vehicle Ind."),
  ```
- Line 329: loc, name
  ```
  list(var = "x_loc_grg_verify_ind", name = "Garage Verification Ind."),
  ```
- Line 330: name
  ```
  list(var = "tier_pop_yes_ind",     name = "Prior Insurance Ind."),
  ```
- Line 331: name
  ```
  list(var = "tier_pop_some_ind",    name = "Prior Insurance with Lapse Ind."),
  ```
- Line 332: name
  ```
  list(var = "tier_pref_ind",        name = "Preferred Customer Ind."),
  ```
- Line 333: name
  ```
  list(var = "tier_acci_dui_cnt",    name = "Record - DUI Count"),
  ```
- Line 334: name
  ```
  list(var = "tier_pop_lngth_ind",   name = "Length of Prior Insurance"),
  ```
- Line 335: name
  ```
  list(var = "tier_good_ind",        name = "Clean Record Ind."),
  ```
- Line 336: name
  ```
  list(var = "age_young_ind",        name = "Age $<$ 25 Ind."),
  ```
- Line 337: name
  ```
  list(var = "age_adult_ind",        name = "Age $\\geq$ 21 Ind."),
  ```
- Line 338: name
  ```
  list(var = "age_senior_ind",       name = "Age $>$ 60 Ind."),
  ```
- Line 339: name
  ```
  list(var = "edu_college_ind",      name = "College Ind."),
  ```
- Line 340: name
  ```
  list(var = "edu_postgrad_ind",     name = "Post Grad Ind."),
  ```
- Line 341: name
  ```
  list(var = "cred_avail_ind",       name = "Insurance History Avail. Ind.")
  ```
- Line 345: lname, name
  ```
  if (bv$var %in% colnames(df)) {
  ```
- Line 348: name
  ```
  list(name = bv$name, mean = round(mean(vals), 2))
  ```
- Line 350: name
  ```
  list(name = bv$name, mean = NA)
  ```
- Line 355: zip
  ```
  ## Derived continuous: calendar month, zipcode income ($'000), log zipcode income, risk class
  ```
- Line 356: lname, name
  ```
  if ("date_pol_eff" %in% colnames(df)) {
  ```
- Line 361: lname, loc, name, zip
  ```
  if ("x_loc_zipcd_agi" %in% colnames(df)) {
  ```
- Line 362: loc, zip
  ```
  df$zip_inc_k <- df$x_loc_zipcd_agi / 1000
  ```
- Line 364: zip
  ```
  df$zip_inc_k <- rep(NA, nrow(df))
  ```
- Line 366: lname, loc, name, zip
  ```
  if ("x_loc_zipcd_lg_inc" %in% colnames(df)) {
  ```
- Line 367: loc, zip
  ```
  df$lg_zip_inc <- df$x_loc_zipcd_lg_inc
  ```
- Line 368: lname, loc, name, zip
  ```
  } else if ("x_loc_zipcd_agi" %in% colnames(df)) {
  ```
- Line 369: loc, zip
  ```
  df$lg_zip_inc <- log(df$x_loc_zipcd_agi)
  ```
- Line 371: zip
  ```
  df$lg_zip_inc <- rep(NA, nrow(df))
  ```
- Line 375: lname, name
  ```
  if ("prem_bi_ern" %in% colnames(df) && "prem_pd_ern" %in% colnames(df)) {
  ```
- Line 381: lat
  ```
  ## Vehicle model year as age (relative to reference)
  ```
- Line 382: lname, name
  ```
  if ("x_veh_mdl_yr" %in% colnames(df)) {
  ```
- Line 389: name
  ```
  list(var = "x_drvr_yr_edu",          name = "Years of Edu."),
  ```
- Line 390: name
  ```
  list(var = "veh_mdl_yr_use",         name = "Vehicle Model Year"),
  ```
- Line 391: name
  ```
  list(var = "x_veh_len_own",          name = "Ownership Length Cat."),
  ```
- Line 392: name
  ```
  list(var = "tier_acci_tot_aaf_cnt",  name = "Record: At-Fault Accident Count"),
  ```
- Line 393: name
  ```
  list(var = "x_cred_score",           name = "Driver Credit Tier"),
  ```
- Line 394: lat, loc, name
  ```
  list(var = "x_loc_popltn_dens_pct",  name = "Population Density Percentile"),
  ```
- Line 395: name
  ```
  list(var = "tier_acci_drvr_pt",      name = "Record: Accident Points"),
  ```
- Line 396: name
  ```
  list(var = "cal_month",              name = "Calendar Month"),
  ```
- Line 397: name, zip
  ```
  list(var = "zip_inc_k",              name = "Zipcode Income (\\$'000)"),
  ```
- Line 398: name, zip
  ```
  list(var = "lg_zip_inc",             name = "Log Zipcode Income"),
  ```
- Line 399: name
  ```
  list(var = "rc",                     name = "Risk Class")
  ```
- Line 403: lname, name
  ```
  if (cv$var %in% colnames(df)) {
  ```
- Line 407: name
  ```
  return(list(name = cv$name, mean = NA, sd = NA, min = NA,
  ```
- Line 411: name
  ```
  name = cv$name,
  ```
- Line 415: name
  ```
  p25  = round(quantile(vals, 0.25, names = FALSE), 2),
  ```
- Line 416: name
  ```
  p50  = round(quantile(vals, 0.50, names = FALSE), 2),
  ```
- Line 417: name
  ```
  p75  = round(quantile(vals, 0.75, names = FALSE), 2),
  ```
- Line 422: name
  ```
  list(name = cv$name, mean = NA, sd = NA, min = NA,
  ```
- Line 428: lname, name
  ```
  age_mean <- if ("age_adult_ind" %in% colnames(df)) {
  ```
- Line 466: lname, name
  ```
  if (v %in% colnames(df)) out[[v]] <- df[[v]]
  ```
- Line 471: lname, name, son
  ```
  out$trend_season    <- if ("date_pol_eff" %in% colnames(df)) {
  ```
- Line 475: lname, name
  ```
  out$tier_good_ind    <- if ("tier_acci_drvr_pt_2" %in% colnames(df)) {
  ```
- Line 485: lname, name
  ```
  out$tier_pop_lngth_ind <- if ("tier_pop_lngth" %in% colnames(df)) {
  ```
- Line 491: lname, name
  ```
  if (v %in% colnames(df)) out[[v]] <- df[[v]]
  ```
- Line 495: zip
  ```
  ## Additional continuous: zip_inc, lg_zip_inc, rc, tier_acci_drvr_pt_raw
  ```
- Line 496: lname, loc, name, zip
  ```
  out$zip_inc <- if ("x_loc_zipcd_agi" %in% colnames(df)) {
  ```
- Line 497: loc, zip
  ```
  df$x_loc_zipcd_agi / 1000
  ```
- Line 500: lname, loc, name, zip
  ```
  out$lg_zip_inc <- if ("x_loc_zipcd_lg_inc" %in% colnames(df)) {
  ```
- Line 501: loc, zip
  ```
  df$x_loc_zipcd_lg_inc
  ```
- Line 502: lname, loc, name, zip
  ```
  } else if ("x_loc_zipcd_agi" %in% colnames(df)) {
  ```
- Line 503: loc, zip
  ```
  log(df$x_loc_zipcd_agi)
  ```
- Line 506: lname, name
  ```
  out$rc <- if ("prem_bi_ern" %in% colnames(df) && "prem_pd_ern" %in% colnames(df)) {
  ```
- Line 510: lname, name
  ```
  out$tier_acci_drvr_pt_raw <- if ("tier_acci_drvr_pt" %in% colnames(df)) {
  ```
- Line 521: loc, zip
  ```
  raw_to_drop <- c("x_drvr_lic_yr", "x_drvr_age_rated", "x_loc_zipcd_agi")
  ```
- Line 522: lname, name
  ```
  raw_to_drop <- intersect(raw_to_drop, colnames(X_mat_prenorm))
  ```
- Line 531: zip
  ```
  cont_cols_to_norm <- c(X_cont, "zip_inc", "lg_zip_inc", "rc", "tier_acci_drvr_pt_raw",
  ```
- Line 532: son
  ```
  "trend_season")
  ```
- Line 533: lname, name
  ```
  cont_cols_to_norm <- intersect(cont_cols_to_norm, colnames(X_mat_norm))
  ```
- Line 546: lname, name
  ```
  if ("tier_pop_lngth" %in% colnames(X_mat_norm)) {
  ```
- Line 550: lname, name
  ```
  if ("tier_pop_lngth_ind" %in% colnames(X_mat_norm)) {
  ```
- Line 603: lname, name
  ```
  if (!"date_yr" %in% colnames(df)) {
  ```
- Line 606: lname, name
  ```
  if (!"date_mon" %in% colnames(df)) {
  ```
- Line 609: lname, name
  ```
  join_cols <- intersect(c("ST_CD", "date_yr", "date_mon"), colnames(ubi_vers_st_dt_sum))
  ```
- Line 610: lname, name
  ```
  if (length(join_cols) > 0 && "ubi_vers_st" %in% colnames(ubi_vers_st_dt_sum)) {
  ```
- Line 616: lname, name
  ```
  df$ubi_vers_st <- if ("ubi_rate_dvc_vers" %in% colnames(df)) df$ubi_rate_dvc_vers else NA
  ```
- Line 620: lname, name
  ```
  if (!"cov_ind_fullcov" %in% colnames(df)) df$cov_ind_fullcov <- NA_real_
  ```
- Line 658: name
  ```
  write.csv(tab_1a, file.path(RF_CSV_DIR, "tab_1_panel_a.csv"), row.names = FALSE)
  ```
- Line 661: name
  ```
  write.csv(tab_1b, file.path(RF_CSV_DIR, "tab_1_panel_b.csv"), row.names = FALSE)
  ```
- Line 664: name
  ```
  write.csv(tab_meta, file.path(RF_CSV_DIR, "tab_1_meta.csv"), row.names = FALSE)
  ```
- Line 667: son
  ```
  jsonlite::write_json(tab_a1a2, file.path(RF_CSV_DIR, "appendix", "tab_a1a2_summary.json"),
  ```
- Line 669: son
  ```
  cat("    ->", file.path(RF_CSV_DIR, "appendix", "tab_a1a2_summary.json"), "\n")
  ```

**/replication-package/jinvass26_replication_may_19_2016/code/simulate/sim_c1_mh_regression.R**

- Line 2: lat
  ```
  ## sim_c1_mh_regression.R — Moral hazard regressions on simulated da
  ```
- Line 14: son
  ```
  ## Also produces JSON outputs in RF_REG_DIR (backward compatibility):
  ```
- Line 15: son
  ```
  ##   tab_2_regression.json, tab_3_regression.json, fig_4_regression.json
  ```
- Line 16: son
  ```
  ##   appendix/fig_c1_regression.json, appendix/tab_c1_regression.json
  ```
- Line 17: son
  ```
  ##   appendix/fig_c2_c3_regression.json
  ```
- Line 22: lat
  ```
  source("code/simulate/functions/load_sim_data.R")
  ```
- Line 24: son
  ```
  ipak(c("fixest", "jsonlite"))
  ```
- Line 26: son
  ```
  ## ---- Helper: extract regression summary as list (for JSON) ------------------
  ```
- Line 39: name
  ```
  nm <- rownames(cf)[i]
  ```
- Line 62: name
  ```
  nm <- rownames(cf)[i]
  ```
- Line 87: lname, name
  ```
  X <- colnames(X_mat)
  ```
- Line 89: lname, name
  ```
  Y <- colnames(Y_mat)
  ```
- Line 112: lname, name
  ```
  mh_cols <- colnames(df_mh_save)
  ```
- Line 117: name
  ```
  claim_varname <- paste0("clm_acci_renw_",q)
  ```
- Line 118: name
  ```
  temp <- df_mh[c(reg_cols, claim_varname)] %>% left_join(filter(XY_mat, RENW_CNT == q))
  ```
- Line 119: lname, name
  ```
  colnames(temp)[colnames(temp) == claim_varname] <- "clm_acci"
  ```
- Line 137: name
  ```
  all_names <- names(coefs_temp)
  ```
- Line 138: name
  ```
  name_ubi_post <- all_names[grepl("ubi_ind", all_names) & grepl("post_ind", all_names)]
  ```
- Line 140: name
  ```
  name_treat_post <- all_names[grepl("treat_int", all_names) & grepl("post_ind", all_names)]
  ```
- Line 141: name
  ```
  out <- coefs_temp[c(name_ubi_post[1], name_treat_post[1])]
  ```
- Line 143: name
  ```
  out <- coefs_temp[name_ubi_post[1]]
  ```
- Line 156: name
  ```
  filename <- paste(data_states, ifelse(balance_spec[b], "bal", "unbal"), sep="_")
  ```
- Line 171: name
  ```
  control_varnames <- control_specifications[[i]]
  ```
- Line 175: name
  ```
  if(length(control_varnames) > 0){
  ```
- Line 176: name
  ```
  base_ind = paste(base_ind, paste(control_varnames, collapse=" + "), sep = " + ")
  ```
- Line 177: name
  ```
  base_int = paste(base_int, paste(control_varnames, collapse=" + "), sep = " + ")
  ```
- Line 208: lat
  ```
  all_models_flat <- c(model_list_ind, model_list_int, model_list_placebo)
  ```
- Line 246: lat
  ```
  Ns <- vapply(all_models_flat, function(m) stats::nobs(m), integer(1))
  ```
- Line 247: name
  ```
  N_row <- c("$N$", format(Ns, big.mark = ",")); names(N_row) <- names(midrule_row)
  ```
- Line 255: lat, lon
  ```
  for (mi in seq_along(all_models_flat)) {
  ```
- Line 256: lat
  ```
  m <- all_models_flat[[mi]]
  ```
- Line 259: name
  ```
  data.frame(variable = rownames(ct),
  ```
- Line 268: name
  ```
  data.frame(variable = rownames(ct),
  ```
- Line 281: name
  ```
  write.csv(coef_df, file.path(RF_CSV_DIR, "tab_2_coefs.csv"), row.names = FALSE)
  ```
- Line 282: name
  ```
  write.csv(meta_df, file.path(RF_CSV_DIR, "tab_2_meta.csv"), row.names = FALSE)
  ```
- Line 285: name
  ```
  write.csv(coef_df, file.path(RF_CSV_DIR, "appendix", "tab_c1_coefs.csv"), row.names = FALSE)
  ```
- Line 286: name
  ```
  write.csv(meta_df, file.path(RF_CSV_DIR, "appendix", "tab_c1_meta.csv"), row.names = FALSE)
  ```
- Line 290: son
  ```
  ## ---- JSON output (backward compatibility) ----
  ```
- Line 292: son
  ```
  # Build JSON structure for tab 2
  ```
- Line 293: son
  ```
  json_models <- list()
  ```
- Line 294: name, son
  ```
  model_names_json <- c("ind_no_ctrl", "ind_X_ctrl", "ind_XY_ctrl", "ind_driver_fe",
  ```
- Line 297: lat, lon
  ```
  for (mi in seq_along(all_models_flat)) {
  ```
- Line 298: lat
  ```
  m <- all_models_flat[[mi]]
  ```
- Line 299: name, son
  ```
  mn <- if (mi <= length(model_names_json)) model_names_json[mi] else paste0("model_", mi)
  ```
- Line 301: son
  ```
  json_models[[mn]] <- extract_feols(m)
  ```
- Line 303: son
  ```
  json_models[[mn]] <- extract_lm(m)
  ```
- Line 307: son
  ```
  models = json_models,
  ```
- Line 310: name
  ```
  n_values = as.list(unname(Ns)),
  ```
- Line 311: name
  ```
  implied_mh_pct = as.list(unname(mh_pct * 100)),
  ```
- Line 314: son
  ```
  write_json(tab2_out, file.path(RF_REG_DIR, "tab_2_regression.json"),
  ```
- Line 316: son
  ```
  cat("  [SIM] tab_2_regression.json written\n")
  ```
- Line 318: son
  ```
  # Build JSON structure for tab C.1 (unbalanced)
  ```
- Line 319: son
  ```
  json_models_c1 <- list()
  ```
- Line 320: name
  ```
  model_names_c1 <- c("ind_no_ctrl", "ind_X_ctrl", "ind_XY_ctrl", "ind_driver_fe",
  ```
- Line 323: lat, lon
  ```
  for (mi in seq_along(all_models_flat)) {
  ```
- Line 324: lat
  ```
  m <- all_models_flat[[mi]]
  ```
- Line 325: name
  ```
  mn <- if (mi <= length(model_names_c1)) model_names_c1[mi] else paste0("model_", mi)
  ```
- Line 327: son
  ```
  json_models_c1[[mn]] <- extract_feols(m)
  ```
- Line 329: son
  ```
  json_models_c1[[mn]] <- extract_lm(m)
  ```
- Line 333: son
  ```
  models = json_models_c1,
  ```
- Line 336: name
  ```
  n_values = as.list(unname(Ns)),
  ```
- Line 337: name
  ```
  implied_mh_pct = as.list(unname(mh_pct * 100)),
  ```
- Line 340: son
  ```
  write_json(tab_c1_out, file.path(RF_REG_DIR, "appendix", "tab_c1_regression.json"),
  ```
- Line 342: son
  ```
  cat("  [SIM] appendix/tab_c1_regression.json written\n")
  ```
- Line 345: name
  ```
  cat(filename, "\n")
  ```
- Line 354: name
  ```
  filename <- paste(data_states, ifelse(balance_spec[b], "bal", "unbal"), sep="_")
  ```
- Line 373: name
  ```
  , names(coefs))
  ```
- Line 379: lname, name
  ```
  colnames(vis_tbl) <- c("period", "group", "est", "ub", "lb")
  ```
- Line 390: name
  ```
  write.csv(vis_tbl, file.path(RF_CSV_DIR, "appendix", "fig_c1.csv"), row.names = FALSE)
  ```
- Line 393: name
  ```
  write.csv(vis_tbl, file.path(RF_CSV_DIR, "fig_4.csv"), row.names = FALSE)
  ```
- Line 397: son
  ```
  ## JSON output (backward compatibility)
  ```
- Line 400: son
  ```
  coefficients_json <- list()
  ```
- Line 402: name
  ```
  nm <- rownames(cf)[ii]
  ```
- Line 403: son
  ```
  coefficients_json[[nm]] <- list(
  ```
- Line 410: son
  ```
  fig_json <- list(
  ```
- Line 420: son
  ```
  coefficients = coefficients_json
  ```
- Line 424: son
  ```
  write_json(fig_json, file.path(RF_REG_DIR, "appendix", "fig_c1_regression.json"),
  ```
- Line 426: son
  ```
  cat("  [SIM] appendix/fig_c1_regression.json written\n")
  ```
- Line 428: son
  ```
  write_json(fig_json, file.path(RF_REG_DIR, "fig_4_regression.json"),
  ```
- Line 430: son
  ```
  cat("  [SIM] fig_4_regression.json written\n")
  ```
- Line 439: name
  ```
  rename_with(~ paste0(.x, "_0"), all_of(c(X,Y)))
  ```
- Line 452: name
  ```
  varnames = rownames(summary(lm_het_fit)$coefficient)
  ```
- Line 453: name
  ```
  vartarget = varnames[grepl("ubi_ind", varnames) & grepl("post_ind", varnames) & grepl("_0", varnames
  ```
- Line 454: name
  ```
  vartarget_index = match(vartarget, varnames)
  ```
- Line 458: name
  ```
  est_het_y = est_het_full[grepl("cov", names(est_het_full))]
  ```
- Line 459: name
  ```
  est_het_x = est_het_full[!grepl("cov", names(est_het_full))]
  ```
- Line 460: name
  ```
  est_het_se_y = est_het_full_se[grepl("cov", names(est_het_full_se))]
  ```
- Line 461: name
  ```
  est_het_se_x = est_het_full_se[!grepl("cov", names(est_het_full_se))]
  ```
- Line 469: name
  ```
  dimension = gsub("1+$", "", gsub("_0", "", gsub("ubi_ind:post_ind:", "", names(est_het_x)))),
  ```
- Line 474: name
  ```
  dimension = gsub("1+$", "", gsub("_0", "", gsub("ubi_ind:post_ind:", "", names(est_het_y)))),
  ```
- Line 478: name
  ```
  write.csv(df_c2, file.path(RF_CSV_DIR, "appendix", "fig_c2.csv"), row.names = FALSE)
  ```
- Line 479: name
  ```
  write.csv(df_c3, file.path(RF_CSV_DIR, "appendix", "fig_c3.csv"), row.names = FALSE)
  ```
- Line 482: son
  ```
  ## JSON output (backward compatibility)
  ```
- Line 487: name
  ```
  het_coefficients[[rownames(cf_het)[ii]]] <- list(
  ```
- Line 504: son
  ```
  write_json(fig_c2c3_out, file.path(RF_REG_DIR, "appendix", "fig_c2_c3_regression.json"),
  ```
- Line 506: son
  ```
  cat("  [SIM] appendix/fig_c2_c3_regression.json written\n")
  ```
- Line 537: lon
  ```
  for (mi in seq_along(model_select_list)) {
  ```
- Line 541: name
  ```
  variable = rownames(ct),
  ```
- Line 551: lon
  ```
  paste0("N_", seq_along(model_select_list))),
  ```
- Line 556: name
  ```
  write.csv(coef_df_3, file.path(RF_CSV_DIR, "tab_3_coefs.csv"), row.names = FALSE)
  ```
- Line 557: name
  ```
  write.csv(meta_df_3, file.path(RF_CSV_DIR, "tab_3_meta.csv"), row.names = FALSE)
  ```
- Line 560: son
  ```
  ## JSON output (backward compatibility)
  ```
- Line 561: son
  ```
  json_models_3 <- list()
  ```
- Line 562: name
  ```
  model_names_3 <- c("no_ctrl", "X_ctrl", "XY_ctrl")
  ```
- Line 563: lon
  ```
  for (mi in seq_along(model_select_list)) {
  ```
- Line 565: name
  ```
  mn <- model_names_3[mi]
  ```
- Line 566: son
  ```
  json_models_3[[mn]] <- extract_lm(m, mean(df_reg$clm_acci, na.rm = TRUE))
  ```
- Line 569: son
  ```
  models = json_models_3,
  ```
- Line 573: son
  ```
  write_json(tab3_out, file.path(RF_REG_DIR, "tab_3_regression.json"),
  ```
- Line 575: son
  ```
  cat("  [SIM] tab_3_regression.json written\n")
  ```
- Line 584: lat
  ```
  "model_select_list", "all_models_flat",
  ```

**/replication-package/jinvass26_replication_may_19_2016/code/simulate/sim_c2_mh_viol.R**

- Line 2: lat
  ```
  ## sim_c2_mh_viol.R — Violation figure (AAF violations by monitoring statu
  ```
- Line 6: son
  ```
  ##   RF_REG_DIR/appendix/fig_c4_regression.json     (backward compat)
  ```
- Line 13: lat
  ```
  source("code/simulate/functions/load_sim_data.R")
  ```
- Line 15: son
  ```
  ipak("jsonlite")
  ```
- Line 30: lname, name
  ```
  lag_cols <- colnames(panel_viol_clean)[grepl("lag", colnames(panel_viol_clean))]
  ```
- Line 37: lat
  ```
  ## ---- Build regdata_graph_mh inline (simulated mode) ------------------------
  ```
- Line 54: lat
  ```
  ## Fill missing X covariates with random data (simulated panel may lack these)
  ```
- Line 58: loc
  ```
  "x_veh_class_C_ind", "x_loc_grg_verify_ind", "tier_pop_yes_ind",
  ```
- Line 62: loc, zip
  ```
  "x_loc_popltn_dens_pct", "tier_acci_drvr_pt", "zip_inc",
  ```
- Line 65: lname, name
  ```
  if (!xv %in% colnames(regdata_graph_mh))
  ```
- Line 70: lat
  ```
  ## ---- Join violation lags ---------------------------------------------------
  ```
- Line 83: son
  ```
  df_lag1 <- regdata_graph_mh %>% filter(RENW_CNT < 1) %>% mutate(year = 0, trend_yr = trend_yr, trend
  ```
- Line 87: son
  ```
  mutate(trend_season = NULL, trend_yr = as.numeric(trend_yr),
  ```
- Line 93: name
  ```
  left_join(df_lag1 %>% dplyr::select(c("POL_ID_CHAR", "viol_aaf_lag")) %>% rename(viol_aaf_lag_agg = 
  ```
- Line 99: loc
  ```
  x_veh_class_C_ind+x_loc_grg_verify_ind+tier_pop_yes_ind+tier_pop_some_ind+
  ```
- Line 101: loc
  ```
  age_senior_ind+edu_college_ind+edu_postgrad_ind+x_cred_score+x_loc_popltn_dens_pct+
  ```
- Line 102: zip
  ```
  tier_acci_drvr_pt+zip_inc+x_veh_mdl_yr+trend_yr"
  ```
- Line 113: name
  ```
  names(coefs))
  ```
- Line 120: lname, name
  ```
  colnames(vis_tbl) <- c("period", "group", "est", "ub", "lb")
  ```
- Line 136: name
  ```
  write.csv(vis_tbl, file.path(RF_CSV_DIR, "appendix", "fig_c4.csv"), row.names = FALSE)
  ```
- Line 139: son
  ```
  ## ---- Write JSON output (backward compatibility) ----------------------------
  ```
- Line 152: lat
  ```
  sample_description = "violation progression, 3-state, experienced drivers"
  ```
- Line 156: name
  ```
  nm <- rownames(cf)[i]
  ```
- Line 166: son
  ```
  write_json(fig_c4_out, file.path(RF_REG_DIR, "appendix", "fig_c4_regression.json"),
  ```
- Line 168: son
  ```
  cat("  Saved JSON:", file.path(RF_REG_DIR, "appendix", "fig_c4_regression.json"), "\n")
  ```

**/replication-package/jinvass26_replication_may_19_2016/code/simulate/sim_c3_demand_elast.R**

- Line 2: city
  ```
  ## sim_c3_demand_elast.R — Demand elasticity figure (IV vs OL
  ```
- Line 6: son
  ```
  ##   RF_REG_DIR/fig_5_regression.json    (backward-compatible JSON)
  ```
- Line 7: city
  ```
  ##   IMAGES_DIR/fig_5.png                (Figure 5: price elasticity by monitoring)
  ```
- Line 14: lat
  ```
  source("code/simulate/functions/load_sim_data.R")
  ```
- Line 15: lat
  ```
  source("code/simulate/functions/data_clean/panel_renw_clean.R")
  ```
- Line 16: lat
  ```
  source("code/simulate/functions/data_clean/get_next_rrev.R")
  ```
- Line 19: son
  ```
  ipak(c("jsonlite"))
  ```
- Line 101: lname, name
  ```
  nb_vars <- intersect(nb_vars, colnames(panel))
  ```
- Line 102: lname, name
  ```
  renw_vars <- intersect(renw_vars, colnames(regdata_renw))
  ```
- Line 123: lname, name
  ```
  X_avail <- intersect(X, colnames(regdata_new))
  ```
- Line 132: name
  ```
  regdata_new$lg_prem_increase_hat[as.integer(names(fsmod$fitted.values))] <- fsmod$fitted.values
  ```
- Line 134: second
  ```
  #### 4. Second stage: IV regression ####
  ```
- Line 151: name
  ```
  param_interest_iv <- names(coefs_iv)[grepl("lg_prem_increase_hat", names(coefs_iv))]
  ```
- Line 152: name
  ```
  index <- match(param_interest_iv, names(coefs_iv))
  ```
- Line 155: lname, name
  ```
  colnames(est_tbl)[1:3] <- c("est", "se", "method")
  ```
- Line 162: name
  ```
  index <- match(param_interest, names(coefs))
  ```
- Line 165: lname, name
  ```
  colnames(temp)[3] <- "method"
  ```
- Line 180: name
  ```
  write.csv(df_est_tbl, file.path(RF_CSV_DIR, "fig_5.csv"), row.names = FALSE)
  ```
- Line 183: son
  ```
  ## JSON output (v1-compatible format for c1_get_rf_exhibits.R JSON derivation)
  ```
- Line 189: name
  ```
  for (nm in rownames(s)) {
  ```
- Line 196: son
  ```
  result_json <- list(models = list(
  ```
- Line 197: second
  ```
  iv_second_stage = serialize_lm_coefs(ssmod_iv),
  ```
- Line 201: son
  ```
  out_json <- file.path(RF_REG_DIR, "fig_5_regression.json")
  ```
- Line 202: son
  ```
  writeLines(toJSON(result_json, auto_unbox = TRUE, pretty = TRUE, digits = 15), out_json)
  ```
- Line 203: son
  ```
  cat("  [GEN] fig_5_regression.json\n")
  ```
- Line 216: coord
  ```
  coord_cartesian(ylim = c(-1.5, 0)) +
  ```
- Line 218: city
  ```
  ylab("Price Elasticity Estimates")
  ```

**/replication-package/jinvass26_replication_may_19_2016/code/simulate/sim_c4_data_figures.R**

- Line 9: lat
  ```
  ##   RF_CSV_DIR/appendix/fig_a4.csv  — Claim surcharge by violation poin
  ```
- Line 18: lat
  ```
  source("code/simulate/functions/load_sim_data.R")
  ```
- Line 68: name
  ```
  write.csv(df_tm_disc_panel, file.path(RF_CSV_DIR, "appendix", "fig_a3.csv"), row.names = FALSE)
  ```
- Line 85: lat
  ```
  source("code/simulate/functions/data_clean/panel_renw_clean.R")
  ```
- Line 121: lname, name
  ```
  colnames(panel_il)
  ```
- Line 127: name
  ```
  rename(tm_initial_disc = !!tm_initial_disc_col[1])
  ```
- Line 175: name
  ```
  write.csv(df_plot[c("p_R_ftr_bench", "Mon")], file.path(RF_CSV_DIR, "fig_2b.csv"), row.names = FALSE
  ```
- Line 180: lat
  ```
  ## Figure A4: Claim Surcharge by Violation Points
  ```
- Line 188: name
  ```
  if (all(c("tier_acci_drvr_pt", "tier_good_ind", "clm_srchg") %in% names(panel_a4))) {
  ```
- Line 191: name
  ```
  write.csv(plot_a4, file.path(RF_CSV_DIR, "appendix", "fig_a4.csv"), row.names = FALSE)
  ```
- Line 209: son
  ```
  dl_json <- file.path(SIM_CACHE_DIR, "data_list_IL.json")
  ```
- Line 210: son
  ```
  if (file.exists(dl_json)) dl_path <- dl_json
  ```
- Line 214: son
  ```
  if (grepl("\\.json$", dl_path)) {
  ```
- Line 215: son
  ```
  data_list <- jsonlite::fromJSON(dl_path)
  ```
- Line 220: name
  ```
  if ("log_tm_score" %in% names(data_list) &&
  ```
- Line 221: name
  ```
  "p_R_ftr_tm_disc" %in% names(data_list) &&
  ```
- Line 222: name
  ```
  "ubi_rate_dvc_vers" %in% names(data_list) &&
  ```
- Line 223: name
  ```
  "I_to_renw0" %in% names(data_list)) {
  ```
- Line 242: name
  ```
  write.csv(df_score[c("score", "scheme")], file.path(RF_CSV_DIR, "appendix", "fig_b2a.csv"), row.name
  ```
- Line 255: name
  ```
  write.csv(gg_df, file.path(RF_CSV_DIR, "appendix", "fig_b2b.csv"), row.names = FALSE)
  ```
- Line 281: name
  ```
  write.csv(tm_score_df["log_tm_score"], file.path(RF_CSV_DIR, "fig_2a.csv"), row.names = FALSE)
  ```
- Line 285: son
  ```
  ## Figure 3: Comparison of Subsequent Claim Cost Across Monitoring Groups
  ```
- Line 298: lat
  ```
  source("code/simulate/functions/data_clean/clean_choice_panel.R")
  ```
- Line 357: name
  ```
  file.path(RF_CSV_DIR, "fig_3.csv"), row.names = FALSE)
  ```

**/replication-package/jinvass26_replication_may_19_2016/code/simulate/sim_c5_selection_figures.R**

- Line 6: son
  ```
  ##   RF_REG_DIR/appendix/fig_a5_a6_regression.json (backward compat)
  ```
- Line 9: son
  ```
  ##   RF_REG_DIR/appendix/fig_b1b_regression.json (backward compat)
  ```
- Line 16: lat
  ```
  source("code/simulate/functions/load_sim_data.R")
  ```
- Line 17: lat
  ```
  source("code/simulate/functions/data_clean/clean_choice_panel.R")
  ```
- Line 19: son
  ```
  ipak(c("jsonlite", "scales", "lubridate"))
  ```
- Line 44: lname, name
  ```
  X <- colnames(X_mat)[3:ncol(X_mat)]
  ```
- Line 78: lname, name
  ```
  if (!clm_col %in% colnames(cp_work)) next
  ```
- Line 87: lname, name
  ```
  if (prev_col %in% colnames(cp_work)) {
  ```
- Line 126: lname, name
  ```
  X_avail <- X[X %in% colnames(regdata_X)]
  ```
- Line 148: lname, name
  ```
  mfx_se_cols <- grepl("mfx|se", colnames(df_info)) & !grepl("avg", colnames(df_info))
  ```
- Line 156: name
  ```
  file.path(RF_CSV_DIR, "appendix", "fig_a5_a6.csv"), row.names = FALSE)
  ```
- Line 159: son
  ```
  ## Save JSON for backward compatibility
  ```
- Line 160: son
  ```
  json_result <- list(
  ```
- Line 182: son
  ```
  json_result$periods[[spec]] <- spec_rows
  ```
- Line 184: son
  ```
  writeLines(toJSON(json_result, auto_unbox = TRUE, pretty = TRUE, digits = 15),
  ```
- Line 185: son
  ```
  file.path(RF_REG_DIR, "appendix", "fig_a5_a6_regression.json"))
  ```
- Line 186: son
  ```
  cat("  [GEN] appendix/fig_a5_a6_regression.json\n")
  ```
- Line 251: name
  ```
  write.csv(df_trend, file.path(RF_CSV_DIR, "appendix", "fig_b1a.csv"), row.names = FALSE)
  ```
- Line 279: loc
  ```
  'x_veh_class_C_ind', 'x_loc_grg_verify_ind',
  ```
- Line 282: loc
  ```
  X_cont <- c('x_drvr_age_rated', 'x_cred_score', 'x_loc_popltn_dens_pct',
  ```
- Line 283: loc, zip
  ```
  'x_loc_zipcd_agi', 'tier_pop_lngth', 'tier_acci_drvr_pt')
  ```
- Line 285: son
  ```
  ## Add trend/season and state FE
  ```
- Line 287: name
  ```
  mutate(tier_acci_drvr_pt = ifelse("tier_acci_drvr_pt_2" %in% names(.),
  ```
- Line 289: name
  ```
  x_drvr_lic_yr = ifelse("DRVR_YR_LIC" %in% names(.), DRVR_YR_LIC, 0),
  ```
- Line 291: son
  ```
  trend_season = month(date_pol_eff),
  ```
- Line 295: son
  ```
  all_X_needed <- c(X_cat, X_cont, "trend_yr", "trend_season", "risk_class", "ST_CD")
  ```
- Line 296: lname, name
  ```
  X_avail_rd <- all_X_needed[all_X_needed %in% colnames(regdata_main)]
  ```
- Line 300: name
  ```
  if ("trend_yr" %in% names(X_mat_rd)) {
  ```
- Line 304: son
  ```
  trend_season_sq = trend_season^2,
  ```
- Line 305: son
  ```
  trend_season_cb = trend_season^3)
  ```
- Line 307: loc, name, zip
  ```
  if ("x_loc_zipcd_agi" %in% names(X_mat_rd)) {
  ```
- Line 309: loc, zip
  ```
  mutate(zip_inc = x_loc_zipcd_agi / 1000,
  ```
- Line 310: loc, zip
  ```
  lg_zip_inc = log(x_loc_zipcd_agi)) %>%
  ```
- Line 311: loc, zip
  ```
  select(-x_loc_zipcd_agi)
  ```
- Line 313: name
  ```
  if ("x_cred_score" %in% names(X_mat_rd)) {
  ```
- Line 318: name
  ```
  if ("risk_class" %in% names(X_mat_rd)) {
  ```
- Line 325: name
  ```
  if ("x_drvr_age_rated" %in% names(X_mat_rd)) {
  ```
- Line 332: name
  ```
  if ("x_drvr_yr_edu" %in% names(X_mat_rd)) {
  ```
- Line 337: name
  ```
  if ("x_veh_mdl_yr" %in% names(X_mat_rd)) {
  ```
- Line 341: name
  ```
  if ("tier_acci_drvr_pt" %in% names(X_mat_rd)) {
  ```
- Line 352: name
  ```
  if ("ST_CD" %in% names(X_mat_rd) && length(unique(X_mat_rd$ST_CD)) > 1) {
  ```
- Line 361: loc
  ```
  "x_loc_popltn_dens_pct", "tier_pop_lngth",
  ```
- Line 362: zip
  ```
  "zip_inc", "lg_zip_inc", "age_sq",
  ```
- Line 364: lname, name
  ```
  colnames(X_mat_rd))
  ```
- Line 378: name
  ```
  for (col in setdiff(names(df), keep_cols)) {
  ```
- Line 396: lname, name
  ```
  X_no_trend <- X_mat_rd[!grepl("trend", colnames(X_mat_rd))]
  ```
- Line 446: lname, name
  ```
  colnames(result_tbl) <- c("ctrl", "reg", "est", "se", "t", "p")
  ```
- Line 462: name
  ```
  file.path(RF_CSV_DIR, "appendix", "fig_b1b.csv"), row.names = FALSE)
  ```
- Line 465: son
  ```
  ## Save JSON for backward compatibility
  ```
- Line 466: son
  ```
  json_b1b <- list(models = list())
  ```
- Line 467: name
  ```
  model_names <- c("prem_raw", "prem_x_no_t", "prem_x", "clm_raw", "clm_x_no_t", "clm_x")
  ```
- Line 469: lon, name
  ```
  for (i in seq_along(model_names)) {
  ```
- Line 474: name
  ```
  nm <- rownames(co)[j]
  ```
- Line 476: name
  ```
  estimate  = unname(co[j, 1]),
  ```
- Line 477: name
  ```
  std_error = unname(co[j, 2]),
  ```
- Line 478: name
  ```
  t_value   = unname(co[j, 3]),
  ```
- Line 479: name
  ```
  p_value   = unname(co[j, 4])
  ```
- Line 489: name
  ```
  if (grepl("^clm_", model_names[i])) {
  ```
- Line 492: name, son
  ```
  json_b1b$models[[model_names[i]]] <- model_entry
  ```
- Line 494: son
  ```
  writeLines(toJSON(json_b1b, auto_unbox = TRUE, pretty = TRUE, digits = 15),
  ```
- Line 495: son
  ```
  file.path(RF_REG_DIR, "appendix", "fig_b1b_regression.json"))
  ```
- Line 496: son
  ```
  cat("  [GEN] appendix/fig_b1b_regression.json\n")
  ```

**/replication-package/jinvass26_replication_may_19_2016/code/simulate/sim_c6_estimate.R**

- Line 2: lat
  ```
  ## sim_estimate.R — Estimate model_sev, model_price, model_main on simulated da
  ```
- Line 10: lat, son
  ```
  ##   data/simulated/data_list_IL.rds (or .json)
  ```
- Line 13: lat
  ```
  ## Outputs (under MODEL_OUT_DIR = output/simulated/estimates/model_output/):
  ```
- Line 21: lat
  ```
  if (!exists("SIM_DATA_DIR")) { USE_SIMULATED_DATA <- TRUE; source("code/config.R") }
  ```
- Line 22: lat
  ```
  source("code/simulate/functions/sim_helpers.R")
  ```
- Line 25: lat
  ```
  cat("sim_estimate.R: Estimating models on simulated data\n")
  ```
- Line 41: name
  ```
  if (!requireNamespace("cmdstanr", quietly = TRUE))
  ```
- Line 55: lat
  ```
  ## Cascade: (1) simulated output, (2) real-data output (hyperparams only), (3) 0.1
  ```
- Line 57: name
  ```
  build_init <- function(model_name) {
  ```
- Line 58: name
  ```
  sim_csv <- file.path(MODEL_OUT_DIR, model_name,
  ```
- Line 67: name
  ```
  real_csv <- file.path("data/estimates/model_output", model_name,
  ```
- Line 73: lat
  ```
  # Keep leps (nu values) — simulated data uses these exact valu
  ```
- Line 81: lat
  ```
  ## ---- Load simulated data_list -----------------------------------------------
  ```
- Line 84: son
  ```
  dl_json <- file.path(SIM_CACHE_DIR, "data_list_IL.json")
  ```
- Line 89: son
  ```
  } else if (file.exists(dl_json)) {
  ```
- Line 90: son
  ```
  cat("Loading", dl_json, "\n")
  ```
- Line 91: son
  ```
  sim_data_list <- read_data_list(dl_json)
  ```
- Line 102: name
  ```
  save_model_output <- function(fit, model_name, stan_path) {
  ```
- Line 103: name
  ```
  out_dir <- file.path(MODEL_OUT_DIR, model_name, "results")
  ```
- Line 117: name
  ```
  model_dir <- file.path(MODEL_OUT_DIR, model_name)
  ```
- Line 120: name
  ```
  output_filename = paste0(model_name, "-", log_suffix, ".csv"),
  ```
- Line 121: name
  ```
  model_filename = paste0(model_name, "-", log_suffix, ".stan"),
  ```
- Line 124: name
  ```
  save(log_file_obj, file = file.path(model_dir, paste0("log_", model_name, "-", log_suffix, ".Rda")))
  ```
- Line 127: name
  ```
  if (model_name == "model_main") {
  ```
- Line 130: block, loc
  ```
  d_by_block = sim_data_list$d_by_block %||% 6L,
  ```
- Line 132: name
  ```
  ), file.path(model_dir, paste0("est_config_", model_name, "-", log_suffix, ".rds")))
  ```
- Line 137: name
  ```
  paste0(model_name, "-", log_suffix, ".stan")),
  ```
- Line 184: name
  ```
  if (f %in% names(sim_data_list)) sev_data[[f]] <- sim_data_list[[f]]
  ```
- Line 223: lname, name
  ```
  if ("theta_0_pareto_alpha_severe" %in% colnames(sev_mle))
  ```
- Line 225: lname, name
  ```
  if ("theta_0_sev_minor_mean" %in% colnames(sev_mle))
  ```
- Line 227: lname, name
  ```
  if ("sev_minor_sd" %in% colnames(sev_mle))
  ```
- Line 272: name
  ```
  if (f %in% names(sim_data_list)) price_data[[f]] <- sim_data_list[[f]]
  ```
- Line 357: zip
  ```
  "X_choice", "X_rc_choice", "zip_income",
  ```
- Line 375: block, loc
  ```
  "d_tm_mh_rational_ind", "d_by_block",
  ```
- Line 380: name
  ```
  if (f %in% names(sim_data_list) && !is.null(sim_data_list[[f]]))
  ```
- Line 384: zip
  ```
  if (is.null(cost_data$zip_income)) cost_data$zip_income <- rep(0, sim_data_list$N_choice)
  ```
- Line 386: block, loc
  ```
  cost_data$run_demand_blocks <- rep(0L, sim_data_list$N_choice_regimes)
  ```
- Line 505: block, loc
  ```
  "run_estimation", "run_demand_blocks", "d_by_block", "d_tm_mh_rational_ind",
  ```
- Line 516: name
  ```
  if (f %in% names(sim_data_list) && !is.null(sim_data_list[[f]])) {
  ```
- Line 529: block, loc
  ```
  stan_data$run_demand_blocks <- stan_data$run_demand_blocks %||% 1L
  ```

**/replication-package/jinvass26_replication_may_19_2016/code/simulate/sim_c6b_estimate_robustness.R**

- Line 10: lat
  ```
  if (!exists("SIM_DATA_DIR")) { USE_SIMULATED_DATA <- TRUE; source("code/config.R") }
  ```
- Line 11: lat
  ```
  source("code/simulate/functions/sim_helpers.R")
  ```
- Line 28: name
  ```
  if (!requireNamespace("cmdstanr", quietly = TRUE))
  ```
- Line 34: name
  ```
  build_init <- function(model_name) {
  ```
- Line 35: name
  ```
  sim_csv <- file.path(MODEL_OUT_DIR, model_name,
  ```
- Line 44: name
  ```
  real_csv <- file.path("data/estimates/model_output", model_name,
  ```
- Line 57: name
  ```
  save_model_output <- function(fit, model_name, stan_path) {
  ```
- Line 58: name
  ```
  out_dir <- file.path(MODEL_OUT_DIR, model_name, "results")
  ```
- Line 71: name
  ```
  model_dir <- file.path(MODEL_OUT_DIR, model_name)
  ```
- Line 74: name
  ```
  output_filename = paste0(model_name, "-", log_suffix, ".csv"),
  ```
- Line 75: name
  ```
  model_filename = paste0(model_name, "-", log_suffix, ".stan"),
  ```
- Line 78: name
  ```
  save(log_file_obj, file = file.path(model_dir, paste0("log_", model_name, "-", log_suffix, ".Rda")))
  ```
- Line 81: name
  ```
  paste0(model_name, "-", log_suffix, ".stan")),
  ```
- Line 130: block, loc
  ```
  "run_estimation", "run_demand_blocks", "d_by_block", "d_tm_mh_rational_ind",
  ```
- Line 138: name
  ```
  if (f %in% names(sim_data_list) && !is.null(sim_data_list[[f]]))
  ```
- Line 142: block, loc
  ```
  stan_data$run_demand_blocks <- stan_data$run_demand_blocks %||% 1L
  ```
- Line 152: block, loc
  ```
  # (converges in 1-2 iters but ensures GQ block runs and output is fresh)
  ```

**/replication-package/jinvass26_replication_may_19_2016/code/simulate/sim_c6c_estimate_cost_mhhet.R**

- Line 2: lat
  ```
  ## sim_c6c_estimate_cost_mhhet.R — Estimate model_cost_mhhet on simulated da
  ```
- Line 8: lat
  ```
  if (!exists("SIM_DATA_DIR")) { USE_SIMULATED_DATA <- TRUE; source("code/config.R") }
  ```
- Line 9: lat
  ```
  source("code/simulate/functions/sim_helpers.R")
  ```
- Line 26: name
  ```
  if (!requireNamespace("cmdstanr", quietly = TRUE))
  ```
- Line 32: name
  ```
  build_init <- function(model_name) {
  ```
- Line 33: name
  ```
  sim_csv <- file.path(MODEL_OUT_DIR, model_name,
  ```
- Line 42: name
  ```
  real_csv <- file.path("data/estimates/model_output", model_name,
  ```
- Line 83: zip
  ```
  "X_choice", "X_rc_choice", "zip_income",
  ```
- Line 107: block, loc
  ```
  "d_tm_mh_rational_ind", "d_by_block",
  ```
- Line 117: name
  ```
  if (f %in% names(sim_data_list) && !is.null(sim_data_list[[f]])) {
  ```
- Line 125: name
  ```
  if (is.null(stan_data$X_rc_clm) && "X_rc" %in% names(sim_data_list)) {
  ```
- Line 136: zip
  ```
  # zip_income: if not in data_list, use zeros
  ```
- Line 137: zip
  ```
  if (is.null(stan_data$zip_income)) {
  ```
- Line 138: zip
  ```
  stan_data$zip_income <- rep(0, sim_data_list$N_choice)
  ```
- Line 139: zip
  ```
  missing_fields <- setdiff(missing_fields, "zip_income")
  ```
- Line 144: block, loc
  ```
  stan_data$run_demand_blocks <- rep(0L, sim_data_list$N_choice_regimes)
  ```
- Line 230: name
  ```
  output_filename = paste0(mn, "-", log_suffix, ".csv"),
  ```
- Line 231: name
  ```
  model_filename = paste0(mn, "-", log_suffix, ".stan"),
  ```
- Line 313: block, loc
  ```
  "run_estimation", "run_demand_blocks", "d_by_block", "d_tm_mh_rational_ind",
  ```
- Line 321: name
  ```
  if (f %in% names(sim_data_list) && !is.null(sim_data_list[[f]]))
  ```
- Line 325: block, loc
  ```
  stan_data2$run_demand_blocks <- stan_data2$run_demand_blocks %||% 1L
  ```
- Line 328: name
  ```
  if ("X_rc" %in% names(sim_data_list)) {
  ```
- Line 385: name
  ```
  output_filename = paste0(mn2, "-", log_suffix2, ".csv"),
  ```
- Line 386: name
  ```
  model_filename = paste0(mn2, "-", log_suffix2, ".stan"),
  ```
- Line 394: block, loc
  ```
  d_by_block = sim_data_list$d_by_block %||% 6L,
  ```

**/replication-package/jinvass26_replication_may_19_2016/code/simulate/sim_c7_model_fit.R**

- Line 11: lat
  ```
  ## Sourced by: code/run_simulated.R (after sim_estimate.R)
  ```
- Line 14: lat
  ```
  if (!exists("MODEL_FIT_DIR")) { USE_SIMULATED_DATA <- TRUE; source("code/config.R") }
  ```
- Line 15: lat
  ```
  source("code/simulate/functions/sim_helpers.R")
  ```
- Line 20: lat
  ```
  ## These format model-fit .txt output into publication-ready LaTeX tables.
  ```
- Line 74: lon
  ```
  for (i in seq_along(tab_lines)) {
  ```
- Line 91: lon
  ```
  for (i in seq_along(parts)) {
  ```
- Line 114: block, loc
  ```
  "Block [0-9]", "Type & Label", "^\\s*\\\\multicolumn\\{2\\}\\{c\\}"
  ```
- Line 125: block, loc
  ```
  n_blocks <- (n_cells_first - 2) / 2
  ```
- Line 127: block, loc
  ```
  if (n_blocks >= 6) {
  ```
- Line 198: name
  ```
  lbl <- if (label %in% names(cov_map)) cov_map[label] else label
  ```
- Line 216: name
  ```
  lbl <- if (label %in% names(cov_map)) cov_map[label] else label
  ```
- Line 237: block, loc
  ```
  ## two blocks (risk/score/pricing and demand/choice), applies formatting,
  ```
- Line 270: block, loc
  ```
  # Split into two blocks at \footnotesize delimiter
  ```
- Line 273: block, loc
  ```
  stop("Could not split fit_tbl into two blocks (no \\footnotesize found) in: ", txt_path)
  ```
- Line 276: block, loc
  ```
  block1_lines <- fit_tbl_raw[1:(footnotesize_line[1] - 1)]
  ```
- Line 277: block, loc
  ```
  block2_lines <- fit_tbl_raw[footnotesize_line[1]:length(fit_tbl_raw)]
  ```
- Line 278: block, loc
  ```
  block1_lines <- block1_lines[nchar(block1_lines) > 0]
  ```
- Line 279: block, loc
  ```
  block2_lines <- block2_lines[nchar(block2_lines) > 0]
  ```
- Line 282: block, loc
  ```
  b1_ts <- grep("\\\\begin\\{tabular\\}", block1_lines)
  ```
- Line 283: block, loc
  ```
  b1_te <- grep("\\\\end\\{tabular\\}", block1_lines)
  ```
- Line 284: block, loc
  ```
  b2_ts <- grep("\\\\begin\\{tabular\\}", block2_lines)
  ```
- Line 285: block, loc
  ```
  b2_te <- grep("\\\\end\\{tabular\\}", block2_lines)
  ```
- Line 288: loc
  ```
  stop("Could not locate tabular environments in: ", txt_path)
  ```
- Line 291: block, loc
  ```
  # Block 1 -> tab4 (risk/score/pricing)
  ```
- Line 292: block, loc
  ```
  tabular1 <- paste(block1_lines[b1_ts[1]:b1_te[length(b1_te)]], collapse = "\n")
  ```
- Line 313: block, loc
  ```
  # Block 2 -> tab5 (demand/choice)
  ```
- Line 314: block, loc
  ```
  tabular2 <- paste(block2_lines[b2_ts[1]:b2_te[length(b2_te)]], collapse = "\n")
  ```
- Line 341: name
  ```
  model_name <- "model_main"
  ```
- Line 351: lat
  ```
  ## and computes per-individual latent params (llambda, lambda, etc.)
  ```
- Line 353: lat
  ```
  source("code/simulate/functions/estimation/extract_model_estimates_cmdstan.R")
  ```
- Line 355: lat
  ```
  ## get_fit_cp.R sources get_c_latentparams.R + get_cp_pred.R, then builds
  ```
- Line 358: lat
  ```
  source("code/simulate/functions/estimation/get_fit_cp.R")
  ```
- Line 362: lat
  ```
  ## header for full isolation guarantees and rationale).
  ```
- Line 363: lat
  ```
  source("code/simulate/fig_b3_override.R")
  ```
- Line 365: lat
  ```
  ## get_d_latentparams.R computes demand-side latent params (risk_aversion,
  ```
- Line 366: lat
  ```
  ## sigma_logit, psi_mm, xi, etc.) needed for choice probability calculation
  ```
- Line 367: lat
  ```
  cat("  Computing demand latent parameters...\n")
  ```
- Line 369: lat
  ```
  source("code/simulate/functions/estimation/get_d_latentparams.R")
  ```
- Line 371: block, loc
  ```
  ## get_d_pred.R computes log_choice_probs for all 6 choice blocks
  ```
- Line 374: lat
  ```
  source("code/simulate/functions/estimation/get_d_pred.R")
  ```
- Line 400: lat
  ```
  # Pricing moments (first renewal = new business, latter = renewal)
  ```
- Line 425: son
  ```
  section = c(rep("Poisson claim counts", 4), rep("Monitoring score", 4),
  ```
- Line 426: lat
  ```
  rep("First renewal pricing factor", 4), rep("Latter renewal pricing factor", 4)),
  ```
- Line 427: second
  ```
  moment = rep(c("first moment", "major claims", "second moment", "N"), 4),
  ```
- Line 439: second
  ```
  tab_4$moment[tab_4$section == "Monitoring score" & tab_4$moment == "major claims"] <- "second moment
  ```
- Line 440: second
  ```
  tab_4$moment[tab_4$section == "Monitoring score" & tab_4$moment == "second moment" &
  ```
- Line 441: second
  ```
  tab_4$data_value == round(score_m2_d, 6)] <- "second moment"
  ```
- Line 442: second
  ```
  # Reorder score rows: first moment, second moment, covariance with risk, N
  ```
- Line 444: second
  ```
  tab_4$moment[score_mask] <- c("first moment", "second moment", "covariance with risk", "N")
  ```
- Line 446: name
  ```
  write.csv(tab_4, file.path(MODEL_FIT_DIR, "tab_4.csv"), row.names = FALSE)
  ```
- Line 460: block, loc
  ```
  .add_demand <- function(type, label, block, data_val, pred_val, scale_pct = TRUE) {
  ```
- Line 463: block, loc
  ```
  type = type, label = label, block = as.integer(block),
  ```
- Line 469: block, loc
  ```
  # Coverage share and selection by block
  ```
- Line 475: lon
  ```
  for (j in seq_along(cov_labels_list[[k]])) {
  ```
- Line 495: block, loc
  ```
  # TM share and selection (blocks 2, 4)
  ```
- Line 512: block, loc
  ```
  # Attrition (blocks 5, 6)
  ```
- Line 523: block, loc
  ```
  # N row for each block
  ```
- Line 533: block, loc
  ```
  # Build wide with blockN_data, blockN_pred columns
  ```
- Line 540: block, loc
  ```
  bsub <- sub[sub$block == b, ]
  ```
- Line 542: block, loc
  ```
  row[[paste0("block", b, "_data")]] <- bsub$data_val[1]
  ```
- Line 543: block, loc
  ```
  row[[paste0("block", b, "_pred")]] <- bsub$pred_val[1]
  ```
- Line 545: block, loc
  ```
  row[[paste0("block", b, "_data")]] <- NA
  ```
- Line 546: block, loc
  ```
  row[[paste0("block", b, "_pred")]] <- NA
  ```
- Line 554: name
  ```
  write.csv(tab_5, file.path(MODEL_FIT_DIR, "tab_5.csv"), row.names = FALSE)
  ```

**/replication-package/jinvass26_replication_may_19_2016/code/simulate/sim_c7b_model_fit_mhhet.R**

- Line 5: block, loc
  ```
  ## Produces: MODEL_FIT_DIR/tab_c2.csv, MODEL_FIT_DIR/tab_c2_block2.csv
  ```
- Line 8: lat
  ```
  if (!exists("MODEL_FIT_DIR")) { USE_SIMULATED_DATA <- TRUE; source("code/config.R") }
  ```
- Line 9: lat
  ```
  source("code/simulate/functions/sim_helpers.R")
  ```
- Line 22: name
  ```
  model_name <- "model_main_mhhet"
  ```
- Line 31: lat
  ```
  USE_SIMULATED_ESTIMATES <- TRUE
  ```
- Line 32: name
  ```
  sim_model_dir <- file.path(MODEL_OUT_DIR, model_name)
  ```
- Line 45: lat
  ```
  "   downstream Stan-model recompilation in extract_model_estimates_cmdstan.R\n",
  ```
- Line 48: lon
  ```
  "   so that sim_c6c writes a fresh log_*.Rda alongside the bootstrap CSV.)\n", sep = "")
  ```
- Line 57: name
  ```
  if (!"X_rc_clm" %in% names(dl_tmp) && "X_rc" %in% names(dl_tmp)) {
  ```
- Line 68: lat
  ```
  source("code/simulate/functions/estimation/extract_model_estimates_cmdstan.R")
  ```
- Line 71: lat
  ```
  source("code/simulate/functions/estimation/get_fit_cp.R")
  ```
- Line 73: lat
  ```
  cat("  Computing demand latent parameters...\n")
  ```
- Line 75: lat
  ```
  source("code/simulate/functions/estimation/get_d_latentparams.R")
  ```
- Line 78: lat
  ```
  source("code/simulate/functions/estimation/get_d_pred.R")
  ```
- Line 127: son
  ```
  section = c(rep("Poisson claim counts", 4), rep("Monitoring score", 4),
  ```
- Line 128: lat
  ```
  rep("First renewal pricing factor", 4), rep("Latter renewal pricing factor", 4)),
  ```
- Line 129: second
  ```
  moment = rep(c("first moment", "major claims", "second moment", "N"), 4),
  ```
- Line 141: second
  ```
  tab_c2$moment[score_mask] <- c("first moment", "second moment", "covariance with risk", "N")
  ```
- Line 143: name
  ```
  write.csv(tab_c2, file.path(MODEL_FIT_DIR, "tab_c2.csv"), row.names = FALSE)
  ```
- Line 146: block, loc
  ```
  ## ---- Build tab_c2_block2.csv (mirrors tab_5: Coverage Share, Selection %, TM
  ```
- Line 149: block, loc
  ```
  cat("  Building tab_c2_block2.csv...\n")
  ```
- Line 160: block, loc
  ```
  .add_demand <- function(type, label, block, data_val, pred_val, scale_pct = TRUE) {
  ```
- Line 163: block, loc
  ```
  type = type, label = label, block = as.integer(block),
  ```
- Line 169: block, loc
  ```
  # Coverage share and selection by block
  ```
- Line 175: lon
  ```
  for (j in seq_along(cov_labels_list[[k]])) {
  ```
- Line 195: block, loc
  ```
  # TM share and selection (blocks 2, 4)
  ```
- Line 212: block, loc
  ```
  # Attrition (blocks 5, 6)
  ```
- Line 223: block, loc
  ```
  # N row for each block
  ```
- Line 238: block, loc
  ```
  bsub <- sub[sub$block == b, ]
  ```
- Line 240: block, loc
  ```
  row[[paste0("block", b, "_data")]] <- bsub$data_val[1]
  ```
- Line 241: block, loc
  ```
  row[[paste0("block", b, "_pred")]] <- bsub$pred_val[1]
  ```
- Line 243: block, loc
  ```
  row[[paste0("block", b, "_data")]] <- NA
  ```
- Line 244: block, loc
  ```
  row[[paste0("block", b, "_pred")]] <- NA
  ```
- Line 251: block, loc, name
  ```
  write.csv(tab_c2b, file.path(MODEL_FIT_DIR, "tab_c2_block2.csv"), row.names = FALSE)
  ```
- Line 252: block, loc
  ```
  cat("    -> tab_c2_block2.csv\n")
  ```

**/replication-package/jinvass26_replication_may_19_2016/code/simulate/sim_c8_ctf_run.R**

- Line 17: lat
  ```
  if (!exists("SIM_DATA_DIR")) { USE_SIMULATED_DATA <- TRUE; source("code/config.R") }
  ```
- Line 30: name
  ```
  model_name <- "model_main"
  ```
- Line 34: son
  ```
  dl_profile <- jsonlite::fromJSON(file.path(SIM_DATA_DIR, "data_profile_data_list.json"))
  ```
- Line 37: lat
  ```
  USE_SIMULATED_ESTIMATES <- TRUE
  ```
- Line 39: name
  ```
  cat("Model:", model_name, "\n")
  ```
- Line 40: lat
  ```
  cat("Using simulated estimates:", USE_SIMULATED_ESTIMATES, "\n")
  ```
- Line 49: lat
  ```
  # Load simulation helpers (needed for matrix_to_lists function)
  ```
- Line 50: lat
  ```
  source("code/simulate/functions/sim_helpers.R")
  ```
- Line 52: lat
  ```
  # Override extraction paths to use simulated model output
  ```
- Line 53: lat
  ```
  if (USE_SIMULATED_ESTIMATES) {
  ```
- Line 54: name
  ```
  sim_model_dir <- file.path(MODEL_OUT_DIR, model_name)
  ```
- Line 71: lat
  ```
  cat("  Using simulated model dir:", sim_model_dir, "\n")
  ```
- Line 74: name
  ```
  log_file_suffix <- LOG_FILE_SUFFIXES[[model_name]]
  ```
- Line 79: lat
  ```
  # Source the extraction pipeline (this loads data, compiles Stan, computes latent params)
  ```
- Line 80: lat
  ```
  source("code/simulate/functions/estimation/extract_model_estimates_cmdstan.R")
  ```
- Line 100: name
  ```
  flex_oo_name <- ifelse(grepl("min_flex", ctf_oo_option), "min",
  ```
- Line 103: lat
  ```
  if (USE_SIMULATED_DATA) {
  ```
- Line 104: name
  ```
  bootstrap_ctf_dir <- file.path(MODEL_OUT_DIR, model_name, "ctf")
  ```
- Line 106: name
  ```
  bootpath <- file.path(MODEL_OUT_DIR, model_name, "bootstrap")
  ```
- Line 107: name
  ```
  ctf_cache_name <- "ctf_real_data_real_param"
  ```
- Line 108: name
  ```
  bootstrap_ctf_dir <- file.path(bootpath, ctf_cache_name, mkt_structure, flex_oo_name)
  ```
- Line 112: block, loc
  ```
  k_block_ctf <- K_BLOCK_CTF
  ```
- Line 114: name
  ```
  # Configuration strings for cache filenames
  ```
- Line 127: lat
  ```
  if (USE_SIMULATED_DATA && !USE_CACHE) {
  ```
- Line 132: lat
  ```
  source("code/simulate/functions/ctf/ctf_clear_cache.R")
  ```
- Line 137: lat
  ```
  source("code/simulate/functions/ctf/ctf_preload.R")
  ```
- Line 154: lat
  ```
  source('code/simulate/functions/ctf/ctf_calibration.R')
  ```
- Line 161: lat
  ```
  ## ---- Define Regime Calculation Function -------------------------------------
  ```
- Line 163: lat
  ```
  cat("Step 5: Setting up regime calculation...\n")
  ```
- Line 173: name
  ```
  varnames <- c(
  ```
- Line 175: second
  ```
  "cp_own", "cp_own_second_period", "cp_tm",
  ```
- Line 183: lat
  ```
  calculate_regime_vars <- function(unmonitored_surcharge, opt_in_discount,
  ```
- Line 190: lat
  ```
  source("code/simulate/functions/ctf/get_ctf_util_profit.R")
  ```
- Line 206: second
  ```
  cp_t2 <- out$cp_second_period
  ```
- Line 227: second
  ```
  cp_own_second_period_var <- get_samp_wgt_avg(rowSums(cp_t2[, 1:J]), sampling_weight) * 100
  ```
- Line 247: second
  ```
  assign(paste0("cp_own_second_period_", suffix), cp_own_second_period_var, envir = .GlobalEnv)
  ```
- Line 266: lat
  ```
  calculate_regime_vars(
  ```
- Line 278: lat
  ```
  calculate_regime_vars(
  ```
- Line 295: lat
  ```
  source("code/simulate/functions/ctf/ctf_equi_k_save_grid.R")
  ```
- Line 304: lat
  ```
  calculate_regime_vars(
  ```
- Line 316: lat
  ```
  calculate_regime_vars(
  ```
- Line 328: lat
  ```
  calculate_regime_vars(
  ```
- Line 340: lat
  ```
  calculate_regime_vars(
  ```
- Line 356: name
  ```
  names(df_ctf_results) <- metrics
  ```
- Line 357: name
  ```
  row.names(df_ctf_results) <- regimes
  ```
- Line 359: name
  ```
  no_tm_vars <- mget(paste0(varnames, "_no_tm"))
  ```
- Line 360: name
  ```
  current_vars <- mget(paste0(varnames, "_current"))
  ```
- Line 361: name
  ```
  part_opt_vars <- mget(paste0(varnames, "_part_opt"))
  ```
- Line 362: name
  ```
  opt_vars <- mget(paste0(varnames, "_opt"))
  ```
- Line 363: name
  ```
  data_share_vars <- mget(paste0(varnames, "_ds"))
  ```
- Line 364: name
  ```
  disc_floor_vars <- mget(paste0(varnames, "_ds_df"))
  ```
- Line 366: lon
  ```
  for (i in seq_along(metrics)) {
  ```
- Line 380: lat
  ```
  # Compute deltas relative to no-monitoring baseline (required by c2_ctf_main.R)
  ```
- Line 382: name
  ```
  for (col in names(df_ctf_results_delta)) {
  ```
- Line 399: lat, name
  ```
  result_filename <- file.path(bootstrap_ctf_dir, paste0("ctf_results-id-", bootstrap_id, "_simulated.
  ```
- Line 400: name
  ```
  saveRDS(final_results, result_filename)
  ```
- Line 401: name
  ```
  cat("  Saved:", result_filename, "\n")
  ```
- Line 403: lat
  ```
  # Save to CTF_DIR (where c2_ctf_main.R reads in simulated mode)
  ```
- Line 433: name
  ```
  write.csv(tab_7_csv, tab_7_path, row.names = FALSE)
  ```

**/replication-package/jinvass26_replication_may_19_2016/code/simulate/sim_c8b_ctf_robustness.R**

- Line 15: lat
  ```
  if (!exists("SIM_DATA_DIR")) { USE_SIMULATED_DATA <- TRUE; source("code/config.R") }
  ```
- Line 23: name
  ```
  list(model_name = "model_main_2p", csv_name = "tab_a11.csv"),
  ```
- Line 24: name
  ```
  list(model_name = "model_main_4p", csv_name = "tab_a12.csv")
  ```
- Line 30: lon
  ```
  for (idx in seq_along(robustness_specs)) {
  ```
- Line 32: name
  ```
  cat("\n--- Dispatching", spec$model_name, "as subprocess ---\n")
  ```
- Line 34: lat
  ```
  "USE_SIMULATED_DATA <- TRUE; ",
  ```
- Line 39: lat
  ```
  "source('code/simulate/sim_c8b_ctf_robustness.R')"
  ```
- Line 42: name
  ```
  if (rc != 0) cat("  WARNING:", spec$model_name, "failed with exit code", rc, "\n")
  ```
- Line 52: name
  ```
  model_name <- spec$model_name
  ```
- Line 53: name
  ```
  ctf_output_csv <- spec$csv_name
  ```
- Line 56: name
  ```
  cat("Running CTF for:", model_name, "->", ctf_output_csv, "\n")
  ```
- Line 60: name
  ```
  est_csv <- file.path(MODEL_OUT_DIR, model_name, "results", "bootstrap-result-id-0.csv")
  ```
- Line 63: name
  ```
  stop("Estimation output missing for ", model_name)
  ```
- Line 72: son
  ```
  dl_profile <- jsonlite::fromJSON(file.path(SIM_DATA_DIR, "data_profile_data_list.json"))
  ```
- Line 75: lat
  ```
  USE_SIMULATED_ESTIMATES <- TRUE
  ```
- Line 77: name
  ```
  cat("Model:", model_name, "\n")
  ```
- Line 83: lat
  ```
  source("code/simulate/functions/sim_helpers.R")
  ```
- Line 85: name
  ```
  sim_model_dir <- file.path(MODEL_OUT_DIR, model_name)
  ```
- Line 98: lat
  ```
  cat("  Using simulated model dir:", sim_model_dir, "\n")
  ```
- Line 101: lat
  ```
  source("code/simulate/functions/estimation/extract_model_estimates_cmdstan.R")
  ```
- Line 119: name
  ```
  flex_oo_name <- ifelse(grepl("min_flex", ctf_oo_option), "min",
  ```
- Line 122: name
  ```
  bootstrap_ctf_dir <- file.path(MODEL_OUT_DIR, model_name, "ctf")
  ```
- Line 125: block, loc
  ```
  k_block_ctf <- K_BLOCK_CTF
  ```
- Line 135: lat
  ```
  if (USE_SIMULATED_DATA && !USE_CACHE) {
  ```
- Line 140: lat
  ```
  source("code/simulate/functions/ctf/ctf_clear_cache.R")
  ```
- Line 145: lat
  ```
  source("code/simulate/functions/ctf/ctf_preload.R")
  ```
- Line 156: lat
  ```
  source("code/simulate/functions/ctf/ctf_calibration.R")
  ```
- Line 160: lat
  ```
  ## ---- Regime Calculation ---------------------------------------------------
  ```
- Line 162: lat
  ```
  cat("Step 5: Setting up regime calculation...\n")
  ```
- Line 163: lat
  ```
  source("code/simulate/functions/sim_helpers.R")  # for get_samp_wgt_avg
  ```
- Line 173: name
  ```
  varnames <- c(
  ```
- Line 175: second
  ```
  "cp_own", "cp_own_second_period", "cp_tm",
  ```
- Line 183: lat
  ```
  calculate_regime_vars <- function(unmonitored_surcharge, opt_in_discount,
  ```
- Line 189: lat
  ```
  source("code/simulate/functions/ctf/get_ctf_util_profit.R")
  ```
- Line 205: second
  ```
  cp_t2 <- out$cp_second_period
  ```
- Line 225: second
  ```
  cp_own_second_period_var <- get_samp_wgt_avg(rowSums(cp_t2[, 1:J]), sampling_weight) * 100
  ```
- Line 244: second
  ```
  assign(paste0("cp_own_second_period_", suffix), cp_own_second_period_var, envir = .GlobalEnv)
  ```
- Line 263: lat
  ```
  calculate_regime_vars(0, (1 - OPT_IN_FTR) * 100, 100, 100, 0, NA, NA, "current")
  ```
- Line 266: lat
  ```
  calculate_regime_vars(0, NA, NA, NA, 0, NA, NA, "no_monitoring")
  ```
- Line 273: lat
  ```
  source("code/simulate/functions/ctf/ctf_equi_k_save_grid.R")
  ```
- Line 281: lat
  ```
  calculate_regime_vars(
  ```
- Line 288: lat
  ```
  calculate_regime_vars(
  ```
- Line 295: lat
  ```
  calculate_regime_vars(
  ```
- Line 303: lat
  ```
  calculate_regime_vars(
  ```
- Line 322: name
  ```
  df_ctf_results <- data.frame(matrix(NA, nrow = length(regimes), ncol = length(varnames)))
  ```
- Line 323: lname, name
  ```
  colnames(df_ctf_results) <- metrics
  ```
- Line 324: lon
  ```
  for (i in seq_along(suffixes)) {
  ```
- Line 325: lon, name
  ```
  for (j in seq_along(varnames)) {
  ```
- Line 326: name
  ```
  val <- get0(paste0(varnames[j], "_", suffixes[i]))
  ```
- Line 367: name
  ```
  write.csv(csv_df, csv_path, row.names = FALSE)
  ```
- Line 371: lat
  ```
  ## simulated welfare/surplus magnitudes close to real-data tab_a12. See
  ```
- Line 372: lat
  ```
  ## tab_a12_horizon_rescale.R header for rationale + isolation guarantees.
  ```
- Line 373: name
  ```
  if (model_name == "model_main_4p") {
  ```
- Line 374: lat
  ```
  source("code/simulate/tab_a12_horizon_rescale.R")
  ```
- Line 385: name
  ```
  rds_name <- ifelse(model_name == "model_main_2p", "ctf_tab_a11_2p.rds", "ctf_tab_a12_4p.rds")
  ```
- Line 386: name
  ```
  saveRDS(final_results, file.path(CTF_DIR, rds_name))
  ```
- Line 388: name
  ```
  cat("\n", model_name, "CTF complete.\n")
  ```

**/replication-package/jinvass26_replication_may_19_2016/code/simulate/sim_c8c_ctf_mhhet_learning.R**

- Line 16: lat
  ```
  if (!exists("SIM_DATA_DIR")) { USE_SIMULATED_DATA <- TRUE; source("code/config.R") }
  ```
- Line 24: name
  ```
  list(config_suffix = "mhhet",   csv_name = "tab_c4.csv", rds_name = "ctf_tab_c4_mhhet.rds"),
  ```
- Line 25: name
  ```
  list(config_suffix = "learning", csv_name = "tab_c5.csv", rds_name = "ctf_tab_c5_learning.rds")
  ```
- Line 31: lon
  ```
  for (idx in seq_along(robustness_specs)) {
  ```
- Line 35: lat
  ```
  "USE_SIMULATED_DATA <- TRUE; ",
  ```
- Line 40: lat
  ```
  "source('code/simulate/sim_c8c_ctf_mhhet_learning.R')"
  ```
- Line 54: name
  ```
  cat("Running CTF for:", spec$config_suffix, "->", spec$csv_name, "\n")
  ```
- Line 63: name
  ```
  model_name <- if (spec$config_suffix == "mhhet") "model_main_mhhet" else "model_main"
  ```
- Line 65: son
  ```
  dl_profile <- jsonlite::fromJSON(file.path(SIM_DATA_DIR, "data_profile_data_list.json"))
  ```
- Line 68: lat
  ```
  USE_SIMULATED_ESTIMATES <- TRUE
  ```
- Line 74: lat
  ```
  source("code/simulate/functions/sim_helpers.R")
  ```
- Line 76: name
  ```
  sim_model_dir <- file.path(MODEL_OUT_DIR, model_name)
  ```
- Line 89: lat
  ```
  cat("  Using simulated model dir:", sim_model_dir, "\n")
  ```
- Line 96: name
  ```
  if (!"X_rc_clm" %in% names(dl_tmp) && "X_rc" %in% names(dl_tmp)) {
  ```
- Line 105: name
  ```
  # For mhhet: model_name = "model_main_mhhet" → model_config = "mhhe
  ```
- Line 108: name
  ```
  # For learning: model_name = "model_main" → model_config = 
  ```
- Line 110: lat
  ```
  source("code/simulate/functions/estimation/extract_model_estimates_cmdstan.R")
  ```
- Line 134: name
  ```
  flex_oo_name <- ifelse(grepl("min_flex", ctf_oo_option), "min",
  ```
- Line 138: name
  ```
  bootstrap_ctf_dir <- file.path(MODEL_OUT_DIR, model_name, paste0("ctf_", spec$config_suffix))
  ```
- Line 141: block, loc
  ```
  k_block_ctf <- K_BLOCK_CTF
  ```
- Line 151: lat
  ```
  if (USE_SIMULATED_DATA && !USE_CACHE) {
  ```
- Line 156: lat
  ```
  source("code/simulate/functions/ctf/ctf_clear_cache.R")
  ```
- Line 161: lat
  ```
  source("code/simulate/functions/ctf/ctf_preload.R")
  ```
- Line 172: lat
  ```
  source("code/simulate/functions/ctf/ctf_calibration.R")
  ```
- Line 176: lat
  ```
  ## ---- Regime Calculation ---------------------------------------------------
  ```
- Line 178: lat
  ```
  cat("Step 5: Setting up regime calculation...\n")
  ```
- Line 179: lat
  ```
  source("code/simulate/functions/sim_helpers.R")
  ```
- Line 189: name
  ```
  varnames <- c(
  ```
- Line 191: second
  ```
  "cp_own", "cp_own_second_period", "cp_tm",
  ```
- Line 199: lat
  ```
  calculate_regime_vars <- function(unmonitored_surcharge, opt_in_discount,
  ```
- Line 205: lat
  ```
  source("code/simulate/functions/ctf/get_ctf_util_profit.R")
  ```
- Line 221: second
  ```
  cp_t2 <- out$cp_second_period
  ```
- Line 241: second
  ```
  cp_own_second_period_var <- get_samp_wgt_avg(rowSums(cp_t2[, 1:J]), sampling_weight) * 100
  ```
- Line 260: second
  ```
  assign(paste0("cp_own_second_period_", suffix), cp_own_second_period_var, envir = .GlobalEnv)
  ```
- Line 279: lat
  ```
  calculate_regime_vars(0, (1 - OPT_IN_FTR) * 100, 100, 100, 0, NA, NA, "current")
  ```
- Line 282: lat
  ```
  calculate_regime_vars(0, NA, NA, NA, 0, NA, NA, "no_monitoring")
  ```
- Line 289: lat
  ```
  source("code/simulate/functions/ctf/ctf_equi_k_save_grid.R")
  ```
- Line 297: lat
  ```
  calculate_regime_vars(
  ```
- Line 304: lat
  ```
  calculate_regime_vars(
  ```
- Line 311: lat
  ```
  calculate_regime_vars(
  ```
- Line 319: lat
  ```
  calculate_regime_vars(
  ```
- Line 338: name
  ```
  df_ctf_results <- data.frame(matrix(NA, nrow = length(regimes), ncol = length(varnames)))
  ```
- Line 339: lname, name
  ```
  colnames(df_ctf_results) <- metrics
  ```
- Line 340: lon
  ```
  for (i in seq_along(suffixes)) {
  ```
- Line 341: lon, name
  ```
  for (j in seq_along(varnames)) {
  ```
- Line 342: name
  ```
  val <- get0(paste0(varnames[j], "_", suffixes[i]))
  ```
- Line 381: name
  ```
  csv_path <- file.path(CTF_DIR, spec$csv_name)
  ```
- Line 382: name
  ```
  write.csv(csv_df, csv_path, row.names = FALSE)
  ```
- Line 392: name
  ```
  saveRDS(final_results, file.path(CTF_DIR, spec$rds_name))
  ```

**/replication-package/jinvass26_replication_may_19_2016/code/simulate/sim_c8d_ctf_appendix_robustness.R**

- Line 19: lat
  ```
  if (!exists("SIM_DATA_DIR")) { USE_SIMULATED_DATA <- TRUE; source("code/config.R") }
  ```
- Line 29: name
  ```
  csv_name = "tab_a9.csv", rds_name = "ctf_tab_a9_low_cost.rds",
  ```
- Line 37: name
  ```
  csv_name = "tab_a10.csv", rds_name = "ctf_tab_a10_high_cost.rds",
  ```
- Line 45: name
  ```
  csv_name = "tab_a13.csv", rds_name = "ctf_tab_a13_unconst.rds",
  ```
- Line 53: name
  ```
  csv_name = "tab_a14.csv", rds_name = "ctf_tab_a14_minmed.rds",
  ```
- Line 61: name
  ```
  csv_name = "tab_a15.csv", rds_name = "ctf_tab_a15_medflex.rds",
  ```
- Line 73: lon
  ```
  for (idx in seq_along(robustness_specs)) {
  ```
- Line 77: lat
  ```
  "USE_SIMULATED_DATA <- TRUE; ",
  ```
- Line 82: lat
  ```
  "source('code/simulate/sim_c8d_ctf_appendix_robustness.R')"
  ```
- Line 99: name
  ```
  cat("Running CTF:", spec$label, "->", spec$csv_name, "\n")
  ```
- Line 107: name
  ```
  model_name <- "model_main"
  ```
- Line 109: son
  ```
  dl_profile <- jsonlite::fromJSON(file.path(SIM_DATA_DIR, "data_profile_data_list.json"))
  ```
- Line 112: lat
  ```
  USE_SIMULATED_ESTIMATES <- TRUE
  ```
- Line 118: lat
  ```
  source("code/simulate/functions/sim_helpers.R")
  ```
- Line 120: name
  ```
  sim_model_dir <- file.path(MODEL_OUT_DIR, model_name)
  ```
- Line 133: lat
  ```
  source("code/simulate/functions/estimation/extract_model_estimates_cmdstan.R")
  ```
- Line 150: name
  ```
  flex_oo_name <- ifelse(grepl("min_flex", ctf_oo_option), "min",
  ```
- Line 153: name
  ```
  bootstrap_ctf_dir <- file.path(MODEL_OUT_DIR, model_name, spec$ctf_subdir)
  ```
- Line 156: block, loc
  ```
  k_block_ctf <- K_BLOCK_CTF
  ```
- Line 169: lat
  ```
  if (USE_SIMULATED_DATA && !USE_CACHE) {
  ```
- Line 174: lat
  ```
  source("code/simulate/functions/ctf/ctf_clear_cache.R")
  ```
- Line 179: lat
  ```
  source("code/simulate/functions/ctf/ctf_preload.R")
  ```
- Line 190: lat
  ```
  source("code/simulate/functions/ctf/ctf_calibration.R")
  ```
- Line 194: lat
  ```
  ## ---- Regime Calculation ---------------------------------------------------
  ```
- Line 196: lat
  ```
  cat("Step 5: Setting up regime calculation...\n")
  ```
- Line 206: name
  ```
  varnames <- c(
  ```
- Line 208: second
  ```
  "cp_own", "cp_own_second_period", "cp_tm",
  ```
- Line 216: lat
  ```
  calculate_regime_vars <- function(unmonitored_surcharge, opt_in_discount,
  ```
- Line 222: lat
  ```
  source("code/simulate/functions/ctf/get_ctf_util_profit.R")
  ```
- Line 238: second
  ```
  cp_t2 <- out$cp_second_period
  ```
- Line 258: second
  ```
  cp_own_second_period_var <- get_samp_wgt_avg(rowSums(cp_t2[, 1:J]), sampling_weight) * 100
  ```
- Line 277: second
  ```
  assign(paste0("cp_own_second_period_", suffix), cp_own_second_period_var, envir = .GlobalEnv)
  ```
- Line 296: lat
  ```
  calculate_regime_vars(0, (1 - OPT_IN_FTR) * 100, 100, 100, 0, NA, NA, "current")
  ```
- Line 299: lat
  ```
  calculate_regime_vars(0, NA, NA, NA, 0, NA, NA, "no_monitoring")
  ```
- Line 306: lat
  ```
  source("code/simulate/functions/ctf/ctf_equi_k_save_grid.R")
  ```
- Line 314: lat
  ```
  calculate_regime_vars(
  ```
- Line 321: lat
  ```
  calculate_regime_vars(
  ```
- Line 328: lat
  ```
  calculate_regime_vars(
  ```
- Line 336: lat
  ```
  calculate_regime_vars(
  ```
- Line 355: name
  ```
  df_ctf_results <- data.frame(matrix(NA, nrow = length(regimes), ncol = length(varnames)))
  ```
- Line 356: lname, name
  ```
  colnames(df_ctf_results) <- metrics
  ```
- Line 357: lon
  ```
  for (i in seq_along(suffixes)) {
  ```
- Line 358: lon, name
  ```
  for (j in seq_along(varnames)) {
  ```
- Line 359: name
  ```
  val <- get0(paste0(varnames[j], "_", suffixes[i]))
  ```
- Line 398: name
  ```
  csv_path <- file.path(CTF_DIR, spec$csv_name)
  ```
- Line 399: name
  ```
  write.csv(csv_df, csv_path, row.names = FALSE)
  ```
- Line 409: name
  ```
  saveRDS(final_results, file.path(CTF_DIR, spec$rds_name))
  ```

**/replication-package/jinvass26_replication_may_19_2016/code/simulate/simulate_data/sim_generate_data_list.R**

- Line 2: son
  ```
  ## sim_generate_data_list.R — Build synthetic data_list_IL.js
  ```
- Line 6: son
  ```
  ## Output is written as JSON via jsonlite for portability.
  ```
- Line 13: lat
  ```
  ## Usage: Rscript code/simulate/simulate_data/sim_generate_data_list.R
  ```
- Line 16: lat
  ```
  if (!exists("SIM_DATA_DIR")) { USE_SIMULATED_DATA <- TRUE; source("code/config.R") }
  ```
- Line 30: son
  ```
  read_data_list <- function(json_path) {
  ```
- Line 31: name, son
  ```
  if (!requireNamespace("jsonlite", quietly = TRUE))
  ```
- Line 32: son
  ```
  install.packages("jsonlite")
  ```
- Line 33: son
  ```
  dl <- jsonlite::fromJSON(json_path, simplifyVector = TRUE)
  ```
- Line 34: name
  ```
  for (nm in names(dl)) {
  ```
- Line 51: lname, name
  ```
  if (!is.null(dl$prices_raw) && is.null(colnames(dl$prices_raw)))
  ```
- Line 52: lname, name
  ```
  colnames(dl$prices_raw) <- as.character(seq_len(ncol(dl$prices_raw)))
  ```
- Line 53: lname, name
  ```
  if (!is.null(dl$prices_oo_full) && is.null(colnames(dl$prices_oo_full))) {
  ```
- Line 55: lname, name
  ```
  colnames(dl$prices_oo_full) <- as.vector(outer(OO_FIRMS, oo_covs, paste, sep = "_"))
  ```
- Line 62: son
  ```
  profile_path <- file.path(SIM_DATA_DIR, "data_profile_data_list.json")
  ```
- Line 64: son
  ```
  stop("data_profile_data_list.json not found at ", profile_path,
  ```
- Line 65: lat
  ```
  "\n  This file must be provided in the replication package under data/simulated/.")
  ```
- Line 66: name, son
  ```
  if (!requireNamespace("jsonlite", quietly = TRUE)) install.packages("jsonlite")
  ```
- Line 67: son
  ```
  prof <- jsonlite::fromJSON(profile_path, simplifyVector = TRUE)
  ```
- Line 84: lat
  ```
  ## ---- Load real-data structural parameters (used for TM selection + outcome simulation)
  ```
- Line 88: lat
  ```
  source("code/simulate/functions/sim_helpers.R")
  ```
- Line 114: name
  ```
  for (nm in names(dim)) dl[[nm]] <- dim[[nm]]
  ```
- Line 121: name
  ```
  for (nm in names(rs)) dl[[nm]] <- rs[[nm]]
  ```
- Line 130: name, son
  ```
  # Resolve X column names (JSON profiles lose named-vector keys)
  ```
- Line 131: lname, name
  ```
  X_colnames <- names(xp$col_means)
  ```
- Line 132: lname, name
  ```
  if (is.null(X_colnames) || length(X_colnames) != M) X_colnames <- xp$col_names
  ```
- Line 133: lname, name
  ```
  if (is.null(X_colnames) || length(X_colnames) != M) {
  ```
- Line 134: name, son
  ```
  stop("X column names not found in data profile. Ensure col_names is present in data_profile_data_lis
  ```
- Line 136: lname, name
  ```
  colnames(X_raw) <- X_colnames
  ```
- Line 140: lat
  ```
  # column's marginal distribution (binary, ordinal, zero-inflated, or continuous).
  ```
- Line 158: lname, name
  ```
  colnames(X_raw) <- X_colnames
  ```
- Line 164: lname, name
  ```
  rc_idx <- which(X_colnames == "rc")
  ```
- Line 211: lname, name
  ```
  rc_idx <- which(X_colnames == "rc")
  ```
- Line 225: lname, name
  ```
  veh_idx <- grep("x_veh", colnames(dl$X))
  ```
- Line 281: son
  ```
  # Ensure matrix types (JSON profile may return lists)
  ```
- Line 287: lname, name
  ```
  rc_col_tmp <- which(colnames(dl$X) == "rc")
  ```
- Line 297: lname, name
  ```
  rc_col <- which(colnames(dl$X) == "rc")
  ```
- Line 303: lat
  ```
  # Fix log(X_rc) correlation with leps: the price model uses log_X_rc
  ```
- Line 316: lat
  ```
  cat("  X_rc-leps correlation:", round(cor(xrc_i, leps), 4), "\n")
  ```
- Line 369: lat
  ```
  # TM participants tend to be moderate-risk types with leps sd ~0.19 vs population 0.36)
  ```
- Line 399: lat
  ```
  ## ---- 5. Outcome variables (placeholder — replaced by c1_simulate_data.R) --
  ```
- Line 403: lat
  ```
  # Claims (will be overwritten by simulation)
  ```
- Line 430: lon
  ```
  # Major below limit: Pareto draws, truncated at standalone per-obs coverage limits
  ```
- Line 443: lon
  ```
  # Major at limit: standalone coverage limits as censored severity values
  ```
- Line 453: lat
  ```
  # clm_sev_raw and log_tm_score: populated in fixup section below (after dependent fields are set)
  ```
- Line 459: block, loc
  ```
  # Choices — per-block sampling to respect valid d_t_new rang
  ```
- Line 460: block, loc
  ```
  # Block 1-4 (NB): no attrition (d_t_new >= 1); Blocks 2/4 (TM): 2xJs options
  ```
- Line 461: block, loc
  ```
  # Block 5-6 (renewal): 0=attrition allowed
  ```
- Line 464: block, loc
  ```
  if (!is.null(om$d_t_new_dist_by_block)) {
  ```
- Line 468: block, loc
  ```
  dbb <- om$d_t_new_dist_by_block
  ```
- Line 469: lon
  ```
  for (k in seq_along(rs$Js)) {
  ```
- Line 484: block, loc
  ```
  # Fallback: aggregate distribution (legacy profile without per-block)
  ```
- Line 515: son
  ```
  ## Person-level risk factor for AGGREGATE prices (prices_raw, prices_oo_full, prices_paid).
  ```
- Line 517: son
  ```
  z_price_person <- as.numeric(scale(dl$X_rc))
  ```
- Line 532: son
  ```
  # Ensure covariance matrices are proper R matrices (JSON may load them as lists)
  ```
- Line 550: name
  ```
  # Ensure means are numeric vectors (not named lists)
  ```
- Line 633: name
  ```
  med_f <- names(sort(cov4_m))[ceiling(length(cov4_m) / 2)]
  ```
- Line 634: name
  ```
  min_f <- names(which.min(cov4_m))
  ```
- Line 665: lat
  ```
  # OO and KC: use marginals with high within-firm correlation
  ```
- Line 671: lat
  ```
  # Build covariance with ~0.99 within-firm correlation (matching real data pattern)
  ```
- Line 735: lat
  ```
  # This preserves cross-column correlations while matching each column's marginal.
  ```
- Line 743: name
  ```
  # Name mapping: dl field -> profile quantile grid key (handles oo_renw vs renw_oo naming)
  ```
- Line 744: name
  ```
  qgrid_name_map <- c(
  ```
- Line 750: name
  ```
  qgrid_key <- if (pf %in% names(qgrid_name_map)) qgrid_name_map[[pf]] else paste0(pf, "_qgrid")
  ```
- Line 776: lon
  ```
  for (fi in seq_along(oo_firms_adj)) {
  ```
- Line 778: name
  ```
  if (fid %in% names(OO_PRICE_MULT_ADJ)) {
  ```
- Line 836: lat
  ```
  # Fix own-OO renewal price correlation: real data has cor ~0.25, sim has ~0
  ```
- Line 837: lat, son
  ```
  # Mix in own-price person factor to create cross-firm correlation
  ```
- Line 838: lat, son
  ```
  # This preserves marginal means/sds while adding the person-level correlation
  ```
- Line 844: son
  ```
  own_person <- scale(rowMeans(dl[[own_key]]))[,1]  # standardized own-price person factor
  ```
- Line 848: son
  ```
  col_resid <- scale(residuals(lm(dl[[full_key]][, j] ~ own_person)))[,1]
  ```
- Line 850: son
  ```
  sqrt(1 - target_own_oo_cor^2) * col_resid + target_own_oo_cor * own_person
  ```
- Line 857: name
  ```
  # Stan-expected field names for renewal: prices_renw_oo_* (not prices_oo_renw_*)
  ```
- Line 869: lat, son
  ```
  # Aggregate prices_raw (9 cols) and prices_oo_full (25 cols) — correlated across pers
  ```
- Line 872: name, son
  ```
  # Resolve column names (JSON loses named-vector keys)
  ```
- Line 873: name
  ```
  raw_names <- names(raw_means)
  ```
- Line 874: name
  ```
  if (is.null(raw_names) || length(raw_names) != 9) raw_names <- pr$prices_raw_col_names
  ```
- Line 888: lname, name
  ```
  colnames(pna) <- raw_names
  ```
- Line 889: lon
  ```
  for (j in seq_along(pna_fracs)) {
  ```
- Line 906: son
  ```
  (PRICE_RHO * z_price_person[not_na] + sqrt(1 - PRICE_RHO^2) * rnorm(sum(not_na)))
  ```
- Line 910: son
  ```
  (PRICE_RHO * z_price_person + sqrt(1 - PRICE_RHO^2) * rnorm(N_choice))
  ```
- Line 913: name
  ```
  if (!is.null(raw_names) && length(raw_names) == 9) {
  ```
- Line 914: lname, name
  ```
  colnames(dl$prices_raw) <- raw_names
  ```
- Line 916: lname, name
  ```
  colnames(dl$prices_raw) <- as.character(1:9)
  ```
- Line 923: name
  ```
  # Resolve OO column names
  ```
- Line 924: name
  ```
  oo_names <- names(oo_means_raw)
  ```
- Line 925: name
  ```
  if (is.null(oo_names) || length(oo_names) != 25) oo_names <- pr$prices_oo_full_col_names
  ```
- Line 933: name
  ```
  if (!is.null(oo_names)) {
  ```
- Line 934: name
  ```
  cov3_cols <- grep("_3$", oo_names)
  ```
- Line 935: lname, name
  ```
  fracs[cov3_cols] <- if ("3" %in% colnames(pna)) mean(pna[, "3"]) else 1
  ```
- Line 954: son
  ```
  (PRICE_RHO * z_price_person[not_na] + sqrt(1 - PRICE_RHO^2) * rnorm(sum(not_na)))
  ```
- Line 958: son
  ```
  (PRICE_RHO * z_price_person + sqrt(1 - PRICE_RHO^2) * rnorm(N_choice))
  ```
- Line 961: name
  ```
  if (!is.null(oo_names) && length(oo_names) == 25) {
  ```
- Line 962: lname, name
  ```
  colnames(dl$prices_oo_full) <- oo_names
  ```
- Line 964: lname, name
  ```
  colnames(dl$prices_oo_full) <- as.vector(outer(oo_firms, oo_covs, paste, sep = "_"))
  ```
- Line 968: lat, son
  ```
  # Aggregate paid price — correlated with person ri
  ```
- Line 970: son
  ```
  (PRICE_RHO * z_price_person + sqrt(1 - PRICE_RHO^2) * rnorm(N_choice))
  ```
- Line 1103: lat
  ```
  obs_per_i <- tabulate(dl$N_choice_to_I, nbins = I)
  ```
- Line 1107: lat
  ```
  # Use log-normal (real data is right-skewed; rnorm + pmax inflated the mean by ~12%)
  ```
- Line 1108: zip
  ```
  zi_sigma2 <- log(1 + (gd$zip_income_sd / gd$zip_income_mean)^2)
  ```
- Line 1109: zip
  ```
  zi_mu     <- log(gd$zip_income_mean) - zi_sigma2 / 2
  ```
- Line 1110: zip
  ```
  dl$zip_income <- rlnorm(N_choice, zi_mu, sqrt(zi_sigma2))
  ```
- Line 1111: zip
  ```
  dl$zip_income_raw <- dl$zip_income * 5
  ```
- Line 1112: zip
  ```
  dl$income_choice <- dl$zip_income / dim$dollar_norm
  ```
- Line 1122: lat
  ```
  # Sampling weights: all 1 for simulated data (no stratified subsampling)
  ```
- Line 1148: block, loc
  ```
  ## ---- 11. Estimation config (Stan data block requires these) -----------------
  ```
- Line 1154: block, loc
  ```
  dl$run_demand_blocks <- rep(1L, dim$N_choice_regimes)
  ```
- Line 1156: block, loc
  ```
  dl$d_by_block <- rs$N_by_choice_regime
  ```
- Line 1189: lat
  ```
  ## ---- Fixup: fields that depend on variables set in later sections -----------
  ```
- Line 1191: lat
  ```
  # log_tm_score: populate for TM enrollees (tm_ind was set in section 7)
  ```
- Line 1200: name
  ```
  if (!is.na(s) && s %in% names(scheme_shift_map)) {
  ```
- Line 1208: lat
  ```
  # Score-correlated renewal factor: higher score -> higher discount factor (Fig B2b)
  ```
- Line 1228: block, loc
  ```
  # d_t1_new: cap per renewal block to Js[k] (block 5: 1-5, block 6: 1-4)
  ```
- Line 1229: block, loc
  ```
  # In real data, MM2 renewals (block 6) never have d_t1_new > 4 because
  ```
- Line 1230: lat
  ```
  # MM2 NB only has 4 coverage tiers. Simulated panel may cross MM boundaries.
  ```
- Line 1233: block, loc
  ```
  # Block 5 (MM1 renewals): cap to Js[5] = 5
  ```
- Line 1238: block, loc
  ```
  # Block 6 (MM2 renewals): cap to Js[6] = 4
  ```
- Line 1245: name
  ```
  # Rename Stan-expected field for OO renewal prices
  ```
- Line 1262: lat
  ```
  ## ---- 13. Structural outcome simulation (claims, scores, choices) ------------
  ```
- Line 1266: lat
  ```
  cat("\n--- Structural outcome simulation ---\n")
  ```
- Line 1269: son
  ```
  ## B. Claims — Poisson(exp(theta_0 + X * theta_1 + leps[i]
  ```
- Line 1276: son
  ```
  # Independent Poisson (Stan model treats total and severe as separate likelihoods)
  ```
- Line 1296: lat
  ```
  ## (leps * theta_1[,1]) is too large because simulated leps have wider variance
  ```
- Line 1324: name
  ```
  if (!requireNamespace("cmdstanr", quietly = TRUE))
  ```
- Line 1325: lat
  ```
  stop("cmdstanr required for choice simulation")
  ```
- Line 1328: name
  ```
  model_name <- "model_main"
  ```
- Line 1345: lat
  ```
  # Point extraction at real-data model output (not simulated)
  ```
- Line 1347: name
  ```
  real_model_dir <- file.path("data/estimates/model_output", model_name)
  ```
- Line 1348: name
  ```
  log_file_suffix <- LOG_FILE_SUFFIXES[[model_name]]
  ```
- Line 1356: lat
  ```
  # computes latent params
  ```
- Line 1357: lat
  ```
  source("code/simulate/functions/estimation/extract_model_estimates_cmdstan.R")
  ```
- Line 1364: lat
  ```
  # Compute latent params and choice probabilities
  ```
- Line 1366: lat
  ```
  source("code/simulate/functions/estimation/get_c_latentparams.R")
  ```
- Line 1367: lat
  ```
  source("code/simulate/functions/estimation/get_d_latentparams.R")
  ```
- Line 1369: lat
  ```
  # Scale sigma_logit by 1/(NUM_RENEWAL_PERIODS-1) for per-period choice simulation
  ```
- Line 1376: lat
  ```
  source("code/simulate/functions/estimation/get_d_pred.R")
  ```
- Line 1384: lon
  ```
  for (k in seq_along(dl$Js)) {
  ```
- Line 1392: block, loc
  ```
  # Blocks 1-4 (NB): 1-indexed plans, no attrition
  ```
- Line 1393: block, loc
  ```
  # Blocks 5-6 (renewal): columns 1..J = plans, column J+1 = outside option (d_t_new=0)
  ```
- Line 1408: lat
  ```
  # simulated d_t_new would introduce inconsistency (attrition→random replacement
  ```
- Line 1416: lat
  ```
  ## E. Update TM mappings based on simulated choices
  ```
- Line 1418: block, loc
  ```
  ## actually chose TM (d_t_new > Js[k] in blocks 2, 4)
  ```
- Line 1424: block, loc
  ```
  block_choices <- dl$d_t_new[n0:n1]
  ```
- Line 1426: block, loc
  ```
  tm_obs <- which(block_choices > J_k)
  ```
- Line 1457: lat
  ```
  ## F. Re-simulate TM scores for the updated TM participants
  ```
- Line 1458: lat
  ```
  cat("  Re-simulating TM scores for updated TM mapping...\n")
  ```
- Line 1484: lat
  ```
  cat("--- Structural simulation complete ---\n\n")
  ```
- Line 1490: name, son
  ```
  if (!requireNamespace("jsonlite", quietly = TRUE)) install.packages("jsonlite")
  ```
- Line 1496: son
  ```
  json_path <- file.path(cache_dir, "data_list_IL.json")
  ```
- Line 1497: son
  ```
  jsonlite::write_json(dl, json_path, auto_unbox = TRUE, digits = 8)
  ```
- Line 1498: son
  ```
  cat("  Saved to", json_path, "\n")
  ```
- Line 1504: son
  ```
  ## ---- JSON round-trip verification ------------------------------------------
  ```
- Line 1506: son
  ```
  cat("\nJSON round-trip verification (all fields)...\n")
  ```
- Line 1507: son
  ```
  dl_json <- read_data_list(json_path)
  ```
- Line 1509: name, son
  ```
  all_fields <- union(names(dl), names(dl_json))
  ```
- Line 1512: name, son
  ```
  if (!(field %in% names(dl)) || !(field %in% names(dl_json))) {
  ```
- Line 1517: son
  ```
  rt <- dl_json[[field]]
  ```
- Line 1545: son
  ```
  cat("  JSON round-trip:", n_pass, "PASS,", n_fail, "FAIL,", n_skip, "SKIP\n")
  ```
- Line 1548: son
  ```
  cat("  NOTE: Pipeline uses RDS (not JSON) for estimation/CTF. JSON losses are informational only.\n"
  ```

**/replication-package/jinvass26_replication_may_19_2016/code/simulate/simulate_data/sim_generate_rf_data.R**

- Line 7: lat, son
  ```
  ## Reads: data/simulated/data_profile_rf.json
  ```
- Line 8: lat
  ```
  ## Writes: data/simulated/ — 4 file
  ```
- Line 11: lat
  ```
  ##   data_rf_viol.csv   — violation lags (3-state, appendix onl
  ```
- Line 14: lat
  ```
  ## Usage: Rscript code/simulate/simulate_data/sim_generate_rf_data.R
  ```
- Line 17: lat
  ```
  if (!exists("SIM_DATA_DIR")) { USE_SIMULATED_DATA <- TRUE; source("code/config.R") }
  ```
- Line 30: lat
  ```
  ## These embed the paper's empirical findings into the simulated data so that
  ```
- Line 34: son
  ```
  ## ---- Load RF data profile (single JSON with moments + panel stats) ----------
  ```
- Line 35: son
  ```
  profile_path <- file.path(SIM_DATA_DIR, "data_profile_rf.json")
  ```
- Line 37: son
  ```
  stop("data_profile_rf.json not found at ", profile_path,
  ```
- Line 38: lat
  ```
  "\n  This file must be provided in the replication package under data/simulated/.")
  ```
- Line 39: name, son
  ```
  if (!requireNamespace("jsonlite", quietly = TRUE)) install.packages("jsonlite")
  ```
- Line 40: son
  ```
  profile <- jsonlite::fromJSON(profile_path, simplifyVector = TRUE)
  ```
- Line 52: lat
  ```
  ## floating-point path used by the main repo's LaTeX regex extraction
  ```
- Line 54: son
  ```
  parse_tab2 <- function(json_path = "data/estimates/regression_output/tab_2_regression.json") {
  ```
- Line 56: son
  ```
  if (!file.exists(json_path)) return(result)
  ```
- Line 58: son
  ```
  reg <- jsonlite::fromJSON(json_path)
  ```
- Line 60: lat
  ```
  # implied_mh_pct[3] → format as "XX.XX%" then parse back, matching LaTeX pa
  ```
- Line 64: lat
  ```
  # Col 3 monitoring coefficient → format as "X.XXX" then parse back, matching LaTeX pa
  ```
- Line 122: name
  ```
  if (!is.null(stat$top5) && length(stat$top5) > 0 && !is.null(names(stat$top5))) {
  ```
- Line 123: name
  ```
  lvls <- names(stat$top5)
  ```
- Line 165: lat
  ```
  ## --- Latent risk per driver (shared across all downstream data) ---
  ```
- Line 166: lat
  ```
  ## This drives claims, selection into monitoring, and score-claim correlation.
  ```
- Line 189: name
  ```
  state_map <- setNames(1:length(states), states)
  ```
- Line 194: name
  ```
  covariate_cols <- setdiff(names(panel_stats),
  ```
- Line 203: lname, name
  ```
  if (!"x_drvr_lic_yr" %in% colnames(panel_rows)) {
  ```
- Line 215: lat
  ```
  # Ensure key relationships
  ```
- Line 225: lat
  ```
  # --- Inject latent risk into observable covariates ---
  ```
- Line 234: lname, name
  ```
  if ("tier_acci_drvr_pt" %in% colnames(panel_rows)) {
  ```
- Line 239: lat
  ```
  # tier_good_ind: 1 for drivers with below-median violation points
  ```
- Line 242: lat
  ```
  # clm_srchg: claim surcharge increases with violation points
  ```
- Line 252: lname, name
  ```
  if ("x_drvr_age_rated" %in% colnames(panel_rows)) {
  ```
- Line 260: lname, name
  ```
  if ("x_cred_score" %in% colnames(panel_rows)) {
  ```
- Line 267: loc, zip
  ```
  # x_loc_zipcd_agi: lower income → higher ri
  ```
- Line 268: lname, loc, name, zip
  ```
  if ("x_loc_zipcd_agi" %in% colnames(panel_rows)) {
  ```
- Line 269: loc, zip
  ```
  orig <- panel_rows$x_loc_zipcd_agi
  ```
- Line 271: loc, zip
  ```
  panel_rows$x_loc_zipcd_agi <- pmax(1000, mu + s * (0.75 * scale(orig)[,1] - 0.25 * risk_z_panel))
  ```
- Line 275: lname, name
  ```
  if ("x_drvr_yr_edu" %in% colnames(panel_rows)) {
  ```
- Line 282: lat
  ```
  # Claims driven by latent risk
  ```
- Line 349: loc, zip
  ```
  # x_loc_zipcd_lg_inc: log zipcode income (used by getX("reg"))
  ```
- Line 350: loc, zip
  ```
  panel_rows$x_loc_zipcd_lg_inc <- rnorm(N_ROWS, 10.5, 0.8)
  ```
- Line 354: lname, loc, name
  ```
  if (!"x_loc_grg_adrs_verify_ind" %in% colnames(panel_rows))
  ```
- Line 355: loc
  ```
  panel_rows$x_loc_grg_adrs_verify_ind <- rbinom(N_ROWS, 1, 0.5)
  ```
- Line 356: lname, loc, name
  ```
  if (!"x_loc_popltn_dens_grp" %in% colnames(panel_rows))
  ```
- Line 357: loc
  ```
  panel_rows$x_loc_popltn_dens_grp <- sample(0:9, N_ROWS, replace = TRUE)
  ```
- Line 358: lname, name
  ```
  if (!"x_hh_hlth_ins_ind" %in% colnames(panel_rows))
  ```
- Line 360: lname, name
  ```
  if (!"tier_pop_cont_ins_ind" %in% colnames(panel_rows))
  ```
- Line 362: lname, name
  ```
  if (!"tier_pop_no_ind" %in% colnames(panel_rows))
  ```
- Line 364: lname, name
  ```
  if (!"tier_pop_not_over_min_ind" %in% colnames(panel_rows))
  ```
- Line 366: lname, name
  ```
  if (!"tier_pop_limit" %in% colnames(panel_rows))
  ```
- Line 368: lname, name
  ```
  if (!"tier_pop_prog_lim" %in% colnames(panel_rows))
  ```
- Line 370: lname, name
  ```
  if (!"tier_pop_prir_liab_lim" %in% colnames(panel_rows))
  ```
- Line 372: lname, name
  ```
  if (!"tier_pop_prir_liab_lim_missing" %in% colnames(panel_rows))
  ```
- Line 407: lat
  ```
  # Map driver-level latent variables to df_mh rows
  ```
- Line 416: lat
  ```
  # Make it correlate with risk so selection patterns are preserved
  ```
- Line 430: lat
  ```
  # Each driver's claim rate is driven by their latent risk
  ```
- Line 444: name
  ```
  col_name <- paste0("clm_acci_renw_", q)
  ```
- Line 447: name
  ```
  df_mh_base[[col_name]] <- val
  ```
- Line 458: lat
  ```
  # 3-state subset for violation analysis
  ```
- Line 464: lat
  ```
  # Look up driver-level risk and monitoring status for each violation row
  ```
- Line 470: lat
  ```
  # Violation indicators (lag periods 1-6) — risk-adjusted with monitoring moral haza
  ```
- Line 477: name
  ```
  col_name <- paste0("VIOL_", v_type, "_", lag)
  ```
- Line 480: lat
  ```
  # Monitoring moral hazard: fewer violations during monitoring (renw 1-2),
  ```
- Line 485: name
  ```
  panel_viol[[col_name]] <- rbinom(n_viol, 1, pmin(p, 1))
  ```
- Line 506: city
  ```
  # Rate revision timing — creates the instrument for demand elastici
  ```
- Line 532: city
  ```
  # Renewal decision depends on price change (creates demand elasticity)
  ```
- Line 540: lat
  ```
  # So simulated premiums must also bake in the discount for monitored finishers.
  ```
- Line 542: lat, son
  ```
  # not just latent risk. High noise ensures reasonable spread across segments:
  ```
- Line 579: city
  ```
  # Elasticity by monitoring group (matches tm_segment_ftr classification):
  ```
- Line 583: city
  ```
  # Align elasticity with the ACTUAL tm_segment_ftr classification used by
  ```
- Line 595: city
  ```
  # Group-specific elasticity on undiscounted price change,
  ```
- Line 603: name
  ```
  # Rate revision dates — use raw RENW_QT_ column names so panel_renw_clean() can process th
  ```
- Line 621: name
  ```
  # Driver demographics (raw RENW_QT_ names for panel_renw_clean)
  ```
- Line 622: sex
  ```
  RENW_QT_DRVR_SEX_CD = sample(c("M", "F"), n_renw, replace = TRUE),
  ```
- Line 691: name
  ```
  PRIOR_CARRIER_NAME = sample(c("Carrier1","Carrier2","Carrier3"), n_renw, replace = TRUE)
  ```
- Line 707: lat
  ```
  ## ---- 6. Generate panel_ubi_renw (IL) — score correlates with risk ---------
  ```
- Line 723: lat
  ```
  # UBI_VALUE_MAX (monitoring score) correlates with RISK:
  ```
- Line 725: lat
  ```
  # Strong correlation (1.2) ensures strictly monotonic quintile means
  ```
- Line 821: lat
  ```
  ## ---- 9. Generate processed files (with score-claim correlation) -------------
  ```
- Line 851: lat
  ```
  # Score-risk correlation: higher risk → higher score value (Fig 
  ```
- Line 852: lat
  ```
  # Strong correlation (1.2) ensures strictly monotonic quintile claims
  ```
- Line 867: lat
  ```
  # Discount inversely correlates with risk
  ```
- Line 889: lname, name
  ```
  colnames(future)[2] <- paste0("clm_cnt_renw_", t_fwd, "_liab")
  ```
- Line 898: name
  ```
  # (with proper column names). No need to pre-generate placeholders here.
  ```
- Line 913: name
  ```
  # that breaks panel_renw_clean()'s rename(x_drvr_lic_yr = DRVR_YR_LIC).
  ```
- Line 929: lname, name
  ```
  cp_extra <- choice_panel[, intersect(cp_extra_cols, colnames(choice_panel))]
  ```
- Line 942: lname, name
  ```
  renw_qt_cols <- grep("^RENW_QT_", colnames(panel_renw), value = TRUE)
  ```
- Line 946: name
  ```
  "PRIOR_CARRIER_NAME", "REQ_DWNPMT_PCT")
  ```
- Line 947: lname, name
  ```
  renw_join_cols <- intersect(renw_join_cols, colnames(panel_renw))
  ```
- Line 959: lname, name
  ```
  df_mh_join <- df_mh[, intersect(df_mh_cols, colnames(df_mh))]
  ```
- Line 964: lname, name
  ```
  dup_suffixes <- grep("\\.(ubi_renw|renw|dfmh)$", colnames(data_rf), value = TRUE)
  ```
- Line 966: lname, name
  ```
  data_rf <- data_rf[, !colnames(data_rf) %in% dup_suffixes]
  ```
- Line 975: name
  ```
  write.csv(data_rf, file.path(SIM_DIR, "data_rf.csv"), row.names = FALSE)
  ```
- Line 979: city
  ```
  # Columns needed by panel_renw_clean.R for demand elasticity analysis
  ```
- Line 984: name
  ```
  "PRIOR_CARRIER_NAME", "REQ_DWNPMT_PCT")
  ```
- Line 985: lname, name
  ```
  rrev_cols <- intersect(rrev_cols, colnames(panel_renw))
  ```
- Line 987: name
  ```
  write.csv(data_rf_rrev, file.path(SIM_DIR, "data_rf_rrev.csv"), row.names = FALSE)
  ```
- Line 990: lat
  ```
  # --- data_rf_viol.csv: violation lags (all states) ---
  ```
- Line 991: name
  ```
  write.csv(panel_viol, file.path(SIM_DIR, "data_rf_viol.csv"), row.names = FALSE)
  ```
- Line 995: name
  ```
  write.csv(ubi_vers_st_dt_sum, file.path(SIM_DIR, "ubi_vers_dates.csv"), row.names = FALSE)
  ```
- Line 1000: lat
  ```
  cat("Simulated RF data generation complete.\n")
  ```
- Line 1007: lat
  ```
  cat("\nTo use simulated data, set in config.R:\n")
  ```
- Line 1008: lat
  ```
  cat("  USE_SIMULATED_DATA <- TRUE\n")
  ```
- Line 1009: son
  ```
  cat("\nFor structural estimation data_list_IL.json, run separately:\n")
  ```
- Line 1010: lat
  ```
  cat("  Rscript code/simulate/simulate_data/sim_generate_data_list.R\n")
  ```

**/replication-package/jinvass26_replication_may_19_2016/code/simulate/tab_a12_horizon_rescale.R**

- Line 10: lat
  ```
  ##   cost_factors against simulated market shares, which produces calibrated
  ```
- Line 12: lat
  ```
  ##   per-year dollar magnitudes are inflated by ~4/3 relative to the real-data
  ```
- Line 20: lat
  ```
  ## ISOLATION GUARANTEES
  ```
- Line 21: lat
  ```
  ##   - Reads:  output/simulated/precomputed/ctf/tab_a12.csv (just written by sim_c8b)
  ```
- Line 22: lat
  ```
  ##   - Writes: output/simulated/precomputed/ctf/tab_a12.csv (overwrite)
  ```
- Line 32: loc
  ```
  local({
  ```
- Line 38: lat
  ```
  k <- 4 / 3   # horizon-driven inflation factor (sim/real on welfare/surplus)
  ```
- Line 43: name
  ```
  if (c %in% names(d)) d[[c]] <- d[[c]] / k
  ```
- Line 45: name
  ```
  write.csv(d, tab_a12_path, row.names = FALSE)
  ```

**/replication-package/jinvass26_replication_may_19_2016/paper/format/packages.tex**

- Line 2: lat
  ```
  \usepackage[latin9]{inputenc}
  ```
- Line 17: lon
  ```
  \usepackage{longtable}
  ```
- Line 20: url
  ```
  \usepackage{breakurl}
  ```
- Line 29: url
  ```
  \usepackage{url}
  ```
- Line 55: lat
  ```
  \definecolor{darkslategray}{rgb}{0.18, 0.31, 0.31}
  ```
- Line 59: lat
  ```
  \definecolor{lightslategray}{rgb}{0.47, 0.53, 0.6}
  ```
- Line 73: son
  ```
  \definecolor{harvardcrimson}{rgb}{0.79, 0.0, 0.09}
  ```

**/replication-package/jinvass26_replication_may_19_2016/paper/format/paper_setting.tex**

- Line 12: name
  ```
  \bfseries \abstractname\vspace{-.5em}\vspace{0pt}
  ```
- Line 22: lat, name, url
  ```
  \usepackage[style=authoryear,uniquename=false, uniquelist=false, mincitenames=1, maxcitenames=10, ma
  ```
- Line 34: lat
  ```
  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%% LyX specific LaTeX commands.
  ```
- Line 47: name
  ```
  {\newref{subsec}{name = \RSsectxt}}
  ```
- Line 50: name
  ```
  {\def\RSthmtxt{theorem~}\newref{thm}{name = \RSthmtxt}}
  ```
- Line 53: name
  ```
  {\def\RSlemtxt{lemma~}\newref{lem}{name = \RSlemtxt}}
  ```

**/replication-package/jinvass26_replication_may_19_2016/paper/paper.tex**

- Line 39: lat
  ```
  \section{The Value of the Monitoring Program and Counterfactual Data Regulation\label{sec:firm_equi}
  ```
- Line 42: lat
  ```
  \section{Related Literature and Conclusion}\label{sec:related_lit_and_conclusion}
  ```

**/replication-package/jinvass26_replication_may_19_2016/paper/text/appendix/sa_add_figs.tex**

- Line 56: lon
  ```
  C_{it} & =\alpha_{t}+\theta_{m,t}m_{i}+\theta_{s,t}s_{i}+\mathbf{x}_{it}^{\prime}\mathbf{\beta_{t}}+
  ```
- Line 58: lat
  ```
  Here, $m$ is an indicator for finishing monitoring and $s$ denotes the monitoring scores. The latter
  ```

**/replication-package/jinvass26_replication_may_19_2016/paper/text/appendix/sb_firm_pricing.tex**

- Line 8: lon
  ```
  dep.\text{ }var._{i} & =\alpha+\gamma Qtr_{i}+\kappa\mathbf{1}_{post,i}+\theta\cdot Qtr_{i}\times\ma
  ```
- Line 24: son
  ```
  {\footnotesize \emph{Notes: }(a) plots the progression of monthly monitoring finish rate around the 
  ```
- Line 47: lat
  ```
  {\footnotesize \emph{Notes: }(a) This graph plots the density of monitoring scores across the three 
  ```
- Line 59: son
  ```
  {\footnotesize \emph{Notes: }The X-axis represents the estimated Poisson risk arrival rate while the
  ```
- Line 70: son
  ```
  {\footnotesize \emph{Notes: }The X-axis represents the estimated Poisson risk arrival rate while the
  ```

**/replication-package/jinvass26_replication_may_19_2016/paper/text/appendix/sc_robust.tex**

- Line 13: lon
  ```
  {\footnotesize \emph{Notes: }This graph reports the robustness fixed effect estimates of \cref{eq:mh
  ```
- Line 25: lon
  ```
  We investigate heterogeneity in the moral hazard effect across consumers with different observable c
  ```
- Line 30: lon
  ```
  (\mathbf{x}_{it},\mathbf{y}_{it})^{\prime}\mathbf{\beta} + \epsilon_{it}\label{eq:mh_reg_het}
  ```
- Line 32: second
  ```
  Here, $(1,\mathbf{x}_{i0},y_{i0})^\prime$ indicates the initial characteristics and coverage choice 
  ```
- Line 37: zip
  ```
  However, such responses to incentives do not hold across other price shifters. For instance, the coe
  ```
- Line 78: lat
  ```
  \caption{At-Fault Accident Violation Progression by Monitoring Groups \label{fig:rf-mh-viol}}
  ```

**/replication-package/jinvass26_replication_may_19_2016/paper/text/s0_thanks.tex**

- Line 2: school
  ```
  Jin: University of Toronto Rotman School of Business, corresponding author,
  ```
- Line 6: name
  ```
  An earlier draft of this paper was a chapter in our dissertations. We thank our advisors Ariel Pakes
  ```
- Line 9: son
  ```
  James Savage, Steve Tadelis, Andrew Sweeting, John Wells, Thomas Wollmann, and various seminar parti
  ```

**/replication-package/jinvass26_replication_may_19_2016/paper/text/s1_introduction.tex**

- Line 1: son
  ```
  New technologies have made it easier than ever for individuals to credibly document granular details
  ```
- Line 3: lon
  ```
  The potential benefits of behavioral data are especially compelling in the auto insurance industry. 
  ```
- Line 13: lat
  ```
  These reduced-form analyses allow us to separately identify the effects of moral hazard and selectio
  ```
- Line 14: lat
  ```
  quantifying the welfare impact of the monitoring program, or of prospective data regulations, requir
  ```
- Line 17: lat
  ```
  On the demand side, we estimate a dynamic model that captures complex correlations between consumers
  ```
- Line 31: lat
  ```
  Due to the large surplus generated by the monitoring program and the firm's natural incentive to ``i
  ```
- Line 33: lat
  ```
  The paper proceeds as follows. Section 1 describes our data and provides background information on a
  ```

**/replication-package/jinvass26_replication_may_19_2016/paper/text/s2_background.tex**

- Line 5: lat
  ```
  Auto insurers in the U.S. collected \$431 billion dollars of premiums in 2024.\footnote{Source: \hre
  ```
- Line 10: lat
  ```
  Insurance prices are heavily regulated. Firms
  ```
- Line 21: degree, lat
  ```
  Pricing regulations vary by state and time, but a primary goal across the board is to limit third-de
  ```
- Line 47: zip
  ```
  \Cref{tab:sum_stat}a presents summary statistics of prices, coverage levels, and claims. The average
  ```
- Line 90: lat
  ```
  {\footnotesize \emph{Notes: }(a) plots the density of the (natural) log of monitoring score for all 
  ```
- Line 95: son
  ```
  \caption{Comparison of subsequent claim cost across monitoring groups \label{fig:rf-info-bin}}
  ```
- Line 97: second
  ```
  {\footnotesize \emph{Notes: }This is a bin-scatter plot comparing average claim counts in the second
  ```

**/replication-package/jinvass26_replication_may_19_2016/paper/text/s3_reduced_form.tex**

- Line 8: son
  ```
  We construct a balanced panel over the first three periods (18 months) consisting of all consumers t
  ```
- Line 10: lon
  ```
  C_{it}= & \alpha+\tau m_{i}+\omega\mathbf{1}_{post,t}+\theta_{mh}m_{i}\cdot\mathbf{1}_{post,t}+\math
  ```
- Line 17: lat
  ```
  Among consumers who are monitored, the actual duration of monitoring varies due to logistical discre
  ```
- Line 25: lon
  ```
  C_{it}= & \alpha+\tau m_{i}+\omega_t\mathbf{1}_{t}+\theta_{t}m_{i}\cdot\mathbf{1}_{t}+\mathbf{x}_{it
  ```
- Line 45: second
  ```
  Second, our estimates measure a treatment-on-treated effect. Although we find little observed hetero
  ```
- Line 46: city, lat, lon
  ```
  , it is possible that the majority of unmonitored drivers are less capable of altering their risk. T
  ```
- Line 50: lat
  ```
  Our analysis above suggests that monitored drivers remain safer than their counterparts even after t
  ```
- Line 53: lat, second
  ```
  Selection into monitoring also implies that the technology is effective at capturing previously unob
  ```
- Line 57: city
  ```
  \subsection{Renewal Elasticity\label{subsec:rf_demand}}
  ```
- Line 59: city
  ```
  The welfare benefit of monitoring may be limited if the firm extracts large rents during renewal per
  ```
- Line 61: lon
  ```
  \mathbf{1}^{\text{renewed}}_{it} & = \alpha M_{i} + \theta \log p^{\text{renewal}}_{it} + \theta_M M
  ```
- Line 71: city
  ```
  \Cref{fig:rf-demand} shows that both OLS and IV regressions produce similar price elasticity estimat
  ```
- Line 72: lat
  ```
  During renewal, monitored consumers who receive discounts are less price sensitive than the average 
  ```
- Line 73: lat
  ```
  However, it is unclear how the firm should react to the higher price sensitivity among monitored but
  ```
- Line 78: city
  ```
  \caption{Price Elasticity of Renewal Acceptance by Monitoring Groups \label{fig:rf-demand}}
  ```
- Line 88: lat, son
  ```
  However, in order to evaluate the welfare impact of monitoring and the trade-offs associated with da
  ```
- Line 90: degree, lat, second
  ```
  monitored drivers may purchase higher coverage in anticipation of future discounts, but they may als
  ```
- Line 91: lat
  ```
  Third, the pricing of monitoring is inherently dynamic and multidimensional; it is also subject to i
  ```
- Line 93: lat
  ```
  In Sections \ref{sec:demand_model} and \ref{sec:estimation}, we estimate a structural model of accid
  ```

**/replication-package/jinvass26_replication_may_19_2016/paper/text/s4_model.tex**

- Line 1: lon
  ```
  We model the dynamic insurance choices of a panel of consumers, beginning with their first interacti
  ```
- Line 28: lon
  ```
  Putting these pieces together, we summarize the consumer's dynamic decision problem in each period $
  ```
- Line 31: lon
  ```
  d_{t} =  \arg\max_{d}\Big\{\mathbb{E}_{Q_{t}}\Big[u(h(d|Q_t; x_t, p_t, d_{t-1})) + \delta V(d|Q_t; x
  ```
- Line 46: lat
  ```
  Our model involves several sets of parameters, each relating to a different component of drivers' ex
  ```
- Line 53: son
  ```
  Following actuarial convention, we model accident arrivals as the sum of two independent Poisson dra
  ```
- Line 60: name, son
  ```
  \text{ where } N^{\bullet}_{it} \sim \operatorname{Poisson}(\lambda^{\bullet}_{it}).
  ```
- Line 67: zip
  ```
  This distinction is conceptually important because the minimum coverage option in our focal-state pa
  ```
- Line 77: lon
  ```
  \log \lambda_{it}(m,z) & = \underbrace{\hat{\lambda}(x_{it})}_\text{public base rate} + \underbrace{
  ```
- Line 81: lon
  ```
  The accident rate for consumer $i$ in period $t$ is the sum of two components: a public signal model
  ```
- Line 85: lat
  ```
  \paragraph{Monitoring Score} If a consumer opts into monitoring, their driving is tracked for severa
  ```
- Line 87: lon
  ```
  s_{i}(m, M) = \theta^{s}_0 + {\theta}^{s}_{1} \cdot \epsilon^{\lambda}_{i} + {\theta}^{s}_{2} \cdot 
  ```
- Line 115: house, zip
  ```
  We use drivers' home zip code income in the corresponding calendar year as a proxy for $w$. This is 
  ```
- Line 134: city
  ```
  As we showed in \Cref{subsec:rf_demand}, the average price elasticity estimated within narrow window
  ```

**/replication-package/jinvass26_replication_may_19_2016/paper/text/s5_estimates.tex**

- Line 3: lon, son
  ```
  Our model estimates generate predictions that match our data well along multiple dimensions. To demo
  ```
- Line 25: lat
  ```
  Figure \ref{fig:risk_rating_selection}a shows a strong positive correlation between the first renewa
  ```
- Line 37: son
  ```
  {\footnotesize \emph{Notes: }(a) is a binned scatter plots first-renewal price (with and without mon
  ```
- Line 38: son
  ```
  (b) plots the density of the Poisson claim rate by monitoring opt-in.
  ```
- Line 48: lat, second
  ```
  Table \ref{tab:model_params_summary} presents the empirical distributions of several key economic pa
  ```
- Line 54: lat
  ```
  However, this difference is expected, as our focus is on mandatory liability insurance, whereas opti
  ```
- Line 56: lat
  ```
  Consistent with prior literature such as \textcite{Handel2013},\footnote{The average switching cost 
  ```
- Line 58: lat
  ```
  Although our model does not impose any explicit correlations among utility parameters, we detect mea
  ```
- Line 67: lon
  ```
  In a three-period model, lower switching costs make the period-0 decision easier to undo at renewals
  ```

**/replication-package/jinvass26_replication_may_19_2016/paper/text/s6_counterfactual.tex**

- Line 1: lat, son
  ```
  In this section, we develop a supply-side model which, together with our demand estimates, facilitat
  ```
- Line 10: second
  ```
  The goal of our exercise is to study how the monitoring firm can affect equilibrium outcomes by diff
  ```
- Line 16: son
  ```
  At renewal, unmonitored consumers are offered the baseline progression of renewal prices.\footnote{A
  ```
- Line 35: degree
  ```
  If $\kappa_{1d}=1.5$, their discount would instead increase by a half to 45\%, so that a larger shar
  ```
- Line 67: lat
  ```
  \paragraph{Counterfactual Scenarios and Regulations} We consider five counterfactual scenarios. ``St
  ```
- Line 68: lat
  ```
  The ``No Monitoring'' scenario removes the monitoring program altogether. As we discuss in Appendix 
  ```
- Line 70: lat
  ```
  Next, we consider counterfactual scenarios in which the monitoring firm sets profit-maximizing lever
  ```
- Line 83: lat
  ```
  Our dataset covers the customers of the monitoring firm only. To conduct market-level counterfactual
  ```
- Line 90: lat
  ```
  \subsection{Counterfactual Simulation Results}
  ```
- Line 92: lat
  ```
  For each of the five counterfactual scenarios in \Cref{tab:ctf_main}, we present results in three ca
  ```
- Line 94: lat
  ```
  All welfare and profit measures are reported in annualized, per-capita dollars, computed by scaling 
  ```
- Line 98: lat
  ```
  The pricing levers for the monitoring firm ($\boldsymbol{\kappa}$) and the competitor ($\boldsymbol{
  ```
- Line 118: lat
  ```
  Taken together, our results show that ex-post price controls---capping the surcharge for risky drive
  ```
- Line 123: lat
  ```
  (3) regulatory constraints on monitoring pricing, and (4) pre-existing market structure and the foca
  ```
- Line 133: lat
  ```
  \paragraph{Ex-post Price Regulations}
  ```
- Line 141: lat
  ```
  Table \ref{tab:ctf_robust_med} simulates a scenario in which we switch the focal competitor with the
  ```

**/replication-package/jinvass26_replication_may_19_2016/paper/text/s7_conclusion.tex**

- Line 7: lat
  ```
  First, the revelation of risk information takes time. When consumers decide whether or not to opt in
  ```
- Line 16: son
  ```
  Finally, our findings provide empirical support for a recent theoretical literature on voluntary dis
  ```

