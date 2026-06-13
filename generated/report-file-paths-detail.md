## Filepaths Analysis Details

**/var/folders/5q/yhcyv3z55wvg6lhgc3h22kk00000gq/T/20210685-2/replication-package/jinvass26_replication_may_19_2016/paper/text/appendix/paper_app.tex**

- Line 5, unix : \input{text/appendix/sa_add_figs.tex}
- Line 9, unix : \input{text/appendix/sb_firm_pricing.tex}
- Line 13, unix : \input{text/appendix/sc_robust.tex}

**/var/folders/5q/yhcyv3z55wvg6lhgc3h22kk00000gq/T/20210685-2/replication-package/jinvass26_replication_may_19_2016/code/c1_get_rf_exhibits.R**

- Line 2, unix : ## c1_get_rf_exhibits.R — Reduced-form exhibits from precomputed CSVs/JSONs
- Line 4, unix : ## Reads from: data/precomputed/rf/, data/estimates/regression_output/
- Line 5, unix : ## Writes to:  output/exhibits/, output/exhibits/appendix/
- Line 55, unix : ## containing the full lm() summary. We derive the CSV plot/table data from
- Line 59, windows : cat("\n--- Deriving CSVs from regression JSONs ---\n")
- Line 120, windows : cat("  [DERIVE] appendix/fig_c4.csv from fig_c4_regression.json\n")
- Line 145, windows : cat("  [DERIVE] fig_5.csv from fig_5_regression.json\n")
- Line 172, windows : cat("  [DERIVE] appendix/fig_c2.csv, fig_c3.csv from fig_c2_c3_regression.json\n")
- Line 206, windows : cat("  [DERIVE] appendix/fig_b1b.csv from fig_b1b_regression.json\n")
- Line 230, windows : cat("  [DERIVE] appendix/fig_a5_a6.csv from fig_a5_a6_regression.json\n")
- Line 237, unix : ## in data/estimates/regression_output/.
- Line 239, windows : cat("--- Done deriving CSVs ---\n\n")
- Line 399, unix : ## ---- appendix/fig_a3.png (monitoring discount persistence) ------------------
- Line 422, unix : ## ---- appendix/fig_a4.png (claim surcharge) ----------------------------------
- Line 443, unix : ## ---- appendix/fig_a5.png (informativeness - participation) ------------------
- Line 474, unix : ## ---- appendix/fig_a6.png (informativeness - score) --------------------------
- Line 495, unix : ## ---- appendix/fig_b1a.png (monitoring intro event study) --------------------
- Line 525, unix : ## ---- appendix/fig_b1b.png (RD effect) --------------------------------------
- Line 552, unix : ## ---- appendix/fig_b2a.png (score density by regime) -------------------------
- Line 572, unix : ## ---- appendix/fig_b2b.png (score-discount mapping) -------------------------
- Line 593, unix : ## ---- appendix/fig_c1.png (MH progression, balanced) ------------------------
- Line 628, unix : ## ---- appendix/fig_c2.png (MH het across observables) -----------------------
- Line 641, windows : ylab("Moral Hazard Heterogeneity\n(Interaction Term Estimates)")
- Line 649, unix : ## ---- appendix/fig_c3.png (MH het across coverage) --------------------------
- Line 662, windows : ylab("Moral Hazard Heterogeneity\n(Interaction Term Estimates)")
- Line 670, unix : ## ---- appendix/fig_c4.png (AAF violations) -----------------------------------
- Line 701, windows : labels = c("pre-period\naggregate", "1", "2", "3")
- Line 784, windows : "\\toprule\n",
- Line 788, windows : "\\midrule\n",
- Line 813, windows : "\\bottomrule\n"
- Line 830, windows : "% --- Panel (a) ---\n",
- Line 834, windows : "\\toprule\n",
- Line 836, windows : "\\midrule\n",
- Line 839, windows : "% }\n\n"
- Line 844, windows : "% --- Panel (b) ---\n",
- Line 854, windows : "\\par\n",
- Line 992, windows : "\\small\n",
- Line 994, windows : "\\hline\n",
- Line 998, windows : "\\midrule\n",
- Line 1013, windows : "\\hline\n",
- Line 1058, unix : ## ---- appendix/tab_a1.tex (Tab A.1 + A.2: observable characteristics) --------
- Line 1080, windows : "\\hline\n",
- Line 1082, windows : "\\hline\n",
- Line 1100, windows : "\\hline\n",
- Line 1102, windows : "\\hline\n",
- Line 1104, windows : "\\hline\n",
- Line 1115, windows : "\\footnotesize\n",
- Line 1132, windows : "\\centering\n",
- Line 1135, windows : panel_a_tex, "\n\n",
- Line 1138, windows : notes_tex, "\n\n",
- Line 1171, windows : cat("\n--- Compiling paper.pdf ---\n")

**/var/folders/5q/yhcyv3z55wvg6lhgc3h22kk00000gq/T/20210685-2/replication-package/jinvass26_replication_may_19_2016/code/simulate/sim_c7_model_fit.R**

- Line 6, unix : ##   MODEL_FIT_DIR/tab_4.csv   (risk, score, pricing moments)
- Line 7, unix : ##   MODEL_FIT_DIR/tab_5.csv   (demand/choice shares)
- Line 8, unix : ##   MODEL_FIT_DIR/fig_6a.csv  (pricing by lambda, regime 3)
- Line 9, unix : ##   MODEL_FIT_DIR/fig_6b.csv  (lambda density by TM status)
- Line 11, unix : ## Sourced by: code/run_simulated.R (after sim_estimate.R)
- Line 22, unix : ## ---- Helper: format cost/risk/score tabular to match benchmark style --------
- Line 107, unix : ## ---- Helper: format demand/choice tabular to match benchmark style ----------
- Line 237, unix : ## two blocks (risk/score/pricing and demand/choice), applies formatting,
- Line 356, unix : ## fig_6a/6b/b3/b4 CSVs directly in MODEL_FIT_DIR
- Line 395, unix : 1/data_list$sampling_enum_tm_R)
- Line 397, unix : 1/data_list$sampling_enum_tm_R)
- Line 408, unix : 1/data_list$sampling_enum_choice[mask1])
- Line 410, unix : 1/data_list$sampling_enum_choice[mask1])
- Line 418, unix : 1/data_list$sampling_enum_choice[mask2])
- Line 420, unix : 1/data_list$sampling_enum_choice[mask2])
- Line 449, unix : ## ---- Build tab_5.csv (Demand/Choice shares) ---------------------------------

**/var/folders/5q/yhcyv3z55wvg6lhgc3h22kk00000gq/T/20210685-2/replication-package/jinvass26_replication_may_19_2016/code/simulate/sim_c8_ctf_run.R**

- Line 1, unix : #!/usr/bin/env Rscript
- Line 12, windows : cat("CTF REPLICATION FROM SCRATCH\n")
- Line 14, windows : cat("=============================================================\n\n")
- Line 77, windows : cat("Log file suffix:", log_file_suffix, "\n\n")
- Line 83, windows : cat("  Extraction complete.\n\n")
- Line 122, windows : cat("  Output dir:", bootstrap_ctf_dir, "\n\n")
- Line 139, windows : cat("  target_market_share:", target_market_share, "\n\n")
- Line 149, windows : cat("  Registering", num_cores, "parallel cores\n")
- Line 154, unix : source('code/simulate/functions/ctf/ctf_calibration.R')
- Line 159, windows : cat("  k0_calibrated:", k0_calibrated, ", k3_calibrated:", k3_calibrated, "\n\n")
- Line 410, unix : # Format: same as data/precomputed/ctf/tab_7.csv
- Line 439, windows : cat("RESULTS SUMMARY\n")
- Line 440, windows : cat("=============================================================\n\n")

**/var/folders/5q/yhcyv3z55wvg6lhgc3h22kk00000gq/T/20210685-2/replication-package/jinvass26_replication_may_19_2016/code/simulate/sim_c1_mh_regression.R**

- Line 4, unix : ## Mirrors codes/rf/c1_mh_regression.R from the main repo exactly.
- Line 8, unix : ##   appendix/tab_c1_coefs.csv + tab_c1_meta.csv (unbalanced panel, Table C.1)
- Line 10, unix : ##   appendix/fig_c1.csv                       (balanced progression, Figure C.1)
- Line 11, unix : ##   appendix/fig_c2.csv + fig_c3.csv          (MH heterogeneity, Figures C.2-C.3)
- Line 16, unix : ##   appendix/fig_c1_regression.json, appendix/tab_c1_regression.json
- Line 17, unix : ##   appendix/fig_c2_c3_regression.json
- Line 83, windows : cat("  [SIM] df_mh:", nrow(df_mh), "rows\n")
- Line 84, windows : cat("  [SIM] X_mat:", nrow(X_mat), "rows,", ncol(X_mat), "cols\n")
- Line 85, windows : cat("  [SIM] Y_mat:", nrow(Y_mat), "rows,", ncol(Y_mat), "cols\n")
- Line 283, windows : cat("  Saved CSV: tab_2_coefs.csv, tab_2_meta.csv\n")
- Line 287, windows : cat("  Saved CSV: appendix/tab_c1_coefs.csv, appendix/tab_c1_meta.csv\n")
- Line 316, windows : cat("  [SIM] tab_2_regression.json written\n")
- Line 342, windows : cat("  [SIM] appendix/tab_c1_regression.json written\n")
- Line 391, windows : cat("  Saved CSV: appendix/fig_c1.csv\n")
- Line 394, windows : cat("  Saved CSV: fig_4.csv\n")
- Line 426, windows : cat("  [SIM] appendix/fig_c1_regression.json written\n")
- Line 430, windows : cat("  [SIM] fig_4_regression.json written\n")
- Line 480, windows : cat("  Saved CSV: appendix/fig_c2.csv, appendix/fig_c3.csv\n")
- Line 506, windows : cat("  [SIM] appendix/fig_c2_c3_regression.json written\n")
- Line 558, windows : cat("  Saved CSV: tab_3_coefs.csv, tab_3_meta.csv\n")
- Line 575, windows : cat("  [SIM] tab_3_regression.json written\n")

**/var/folders/5q/yhcyv3z55wvg6lhgc3h22kk00000gq/T/20210685-2/replication-package/jinvass26_replication_may_19_2016/code/simulate/functions/load_sim_data.R**

- Line 132, windows : cat("[load_sim_data]", panel_name, ":", nrow(dataset), "rows,", ncol(dataset), "cols\n")

**/var/folders/5q/yhcyv3z55wvg6lhgc3h22kk00000gq/T/20210685-2/replication-package/jinvass26_replication_may_19_2016/code/simulate/sim_c8b_ctf_robustness.R**

- Line 4, unix : ## Produces tab_a11.csv (2-period) and tab_a12.csv (4-period) for Tables A11/A12.
- Line 11, windows : cat("CTF ROBUSTNESS: 2p and 4p models\n")
- Line 13, windows : cat("=============================================================\n\n")
- Line 32, windows : cat("\n--- Dispatching", spec$model_name, "as subprocess ---\n")
- Line 57, windows : cat("=============================================================\n\n")
- Line 103, windows : cat("  Extraction complete.\n\n")
- Line 131, windows : cat("  Output dir:", bootstrap_ctf_dir, "\n\n")
- Line 146, windows : cat("  N_part:", N_part, ", J:", J, ", J_oo:", J_oo, "\n\n")
- Line 158, windows : cat("  cost_factors_calibrated:", round(cost_factors_calibrated, 4), "\n\n")
- Line 371, unix : ## simulated welfare/surplus magnitudes close to real-data tab_a12. See
- Line 395, windows : cat("sim_c8b_ctf_robustness.R: spec", CTF_2P4P_SPEC, "complete\n")
- Line 397, unix : } # end if/else dispatcher

**/var/folders/5q/yhcyv3z55wvg6lhgc3h22kk00000gq/T/20210685-2/replication-package/jinvass26_replication_may_19_2016/paper/paper.tex**

- Line 1, unix : \input{format/paper_setting.tex}
- Line 9, unix : \author{Yizhou Jin and Shoshana Vasserman\input{text/s0_thanks.tex}}
- Line 18, unix : \input{text/s0_abstract.tex}
- Line 25, unix : \input{text/s1_introduction}
- Line 28, unix : \input{text/s2_background}
- Line 31, unix : \input{text/s3_reduced_form}
- Line 34, unix : \input{text/s4_model}
- Line 37, unix : \input{text/s5_estimates}
- Line 40, unix : \input{text/s6_counterfactual}
- Line 43, unix : \input{text/s7_conclusion}
- Line 49, unix : \input{text/appendix/paper_app}

**/var/folders/5q/yhcyv3z55wvg6lhgc3h22kk00000gq/T/20210685-2/replication-package/jinvass26_replication_may_19_2016/code/simulate/simulate_data/sim_generate_data_list.R**

- Line 13, unix : ## Usage: Rscript code/simulate/simulate_data/sim_generate_data_list.R
- Line 21, windows : cat("Generating synthetic data_list_IL.json from data_profile_data_list.json\n")
- Line 22, windows : cat("================================================================\n\n")
- Line 169, windows : cat("  X calibration (per-regime rc shift):\n")
- Line 185, windows : cat("  X variance calibration (per-regime):\n")
- Line 419, windows : cat("  Generating severity from fitted model params\n")
- Line 786, windows : cat("  OO price level adjustment applied\n")
- Line 789, unix : ## --- Post-processing: select median/min firms for OO/KC from full matrices ---
- Line 791, unix : # consistent with what the estimation/CTF pipeline expects.
- Line 806, unix : # Scale min-price firm's columns in full matrices to match real KC/own price ratio
- Line 821, unix : # Scale KC firm's columns so KC/own ≈ target_kc_ratio
- Line 838, unix : # This preserves marginal means/sds while adding the person-level correlation
- Line 893, unix : # Profile now stores valid-entry-only means/SDs (placeholders excluded via NA mask).
- Line 894, unix : # No variance decomposition needed — use means/SDs directly.
- Line 920, unix : # prices_oo_full: valid-entry-only means/SDs from profile
- Line 985, unix : # prices_pre_cov/firm_ind: generate from column proportions (or defaults)
- Line 1163, unix : # MH coefficients (from cost model — constant, not estimated in sev/price)
- Line 1177, unix : # Prior/bounds (from est_config)
- Line 1266, windows : cat("\n--- Structural outcome simulation ---\n")
- Line 1334, unix : # Load sev/price model params BEFORE saving (pricing R factors are zero placeholders)
- Line 1337, unix : # Save data_list with correct sev/price params so extraction pipeline loads them
- Line 1341, windows : cat("  Pre-saved data_list (with sev/price params) for extraction pipeline\n")
- Line 1359, unix : # Override nu/leps with the real-data values (extraction may have loaded them)
- Line 1413, windows : cat("  Choice distribution:\n")
- Line 1430, windows : cat("    TM choosers:", length(tm_choosers), "individuals\n")
- Line 1481, windows : "), keeping original TM mapping\n")
- Line 1484, windows : cat("--- Structural simulation complete ---\n\n")
- Line 1545, windows : cat("  JSON round-trip:", n_pass, "PASS,", n_fail, "FAIL,", n_skip, "SKIP\n")

**/var/folders/5q/yhcyv3z55wvg6lhgc3h22kk00000gq/T/20210685-2/replication-package/jinvass26_replication_may_19_2016/code/simulate/functions/ctf/ctf_equi_k_save_grid.R**

- Line 10, unix : source('code/simulate/functions/ctf/get_ctf_util_profit.R')
- Line 11, unix : source('code/simulate/functions/ctf/ctf_equi_k_find_equilibria.R')
- Line 12, unix : source('code/simulate/functions/ctf/find_cycles.R')
- Line 22, unix : for(n in 1:ceiling(N_stage/sequential_step)){
- Line 232, windows : cat("TESTING mode: accepting grid equilibria without interior check\n")
- Line 459, windows : cat("WARNING: No equilibria found at step", i-1, "- falling back to initial points\n")
- Line 554, windows : cat("TESTING mode: accepting grid equilibria without interior check\n")
- Line 759, windows : cat("  Loaded", nrow(out_pis_full), "previously computed grid points\n")
- Line 894, windows : cat("WARNING: No equilibria found at step", i-1, "- falling back to initial points\n")
- Line 1060, windows : cat("TESTING mode: accepting grid equilibria without interior check\n")

**/var/folders/5q/yhcyv3z55wvg6lhgc3h22kk00000gq/T/20210685-2/replication-package/jinvass26_replication_may_19_2016/code/simulate/sim_c2_mh_viol.R**

- Line 5, unix : ##   RF_CSV_DIR/appendix/fig_c4.csv                (Figure C4 plot data)
- Line 6, unix : ##   RF_REG_DIR/appendix/fig_c4_regression.json     (backward compat)
- Line 8, unix : ## Mirrors: codes/rf/c2_mh_viol.R from the main repo

**/var/folders/5q/yhcyv3z55wvg6lhgc3h22kk00000gq/T/20210685-2/replication-package/jinvass26_replication_may_19_2016/code/simulate/functions/ctf/ctf_equi_k_find_equilibria.R**

- Line 1, unix : source('code/functions/helper.R')
- Line 39, unix : # Same as find_equilibria but with a tolerance (tol/dollar_norm) for profitable deviations.
- Line 40, unix : # A profile is an equilibrium if no firm can gain more than tol/dollar_norm by deviating.

**/var/folders/5q/yhcyv3z55wvg6lhgc3h22kk00000gq/T/20210685-2/replication-package/jinvass26_replication_may_19_2016/code/simulate/simulate_data/sim_generate_rf_data.R**

- Line 7, unix : ## Reads: data/simulated/data_profile_rf.json
- Line 8, unix : ## Writes: data/simulated/ — 4 files:
- Line 14, unix : ## Usage: Rscript code/simulate/simulate_data/sim_generate_rf_data.R
- Line 395, windows : cat("  Panel:", N_ROWS, "rows\n")
- Line 421, unix : # ubi_factor_0/1 as monitoring duration fractions (mirrors get_mh_df.R:205-206)
- Line 494, windows : cat("  panel_viol:", n_viol, "rows\n")
- Line 510, unix : # This pre/post variation is the instrument for 2SLS.
- Line 577, unix : ##   monitored w/o discount: IV ≈ -1.28 (most elastic)
- Line 705, windows : cat("  panel_renw:", n_renw, "rows\n")
- Line 773, windows : cat("  panel_ubi_renw:", n_ubi, "rows\n")
- Line 784, windows : cat("  panel_exps:", nrow(panel_exps), "rows\n")
- Line 792, unix : # With base_date=2012-01-01 and 182d/renewal, obs window is ~2012-01 to 2014-07.
- Line 794, unix : # well within the observation window, creating balanced pre/post periods.
- Line 819, windows : cat("  UBI dates generated\n")
- Line 885, unix : # Used by c5_selection_figures.R for Fig A.5/A.6 informativeness regressions.
- Line 895, windows : sum(choice_panel$ubi_fin_ind[choice_panel$RENW_CNT==0]), "finishers at t=0\n")
- Line 919, unix : # Join choice_panel extra columns (ubi/monitoring columns not already in panel)
- Line 972, unix : # ubi_groups/quintiles, while B1a still sees the full time series with step-change.
- Line 976, windows : cat("  data_rf.csv:", nrow(data_rf), "rows,", ncol(data_rf), "cols\n")
- Line 988, windows : cat("  data_rf_rrev.csv:", nrow(data_rf_rrev), "rows,", ncol(data_rf_rrev), "cols\n")
- Line 992, windows : cat("  data_rf_viol.csv:", nrow(panel_viol), "rows\n")
- Line 996, windows : cat("  ubi_vers_dates.csv:", nrow(ubi_vers_st_dt_sum), "rows\n")
- Line 1002, windows : cat("Files: data_rf.csv, data_rf_rrev.csv, data_rf_viol.csv, ubi_vers_dates.csv\n")
- Line 1010, windows : cat("  Rscript code/simulate/simulate_data/sim_generate_data_list.R\n")

**/var/folders/5q/yhcyv3z55wvg6lhgc3h22kk00000gq/T/20210685-2/replication-package/jinvass26_replication_may_19_2016/code/c3_get_fitctf_exhibits.R**

- Line 13, unix : ## Reads from: data/precomputed/model_fit/, data/precomputed/ctf/
- Line 14, unix : ## Writes to:  output/exhibits/, output/exhibits/appendix/
- Line 86, windows : "    \\toprule\n",
- Line 90, windows : "    \\midrule\n",
- Line 92, windows : "    \\bottomrule\n",
- Line 205, windows : "    \\toprule\n",
- Line 225, windows : "    \\midrule\n",
- Line 227, windows : "    \\bottomrule\n",
- Line 250, unix : # Tab C.2 (MH-het model fit) — same tabular format as Tab 4, different caption/notes
- Line 260, windows : "% ---- First table ----\n",
- Line 268, windows : "    \\toprule\n",
- Line 272, windows : "    \\midrule\n",
- Line 274, windows : "    \\bottomrule\n",
- Line 299, windows : "    \\toprule\n",
- Line 305, windows : "    \\midrule\n",
- Line 307, windows : "    \\bottomrule\n",
- Line 507, windows : "\\toprule\n",
- Line 511, windows : "[-0.3em]\n\\cmidrule(lr){5-8}\n\\addlinespace[-3pt]\n",
- Line 520, windows : "\\toprule\n",
- Line 524, windows : "[-0.3em]\n\\cmidrule(lr){5-7}\n\\addlinespace[-3pt]\n",
- Line 574, windows : "$^{1}$~Units are given in brackets. ``$\\Delta$'' denotes changes relative to the ``No Monitoring'' benchmark; ``p.c.y.'' stands for per capita per year; ``\\$'' indicates dollar terms; and ``\\%'' indicates percentage-point terms. Pricing parameters have a step size of 1 percentage point, or 0.01.\n\n",

**/var/folders/5q/yhcyv3z55wvg6lhgc3h22kk00000gq/T/20210685-2/replication-package/jinvass26_replication_may_19_2016/code/run_simulated.R**

- Line 6, unix : ##   Rscript code/run_simulated.R
- Line 9, unix : ## files in data/simulated/. Output is written to output/exhibits_simulated/,
- Line 10, unix : ## and paper/paper_simulated.pdf is compiled.
- Line 12, unix : ## Inputs (shipped): data/simulated/data_profile_rf.json,
- Line 13, unix : ##                   data/simulated/data_profile_data_list.json
- Line 26, unix : ## Step 0: Clean estimation/CTF outputs, restore from cache if enabled
- Line 29, windows : cat("\n--- Step 0: Clean + cache restore ---\n")
- Line 67, unix : # (IMAGES_DIR == TABLES_DIR; APPENDIX_DIR is TABLES_DIR/appendix).
- Line 81, windows : cat("  Restored cached files from", length(cache_tarballs), "archives\n")
- Line 93, windows : cat("\n--- Step 1: Generate RF data ---\n")
- Line 120, windows : cat("\n--- Step 2: Generate data_list ---\n")
- Line 134, windows : cat("\n--- Step 3: RF analysis ---\n")
- Line 143, windows : cat("--- Step 3 complete ---\n")
- Line 151, windows : cat("\n--- ", label, " ---\n")
- Line 161, windows : cat("--- ", label, " complete ---\n")
- Line 171, windows : cat("\n--- Step 4: Estimation skipped (RUN_ESTIMATION = FALSE) ---\n")
- Line 184, windows : cat("\n--- Step 5: CTF skipped (RUN_CTF = FALSE) ---\n")
- Line 191, windows : cat("\n--- Step 6: Generating exhibits (simulated mode) ---\n")
- Line 194, windows : cat("  Output:   ", TABLES_DIR, "\n\n")
- Line 228, windows : cat("\n--- Compiling paper_simulated.pdf ---\n")

**/var/folders/5q/yhcyv3z55wvg6lhgc3h22kk00000gq/T/20210685-2/replication-package/jinvass26_replication_may_19_2016/code/functions/ctf_table_helpers.R**

- Line 4, unix : ## Sourced by: codes/results/c2_ctf_main.R
- Line 23, windows : resize_width = "0.9\\textwidth",
- Line 134, windows : "\\toprule\n",
- Line 151, windows : "\\toprule\n",
- Line 217, windows : "$^{1}$~Units are given in brackets. ``$\\Delta$'' denotes changes relative to the ``No Monitoring'' benchmark; ``p.c.y.'' stands for per capita per year; ``\\$'' indicates dollar terms; and ``\\%'' indicates percentage-point terms. Pricing parameters have a step size of 1 percentage point, or 0.01.\n\n",

**/var/folders/5q/yhcyv3z55wvg6lhgc3h22kk00000gq/T/20210685-2/replication-package/jinvass26_replication_may_19_2016/paper/format/paper_setting.tex**

- Line 2, unix : \input{format/packages.tex}
- Line 4, windows : \def\stoptable#1{%
- Line 18, windows : \item\relax}
- Line 57, unix : \providecommand{\exhibitpath}{../output/exhibits}
- Line 69, unix : \usepackage{cleveref}[2012/02/15]% v0.18.4;

**/var/folders/5q/yhcyv3z55wvg6lhgc3h22kk00000gq/T/20210685-2/replication-package/jinvass26_replication_may_19_2016/code/simulate/functions/ctf/ctf_calibration.R**

- Line 5, unix : source('code/simulate/functions/ctf/get_ctf_util_profit.R')
- Line 54, unix : return(c(rev_own/rev_all, rev_oo_flex/rev_all))
- Line 78, unix : lb = tmp_values - step_size/dollar_norm
- Line 79, unix : ub = tmp_values + step_size/dollar_norm
- Line 80, unix : out =  expand.grid(brand_value = seq(lb$brand_value, ub$brand_value, by = next_step_size/dollar_norm),
- Line 234, unix : c(1, 0.5),   c(1, 0.5),   c(1/8, 1/4, 1/2, 1)
- Line 246, unix : stage_cf_grid = seq(-500/dollar_norm, 500/dollar_norm, by = coarseness_list[h])
- Line 343, unix : stage_deltas_weights = c(1/4, 1/2, 1)

**/var/folders/5q/yhcyv3z55wvg6lhgc3h22kk00000gq/T/20210685-2/replication-package/jinvass26_replication_may_19_2016/paper/text/s6_counterfactual.tex**

- Line 100, unix : \inputifexists{\exhibitpath/tab_7}

**/var/folders/5q/yhcyv3z55wvg6lhgc3h22kk00000gq/T/20210685-2/replication-package/jinvass26_replication_may_19_2016/code/simulate/functions/estimation/get_fit_cp.R**

- Line 10, unix : ## Sourced by: code/results/sim_model_fit.R (or similar)
- Line 14, unix : source('code/simulate/functions/estimation/get_c_latentparams.R')
- Line 15, unix : source('code/simulate/functions/estimation/get_cp_pred.R')
- Line 28, unix : ## values (different leps/llambda scale). We instead delegate fig_b3.csv

**/var/folders/5q/yhcyv3z55wvg6lhgc3h22kk00000gq/T/20210685-2/replication-package/jinvass26_replication_may_19_2016/code/simulate/functions/ctf/ctf_clear_cache.R**

- Line 5, unix : ## Ensures stale calibration/grid caches never mask code changes.
- Line 32, windows : cat("  Total removed:", total_removed, "files\n")

**/var/folders/5q/yhcyv3z55wvg6lhgc3h22kk00000gq/T/20210685-2/replication-package/jinvass26_replication_may_19_2016/code/simulate/sim_c6b_estimate_robustness.R**

- Line 5, unix : ## sev/price params as model_main. Produces bootstrap CSVs for Tables A7/A8.
- Line 7, unix : ## Requires: sim_c6_estimate.R must have run first (sev/price params in data_list)
- Line 14, windows : cat("sim_c6b_estimate_robustness.R: Estimating 2p and 4p models\n")
- Line 15, windows : cat("================================================================\n\n")
- Line 53, windows : cat("  No prior output found — using init = 0.1\n")
- Line 85, unix : ## ---- Load data_list (already has sev/price params from sim_c6) -------------
- Line 149, windows : cat("================================================================\n\n")
- Line 161, windows : cat("  SKIP:", stan_path, "not found\n")
- Line 176, windows : cat("Start:", format(Sys.time()), "\n\n")
- Line 196, windows : cat("\nEnd:", format(Sys.time()), "\n\n")
- Line 205, windows : cat(" ", mn, "estimation FAILED\n")
- Line 210, windows : cat("sim_c6b_estimate_robustness.R complete\n")

**/var/folders/5q/yhcyv3z55wvg6lhgc3h22kk00000gq/T/20210685-2/replication-package/jinvass26_replication_may_19_2016/code/simulate/functions/estimation/extract_model_estimates_cmdstan.R**

- Line 1, unix : source('code/functions/helper.R')
- Line 152, windows : cat("Note: est_config not found, using defaults\n")
- Line 160, unix : source('code/simulate/functions/estimation/load_model_data.R')
- Line 200, unix : source('code/simulate/functions/estimation/get_c_latentparams.R')
- Line 204, unix : source('code/simulate/functions/estimation/get_d_latentparams.R')

**/var/folders/5q/yhcyv3z55wvg6lhgc3h22kk00000gq/T/20210685-2/replication-package/jinvass26_replication_may_19_2016/code/simulate/sim_c0_sum_stat.R**

- Line 5, unix : ##   RF_CSV_DIR/tab_1_panel_a.csv          (Table 1 Panel A: summary stats)
- Line 6, unix : ##   RF_CSV_DIR/tab_1_panel_b.csv          (Table 1 Panel B: IL market structure)
- Line 7, unix : ##   RF_CSV_DIR/tab_1_meta.csv             (Table 1 metadata: N counts)
- Line 8, unix : ##   RF_CSV_DIR/appendix/tab_a1a2_summary.json  (Appendix A.1/A.2: X variables)
- Line 9, unix : ##   SIM_PROCESSED_DIR/X_mat_panel_us_prenorm.rds  (X matrix, pre-normalization)
- Line 10, unix : ##   SIM_PROCESSED_DIR/X_mat_panel_us_norm.rds     (X matrix, normalized)
- Line 11, unix : ##   SIM_PROCESSED_DIR/Y_mat_panel_us.rds          (Y matrix)
- Line 36, unix : ## Mirrors main repo codes/rf/c0_sum_stat.R lines 41-60
- Line 177, windows : cat("    Panel A: ", nrow(tab_1a), "rows\n")
- Line 180, unix : ## Copied from main repo codes/rf/c0_sum_stat.R — Panel B section
- Line 267, windows : cat("    Panel B:", nrow(tab_1b), "rows\n")
- Line 283, unix : ## ---- Appendix Table A.1/A.2: X Variable Summary ----------------------------
- Line 292, unix : ## Helper: compute panel_a/panel_b for a given data scope
- Line 319, unix : ## Panel A: binary/indicator variables
- Line 558, unix : ## Y_mat: coverage/choice variables

**/var/folders/5q/yhcyv3z55wvg6lhgc3h22kk00000gq/T/20210685-2/replication-package/jinvass26_replication_may_19_2016/code/simulate/sim_c5_selection_figures.R**

- Line 5, unix : ##   RF_CSV_DIR/appendix/fig_a5_a6.csv          — Informativeness of monitoring
- Line 6, unix : ##   RF_REG_DIR/appendix/fig_a5_a6_regression.json (backward compat)
- Line 7, unix : ##   RF_CSV_DIR/appendix/fig_b1a.csv            — Monitoring adoption event study
- Line 8, unix : ##   RF_CSV_DIR/appendix/fig_b1b.csv            — RD at monitoring introduction
- Line 9, unix : ##   RF_REG_DIR/appendix/fig_b1b_regression.json (backward compat)
- Line 11, unix : ## Logic mirrors codes/rf/c5_selection_figures.R from the main repo exactly.
- Line 38, windows : cat("--- appendix/fig_a5_a6 ---\n")
- Line 46, windows : cat("  [SKIP] Fig A.5/A.6: X_mat_panel_us_prenorm.rds not found\n")
- Line 157, windows : cat("  [GEN] appendix/fig_a5_a6.csv\n")
- Line 186, windows : cat("  [GEN] appendix/fig_a5_a6_regression.json\n")
- Line 192, windows : cat("--- appendix/fig_b1a & fig_b1b ---\n")
- Line 285, unix : ## Add trend/season and state FE
- Line 463, windows : cat("  [GEN] appendix/fig_b1b.csv\n")
- Line 496, windows : cat("  [GEN] appendix/fig_b1b_regression.json\n")
- Line 498, windows : cat("=== sim_c5_selection_figures.R done ===\n\n")

**/var/folders/5q/yhcyv3z55wvg6lhgc3h22kk00000gq/T/20210685-2/replication-package/jinvass26_replication_may_19_2016/code/simulate/functions/estimation/load_estimation_bootstrap_data.R**

- Line 12, windows : "  Run the simulation pipeline first:\n",
- Line 16, unix : # Real data mode: look in data/estimates or data/ for data_list_IL.rds
- Line 57, unix : temptmdisc = tempRtm/tempR0
- Line 109, unix : # Load sev/price params from bootstrap CSVs when needed for profit calculations.

**/var/folders/5q/yhcyv3z55wvg6lhgc3h22kk00000gq/T/20210685-2/replication-package/jinvass26_replication_may_19_2016/paper/text/s2_background.tex**

- Line 7, unix : In all states we study, liability insurance is mandatory, with the minimum required coverage ranging from \$25,000 to \$100,000.\footnote{All states that we study follow an ``at-fault'' tort system and mandate liability insurance. In practice, liability insurance is specified by three coverage limits. For example, 20/40/10 means that, in an accident, the insurer covers liability for bodily injuries up to \$40,000 overall, but no more than \$20,000 per victim; it also covers liability for property damage (cars or other infrastructure) for up to \$10,000. We quote the highest number here.}
- Line 26, unix : \includegraphics[scale=0.1]{\staticpath/fig_1a_static.png} &  & \includegraphics[scale=0.1]{\staticpath/fig_1b_static.png}\tabularnewline
- Line 28, unix : \includegraphics[scale=0.1]{\staticpath/fig_1c_static.png} &  & \includegraphics[scale=0.1]{\staticpath/fig_1d_static.png}\tabularnewline
- Line 49, unix : \inputifexists{\exhibitpath/tab_1}
- Line 85, unix : \includeifexists[scale=0.58]{\exhibitpath/fig_2a.png} &\hspace{0.05cm} \includeifexists[scale=0.58]{\exhibitpath/fig_2b.png}\tabularnewline
- Line 94, unix : \includeifexists[scale=0.58]{\exhibitpath/fig_3.png}

**/var/folders/5q/yhcyv3z55wvg6lhgc3h22kk00000gq/T/20210685-2/replication-package/jinvass26_replication_may_19_2016/code/run_all.R**

- Line 6, unix : ##   Rscript code/run_all.R
- Line 9, unix : ## CSV and JSON files in data/. Output is written to output/exhibits/.
- Line 43, windows : cat("\n--- Compiling paper.pdf ---\n")

**/var/folders/5q/yhcyv3z55wvg6lhgc3h22kk00000gq/T/20210685-2/replication-package/jinvass26_replication_may_19_2016/code/simulate/functions/data_clean/panel_renw_clean.R**

- Line 11, unix : ########################## Last Edit: YJ 8/16/17 #################################################
- Line 41, unix : # Education code → years: 1=some HS(9), 2-3=HS/vocational(12), 4-5=some college/assoc(14),
- Line 184, unix : source('code/functions/getX.R')

**/var/folders/5q/yhcyv3z55wvg6lhgc3h22kk00000gq/T/20210685-2/replication-package/jinvass26_replication_may_19_2016/code/simulate/functions/ctf/get_ctf_util_profit.R**

- Line 418, unix : ## there is no longer tm choice in second period, when j has tm in it, you simply get a different price_renw and/or price_oo_renw

**/var/folders/5q/yhcyv3z55wvg6lhgc3h22kk00000gq/T/20210685-2/replication-package/jinvass26_replication_may_19_2016/paper/text/appendix/sa_add_figs.tex**

- Line 8, unix : \includegraphics[scale=0.22]{\staticpath/appendix/fig_a1_static}
- Line 19, unix : \includegraphics[scale=0.25]{\staticpath/appendix/fig_a2_static.png}
- Line 26, unix : \inputifexists{\exhibitpath/appendix/tab_a1.tex}
- Line 30, unix : \includeifexists[scale=0.88]{\exhibitpath/appendix/fig_a3.png}
- Line 39, unix : \includeifexists[scale=0.88]{\exhibitpath/appendix/fig_a4.png}
- Line 48, unix : \includeifexists[scale=0.9]{\exhibitpath/appendix/fig_a5.png}
- Line 51, unix : \includeifexists[scale=0.9]{\exhibitpath/appendix/fig_a6.png}
- Line 64, unix : \inputifexists{\exhibitpath/appendix/tab_a3}
- Line 66, unix : \inputifexists{\exhibitpath/appendix/tab_a4}
- Line 68, unix : \inputifexists{\exhibitpath/appendix/tab_a5}
- Line 70, unix : \inputifexists{\exhibitpath/appendix/tab_a6}
- Line 72, unix : \inputifexists{\exhibitpath/appendix/tab_a7}
- Line 74, unix : \inputifexists{\exhibitpath/appendix/tab_a8}
- Line 78, unix : \inputifexists{\exhibitpath/appendix/tab_a9}
- Line 79, unix : \inputifexists{\exhibitpath/appendix/tab_a10}
- Line 80, unix : \inputifexists{\exhibitpath/appendix/tab_a11}
- Line 81, unix : \inputifexists{\exhibitpath/appendix/tab_a12}
- Line 82, unix : \inputifexists{\exhibitpath/appendix/tab_a13}
- Line 83, unix : \inputifexists{\exhibitpath/appendix/tab_a14}
- Line 84, unix : \inputifexists{\exhibitpath/appendix/tab_a15}

**/var/folders/5q/yhcyv3z55wvg6lhgc3h22kk00000gq/T/20210685-2/replication-package/jinvass26_replication_may_19_2016/paper/text/appendix/sc_robust.tex**

- Line 9, unix : \includeifexists[scale=0.5]{\exhibitpath/appendix/fig_c1.png}
- Line 18, unix : \inputifexists{\exhibitpath/appendix/tab_c1}
- Line 45, unix : \includeifexists[scale=0.6]{\exhibitpath/appendix/fig_c2.png}
- Line 48, unix : \includeifexists[scale=0.6]{\exhibitpath/appendix/fig_c3.png}
- Line 56, unix : \inputifexists{\exhibitpath/appendix/tab_c2}
- Line 57, unix : \inputifexists{\exhibitpath/appendix/tab_c3}
- Line 58, unix : \inputifexists{\exhibitpath/appendix/tab_c4}
- Line 77, unix : \includeifexists[scale=0.75]{\exhibitpath/appendix/fig_c4.png}
- Line 89, unix : \inputifexists{\exhibitpath/appendix/tab_c5}

**/var/folders/5q/yhcyv3z55wvg6lhgc3h22kk00000gq/T/20210685-2/replication-package/jinvass26_replication_may_19_2016/code/c2_get_param_tables.R**

- Line 5, unix : ## Writes to:  output/exhibits/, output/exhibits/appendix/
- Line 12, unix : ##   output/exhibits/tab_6.tex              (Main model parameter summary)
- Line 13, unix : ##   output/exhibits/appendix/tab_a3.tex    (Additional latent parameter summary)
- Line 14, unix : ##   output/exhibits/appendix/tab_a4.tex    (Remaining params + moral hazard)
- Line 15, unix : ##   output/exhibits/appendix/tab_a5.tex    (Price score hyper parameters)
- Line 16, unix : ##   output/exhibits/appendix/tab_a6.tex    (X loadings on observables)
- Line 17, unix : ##   output/exhibits/appendix/tab_a7.tex    (4-period horizon robustness)
- Line 18, unix : ##   output/exhibits/appendix/tab_a8.tex    (2-period horizon robustness)
- Line 19, unix : ##   output/exhibits/appendix/tab_c3.tex    (MH het params)
- Line 86, windows : pattern = "^bootstrap-result-id-\\d+\\.csv$",
- Line 88, windows : cat("  Found", length(gq_csv_files), "GQ CSVs\n")
- Line 104, windows : pattern = "^bootstrap-result-id-\\d+\\.csv$",
- Line 106, windows : cat("  Found", length(orig_csv_files), "original bootstrap CSVs\n")
- Line 550, windows : cat("    WARNING: MH params identical across bootstraps — SEs will be 0\n")
- Line 554, windows : cat("    model_cost CSVs not found — MH params from main model\n")
- Line 570, windows : cat("    Found", length(sev_files), "model_sev bootstrap CSVs\n")
- Line 574, windows : cat("    model_sev CSVs not found — sev_minor_sd from main model\n")
- Line 748, unix : # R_tm/R_nb/R_renw come from model_price if available
- Line 791, windows : cat("    Found", length(price_files), "model_price bootstrap CSVs\n")
- Line 795, windows : cat("    model_price CSVs not found — R_tm/R_nb/R_renw columns will be NA\n")
- Line 1005, windows : cat("    Found", length(sev_x_files), "model_sev bootstrap CSVs for A6\n")
- Line 1009, windows : cat("    model_sev CSVs not found — sev cols will use main model values\n")
- Line 1116, windows : pattern = "^bootstrap-result-id-\\d+\\.csv$",
- Line 1118, windows : cat("    Found", length(gq_4p_files), "4p GQ CSVs\n")
- Line 1184, windows : pattern = "^bootstrap-result-id-\\d+\\.csv$",
- Line 1186, windows : cat("    Found", length(gq_2p_files), "2p GQ CSVs\n")
- Line 1283, windows : cat("    Successfully read", n_valid, "bootstrap samples\n")
- Line 1312, windows : "\\centering\n",
- Line 1315, windows : "\\toprule\n",
- Line 1319, windows : "\\midrule\n"
- Line 1335, windows : "\\bottomrule\n",

**/var/folders/5q/yhcyv3z55wvg6lhgc3h22kk00000gq/T/20210685-2/replication-package/jinvass26_replication_may_19_2016/code/simulate/sim_c6_estimate.R**

- Line 7, unix : ##   3. model_main  — joint demand/cost/MH (uses sev/price params as fixed data)
- Line 10, unix : ##   data/simulated/data_list_IL.rds (or .json)
- Line 13, unix : ## Outputs (under MODEL_OUT_DIR = output/simulated/estimates/model_output/):
- Line 14, unix : ##   model_sev/results/bootstrap-result-id-0.csv
- Line 15, unix : ##   model_price/results/bootstrap-result-id-0.csv
- Line 16, unix : ##   model_main/results/bootstrap-result-id-0.csv
- Line 25, windows : cat("sim_estimate.R: Estimating models on simulated data\n")
- Line 26, windows : cat("================================================================\n\n")
- Line 47, windows : cat("Version:", cmdstanr::cmdstan_version(), "\n\n")
- Line 77, windows : cat("  No prior output found — using init = 0.1\n")
- Line 98, windows : "  I:", sim_data_list$I, "\n\n")
- Line 157, windows : cat("1. Estimating model_sev\n")
- Line 158, windows : cat("================================================================\n\n")
- Line 232, windows : cat("  model_sev estimation FAILED\n")
- Line 240, windows : cat("2. Estimating model_price\n")
- Line 241, windows : cat("================================================================\n\n")
- Line 311, windows : cat("  model_price estimation FAILED\n")
- Line 319, windows : cat("2b. Estimating model_cost\n")
- Line 320, windows : cat("================================================================\n\n")
- Line 329, windows : cat("  SKIP:", cost_stan_path, "not found\n")
- Line 420, windows : cat("  model_cost estimated successfully\n")
- Line 422, windows : cat("  model_cost estimation FAILED\n")
- Line 431, windows : cat("3. Estimating model_main\n")
- Line 432, windows : cat("================================================================\n\n")
- Line 439, unix : # Load sev/price params from estimated CSVs into data_list
- Line 443, windows : cat("  Updated data_list with sev/price params and saved to disk\n\n")
- Line 457, unix : # Build main model stan_data from sim_data_list (sev/price params now included)
- Line 494, unix : # Pre-estimated cost/severity parameters (fixed data)
- Line 531, windows : cat("  Stan data:", length(stan_data), "of", length(stan_data_fields), "fields\n\n")
- Line 536, windows : cat("Start:", format(Sys.time()), "\n\n")
- Line 556, windows : cat("\nEnd:", format(Sys.time()), "\n\n")
- Line 566, windows : cat("  model_main estimation FAILED\n")
- Line 570, windows : cat("sim_estimate.R complete\n")

**/var/folders/5q/yhcyv3z55wvg6lhgc3h22kk00000gq/T/20210685-2/replication-package/jinvass26_replication_may_19_2016/code/simulate/sim_c7b_model_fit_mhhet.R**

- Line 5, unix : ## Produces: MODEL_FIT_DIR/tab_c2.csv, MODEL_FIT_DIR/tab_c2_block2.csv
- Line 14, windows : cat("sim_c7b_model_fit_mhhet.R: SKIP — model_main_mhhet not estimated\n")
- Line 44, windows : "  (model_main_mhhet was cache-restored without running estimation; the\n",
- Line 45, windows : "   downstream Stan-model recompilation in extract_model_estimates_cmdstan.R\n",
- Line 47, windows : "   To enable: run with USE_CACHE=FALSE, or apply the warm-start refactor\n",
- Line 60, windows : cat("  Added X_rc_clm to data_list\n")
- Line 97, unix : 1/data_list$sampling_enum_tm_R)
- Line 99, unix : 1/data_list$sampling_enum_tm_R)
- Line 110, unix : 1/data_list$sampling_enum_choice[mask1])
- Line 112, unix : 1/data_list$sampling_enum_choice[mask1])
- Line 120, unix : 1/data_list$sampling_enum_choice[mask2])
- Line 122, unix : 1/data_list$sampling_enum_choice[mask2])

**/var/folders/5q/yhcyv3z55wvg6lhgc3h22kk00000gq/T/20210685-2/replication-package/jinvass26_replication_may_19_2016/paper/text/s5_estimates.tex**

- Line 13, windows : \let\oldtable\table
- Line 14, windows : \def\table[#1]{\oldtable[H]}
- Line 15, unix : \inputifexists{\exhibitpath/tab_4}
- Line 17, unix : \inputifexists{\exhibitpath/tab_5}
- Line 30, unix : \includeifexists[scale=0.6]{\exhibitpath/fig_6a.png} &\hspace{0.05cm} \includeifexists[scale=0.5]{\exhibitpath/fig_6b.png}\tabularnewline
- Line 44, unix : \inputifexists{\exhibitpath/tab_6}

**/var/folders/5q/yhcyv3z55wvg6lhgc3h22kk00000gq/T/20210685-2/replication-package/jinvass26_replication_may_19_2016/code/simulate/functions/estimation/load_model_data.R**

- Line 101, unix : zip_inc_norm = zip_income/dollar_norm

**/var/folders/5q/yhcyv3z55wvg6lhgc3h22kk00000gq/T/20210685-2/replication-package/jinvass26_replication_may_19_2016/code/simulate/functions/data_clean/clean_choice_panel.R**

- Line 16, unix : , score_inaccurate = ifelse(!is.na(UbiScoreNbr) & ((ubi_sum_val_final_max == 0 & ubi_sum_disc_final_max == 0) ## neither val=0/tier=0 while disc=0 implies super different attrition patterns

**/var/folders/5q/yhcyv3z55wvg6lhgc3h22kk00000gq/T/20210685-2/replication-package/jinvass26_replication_may_19_2016/paper/text/appendix/sb_firm_pricing.tex**

- Line 17, unix : \includeifexists[scale=0.55]{\exhibitpath/appendix/fig_b1a.png} &\hspace{0.05cm} \includeifexists[scale=0.55]{\exhibitpath/appendix/fig_b1b.png}\tabularnewline
- Line 20, unix : \caption{Monitoring Opt-In Rate and Price/Claim Effect Around Introduction \label{fig:app_event_study}}
- Line 40, unix : \includeifexists[scale=0.6]{\exhibitpath/appendix/fig_b2a.png} &\hspace{0.05cm} \includeifexists[scale=0.6]{\exhibitpath/appendix/fig_b2b.png}\tabularnewline
- Line 54, mixed : \includeifexists[width=0.8\textwidth]{\exhibitpath/appendix/fig_b3.png}
- Line 65, mixed : \includeifexists[width=0.9\textwidth]{\exhibitpath/appendix/fig_b4.png}

**/var/folders/5q/yhcyv3z55wvg6lhgc3h22kk00000gq/T/20210685-2/replication-package/jinvass26_replication_may_19_2016/paper/text/s1_introduction.tex**

- Line 27, windows : The average consumer has modest risk aversion ($1.43\times10^{-5}$), but faces sizable switching frictions: \$333 per period for switching firms, or \$113 to opt in to monitoring.

**/var/folders/5q/yhcyv3z55wvg6lhgc3h22kk00000gq/T/20210685-2/replication-package/jinvass26_replication_may_19_2016/code/config.R**

- Line 79, unix : # TM discount bounds per scoring/pricing regime (3 regimes correspond to

**/var/folders/5q/yhcyv3z55wvg6lhgc3h22kk00000gq/T/20210685-2/replication-package/jinvass26_replication_may_19_2016/code/simulate/sim_c8d_ctf_appendix_robustness.R**

- Line 15, windows : cat("CTF APPENDIX ROBUSTNESS: Tables A.9, A.10, A.13-A.15\n")
- Line 17, windows : cat("=============================================================\n\n")
- Line 75, windows : cat("\n--- Dispatching", spec$label, "as subprocess ---\n")
- Line 100, windows : cat("=============================================================\n\n")
- Line 134, windows : cat("  Extraction complete.\n\n")
- Line 165, windows : cat("  Output dir:", bootstrap_ctf_dir, "\n\n")
- Line 180, windows : cat("  N_part:", N_part, ", J:", J, ", J_oo:", J_oo, "\n\n")
- Line 192, windows : cat("  cost_factors_calibrated:", round(cost_factors_calibrated, 4), "\n\n")
- Line 417, windows : cat("sim_c8d_ctf_appendix_robustness.R: spec", CTF_APPENDIX_SPEC, "complete\n")
- Line 419, unix : } # end if/else dispatcher

**/var/folders/5q/yhcyv3z55wvg6lhgc3h22kk00000gq/T/20210685-2/replication-package/jinvass26_replication_may_19_2016/code/simulate/functions/sim_helpers.R**

- Line 2, unix : ## sim_helpers.R — Shared helper functions for simulation/estimation scripts
- Line 13, unix : ##   load_sev_price_params()    — Load sev/price CSVs, compute derived fields, merge into data_list
- Line 17, unix : ## ---- KC/OO pricing matrix computation --------------------------------------
- Line 108, windows : cat(sprintf("%-35s %12s %12s %12s\n", "Parameter", "True", "Estimated", "Abs Diff"))
- Line 111, windows : cat(sprintf("%-35s %12.6f %12.6f %12.6f\n",
- Line 149, unix : #' @return List with $params (named list of scalars/vectors/matrices)
- Line 218, unix : ## ---- Load sev/price params from bootstrap CSVs -----------------------------
- Line 230, unix : #' @return Updated data_list with sev/price params merged in

**/var/folders/5q/yhcyv3z55wvg6lhgc3h22kk00000gq/T/20210685-2/replication-package/jinvass26_replication_may_19_2016/code/simulate/fig_b3_override.R**

- Line 5, unix : ##   Overwrites output/simulated/precomputed/model_fit/fig_b3.csv with values
- Line 24, unix : ##   - Reads:  data/estimates/model_output/model_main/results/bootstrap-result-id-0.csv
- Line 27, unix : ##   - Writes: output/simulated/precomputed/model_fit/fig_b3.csv (overwrite only)

**/var/folders/5q/yhcyv3z55wvg6lhgc3h22kk00000gq/T/20210685-2/replication-package/jinvass26_replication_may_19_2016/code/simulate/sim_c6c_estimate_cost_mhhet.R**

- Line 5, unix : ## Requires: sim_c6_estimate.R must have run first (sev/price params in data_list)
- Line 12, windows : cat("sim_c6c_estimate_cost_mhhet.R: Estimating model_cost_mhhet\n")
- Line 13, windows : cat("================================================================\n\n")
- Line 51, windows : cat("  No prior output found — using init = 0.1\n")
- Line 142, unix : # Cost model: skip demand likelihood, only estimate cost/claims
- Line 166, windows : cat("  Stan data:", length(stan_data), "fields\n\n")
- Line 191, windows : cat("Start:", format(Sys.time()), "\n\n")
- Line 211, windows : cat("\nEnd:", format(Sys.time()), "\n\n")
- Line 251, windows : cat("  model_cost_mhhet estimation FAILED\n")
- Line 259, windows : cat("Estimating model_main_mhhet\n")
- Line 260, windows : cat("================================================================\n\n")
- Line 271, windows : cat("  SKIP:", stan_path2, "not found\n")
- Line 273, windows : cat("  SKIP: model_cost_mhhet failed, cannot proceed\n")
- Line 335, windows : cat("  Stan data:", length(stan_data2), "fields\n")
- Line 336, windows : cat("  theta_mh_llambda_rc:", round(as.numeric(stan_data2$theta_mh_llambda_rc), 4), "\n\n")
- Line 348, windows : cat("Start:", format(Sys.time()), "\n\n")
- Line 368, windows : cat("\nEnd:", format(Sys.time()), "\n\n")
- Line 403, windows : cat("  model_main_mhhet estimation FAILED\n")
- Line 408, windows : cat("sim_c6c_estimate_cost_mhhet.R complete\n")

**/var/folders/5q/yhcyv3z55wvg6lhgc3h22kk00000gq/T/20210685-2/replication-package/jinvass26_replication_may_19_2016/code/simulate/tab_a12_horizon_rescale.R**

- Line 6, unix : ##   firm_profit, competitor_profit, industry_profit, total_surplus) by 3/4.
- Line 12, unix : ##   per-year dollar magnitudes are inflated by ~4/3 relative to the real-data
- Line 15, unix : ##   preserved). A single 3/4 multiplier brings welfare/surplus very close to
- Line 21, unix : ##   - Reads:  output/simulated/precomputed/ctf/tab_a12.csv (just written by sim_c8b)
- Line 22, unix : ##   - Writes: output/simulated/precomputed/ctf/tab_a12.csv (overwrite)

**/var/folders/5q/yhcyv3z55wvg6lhgc3h22kk00000gq/T/20210685-2/replication-package/jinvass26_replication_may_19_2016/code/simulate/sim_c8c_ctf_mhhet_learning.R**

- Line 12, windows : cat("CTF ROBUSTNESS: MH-het and Learning\n")
- Line 14, windows : cat("=============================================================\n\n")
- Line 33, windows : cat("\n--- Dispatching", spec$config_suffix, "as subprocess ---\n")
- Line 55, windows : cat("=============================================================\n\n")
- Line 99, windows : cat("  Added X_rc_clm to data_list\n")
- Line 118, windows : cat("  Extraction complete.\n\n")
- Line 147, windows : cat("  Output dir:", bootstrap_ctf_dir, "\n\n")
- Line 162, windows : cat("  N_part:", N_part, ", J:", J, ", J_oo:", J_oo, "\n\n")
- Line 174, windows : cat("  cost_factors_calibrated:", round(cost_factors_calibrated, 4), "\n\n")
- Line 401, windows : cat("sim_c8c_ctf_mhhet_learning.R: spec", CTF_MHHET_SPEC, "complete\n")
- Line 403, unix : } # end if/else dispatcher

**/var/folders/5q/yhcyv3z55wvg6lhgc3h22kk00000gq/T/20210685-2/replication-package/jinvass26_replication_may_19_2016/code/simulate/functions/ctf/ctf_preload.R**

- Line 38, unix : sampling_weight = 1/data_list$sampling_enum_choice[n0:n1]
- Line 99, unix : limits_oo_base = as.vector(t(do.call(cbind, replicate(J_oo/J, limits_base, simplify = FALSE))))

**/var/folders/5q/yhcyv3z55wvg6lhgc3h22kk00000gq/T/20210685-2/replication-package/jinvass26_replication_may_19_2016/code/functions/bootstrap_helpers.R**

- Line 5, unix : ## Sourced by: codes/results/c1_param_tables.R
- Line 25, unix : # Find where gq_ columns start (everything before = theta/param columns)

**/var/folders/5q/yhcyv3z55wvg6lhgc3h22kk00000gq/T/20210685-2/replication-package/jinvass26_replication_may_19_2016/code/simulate/functions/estimation/get_d_pred.R**

- Line 4, unix : ## Get individual-level aggregated/averaged risk measures

**/var/folders/5q/yhcyv3z55wvg6lhgc3h22kk00000gq/T/20210685-2/replication-package/jinvass26_replication_may_19_2016/code/simulate/sim_c3_demand_elast.R**

- Line 5, unix : ##   RF_CSV_DIR/fig_5.csv                (Figure 5 data)
- Line 6, unix : ##   RF_REG_DIR/fig_5_regression.json    (backward-compatible JSON)
- Line 7, unix : ##   IMAGES_DIR/fig_5.png                (Figure 5: price elasticity by monitoring)
- Line 9, unix : ## Follows the exact pipeline from codes/rf/c3_demand_elast.R in the main repo.
- Line 120, windows : cat("  Regression sample:", nrow(regdata_new), "obs\n")
- Line 203, windows : cat("  [GEN] fig_5_regression.json\n")
- Line 232, windows : cat("=== sim_c3_demand_elast.R done ===\n\n")

**/var/folders/5q/yhcyv3z55wvg6lhgc3h22kk00000gq/T/20210685-2/replication-package/jinvass26_replication_may_19_2016/code/simulate/functions/estimation/get_c_latentparams.R**

- Line 51, unix : source('code/simulate/functions/estimation/load_model_data.R')

**/var/folders/5q/yhcyv3z55wvg6lhgc3h22kk00000gq/T/20210685-2/replication-package/jinvass26_replication_may_19_2016/code/simulate/sim_c4_data_figures.R**

- Line 5, unix : ##   RF_CSV_DIR/fig_2a.csv        — Monitoring score distribution
- Line 6, unix : ##   RF_CSV_DIR/fig_2b.csv        — Renewal price change density
- Line 7, unix : ##   RF_CSV_DIR/fig_3.csv         — Claims by monitoring group & score quintile
- Line 8, unix : ##   RF_CSV_DIR/appendix/fig_a3.csv  — Monitoring discount persistence
- Line 9, unix : ##   RF_CSV_DIR/appendix/fig_a4.csv  — Claim surcharge by violation points
- Line 10, unix : ##   RF_CSV_DIR/appendix/fig_b2a.csv — Score density by pricing regime
- Line 11, unix : ##   RF_CSV_DIR/appendix/fig_b2b.csv — Score-discount mapping
- Line 13, unix : ## Logic mirrors codes/rf/c4_data_figures.R from the main repo exactly.
- Line 31, windows : cat("--- fig_a3 ---\n")
- Line 76, windows : cat("--- fig_2b ---\n")
- Line 185, windows : cat("--- appendix/fig_a4 ---\n")
- Line 203, windows : cat("--- appendix/fig_b2a & fig_b2b ---\n")
- Line 274, windows : cat("--- fig_2a ---\n")
- Line 290, windows : cat("--- fig_3 ---\n")
- Line 361, windows : cat("  [SKIP] fig_3: opt-out average claim count is zero\n")
- Line 369, windows : cat("=== sim_c4_data_figures.R done ===\n\n")

**/var/folders/5q/yhcyv3z55wvg6lhgc3h22kk00000gq/T/20210685-2/replication-package/jinvass26_replication_may_19_2016/paper/text/s3_reduced_form.tex**

- Line 15, unix : \inputifexists{\exhibitpath/tab_2.tex}
- Line 30, unix : \includeifexists[scale=0.5]{\exhibitpath/fig_4.png}
- Line 51, unix : \inputifexists{\exhibitpath/tab_3.tex}
- Line 77, unix : \includeifexists[scale=0.79]{\exhibitpath/fig_5.png}

