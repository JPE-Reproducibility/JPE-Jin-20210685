## Potential Personal Identifiable Information (PII)

⚠️ We found the following instances of potentially personally identifying information. This may be completely legitimate but might be worth checking. *As a reminder, privacy legislation in many countries (e.g. GDPR in EU) prohibits the dissemination of personal identifiable information without prior (and documented) consent of individuals.* If indeed you want to publish such information with your replication package, you should probably have obtained IRB approval for this - please check!

**Summary:**
- Data files with PII indicators: 3
- Variables flagged in data: 25
- Code files with PII references: 52
- PII references in code: 1739

### Summary of Flagged Files

| File Type | File | Variables/References | PII Categories |
|-----------|------|----------------------|----------------|
| Data | `LICENSE.txt` | 1 | son |
| Data | `tab_5.csv` | 12 | block, loc |
| Data | `tab_c2_block2.csv` | 12 | block, loc |
| Code | `bootstrap_helpers.R` | 25 | name, lat |
| Code | `c1_get_rf_exhibits.R` | 95 | lon, son, name, second, city, coord, lat, block, loc, zip |
| Code | `c2_get_param_tables.R` | 177 | lat, name, lon, second, lname, block, loc, zip |
| Code | `c3_get_fitctf_exhibits.R` | 32 | son, lat, lon, name, block, loc, coord |
| Code | `config.R` | 17 | lat, block, loc, son |
| Code | `ctf_calibration.R` | 10 | lat, name, lname |
| Code | `ctf_clear_cache.R` | 1 | name |
| Code | `ctf_equi_k_find_equilibria.R` | 1 | lat |
| Code | `ctf_equi_k_save_grid.R` | 33 | lat, name, block, loc, lname, lon |
| Code | `ctf_preload.R` | 22 | block, loc, lat, lname, name |
| Code | `ctf_table_helpers.R` | 18 | lat, name, lon |
| Code | `extract_model_estimates_cmdstan.R` | 41 | name, block, loc, lat |
| Code | `find_cycles.R` | 12 | lat, name, lon |
| Code | `getX.R` | 9 | name, loc, zip, lat, lname |
| Code | `get_c_latentparams.R` | 10 | lname, name, lat |
| Code | `get_ctf_util_profit.R` | 57 | lat, second, zip, lon |
| Code | `get_d_latentparams.R` | 7 | lat, block, loc |
| Code | `get_d_pred.R` | 39 | lat, block, loc, zip, second |
| Code | `get_fit_cp.R` | 14 | lat, name, lname |
| Code | `get_next_rrev.R` | 7 | lat, name |
| Code | `load_estimation_bootstrap_data.R` | 22 | lat, son, zip, lname, name |
| Code | `load_model_data.R` | 21 | name, block, loc, zip |
| Code | `load_sim_data.R` | 29 | name, lname, loc, zip, son |
| Code | `packages.tex` | 7 | lat, lon, url, son |
| Code | `panel_renw_clean.R` | 33 | zip, sex, school, second, loc, location, lname, name, lat, email, social, city, community |
| Code | `paper.tex` | 2 | lat |
| Code | `paper_setting.tex` | 6 | name, lat, url |
| Code | `run_all.R` | 13 | son, name, lat |
| Code | `run_simulated.R` | 57 | lat, son, name |
| Code | `s0_thanks.tex` | 3 | school, name, son |
| Code | `s1_introduction.tex` | 7 | son, lon, lat |
| Code | `s2_background.tex` | 7 | lat, degree, zip, son, second |
| Code | `s3_reduced_form.tex` | 19 | son, lon, lat, second, city, degree |
| Code | `s4_model.tex` | 13 | lon, lat, son, name, zip, house, city |
| Code | `s5_estimates.tex` | 9 | lon, son, lat, second |
| Code | `s6_counterfactual.tex` | 16 | lat, son, second, degree |
| Code | `s7_conclusion.tex` | 2 | lat, son |
| Code | `sa_add_figs.tex` | 2 | lon, lat |
| Code | `sb_firm_pricing.tex` | 5 | lon, son, lat |
| Code | `sc_robust.tex` | 6 | lon, second, zip, lat |
| Code | `sim_c0_sum_stat.R` | 108 | son, lat, lname, name, loc, zip |
| Code | `sim_c1_mh_regression.R` | 111 | lat, son, name, lname, lon |
| Code | `sim_c2_mh_viol.R` | 25 | lat, son, lname, name, loc, zip |
| Code | `sim_c3_demand_elast.R` | 27 | city, son, lat, lname, name, second, coord |
| Code | `sim_c4_data_figures.R` | 24 | lat, name, lname, son |
| Code | `sim_c5_selection_figures.R` | 62 | son, lat, lname, name, loc, zip, lon |
| Code | `sim_c6_estimate.R` | 35 | lat, son, name, block, loc, lname |
| Code | `sim_c7_model_fit.R` | 62 | lat, lon, block, loc, name, son, second |
| Code | `sim_c8_ctf_run.R` | 59 | lat, name, son, block, loc, second, lon |
| Code | `sim_generate_data_list.R` | 158 | son, lat, name, lname, lon, block, loc, zip |
| Code | `sim_generate_rf_data.R` | 104 | lat, son, name, lname, loc, zip, city, sex |
| Code | `sim_helpers.R` | 58 | lat, son, name, lname |

*See [Appendix](report-pii-appendix.md) for detailed listing of all flagged instances.*
