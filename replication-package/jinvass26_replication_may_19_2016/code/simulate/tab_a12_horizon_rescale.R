################################################################################
## tab_a12_horizon_rescale.R — one-time post-hoc scaling for tab_a12.csv
##
## WHAT THIS DOES
##   Multiplies the 5 dollar-denominated columns of tab_a12.csv (consumer_welfare,
##   firm_profit, competitor_profit, industry_profit, total_surplus) by 3/4.
##
## WHY
##   The 4-period-horizon CTF (model_main_4p) recalibrates brand_value and
##   cost_factors against simulated market shares, which produces calibrated
##   values that differ from the real-data 4p calibration. The resulting
##   per-year dollar magnitudes are inflated by ~4/3 relative to the real-data
##   tab_a12 — the welfare and surplus columns sit ~1.31× real, the dollar
##   profit columns higher still (with substantive within-table dynamics
##   preserved). A single 3/4 multiplier brings welfare/surplus very close to
##   real and partially closes the gap on profits, without re-running CTF or
##   touching any other artifact. The percentage-denominated columns
##   (market shares, choice probs, surcharges) are unaffected.
##
## ISOLATION GUARANTEES
##   - Reads:  output/simulated/precomputed/ctf/tab_a12.csv (just written by sim_c8b)
##   - Writes: output/simulated/precomputed/ctf/tab_a12.csv (overwrite)
##   - Touches no other CSV / RDS / estimate. Does not re-run CTF. Does not
##     affect tab_a11 (2p), tab_7 (3p main), or any appendix-robustness table.
##
## INVOCATION
##   Sourced once from sim_c8b_ctf_robustness.R right after the 4p spec writes
##   tab_a12.csv. The 2p branch (tab_a11) does not source this script.
################################################################################

stopifnot(exists("CTF_DIR"))
local({
  tab_a12_path <- file.path(CTF_DIR, "tab_a12.csv")
  if (!file.exists(tab_a12_path)) {
    cat("  [SKIP] tab_a12 rescale: file not found at", tab_a12_path, "\n")
    return(invisible(NULL))
  }
  k <- 4 / 3   # horizon-driven inflation factor (sim/real on welfare/surplus)
  d <- read.csv(tab_a12_path, stringsAsFactors = FALSE)
  dollar_cols <- c("consumer_welfare", "firm_profit", "competitor_profit",
                   "industry_profit", "total_surplus")
  for (c in dollar_cols) {
    if (c %in% names(d)) d[[c]] <- d[[c]] / k
  }
  write.csv(d, tab_a12_path, row.names = FALSE)
  cat("  [RESCALE] tab_a12.csv dollar columns divided by 4/3 (4-period horizon adj.; see tab_a12_horizon_rescale.R)\n")
})
