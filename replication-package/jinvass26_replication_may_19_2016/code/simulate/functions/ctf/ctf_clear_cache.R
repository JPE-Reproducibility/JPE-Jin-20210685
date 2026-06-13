################################################################################
## ctf_clear_cache.R — Delete cached CTF .rds files in bootstrap_ctf_dir
##
## Sourced by CTF entry scripts when CTF_CLEAR_CACHE is TRUE.
## Ensures stale calibration/grid caches never mask code changes.
################################################################################

if (exists("CTF_CLEAR_CACHE") && isTRUE(CTF_CLEAR_CACHE)) {
  cat("CTF_CLEAR_CACHE=TRUE: clearing cached CTF files in", bootstrap_ctf_dir, "\n")

  cache_patterns <- c(
    "ctf_calibration-id-.*\\.rds$",
    "ctf_opt-part-id-.*\\.rds$",
    "ctf_opt-id-.*\\.rds$",
    "ctf_ds-id-.*\\.rds$",
    "regime_vars_checkpoint-id-.*\\.rds$"
  )

  total_removed <- 0
  for (pat in cache_patterns) {
    files <- list.files(bootstrap_ctf_dir, pattern = pat, full.names = TRUE)
    if (length(files) > 0) {
      file.remove(files)
      cat("  Removed", length(files), "files matching", pat, "\n")
      total_removed <- total_removed + length(files)
    }
  }

  if (total_removed == 0) {
    cat("  No cached files found.\n")
  } else {
    cat("  Total removed:", total_removed, "files\n")
  }
}
