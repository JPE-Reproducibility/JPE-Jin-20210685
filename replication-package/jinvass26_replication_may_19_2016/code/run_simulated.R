################################################################################
## run_simulated.R — Simulated-data verification pipeline
##
## From the repository root, run:
##
##   Rscript code/run_simulated.R
##
## This generates all tables (.tex) and figures (.png) from two JSON profile
## files in data/simulated/. Output is written to output/exhibits_simulated/,
## and paper/paper_simulated.pdf is compiled.
##
## Inputs (shipped): data/simulated/data_profile_rf.json,
##                   data/simulated/data_profile_data_list.json
## Stan models:      data/estimates/model_output/model_*/
##
## Optional toggles (set before sourcing):
##   RUN_ESTIMATION <- TRUE   # estimate model_sev + model_price + model_main
##   RUN_CTF        <- TRUE   # run counterfactual simulation
##   CTF_COARSE_GRID <- TRUE  # coarse grid (~30-60 min) vs full (~1-4 hours)
################################################################################

USE_SIMULATED_DATA <- TRUE
source("code/config.R")

################################################################################
## Step 0: Clean estimation/CTF outputs, restore from cache if enabled
################################################################################

cat("\n--- Step 0: Clean + cache restore ---\n")

## Remove contents of working directories (preserve folder structure)
clean_contents <- function(dir_path, pattern = NULL) {
  if (!dir.exists(dir_path)) return(invisible(NULL))
  if (is.null(pattern)) {
    files <- list.files(dir_path, full.names = TRUE, recursive = TRUE)
  } else {
    files <- list.files(dir_path, pattern = pattern, full.names = TRUE, recursive = TRUE)
  }
  if (length(files) > 0) file.remove(files)
}

if (FROM_SCRATCH) {
  # Clean everything: data cache + all generated output
  clean_contents(SIM_CACHE_DIR)
  cat("  Cleaned data cache (FROM_SCRATCH = TRUE)\n")
}

if (!USE_CACHE || FROM_SCRATCH) {
  # Clean generated intermediates. Triggered by either flag:
  #   USE_CACHE=FALSE   -> skip cache tarball restore (below) and start cold
  #   FROM_SCRATCH=TRUE -> wipe intermediates so any user-edited script
  #                        regenerates its output; cache restore still runs
  #                        afterward if USE_CACHE=TRUE
  # Wipe every model_* subdir uniformly (bootstrap CSVs, generated .stan
  # copies, compiled binaries, .Rda warm-start checkpoints, run-log markers).
  # The shipped Stan source at data/estimates/model_output/*/model_*.stan is
  # never touched.
  for (md in list.dirs(MODEL_OUT_DIR, recursive = FALSE, full.names = TRUE)) {
    clean_contents(md)
  }
  clean_contents(RF_CSV_DIR, pattern = "\\.csv$")
  clean_contents(file.path(RF_CSV_DIR, "appendix"), pattern = "\\.csv$")
  clean_contents(MODEL_FIT_DIR, pattern = "\\.csv$")
  clean_contents(RF_REG_DIR, pattern = "\\.json$")
  clean_contents(CTF_DIR)
  # TABLES_DIR cleanup is recursive, so it also covers APPENDIX_DIR and IMAGES_DIR
  # (IMAGES_DIR == TABLES_DIR; APPENDIX_DIR is TABLES_DIR/appendix).
  clean_contents(TABLES_DIR)
  cat("  Cleaned intermediate output directories (USE_CACHE =", USE_CACHE,
      ", FROM_SCRATCH =", FROM_SCRATCH, ")\n")
}

CACHE_DIR <- "output/simulated/cached"
if (USE_CACHE && dir.exists(CACHE_DIR)) {
  cache_tarballs <- list.files(CACHE_DIR, pattern = "\\.tar\\.gz$", full.names = TRUE)
  if (length(cache_tarballs) > 0) {
    for (tarball in cache_tarballs) {
      cat("  Extracting cache:", basename(tarball), "\n")
      untar(tarball, exdir = "output/simulated")
    }
    cat("  Restored cached files from", length(cache_tarballs), "archives\n")
  } else {
    cat("  No cache archives found in", CACHE_DIR, "\n")
  }
} else {
  cat("  No cache restored (USE_CACHE =", USE_CACHE, ")\n")
}

################################################################################
## Step 1: Generate simulated RF data (if not already generated)
################################################################################

cat("\n--- Step 1: Generate RF data ---\n")

rf_data_path <- file.path(SIM_CACHE_DIR, "data_rf.csv")
if (!file.exists(rf_data_path)) {
  cat("  Generating from data_profile_rf.json...\n")
  source("code/simulate/simulate_data/sim_generate_rf_data.R")
} else {
  cat("  data_rf.csv already exists, skipping generation.\n")
}

## Verify required files exist
raw_files <- c(
  file.path(SIM_CACHE_DIR, "data_rf.csv"),
  file.path(SIM_CACHE_DIR, "data_rf_rrev.csv"),
  file.path(SIM_CACHE_DIR, "data_rf_viol.csv"),
  file.path(SIM_CACHE_DIR, "ubi_vers_dates.csv")
)
missing <- raw_files[!file.exists(raw_files)]
if (length(missing) > 0) {
  stop("Missing simulated data files:\n  ", paste(missing, collapse = "\n  "),
       "\nRun data generation scripts first, or provide pre-generated files.")
}

################################################################################
## Step 2: Generate simulated data_list (if not already generated)
################################################################################

cat("\n--- Step 2: Generate data_list ---\n")

dl_path <- file.path(SIM_CACHE_DIR, "data_list_IL.rds")
if (!file.exists(dl_path)) {
  cat("  Generating from data_profile_data_list.json...\n")
  source("code/simulate/simulate_data/sim_generate_data_list.R")
} else {
  cat("  data_list_IL.rds already exists, skipping generation.\n")
}

################################################################################
## Step 3: RF analysis (data_rf.csv -> output/simulated/ CSVs + JSONs)
################################################################################

cat("\n--- Step 3: RF analysis ---\n")

source("code/simulate/sim_c0_sum_stat.R")
source("code/simulate/sim_c1_mh_regression.R")
source("code/simulate/sim_c2_mh_viol.R")
source("code/simulate/sim_c3_demand_elast.R")
source("code/simulate/sim_c4_data_figures.R")
source("code/simulate/sim_c5_selection_figures.R")

cat("--- Step 3 complete ---\n")

################################################################################
## Step 4: Estimation (requires CmdStan)
################################################################################

## Helper: run a sim_c*.R script in a fresh R process
run_step <- function(script, label) {
  cat("\n--- ", label, " ---\n")
  preamble <- paste0(
    "USE_SIMULATED_DATA <- TRUE; ",
    "TESTING <- ", TESTING, "; ",
    "USE_CACHE <- ", USE_CACHE, "; ",
    "source('code/config.R'); "
  )
  cmd <- paste0("Rscript -e \"", preamble, "source('", script, "')\"")
  rc <- system(cmd)
  if (rc != 0) stop(label, " failed with exit code ", rc)
  cat("--- ", label, " complete ---\n")
}

if (RUN_ESTIMATION) {
  run_step("code/simulate/sim_c6_estimate.R", "Step 4a: Structural estimation")
  run_step("code/simulate/sim_c7_model_fit.R", "Step 4b: Model fit")
  run_step("code/simulate/sim_c6b_estimate_robustness.R", "Step 4c: Robustness estimation (2p/4p)")
  run_step("code/simulate/sim_c6c_estimate_cost_mhhet.R", "Step 4d: Cost MH-het estimation")
  run_step("code/simulate/sim_c7b_model_fit_mhhet.R", "Step 4e: MH-het model fit (Tab C.2)")
} else {
  cat("\n--- Step 4: Estimation skipped (RUN_ESTIMATION = FALSE) ---\n")
}

################################################################################
## Step 5: CTF simulation (requires CmdStan + estimation output)
################################################################################

if (RUN_CTF) {
  run_step("code/simulate/sim_c8_ctf_run.R", "Step 5a: CTF simulation (main)")
  run_step("code/simulate/sim_c8b_ctf_robustness.R", "Step 5b: CTF robustness (2p/4p)")
  run_step("code/simulate/sim_c8c_ctf_mhhet_learning.R", "Step 5c: CTF mhhet + learning")
  run_step("code/simulate/sim_c8d_ctf_appendix_robustness.R", "Step 5d: CTF appendix robustness (A.9-A.15)")
} else {
  cat("\n--- Step 5: CTF skipped (RUN_CTF = FALSE) ---\n")
}

################################################################################
## Step 6: Generate exhibits (shared code with real-data mode)
################################################################################

cat("\n--- Step 6: Generating exhibits (simulated mode) ---\n")
cat("  RF CSVs:  ", RF_CSV_DIR, "\n")
cat("  RF JSONs: ", RF_REG_DIR, "\n")
cat("  Output:   ", TABLES_DIR, "\n\n")

## Extract bootstrap archives (no-op for simulated unless estimation was run)
if (dir.exists(MODEL_OUT_DIR)) {
  model_dirs <- list.dirs(MODEL_OUT_DIR, recursive = FALSE, full.names = TRUE)
  for (model_dir in model_dirs) {
    results_dir <- file.path(model_dir, "results")
    if (!dir.exists(results_dir)) next
    archives <- list.files(results_dir, pattern = "\\.tar\\.gz$", full.names = TRUE)
    if (length(archives) > 0) {
      n_existing <- length(list.files(results_dir, pattern = "\\.csv$"))
      if (n_existing <= 1) {
        for (archive in archives) {
          cat("Extracting", basename(archive), "...\n")
          untar(archive, exdir = model_dir)
        }
      }
    }
  }
}

## c2: param tables — requires bootstrap CSVs (may not exist in simulated mode)
tryCatch(
  source("code/c2_get_param_tables.R"),
  error = function(e) cat("[SKIP] c2_get_param_tables:", conditionMessage(e), "\n")
)

source("code/c1_get_rf_exhibits.R")
source("code/c3_get_fitctf_exhibits.R")

################################################################################
## Step 7: Compile paper_simulated.pdf
################################################################################

cat("\n--- Compiling paper_simulated.pdf ---\n")
paper_dir <- "paper"
log_dir <- file.path(paper_dir, "log")
dir.create(log_dir, showWarnings = FALSE)

tryCatch({
  old_wd <- getwd()
  setwd(paper_dir)

  ## Override \exhibitpath to point to simulated exhibits.
  ## \providecommand in paper_setting.tex is a no-op when \exhibitpath
  ## is already defined, so this \def takes priority.
  ## Use system() instead of system2() to preserve shell quoting of TeX input.
  run_latex <- function(label) {
    cmd <- paste0(
      "pdflatex -interaction=nonstopmode -jobname=paper_simulated ",
      "'\\def\\exhibitpath{../output/exhibits_simulated}\\input{paper.tex}' ",
      "> log/sim_", label, "_stdout.log 2> log/sim_", label, "_stderr.log"
    )
    system(cmd)
  }

  cat("  Pass 1: pdflatex...\n"); run_latex("pass1")
  cat("  Pass 2: biber...\n")
  system2("biber", args = "paper_simulated",
    stdout = file.path("log", "sim_biber_stdout.log"),
    stderr = file.path("log", "sim_biber_stderr.log"))
  cat("  Pass 3: pdflatex...\n"); run_latex("pass3")
  cat("  Pass 4: pdflatex...\n"); run_latex("pass4")

  # Move build artifacts to log/
  artifacts <- list.files(".", pattern = "^paper_simulated\\.(aux|bbl|bcf|blg|log|out|run\\.xml|synctex\\.gz|toc|fls|fdb_latexmk)$",
                         full.names = TRUE)
  if (length(artifacts) > 0) file.rename(artifacts, file.path("log", basename(artifacts)))

  if (file.exists("paper_simulated.pdf")) {
    cat("  paper_simulated.pdf compiled (",
        round(file.size("paper_simulated.pdf") / 1e6, 1), "MB).",
        "Build logs in paper/log/\n")
  } else {
    cat("  WARNING: paper_simulated.pdf not produced. Check paper/log/\n")
  }

  setwd(old_wd)
}, error = function(e) {
  cat("  WARNING: pdflatex not found or failed:", conditionMessage(e), "\n")
  try(setwd(old_wd), silent = TRUE)
})

cat("\nDone. Simulated exhibits written to:", TABLES_DIR, "\n")
