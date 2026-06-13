################################################################################
## run_all.R — Master replication script
##
## From the repository root, run:
##
##   Rscript code/run_all.R
##
## This regenerates all tables (.tex) and figures (.png) from the pre-computed
## CSV and JSON files in data/. Output is written to output/exhibits/.
################################################################################

if (!file.exists("code/config.R"))
  stop("Must run from the repository root: Rscript code/run_all.R")
source("code/config.R")

################################################################################
## Extract bootstrap archives (if present and not yet extracted)
################################################################################

model_dirs <- list.dirs(MODEL_OUT_DIR, recursive = FALSE, full.names = TRUE)
for (model_dir in model_dirs) {
  results_dir <- file.path(model_dir, "results")
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

source("code/c2_get_param_tables.R")
source("code/c1_get_rf_exhibits.R")
source("code/c3_get_fitctf_exhibits.R")

################################################################################
## Compile paper.pdf
################################################################################

cat("\n--- Compiling paper.pdf ---\n")
paper_dir <- "paper"
log_dir <- file.path(paper_dir, "log")
dir.create(log_dir, showWarnings = FALSE)

tryCatch({
  old_wd <- getwd()
  setwd(paper_dir)

  run_latex <- function(label) {
    system2("pdflatex",
      args = c("-interaction=nonstopmode", "paper.tex"),
      stdout = file.path("log", paste0(label, "_stdout.log")),
      stderr = file.path("log", paste0(label, "_stderr.log")))
  }

  cat("  Pass 1: pdflatex...\n"); run_latex("pass1")
  cat("  Pass 2: biber...\n")
  system2("biber", args = "paper",
    stdout = file.path("log", "biber_stdout.log"),
    stderr = file.path("log", "biber_stderr.log"))
  cat("  Pass 3: pdflatex...\n"); run_latex("pass3")
  cat("  Pass 4: pdflatex...\n"); run_latex("pass4")

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
}, error = function(e) {
  cat("  WARNING: pdflatex not found or failed:", conditionMessage(e), "\n")
  try(setwd(old_wd), silent = TRUE)
})

cat("\nDone. Exhibits written to:", TABLES_DIR, "\n")
