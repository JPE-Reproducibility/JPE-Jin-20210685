################################################################################
## config.R — Configuration for the replication package
##
## All paths are relative to the repo root. No .env file needed.
################################################################################

if (!exists("USE_SIMULATED_DATA")) USE_SIMULATED_DATA <- FALSE
if (!exists("RUN_ESTIMATION"))    RUN_ESTIMATION    <- TRUE
if (!exists("RUN_CTF"))           RUN_CTF           <- TRUE
if (!exists("TESTING"))           TESTING           <- TRUE
if (!exists("CTF_COARSE_GRID"))   CTF_COARSE_GRID   <- TESTING
if (!exists("USE_CACHE"))         USE_CACHE         <- TRUE
if (!exists("FROM_SCRATCH"))     FROM_SCRATCH      <- FALSE

## ---- Constants (shared, identical in v1 and sim) ----------------------------

STATE              <- "IL"      # State identifier (focal state)
K_BLOCK_CTF        <- 4         # Index of the CTF choice block (latest TM scoring/pricing regime)
OO_FIRMS           <- c(4, 3, 35, 6, 2)  # Competitor firm IDs for outside-option pricing
LOG_FILE_SUFFIXES  <- list(     # CmdStan run identifiers for locating estimation output
  model_main      = "202508210231-1-64dd17",
  model_main_2p   = "202508210231-1-64dd17",
  model_main_4p   = "202508210231-1-64dd17"
)
NUM_RENEWAL_PERIODS    <- 3     # Number of renewal periods in the model
D_X                    <- 28    # Dimension of observable covariate vector X
N_PRICING_REGIMES      <- 3     # Number of pricing regime categories
N_MH_COVARIATES        <- 2     # Number of moral hazard covariates
SIGMA_LOGIT_LB         <- 0.025 # Lower bound on logit-scale preference heterogeneity
COVERAGE_DIVISOR   <- 500       # Coverage limits in data are in $1000 units over a year (two periods)

## ---- Mode-dependent paths ---------------------------------------------------

if (!USE_SIMULATED_DATA) {
  ## === EXACTLY v1 — do not modify ===
  RF_CSV_DIR    <- "data/precomputed/rf"
  RF_REG_DIR    <- "data/estimates/regression_output"
  MODEL_FIT_DIR <- "data/precomputed/model_fit"
  MODEL_OUT_DIR <- "data/estimates/model_output"
  CTF_DIR       <- "data/precomputed/ctf"
  STATIC_DIR    <- "data/static"
  TABLES_DIR    <- "output/exhibits"
  IMAGES_DIR    <- "output/exhibits"
  APPENDIX_DIR  <- file.path(TABLES_DIR, "appendix")
} else {
  ## === Simulated-data paths ===
  SIM_DATA_DIR      <- "data/simulated"
  SIM_CACHE_DIR     <- file.path(SIM_DATA_DIR, "cache")
  SIM_PROCESSED_DIR <- SIM_CACHE_DIR
  RF_CSV_DIR    <- "output/simulated/precomputed/rf"
  RF_REG_DIR    <- "output/simulated/estimates/regression_output"
  MODEL_FIT_DIR <- "output/simulated/precomputed/model_fit"
  MODEL_OUT_DIR <- "output/simulated/estimates/model_output"
  CTF_DIR       <- "output/simulated/precomputed/ctf"
  STATIC_DIR    <- "data/static"
  TABLES_DIR    <- "output/exhibits_simulated"
  IMAGES_DIR    <- "output/exhibits_simulated"
  APPENDIX_DIR  <- file.path(TABLES_DIR, "appendix")

  ## Sim-only constants
  CRED_SCORE_THRESHOLD   <- 400
  CRED_SCORE_PLACEHOLDER <- 500
  BOOTSTRAP_ID           <- 0
  N_CHOICE_REGIMES       <- 6

  # Moral hazard coefficients from model_cost estimation (3 TM regimes × 2 claim types)
  # Rounded to 3 decimals; used as fixed data in all demand models (not re-estimated)
  # Always sourced from real-data cost model for consistency across pipelines.
  # Simulated model_cost is estimated separately for Tab A.4 display only.
  source("code/simulate/functions/sim_helpers.R")
  .mh_csv <- file.path("data/estimates/model_output/model_cost/results/bootstrap-result-id-0.csv")
  MH_PARAMS <- round(parse_bootstrap_csv(.mh_csv)$params$theta_mh_llambda, 3)
  rm(.mh_csv)

  # Coverage limits: annual dollar amounts / dollar_norm / 2 periods per year
  COVERAGE_LIMITS_1      <- c(40000, 50000, 100000, 150000, 300000) / 1000 / 2   # 5-tier (new business)
  COVERAGE_LIMITS_2      <- c(50000, 100000, 150000, 300000) / 1000 / 2           # 4-tier (TM/renewal, drops lowest)

  # TM discount bounds per scoring/pricing regime (3 regimes correspond to
  # different TM device versions and associated discount schedules)
  TM_DISCOUNT_BOUNDS     <- list(
    list(lb = 0.7, ub = 1.0),   # Regime 1
    list(lb = 0.7, ub = 1.0),   # Regime 2
    list(lb = 0.8, ub = 1.1)    # Regime 3
  )

  ## Analysis thresholds (shared across sim_c0_sum_stat.R, sim_c5_selection_figures.R)
  SIM_AGE_YOUNG          <- 25
  SIM_AGE_ADULT          <- 21
  SIM_AGE_SENIOR         <- 60
  SIM_EDU_COLLEGE        <- 14
  SIM_EDU_POSTGRAD       <- 16
  SIM_EXPOSURE_DIVISOR   <- 6
  SIM_CLAIM_WINSOR       <- 600000   # Largest observed claim (~$500K) rounded up by $100K
  SIM_CLAIM_COUNT_CAP    <- 3
  SIM_COVERAGE_DISPLAY_DIV <- 1000
  SIM_PREMIUM_FLOOR      <- 100

  ## CTF market calibration (focal state) — sourced from data profile
  .sp <- jsonlite::fromJSON(file.path(SIM_DATA_DIR, "data_profile_data_list.json"))$estimation$sim_params
  IL_OWN_MKT_SHARE      <- .sp$il_own_mkt_share
  IL_OO_MKT_SHARES      <- unlist(.sp$il_oo_mkt_shares)
  IL_DIRECT_SHARES       <- unlist(.sp$il_direct_shares)
  rm(.sp)

  ## CTF parameters
  TM_RESOURCE_COST_BASE  <- 0.035
  LEARNING_EFFECT_MULT   <- 0.9
  FIRM_DISCOUNT_FACTOR   <- 1

  ## Estimation tuning
  EST_INIT_ALPHA         <- 1e-16
  EST_TOL_OBJ            <- 1e-8
  EST_NUM_THREADS_CAP    <- 100

}

## ---- Derived paths (shared) -------------------------------------------------

CTF_RESULTS_DIR        <- CTF_DIR
MODEL_DIR              <- file.path(MODEL_OUT_DIR, "model_main")
MODEL_2P_DIR           <- file.path(MODEL_OUT_DIR, "model_main_2p")
MODEL_4P_DIR           <- file.path(MODEL_OUT_DIR, "model_main_4p")
MODEL_COST_DIR         <- file.path(MODEL_OUT_DIR, "model_cost")
MODEL_SEV_DIR          <- file.path(MODEL_OUT_DIR, "model_sev")
MODEL_PRICE_DIR        <- file.path(MODEL_OUT_DIR, "model_price")
COST_MHHET_MODEL_DIR   <- file.path(MODEL_OUT_DIR, "model_cost_mhhet")
MODEL_TAB_DIR          <- MODEL_OUT_DIR
OUTPUT_DIR             <- "output"

## ---- Create output directories ----------------------------------------------

dir.create(TABLES_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(IMAGES_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(APPENDIX_DIR, recursive = TRUE, showWarnings = FALSE)
if (USE_SIMULATED_DATA) {
  dir.create(RF_CSV_DIR, recursive = TRUE, showWarnings = FALSE)
  dir.create(RF_REG_DIR, recursive = TRUE, showWarnings = FALSE)
  dir.create(SIM_PROCESSED_DIR, recursive = TRUE, showWarnings = FALSE)
}

cat("config.R loaded.",
    "| TABLES_DIR =", TABLES_DIR,
    "| RF_CSV_DIR =", RF_CSV_DIR, "\n")
