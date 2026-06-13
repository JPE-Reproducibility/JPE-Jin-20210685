################################################################################
## sim_generate_data_list.R — Build synthetic data_list_IL.json
##
## Generates a synthetic Stan-compatible data_list from data_profile_data_list.json
## (aggregate statistics only — no proprietary individual-level data required).
## Output is written as JSON via jsonlite for portability.
##
## The synthetic data_list has the same dimensions and structure as the real
## data_list, enabling the estimation model to compile and run. Outcomes
## (claims, scores, choices) are placeholder zeros — they get replaced by
## c1_simulate_data.R which draws from the model at MLE parameters.
##
## Usage: Rscript code/simulate/simulate_data/sim_generate_data_list.R
################################################################################

if (!exists("SIM_DATA_DIR")) { USE_SIMULATED_DATA <- TRUE; source("code/config.R") }

set.seed(42)

cat("================================================================\n")
cat("Generating synthetic data_list_IL.json from data_profile_data_list.json\n")
cat("================================================================\n\n")

## ---- Constants needed by this script (not all are in replication config) ----

## COVERAGE_LIMITS_1, COVERAGE_LIMITS_2, MH_PARAMS are set in config.R

## ---- read_data_list helper (JSON -> R with type fixups) --------------------

read_data_list <- function(json_path) {
  if (!requireNamespace("jsonlite", quietly = TRUE))
    install.packages("jsonlite")
  dl <- jsonlite::fromJSON(json_path, simplifyVector = TRUE)
  for (nm in names(dl)) {
    v <- dl[[nm]]
    if (is.data.frame(v)) {
      dl[[nm]] <- as.matrix(v)
    } else if (is.list(v) && length(v) > 0 && all(sapply(v, is.numeric))) {
      lens <- sapply(v, length)
      if (all(lens == 1)) {
        dl[[nm]] <- unlist(v)
      } else if (length(unique(lens)) == 1) {
        if (lens[1] > length(v)) {
          dl[[nm]] <- do.call(cbind, v)
        } else {
          dl[[nm]] <- do.call(rbind, v)
        }
      }
    }
  }
  if (!is.null(dl$prices_raw) && is.null(colnames(dl$prices_raw)))
    colnames(dl$prices_raw) <- as.character(seq_len(ncol(dl$prices_raw)))
  if (!is.null(dl$prices_oo_full) && is.null(colnames(dl$prices_oo_full))) {
    oo_covs  <- 3:7
    colnames(dl$prices_oo_full) <- as.vector(outer(OO_FIRMS, oo_covs, paste, sep = "_"))
  }
  dl
}

## ---- Load data profile ------------------------------------------------------

profile_path <- file.path(SIM_DATA_DIR, "data_profile_data_list.json")
if (!file.exists(profile_path))
  stop("data_profile_data_list.json not found at ", profile_path,
       "\n  This file must be provided in the replication package under data/simulated/.")
if (!requireNamespace("jsonlite", quietly = TRUE)) install.packages("jsonlite")
prof <- jsonlite::fromJSON(profile_path, simplifyVector = TRUE)
est <- prof$estimation
sp <- est$sim_params
stopifnot("sim_params section missing from data_profile_data_list.json" = !is.null(sp))

dim <- est$dimensions
rs <- est$regime_structure
xp <- est$X_profile
ps <- est$panel_structure
pr <- est$pricing
om <- est$outcome_moments
rt <- est$rate_stats
gd <- est$geo_demo
cfg <- est$config

cat("Profile loaded. Building synthetic data_list...\n")

## ---- Load real-data structural parameters (used for TM selection + outcome simulation)
real_csv <- file.path("data/estimates/model_output/model_main",
                      "results", "bootstrap-result-id-0.csv")
if (!file.exists(real_csv)) stop("Real-data estimates not found: ", real_csv)
source("code/simulate/functions/sim_helpers.R")
rp <- parse_bootstrap_csv(real_csv)$params
leps <- rp$nu * rp$eps_sd
cat("  Loaded real-data params: eps_sd =", round(rp$eps_sd, 4),
    ", leps sd =", round(sd(leps), 4), "\n")

## ---- Dimensions (from profile) ----------------------------------------------

I <- dim$I                     # 12288
N_choice <- dim$N_choice       # 48618
N_clm <- dim$N_clm             # 44662
M <- dim$M                     # 28
N_R <- dim$N_R
N_R_nb <- dim$N_R_nb
I_tm <- dim$I_tm
I_tm_R <- dim$I_tm_R
I_tm_mh <- dim$I_tm_mh
N_sev <- dim$N_sev
N_sev_lnorm <- dim$N_sev_lnorm
N_sev_pareto_below_limit <- dim$N_sev_pareto_below_limit
N_sev_pareto_at_limit <- dim$N_sev_pareto_at_limit

dl <- list()

## ---- 1. Scalar dimensions ---------------------------------------------------

for (nm in names(dim)) dl[[nm]] <- dim[[nm]]
dl$N <- N_choice  # alias

cat("  Dimensions set: I=", I, " N_choice=", N_choice, " N_clm=", N_clm, "\n")

## ---- 2. Regime structure (exact from profile) -------------------------------

for (nm in names(rs)) dl[[nm]] <- rs[[nm]]

## ---- 3. Generate synthetic X matrix (covariance-preserving) -----------------

cat("  Generating X matrix...\n")

# Generate multivariate normal with matching covariance
X_raw <- MASS::mvrnorm(n = N_choice, mu = xp$col_means, Sigma = xp$cov_matrix)

# Resolve X column names (JSON profiles lose named-vector keys)
X_colnames <- names(xp$col_means)
if (is.null(X_colnames) || length(X_colnames) != M) X_colnames <- xp$col_names
if (is.null(X_colnames) || length(X_colnames) != M) {
  stop("X column names not found in data profile. Ensure col_names is present in data_profile_data_list.json.")
}
colnames(X_raw) <- X_colnames

# Gaussian copula: rank-transform then map through quantile grid.
# This preserves rank correlations from mvrnorm while matching each
# column's marginal distribution (binary, ordinal, zero-inflated, or continuous).
qgrid_probs <- seq(0, 1, 0.001)  # 1001 points matching extract_data_list_profile.R
if (!is.null(xp$col_quantile_grid)) {
  for (j in seq_len(M)) {
    ranks <- rank(X_raw[, j], ties.method = "average") / (N_choice + 1)
    X_raw[, j] <- approx(qgrid_probs, xp$col_quantile_grid[, j],
                          xout = ranks, rule = 2)$y
  }
} else {
  # Fallback: clip to [0,1] + binary threshold (if quantile grid missing)
  X_raw <- pmin(pmax(X_raw, 0), 1)
  for (j in seq_len(M)) {
    if (!is.null(xp$col_is_binary) && xp$col_is_binary[j]) {
      threshold <- quantile(X_raw[, j], 1 - xp$col_means[j])
      X_raw[, j] <- as.numeric(X_raw[, j] >= threshold)
    }
  }
}
colnames(X_raw) <- X_colnames

# Calibrate X so that mean(X * theta_1_llambda) matches real data per regime
if (!is.null(xp$theta_1_llambda) && !is.null(xp$Xtheta_llambda_regime_means)) {
  theta_1 <- xp$theta_1_llambda
  regime_targets <- xp$Xtheta_llambda_regime_means
  rc_idx <- which(X_colnames == "rc")
  cuts <- rs$n_choice_regime_cutoffs
  n_regimes <- min(length(regime_targets), length(cuts) - 1)

  if (length(rc_idx) == 1 && abs(theta_1[rc_idx]) > 0.1) {
    cat("  X calibration (per-regime rc shift):\n")
    for (r in seq_len(n_regimes)) {
      rn0 <- cuts[r]; rn1 <- cuts[r + 1] - 1
      if (rn1 < rn0) next
      current_mean <- mean(X_raw[rn0:rn1, ] %*% theta_1)
      gap <- regime_targets[r] - current_mean
      rc_shift <- gap / theta_1[rc_idx]
      X_raw[rn0:rn1, rc_idx] <- pmin(pmax(X_raw[rn0:rn1, rc_idx] + rc_shift, 0), 1)
      residual <- mean(X_raw[rn0:rn1, ] %*% theta_1) - regime_targets[r]
      cat("    regime", r, ": rc shift", round(rc_shift, 4),
          ", gap", round(gap, 4), "->", round(residual, 4), "\n")
    }
  }

  # Variance calibration: match Var[X*theta] per regime (Jensen's inequality matters for exp())
  if (!is.null(xp$Xtheta_llambda_regime_vars)) {
    cat("  X variance calibration (per-regime):\n")
    for (r in seq_len(n_regimes)) {
      rn0 <- cuts[r]; rn1 <- cuts[r + 1] - 1
      if (rn1 < rn0) next
      Xtheta_r <- as.numeric(X_raw[rn0:rn1, ] %*% theta_1)
      current_var <- var(Xtheta_r)
      target_var <- xp$Xtheta_llambda_regime_vars[r]
      if (current_var > 0 && target_var > 0) {
        scale <- sqrt(target_var / current_var)
        mu_r <- mean(Xtheta_r)
        # Scale rc to adjust Var[X*theta] while preserving mean
        X_raw[rn0:rn1, rc_idx] <- X_raw[rn0:rn1, rc_idx] +
          (scale - 1) * (Xtheta_r - mu_r) / theta_1[rc_idx]
        X_raw[rn0:rn1, rc_idx] <- pmin(pmax(X_raw[rn0:rn1, rc_idx], 0), 1)
        new_var <- var(as.numeric(X_raw[rn0:rn1, ] %*% theta_1))
        cat("    regime", r, ": var", round(current_var, 4), "->", round(new_var, 4),
            "(target", round(target_var, 4), ")\n")
      }
    }
  }
} else if (!is.null(xp$theta_1_llambda) && !is.null(xp$Xtheta_llambda_mean)) {
  # Fallback: full-data calibration
  theta_1 <- xp$theta_1_llambda
  target_mean <- xp$Xtheta_llambda_mean
  current_mean <- mean(X_raw %*% theta_1)
  gap <- target_mean - current_mean
  rc_idx <- which(X_colnames == "rc")
  if (length(rc_idx) == 1 && abs(theta_1[rc_idx]) > 0.1) {
    rc_shift <- gap / theta_1[rc_idx]
    X_raw[, rc_idx] <- pmin(pmax(X_raw[, rc_idx] + rc_shift, 0), 1)
    cat("  X calibration (full): rc shifted by", round(rc_shift, 4), "\n")
  }
}

dl$X <- X_raw
dl$X_choice <- X_raw  # same for choice-level
dl$X_clm <- X_raw[1:N_clm, ]  # first N_clm rows
dl$X_choice_complete <- X_raw[, 1:min(25, M)]  # first 25 columns

# X_veh: extract vehicle columns from X (preserves driver-vehicle correlations)
veh_idx <- grep("x_veh", colnames(dl$X))
if (length(veh_idx) == dim$M_veh) {
  dl$X_veh <- dl$X[, veh_idx, drop = FALSE]
} else {
  # Fallback: generate from sub-covariance (severs driver-vehicle correlations)
  dl$X_veh <- MASS::mvrnorm(n = N_choice, mu = xp$X_veh_means, Sigma = xp$X_veh_cov)
}

# X_clm_record: generate from marginals
dl$X_clm_record <- matrix(rnorm(N_choice * dim$M_clm_record,
                                  mean = rep(xp$X_clm_record_means, each = N_choice),
                                  sd = rep(pmax(xp$X_clm_record_sds, 0.01), each = N_choice)),
                            nrow = N_choice, ncol = dim$M_clm_record)

# X_renw_active: binary columns
dl$X_renw_active <- matrix(0, nrow = N_choice, ncol = dim$M_renw_active)
for (j in seq_len(dim$M_renw_active)) {
  if (xp$X_renw_active_is_binary[j]) {
    dl$X_renw_active[, j] <- rbinom(N_choice, 1, xp$X_renw_active_means[j])
  } else {
    col_sd <- if (!is.null(xp$X_renw_active_sds)) xp$X_renw_active_sds[j] else 0.1
    dl$X_renw_active[, j] <- rnorm(N_choice, xp$X_renw_active_means[j], col_sd)
  }
}

# X_mh: 2-column matrix for claim-level obs
dl$X_mh <- matrix(rnorm(N_clm * 2,
                          mean = rep(xp$X_mh_means, each = N_clm),
                          sd = rep(pmax(xp$X_mh_sds, 0.01), each = N_clm)),
                    nrow = N_clm, ncol = 2)

# Severity X matrices (subsets of X at severity obs indices)
# Scale up severity samples for better identification (28 covariates need more than 124 obs)
SEV_SCALE_FACTOR <- sp$sev_scale_factor
N_sev_lnorm <- N_sev_lnorm * SEV_SCALE_FACTOR
N_sev_pareto_below_limit <- N_sev_pareto_below_limit * SEV_SCALE_FACTOR
N_sev_pareto_at_limit <- N_sev_pareto_at_limit * SEV_SCALE_FACTOR
N_sev <- N_sev_lnorm + N_sev_pareto_below_limit + N_sev_pareto_at_limit
dl$N_sev_lnorm <- N_sev_lnorm
dl$N_sev_pareto_below_limit <- N_sev_pareto_below_limit
dl$N_sev_pareto_at_limit <- N_sev_pareto_at_limit
cat("  Severity scaled", SEV_SCALE_FACTOR, "x: N_sev_lnorm=", N_sev_lnorm,
    " N_sev_pareto_below=", N_sev_pareto_below_limit,
    " N_sev_pareto_at=", N_sev_pareto_at_limit, "\n")

sev_indices <- sort(sample(1:N_clm, N_sev, replace = TRUE))
dl$X_sev_lnorm <- X_raw[sev_indices[1:N_sev_lnorm], , drop = FALSE]
dl$X_sev_pareto_below_limit <- X_raw[sev_indices[(N_sev_lnorm + 1):(N_sev_lnorm + N_sev_pareto_below_limit)], , drop = FALSE]
dl$X_sev_pareto_at_limit <- X_raw[sev_indices[(N_sev - N_sev_pareto_at_limit + 1):N_sev], , drop = FALSE]

# X normalization bounds
dl$X_max <- xp$X_max
dl$X_min <- xp$X_min
dl$X_rc_max <- xp$X_rc_max
dl$X_rc_min <- xp$X_rc_min
dl$X_mh_mean <- xp$X_mh_mean
# Ensure matrix types (JSON profile may return lists)
as_mat <- function(x) { if (is.list(x)) do.call(cbind, x) else if (is.data.frame(x)) as.matrix(x) else x }
dl$X_mh_1 <- as_mat(cfg$X_mh_1)
dl$X_mh_2 <- as_mat(cfg$X_mh_2)
dl$X_mh_3 <- as_mat(cfg$X_mh_3)
# X_no_rc: derive from X by dropping the rc column (not stored in profile)
rc_col_tmp <- which(colnames(dl$X) == "rc")
if (length(rc_col_tmp) == 1) {
  dl$X_no_rc <- dl$X[, -rc_col_tmp, drop = FALSE]
} else {
  dl$X_no_rc <- dl$X[, -ncol(dl$X), drop = FALSE]
}
# X_sup: not stored in profile (150 MB individual-level data).
# Only ubi_rate_dvc_vers is needed downstream; generated later from distribution.

# X_rc: extract risk class from X, then adjust to match cor(X_rc, leps) from real data
rc_col <- which(colnames(dl$X) == "rc")
if (length(rc_col) == 1) {
  dl$X_rc <- dl$X[, rc_col]
} else {
  dl$X_rc <- dl$X[, M]
}
# Fix log(X_rc) correlation with leps: the price model uses log_X_rc
# Real data has cor(log(X_rc), leps) ≈ +0.034
target_cor <- 0.034
xrc_i <- dl$X_rc[1:I]
log_xrc <- log(pmax(0.01, xrc_i))
log_xrc_mean <- mean(log_xrc); log_xrc_sd <- sd(log_xrc)
log_xrc_resid <- residuals(lm(log_xrc ~ leps))
log_xrc <- log_xrc_mean + log_xrc_sd * (
  sqrt(1 - target_cor^2) * scale(log_xrc_resid)[,1] + target_cor * scale(leps)[,1]
)
xrc_i <- as.numeric(exp(log_xrc))
# Store individual-level X_rc; observation-level mapping done after N_choice_to_I is created
dl$X_rc <- c(xrc_i, rep(NA, N_choice - I))  # placeholder, filled below
cat("  X_rc-leps correlation:", round(cor(xrc_i, leps), 4), "\n")
# X_rc derived fields deferred until after N_choice_to_I mapping (see below)

## ---- 4. Index mappings (build from panel structure) -------------------------

cat("  Building index mappings...\n")

# First I obs are individuals (period 0), rest are renewals
# N_choice_to_I: maps each obs to its individual
N_choice_to_I <- integer(N_choice)
N_choice_to_I[1:I] <- 1:I  # first I obs = one per individual

# Assign renewal obs to individuals (obs I+1 to N_choice)
n_renewals <- N_choice - I  # 36330
renewal_individuals <- sample(1:I, n_renewals, replace = TRUE)
N_choice_to_I[(I + 1):N_choice] <- renewal_individuals

dl$N_choice_to_I <- as.integer(N_choice_to_I)
dl$N_choice_to_I_choice <- dl$N_choice_to_I  # same mapping for choice
dl$N_to_I <- dl$N_choice_to_I

# Now fill observation-level X_rc and derived fields using N_choice_to_I
dl$X_rc <- xrc_i[dl$N_choice_to_I]
dl$X_rc_choice       <- (dl$X_rc + 0.1) * (1/1.1)
dl$X_rc_R             <- dl$X_rc[1:N_R]
dl$X_rc_R_ordered     <- (dl$X_rc_R + 0.1) * (1/1.1)
dl$X_rc_R_tm          <- dl$X_rc[1:I_tm_R]
dl$X_rc_R_tm_ordered  <- (dl$X_rc_R_tm + 0.1) * (1/1.1)

# N_clm_to_I and N_clm_to_N: first N_clm of the N_choice obs have claim data
# The 3956 obs without claims are in regimes 5-6 (renewals)
dl$N_clm_to_I <- as.integer(N_choice_to_I[1:N_clm])
dl$N_clm_to_N <- as.integer(1:N_clm)

# N_to_N_clm: maps N_choice -> N_clm (0 if no claim obs)
dl$N_to_N_clm <- numeric(N_choice)
dl$N_to_N_clm[1:N_clm] <- 1:N_clm

# N_choice_to_N: identity for aligned obs
dl$N_choice_to_N <- as.integer(1:N_choice)
dl$N_choice_to_N_clm <- dl$N_to_N_clm

# N_R mappings (rate revision obs)
dl$N_R_to_N <- as.integer(1:N_R)
dl$N_R_to_N_clm <- dl$N_to_N_clm[1:N_R]
dl$N_R_ordered_to_N_clm <- dl$N_R_to_N_clm
dl$N_R_to_N_R <- as.integer(1:N_R)
dl$N_R_to_I_tm_R <- as.integer(pmin(1:N_R, I_tm_R))
dl$is_nb_R <- as.integer(c(rep(1L, N_R_nb), rep(0L, N_R - N_R_nb)))
dl$N_choice_to_N_R <- as.integer(pmin(1:N_choice, N_R))
dl$N_to_N_R <- dl$N_choice_to_N_R

# TM mappings — select individuals with low |leps| (matching real TM selection pattern:
# TM participants tend to be moderate-risk types with leps sd ~0.19 vs population 0.36)
leps_abs <- abs(leps)
tm_individuals <- sort(order(leps_abs)[1:I_tm])
dl$I_tm_to_N <- as.numeric(tm_individuals)
dl$I_tm_R_to_I <- as.integer(tm_individuals[1:I_tm_R])
dl$I_tm_R_to_N <- as.integer(tm_individuals[1:I_tm_R])
dl$I_tm_R_to_N_clm <- as.numeric(pmin(tm_individuals[1:I_tm_R], N_clm))
dl$I_tm_R_to_N_R <- as.integer(pmin(tm_individuals[1:I_tm_R], N_R))
dl$I_tm_R_ordered_to_I <- dl$I_tm_R_to_I
dl$I_tm_R_ordered_to_N_clm <- dl$I_tm_R_to_N_clm

# Severity indices
dl$N_sev_to_N <- as.integer(sev_indices)
dl$N_sev_lnorm_to_N <- as.integer(sev_indices[1:N_sev_lnorm])
dl$N_sev_pareto_below_limit_to_N <- as.integer(sev_indices[(N_sev_lnorm + 1):(N_sev_lnorm + N_sev_pareto_below_limit)])
dl$N_sev_pareto_at_limit_to_N <- as.integer(sev_indices[(N_sev - N_sev_pareto_at_limit + 1):N_sev])

# N_to_renw0: maps individual to their first obs index
dl$N_to_renw0 <- as.integer(1:I)
# I_to_renw0: maps individual to their renewal-0 obs index (same as N_to_renw0)
dl$I_to_renw0 <- as.integer(1:I)

# Old dataset indices (needed by some scripts, set to placeholder)
dl$I_old <- if (!is.null(dim$I_old)) dim$I_old else I * 5L
dl$N_old <- if (!is.null(dim$N_old)) dim$N_old else N_choice * 5L
dl$N_to_I_old <- as.integer(rep(1:I, length.out = dl$N_old))
dl$sample_ind_old <- rep(TRUE, dl$I_old)
dl$sample_pb_old <- rep(0.2, dl$I_old)
dl$i_filter_index_old <- as.integer(1:min(54200, dl$I_old))

## ---- 5. Outcome variables (placeholder — replaced by c1_simulate_data.R) ----

cat("  Setting placeholder outcomes...\n")

# Claims (will be overwritten by simulation)
dl$clm_count_N_clm <- as.integer(rbinom(N_clm, 1, om$clm_count_mean))
dl$clm_count_severe_N_clm <- as.integer(rbinom(N_clm, 1, om$clm_severe_mean))
dl$clm_count <- as.integer(c(dl$clm_count_N_clm, rep(0L, N_choice - N_clm)))
dl$clm_count_minor <- integer(N_choice)
dl$clm_count_minor[1:N_clm] <- pmax(dl$clm_count_N_clm - dl$clm_count_severe_N_clm, 0L)
dl$clm_count_minor_N_clm <- pmax(dl$clm_count_N_clm - dl$clm_count_severe_N_clm, 0)
dl$clm_count_severe <- numeric(N_choice)
dl$clm_count_severe[1:N_clm] <- dl$clm_count_severe_N_clm
dl$clm_t1 <- as.integer(rep(0L, N_choice))

# Severity — generate from fitted sev model params (if available in profile)
if (!is.null(prof$sev_params)) {
  sev_p <- prof$sev_params
  t1_pareto <- as.numeric(sev_p$theta_1_pareto_alpha_severe)
  t1_minor <- as.numeric(sev_p$theta_1_sev_minor_mean)
  cat("  Generating severity from fitted model params\n")

  # Minor (lognormal, truncated at 10K)
  # theta_0/theta_1 are in normalized scale (clm_sev / dollar_norm)
  # Generate in normalized scale, then multiply back to dollar scale
  dollar_norm <- dl$dollar_norm
  X_ln <- dl$X_sev_lnorm
  mu_ln <- as.numeric(sev_p$theta_0_sev_minor_mean + X_ln %*% t1_minor)
  dl$clm_sev_lnorm <- rlnorm(N_sev_lnorm, mu_ln, sev_p$sev_minor_sd) * dollar_norm
  dl$clm_sev_lnorm <- pmin(dl$clm_sev_lnorm, 10000)

  # Major below limit: Pareto draws, truncated at standalone per-obs coverage limits
  X_pb <- dl$X_sev_pareto_below_limit
  alpha_pb <- as.numeric(1 + plogis(sev_p$theta_0_pareto_alpha_severe + X_pb %*% t1_pareto) * 10)
  sev_cov_limits <- sample(c(40000, 50000, 100000), N_sev_pareto_below_limit, replace = TRUE, prob = c(4, 4, 2))
  dl$clm_sev_pareto_below_limit <- numeric(N_sev_pareto_below_limit)
  for (i in seq_len(N_sev_pareto_below_limit)) {
    repeat {
      draw <- sp$pareto_minimum / runif(1)^(1/alpha_pb[i])
      if (draw < sev_cov_limits[i]) break
    }
    dl$clm_sev_pareto_below_limit[i] <- draw
  }

  # Major at limit: standalone coverage limits as censored severity values
  dl$clm_sev_pareto_at_limit <- sample(c(40000, 50000, 100000), N_sev_pareto_at_limit, replace = TRUE, prob = c(4, 4, 2))

  # Overall severity (for clm_sev field)
  dl$clm_sev <- c(dl$clm_sev_lnorm, dl$clm_sev_pareto_below_limit, dl$clm_sev_pareto_at_limit)

} else {
  stop("sev_params not found in profile. Profile must include sev_params for severity generation.")
}

# clm_sev_raw and log_tm_score: populated in fixup section below (after dependent fields are set)
dl$clm_sev_raw <- numeric(N_choice)
dl$log_tm_score <- numeric(I)
dl$log_tm_score_R <- rnorm(I_tm_R, om$log_tm_score_mean, om$log_tm_score_sd)
dl$log_tm_score_R_ordered <- dl$log_tm_score_R

# Choices — per-block sampling to respect valid d_t_new ranges
# Block 1-4 (NB): no attrition (d_t_new >= 1); Blocks 2/4 (TM): 2xJs options
# Block 5-6 (renewal): 0=attrition allowed
dl$d_t_new <- integer(N_choice)
ncc <- rs$n_choice_regime_cutoffs
if (!is.null(om$d_t_new_dist_by_block)) {
  # d_t_new_dist_by_block may be a data.frame (from JSON) or list (from RDS)
  # For data.frame: row k has $values[[k]] and $counts[[k]]
  # For list: element k has $values and $counts
  dbb <- om$d_t_new_dist_by_block
  for (k in seq_along(rs$Js)) {
    n0 <- ncc[k]; n1 <- ncc[k + 1] - 1
    N_k <- n1 - n0 + 1
    if (N_k == 0) next
    if (is.data.frame(dbb)) {
      bv <- dbb$values[[k]]
      bc <- dbb$counts[[k]]
    } else {
      bv <- dbb[[k]]$values
      bc <- dbb[[k]]$counts
    }
    dl$d_t_new[n0:n1] <- as.integer(sample(bv, N_k, replace = TRUE,
                                            prob = bc / sum(bc)))
  }
} else {
  # Fallback: aggregate distribution (legacy profile without per-block)
  n_choices <- length(om$d_t_new_dist)
  dl$d_t_new <- as.integer(sample(0:(n_choices - 1), N_choice, replace = TRUE,
                                    prob = om$d_t_new_dist / sum(om$d_t_new_dist)))
}
# d_t1_new: lagged choice from panel structure (previous period's d_t_new per individual)
d_t1_n <- rs$N_by_choice_regime[5] + rs$N_by_choice_regime[6]
dl$d_t1_new <- integer(d_t1_n)
renw_start <- rs$n_choice_regime_cutoffs[5]
for (i in 1:I) {
  obs_idx <- which(dl$N_choice_to_I == i)
  if (length(obs_idx) < 2) next
  for (k in 2:length(obs_idx)) {
    renw_pos <- obs_idx[k] - renw_start + 1
    if (renw_pos >= 1 && renw_pos <= d_t1_n) {
      dl$d_t1_new[renw_pos] <- dl$d_t_new[obs_idx[k - 1]]
    }
  }
}
dl$d_chg_record <- as.integer(rep(0L, N_choice))

# Derived claim counts
clm_count_I <- numeric(I)
for (n in seq_len(N_clm)) clm_count_I[dl$N_clm_to_I[n]] <- clm_count_I[dl$N_clm_to_I[n]] + dl$clm_count_N_clm[n]
dl$clm_count_I_tm_R_ordered <- clm_count_I[dl$I_tm_R_ordered_to_I]
dl$clm_count_N_choice <- clm_count_I[dl$N_choice_to_I]

## ---- 6. Pricing matrices (from real covariance structure) --------------------

cat("  Generating pricing matrices...\n")

## Person-level risk factor for AGGREGATE prices (prices_raw, prices_oo_full, prices_paid).
## NOT used for regime-specific prices (which use mvrnorm from real covariance).
z_price_person <- as.numeric(scale(dl$X_rc))
PRICE_RHO <- sp$price_rho

# OO price covariance tuning: adjust cross-covariance scaling per firm type
# to steer CTF calibration toward original paper cost_factors (0.084, 0.025, 0.209).
# flex = firm 3 (cheapest, cost_factor[2]); passive = median firms (cost_factor[3]).
OO_FLEX_COV_SCALE  <- sp$oo_flex_cov_scale
OO_PASS_COV_SCALE  <- sp$oo_pass_cov_scale
# Post-copula multiplicative adjustment to OO price levels per firm
OO_PRICE_MULT_ADJ <- unlist(sp$oo_price_mult_adj)

N_by <- rs$N_by_choice_regime
Js <- rs$Js

## --- Helper: generate from real covariance (mvrnorm) or fallback to factor model
# Ensure covariance matrices are proper R matrices (JSON may load them as lists)
ensure_matrix <- function(x) {
  if (is.null(x)) return(NULL)
  if (is.list(x) && !is.matrix(x)) return(do.call(rbind, lapply(x, as.numeric)))
  if (is.data.frame(x)) return(as.matrix(x))
  return(x)
}
ensure_cov_list <- function(rc) {
  if (is.null(rc)) return(NULL)
  rc$own_cov <- ensure_matrix(rc$own_cov)
  rc$kc_cov <- ensure_matrix(rc$kc_cov)
  rc$own_kc_cov <- ensure_matrix(rc$own_kc_cov)
  rc$oo_agg_cov <- ensure_matrix(rc$oo_agg_cov)
  rc$own_oo_agg_cov <- ensure_matrix(rc$own_oo_agg_cov)
  rc$renw_oo_cov <- ensure_matrix(rc$renw_oo_cov)
  rc$own_renw_oo_cov <- ensure_matrix(rc$own_renw_oo_cov)
  if (!is.null(rc$oo_covs)) rc$oo_covs <- lapply(rc$oo_covs, ensure_matrix)
  if (!is.null(rc$own_oo_covs)) rc$own_oo_covs <- lapply(rc$own_oo_covs, ensure_matrix)
  # Ensure means are numeric vectors (not named lists)
  if (is.list(rc$own_means) && !is.numeric(rc$own_means))
    rc$own_means <- as.numeric(rc$own_means)
  if (is.list(rc$oo_means))
    rc$oo_means <- lapply(rc$oo_means, function(x) if (is.list(x)) as.numeric(x) else x)
  if (is.list(rc$kc_means) && !is.numeric(rc$kc_means))
    rc$kc_means <- as.numeric(rc$kc_means)
  if (is.list(rc$oo_agg_means) && !is.numeric(rc$oo_agg_means))
    rc$oo_agg_means <- as.numeric(rc$oo_agg_means)
  if (is.list(rc$renw_oo_means) && !is.numeric(rc$renw_oo_means))
    rc$renw_oo_means <- as.numeric(rc$renw_oo_means)
  return(rc)
}

rcovs_raw <- pr$regime_price_covs  # extracted from real data
rcovs <- if (!is.null(rcovs_raw)) lapply(rcovs_raw, ensure_cov_list) else NULL

generate_regime_prices <- function(regime_key, N_regime, J,
                                   own_prefix, oo_prefix, oo_full_prefix,
                                   kc_prefix, suffix, oo_ncol_full) {
  rc <- if (!is.null(rcovs)) rcovs[[regime_key]] else NULL

  has_full_oo <- !is.null(rc) && !is.null(rc$oo_covs) && length(rc$oo_covs) > 0

  if (!is.null(rc) && !is.null(rc$own_cov) && has_full_oo) {
    ## ---- Full covariance path: own + per-firm OO (NB regimes) ----

    # Own-firm prices from real covariance
    own <- MASS::mvrnorm(N_regime, mu = rc$own_means, Sigma = rc$own_cov)
    own <- pmax(own, 0.01)

    # Per-firm OO prices from real per-firm covariance
    firms <- rc$firms
    oo_per_firm <- list()
    for (f in firms) {
      if (!is.null(rc$oo_covs[[f]])) {
        # Generate OO firm from its own covariance, correlated with own via cross-cov
        # Use conditional distribution: OO | own ~ N(mu_oo|own, Sigma_oo|own)
        cross_cov <- rc$own_oo_covs[[f]]  # J x J cross-covariance
        if (!is.null(cross_cov)) {
          # Apply firm-specific covariance scaling for CTF calibration tuning
          cov_scale <- if (f == "3") OO_FLEX_COV_SCALE else OO_PASS_COV_SCALE
          cross_cov <- cross_cov * cov_scale
          own_inv <- solve(rc$own_cov)
          cond_mean_shift <- t(cross_cov) %*% own_inv  # J x J
          cond_cov <- rc$oo_covs[[f]] - t(cross_cov) %*% own_inv %*% cross_cov
          # Ensure positive definite
          eig <- eigen(cond_cov, symmetric = TRUE)
          eig$values <- pmax(eig$values, sp$eigenvalue_floor)
          cond_cov <- eig$vectors %*% diag(eig$values) %*% t(eig$vectors)
          # Generate: oo = mu_oo + cond_mean_shift * (own - mu_own) + noise
          residual <- MASS::mvrnorm(N_regime, mu = rep(0, J), Sigma = cond_cov)
          oo_f <- matrix(rc$oo_means[[f]], nrow = N_regime, ncol = J, byrow = TRUE) +
            (own - matrix(rc$own_means, nrow = N_regime, ncol = J, byrow = TRUE)) %*%
            t(cond_mean_shift) + residual
        } else {
          oo_f <- MASS::mvrnorm(N_regime, mu = rc$oo_means[[f]], Sigma = rc$oo_covs[[f]])
        }
        oo_per_firm[[f]] <- pmax(oo_f, 0.01)
      }
    }

    # KC prices from real covariance (conditional on own)
    kc <- NULL
    if (!is.null(rc$kc_cov) && !is.null(rc$own_kc_cov)) {
      own_inv <- solve(rc$own_cov)
      cond_mean_shift <- t(rc$own_kc_cov) %*% own_inv
      cond_cov <- rc$kc_cov - t(rc$own_kc_cov) %*% own_inv %*% rc$own_kc_cov
      eig <- eigen(cond_cov, symmetric = TRUE)
      eig$values <- pmax(eig$values, 1e-8)
      cond_cov <- eig$vectors %*% diag(eig$values) %*% t(eig$vectors)
      residual <- MASS::mvrnorm(N_regime, mu = rep(0, J), Sigma = cond_cov)
      kc <- matrix(rc$kc_means, nrow = N_regime, ncol = J, byrow = TRUE) +
        (own - matrix(rc$own_means, nrow = N_regime, ncol = J, byrow = TRUE)) %*%
        t(cond_mean_shift) + residual
      kc <- pmax(kc, 0.01)
    }

    # OO = median-price firm, KC = min-price firm (matching load_estimation_bootstrap_data.R).
    # Select based on coverage-4 means (first coverage in each firm's mean vector).
    oo_agg <- NULL
    if (length(oo_per_firm) >= 2) {
      cov4_m <- sapply(oo_per_firm, function(m) mean(m[, 1]))
      med_f <- names(sort(cov4_m))[ceiling(length(cov4_m) / 2)]
      min_f <- names(which.min(cov4_m))
      oo_agg <- oo_per_firm[[med_f]]
      if (is.null(kc)) kc <- oo_per_firm[[min_f]]
    } else if (!is.null(rc$oo_agg_cov)) {
      oo_agg <- MASS::mvrnorm(N_regime, mu = rc$oo_agg_means, Sigma = rc$oo_agg_cov)
      oo_agg <- pmax(oo_agg, 0.01)
    }

    # Full OO matrix: interleave firm columns in coverage-first order
    oo_full <- NULL
    if (length(oo_per_firm) == 5) {
      # Column order: f1_c1, f2_c1, f3_c1, f4_c1, f5_c1, f1_c2, ...
      oo_full <- matrix(0, N_regime, 5 * J)
      for (j in 1:J) {
        for (fi in 1:5) {
          col_idx <- (j - 1) * 5 + fi
          oo_full[, col_idx] <- oo_per_firm[[firms[fi]]][, j]
        }
      }
    }

    return(list(own = own, oo = oo_agg, kc = if (!is.null(kc)) kc else oo_agg,
                oo_full = oo_full))
  }

  ## ---- Fallback: use own_cov if available, else factor model --------

  # If we have own_cov but not full OO structure (e.g., renewal regimes)
  if (!is.null(rc) && !is.null(rc$own_cov) && (is.null(rc$oo_covs) || length(rc$oo_covs) == 0)) {
    own <- MASS::mvrnorm(N_regime, mu = rc$own_means, Sigma = rc$own_cov)
    own <- pmax(own, 0.01)
    # OO and KC: use marginals with high within-firm correlation
    generate_factor_cov <- function(N, J, prefix, suffix, Sigma_own = NULL) {
      nm_means <- paste0(prefix, suffix, "_colmeans")
      nm_sds <- paste0(prefix, suffix, "_colsds")
      if (is.null(pr[[nm_means]])) return(NULL)
      means <- pr[[nm_means]]; sds <- pmax(pr[[nm_sds]], 0.01)
      # Build covariance with ~0.99 within-firm correlation (matching real data pattern)
      Sigma <- outer(sds, sds) * 0.99; diag(Sigma) <- sds^2
      mat <- MASS::mvrnorm(N, mu = means, Sigma = Sigma)
      pmax(mat, 0.01)
    }
    # Use stored renw_oo covariance if available (for renewal regimes)
    if (!is.null(rc$renw_oo_cov)) {
      oo_cov <- ensure_matrix(rc$renw_oo_cov)
      oo_mu <- if (is.list(rc$renw_oo_means)) as.numeric(rc$renw_oo_means) else rc$renw_oo_means
      oo <- MASS::mvrnorm(N_regime, mu = oo_mu, Sigma = oo_cov)
      oo <- pmax(oo, 0.01)
    } else {
      oo <- generate_factor_cov(N_regime, J, oo_prefix, suffix)
      if (is.null(oo)) {
        oo <- MASS::mvrnorm(N_regime, mu = rc$own_means * 0.85, Sigma = rc$own_cov * 0.7)
        oo <- pmax(oo, 0.01)
      }
    }
    kc <- oo  # KC = OO for renewal regimes (no separate KC in real data)
    oo_full_gen <- generate_factor_cov(N_regime, oo_ncol_full, oo_full_prefix, suffix)
    return(list(own = own, oo = oo, kc = kc, oo_full = oo_full_gen))
  }

  generate_factor <- function(N, ncol, prefix, suffix) {
    nm_means <- paste0(prefix, suffix, "_colmeans")
    nm_sds <- paste0(prefix, suffix, "_colsds")
    if (is.null(pr[[nm_means]])) return(NULL)
    means <- pr[[nm_means]]; sds <- pmax(pr[[nm_sds]], 0.01)
    z <- rnorm(N)
    mat <- matrix(0, N, ncol)
    for (j in seq_len(ncol)) {
      mat[, j] <- means[j] + sds[j] * (0.99 * z + sqrt(1 - 0.99^2) * rnorm(N))
    }
    pmax(mat, 0.01)
  }
  list(
    own = generate_factor(N_regime, J, own_prefix, suffix),
    oo = generate_factor(N_regime, J, oo_prefix, suffix),
    kc = generate_factor(N_regime, J, oo_prefix, suffix),
    oo_full = generate_factor(N_regime, oo_ncol_full, oo_full_prefix, suffix)
  )
}

# Generate all 6 regimes
regime_specs <- list(
  list(key="nb_1",    r=1, own="prices_nb_",    oo="prices_oo_nb_",    full="prices_oo_nb_full_",    kc="prices_kc_nb_",    s="1", fc=25),
  list(key="nb_tm_1", r=2, own="prices_nb_tm_", oo="prices_oo_nb_tm_", full="prices_oo_nb_tm_full_", kc="prices_kc_nb_tm_", s="1", fc=25),
  list(key="nb_2",    r=3, own="prices_nb_",    oo="prices_oo_nb_",    full="prices_oo_nb_full_",    kc="prices_kc_nb_",    s="2", fc=20),
  list(key="nb_tm_2", r=4, own="prices_nb_tm_", oo="prices_oo_nb_tm_", full="prices_oo_nb_tm_full_", kc="prices_kc_nb_tm_", s="2", fc=20),
  list(key="renw_1",  r=5, own="prices_renw_",  oo="prices_oo_renw_",  full="prices_oo_renw_full_",  kc="prices_kc_renw_",  s="1", fc=25),
  list(key="renw_2",  r=6, own="prices_renw_",  oo="prices_oo_renw_",  full="prices_oo_renw_full_",  kc="prices_kc_renw_",  s="2", fc=20)
)

for (spec in regime_specs) {
  res <- generate_regime_prices(spec$key, N_by[spec$r], Js[spec$r],
    spec$own, spec$oo, spec$full, spec$kc, spec$s, spec$fc)
  dl[[paste0(spec$own, spec$s)]] <- res$own
  if (!is.null(res$oo))      dl[[paste0(spec$oo, spec$s)]] <- res$oo
  if (!is.null(res$kc))      dl[[paste0(spec$kc, spec$s)]] <- res$kc
  if (!is.null(res$oo_full)) dl[[paste0(spec$full, spec$s)]] <- res$oo_full
}

# Gaussian copula post-processing for prices: rank-transform each column then
# map through the real empirical CDF (same approach as for X covariates).
# This preserves cross-column correlations while matching each column's marginal.
copula_price_fields <- c(
  "prices_nb_1", "prices_nb_2", "prices_nb_tm_1", "prices_nb_tm_2",
  "prices_renw_1", "prices_renw_2",
  "prices_oo_nb_full_1", "prices_oo_nb_full_2",
  "prices_oo_nb_tm_full_1", "prices_oo_nb_tm_full_2",
  "prices_oo_renw_full_1", "prices_oo_renw_full_2"
)
# Name mapping: dl field -> profile quantile grid key (handles oo_renw vs renw_oo naming)
qgrid_name_map <- c(
  "prices_oo_renw_full_1" = "prices_renw_oo_full_1_qgrid",
  "prices_oo_renw_full_2" = "prices_renw_oo_full_2_qgrid"
)
n_copula_price <- 0
for (pf in copula_price_fields) {
  qgrid_key <- if (pf %in% names(qgrid_name_map)) qgrid_name_map[[pf]] else paste0(pf, "_qgrid")
  if (!is.null(pr[[qgrid_key]]) && !is.null(dl[[pf]]) && is.matrix(dl[[pf]])) {
    mat <- dl[[pf]]
    qgrid <- as.matrix(pr[[qgrid_key]])  # 1001 x ncol
    ncols <- min(ncol(mat), ncol(qgrid))
    for (j in seq_len(ncols)) {
      ranks <- rank(mat[, j], ties.method = "average") / (nrow(mat) + 1)
      mat[, j] <- approx(qgrid_probs, qgrid[, j], xout = ranks, rule = 2)$y
    }
    dl[[pf]] <- pmax(mat, 0.01)
    n_copula_price <- n_copula_price + 1
  }
}
if (n_copula_price > 0) cat("  Price copula transform applied to", n_copula_price, "matrices\n")

# Post-copula OO price level adjustment: scale per-firm columns to steer calibration
if (!is.null(rcovs) && !is.null(rcovs$nb_tm_2$firms)) {
  oo_firms_adj <- rcovs$nb_tm_2$firms  # e.g. c("2","3","4","6","35")
  oo_full_fields <- c("prices_oo_nb_full_1", "prices_oo_nb_full_2",
                       "prices_oo_nb_tm_full_1", "prices_oo_nb_tm_full_2",
                       "prices_oo_renw_full_1", "prices_oo_renw_full_2")
  for (pf in oo_full_fields) {
    if (!is.null(dl[[pf]]) && is.matrix(dl[[pf]])) {
      mat <- dl[[pf]]
      nf <- length(oo_firms_adj)
      J_f <- ncol(mat) / nf
      for (fi in seq_along(oo_firms_adj)) {
        fid <- oo_firms_adj[fi]
        if (fid %in% names(OO_PRICE_MULT_ADJ)) {
          cols <- seq(fi, ncol(mat), by = nf)
          mat[, cols] <- mat[, cols] * OO_PRICE_MULT_ADJ[[fid]]
        }
      }
      dl[[pf]] <- pmax(mat, 0.01)
    }
  }
  cat("  OO price level adjustment applied\n")
}

## --- Post-processing: select median/min firms for OO/KC from full matrices ---
# Replicates load_estimation_bootstrap_data.R logic so the saved data_list is
# consistent with what the estimation/CTF pipeline expects.
if (!is.null(rcovs) && !is.null(rcovs$nb_tm_2$firms)) {
  # Firm order in full matrices matches rcovs$*$firms = c("2","3","4","6","35")
  oo_firm_ids <- rcovs$nb_tm_2$firms  # e.g. c("2","3","4","6","35")
  # Coverage-4 means from nb_tm_2 per-firm OO (first coverage in scheme 2)
  cov4_means <- sapply(oo_firm_ids, function(f) rcovs$nb_tm_2$oo_means[[f]][1])
  min_firm_idx <- which.min(cov4_means)         # KC firm (cheapest)
  # Uses median-priced OO firm: the median better represents the typical alternative a consumer evaluates vs. the cheapest competitor.
  median_firm_idx <- order(cov4_means)[ceiling(length(cov4_means) / 2)]  # OO firm

  cat("  Firm selection: OO = firm", oo_firm_ids[median_firm_idx],
      "(median), KC = firm", oo_firm_ids[min_firm_idx], "(min)\n")

  # Build column masks: 5 firms x J coverages, firm-within-coverage order
  # Scheme 1: 25 cols (5 firms x 5 coverages), scheme 2: 20 cols (5 firms x 4 coverages)
  # Scale min-price firm's columns in full matrices to match real KC/own price ratio
  target_kc_ratio <- if (!is.null(sp$kc_own_price_ratio)) sp$kc_own_price_ratio else 1.0

  for (s in c("1", "2")) {
    J_s <- if (s == "1") 5L else 4L
    ncol_full <- 5L * J_s
    med_cols <- seq(median_firm_idx, ncol_full, by = 5L)
    min_cols <- seq(min_firm_idx, ncol_full, by = 5L)

    for (base in c("nb", "nb_tm", "renw")) {
      full_key <- paste0("prices_oo_", base, "_full_", s)
      own_key <- paste0("prices_", base, "_", s)
      oo_key <- paste0("prices_oo_", base, "_", s)
      kc_key <- paste0("prices_kc_", base, "_", s)
      if (!is.null(dl[[full_key]])) {
        # Scale KC firm's columns so KC/own ≈ target_kc_ratio
        if (!is.null(dl[[own_key]])) {
          current_ratio <- mean(dl[[full_key]][, min_cols]) / mean(dl[[own_key]])
          if (current_ratio > 0) {
            scale_factor <- target_kc_ratio / current_ratio
            dl[[full_key]][, min_cols] <- dl[[full_key]][, min_cols] * scale_factor
          }
        }
        dl[[oo_key]] <- dl[[full_key]][, med_cols, drop = FALSE]
        dl[[kc_key]] <- dl[[full_key]][, min_cols, drop = FALSE]
      }
    }
  }
}

# Fix own-OO renewal price correlation: real data has cor ~0.25, sim has ~0
# Mix in own-price person factor to create cross-firm correlation
# This preserves marginal means/sds while adding the person-level correlation
target_own_oo_cor <- 0.25
for (s in c("1", "2")) {
  own_key <- paste0("prices_renw_", s)
  full_key <- paste0("prices_oo_renw_full_", s)
  if (!is.null(dl[[own_key]]) && !is.null(dl[[full_key]])) {
    own_person <- scale(rowMeans(dl[[own_key]]))[,1]  # standardized own-price person factor
    for (j in seq_len(ncol(dl[[full_key]]))) {
      col_mean <- mean(dl[[full_key]][, j])
      col_sd <- sd(dl[[full_key]][, j])
      col_resid <- scale(residuals(lm(dl[[full_key]][, j] ~ own_person)))[,1]
      dl[[full_key]][, j] <- col_mean + col_sd * (
        sqrt(1 - target_own_oo_cor^2) * col_resid + target_own_oo_cor * own_person
      )
    }
    dl[[full_key]] <- pmax(dl[[full_key]], 0.01)
  }
}

# Stan-expected field names for renewal: prices_renw_oo_* (not prices_oo_renw_*)
dl$prices_renw_oo_1 <- if (!is.null(dl$prices_oo_renw_1)) dl$prices_oo_renw_1 else dl$prices_renw_1
dl$prices_renw_oo_2 <- if (!is.null(dl$prices_oo_renw_2)) dl$prices_oo_renw_2 else dl$prices_renw_2
dl$prices_kc_renw_1 <- if (is.null(dl$prices_kc_renw_1)) dl$prices_renw_oo_1 else dl$prices_kc_renw_1
dl$prices_kc_renw_2 <- if (is.null(dl$prices_kc_renw_2)) dl$prices_renw_oo_2 else dl$prices_kc_renw_2
dl$prices_oo_renw_1 <- dl$prices_renw_oo_1
dl$prices_renw_oo_2 <- dl$prices_oo_renw_2

# Full OO matrices: also add load_estimation_bootstrap_data.R expected aliases
dl$prices_renw_oo_full_1 <- dl$prices_oo_renw_full_1
dl$prices_renw_oo_full_2 <- dl$prices_oo_renw_full_2

# Aggregate prices_raw (9 cols) and prices_oo_full (25 cols) — correlated across person
raw_means <- pr$prices_raw_means
raw_sds <- pmax(pr$prices_raw_sds, 0.01)
# Resolve column names (JSON loses named-vector keys)
raw_names <- names(raw_means)
if (is.null(raw_names) || length(raw_names) != 9) raw_names <- pr$prices_raw_col_names

# prices_not_na_ind: generate from column-level proportions (not stored as individual-level data).
# The profile stores per-column not-na fractions; we generate a binary indicator matrix.
PLACEHOLDER_PRICE <- 99999  # typical NA-coded value in real data
if (!is.null(cfg$prices_not_na_ind_colmeans)) {
  pna_fracs <- as.numeric(cfg$prices_not_na_ind_colmeans)
} else if (!is.null(cfg$prices_not_na_ind)) {
  # Legacy: individual-level matrix still in profile
  pna_fracs <- colMeans(as.matrix(cfg$prices_not_na_ind))
} else {
  pna_fracs <- rep(1, 9)  # fallback: all valid
}
pna <- matrix(0L, N_choice, length(pna_fracs))
colnames(pna) <- raw_names
for (j in seq_along(pna_fracs)) {
  pna[, j] <- as.integer(runif(N_choice) < pna_fracs[j])
}

# Profile now stores valid-entry-only means/SDs (placeholders excluded via NA mask).
# No variance decomposition needed — use means/SDs directly.
PLACEHOLDER_PRICE <- 99999  # value for entries where prices_not_na_ind == 0

dl$prices_raw <- matrix(0, N_choice, 9)
for (j in 1:9) {
  not_na_frac <- mean(pna[, j])
  if (not_na_frac <= 0.01) {
    dl$prices_raw[, j] <- PLACEHOLDER_PRICE
  } else if (not_na_frac < 0.99) {
    # Mixed column: valid entries get draws around true mean, rest get placeholder
    not_na <- as.logical(pna[, j])
    dl$prices_raw[not_na, j] <- raw_means[j] + raw_sds[j] *
      (PRICE_RHO * z_price_person[not_na] + sqrt(1 - PRICE_RHO^2) * rnorm(sum(not_na)))
    dl$prices_raw[!not_na, j] <- PLACEHOLDER_PRICE
  } else {
    dl$prices_raw[, j] <- raw_means[j] + raw_sds[j] *
      (PRICE_RHO * z_price_person + sqrt(1 - PRICE_RHO^2) * rnorm(N_choice))
  }
}
if (!is.null(raw_names) && length(raw_names) == 9) {
  colnames(dl$prices_raw) <- raw_names
} else {
  colnames(dl$prices_raw) <- as.character(1:9)
}
dl$prices_raw <- pmax(dl$prices_raw, 0.01)

# prices_oo_full: valid-entry-only means/SDs from profile
oo_means_raw <- pr$prices_oo_full_means
oo_sds <- pmax(pr$prices_oo_full_sds, 0.01)
# Resolve OO column names
oo_names <- names(oo_means_raw)
if (is.null(oo_names) || length(oo_names) != 25) oo_names <- pr$prices_oo_full_col_names

# Generate OO NA mask from profile (per-column valid fractions)
oo_na_fracs <- if (!is.null(cfg$prices_oo_not_na_ind_full_colmeans)) {
  as.numeric(cfg$prices_oo_not_na_ind_full_colmeans)
} else {
  # Fallback: coverage-3 columns share prices_raw col 3 NA pattern
  fracs <- rep(1, 25)
  if (!is.null(oo_names)) {
    cov3_cols <- grep("_3$", oo_names)
    fracs[cov3_cols] <- if ("3" %in% colnames(pna)) mean(pna[, "3"]) else 1
  }
  fracs
}
oo_pna <- matrix(0L, N_choice, 25)
for (j in 1:25) {
  oo_pna[, j] <- as.integer(runif(N_choice) < oo_na_fracs[j])
}

dl$prices_oo_full <- matrix(0, N_choice, 25)
oo_firms <- OO_FIRMS
oo_covs <- 3:7
for (j in 1:25) {
  f <- oo_na_fracs[j]
  if (f <= 0.01) {
    dl$prices_oo_full[, j] <- PLACEHOLDER_PRICE
  } else if (f < 0.99) {
    not_na <- as.logical(oo_pna[, j])
    dl$prices_oo_full[not_na, j] <- oo_means_raw[j] + oo_sds[j] *
      (PRICE_RHO * z_price_person[not_na] + sqrt(1 - PRICE_RHO^2) * rnorm(sum(not_na)))
    dl$prices_oo_full[!not_na, j] <- PLACEHOLDER_PRICE
  } else {
    dl$prices_oo_full[, j] <- oo_means_raw[j] + oo_sds[j] *
      (PRICE_RHO * z_price_person + sqrt(1 - PRICE_RHO^2) * rnorm(N_choice))
  }
}
if (!is.null(oo_names) && length(oo_names) == 25) {
  colnames(dl$prices_oo_full) <- oo_names
} else {
  colnames(dl$prices_oo_full) <- as.vector(outer(oo_firms, oo_covs, paste, sep = "_"))
}
dl$prices_oo_full <- pmax(dl$prices_oo_full, 0.01)

# Aggregate paid price — correlated with person risk
dl$prices_paid <- pr$prices_paid_mean + pr$prices_paid_sd *
  (PRICE_RHO * z_price_person + sqrt(1 - PRICE_RHO^2) * rnorm(N_choice))
dl$prices_paid <- pmax(dl$prices_paid, 0.01)
dl$prices_paid_t1 <- numeric(N_choice)

# Coverage limits — match load_estimation_bootstrap_data.R (the authoritative source)
# This overrides get_data_list_final.R; the real data_list has these values.
dl$limits_raw <- pr$limits_raw
dl$limits_1 <- COVERAGE_LIMITS_1
dl$limits_2 <- COVERAGE_LIMITS_2
dl$limits_oo_raw <- pr$limits_oo_raw
dl$limits_oo_full_1 <- pr$limits_oo_full_1
dl$limits_oo_full_2 <- pr$limits_oo_full_2

# Price indicator matrices — generated from column-level proportions
dl$prices_not_na_ind <- pna  # already generated above from pna_fracs
# prices_pre_cov/firm_ind: generate from column proportions (or defaults)
if (!is.null(cfg$prices_pre_cov_ind_colmeans)) {
  pre_cov_fracs <- as.numeric(cfg$prices_pre_cov_ind_colmeans)
  dl$prices_pre_cov_ind <- matrix(as.integer(runif(N_choice * length(pre_cov_fracs)) <
    rep(pre_cov_fracs, each = N_choice)), nrow = N_choice)
} else {
  dl$prices_pre_cov_ind <- matrix(0L, nrow = N_choice, ncol = 9)
}
if (!is.null(cfg$prices_pre_firm_ind_colmeans)) {
  pre_firm_fracs <- as.numeric(cfg$prices_pre_firm_ind_colmeans)
  dl$prices_pre_firm_ind <- matrix(as.integer(runif(N_choice * length(pre_firm_fracs)) <
    rep(pre_firm_fracs, each = N_choice)), nrow = N_choice)
} else {
  dl$prices_pre_firm_ind <- matrix(0L, nrow = N_choice, ncol = 9)
}
# Use the generated OO NA mask (oo_pna from prices_oo_full section)
# prices_oo_not_na_ind: 5-column version (one col per coverage, all firms share same availability)
dl$prices_oo_not_na_ind <- if (exists("oo_pna")) {
  # Extract one column per coverage (firms share availability within coverage)
  cov_cols <- seq(1, 25, by = 5)  # first firm's column for each coverage
  oo_pna[, cov_cols, drop = FALSE]
} else {
  matrix(1L, nrow = N_choice, ncol = 5)
}
dl$prices_oo_not_na_ind_full <- if (exists("oo_pna")) {
  data.frame(oo_pna)
} else {
  data.frame(matrix(1L, nrow = N_choice, ncol = 25))
}
dl$prices_oo_pre_cov_ind_nb <- matrix(0, nrow = I, ncol = 5)
dl$prices_oo_pre_firm_ind_nb <- matrix(0, nrow = I, ncol = 5)
# limit_mm_ind: generate from column proportions
if (!is.null(cfg$limit_mm_ind_colmeans)) {
  mm_fracs <- as.numeric(cfg$limit_mm_ind_colmeans)
  dl$limit_mm_ind <- matrix(as.integer(runif(N_choice * length(mm_fracs)) <
    rep(mm_fracs, each = N_choice)), nrow = N_choice)
} else {
  dl$limit_mm_ind <- matrix(0L, nrow = N_choice, ncol = 9)
}
dl$limit_oo_csl_ind <- rep(0, 5)
dl$limit_oo_csl_ind_full <- rep(0, 25)
dl$limit_oo_mm_ind <- matrix(0, nrow = N_choice, ncol = 5)
dl$limit_oo_mm_ind_full <- data.frame(matrix(0, nrow = N_choice, ncol = 25))

# Coverage space
dl$cov_space <- matrix(rbinom(N_choice * 15, 1, 0.5), nrow = N_choice, ncol = 15)
dl$cov_space_index <- as.integer(rep(2L, N_choice))
dl$cov_space_oo_index <- as.integer(sample(15:26, N_choice, replace = TRUE))
dl$cov_space_options <- pr$cov_space_options
dl$cov_space_oo_options <- pr$cov_space_oo_options

## ---- 7. Rate revision and TM variables --------------------------------------

cat("  Setting rate/TM variables...\n")

# UBI pricing regime (ubi_rate_dvc_vers) — top-level field for runtime use
if (!is.null(rt$ubi_rate_dvc_vers_dist) && !is.null(rt$ubi_rate_dvc_vers_values)) {
  dl$ubi_rate_dvc_vers <- as.numeric(sample(
    rt$ubi_rate_dvc_vers_values, N_choice, replace = TRUE,
    prob = rt$ubi_rate_dvc_vers_dist
  ))
} else {
  dl$ubi_rate_dvc_vers <- as.numeric(sample(1:3, N_choice, replace = TRUE))
}

dl$R_t1 <- rnorm(N_choice, rt$p_R_ftr_raw_mean, rt$p_R_ftr_raw_sd)
dl$p_R_ftr_raw <- rnorm(N_choice, rt$p_R_ftr_raw_mean, rt$p_R_ftr_raw_sd)
dl$p_R_ftr_na <- rbinom(N_choice, 1, 0.087)
dl$p_R_ftr_R <- rnorm(N_R, 1.04, 0.1)
dl$p_R_ftr_wo_clm <- rnorm(N_choice, rt$p_R_ftr_wo_clm_mean, 0.1)
dl$p_R_ftr_wo_clm_R <- rnorm(N_R, 1.036, 0.1)
dl$p_R_ftr_wo_clm_R_ordered <- dl$p_R_ftr_wo_clm_R
dl$p_R_ftr_wo_clm_w_tm <- rnorm(N_choice, 1.019, 0.1)
dl$p_R_ftr_wo_clm_w_tm_R <- rnorm(I_tm_R, 1.055, 0.1)
dl$p_R_ftr_wo_clm_w_tm_R_ordered <- dl$p_R_ftr_wo_clm_w_tm_R
dl$p_R_ftr_tm_disc <- rnorm(I, rt$p_R_ftr_tm_disc_mean, rt$p_R_ftr_tm_disc_sd)
dl$p_R_ftr_tm_disc <- pmin(pmax(dl$p_R_ftr_tm_disc, 0.7), 1.1)
dl$p_R_ftr_tm_disc_tm_R <- dl$p_R_ftr_tm_disc[1:I_tm_R]
dl$p_R_ftr_w_tm_R <- rnorm(I_tm_R, 1.058, 0.1)

dl$clm_surcharge <- rnorm(N_choice, rt$clm_surcharge_mean, rt$clm_surcharge_sd)
dl$clm_surcharge <- pmax(dl$clm_surcharge, 0.1)
dl$clm_surcharge_ftr <- 1 + dl$clm_surcharge

dl$tm_ind <- as.numeric(rbinom(I, 1, rt$tm_ind_prop))
dl$tm_int <- dl$tm_ind * runif(I, 0, 1.3)
dl$tm_start_ind <- as.numeric(rbinom(I, 1, rt$tm_start_ind_prop))
dl$tm_lb <- ifelse(dl$tm_ind == 1, runif(I, -10, 0), -10)
dl$tm_ub <- ifelse(dl$tm_ind == 1, runif(I, 0, 30), 0)
dl$tm_not_na_ind <- as.numeric(dl$tm_ind > 0 | runif(I) > 0.07)
# tm_optin_disc: discrete-ish values for TM enrollees, zero for non-TM
dl$tm_optin_disc <- numeric(I)
tm_enrolled <- which(dl$tm_ind == 1)
dl$tm_optin_disc[tm_enrolled] <- sample(c(0, 1, 2, 3, 5, 7, 10),
  length(tm_enrolled), replace = TRUE, prob = c(0.12, 0.30, 0.15, 0.15, 0.12, 0.08, 0.08))
dl$tm_optin_disc_ftr_2 <- runif(N_by[4], 0.9, 1)

dl$active_ind <- as.numeric(rbinom(N_choice, 1, rt$active_ind_prop))
dl$x_new_ind <- as.numeric(rbinom(N_choice, 1, rt$x_new_ind_prop))

## ---- 8. Tenure and demographic variables ------------------------------------

# renw_cnt: sequential 0, 1, ..., K-1 per individual (not random)
dl$renw_cnt <- integer(N_choice)
for (i in 1:I) {
  obs_idx <- which(dl$N_choice_to_I == i)
  dl$renw_cnt[obs_idx] <- seq(0, length(obs_idx) - 1)
}
dl$renw_cnt_choice <- dl$renw_cnt
dl$renw_cnt_choice0 <- as.numeric(dl$renw_cnt == 0)
dl$renw_cnt_choice1 <- as.numeric(dl$renw_cnt == 1)
dl$renw_cnt_choice59 <- as.numeric(dl$renw_cnt >= 5)
dl$d_t1 <- rnorm(N_choice, 1.458, 1)
dl$d_t1 <- pmax(dl$d_t1, 0)
dl$d_t1_old <- rnorm(dl$N_old, 1.83, 1)
dl$demand_choice_index <- as.numeric(dl$d_t_new)
dl$demand_choice_index_old <- rnorm(dl$N_old, 2.34, 1)
# last_renewal_seen_i: count of renewal obs per individual (= obs_count - 1)
obs_per_i <- tabulate(dl$N_choice_to_I, nbins = I)
dl$last_renewal_seen_i <- pmax(obs_per_i - 1L, 0L)
dl$last_renewal_seen <- dl$last_renewal_seen_i[dl$N_choice_to_I]

# Use log-normal (real data is right-skewed; rnorm + pmax inflated the mean by ~12%)
zi_sigma2 <- log(1 + (gd$zip_income_sd / gd$zip_income_mean)^2)
zi_mu     <- log(gd$zip_income_mean) - zi_sigma2 / 2
dl$zip_income <- rlnorm(N_choice, zi_mu, sqrt(zi_sigma2))
dl$zip_income_raw <- dl$zip_income * 5
dl$income_choice <- dl$zip_income / dim$dollar_norm
dl$mm_index <- as.numeric(sample(1:2, N_choice, replace = TRUE, prob = gd$mm_index_dist / sum(gd$mm_index_dist)))
dl$mm_oo_index <- dl$mm_index

dl$attriter <- as.integer(sample(1:N_choice, min(22982, N_choice)))
dl$plan_switcher <- as.integer(sample(1:I, min(1851, I)))
dl$remaining_problematic_n_s <- as.integer(sample(1:N_clm, min(224, N_clm)))

## ---- 9. Sampling weights ----------------------------------------------------

# Sampling weights: all 1 for simulated data (no stratified subsampling)
dl$sampling_weight <- rep(1, I)
dl$sampling_enum_choice <- rep(1, N_choice)
dl$sampling_enum_clm <- rep(1, N_clm)
dl$sampling_enum_tm_R <- rep(1, I_tm_R)
dl$estimation_weights_choice <- rep(1, N_choice)
dl$bootstrap_weights_clm <- rep(1, N_clm)
dl$bootstrap_weights_tm_R <- rep(1, I_tm_R)
dl$bootstrap_weights_choice <- rep(1, N_choice)

## ---- 10. Scheme indicators --------------------------------------------------

dl$R_nb_scheme <- factor(sample(1:dim$D_R_nb_scheme, N_R_nb, replace = TRUE))
dl$R_nb_scheme_raw <- factor(sample(1:dim$D_R_nb_scheme, I, replace = TRUE))
dl$R_nb_scheme_I <- dl$R_nb_scheme_raw
dl$R_nb_scheme_cutoffs <- rs$R_nb_scheme_cutoffs
dl$R_renw_scheme <- factor(sample(1:dim$D_R_renw_scheme, N_R - N_R_nb, replace = TRUE))
dl$R_renw_scheme_raw <- factor(sample(1:dim$D_R_renw_scheme, N_choice - I, replace = TRUE))
dl$R_renw_scheme_N <- dl$R_renw_scheme_raw
dl$R_renw_scheme_cutoffs <- rs$R_renw_scheme_cutoffs
dl$R_tm_scheme <- factor(sample(1:dim$D_R_tm_scheme, I_tm_R, replace = TRUE))
dl$R_tm_scheme_cutoffs <- rs$R_tm_scheme_cutoffs
dl$R_scheme_choice <- as.numeric(sample(1:4, N_choice, replace = TRUE))
dl$R_tm_scheme_choice <- as.numeric(sample(0:3, N_choice, replace = TRUE))
dl$R_tm_scheme_I <- as.numeric(sample(0:3, I, replace = TRUE))

## ---- 11. Estimation config (Stan data block requires these) -----------------

dl$dollar_norm <- dim$dollar_norm
dl$discount_factor <- dim$discount_factor
dl$debug <- as.integer(sp$debug)
dl$run_estimation <- 1L
dl$run_demand_blocks <- rep(1L, dim$N_choice_regimes)
dl$d_tm_mh_rational_ind <- 1L
dl$d_by_block <- rs$N_by_choice_regime
dl$grainsize <- as.integer(sp$grainsize)
dl$L <- dim$L

# Pareto alpha (individual-level, from severity model — placeholder)
dl$pareto_alpha_choice_base <- runif(N_choice, 1.2, 3)

# MH coefficients (from cost model — constant, not estimated in sev/price)
dl$theta_mh_llambda <- lapply(1:nrow(MH_PARAMS), function(i) MH_PARAMS[i, ])

# Price model params (from external estimation — placeholder scalars)
dl$theta_0_R_nb_mean <- rep(0, dim$D_R_nb_scheme)
dl$theta_1_R_nb_mean <- rep(0, dim$D_R_nb_scheme)
dl$theta_0_R_nb_sd <- rep(0.1, dim$D_R_nb_scheme)
dl$theta_0_R_renw_mean <- rep(0, dim$D_R_renw_scheme)
dl$theta_1_R_renw_mean <- rep(0, dim$D_R_renw_scheme)
dl$theta_0_R_renw_sd <- rep(0.1, dim$D_R_renw_scheme)
dl$theta_0_R_tm_mean <- rep(0, dim$D_R_tm_scheme)
dl$theta_1_R_tm_mean <- lapply(1:dim$D_R_tm_scheme, function(i) c(0, 0))
dl$theta_0_R_tm_sd <- rep(0.1, dim$D_R_tm_scheme)

# Prior/bounds (from est_config)
dl$eps_prior <- 1
dl$score_sd_lb <- sp$score_sd_lb
dl$score_sd_ub <- 1
dl$risk_aversion_ub <- 1
dl$sigma_logit_lb <- SIGMA_LOGIT_LB
dl$risk_target_scale_factor <- 1
dl$risk_moment_scale_factor <- 1
dl$score_moment_scale_factor <- 1
dl$choice_moment_scale_factor <- 1
dl$select_moment_scale_factor <- 1

## ---- Fixup: fields that depend on variables set in later sections -----------

# log_tm_score: populate for TM enrollees (tm_ind was set in section 7)
tm_enrolled <- which(dl$tm_ind == 1)
dl$log_tm_score[tm_enrolled] <- rnorm(length(tm_enrolled), om$log_tm_score_mean, om$log_tm_score_sd)

# Scheme-dependent score shift: scheme 3 has distinctly lower scores (Fig B2a)
# Real scheme codes are -99, 0, 2, 3 (from profile rate_stats)
scheme_shift_map <- c("-99" = 0, "0" = 0.2, "2" = 0.0, "3" = -0.6)
for (i in tm_enrolled) {
  s <- as.character(as.integer(dl$ubi_rate_dvc_vers[i]))
  if (!is.na(s) && s %in% names(scheme_shift_map)) {
    dl$log_tm_score[i] <- dl$log_tm_score[i] + scheme_shift_map[s]
  }
}
# Update log_tm_score_R and its ordered version to match
dl$log_tm_score_R <- dl$log_tm_score[tm_enrolled[seq_len(length(dl$log_tm_score_R))]]
dl$log_tm_score_R_ordered <- dl$log_tm_score_R

# Score-correlated renewal factor: higher score -> higher discount factor (Fig B2b)
# Slope calibrated to stay within 0.7-1.1 bounds over score range ~2.5-6.0
# Overwrites the IID version from section 7
score_for_disc <- dl$log_tm_score
score_tm_mean <- mean(score_for_disc[tm_enrolled])
dl$p_R_ftr_tm_disc <- rt$p_R_ftr_tm_disc_mean +
  sp$score_to_discount$slope * (score_for_disc - score_tm_mean) +
  rnorm(I, 0, sp$score_to_discount$noise_sd)
dl$p_R_ftr_tm_disc <- pmin(pmax(dl$p_R_ftr_tm_disc, sp$score_to_discount$lb), sp$score_to_discount$ub)
dl$p_R_ftr_tm_disc_tm_R <- dl$p_R_ftr_tm_disc[tm_enrolled[seq_len(length(dl$p_R_ftr_tm_disc_tm_R))]]

# clm_sev_raw: assign severity to claiming observations
dl$clm_sev_raw[dl$N_sev_to_N] <- dl$clm_sev

# clm_sev_pareto_below_limit: cap at coverage limits (demand_choice_index was set in section 8)
sev_below_idx <- dl$N_sev_pareto_below_limit_to_N
sev_demand <- pmax(dl$demand_choice_index[sev_below_idx], 1)
sev_limits <- dl$limits_raw[pmin(sev_demand, length(dl$limits_raw))]
dl$clm_sev_pareto_below_limit <- pmin(dl$clm_sev_pareto_below_limit, sev_limits - 1)

# d_t1_new: cap per renewal block to Js[k] (block 5: 1-5, block 6: 1-4)
# In real data, MM2 renewals (block 6) never have d_t1_new > 4 because
# MM2 NB only has 4 coverage tiers. Simulated panel may cross MM boundaries.
n5 <- rs$N_by_choice_regime[5]
n6 <- rs$N_by_choice_regime[6]
# Block 5 (MM1 renewals): cap to Js[5] = 5
dl$d_t1_new[1:n5][dl$d_t1_new[1:n5] > rs$Js[5]] <-
  ((dl$d_t1_new[1:n5][dl$d_t1_new[1:n5] > rs$Js[5]] - 1) %% rs$Js[5]) + 1L
dl$d_t1_new[1:n5][dl$d_t1_new[1:n5] == 0] <- as.integer(sample(
  1:rs$Js[5], sum(dl$d_t1_new[1:n5] == 0), replace = TRUE))
# Block 6 (MM2 renewals): cap to Js[6] = 4
idx6 <- (n5 + 1):(n5 + n6)
dl$d_t1_new[idx6][dl$d_t1_new[idx6] > rs$Js[6]] <-
  ((dl$d_t1_new[idx6][dl$d_t1_new[idx6] > rs$Js[6]] - 1) %% rs$Js[6]) + 1L
dl$d_t1_new[idx6][dl$d_t1_new[idx6] == 0] <- as.integer(sample(
  1:rs$Js[6], sum(dl$d_t1_new[idx6] == 0), replace = TRUE))

# Rename Stan-expected field for OO renewal prices
dl$prices_renw_oo_1 <- dl$prices_oo_renw_1
dl$prices_renw_oo_2 <- dl$prices_oo_renw_2

## ---- 12. Choice range metadata (from regime structure) ----------------------

# These define the nested logit boundaries
dl$choice_to_R_nb_max <- matrix(0, nrow = dim$D_R_nb_scheme, ncol = 4)
dl$choice_to_R_nb_min <- matrix(0, nrow = dim$D_R_nb_scheme, ncol = 4)
dl$choice_to_R_nb_ranges <- data.frame(matrix(0, nrow = dim$D_R_nb_scheme, ncol = 3))
dl$choice_to_R_renw_max <- matrix(0, nrow = dim$D_R_renw_scheme, ncol = 2)
dl$choice_to_R_renw_min <- matrix(0, nrow = dim$D_R_renw_scheme, ncol = 2)
dl$choice_to_R_renw_ranges <- data.frame(matrix(0, nrow = dim$D_R_renw_scheme - 1, ncol = 3))
dl$choice_to_R_tm_max <- matrix(0, nrow = dim$D_R_tm_scheme, ncol = 2)
dl$choice_to_R_tm_min <- matrix(0, nrow = dim$D_R_tm_scheme, ncol = 2)
dl$choice_to_R_tm_ranges <- data.frame(matrix(0, nrow = dim$D_R_tm_scheme, ncol = 3))

## ---- 13. Structural outcome simulation (claims, scores, choices) ------------
## Replace marginal-drawn outcomes with draws from the structural model
## using real-data parameter estimates as the true DGP.

cat("\n--- Structural outcome simulation ---\n")
## rp and leps already loaded at script start

## B. Claims — Poisson(exp(theta_0 + X * theta_1 + leps[i]))
cat("  Simulating claims from structural model...\n")
llambda_clm <- as.numeric(rp$theta_0_llambda + dl$X_clm %*% rp$theta_1_llambda +
                           leps[dl$N_clm_to_I])
dl$clm_count_N_clm <- as.integer(rpois(N_clm, exp(llambda_clm)))

llambda_sev_clm <- llambda_clm + rp$theta_0_llambda_severe
# Independent Poisson (Stan model treats total and severe as separate likelihoods)
dl$clm_count_severe_N_clm <- as.integer(rpois(N_clm, exp(llambda_sev_clm)))

# Update derived fields
dl$clm_count <- as.integer(c(dl$clm_count_N_clm, rep(0L, N_choice - N_clm)))
dl$clm_count_minor_N_clm <- pmax(dl$clm_count_N_clm - dl$clm_count_severe_N_clm, 0L)
dl$clm_count_minor <- integer(N_choice)
dl$clm_count_minor[1:N_clm] <- dl$clm_count_minor_N_clm
dl$clm_count_severe <- numeric(N_choice)
dl$clm_count_severe[1:N_clm] <- dl$clm_count_severe_N_clm

# Update aggregate claim fields used by moment matching
dl$clm_count_I_tm_R_ordered <- as.numeric(dl$clm_count_N_clm[dl$I_tm_R_ordered_to_N_clm])
dl$clm_count_N_choice <- as.numeric(dl$clm_count[1:N_choice])

cat("    mean claim rate:", round(mean(dl$clm_count_N_clm), 4),
    "  mean severe rate:", round(mean(dl$clm_count_severe_N_clm), 4), "\n")

## C. TM Scores — Normal(score_mean(leps, llambda), score_sd)
## Scale score slopes to match real data score variance. The leps contribution
## (leps * theta_1[,1]) is too large because simulated leps have wider variance
## for TM participants. Scale down score slopes and compensate by scaling up
## the price model coefficient on scores (preserving leps -> discount mapping).
cat("  Simulating TM scores from structural model...\n")
R_tm_cuts <- dl$R_tm_scheme_cutoffs  # 0-indexed cutoffs: [0, c1, c2, I_tm_R]
score_scale <- 0.4  # scale down score slopes to match real data score variance
for (r in seq_len(dl$D_R_tm_scheme)) {
  idx <- (R_tm_cuts[r] + 1):R_tm_cuts[r + 1]
  if (length(idx) == 0) next
  n_clm_idx <- dl$I_tm_R_ordered_to_N_clm[idx]
  i_idx <- dl$N_clm_to_I[n_clm_idx]
  leps_r <- leps[i_idx]
  llambda_r <- llambda_clm[n_clm_idx]
  scaled_slopes <- rp$theta_1_log_score_mean[r, ] * score_scale
  score_mean <- rp$theta_0_log_score_mean[r] +
    cbind(leps_r, llambda_r - leps_r) %*% scaled_slopes
  dl$log_tm_score_R_ordered[idx] <- rnorm(length(idx), score_mean,
                                           rp$theta_0_log_score_sd[r])
}
# Update the non-ordered version
dl$log_tm_score_R <- dl$log_tm_score_R_ordered
cat("    Score scale factors:", round(score_scale, 3), "\n")
cat("    mean log_tm_score:", round(mean(dl$log_tm_score_R_ordered), 4),
    " sd:", round(sd(dl$log_tm_score_R_ordered), 4), "\n")

## D. Choices — multinomial logit from Stan exposed utility functions
cat("  Simulating choices from demand model (compiling Stan)...\n")

if (!requireNamespace("cmdstanr", quietly = TRUE))
  stop("cmdstanr required for choice simulation")

# Set up environment for extraction pipeline
model_name <- "model_main"
model_config <- ""
bootstrap_id <- 0
estimation_type <- "opt"
use_mle <- TRUE

# Load sev/price model params BEFORE saving (pricing R factors are zero placeholders)
dl <- load_sev_price_params(dl, "data/estimates/model_output", bootstrap_id = 0)

# Save data_list with correct sev/price params so extraction pipeline loads them
cache_dir <- file.path(SIM_DATA_DIR, "cache")
dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
saveRDS(dl, file.path(cache_dir, "data_list_IL.rds"))
cat("  Pre-saved data_list (with sev/price params) for extraction pipeline\n")

data_list <- dl  # extraction scripts read from data_list

# Point extraction at real-data model output (not simulated)
# We need log_file_suffix to find the .Rda metadata file
real_model_dir <- file.path("data/estimates/model_output", model_name)
log_file_suffix <- LOG_FILE_SUFFIXES[[model_name]]

# Temporarily override paths so extraction reads real-data model
saved_MODEL_OUT_DIR <- MODEL_OUT_DIR
MODEL_OUT_DIR <- "data/estimates/model_output"

# Run the full extraction pipeline: loads params from real-data CSV,
# compiles Stan with expose_functions, loads data from data_list,
# computes latent params
source("code/simulate/functions/estimation/extract_model_estimates_cmdstan.R")

# Override nu/leps with the real-data values (extraction may have loaded them)
nu <- rp$nu
eps_sd <- rp$eps_sd
leps <- nu * eps_sd

# Compute latent params and choice probabilities
d_tm_mh_rational_ind <- ifelse(is.null(d_tm_mh_rational_ind), 1, d_tm_mh_rational_ind)
source("code/simulate/functions/estimation/get_c_latentparams.R")
source("code/simulate/functions/estimation/get_d_latentparams.R")

# Scale sigma_logit by 1/(NUM_RENEWAL_PERIODS-1) for per-period choice simulation
sigma_logit <- sigma_logit / (NUM_RENEWAL_PERIODS - 1)
sigma_logit_nb <- sigma_logit_nb / (NUM_RENEWAL_PERIODS - 1)
sigma_logit_renw <- sigma_logit_renw / (NUM_RENEWAL_PERIODS - 1)
sigma_logit_renw1 <- sigma_logit_renw1 / (NUM_RENEWAL_PERIODS - 1)

gc()
source("code/simulate/functions/estimation/get_d_pred.R")

# Restore MODEL_OUT_DIR
MODEL_OUT_DIR <- saved_MODEL_OUT_DIR

# Sample choices from computed probabilities
cat("  Sampling choices from computed probabilities...\n")
ncc <- dl$n_choice_regime_cutoffs
for (k in seq_along(dl$Js)) {
  n0 <- ncc[k]; n1 <- ncc[k + 1] - 1
  N_k <- n1 - n0 + 1
  if (N_k == 0) next
  probs <- exp(log_choice_probs[[k]])
  # Ensure valid probabilities (clip numerical issues)
  probs <- pmax(probs, 0)
  probs <- probs / rowSums(probs)
  # Blocks 1-4 (NB): 1-indexed plans, no attrition
  # Blocks 5-6 (renewal): columns 1..J = plans, column J+1 = outside option (d_t_new=0)
  is_renewal <- (k >= 5)
  J_k <- dl$Js[k]
  for (n in seq_len(N_k)) {
    chosen_col <- sample.int(ncol(probs), 1, prob = probs[n, ])
    if (is_renewal && chosen_col > J_k) {
      dl$d_t_new[n0 + n - 1] <- 0L  # outside option = attrition
    } else {
      dl$d_t_new[n0 + n - 1] <- as.integer(chosen_col)  # 1-indexed plan
    }
  }
}

# Keep original d_t1_new (from section 5) — it was already used in the choice
# probability computation, so d_t_new is consistent with it. Rebuilding from
# simulated d_t_new would introduce inconsistency (attrition→random replacement).

# Update demand_choice_index
dl$demand_choice_index <- as.numeric(dl$d_t_new)

cat("  Choice distribution:\n")
print(table(dl$d_t_new))

## E. Update TM mappings based on simulated choices
## TM participation is endogenous — update I_tm_R indices to reflect who
## actually chose TM (d_t_new > Js[k] in blocks 2, 4)
cat("  Updating TM mappings from simulated choices...\n")
tm_choosers <- integer(0)
for (k in c(2, 4)) {
  n0 <- ncc[k]; n1 <- ncc[k + 1] - 1
  J_k <- dl$Js[k]
  block_choices <- dl$d_t_new[n0:n1]
  # Individuals who chose TM option (d_t_new > Js)
  tm_obs <- which(block_choices > J_k)
  tm_choosers <- c(tm_choosers, dl$N_choice_to_I[n0 + tm_obs - 1])
}
tm_choosers <- sort(unique(tm_choosers))
cat("    TM choosers:", length(tm_choosers), "individuals\n")

# Update TM mappings if we have enough TM choosers
if (length(tm_choosers) >= I_tm_R) {
  # Select I_tm_R from the TM choosers (may need to sample if too many)
  tm_selected <- if (length(tm_choosers) > I_tm_R) {
    sort(sample(tm_choosers, I_tm_R))
  } else {
    tm_choosers[1:I_tm_R]
  }
  dl$I_tm_R_to_I <- as.integer(tm_selected)
  dl$I_tm_R_to_N <- as.integer(tm_selected)
  dl$I_tm_R_to_N_clm <- as.numeric(pmin(tm_selected, N_clm))
  dl$I_tm_R_to_N_R <- as.integer(pmin(tm_selected, N_R))
  dl$I_tm_R_ordered_to_I <- dl$I_tm_R_to_I
  dl$I_tm_R_ordered_to_N_clm <- dl$I_tm_R_to_N_clm

  # Also update I_tm (all TM individuals, >= I_tm_R)
  if (length(tm_choosers) >= I_tm) {
    tm_all <- if (length(tm_choosers) > I_tm) {
      sort(sample(tm_choosers, I_tm))
    } else {
      tm_choosers[1:I_tm]
    }
    dl$I_tm_to_N <- as.numeric(tm_all)
  }

  ## F. Re-simulate TM scores for the updated TM participants
  cat("  Re-simulating TM scores for updated TM mapping...\n")
  R_tm_cuts <- dl$R_tm_scheme_cutoffs
  for (r in seq_len(dl$D_R_tm_scheme)) {
    idx <- (R_tm_cuts[r] + 1):R_tm_cuts[r + 1]
    if (length(idx) == 0) next
    n_clm_idx <- dl$I_tm_R_ordered_to_N_clm[idx]
    i_idx <- dl$N_clm_to_I[n_clm_idx]
    leps_r <- leps[i_idx]
    llambda_r <- llambda_clm[n_clm_idx]
    score_mean <- rp$theta_0_log_score_mean[r] +
      cbind(leps_r, llambda_r - leps_r) %*% rp$theta_1_log_score_mean[r, ]
    dl$log_tm_score_R_ordered[idx] <- rnorm(length(idx), score_mean,
                                             rp$theta_0_log_score_sd[r])
  }
  dl$log_tm_score_R <- dl$log_tm_score_R_ordered

  # Update claim counts for TM observations
  dl$clm_count_I_tm_R_ordered <- as.numeric(dl$clm_count_N_clm[dl$I_tm_R_ordered_to_N_clm])

  cat("    Updated TM scores: mean=", round(mean(dl$log_tm_score_R_ordered), 4),
      " sd=", round(sd(dl$log_tm_score_R_ordered), 4), "\n")
} else {
  cat("    WARNING: only", length(tm_choosers), "TM choosers (<", I_tm_R,
      "), keeping original TM mapping\n")
}

cat("--- Structural simulation complete ---\n\n")

## ---- Save -------------------------------------------------------------------

cat("\nSaving synthetic data_list...\n")

if (!requireNamespace("jsonlite", quietly = TRUE)) install.packages("jsonlite")

# Save to cache/ under SIM_DATA_DIR (separate from raw profile JSONs)
cache_dir <- file.path(SIM_DATA_DIR, "cache")
dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

json_path <- file.path(cache_dir, "data_list_IL.json")
jsonlite::write_json(dl, json_path, auto_unbox = TRUE, digits = 8)
cat("  Saved to", json_path, "\n")

rds_path <- file.path(cache_dir, "data_list_IL.rds")
saveRDS(dl, rds_path)
cat("  Saved to", rds_path, "\n")

## ---- JSON round-trip verification ------------------------------------------

cat("\nJSON round-trip verification (all fields)...\n")
dl_json <- read_data_list(json_path)

all_fields <- union(names(dl), names(dl_json))
n_pass <- 0; n_fail <- 0; n_skip <- 0; fail_fields <- character(0)
for (field in all_fields) {
  if (!(field %in% names(dl)) || !(field %in% names(dl_json))) {
    n_skip <- n_skip + 1
    next
  }
  orig <- dl[[field]]
  rt <- dl_json[[field]]
  # Skip non-atomic types (lists, functions, etc.)
  if (is.list(orig) && !is.data.frame(orig)) { n_skip <- n_skip + 1; next }
  ok <- FALSE
  if (is.matrix(orig) || is.data.frame(orig)) {
    orig_mat <- if (is.data.frame(orig)) as.matrix(orig) else orig
    rt_mat <- if (is.data.frame(rt)) as.matrix(rt) else rt
    if (is.matrix(rt_mat) && all(dim(orig_mat) == dim(rt_mat))) {
      ok <- isTRUE(all.equal(as.numeric(orig_mat), as.numeric(rt_mat), tolerance = 1e-6))
    }
  } else if ((is.numeric(orig) || is.integer(orig)) && (is.numeric(rt) || is.integer(rt))) {
    if (length(orig) == length(rt)) {
      ok <- isTRUE(all.equal(as.numeric(orig), as.numeric(rt), tolerance = 1e-6))
    }
  } else if (is.factor(orig) || is.character(orig)) {
    ok <- isTRUE(all.equal(as.character(orig), as.character(rt)))
  } else if (is.logical(orig)) {
    ok <- isTRUE(all.equal(as.logical(orig), as.logical(rt)))
  } else {
    n_skip <- n_skip + 1; next
  }
  if (ok) {
    n_pass <- n_pass + 1
  } else {
    n_fail <- n_fail + 1
    fail_fields <- c(fail_fields, field)
  }
}
cat("  JSON round-trip:", n_pass, "PASS,", n_fail, "FAIL,", n_skip, "SKIP\n")
if (n_fail > 0) {
  cat("  Failed fields:", paste(fail_fields, collapse = ", "), "\n")
  cat("  NOTE: Pipeline uses RDS (not JSON) for estimation/CTF. JSON losses are informational only.\n")
}

cat("\nSynthetic data_list:", length(dl), "fields\n")
cat("  I =", dl$I, "N_choice =", dl$N_choice, "N_clm =", dl$N_clm, "\n")
cat("Done.\n")
