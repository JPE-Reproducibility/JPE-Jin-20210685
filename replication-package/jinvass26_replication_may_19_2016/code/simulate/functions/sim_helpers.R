################################################################################
## sim_helpers.R — Shared helper functions for simulation/estimation scripts
##
## Functions:
##   compute_kc_oo_matrices()   — Derive key-competitor and outside-option prices
##   format_param_comparison()  — Format true-vs-estimated parameter table
##   print_param_comparison()   — Print formatted comparison table
##   matrix_to_lists()          — Convert matrix rows to list of vectors
##   vec_to_lists()             — Convert vector to list of scalars
##   get_samp_wgt_avg()         — Sampling-weighted average
##   normalize_mat_to_0_1()     — Normalize matrix columns to [0,1]
##   parse_bootstrap_csv()      — Parse CmdStan bootstrap CSV into named param list
##   load_sev_price_params()    — Load sev/price CSVs, compute derived fields, merge into data_list
##   read_data_list()           — Read JSON data_list with type fixups
################################################################################

## ---- KC/OO pricing matrix computation --------------------------------------

compute_kc_oo_matrices <- function(dl) {
  if (!requireNamespace("stringr", quietly = TRUE)) {
    stop("stringr package required for KC/OO matrix computation")
  }

  which.median <- function(x) {
    ord <- order(x)
    ord[ceiling(length(ord) / 2)]
  }

  full_oo_cov_name_1 <- colnames(dl$prices_oo_full)
  full_oo_cov_name_2 <- full_oo_cov_name_1[!grepl("_3", full_oo_cov_name_1)]
  firm_indices <- stringr::str_extract(full_oo_cov_name_2[1:5], "^[^_]+")

  min_price_firm_index <- which.min(
    colMeans(dl$prices_oo_nb_tm_full_2[, grepl("_4", full_oo_cov_name_2)])
  )
  median_price_firm_index <- which.median(
    colMeans(dl$prices_oo_nb_tm_full_2[, grepl("_4", full_oo_cov_name_2)])
  )

  min_cov_1 <- grepl(paste0(firm_indices[min_price_firm_index], "_"), full_oo_cov_name_1)
  med_cov_1 <- grepl(paste0(firm_indices[median_price_firm_index], "_"), full_oo_cov_name_1)

  dl$prices_oo_nb_1    <- dl$prices_oo_nb_full_1[, med_cov_1]
  dl$prices_oo_nb_tm_1 <- dl$prices_oo_nb_tm_full_1[, med_cov_1]
  dl$prices_renw_oo_1  <- dl$prices_renw_oo_full_1[, med_cov_1]
  dl$prices_kc_nb_1    <- dl$prices_oo_nb_full_1[, min_cov_1]
  dl$prices_kc_nb_tm_1 <- dl$prices_oo_nb_tm_full_1[, min_cov_1]
  dl$prices_kc_renw_1  <- dl$prices_renw_oo_full_1[, min_cov_1]

  min_cov_2 <- grepl(paste0(firm_indices[min_price_firm_index], "_"), full_oo_cov_name_2)
  med_cov_2 <- grepl(paste0(firm_indices[median_price_firm_index], "_"), full_oo_cov_name_2)

  dl$prices_oo_nb_2    <- dl$prices_oo_nb_full_2[, med_cov_2]
  dl$prices_oo_nb_tm_2 <- dl$prices_oo_nb_tm_full_2[, med_cov_2]
  dl$prices_renw_oo_2  <- dl$prices_renw_oo_full_2[, med_cov_2]
  dl$prices_kc_nb_2    <- dl$prices_oo_nb_full_2[, min_cov_2]
  dl$prices_kc_nb_tm_2 <- dl$prices_oo_nb_tm_full_2[, min_cov_2]
  dl$prices_kc_renw_2  <- dl$prices_renw_oo_full_2[, min_cov_2]

  dl
}

## ---- Parameter comparison ---------------------------------------------------

format_param_comparison <- function(fit_params, true_params, scalar_params,
                                     array_params = NULL) {
  comparison <- data.frame(
    parameter  = character(),
    true_value = numeric(),
    estimated  = numeric(),
    abs_diff   = numeric(),
    stringsAsFactors = FALSE
  )

  for (pname in names(scalar_params)) {
    col_name <- scalar_params[[pname]]
    if (col_name %in% colnames(fit_params) && !is.null(true_params[[pname]])) {
      est_val  <- as.numeric(fit_params[[col_name]])
      true_val <- true_params[[pname]]
      comparison <- rbind(comparison, data.frame(
        parameter = pname, true_value = round(true_val, 6),
        estimated = round(est_val, 6), abs_diff = round(abs(est_val - true_val), 6),
        stringsAsFactors = FALSE
      ))
    }
  }

  if (!is.null(array_params)) {
    for (ap in array_params) {
      for (d in seq_len(ap$dim)) {
        col_name <- paste0(ap$name, "[", d, "]")
        if (col_name %in% colnames(fit_params)) {
          est_val  <- as.numeric(fit_params[[col_name]])
          true_val <- ap$true[d]
          comparison <- rbind(comparison, data.frame(
            parameter = col_name, true_value = round(true_val, 6),
            estimated = round(est_val, 6), abs_diff = round(abs(est_val - true_val), 6),
            stringsAsFactors = FALSE
          ))
        }
      }
    }
  }
  comparison
}

print_param_comparison <- function(comparison) {
  cat(sprintf("%-35s %12s %12s %12s\n", "Parameter", "True", "Estimated", "Abs Diff"))
  cat(paste(rep("-", 71), collapse = ""), "\n")
  for (i in seq_len(nrow(comparison))) {
    cat(sprintf("%-35s %12.6f %12.6f %12.6f\n",
                comparison$parameter[i], comparison$true_value[i],
                comparison$estimated[i], comparison$abs_diff[i]))
  }
  cat(paste(rep("-", 71), collapse = ""), "\n")
}

## ---- Utility functions ------------------------------------------------------

matrix_to_lists <- function(x) {
  lapply(seq_len(nrow(x)), function(i) x[i, ])
}

vec_to_lists <- function(x) {
  as.list(x)
}

get_samp_wgt_avg <- function(quant_vec, sampling_weights) {
  filter <- is.na(quant_vec) | is.nan(quant_vec) | is.infinite(quant_vec)
  sum(quant_vec[!filter] / sampling_weights[!filter]) / sum(1 / sampling_weights[!filter])
}

normalize_mat_to_0_1 <- function(mat) {
  colmins <- apply(mat, 2, min, na.rm = TRUE)
  mat1 <- sweep(mat, 2, colmins, FUN = "-")
  colmaxes <- apply(mat1, 2, max, na.rm = TRUE)
  colmaxes[colmaxes == 0] <- 1  # avoid division by zero
  sweep(mat1, 2, colmaxes, FUN = "/")
}

## ---- Bootstrap CSV parser ---------------------------------------------------

#' Parse a CmdStan bootstrap CSV into a named list of R objects.
#'
#' Handles both standard CmdStan format (read via cmdstanr::read_cmdstan_csv)
#' and dot-separated format from regenerated CSVs.
#'
#' @param csv_path Path to a bootstrap-result-id-*.csv file
#' @return List with $params (named list of scalars/vectors/matrices)
#'         and $meta (stan_variables, stan_variable_sizes)
parse_bootstrap_csv <- function(csv_path) {
  lines <- readLines(csv_path)
  comment_lines <- grepl("^#", lines)
  non_comment <- which(!comment_lines)
  header <- lines[non_comment[1]]
  data_line <- lines[non_comment[2]]

  col_names_dot <- strsplit(header, ",")[[1]]
  values <- as.numeric(strsplit(data_line, ",")[[1]])

  # Convert dot notation to bracket notation: theta.1.2 -> theta[1,2]
  col_names <- sapply(col_names_dot, function(nm) {
    parts <- strsplit(nm, "\\.")[[1]]
    if (length(parts) == 1) return(nm)
    base <- parts[1]
    idx <- parts[-1]
    paste0(base, "[", paste(idx, collapse = ","), "]")
  }, USE.NAMES = FALSE)

  names(values) <- col_names

  # Build metadata: stan_variables and stan_variable_sizes
  base_names <- sapply(col_names, function(nm) sub("\\[.*", "", nm))
  unique_vars <- unique(base_names)
  stan_variable_sizes <- list()
  for (v in unique_vars) {
    cols_v <- col_names[base_names == v]
    if (!any(grepl("\\[", cols_v))) {
      stan_variable_sizes[[v]] <- 1
    } else {
      bracketed <- cols_v[grepl("\\[", cols_v)]
      idx_strs <- sub(".*\\[(.*)\\]", "\\1", bracketed)
      idx_list <- strsplit(idx_strs, ",")
      ndims <- length(idx_list[[1]])
      dims <- integer(ndims)
      for (d in seq_len(ndims)) dims[d] <- max(as.integer(sapply(idx_list, `[`, d)))
      stan_variable_sizes[[v]] <- dims
    }
  }

  # Extract to named list of R objects
  params <- list()
  for (v in unique_vars) {
    if (v == "lp__") next
    sz <- stan_variable_sizes[[v]]
    if (length(sz) > 1) {
      D <- sz[1]; M <- sz[2]
      mat <- matrix(0, D, M)
      for (d in seq_len(D)) {
        cn <- paste0(v, "[", d, ",", seq_len(M), "]")
        mat[d, ] <- values[cn]
      }
      params[[v]] <- mat
    } else if (sz > 1) {
      cn <- paste0(v, "[", seq_len(sz), "]")
      params[[v]] <- as.numeric(values[cn])
    } else {
      params[[v]] <- as.numeric(values[v])
    }
  }

  list(
    params = params,
    meta = list(stan_variables = unique_vars, stan_variable_sizes = stan_variable_sizes)
  )
}

## ---- Load sev/price params from bootstrap CSVs -----------------------------

#' Load model_sev and model_price bootstrap CSVs, extract params, compute
#' derived fields, and merge everything into data_list.
#'
#' Derived fields computed:
#'   pareto_alpha_choice_base[N_choice] = 1 + plogis(θ₀ + X_choice · θ₁) × 10
#'   sev_minor_mean_choice[N_choice]    = θ₀_sev_minor_mean + X_choice · θ₁
#'
#' @param data_list The Stan data list (must contain X_choice)
#' @param model_out_dir Base model output directory containing model_sev/ and model_price/
#' @param bootstrap_id Bootstrap ID (default 0 for point estimate)
#' @return Updated data_list with sev/price params merged in
load_sev_price_params <- function(data_list, model_out_dir, bootstrap_id = 0) {
  sev_csv <- file.path(model_out_dir, "model_sev", "results",
                        paste0("bootstrap-result-id-", bootstrap_id, ".csv"))
  price_csv <- file.path(model_out_dir, "model_price", "results",
                          paste0("bootstrap-result-id-", bootstrap_id, ".csv"))

  if (file.exists(sev_csv)) {
    cat("Loading sev params from:", sev_csv, "\n")
    sev <- parse_bootstrap_csv(sev_csv)$params
    for (v in names(sev)) data_list[[v]] <- sev[[v]]

    # Compute derived: pareto_alpha_choice_base
    if (!is.null(data_list$X_choice) && !is.null(data_list$theta_0_pareto_alpha_severe)) {
      data_list$pareto_alpha_choice_base <- as.vector(
        1 + plogis(data_list$theta_0_pareto_alpha_severe +
                     data_list$X_choice %*% data_list$theta_1_pareto_alpha_severe) * 10
      )
    }

    # Compute derived: sev_minor_mean_choice
    if (!is.null(data_list$X_choice) && !is.null(data_list$theta_0_sev_minor_mean)) {
      data_list$sev_minor_mean_choice <- as.vector(
        data_list$theta_0_sev_minor_mean +
          data_list$X_choice %*% data_list$theta_1_sev_minor_mean
      )
    }
  } else {
    cat("WARNING: model_sev CSV not found:", sev_csv, "\n")
  }

  if (file.exists(price_csv)) {
    cat("Loading price params from:", price_csv, "\n")
    price <- parse_bootstrap_csv(price_csv)$params
    for (v in names(price)) data_list[[v]] <- price[[v]]
  } else {
    cat("WARNING: model_price CSV not found:", price_csv, "\n")
  }

  data_list
}

## ---- JSON data_list reader --------------------------------------------------

read_data_list <- function(json_path, cache_dir = NULL) {
  # Check for cached RDS first
  rds_path <- sub("\\.json$", ".rds", json_path)
  if (file.exists(rds_path) &&
      (!file.exists(json_path) || file.mtime(rds_path) >= file.mtime(json_path))) {
    return(readRDS(rds_path))
  }

  if (!requireNamespace("jsonlite", quietly = TRUE))
    stop("jsonlite package required")

  dl <- jsonlite::fromJSON(json_path, simplifyVector = TRUE)

  # Fix JSON -> R type mismatches
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

  # Restore matrix column names
  OO_FIRMS <- c(4, 3, 35, 6, 2)
  oo_covs  <- 3:7
  if (!is.null(dl$prices_raw) && is.null(colnames(dl$prices_raw)))
    colnames(dl$prices_raw) <- as.character(seq_len(ncol(dl$prices_raw)))
  if (!is.null(dl$prices_oo_full) && is.null(colnames(dl$prices_oo_full)))
    colnames(dl$prices_oo_full) <- as.vector(outer(OO_FIRMS, oo_covs, paste, sep = "_"))

  # Cache as RDS for faster subsequent loads
  tryCatch(saveRDS(dl, rds_path), error = function(e) NULL)

  dl
}
