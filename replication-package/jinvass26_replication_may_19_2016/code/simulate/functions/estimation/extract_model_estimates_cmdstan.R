source('code/functions/helper.R')

# Specify the location of the bootstrap results (paths from config.R)
if (!exists("MODEL_OUT_DIR")) source("code/config.R")
results_dir <- file.path(MODEL_OUT_DIR, model_name)
bootpath <- file.path(results_dir, "results")
bootstrap_results_dir <- bootpath
outdir <- results_dir


## Determine welfare horizon from model name
if(grepl("model_joint_final_", model_name)){
  model_config = gsub("model_joint_final_", "", model_name)
  welfare_horizon = ifelse(grepl("4p", model_config), 4,
                           ifelse(grepl("2p", model_config), 2, 3))
} else if(grepl("^model_main", model_name)){
  model_config = gsub("^model_main_?", "", model_name)
  welfare_horizon = ifelse(grepl("4p", model_config), 4,
                           ifelse(grepl("2p", model_config), 2, 3))
} else {
  welfare_horizon = NUM_RENEWAL_PERIODS
}


log_file_rda = file.path(outdir, paste0("log_", model_name, "-", log_file_suffix, ".Rda"))
if (file.exists(log_file_rda)) {
  load(log_file_rda)
  for(i in c("output_filename","est_config_summary")) {
    assign(i, log_file_obj[[i]])
  }
} else {
  # Derive from .txt log or hardcode defaults for replication
  output_filename <- paste0(model_name, "-", log_file_suffix, ".csv")
  est_config_summary <- ""
  cat("Note: .Rda log not found, using defaults. output_filename:", output_filename, "\n")
}

if(!exists("bootstrap_id")){
  bootstrap_id <- as.numeric(Sys.getenv("BOOTSTRAP_ID"))
}
# if (is.na(bootstrap_id) || bootstrap_id == "") {
#   bootstrap_id <- 0
# }

#### Load cmdStan objects and estimation DATA ####
if(is.null(output_filename)|| is.null(outdir)){ # || is.null(data_filename)
  print("error - must specify 'output_filename' (.csv), 'data_filename' (.Rda), and put them in the same 'outdir'")
} else {
  if (!exists("bootstrap_results_dir")){
    tmp = file.path(outdir, output_filename)
    print(paste0("loading non-bootstrap result: ", tmp))
  } else {
    tmp = file.path(bootstrap_results_dir, paste0("bootstrap-result-id-", bootstrap_id, ".csv"))
    print(paste0("loading bootstrap result: ", tmp))
  }
  # Try standard CmdStan CSV first; fall back to c1b_regenerate_gq.R format
  results <- tryCatch(read_cmdstan_csv(tmp), error = function(e) NULL)
  if (is.null(results)) {
    # Read regenerated CSV (dot-separated indices, custom comment headers)
    lines <- readLines(tmp)
    comment_lines <- grepl("^#", lines)
    header <- lines[which(!comment_lines)[1]]
    data_line <- lines[which(!comment_lines)[2]]
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
    point_est <- as.data.frame(as.list(values), check.names = FALSE)

    # Reconstruct metadata: stan_variables and stan_variable_sizes
    base_names <- sapply(col_names, function(nm) sub("\\[.*", "", nm))
    unique_vars <- unique(base_names)
    stan_variable_sizes <- list()
    for (v in unique_vars) {
      cols_v <- col_names[base_names == v]
      has_brackets <- any(grepl("\\[", cols_v))
      if (!has_brackets) {
        # Scalar (possibly duplicated GQ column -- treat as scalar)
        stan_variable_sizes[[v]] <- 1
      } else {
        # Parse dimensions from bracket indices
        bracketed <- cols_v[grepl("\\[", cols_v)]
        idx_strs <- sub(".*\\[(.*)\\]", "\\1", bracketed)
        idx_list <- strsplit(idx_strs, ",")
        ndims <- length(idx_list[[1]])
        dims <- integer(ndims)
        for (d in seq_len(ndims)) dims[d] <- max(as.integer(sapply(idx_list, `[`, d)))
        stan_variable_sizes[[v]] <- dims
      }
    }

    results <- list(
      point_estimates = point_est,
      metadata = list(stan_variables = unique_vars, stan_variable_sizes = stan_variable_sizes)
    )
  }
  meta = results$metadata
}

df = as.data.frame(results$point_estimates)

#### Load demand hyperparams ####
print("Loading demand hyper-params")
variables_to_extract = meta$stan_variables
variables_to_extract = variables_to_extract[!grepl("gq", variables_to_extract)]
load_estimation_data_only_ind = T

for(i in variables_to_extract){
  if(i != "lp__"){
    if(length(meta$stan_variable_sizes[[i]]) > 1){
      D = meta$stan_variable_sizes[[i]][1]
      M = meta$stan_variable_sizes[[i]][2]
      tmp = matrix(rep(0,D*M),D,M)
      for(d in 1:D){
        col_names = paste0(i,paste0("[",d,",",paste0((1:M), "]")))
        tmp[d,] = as.numeric(df[col_names])
      }
      assign(i, tmp)
    } else {
      if(meta$stan_variable_sizes[[i]] > 1){
        N = meta$stan_variable_sizes[[i]]
        col_names = paste0(i,paste0("[", paste0((1:N), "]")))
        assign(i, as.numeric(df[col_names]))
      } else {
        assign(i, as.numeric(df[i]))
      }
      print(i)
      print(get(i))
    }
  }
}

est_config_path <- paste0(outdir, "/", "est_config_", gsub(".csv", ".rds", output_filename))
if (file.exists(est_config_path)) {
  est_config_obj = readRDS(est_config_path)
} else {
  # Derive from model_config and data_list
  est_config_obj <- list(
    d_tm_mh_rational_ind = 1L,
    d_by_block = if (exists("data_list")) data_list$N_by_choice_regime else NULL,
    sigma_logit_lb = if (exists("data_list") && !is.null(data_list$sigma_logit_lb)) data_list$sigma_logit_lb else SIGMA_LOGIT_LB
  )
  cat("Note: est_config not found, using defaults\n")
}
for(k in names(est_config_obj)){
  print(k)
  assign(k, est_config_obj[[k]])
}

source("code/simulate/functions/estimation/load_estimation_bootstrap_data.R")
source('code/simulate/functions/estimation/load_model_data.R')


recompile = ifelse(!exists("out_model"), T, !out_model$functions$compiled)
if(recompile){
  model_filename <- if (exists("log_file_obj") && !is.null(log_file_obj$model_filename)) {
    log_file_obj$model_filename
  } else {
    paste0(model_name, ".stan")
  }
  model_expose_function_path = file.path(outdir, model_filename)
  if (!file.exists(model_expose_function_path)) {
    # Try alternative naming: results dir may have model_* name
    alt_names <- list.files(outdir, pattern = "\\.stan$", full.names = TRUE)
    if (length(alt_names) >= 1) {
      model_expose_function_path <- alt_names[1]
      cat("Note: Using alternative Stan file:", model_expose_function_path, "\n")
    } else {
      # Fallback: try models directory
      alt_path <- file.path("data/estimates/model_output", model_name, paste0(model_name, ".stan"))
      if (file.exists(alt_path)) {
        cat("Note: Stan model not found in results dir, using:", alt_path, "\n")
        model_expose_function_path <- alt_path
      }
    }
  }
  out_model = cmdstanr::cmdstan_model(model_expose_function_path, force_recompile=TRUE)
  out_model$expose_functions()
}

# this is added functions needed only for counterfactual calculations
recompile = ifelse(!exists("ctf_model"), T, !ctf_model$functions$compiled)
if(recompile){
  ctf_model = cmdstanr::cmdstan_model("code/simulate/functions/estimation/ctf_functions.stan", force_recompile=TRUE)
  ctf_model$expose_functions()
}

if(!exists("llambda")){
  use_mle <- ifelse(exists("use_mle"), use_mle, T)
  mle_out_dir <- ifelse(exists("mle_out_dir"), mle_out_dir, results_dir)
  source('code/simulate/functions/estimation/get_c_latentparams.R')
}
if(!exists("risk_aversion")){
  d_tm_mh_rational_ind <- ifelse(is.null(d_tm_mh_rational_ind), 1, d_tm_mh_rational_ind)
  source('code/simulate/functions/estimation/get_d_latentparams.R')
}

## add the expected cost to insurer
major_accident_expected_cost = (out_model$functions$m1_accident_oop(pareto_alpha_choice_base, 0.0001/dollar_norm) * lambda_severe ) * dollar_norm

minor_accident_expected_cost = (out_model$functions$m1_claim_minor(sev_minor_mean_choice, sev_minor_sd) * (lambda - lambda_severe)) * dollar_norm

expected_cost_to_insurer = major_accident_expected_cost + minor_accident_expected_cost
