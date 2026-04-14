# Equilibrium grid search with intermediate saves.
#
# HARDCODED ASSUMPTIONS:
#   initial_points: k1=1.0, k2=0.5, k3=k3_calibrated, k4=0, k4s=1
#   hard_coded_upper_limits for k4s: 1.0 (DS), clamped to 1.0 (disc floor)
#   Grid refinement: 5 stages from delta=0.05 to delta=0.01
#   Max 20 refinement iterations before stopping

ipak(c("tictoc"))
source('code/simulate/functions/ctf/get_ctf_util_profit.R')
source('code/simulate/functions/ctf/ctf_equi_k_find_equilibria.R')
source('code/simulate/functions/ctf/find_cycles.R')

part_opt_save_filename <- file.path(bootstrap_ctf_dir, paste0("ctf_opt-part-id-", bootstrap_id, ctf_config_preds,".rds"))
opt_save_filename <- file.path(bootstrap_ctf_dir, paste0("ctf_opt-id-", bootstrap_id, ctf_config_preds,".rds"))

get_pis_grid <- function(stage_grid_full, sequential_step, brand_value, cost_factors, weights_supplied){

  N_stage <- nrow(stage_grid_full)
  out_pis <- matrix(0, N_stage, 4)

  for(n in 1:ceiling(N_stage/sequential_step)){
    tic(cat("My code block", n, "(", N_stage, "total points)\n"))
    n_0 = ((n-1)*sequential_step+1)
    n_1 = min(n*sequential_step, nrow(stage_grid_full))
    stage_grid <- stage_grid_full[n_0:n_1, ]

    out_pis[n_0:n_1,] <- foreach(i = seq_len(nrow(stage_grid)), .combine = 'rbind') %dopar% {
      row_i <- stage_grid[i, ]
      out <- get_ctf_util_profit(
        k0 = row_i$k0,
        k1 = row_i$k1,
        k2 = row_i$k2,
        k2s = row_i$k2s,
        k3 = row_i$k3,
        k4 = row_i$k4,
        k4s = row_i$k4s,
        brand_value = brand_value,
        cost_factors = cost_factors,
        oo_tm_response_ind = !is.na(row_i$k4s),
        ctf_has_tm = T)
      out_pi_i <- get_samp_wgt_avg(rowSums(out$cp_first_period * out$profits), weights_supplied)
      out_pi_oo_i <- get_samp_wgt_avg(rowSums(out$cp_first_period * out$profits_oo_flex), weights_supplied)
      out_pi0_i <- get_samp_wgt_avg(rowSums(out$cp_first_period * out$profits_first_period), weights_supplied)
      out_pi0_oo_i <- get_samp_wgt_avg(rowSums(out$cp_first_period * out$profits_oo_flex_first_period), weights_supplied)

      return(c(out_pi_i, out_pi_oo_i, out_pi0_i, out_pi0_oo_i))
    }
    toc()
  }

  colnames(out_pis) <- c("pi", "pi_oo", "pi0", "pi0_oo")
  return( cbind.data.frame(stage_grid_full,out_pis) )
}

# Grid search bounds for 7 equilibrium strategy parameters:
#   [1] k0  = firm 1 base price level          [4] k2s = firm 1 score-based TM disc slope
#   [2] k1  = firm 1 experience rating slope    [5] k3  = firm 2 (OO) base price level
#   [3] k2  = firm 1 TM discount level          [6] k4  = firm 2 (OO) TM discount level
#                                                [7] k4s = firm 2 (OO) score-based TM disc slope
# Constrained case: tighter bounds to ensure interior solutions
# Unconstrained case: wider bounds for exhaustive search
if(oo_constrained_toggle){
  hard_coded_lower_limits <- c(0.5, -0.5,   0, -0.5, 0.5, 0, -0.5)
  hard_coded_upper_limits <- c(1.5,    5,   5,    1, 1.5, 5,    1)
} else {
  hard_coded_lower_limits <- c(0, -1, -2, -5, 0, -2, -5)
  hard_coded_upper_limits <- c(2,  2,  5,  2, 2,  5,  2)
}

######################
#### PARTIAL EQUI ####
######################
if(USE_CACHE && file.exists(part_opt_save_filename)){
  out_opt_part <- readRDS(part_opt_save_filename)
  k0_part_opt <- out_opt_part$k0_part_opt
  k1_part_opt <- out_opt_part$k1_part_opt
  k2_part_opt <- out_opt_part$k2_part_opt
  k2s_part_opt <- out_opt_part$k2s_part_opt
  out_pis_full <- data.frame()
  for(r in 1:length(out_opt_part$out_pis_list)){
    out_pis_full <- rbind.data.frame(out_pis_full, out_opt_part$out_pis_list[[r]])
  }
  cat("Best K0 PART OPT:", k0_part_opt, "\n")
  cat("Best K1 PART OPT:", k1_part_opt, "\n")
  cat("Best K2 PART OPT:", k2_part_opt, "\n")
  cat("Best K2s PART OPT:", k2s_part_opt, "\n")
} else {
  out_pis_full <- data.frame()
  out_pis_done <- data.frame()

  # list all matching out_pis_file
  out_pis_files <- list.files(
    path = bootstrap_ctf_dir, # ctf_oo_option and thus ctf_dir are for tm cost, mkt shares, and EU horizons, all of which affect the value of out_pis_full
    pattern = paste0("^ctf_opt-part-id-", bootstrap_id,
                     ".*\\.rds$"), #the equilibrium concept or grid constraints do not affect the value of out_pis_full
    full.names = TRUE
  )

  if(USE_CACHE && length(out_pis_files)>0){

    for(obj in out_pis_files){
      out_opt_part <- readRDS(obj)
      for(r in 1:length(out_opt_part$out_pis_list)){
        out_pis_full <- rbind.data.frame(out_pis_full, out_opt_part$out_pis_list[[r]])
      }
    }

    cat("loading existing out_pis_full with dim ", dim(out_pis_full), "\n")
  }

  initial_points <- tibble(k0 = k0_calibrated,
                           k1 = 1.0, #0.6,
                           k2 = 0.5,
                           k2s= 0.5)
  if (exists("CTF_COARSE_GRID") && CTF_COARSE_GRID) {
    precision <- list(c(0.10, 0.40, 1.00, 1.00))
    radius    <- list(c(0.20, 1.00, 2.00, 2.00))
  } else {
    precision <- list(
      c(0.05, 0.20, 0.50, 0.50),
      c(0.02, 0.10, 0.20, 0.20),
      c(0.01, 0.05, 0.10, 0.10)
    )
    radius <- list(
      c(0.1, 0.6, 1.00, 1.00),
      c(0.04, 0.30, 0.40, 0.40),
      c(0.03, 0.15, 0.20, 0.20)
    )
  }
  max_row_coarse = 5  # stages 1-3: broader search
  max_row_fine = 2    # refinement: keep top-2 by own-firm profit

  sequential_step = if (exists("CTF_COARSE_GRID") && CTF_COARSE_GRID) 5000 else 50000
  firm1_params <- c("k0", "k1", "k2", "k2s")
  firm2_params <- c() #"k3", "k4", "k4s"
  out_pis_list <- list()
  equi_list <- list()

  for(i in 1:length(precision)){
    cat("start step", i, "\n")
    max_row <- if (i <= length(precision)) max_row_coarse else max_row_fine
    if(i < 2){
      mid_point = initial_points
    } else {
      if(nrow(equi) > max_row){
        equi <- equi %>% arrange(desc(pi))
        equi <- equi[1:max_row,]
      }
      mid_point = equi
    }

    stage_grid = matrix(nrow=0, ncol=4)
    for(r in 1:nrow(mid_point)){
      stage_k0_grid =  seq(mid_point$k0[r] -radius[[i]][1], mid_point$k0[r] +radius[[i]][1], by = precision[[i]][1])
      stage_k1_grid =  seq(mid_point$k1[r] -radius[[i]][2], mid_point$k1[r] +radius[[i]][2], by = precision[[i]][2])
      stage_k2_grid =  seq(mid_point$k2[r] -radius[[i]][3], mid_point$k2[r] +radius[[i]][3], by = precision[[i]][3])
      stage_k2s_grid = seq(mid_point$k2s[r]-radius[[i]][4], mid_point$k2s[r]+radius[[i]][4], by = precision[[i]][4])
      stage_grid = rbind(stage_grid,
                         expand.grid(k0  = stage_k0_grid,
                                     k1  = stage_k1_grid,
                                     k2  = stage_k2_grid,
                                     k2s  = stage_k2s_grid,
                                     k3  = k3_calibrated,
                                     k4  = NA,
                                     k4s = NA
                         ))
    }

    stage_grid = unique(round(stage_grid,2)) %>% filter(k0  >= hard_coded_lower_limits[1] &
                                                          k1  >= hard_coded_lower_limits[2] &
                                                          k2  >= hard_coded_lower_limits[3] &
                                                          k2s >= hard_coded_lower_limits[4] &
                                                          k0  <= hard_coded_upper_limits[1] &
                                                          k1  <= hard_coded_upper_limits[2] &
                                                          k2  <= hard_coded_upper_limits[3] &
                                                          k2s <= hard_coded_upper_limits[4]
    )
    cat("Stage grid size:", nrow(stage_grid), "\n")
    gridminmax <- cbind(colMin(stage_grid), colMax(stage_grid))
    cat("gridminmax: \n")
    print(gridminmax)
    if(nrow(out_pis_full) > 0){
      out_pis_done <- out_pis_full %>% inner_join(stage_grid)
      stage_grid <- anti_join(stage_grid, out_pis_full)
    }
    cat("Stage grid size effective:", nrow(stage_grid), "\n")

    if(nrow(stage_grid)>0){
      out_pis <- get_pis_grid(
        stage_grid_full = stage_grid,
        sequential_step = min(nrow(stage_grid), sequential_step),
        brand_value = brand_value_calibrated,
        cost_factors = cost_factors_calibrated,
        weights_supplied = weights_combined
      )
    } else {
      out_pis <- data.frame()
    }
    out_pis_full = rbind.data.frame(out_pis_full, out_pis)
    if(nrow(out_pis_done) > 0){
      out_pis <- rbind(out_pis, out_pis_done)
    }

    equi <- find_equilibria(out_pis, firm1_params, firm2_params)

    # Check if equilibrium is on grid boundary (interior solutions preferred)
    equi_hit_minmax_local <-
      (equi$k0 %in% gridminmax[1,]) & !(equi$k0 %in% c(hard_coded_lower_limits[1], hard_coded_upper_limits[1])) |
      (equi$k1 %in% gridminmax[2,]) & !(equi$k1 %in% c(hard_coded_lower_limits[2], hard_coded_upper_limits[2])) |
      (equi$k2 %in% gridminmax[3,]) & !(equi$k2 %in% c(hard_coded_lower_limits[3], hard_coded_upper_limits[3])) |
      (equi$k2s%in% gridminmax[4,]) & !(equi$k2s%in% c(hard_coded_lower_limits[4], hard_coded_upper_limits[4]))

    if(min(equi_hit_minmax_local) < 1){
      equi = equi[!equi_hit_minmax_local,]
    }

    out_pis_list[[i]] = out_pis
    equi_list[[i]] = equi
    print(equi_list[[i]])
    cat("end step", i, "\n")
  }

  i_fine = i
  out_pis_fine = out_pis_list[[i_fine]]

  success_ind = F
  equi_hit_minmax = rep(T, nrow(equi_list[[i]]))

  #### checking whether the equilibria found are interior to our finer search ####
  if (exists("TESTING") && TESTING) {
    cat("TESTING mode: accepting grid equilibria without interior check\n")
    success_ind <- TRUE
  }
  while(success_ind<1){
    i = i + 1
    cycle_ind = F
    cat("start step", i, "\n")
    mid_point <- equi_list[[i-1]] %>% arrange(desc(pi))
    if (nrow(mid_point) > max_row_fine) mid_point <- mid_point[1:max_row_fine, ]

    stage_grid = matrix(nrow=0, ncol=4)
    out_pis_list[[i]] <- data.frame()
    equi_list[[i]] <- data.frame()
    equi_tmp = data.frame()
    equi_hit_minmax_tmp = c()
    for(r in 1:nrow(mid_point)){
      stage_k0_grid =  seq(mid_point$k0[r] -radius[[i_fine]][1], mid_point$k0[r] +radius[[i_fine]][1], by = precision[[i_fine]][1])
      stage_k1_grid =  seq(mid_point$k1[r] -radius[[i_fine]][2], mid_point$k1[r] +radius[[i_fine]][2], by = precision[[i_fine]][2])
      stage_k2_grid =  seq(mid_point$k2[r] -radius[[i_fine]][3], mid_point$k2[r] +radius[[i_fine]][3], by = precision[[i_fine]][3])
      stage_k2s_grid = seq(mid_point$k2s[r]-radius[[i_fine]][4], mid_point$k2s[r]+radius[[i_fine]][4], by = precision[[i_fine]][4])
      stage_grid = rbind(stage_grid,
                         expand.grid(k0  = stage_k0_grid,
                                     k1  = stage_k1_grid,
                                     k2  = stage_k2_grid,
                                     k2s = stage_k2s_grid,
                                     k3  = k3_calibrated,
                                     k4  = NA,
                                     k4s = NA
                         ))
      stage_grid = unique(round(stage_grid,2)) %>% filter(k0  >= hard_coded_lower_limits[1] &
                                                            k1  >= hard_coded_lower_limits[2] &
                                                            k2  >= hard_coded_lower_limits[3] &
                                                            k2s >= hard_coded_lower_limits[4] &
                                                            k0  <= hard_coded_upper_limits[1] &
                                                            k1  <= hard_coded_upper_limits[2] &
                                                            k2  <= hard_coded_upper_limits[3] &
                                                            k2s <= hard_coded_upper_limits[4]
      )
      cat("Stage grid size:", nrow(stage_grid), "\n")
      gridminmax <- cbind(colMin(stage_grid), colMax(stage_grid))
      cat("gridminmax: \n")
      print(gridminmax)
      out_pis_done <- out_pis_full %>% inner_join(stage_grid)
      stage_grid <- anti_join(stage_grid, out_pis_full)
      cat("Stage grid size effective:", nrow(stage_grid), "\n")

      if(nrow(stage_grid)>0){
        out_pis <- get_pis_grid(
          stage_grid_full = stage_grid,
          sequential_step = min(nrow(stage_grid), sequential_step),
          brand_value = brand_value_calibrated,
          cost_factors = cost_factors_calibrated,
          weights_supplied = weights_combined
        )
      } else {
        out_pis <- data.frame()
      }
      out_pis_full = rbind.data.frame(out_pis_full, out_pis)

      if(nrow(out_pis_done) > 0){
        out_pis <- rbind(out_pis, out_pis_done)
      }

      equi <- find_equilibria(out_pis, firm1_params, firm2_params)
      # Check if equilibrium is on grid boundary (interior solutions preferred)
      equi_hit_minmax_local <-
        (equi$k0 %in% gridminmax[1,]) & !(equi$k0 %in% c(hard_coded_lower_limits[1], hard_coded_upper_limits[1])) |
        (equi$k1 %in% gridminmax[2,]) & !(equi$k1 %in% c(hard_coded_lower_limits[2], hard_coded_upper_limits[2])) |
        (equi$k2 %in% gridminmax[3,]) & !(equi$k2 %in% c(hard_coded_lower_limits[3], hard_coded_upper_limits[3])) |
        (equi$k2s%in% gridminmax[4,]) & !(equi$k2s%in% c(hard_coded_lower_limits[4], hard_coded_upper_limits[4]))

      if(!(ncol(out_pis_fine) == ncol(out_pis))){
        out_pis_fine <- data.frame()
        for(z in 5:(i-1)){
          out_pis_fine <- rbind.data.frame(out_pis_fine, out_pis_list[[z]])
        }
      }
      out_pis_fine = rbind.data.frame(out_pis_fine, out_pis)

      out_pis_list[[i]] = rbind.data.frame(out_pis_list[[i]], out_pis)
      equi_tmp = rbind.data.frame(equi_tmp, equi)
      equi_hit_minmax_tmp = c(equi_hit_minmax_tmp, equi_hit_minmax_local)
    } #close r loop over rows of previous equilirbia

    dup_ind = duplicated(equi_tmp)
    equi_tmp = equi_tmp[!dup_ind,]
    equi_hit_minmax_tmp = equi_hit_minmax_tmp[!dup_ind]

    dup_ind = duplicated(out_pis_fine)
    out_pis_fine = out_pis_fine[!dup_ind,]
    equi_global <- find_equilibria(out_pis_fine, firm1_params, firm2_params)

    if(sum(!equi_hit_minmax_tmp) > 0){
      equi <- equi_tmp[!equi_hit_minmax_tmp,]
      equi <- equi %>% inner_join(equi_global)
      if(nrow(equi) > 0){
        print(equi)
        success_ind = T
      } else {
        equi <- unique(rbind(equi_tmp, equi_global)) %>% arrange(desc(pi))
      }
    } else {
      equi <- equi_global
      equi_hit_minmax <- rep(T, nrow(equi))
    }

    equi_list[[i]] = equi

    if(!success_ind){
      cat("cannot find interior solution at step", i, "for prior equilibrium number", r,"\n")
      print(equi)
      if(i > 50){
        break ##stop if we have searched for 20 times
      }
    }


    cat("end step", i, "\n")
  }

  index = which.max(equi$pi)
  k0_part_opt <- equi$k0[index]
  k1_part_opt <- equi$k1[index]
  k2_part_opt <- equi$k2[index]
  k2s_part_opt <-equi$k2s[index]
  pi_part_opt <- equi$pi[index]
  pi_oo_part_opt <- equi$pi_oo[index]
  cat("Best K0 PART OPT:", k0_part_opt, "\n")
  cat("Best K1 PART OPT:", k1_part_opt, "\n")
  cat("Best K2 PART OPT:", k2_part_opt, "\n")
  cat("Best K2s PART OPT:", k2s_part_opt, "\n")

  out_opt_part <- list(out_pis_list = out_pis_list,
                       equi_list = equi_list,
                       config = list(
                         initial_points= initial_points,
                         precision = precision,
                         radius = radius,
                         max_row = max_row
                       ),
                       k0_part_opt = k0_part_opt,
                       k1_part_opt = k1_part_opt,
                       k2_part_opt = k2_part_opt,
                       k2s_part_opt = k2s_part_opt,
                       pi_part_opt = pi_part_opt,
                       pi_oo_part_opt = pi_oo_part_opt
  )
  saveRDS(out_opt_part, part_opt_save_filename)
}

##################
#### GEN EQUI ####
##################
if(USE_CACHE && file.exists(opt_save_filename)){
  out_opt <- readRDS(opt_save_filename)
  k0_opt <- out_opt$k0_opt
  k1_opt <- out_opt$k1_opt
  k2_opt <- out_opt$k2_opt
  k2s_opt <- out_opt$k2s_opt
  k3_opt <- out_opt$k3_opt
  out_pis_fine <- out_opt$out_pis_fine
  out_pis_full <- out_opt$out_pis_full
  cat("Best K0 OPT:",  k0_opt, "\n")
  cat("Best K1 OPT:",  k1_opt, "\n")
  cat("Best K2 OPT:",  k2_opt, "\n")
  cat("Best K2s OPT:", k2s_opt, "\n")
  cat("Best K3 OPT:",  k3_opt, "\n")
} else {

  # list all matching out_pis_file
  out_pis_files <- list.files(
    path = bootstrap_ctf_dir, # ctf_oo_option and thus ctf_dir are for tm cost, mkt shares, and EU horizons, all of which affect the value of out_pis_full
    pattern = paste0("^ctf_opt-id-", bootstrap_id,
                     ".*\\.rds$"), #the equilibrium concept or grid constraints do not affect the value of out_pis_full
    full.names = TRUE
  )

  if(length(out_pis_files)>0){
    out_pis_full <- data.frame()
    for(obj in out_pis_files){
      out_opt <- readRDS(obj)
      for(r in 1:length(out_opt$out_pis_list)){
        out_pis_full <- rbind.data.frame(out_pis_full, out_opt$out_pis_list[[r]])
      }
    }

    cat("loading existing out_pis_full with dim ", dim(out_pis_full), "\n")
  }

  initial_points <- tibble(k0 = k0_part_opt,
                           k1 = k1_part_opt,
                           k2 = k2_part_opt,
                           k2s= k2s_part_opt,
                           k3 = k3_calibrated)
  if (exists("CTF_COARSE_GRID") && CTF_COARSE_GRID) {
    precision <- list(c(0.10, 0.40, 0.50, 0.50, 0.10))
    radius    <- list(c(0.20, 1.00, 2.00, 2.00, 0.20))
  } else {
    precision <- list(
      c(0.05, 0.30, 0.50, 0.50, 0.05),
      c(0.05, 0.10, 0.20, 0.20, 0.05),
      c(0.02, 0.10, 0.10, 0.10, 0.02),
      c(0.01, 0.05, 0.10, 0.10, 0.01)
    )
    radius <- list(
      c(0.10, 0.60, 1.50, 1.50, 0.10),
      c(0.05, 0.30, 0.40, 0.40, 0.05),
      c(0.04, 0.10, 0.20, 0.20, 0.04),
      c(0.03, 0.10, 0.20, 0.20, 0.03)
    )
  }
  max_row_coarse = 3
  max_row_fine = 2
  max_row <- max_row_coarse

  sequential_step = if (exists("CTF_COARSE_GRID") && CTF_COARSE_GRID) 5000 else 50000
  firm1_params <- c("k0", "k1", "k2", "k2s")
  firm2_params <- c("k3") #"k4", "k4s"
  out_pis_list <- list()
  equi_list <- list()
  out_pis_fine <- data.frame()

  for(i in 1:length(precision)){
    cat("start step", i, "\n")
    if(i < 2){
      mid_point = initial_points
    } else {
      if(is.null(equi) || nrow(equi) == 0){
        cat("WARNING: No equilibria found at step", i-1, "- falling back to initial points\n")
        mid_point = initial_points
      } else {
        max_row <- if (i <= length(precision)) max_row_coarse else max_row_fine
        if(nrow(equi) > max_row){
          equi <- equi %>% arrange(desc(pi)) # + pi_oo
          equi <- equi[1:max_row,]
        }
        mid_point = equi
      }
    }

    stage_grid = matrix(nrow=0, ncol=5)
    for(r in 1:nrow(mid_point)){
      stage_k0_grid =  seq(mid_point$k0[r] -radius[[i]][1], mid_point$k0[r] +radius[[i]][1], by = precision[[i]][1])
      stage_k1_grid =  seq(mid_point$k1[r] -radius[[i]][2], mid_point$k1[r] +radius[[i]][2], by = precision[[i]][2])
      stage_k2_grid =  seq(mid_point$k2[r] -radius[[i]][3], mid_point$k2[r] +radius[[i]][3], by = precision[[i]][3])
      stage_k2s_grid = seq(mid_point$k2s[r]-radius[[i]][4], mid_point$k2s[r]+radius[[i]][4], by = precision[[i]][4])
      stage_k3_grid =  seq(mid_point$k3[r] -radius[[i]][5], mid_point$k3[r] +radius[[i]][5], by = precision[[i]][5])
      stage_grid = rbind(stage_grid,
                         expand.grid(k0  = stage_k0_grid,
                                     k1  = stage_k1_grid,
                                     k2  = stage_k2_grid,
                                     k2s  = stage_k2s_grid,
                                     k3  = stage_k3_grid,
                                     k4  = NA,
                                     k4s = NA
                         ))
    }
    stage_grid = unique(round(stage_grid,2)) %>% filter(k0  >= hard_coded_lower_limits[1] &
                                                          k1  >= hard_coded_lower_limits[2] &
                                                          k2  >= hard_coded_lower_limits[3] &
                                                          k2s >= hard_coded_lower_limits[4] &
                                                          k3  >= hard_coded_lower_limits[5] &
                                                          k0  <= hard_coded_upper_limits[1] &
                                                          k1  <= hard_coded_upper_limits[2] &
                                                          k2  <= hard_coded_upper_limits[3] &
                                                          k2s <= hard_coded_upper_limits[4] &
                                                          k3  <= hard_coded_upper_limits[5]
    )
    cat("Stage grid size:", nrow(stage_grid), "\n")
    gridminmax <- cbind(colMin(stage_grid), colMax(stage_grid))
    cat("gridminmax: \n")
    print(gridminmax)
    out_pis_done <- out_pis_full %>% inner_join(stage_grid)
    stage_grid <- anti_join(stage_grid, out_pis_full)
    cat("Stage grid size effective:", nrow(stage_grid), "\n")

    if(nrow(stage_grid)>0){
      out_pis <- get_pis_grid(
        stage_grid_full = stage_grid,
        sequential_step = min(nrow(stage_grid), sequential_step),
        brand_value = brand_value_calibrated,
        cost_factors = cost_factors_calibrated,
        weights_supplied = weights_combined
      )
    } else {
      out_pis <- data.frame()
    }
    out_pis_full = rbind.data.frame(out_pis_full, out_pis)

    if(nrow(out_pis_done) > 0){
      out_pis <- rbind(out_pis, out_pis_done)
    }

    equi <- find_equilibria(out_pis, firm1_params, firm2_params)
    if(nrow(equi) < 1){
      equi <- find_equilibria(out_pis_full, firm1_params, firm2_params)
      if(nrow(equi) < 1){
        # No PSNE found; detect cycles in best-response dynamics
        cycle_ind = T
        equi_cycle <- get_equi_cycle(out_pis_full, firm1_params, firm2_params)
        equi <- equi_cycle$cycles
      }
    }

    out_pis_list[[i]] = out_pis
    equi_list[[i]] = equi
    print(equi_list[[i]])
    cat("end step", i, "\n")
  }

  i_fine = i
  out_pis_fine = out_pis_list[[i_fine]]
  dup_ind = duplicated(out_pis_fine)
  out_pis_fine = out_pis_fine[!dup_ind,]

  dup_ind = duplicated(equi_list[[i]])
  equi_list[[i]] = equi_list[[i]][!dup_ind,]

  success_ind = F
  equi_hit_minmax = rep(T, nrow(equi_list[[i]]))

  #### checking whether the equilibria found are interior to our finer search ####
  if (exists("TESTING") && TESTING) {
    cat("TESTING mode: accepting grid equilibria without interior check\n")
    success_ind <- TRUE
    cycle_ind <- FALSE
  }
  while(success_ind<1){
    i = i + 1
    cycle_ind = F
    cat("start step", i, "\n")
    mid_point <- equi_list[[i-1]] #[equi_hit_minmax,]
    mid_point <- mid_point %>% filter(pi_oo>0) %>%
      mutate(rank1 = rank(-pi),
             rank2 = rank(-pi_oo),
             rank3 = rank(-pi-pi_oo),
             rank = pmin(rank1, rank2, rank3)) %>% filter(rank < 4)

    stage_grid = matrix(nrow=0, ncol=4)
    out_pis_list[[i]] <- data.frame()
    equi_list[[i]] <- data.frame()
    equi_tmp = data.frame()
    equi_hit_minmax_tmp = c()
    for(r in 1:nrow(mid_point)){
      stage_k0_grid =  seq(mid_point$k0[r] -radius[[i_fine]][1], mid_point$k0[r] +radius[[i_fine]][1], by = precision[[i_fine]][1])
      stage_k1_grid =  seq(mid_point$k1[r] -radius[[i_fine]][2], mid_point$k1[r] +radius[[i_fine]][2], by = precision[[i_fine]][2])
      stage_k2_grid =  seq(mid_point$k2[r] -radius[[i_fine]][3], mid_point$k2[r] +radius[[i_fine]][3], by = precision[[i_fine]][3])
      stage_k2s_grid = seq(mid_point$k2s[r]-radius[[i_fine]][4], mid_point$k2s[r]+radius[[i_fine]][4], by = precision[[i_fine]][4])
      stage_k3_grid =  seq(mid_point$k3[r] -radius[[i_fine]][5], mid_point$k3[r] +radius[[i_fine]][5], by = precision[[i_fine]][5])
      stage_grid = rbind(stage_grid,
                         expand.grid(k0  = stage_k0_grid,
                                     k1  = stage_k1_grid,
                                     k2  = stage_k2_grid,
                                     k2s = stage_k2s_grid,
                                     k3  = stage_k3_grid,
                                     k4  = NA,
                                     k4s = NA
                         ))
      stage_grid = unique(round(stage_grid,2)) %>% filter(k0  >= hard_coded_lower_limits[1] &
                                                            k1  >= hard_coded_lower_limits[2] &
                                                            k2  >= hard_coded_lower_limits[3] &
                                                            k2s >= hard_coded_lower_limits[4] &
                                                            k3  >= hard_coded_lower_limits[5] &
                                                            k0  <= hard_coded_upper_limits[1] &
                                                            k1  <= hard_coded_upper_limits[2] &
                                                            k2  <= hard_coded_upper_limits[3] &
                                                            k2s <= hard_coded_upper_limits[4] &
                                                            k3  <= hard_coded_upper_limits[5]
      )
      cat("Stage grid size:", nrow(stage_grid), "\n")
      gridminmax <- cbind(colMin(stage_grid), colMax(stage_grid))
      cat("gridminmax: \n")
      print(gridminmax)
      out_pis_done <- out_pis_full %>% inner_join(stage_grid)
      stage_grid <- anti_join(stage_grid, out_pis_full)
      cat("Stage grid size effective:", nrow(stage_grid), "\n")

      if(nrow(stage_grid)>0){
        out_pis <- get_pis_grid(
          stage_grid_full = stage_grid,
          sequential_step = min(nrow(stage_grid), sequential_step),
          brand_value = brand_value_calibrated,
          cost_factors = cost_factors_calibrated,
          weights_supplied = weights_combined
        )
      } else {
        out_pis <- data.frame()
      }
      out_pis_full = rbind.data.frame(out_pis_full, out_pis)

      if(nrow(out_pis_done) > 0){
        out_pis <- rbind(out_pis, out_pis_done)
      }

      equi <- find_equilibria(out_pis, firm1_params, firm2_params)
      # Check if equilibrium is on grid boundary (interior solutions preferred)
      equi_hit_minmax_local <-
        (equi$k0 %in% gridminmax[1,]) & !(equi$k0 %in% c(hard_coded_lower_limits[1], hard_coded_upper_limits[1])) |
        (equi$k1 %in% gridminmax[2,]) & !(equi$k1 %in% c(hard_coded_lower_limits[2], hard_coded_upper_limits[2])) |
        (equi$k2 %in% gridminmax[3,]) & !(equi$k2 %in% c(hard_coded_lower_limits[3], hard_coded_upper_limits[3])) |
        (equi$k2s%in% gridminmax[4,]) & !(equi$k2s%in% c(hard_coded_lower_limits[4], hard_coded_upper_limits[4])) |
        (equi$k3 %in% gridminmax[5,]) & !(equi$k3 %in% c(hard_coded_lower_limits[5], hard_coded_upper_limits[5]))

      if(!(ncol(out_pis_fine) == ncol(out_pis))){
        out_pis_fine <- data.frame()
        for(z in 5:(i-1)){
          out_pis_fine <- rbind.data.frame(out_pis_fine, out_pis_list[[z]])
        }
      }
      out_pis_fine = rbind.data.frame(out_pis_fine, out_pis)

      out_pis_list[[i]] = rbind.data.frame(out_pis_list[[i]], out_pis)
      equi_tmp = rbind.data.frame(equi_tmp, equi)
      equi_hit_minmax_tmp = c(equi_hit_minmax_tmp, equi_hit_minmax_local)
    } #close r loop over rows of previous equilirbia

    dup_ind = duplicated(equi_tmp)
    equi_tmp = equi_tmp[!dup_ind,]
    equi_hit_minmax_tmp = equi_hit_minmax_tmp[!dup_ind]
    dup_ind = duplicated(out_pis_fine)
    out_pis_fine = out_pis_fine[!dup_ind,]
    if(nrow(equi_tmp) < 1){
      # No PSNE found; detect cycles in best-response dynamics
      cycle_ind = T
      equi_cycle <- get_equi_cycle(out_pis_fine, firm1_params, firm2_params)
      equi_tmp <- equi_cycle$cycles
      equi_global <- equi_tmp
      equi_hit_minmax_tmp <-
        (equi$k0 %in% gridminmax[1,]) & !(equi$k0 %in% c(hard_coded_lower_limits[1], hard_coded_upper_limits[1])) |
        (equi$k1 %in% gridminmax[2,]) & !(equi$k1 %in% c(hard_coded_lower_limits[2], hard_coded_upper_limits[2])) |
        (equi$k2 %in% gridminmax[3,]) & !(equi$k2 %in% c(hard_coded_lower_limits[3], hard_coded_upper_limits[3])) |
        (equi$k2s%in% gridminmax[4,]) & !(equi$k2s%in% c(hard_coded_lower_limits[4], hard_coded_upper_limits[4])) |
        (equi$k3 %in% gridminmax[5,]) & !(equi$k3 %in% c(hard_coded_lower_limits[5], hard_coded_upper_limits[5]))
    } else {
      equi_global <- find_equilibria(out_pis_fine, firm1_params, firm2_params)
    }

    if(cycle_ind){
      success_ind = !(sum(equi_hit_minmax_tmp)>0) # all equilibria are interior
    } else {
      success_ind = sum(!equi_hit_minmax_tmp)>0 # at least one is interior
    }

    if(success_ind){
      equi <- equi_tmp[!equi_hit_minmax_tmp,]
      equi <- unique(equi)
      equi <- equi %>% inner_join(equi_global)
    } else {
      cat("cannot find interior solution at step", i, "for prior equilibrium number", r,"\n")
      tmp <- cbind.data.frame(equi_tmp, equi_hit_minmax_tmp)
      equi <- tmp %>% inner_join(equi_global)
      equi_hit_minmax <- equi$equi_hit_minmax_tmp
      equi$equi_hit_minmax_tmp <- NULL
      if(i > 20){
        break ##stop if we have searched for 20 times
      }
    }

    if(nrow(equi) < 1){
      success_ind = F
      equi <- rbind.data.frame(equi_tmp[equi_hit_minmax_tmp,], equi_global)
      equi_hit_minmax <- rep(T, nrow(equi))
    }
    print(equi)
    print(equi_hit_minmax)

    equi_list[[i]] = equi

    cat("end step", i, "\n")
  }

  index = which.max(equi$pi) # + equi$pi_oo
  k0_opt <- equi$k0[index]
  k1_opt <- equi$k1[index]
  k2_opt <- equi$k2[index]
  k2s_opt <-equi$k2s[index]
  k3_opt <-equi$k3[index]
  pi_opt <- equi$pi[index]
  pi_oo_opt <- equi$pi_oo[index]
  cat("Best K0:",  k0_opt, "\n")
  cat("Best K1:",  k1_opt, "\n")
  cat("Best K2:",  k2_opt, "\n")
  cat("Best K2s:", k2s_opt, "\n")
  cat("Best K3:",  k3_opt, "\n")

  out_opt <- list(out_pis_list = out_pis_list,
                  equi_list = equi_list,
                  out_pis_fine = out_pis_fine,
                  out_pis_full = out_pis_full,
                  config = list(
                    initial_points= initial_points,
                    precision = precision,
                    radius = radius,
                    max_row = max_row
                  ),
                  k0_opt = k0_opt,
                  k1_opt = k1_opt,
                  k2_opt = k2_opt,
                  k2s_opt = k2s_opt,
                  k3_opt = k3_opt,
                  pi_opt = pi_opt,
                  pi_oo_opt = pi_oo_opt
  )
  saveRDS(out_opt, opt_save_filename)
}

####################
#### DATA SHARE ####
####################

for(oo_disc_floor in c(F,T)){

  if(oo_disc_floor){ # we only use it here
    hard_coded_lower_limits[3] <- 1
    hard_coded_lower_limits[6] <- 1
  }

  ds_save_filename <- file.path(bootstrap_ctf_dir, paste0("ctf_ds-id-", bootstrap_id, paste0(ctf_config_full, ifelse(oo_disc_floor,"_disc_floor","")),".rds"))

  # Load DS progress file if available (resume from crash)
  ds_progress_filename <- file.path(bootstrap_ctf_dir, paste0("ctf_ds_progress-id-", bootstrap_id,
                                    ifelse(oo_disc_floor, "_disc_floor", ""), ".rds"))
  if(USE_CACHE && file.exists(ds_progress_filename) && !file.exists(ds_save_filename)){
    cat("  Resuming DS from progress file:", ds_progress_filename, "\n")
    ds_progress <- readRDS(ds_progress_filename)
    out_pis_full <- ds_progress$out_pis_full
    out_pis_list <- ds_progress$out_pis_list
    equi_list <- ds_progress$equi_list
    cat("  Loaded", nrow(out_pis_full), "previously computed grid points\n")
  }

  if(USE_CACHE && file.exists(ds_save_filename)){
    if(oo_disc_floor){
      out_ds_df <- readRDS(ds_save_filename)
      k0_ds_df =      out_ds_df$k0_ds_df
      k1_ds_df =      out_ds_df$k1_ds_df
      k2_ds_df =      out_ds_df$k2_ds_df
      k2s_ds_df=      out_ds_df$k2s_ds_df
      k3_ds_df =      out_ds_df$k3_ds_df
      k4_ds_df =      out_ds_df$k4_ds_df
      k4s_ds_df=      out_ds_df$k4s_ds_df
      out_pis_fine <- out_ds_df$out_pis_fine
      out_pis_full <- out_ds_df$out_pis_full
      cat("Best K0 DS DF:",  k0_ds_df, "\n")
      cat("Best K1 DS DF:",  k1_ds_df, "\n")
      cat("Best K2 DS DF:",  k2_ds_df, "\n")
      cat("Best K2s DS DF:", k2s_ds_df, "\n")
      cat("Best K3 DS DF:",  k3_ds_df, "\n")
      cat("Best K4 DS DF:",  k4_ds_df, "\n")
      cat("Best K4s DS DF:", k4s_ds_df, "\n")
    } else {
      out_ds <- readRDS(ds_save_filename)
      k0_ds = out_ds$k0_ds
      k1_ds = out_ds$k1_ds
      k2_ds = out_ds$k2_ds
      k2s_ds= out_ds$k2s_ds
      k3_ds = out_ds$k3_ds
      k4_ds = out_ds$k4_ds
      k4s_ds= out_ds$k4s_ds
      out_pis_fine <- out_ds$out_pis_fine
      out_pis_full <- out_ds$out_pis_full
      cat("Best K0 DS:",  k0_ds, "\n")
      cat("Best K1 DS:",  k1_ds, "\n")
      cat("Best K2 DS:",  k2_ds, "\n")
      cat("Best K2s DS:", k2s_ds, "\n")
      cat("Best K3 DS:",  k3_ds, "\n")
      cat("Best K4 DS:",  k4_ds, "\n")
      cat("Best K4s DS:", k4s_ds, "\n")
    }

  } else {

    # list all matching out_pis_file
    out_pis_files <- list.files(
      path = bootstrap_ctf_dir, # ctf_oo_option and thus ctf_dir are for tm cost, mkt shares, and EU horizons, all of which affect the value of out_pis_full
      pattern = paste0("^ctf_ds-id-", bootstrap_id, ".*\\.rds$"),
      full.names = TRUE
    )

    if(length(out_pis_files) > 0){
      out_pis_full <- out_pis_files |>
        lapply(function(f) readRDS(f)$out_pis_full) |>
        dplyr::bind_rows() |>      # or do.call(rbind, ...)
        dplyr::distinct()
      cat("loading existing out_pis_full with dim ", dim(out_pis_full), "\n")
    }

    out_pis_full <- filter(out_pis_full, !is.na(k4))

    if(oo_constrained_toggle){
      initial_points <- tibble(k0 = c(k0_opt, k0_opt),
                               k1 = c(0.6, 0.2),
                               k2 = c(0.5, 0.7),
                               k2s= c(0.5, 0.8),
                               k3 = c(k3_opt, 1.04),
                               k4 = c(0.5, 0.0),
                               k4s= c(0.5, 1.0))

      if (exists("CTF_COARSE_GRID") && CTF_COARSE_GRID) {
        precision <- list(c(0.10, 0.40, 0.50, 0.50, 0.10, 0.50, 0.50))
        radius    <- list(c(0.10, 0.40, 0.50, 0.50, 0.10, 0.50, 0.50))
      } else {
        precision <- list(
          c(0.05, 0.30, 0.50, 0.50, 0.05, 0.50, 0.50),
          c(0.05, 0.10, 0.30, 0.30, 0.05, 0.30, 0.30),
          c(0.04, 0.05, 0.20, 0.20, 0.04, 0.20, 0.20),
          c(0.02, 0.05, 0.10, 0.10, 0.02, 0.10, 0.10),
          c(0.01, 0.05, 0.10, 0.10, 0.01, 0.10, 0.10)
        )
        radius <- list(
          c(0.10, 0.60, 2.00, 2.00, 0.10, 2.00, 2.00),
          c(0.05, 0.30, 0.60, 0.60, 0.05, 0.60, 0.60),
          c(0.04, 0.10, 0.20, 0.20, 0.04, 0.20, 0.20),
          c(0.04, 0.05, 0.20, 0.20, 0.04, 0.20, 0.20),
          c(0.02, 0.05, 0.10, 0.10, 0.02, 0.10, 0.10)
        )
      }
    } else {
      initial_points <- tibble(k0 = k0_opt,
                               k1 = 0.7,
                               k2 = 0.5,
                               k2s= 0.5,
                               k3 = ifelse(grepl("median_flex", ctf_oo_option), 1.1, k3_opt),
                               k4 = 0.0,
                               k4s= 0.5)

      if (exists("CTF_COARSE_GRID") && CTF_COARSE_GRID) {
        precision <- list(c(0.10, 0.40, 0.50, 0.50, 0.10, 0.50, 0.50))
        radius    <- list(c(0.10, 0.40, 0.50, 0.50, 0.10, 0.50, 0.50))
      } else {
        precision <- list(
          c(0.10, 0.20, 0.50, 0.50, 0.10, 0.50, 0.50),
          c(0.05, 0.10, 0.30, 0.30, 0.05, 0.30, 0.30),
          c(0.04, 0.05, 0.10, 0.10, 0.04, 0.10, 0.10),
          c(0.02, 0.05, 0.10, 0.10, 0.02, 0.10, 0.10),
          c(0.01, 0.05, 0.10, 0.10, 0.01, 0.10, 0.10)
        )
        radius <- list(
          c(0.10, 0.40, 1.00, 1.00, 0.20, 2.00, 1.50),
          c(0.05, 0.20, 0.60, 0.60, 0.05, 0.60, 0.60),
          c(0.04, 0.10, 0.30, 0.30, 0.04, 0.30, 0.30),
          c(0.04, 0.05, 0.20, 0.20, 0.04, 0.20, 0.20),
          c(0.02, 0.05, 0.10, 0.10, 0.02, 0.10, 0.10)
        )
      }
    }
    params_all <- c("k0", "k1", "k2", "k2s", "k3", "k4", "k4s")
    max_row_coarse = 3
    max_row_fine = 2

    sequential_step = if (exists("CTF_COARSE_GRID") && CTF_COARSE_GRID) 5000 else 50000
    firm1_params <- c("k0", "k1", "k2", "k2s")
    firm2_params <- c("k3", "k4", "k4s")
    out_pis_list <- list()
    equi_list <- list()

    for(i in 1:length(precision)){
      cat("start step", i, "\n")
      max_row <- if (i <= length(precision)) max_row_coarse else max_row_fine
      if(i < 2){
        mid_point = initial_points
      } else {
        if(is.null(equi_list[[i-1]]) || nrow(equi_list[[i-1]]) == 0){
          cat("WARNING: No equilibria found at step", i-1, "- falling back to initial points\n")
          mid_point = initial_points
        } else if(nrow(equi_list[[i-1]]) > max_row){
          mid_point <- equi_list[[i-1]] %>%
            arrange(desc(pi)) # + pi_oo
          mid_point <- mid_point[1:max_row,]
        } else {
          mid_point <- equi_list[[i-1]]
        }
      }

      stage_grid = matrix(nrow=0, ncol=7)
      for(r in 1:nrow(mid_point)){
        stage_k0_grid =  seq(mid_point$k0[r] -radius[[i]][1], mid_point$k0[r] +radius[[i]][1], by = precision[[i]][1])
        stage_k1_grid =  seq(mid_point$k1[r] -radius[[i]][2], mid_point$k1[r] +radius[[i]][2], by = precision[[i]][2])
        stage_k2_grid =  seq(mid_point$k2[r] -radius[[i]][3], mid_point$k2[r] +radius[[i]][3], by = precision[[i]][3])
        stage_k2s_grid = seq(mid_point$k2s[r]-radius[[i]][4], mid_point$k2s[r]+radius[[i]][4], by = precision[[i]][4])
        stage_k3_grid =  seq(mid_point$k3[r] -radius[[i]][5], mid_point$k3[r] +radius[[i]][5], by = precision[[i]][5])
        stage_k4_grid =  seq(mid_point$k4[r] -radius[[i]][6], mid_point$k4[r] +radius[[i]][6], by = precision[[i]][6])
        stage_k4s_grid = seq(mid_point$k4s[r]-radius[[i]][7], mid_point$k4s[r]+radius[[i]][7], by = precision[[i]][7])
        stage_grid = rbind(stage_grid,
                           expand.grid(k0  = stage_k0_grid,
                                       k1  = stage_k1_grid,
                                       k2  = stage_k2_grid,
                                       k2s = stage_k2s_grid,
                                       k3  = stage_k3_grid,
                                       k4  = stage_k4_grid,
                                       k4s = stage_k4s_grid
                           ))
      }
      # Clamp k4s upper limit: surcharge cannot exceed baseline (k4s <= 1)
      stage_grid = unique(round(stage_grid,2)) %>% filter(k0  >= hard_coded_lower_limits[1] &
                                                          k1  >= hard_coded_lower_limits[2] &
                                                          k2  >= hard_coded_lower_limits[3] &
                                                          k2s >= hard_coded_lower_limits[4] &
                                                          k3  >= hard_coded_lower_limits[5] &
                                                          k4  >= hard_coded_lower_limits[6] &
                                                          k4s >= hard_coded_lower_limits[7] &
                                                          k0  <= hard_coded_upper_limits[1] &
                                                          k1  <= hard_coded_upper_limits[2] &
                                                          k2  <= hard_coded_upper_limits[3] &
                                                          k2s <= hard_coded_upper_limits[4] &
                                                          k3  <= hard_coded_upper_limits[5] &
                                                          k4  <= hard_coded_upper_limits[6] &
                                                          k4s <= hard_coded_upper_limits[7]
                                                          )
      cat("Stage grid size:", nrow(stage_grid), "\n")
      gridminmax <- cbind(colMin(stage_grid), colMax(stage_grid))
      cat("gridminmax: \n")
      print(gridminmax)
      if(nrow(out_pis_full) > 0){
        out_pis_done <- out_pis_full %>% inner_join(stage_grid)
        stage_grid <- anti_join(stage_grid, out_pis_full)
        cat("Stage grid size effective:", nrow(stage_grid), "\n")
      } else {
        out_pis_done <- data.frame()
      }

      if(nrow(stage_grid) > 0){
        chunk_size = 50000
        if (nrow(stage_grid) > chunk_size) {
          num_chunks <- ceiling(nrow(stage_grid) / chunk_size)
          out_pis <- data.frame()
          # Loop through each chunk
          for (j in 1:num_chunks) {
            # Determine the start and end rows for the current chunk
            start_row <- (j - 1) * chunk_size + 1
            end_row <- min(j * chunk_size, nrow(stage_grid))
            current_chunk <- stage_grid[start_row:end_row, ]

            out_pis_current <- get_pis_grid(
              stage_grid_full = current_chunk,
              sequential_step = min(nrow(current_chunk), sequential_step),
              brand_value = brand_value_calibrated,
              cost_factors = cost_factors_calibrated,
              weights_supplied = weights_combined
            )

            out_pis <- rbind.data.frame(out_pis, out_pis_current)
            out_pis_full = rbind.data.frame(out_pis_full, out_pis_current)
            saveRDS(list(out_pis_full = out_pis_full, out_pis_list = out_pis_list,
                         equi_list = equi_list, step = i, chunk = j),
                    file.path(bootstrap_ctf_dir, paste0("ctf_ds_progress-id-", bootstrap_id,
                              ifelse(oo_disc_floor, "_disc_floor", ""), ".rds")))
            print(paste("Processed and saved chunk", j, "of", num_chunks))
          }
        } else {
          # If the data is small enough, run the function on the entire data frame
          out_pis <- get_pis_grid(
            stage_grid_full = stage_grid,
            sequential_step = min(nrow(stage_grid), sequential_step),
            brand_value = brand_value_calibrated,
            cost_factors = cost_factors_calibrated,
            weights_supplied = weights_combined
          )
          out_pis_full = rbind.data.frame(out_pis_full, out_pis)
        }

        # Save progress after stage
        saveRDS(list(out_pis_full = out_pis_full, out_pis_list = out_pis_list,
                     equi_list = equi_list, step = i),
                file.path(bootstrap_ctf_dir, paste0("ctf_ds_progress-id-", bootstrap_id,
                          ifelse(oo_disc_floor, "_disc_floor", ""), ".rds")))

        if(nrow(out_pis_done) > 0){
          out_pis <- rbind(out_pis, out_pis_done)
        }

      } else {
        out_pis <- out_pis_done
      }

      dup_ind = duplicated(out_pis)
      out_pis = out_pis[!dup_ind,]
      if (nrow(out_pis) == 0) out_pis <- out_pis_full
      equi <- find_equilibria(out_pis, firm1_params, firm2_params)
      if(nrow(equi) < 1){
        equi <- find_equilibria(out_pis_full, firm1_params, firm2_params)
        if(nrow(equi) < 1){
          # No PSNE found; detect cycles in best-response dynamics
          cycle_ind = T
          equi_cycle <- get_equi_cycle(out_pis_full, firm1_params, firm2_params)
          equi <- equi_cycle$cycles
        }
      }

      # Check if equilibrium is on grid boundary (interior solutions preferred)
      equi_hit_minmax <- (equi$k0 %in% gridminmax[1,]) & !(equi$k0 %in% c(hard_coded_lower_limits[1], hard_coded_upper_limits[1])) |
                         (equi$k1 %in% gridminmax[2,]) & !(equi$k1 %in% c(hard_coded_lower_limits[2], hard_coded_upper_limits[2])) |
                         (equi$k2 %in% gridminmax[3,]) & !(equi$k2 %in% c(hard_coded_lower_limits[3], hard_coded_upper_limits[3])) |
                         (equi$k2s%in% gridminmax[4,]) & !(equi$k2s%in% c(hard_coded_lower_limits[4], hard_coded_upper_limits[4])) |
                         (equi$k3 %in% gridminmax[5,]) & !(equi$k3 %in% c(hard_coded_lower_limits[5], hard_coded_upper_limits[5])) |
                         (equi$k4 %in% gridminmax[6,]) & !(equi$k4 %in% c(hard_coded_lower_limits[6], hard_coded_upper_limits[6])) |
                         (equi$k4s%in% gridminmax[7,]) & !(equi$k4s%in% c(hard_coded_lower_limits[7], hard_coded_upper_limits[7])) #c(colMin(out_pis)[7]) ## hardcode
      if(sum(!equi_hit_minmax)>0){
        equi <- equi[!equi_hit_minmax,]
      } else {
        equi_hit_minmax <-
            (equi$k2 %in% gridminmax[3,]  +
             equi$k2s %in% gridminmax[4,] +
             equi$k4 %in%  gridminmax[6,] +
             equi$k4s %in% c(colMin(out_pis)[7]) ## hardcode
             ) > 0
        if(sum(!equi_hit_minmax)>0){
          equi <- equi[!equi_hit_minmax,]
        }
      }
      out_pis_list[[i]] = out_pis
      equi_list[[i]] = equi
      print(equi_list[[i]])

      cat("end step", i, "\n")
    }

    i_fine = i
    out_pis_fine = out_pis_list[[i_fine]]

    success_ind = F
    equi_hit_minmax = rep(T, nrow(equi_list[[i]]))
    # Clamp k4s upper limit: surcharge cannot exceed baseline (k4s <= 1)
    if(min(equi_list[[i]]$k4s) == hard_coded_upper_limits[7]){
      hard_coded_lower_limits[7] = hard_coded_upper_limits[7] #no longer need to search for k4s
    }

    #### checking whether the equilibria found are interior to our finer search ####
    if (exists("TESTING") && TESTING) {
      cat("TESTING mode: accepting grid equilibria without interior check\n")
      success_ind <- TRUE
      cycle_ind <- FALSE
    }
    while(success_ind<1){
      i = i + 1
      cycle_ind = F
      cat("start step", i, "\n")
      mid_point <- equi_list[[i-1]]#[equi_hit_minmax,]
      mid_point <- mid_point %>% filter(pi_oo>0) %>%
        mutate(rank1 = rank(-pi),
               rank2 = rank(-pi_oo),
               rank3 = rank(-pi-pi_oo),
               rank = pmin(rank1, rank2, rank3)) %>% filter(rank < 3)


      stage_grid = matrix(nrow=0, ncol=7)
      out_pis_list[[i]] <- data.frame()
      equi_list[[i]] <- data.frame()
      equi_hit_limit_index <- c()
      for(r in 1:nrow(mid_point)){
        stage_k0_grid =  seq(mid_point$k0[r] -radius[[i_fine]][1], mid_point$k0[r] +radius[[i_fine]][1], by = precision[[i_fine]][1])
        stage_k1_grid =  seq(mid_point$k1[r] -radius[[i_fine]][2], mid_point$k1[r] +radius[[i_fine]][2], by = precision[[i_fine]][2])
        stage_k2_grid =  seq(mid_point$k2[r] -radius[[i_fine]][3], mid_point$k2[r] +radius[[i_fine]][3], by = precision[[i_fine]][3])
        stage_k2s_grid = seq(mid_point$k2s[r]-radius[[i_fine]][4], mid_point$k2s[r]+radius[[i_fine]][4], by = precision[[i_fine]][4])
        stage_k3_grid =  seq(mid_point$k3[r] -radius[[i_fine]][5], mid_point$k3[r] +radius[[i_fine]][5], by = precision[[i_fine]][5])
        stage_k4_grid =  seq(mid_point$k4[r] -radius[[i_fine]][6], mid_point$k4[r] +radius[[i_fine]][6], by = precision[[i_fine]][6])
        stage_k4s_grid = seq(mid_point$k4s[r]-radius[[i_fine]][7], mid_point$k4s[r]+radius[[i_fine]][7], by = precision[[i_fine]][7])
        stage_grid = rbind(stage_grid,
                           expand.grid(k0  = stage_k0_grid,
                                       k1  = stage_k1_grid,
                                       k2  = stage_k2_grid,
                                       k2s = stage_k2s_grid,
                                       k3  = stage_k3_grid,
                                       k4  = stage_k4_grid,
                                       k4s = stage_k4s_grid
                           ))
        stage_grid = unique(round(stage_grid,2)) %>% filter(k0  >= hard_coded_lower_limits[1] &
                                                            k1  >= hard_coded_lower_limits[2] &
                                                            k2  >= hard_coded_lower_limits[3] &
                                                            k2s >= hard_coded_lower_limits[4] &
                                                            k3  >= hard_coded_lower_limits[5] &
                                                            k4  >= hard_coded_lower_limits[6] &
                                                            k4s >= hard_coded_lower_limits[7] &
                                                            k0  <= hard_coded_upper_limits[1] &
                                                            k1  <= hard_coded_upper_limits[2] &
                                                            k2  <= hard_coded_upper_limits[3] &
                                                            k2s <= hard_coded_upper_limits[4] &
                                                            k3  <= hard_coded_upper_limits[5] &
                                                            k4  <= hard_coded_upper_limits[6] &
                                                            k4s <= hard_coded_upper_limits[7]
        )
        cat("Stage grid size:", nrow(stage_grid), "\n")
        gridminmax <- cbind(colMin(stage_grid[params_all]), colMax(stage_grid[params_all]))
        cat("gridminmax: \n")
        print(gridminmax)
        out_pis_done <- out_pis_full %>% inner_join(stage_grid)
        stage_grid <- anti_join(stage_grid, out_pis_full)
        cat("Stage grid size effective:", nrow(stage_grid), "\n")

        if(nrow(stage_grid)>0){
          out_pis <- get_pis_grid(
            stage_grid_full = stage_grid,
            sequential_step = min(nrow(stage_grid), sequential_step),
            brand_value = brand_value_calibrated,
            cost_factors = cost_factors_calibrated,
            weights_supplied = weights_combined
          )
        } else {
          out_pis <- data.frame()
        }
        out_pis_full = rbind.data.frame(out_pis_full, out_pis)

        if(nrow(out_pis_done) > 0){
          out_pis <- rbind(out_pis, out_pis_done)
        }

        if(i > 4){ #entering into the finest grid search
          if(!(ncol(out_pis_fine) == ncol(out_pis))){
            out_pis_fine <- data.frame()
            for(z in 5:(i-1)){
              out_pis_fine <- rbind.data.frame(out_pis_fine, out_pis_list[[z]])
            }
          }
          out_pis_fine = rbind.data.frame(out_pis_fine, out_pis)
        }

        out_pis_list[[i]] = rbind.data.frame(out_pis_list[[i]], out_pis)
      } #close r loop over rows of previous equilirbia

      dup_ind = duplicated(out_pis_fine)
      out_pis_fine = out_pis_fine[!dup_ind,]
      if (nrow(out_pis) == 0) out_pis <- out_pis_full
      equi <- find_equilibria(out_pis, firm1_params, firm2_params)
      if(nrow(equi) < 1){
        equi <- find_equilibria(out_pis_full, firm1_params, firm2_params)
      }
      ## if there is no pure strategy equi, most likely stuck in a cycle
      if(nrow(equi) < 1){
        # No PSNE found; detect cycles in best-response dynamics
        cycle_ind = T
        equi_cycle <- get_equi_cycle(out_pis_fine, firm1_params, firm2_params)
        equi <- equi_cycle$cycles
      }

      # Check if equilibrium is on grid boundary (interior solutions preferred)
      equi_hit_minmax <- (equi$k0 %in% gridminmax[1,]) & !(equi$k0 %in% c(hard_coded_lower_limits[1], hard_coded_upper_limits[1])) |
        (equi$k1 %in% gridminmax[2,]) & !(equi$k1 %in% c(hard_coded_lower_limits[2], hard_coded_upper_limits[2])) |
        (equi$k2 %in% gridminmax[3,]) & !(equi$k2 %in% c(hard_coded_lower_limits[3], hard_coded_upper_limits[3])) |
        (equi$k2s%in% gridminmax[4,]) & !(equi$k2s%in% c(hard_coded_lower_limits[4], hard_coded_upper_limits[4])) |
        (equi$k3 %in% gridminmax[5,]) & !(equi$k3 %in% c(hard_coded_lower_limits[5], hard_coded_upper_limits[5])) |
        (equi$k4 %in% gridminmax[6,]) & !(equi$k4 %in% c(hard_coded_lower_limits[6], hard_coded_upper_limits[6])) |
        (equi$k4s%in% gridminmax[7,]) & !(equi$k4s%in% c(hard_coded_lower_limits[7], hard_coded_upper_limits[7])) #c(colMin(out_pis)[7]) ## hardcode

      if(cycle_ind){
        success_ind = !(sum(equi_hit_minmax)>0) # all equilibria are interior
      } else {
        success_ind = sum(!equi_hit_minmax)>0 # at least one is interior
      }

      if(success_ind){
        equi <- equi[!equi_hit_minmax,]
        print(equi)
      } else {
        cat("cannot find interior solution at step", i, "for prior equilibrium number", r,"\n")
        print(equi)
        if(i > 20){
          break ##stop if we have searched for 20 times
        }
      }

      equi_list[[i]] = equi

      cat("end step", i, "\n")
    }

    if(cycle_ind){## change algo here !!!##
      equi_mixed <- equi
      equi[1,] <- as.list(colMeans(equi_mixed))
      equi <- equi[1,]
      index = 1
    } else {
      index = which.max(equi$pi)# + equi$pi_oo
    }

    if(oo_disc_floor){
      k0_ds_df <- equi$k0[index]
      k1_ds_df <- equi$k1[index]
      k2_ds_df <- equi$k2[index]
      k2s_ds_df<- equi$k2s[index]
      k3_ds_df <- equi$k3[index]
      k4_ds_df <- equi$k4[index]
      k4s_ds_df<- equi$k4s[index]
      pi_ds_df <- equi$pi[index]
      pi_oo_ds_df <- equi$pi_oo[index]
      cat("Best K0 DS DF:",  k0_ds_df, "\n")
      cat("Best K1 DS DF:",  k1_ds_df, "\n")
      cat("Best K2 DS DF:",  k2_ds_df, "\n")
      cat("Best K2s DS DF:", k2s_ds_df, "\n")
      cat("Best K3 DS DF:",  k3_ds_df, "\n")
      cat("Best K4 DS DF:",  k4_ds_df, "\n")
      cat("Best K4s DS DF:", k4s_ds_df, "\n")

      out_ds_df <- list(out_pis_list = out_pis_list,
                     equi_list = equi_list,
                     out_pis_full = out_pis_full,
                     out_pis_fine = out_pis_fine,
                     config = list(
                       initial_points= initial_points,
                       precision = precision,
                       radius = radius,
                       max_row = max_row
                     ),
                     k0_ds_df = k0_ds_df,
                     k1_ds_df = k1_ds_df,
                     k2_ds_df = k2_ds_df,
                     k2s_ds_df= k2s_ds_df,
                     k3_ds_df = k3_ds_df,
                     k4_ds_df = k4_ds_df,
                     k4s_ds_df= k4s_ds_df,
                     pi_ds_df = pi_ds_df,
                     pi_oo_ds_df = pi_oo_ds_df
      )
      saveRDS(out_ds_df, ds_save_filename)
    } else {
      k0_ds <- equi$k0[index]
      k1_ds <- equi$k1[index]
      k2_ds <- equi$k2[index]
      k2s_ds<- equi$k2s[index]
      k3_ds <- equi$k3[index]
      k4_ds <- equi$k4[index]
      k4s_ds<- equi$k4s[index]
      pi_ds <- equi$pi[index]
      pi_oo_ds <- equi$pi_oo[index]
      cat("Best K0 DS:",  k0_ds, "\n")
      cat("Best K1 DS:",  k1_ds, "\n")
      cat("Best K2 DS:",  k2_ds, "\n")
      cat("Best K2s DS:", k2s_ds, "\n")
      cat("Best K3 DS:",  k3_ds, "\n")
      cat("Best K4 DS:",  k4_ds, "\n")
      cat("Best K4s DS:", k4s_ds, "\n")

      out_ds <- list(out_pis_list = out_pis_list,
                     equi_list = equi_list,
                     out_pis_full = out_pis_full,
                     out_pis_fine = out_pis_fine,
                     config = list(
                       initial_points= initial_points,
                       precision = precision,
                       radius = radius,
                       max_row = max_row
                     ),
                     k0_ds = k0_ds,
                     k1_ds = k1_ds,
                     k2_ds = k2_ds,
                     k2s_ds= k2s_ds,
                     k3_ds = k3_ds,
                     k4_ds = k4_ds,
                     k4s_ds= k4s_ds,
                     pi_ds = pi_ds,
                     pi_oo_ds = pi_oo_ds
      )
      saveRDS(out_ds, ds_save_filename)
    }
  }
}
