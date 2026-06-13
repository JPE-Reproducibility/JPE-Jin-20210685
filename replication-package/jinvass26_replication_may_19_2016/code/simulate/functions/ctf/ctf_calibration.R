################################################################################
## ctf_calibration.R — Calibrate brand effects and per-customer costs
################################################################################

source('code/simulate/functions/ctf/get_ctf_util_profit.R')

calibration_save_filename <- file.path(bootstrap_ctf_dir, paste0("ctf_calibration-id-", bootstrap_id,".rds"))
if(USE_CACHE && file.exists(calibration_save_filename)){
  out_calibration <- readRDS(calibration_save_filename)
  brand_value_calibrated = out_calibration$brand_value_calibrated
  own_var_cost_calibrated = out_calibration$own_var_cost_calibrated
  k0_calibrated = out_calibration$k0_calibrated
  k3_calibrated = out_calibration$k3_calibrated

  if(length(out_calibration$cost_factors_calibrated) > 2){
    oo_flex_var_cost_calibrated = out_calibration$oo_flex_var_cost_calibrated
    oo_pass_var_cost_calibrated = out_calibration$oo_pass_var_cost_calibrated
    cost_factors_calibrated = out_calibration$cost_factors_calibrated
    brand_calibrate_toggle = F
    cost_calibrate_toggle = F
    cost_flex_calibrate_toggle = F
    cat("loading previous calibration results \n")
  } else {
    oo_flex_var_cost_calibrated = out_calibration$oo_var_cost_calibrated
    brand_calibrate_toggle = F
    cost_calibrate_toggle = T
    cost_flex_calibrate_toggle = F
    cat("loading previous calibration results, adding passive oo cost calibration \n")
  }

} else {
  brand_calibrate_toggle = T
  cost_calibrate_toggle = T
  cost_flex_calibrate_toggle = T
  cat("no previous calibration results found. Calibrating brand value and cost factors \n")
}

#### BRAND VALUE CALIBRATION ####
if(brand_calibrate_toggle){
  obj_function <- function(brand_value, weight_supplied) {
    opt_in_ftr = OPT_IN_FTR
    out <- get_ctf_util_profit(k0 = 1, k1 = opt_in_ftr, k2 = 1, k2s = 1,
                               k3 = 1, k4 = NA, k4s = NA,
                               brand_value = brand_value,
                               cost_factors = c(0,0),
                               oo_tm_response_ind = F,
                               ctf_has_tm = T
    )
    cp = out$cp_first_period
    rev_own = get_samp_wgt_avg(rowSums(out$revs * cp), weight_supplied) #enumeration_weights *
    rev_oo_flex = get_samp_wgt_avg(rowSums(out$revs_oo_flex * cp), weight_supplied) #enumeration_weights *
    rev_oo = get_samp_wgt_avg(rowSums(out$revs_oo * cp), weight_supplied) #enumeration_weights *
    rev_all = rev_own + rev_oo
    return(c(rev_own/rev_all, rev_oo_flex/rev_all))
  }

  estimate_mkt_share <- function(brand_value_grid, step_size, next_step_size){
    mkt_share <- foreach(i = seq_len(nrow(brand_value_grid)), .combine = 'rbind') %dopar% {
      obj_function(
        brand_value = as.vector(unlist(brand_value_grid[i,])),
        weight_supplied = sampling_weight
      )
    }
    eval = mkt_share
    for(k in 1:nrow(eval)){
      eval[k,] = abs(eval[k,] - target_market_share)
    }
    tmp_eval = rowSums(eval)
    best_index = which.min(tmp_eval)
    cat("best index: ", best_index, "\n")
    tmp_values = cbind.data.frame(
                   brand_value_grid[best_index,1], #which.min(eval[,1]),1],
                   brand_value_grid[best_index,2])

    if(is.na(next_step_size)){
      out = tmp_values
    } else {
      lb = tmp_values - step_size/dollar_norm
      ub = tmp_values + step_size/dollar_norm
      out =  expand.grid(brand_value = seq(lb$brand_value, ub$brand_value, by = next_step_size/dollar_norm),
                                       brand_value_oo = seq(lb$brand_value_oo, ub$brand_value_oo, by = next_step_size/dollar_norm)) %>%
        as_tibble()
    }
    return(out)
  }

  if(sum(target_market_share) < 1){
    sz_init = 200
    if(grepl("all", ctf_oo_option)){
      brand_value_grid <- expand.grid(brand_value = seq(-1000, 100, by = sz_init)/dollar_norm,
                                      brand_value_oo = seq(-1000, 100, by = sz_init)/dollar_norm) %>%
        as_tibble()
    } else {
      brand_value_grid <- expand.grid(brand_value = seq(-500, 500, by = sz_init)/dollar_norm,
                                      brand_value_oo = seq(-500, 500, by = sz_init)/dollar_norm) %>%
        as_tibble()
    }
    brand_value_grid <- estimate_mkt_share(brand_value_grid, step_size = sz_init, next_step_size = 100)
  } else {
    sz_init = 100
    brand_value_grid <- as_tibble(seq(-500, 500, by = sz_init)/dollar_norm) %>%
      rename(brand_value = value) %>% mutate(brand_value_oo = 0)
  }

  brand_value_grid <- estimate_mkt_share(brand_value_grid, 100, next_step_size = 50)
  brand_value_grid <- estimate_mkt_share(brand_value_grid, 50, next_step_size = 20)
  brand_value_grid <- estimate_mkt_share(brand_value_grid, 20, next_step_size = 5)
  brand_value_grid <- estimate_mkt_share(brand_value_grid, 5, next_step_size = 1)
  brand_value_calibrated <- estimate_mkt_share(brand_value_grid, next_step_size = NA)
  brand_value_calibrated <- as.vector(unlist(brand_value_calibrated))
  cat("brand_value_calibrated ", paste(brand_value_calibrated), "\n")
  cat("mkt_share_calibrated ", obj_function(brand_value = unlist(brand_value_calibrated), weight_supplied = sampling_weight), "\n")
  cat("target_market_share ", target_market_share, "\n")

} #close if(brand_calibrate_toggle)

  ##### COST CALIBRATION ####
if(cost_calibrate_toggle){
  get_ctf_profit <- function(k0, k3, cost_factors, target){
    out <- get_ctf_util_profit(k0 = k0, k1 = NA, k2 = NA, k2s = NA,
                               k3 = k3, k4 = NA, k4s = NA,
                               brand_value = brand_value_calibrated,
                               cost_factors = c(cost_factors,0), #the passive competitor cost set to 0 for now since we calibrate passive competitor cost by flipping the oo_flex_index
                               oo_tm_response_ind = F,
                               ctf_has_tm = F ## assuming calibration is done without taking into account telematics
    )
    return(get_samp_wgt_avg(rowSums(out$cp_first_period * out[[target]]), weights_combined))
  }

  get_pi_diff <- function(deltas, cost_factors_v, k0_base, k3_base){

    D = length(deltas)
    k0_m = rep(k0_base, D)
    k0_p = rep(k0_base, D)
    k3_m = rep(k3_base, D)
    k3_p = rep(k3_base, D)

    if(!is.na(cost_factors_v[1])){
      target_quant = "profits"
      k0_m = k0_m - deltas
      k0_p = k0_p + deltas
    } else {
      target_quant = "profits_oo_flex"
      k3_m = k3_m - deltas
      k3_p = k3_p + deltas
    }

    pi_100 <- get_ctf_profit(k0 = k0_base, k3 = k3_base, cost_factors = cost_factors_v, target = target_quant)
    signs = rep(0,D)
    mags  = rep(0,D)

    for(d in 1:length(deltas)){
        pi_m <- get_ctf_profit(k0 = k0_m[d], k3 = k3_m[d], cost_factors = cost_factors_v, target = target_quant)
        pi_p <- get_ctf_profit(k0 = k0_p[d], k3 = k3_p[d], cost_factors = cost_factors_v, target = target_quant)
        signs[d] = sign(pi_100 - pi_m) + sign(pi_100 - pi_p)
        mags[d] = abs(pi_100 - pi_m) + abs(pi_100 - pi_p)
    }

    return(list(signs = signs,
                mags = mags,
                deltas = deltas
    ))
  }

  obj_function <- function(cf1, cf2, k0, k3,
                           deltas, deltas_weights
                           ) {

    if(is.na(cf1) + is.na(cf2) > 1){
      cat("default to calibrating own firm cost \n")
    }

    out <- get_pi_diff(
      deltas         = deltas,
      cost_factors_v = c(cf1, cf2),
      k0_base        = k0,
      k3_base        = k3
    )

    score <- 0
    for(d in 1:length(deltas)){
      tmp_score = out$signs[d] / 2 #normalize to 1 being optimum found
      score <- score + tmp_score * deltas_weights[d]
    }

    return(c(score, out$signs))
  }

  # Evaluate in parallel (fork-based)
  get_grid_search_result <- function(stage_grid, deltas, deltas_weights, select_best_toggle) {
    stage_scores <- foreach(i = seq_len(nrow(stage_grid)), .combine = 'rbind') %dopar% {
      row_i <- stage_grid[i, ]
      obj_function(
        cf1 = row_i$cf1,
        cf2 = row_i$cf2,
        k0 = row_i$k0,
        k3 = row_i$k3,
        deltas = deltas,
        deltas_weights = deltas_weights
      )
    }
    colnames(stage_scores) = c("score", deltas)

    D = length(deltas)
    max_delta_index = which.max(deltas)
    found_optima <- max(stage_scores[,(1+max_delta_index)]) > 0 #the coarsest delta has signs > 0 - notice the output of obj_function is c(score, out$signs)

    if(max(found_optima) > 0){
      cat("stage complete, calibrated optima found")
    } else {
      cat("stage complete, calibrated optima NOT found.")
    }

    best_score   <- max(stage_scores[,1])
    cat("Best Stage score:", best_score, "\n")
    best_indices <- which(stage_scores[,1] == best_score)
    cat("Number of combos achieving best Stage score:", length(best_indices), "out of", nrow(stage_grid),"\n")
    stage_results <- cbind.data.frame(stage_grid, as.data.frame(stage_scores))
    if(select_best_toggle){
      # Keep all combos that tie for best Stage-1 score
      stage_results <- stage_results[best_indices,]
      print(stage_results)
    }

    return(stage_results)
  }

  ##### ADAPTIVE COST CALIBRATION #####

  stage_deltas_list <- list(
    c(0.5, 0.3), c(0.2, 0.1), c(0.005, seq(0.01,0.05,by=0.02))
  )
  stage_deltas_weights_list <- list(
    c(1, 0.5),   c(1, 0.5),   c(1/8, 1/4, 1/2, 1)
  )
  coarseness_list <- c(50, 10, 1)/dollar_norm

  ## calibrate passive oo
  flexible_oo_index_save <- flexible_oo_index

  for(h in 1:length(stage_deltas_list)){
    stage_deltas = stage_deltas_list[[h]]
    stage_deltas_weights = stage_deltas_weights_list[[h]]

    if(h < 2){
      stage_cf_grid = seq(-500/dollar_norm, 500/dollar_norm, by = coarseness_list[h])
      stage_cf_oo_flex_grid = stage_cf_grid
      stage_cf_oo_pass_grid = stage_cf_grid
    } else {
      if(cost_flex_calibrate_toggle){
        stage_cf_grid = round(seq(min(stage_own_results$cf1) - coarseness_list[h-1],
                                  max(stage_own_results$cf1) + coarseness_list[h-1]
                                  , by = coarseness_list[h]), log10(dollar_norm))
        stage_cf_oo_flex_grid = round(seq(min(stage_oo_flex_results$cf2) - coarseness_list[h-1],
                                          max(stage_oo_flex_results$cf2) + coarseness_list[h-1]
                                          , by = coarseness_list[h]), log10(dollar_norm))
      }# close if(cost_flex_calibrate_toggle)
      stage_cf_oo_pass_grid = round(seq(min(stage_oo_pass_results$cf2) - coarseness_list[h-1],
                                        max(stage_oo_pass_results$cf2) + coarseness_list[h-1]
                                        , by = coarseness_list[h]), log10(dollar_norm))
    }

    if(cost_flex_calibrate_toggle){
      stage_own_results <- get_grid_search_result(stage_grid = expand.grid(cf1 = stage_cf_grid,
                                                                           cf2 = NA,
                                                                           k0  = 1, k3  = 1)
                                                , deltas = stage_deltas
                                                , deltas_weights = stage_deltas_weights
                                                , select_best_toggle = T
                                                )

      stage_oo_flex_results <- get_grid_search_result(stage_grid = expand.grid(cf1 = NA,
                                                                           cf2 = stage_cf_oo_flex_grid,
                                                                           k0  = 1, k3  = 1)
                                                  , deltas = stage_deltas
                                                  , deltas_weights = stage_deltas_weights
                                                  , select_best_toggle = T
                                                  )
    }# close if(cost_flex_calibrate_toggle)

    flexible_oo_index <- !flexible_oo_index_save
    stage_oo_pass_results <- get_grid_search_result(stage_grid = expand.grid(cf1 = NA,
                                                                             cf2 = stage_cf_oo_pass_grid,
                                                                             k0  = 1, k3  = 1)
                                                  , deltas = stage_deltas
                                                  , deltas_weights = stage_deltas_weights
                                                  , select_best_toggle = T
                                                  )
    flexible_oo_index <- flexible_oo_index_save
  }

  own_var_cost_calibrated = stage_own_results$cf1[which.min(abs(stage_own_results$cf1))]
  oo_flex_var_cost_calibrated = stage_oo_flex_results$cf2[which.min(abs(stage_oo_flex_results$cf2))]
  oo_pass_var_cost_calibrated = stage_oo_pass_results$cf2[which.min(abs(stage_oo_pass_results$cf2))]
  cost_factors_calibrated = c(own_var_cost_calibrated,oo_flex_var_cost_calibrated,oo_pass_var_cost_calibrated)
  cat("own_var_cost_calibrated: ", own_var_cost_calibrated, "\n")
  cat("oo_flex_var_cost_calibrated: ", oo_flex_var_cost_calibrated, "\n")
  cat("oo_pass_var_cost_calibrated: ", oo_pass_var_cost_calibrated, "\n")

  ##### RE-EQUILIBRIATE BETWEEN OWN FIRM AND OO FLEX FIRM #####
  if(cost_flex_calibrate_toggle){
    if(max(stage_own_results[,colnames(stage_own_results) == "0.01"])<2 |
       max(stage_oo_flex_results[,colnames(stage_oo_flex_results) == "0.01"])<2){

      get_equi <- function(
        stage_deltas,
        stage_deltas_weights,
        stage_k0_grid,
        stage_k3_grid
        ){
        stage_k0_results <- get_grid_search_result(stage_grid = expand.grid(cf1 = own_var_cost_calibrated,
                                                                            cf2 = NA,
                                                                            k0  = stage_k0_grid,
                                                                            k3  = stage_k3_grid)
                                              , deltas = stage_deltas
                                              , deltas_weights = stage_deltas_weights
                                              , select_best_toggle = F
                                              )
        stage_k3_results <- get_grid_search_result(stage_grid = expand.grid(cf1 = NA,
                                                                            cf2 = oo_flex_var_cost_calibrated,
                                                                            k0  = stage_k0_grid,
                                                                            k3  = stage_k3_grid)
                                                        , deltas = stage_deltas
                                                        , deltas_weights = stage_deltas_weights
                                                        , select_best_toggle = F
                                                   )

        stage_results <- cbind.data.frame(stage_k0_results, stage_k3_results)
        stage_results$score_combined = stage_k0_results$score + stage_k3_results$score

        best_score <- max(stage_results$score_combined)
        cat("Best Stage score:", best_score, "\n")
        best_indices <- which(stage_results$score_combined == best_score)
        cat("Number of combos achieving best Stage score:", length(best_indices), "out of", nrow(stage_results),"\n")
        stage_results <- stage_results[best_indices,]
        print(stage_results)

        return(stage_results)
      }

      ## coarser search
      stage_deltas = seq(0.01,0.05,by=0.02)
      stage_deltas_weights = c(1/4, 1/2, 1)
      stage_k0_grid = seq(0.96, 1.04, by = 0.02)
      stage_k3_grid = seq(0.96, 1.04, by = 0.02)
      cat("Stage grid size:", length(stage_k0_grid) * length(stage_k3_grid), "\n")
      equi_baseline_results <- get_equi(stage_deltas,
                                        stage_deltas_weights,
                                        stage_k0_grid,
                                        stage_k3_grid)
      ## finer search
      stage_k0_grid = seq(min(equi_baseline_results$k0)-0.01, max(equi_baseline_results$k0)+0.01, by = 0.01)
      stage_k3_grid = seq(min(equi_baseline_results$k3)-0.01, max(equi_baseline_results$k3)+0.01, by = 0.01)
      cat("Stage grid size:", length(stage_k0_grid) * length(stage_k3_grid), "\n")

      equi_baseline_results <- get_equi(stage_deltas,
                                        stage_deltas_weights,
                                        stage_k0_grid,
                                        stage_k3_grid)

      index = which.min(abs(equi_baseline_results$k0-1) * 100 + abs(equi_baseline_results$k3-1))
      k0_calibrated = equi_baseline_results$k0[index]
      k3_calibrated = equi_baseline_results$k3[index]
    } else {
      k0_calibrated = 1
      k3_calibrated = 1
    }
    cat("k0_calibrated: ", k0_calibrated, "\n")
    cat("k3_calibrated: ", k3_calibrated, "\n")
  } # close if(cost_flex_calibrate_toggle)
} # close if(cost_calibrate_toggle)

if(brand_calibrate_toggle | cost_calibrate_toggle){
  out_calibration <- list(brand_value_calibrated = brand_value_calibrated,
                          own_var_cost_calibrated = own_var_cost_calibrated,
                          oo_flex_var_cost_calibrated = oo_flex_var_cost_calibrated,
                          oo_pass_var_cost_calibrated = oo_pass_var_cost_calibrated,
                          cost_factors_calibrated = cost_factors_calibrated,
                          k0_calibrated = k0_calibrated,
                          k3_calibrated = k3_calibrated
  )
  saveRDS(out_calibration, calibration_save_filename)
  cat("calibration results saved as: ", calibration_save_filename,"\n")
}
