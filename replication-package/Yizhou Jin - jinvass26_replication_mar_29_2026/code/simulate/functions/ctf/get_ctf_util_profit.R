# Core utility and profit computation for counterfactual pricing regimes.
#
# HARDCODED ASSUMPTIONS:
#   opt_in_factor: 0.967 (3.3% monitoring opt-in discount, set via k1 parameter)
#   Consumers are rational about reclassification risk (always applied)
#   Renewal choice probabilities use structural model (not CCP)
#   cost_factors[1]=own TM cost, [2]=flex OO cost, [3]=passive OO cost
#   Learning effect: 10% persistent reduction in lambda when model_config contains "learning"
#
# Parameters:
#   k0: own-firm baseline price factor (k0=1 means no change from estimated price)
#   k1: monitoring opt-in discount factor (k1=1 means no discount)
#   k2, k2s: own-firm renewal pricing weights for monitored vs unmonitored (hybrid pricing)
#   k3: flexible competitor baseline price factor
#   k4, k4s: competitor renewal pricing weights (analogous to k2, k2s)
#   brand_value: [own_firm, flex_competitor] brand effects
#   cost_factors: [own, flex_OO, passive_OO] per-customer costs
#   oo_tm_response_ind: whether competitor responds to telematics signals
#   ctf_has_tm: whether telematics option is available
get_ctf_util_profit = function(k0, k1, k2, k2s, k3, k4, k4s,
                               brand_value, cost_factors,
                               oo_tm_response_ind, ctf_has_tm){
  J = ncol(price_base)
  J_oo = ncol(price_oo_base)
  zero_matrix_J = matrix(0, N_part, J)
  zero_matrix_J_oo = matrix(0, N_part, J_oo)

  price_ctf = price_base * k0
  # price_oo_ctf = price_oo_base * k3
  price_oo_ctf = price_oo_base
  price_oo_ctf[,flexible_oo_index] = price_oo_ctf[,flexible_oo_index] * k3
  tm_optin_disc_dollar_ctf = price_ctf * (1 - k1) #k1 = 1 --> no discount

  get_hybrid_pricing_moments <- function(a, b, mu1, mu2, v1, v2, c) {
    # Derived parameters
    sigma_D <- sqrt(v1 + v2 - 2 * c)
    alpha   <- (mu2 - mu1) / sigma_D
    alpha   <- pmin(pmax(-5, alpha), 5)

    # Scenario probabilities
    P_discount  <- pnorm(alpha)
    P_surcharge <- 1 - P_discount

    # Inverse Mills ratios
    ivr_discount  <- dnorm(alpha) / pnorm(alpha)
    ivr_surcharge <- dnorm(alpha) / (1 - pnorm(alpha))

    # Conditional means
    E_R1_discount  <- mu1 - ivr_discount * (v1 - c) / sigma_D
    E_R0_discount  <- mu2 - ivr_discount * (c - v2) / sigma_D

    E_R1_surcharge <- mu1 + ivr_surcharge * (v1 - c) / sigma_D
    E_R0_surcharge <- mu2 + ivr_surcharge * (c - v2) / sigma_D

    # First moment (mean) of R
    E_R <- (a * E_R1_discount + (1 - a) * E_R0_discount) * P_discount +
           (b * E_R1_surcharge + (1 - b) * E_R0_surcharge) * P_surcharge

    # Second moments calculations:
    Var_R1_discount <- v1 - ((v1 - c)^2 / sigma_D^2)*(alpha*ivr_discount + ivr_discount^2)
    Var_R0_discount <- v2 - ((c - v2)^2 / sigma_D^2)*(alpha*ivr_discount + ivr_discount^2)
    Cov_R1R0_discount <- c - ((v1 - c)*(c - v2)/sigma_D^2)*(alpha*ivr_discount + ivr_discount^2)

    Var_R1_surcharge <- v1 - ((v1 - c)^2 / sigma_D^2)*(-alpha*ivr_surcharge + ivr_surcharge^2)
    Var_R0_surcharge <- v2 - ((c - v2)^2 / sigma_D^2)*(-alpha*ivr_surcharge + ivr_surcharge^2)
    Cov_R1R0_surcharge <- c + ((v1 - c)*(c - v2)/sigma_D^2)*(alpha*ivr_surcharge - ivr_surcharge^2)

    # Second moment of R
    E_R2_discount <- a^2*(Var_R1_discount + E_R1_discount^2) +
      (1 - a)^2*Var_R0_discount + (1 - a)^2*E_R0_discount^2 +
      2*a*(1 - a)*(Cov_R1R0_discount + E_R1_discount*E_R0_discount)

    E_R2_surcharge <- b^2*(Var_R1_surcharge + E_R1_surcharge^2) +
      (1 - b)^2*(Var_R0_surcharge + E_R0_surcharge^2) +
      2*b*(1 - b)*(Cov_R1R0_surcharge + E_R1_surcharge*E_R0_surcharge)

    # Overall second moment
    E_R2 <- E_R2_discount*P_discount + E_R2_surcharge*P_surcharge

    # Variance of R
    Var_R <- E_R2 - E_R^2

    # Return structured results
    list(
      mean = E_R,
      sigma = sqrt(Var_R)
    )
  }

  mu_tm = R_tm_choice[n0_tm:(n0_tm-1+N_part),1]
  v_tm = (R_tm_choice[n0_tm:(n0_tm-1+N_part),2])^2
  mu_0 = R_choice[n0:n1,1]
  v_0 = (R_choice[n0:n1,2])^2
  c_tm_0 = cov(p_R_ftr_wo_clm_w_tm_R_ordered, p_R_ftr_wo_clm_R_ordered[data_list$I_tm_R_to_N_R])

  tmp_own <- get_hybrid_pricing_moments(k2, k2s, mu_tm, mu_0, v_tm, v_0, c_tm_0)
  R_tm_ctf_m1 <- tmp_own$mean
  R_tm_ctf_m2 <- tmp_own$sigma

  if(oo_tm_response_ind){
    tmp_oo <- get_hybrid_pricing_moments(k4, k4s, mu_tm, mu_0, v_tm, v_0, c_tm_0)
    R_oo_tm_ctf_m1 <- tmp_oo$mean
    R_oo_tm_ctf_m2 <- tmp_oo$sigma
    # Consumers rationally anticipate reclassification risk from monitoring signals
    R_oo_tm_ctf_m1_t1 <- R_oo_tm_ctf_m1
    R_oo_tm_ctf_m2_t1 <- R_oo_tm_ctf_m2
  } else {
    R_oo_tm_ctf_m1 = mu_0
    R_oo_tm_ctf_m2 = v_0
    R_oo_tm_ctf_m1_t1 = mu_0
    R_oo_tm_ctf_m2_t1 = v_0
  }
  #### calculate baseline utils and profits #####

  utils_oo_flex = out_model$functions$get_util_nb_w_joo(
    price_oo_ctf[,flexible_oo_index],
    cbind(price_ctf, price_oo_ctf[,!flexible_oo_index]), #assuming that tm options unavailable in people's expectation about choosing oo plans
    clm_surcharge_ftr[n0:n1],
    limits_oo_base[flexible_oo_index],
    c(limits_base,limits_oo_base[!flexible_oo_index]),
    matrix(brand_value[2], N_part, sum(flexible_oo_index)), #adj
    lambda_choice[n0:n1],
    lambda_severe_choice[n0:n1],
    pareto_alpha_choice_base[n0:n1],
    R_choice[n0:n1,1],
    R_choice[n0:n1,2],
    R_choice[n0:n1,1],
    R_choice[n0:n1,2],
    zip_inc_norm[n0:n1],
    risk_aversion[n0:n1],
    firm_switch_cost[n0:n1],
    inert_cost[n0:n1],
    discount_factor,
    sigma_logit_renw1
  )

  if(min(flexible_oo_index) < 1){
    utils_oo_not_flex = out_model$functions$get_util_nb_w_joo(
      price_oo_ctf[,!flexible_oo_index],
      cbind(price_ctf, price_oo_ctf[,flexible_oo_index]), #assuming that tm options unavailable in people's expectation about choosing oo plans
      clm_surcharge_ftr[n0:n1],
      limits_oo_base[!flexible_oo_index],
      c(limits_base, limits_oo_base[flexible_oo_index]),
      matrix(0, N_part, sum(!flexible_oo_index)), #adj
      lambda_choice[n0:n1],
      lambda_severe_choice[n0:n1],
      pareto_alpha_choice_base[n0:n1],
      R_choice[n0:n1,1],
      R_choice[n0:n1,2],
      R_choice[n0:n1,1],
      R_choice[n0:n1,2],
      zip_inc_norm[n0:n1],
      risk_aversion[n0:n1],
      firm_switch_cost[n0:n1],
      inert_cost[n0:n1],
      discount_factor,
      sigma_logit_renw1
    )

    utils_oo = matrix(0, N_part, J_oo)
    utils_oo[,flexible_oo_index] = utils_oo_flex
    utils_oo[,!flexible_oo_index] = utils_oo_not_flex
  } else {
    utils_oo = utils_oo_flex
  }

  utils_own = out_model$functions$get_util_nb_w_joo(
    price_ctf,
    price_oo_ctf,
    clm_surcharge_ftr[n0:n1],
    limits_base,
    limits_oo_base,
    matrix(brand_value[1], N_part, J), #adj
    lambda_choice[n0:n1],
    lambda_severe_choice[n0:n1],
    pareto_alpha_choice_base[n0:n1],
    R_choice[n0:n1,1],
    R_choice[n0:n1,2],
    R_choice[n0:n1,1],
    R_choice[n0:n1,2],
    zip_inc_norm[n0:n1],
    risk_aversion[n0:n1],
    firm_switch_cost[n0:n1],
    inert_cost[n0:n1],
    discount_factor,
    sigma_logit_renw1
  )

  revs = price_ctf
  revs_oo = price_oo_ctf
  revs_oo_flex = revs_oo
  revs_oo_flex[,!flexible_oo_index] <- 0

  profits = out_model$functions$get_profit(
    price_ctf,
    limits_base,
    lambda[N_choice_to_N][n0:n1],
    lambda_severe[N_choice_to_N][n0:n1],
    pareto_alpha_choice_base[n0:n1],
    sev_minor_mean_choice[n0:n1],
    sev_minor_sd) - cost_factors[1]; #head(profit)

  profits_oo = out_model$functions$get_profit(
    price_oo_ctf,
    limits_oo_base,
    lambda[N_choice_to_N][n0:n1],
    lambda_severe[N_choice_to_N][n0:n1],
    pareto_alpha_choice_base[n0:n1],
    sev_minor_mean_choice[n0:n1],
    sev_minor_sd); #head(profits_oo)

  profits_oo[,flexible_oo_index] = profits_oo[,flexible_oo_index] - cost_factors[2]
  profits_oo[,!flexible_oo_index] = profits_oo[,!flexible_oo_index] - cost_factors[3]

  profits_oo_flex = profits_oo
  profits_oo_flex[,!flexible_oo_index] <- 0

  #### add tm options #####

  if(ctf_has_tm){
    utils_tm = out_model$functions$get_util_nb_w_joo(
      price_ctf,
      price_oo_ctf,
      clm_surcharge_ftr[n0:n1],
      limits_base,
      limits_oo_base,
      tm_optin_disc_dollar_ctf - replicate(J,xi_choice[n0:n1]) + matrix(brand_value[1], N_part, J), #adj
      lambda_choice_tm_tmp[n0:n1],
      lambda_severe_choice_tm_tmp[n0:n1],
      pareto_alpha_choice_base[n0:n1],
      R_tm_ctf_m1,
      R_tm_ctf_m2,
      ###### TOGGLE FOR RATIONALITY####
      R_oo_tm_ctf_m1_t1, #if oo responds and monitoring signals becomes public,
      R_oo_tm_ctf_m2_t1, #then oo firm can poach you away after you get monitored at the own firm
      zip_inc_norm[n0:n1],
      risk_aversion[n0:n1],
      firm_switch_cost[n0:n1],
      inert_cost[n0:n1],
      discount_factor,
      sigma_logit_renw1
    )

    revs_tm = price_ctf - tm_optin_disc_dollar_ctf

    profits_tm = out_model$functions$get_profit(
      revs_tm, #notice that we are now calculating only the first-period profit
      limits_base,
      lambda_choice_w_mh[n0:n1], #notice that this may not be the same as lambda_choice_tm_tmp[n0:n1]
      lambda_severe_choice_w_mh[n0:n1],
      pareto_alpha_choice_base[n0:n1],
      sev_minor_mean_choice[n0:n1],
      sev_minor_sd
    ) - cost_factors[1] - tm_resource_cost #subtract resource cost

    utils_combined = cbind(utils_own, utils_oo, utils_tm)
    revs = cbind(revs, zero_matrix_J_oo, revs_tm)
    revs_oo = cbind(zero_matrix_J, revs_oo, zero_matrix_J)
    revs_oo_flex = cbind(zero_matrix_J, revs_oo_flex, zero_matrix_J)
    profits = cbind(profits, zero_matrix_J_oo, profits_tm)
    profits_oo = cbind(zero_matrix_J, profits_oo, zero_matrix_J)
    profits_oo_flex = cbind(zero_matrix_J, profits_oo_flex, zero_matrix_J)

  } else {
    utils_combined = cbind(utils_own, utils_oo)
    revs = cbind(revs, zero_matrix_J_oo)
    revs_oo = cbind(zero_matrix_J, revs_oo)
    revs_oo_flex = cbind(zero_matrix_J, revs_oo_flex)
    profits = cbind(profits, zero_matrix_J_oo)
    profits_oo = cbind(zero_matrix_J, profits_oo)
    profits_oo_flex = cbind(zero_matrix_J, profits_oo_flex)
  }

  # For 4p: add brand_value to continuation utility for the extra period (t=4).
  # The Stan get_util_nb_w_joo already embeds continuation through get_inclusive_value_3p,
  # but brand_value only enters the flow utility (adj), not the continuation CE terms.
  # This adds a discounted brand_value offset for the t=4 continuation period.
  # Only affects 4p (profit_horizon > 3); 2p and 3p are unchanged.
  if (profit_horizon > 3) {
    bv_offset_own <- discount_factor^3 * brand_value[1]
    bv_offset_oo  <- discount_factor^3 * brand_value[2]
    utils_combined[, 1:J] <- utils_combined[, 1:J] + bv_offset_own
    utils_combined[, (J+1):(J+J_oo)] <- utils_combined[, (J+1):(J+J_oo)] + bv_offset_oo
    if (ctf_has_tm) {
      utils_combined[, (J+J_oo+1):(J+J_oo+J)] <- utils_combined[, (J+J_oo+1):(J+J_oo+J)] + bv_offset_own
    }
  }

  log_choice_probs_raw = ctf_model$functions$get_loglikelihood( ##equalize coverage options across firms
    utils_combined, rep(tmp_sigma, N_part), 1
  )

  profits_first_period = profits
  profits_oo_first_period = profits_oo
  profits_oo_flex_first_period = profits_oo_flex

  cp_first_period = exp(log_choice_probs_raw) #J*3 first period choices: own plans, own/tm plans, oo plans

  utils_second_period = data.frame(matrix(0, N_part, (J+J_oo))) #J*2 second period choices: own plans, oo plans
  cp_second_period = data.frame(matrix(0, N_part, (J+J_oo))) #J*2 second period choices: own plans, oo plans

  #### get 2+ period profits #####
  renewal_R1 = rep(mean(R_choice[(I:N_choice),1]), N_part)
  renewal_R2 = rep(mean(R_choice[(I:N_choice),2]), N_part)
  for (acc_scenario in 0:(welfare_horizon-1)){
    ## first calculate the baseline renewal price that consumers face in second period
    tmp_surcharge = clm_surcharge_ftr[n0:n1] * (acc_scenario > 0) + 1 * (acc_scenario == 0)
    price_renw_base = price_ctf * tmp_surcharge
    price_oo_renw_base = price_oo_ctf * tmp_surcharge

    ## get second-period profit matrix for each own-firm first-period choice
    counter_oo_flex_index = 1
    counter_oo_non_flex_index = 1
    for (j in 1:(J + J_oo + ctf_has_tm*J)){ # first-period choices J+J_oo+J when there is tm
      price_oo_renw = price_oo_renw_base
      # first do first-to-second period transition (cut of tm)
      lambda_first_period = lambda_choice[n0:n1]  ## we have already calculated t0 profit, only need transition probability now
      lambda_choice_latter_period = lambda_choice[n0:n1] ## technically their X'es evolves over time, but we are assuming that lambda they stay constant here
      lambda_severe_choice_latter_period = lambda_severe_choice[n0:n1]
      lambda_latter_period = lambda[N_choice_to_N][n0:n1]
      lambda_severe_latter_period = lambda_severe[N_choice_to_N][n0:n1]
      if(j > J & j < (J+J_oo+1)){  # first period choosing oo option
        price_renw = price_renw_base
        if(flexible_oo_index[j-J]){ # first period choosing flex oo option
          price_oo_renw[,flexible_oo_index] = price_oo_renw_base[,flexible_oo_index] * R_choice[n0:n1,1]
        } else {
          price_oo_renw[,!flexible_oo_index] = price_oo_renw_base[,!flexible_oo_index] * R_choice[n0:n1,1]
        }
      } else {
        if(ctf_has_tm & j > (J+J_oo)){ # first period choosing tm options
          lambda_first_period = lambda_choice_tm_tmp[n0:n1]
          lambda_choice_latter_period = lambda_choice[n0:n1] * ifelse(grepl("learning", model_config), LEARNING_EFFECT_MULT, 1)
          lambda_severe_choice_latter_period = lambda_severe_choice[n0:n1] * ifelse(grepl("learning", model_config), LEARNING_EFFECT_MULT, 1)
          lambda_latter_period = lambda[N_choice_to_N][n0:n1] * ifelse(grepl("learning", model_config), LEARNING_EFFECT_MULT, 1)
          lambda_severe_latter_period = lambda_severe[N_choice_to_N][n0:n1] * ifelse(grepl("learning", model_config), LEARNING_EFFECT_MULT, 1)
          price_renw = price_renw_base * R_tm_ctf_m1
          price_oo_renw[,flexible_oo_index] = price_oo_renw_base[,flexible_oo_index] * R_oo_tm_ctf_m1
        } else { # first period choosing own firm non tm options
          price_renw = price_renw_base * R_choice[n0:n1,1]
          # price_oo_renw = price_oo_renw_base
        }
      }
      ## this must come after lambda is defined (which varies with j due to mh effects associated with tm options!)
      pr_acc_scenario = out_model$functions$get_acc_weight((acc_scenario>0), lambda_first_period)

      # first do 2+ period transition (no cut of tm)
      R_m1_vec = rep(NA, N_part)
      R_m2_vec = rep(NA, N_part)
      R_oo_flex_m0_vec = rep(NA, N_part)
      R_oo_flex_m2_vec = rep(NA, N_part)
      R_oo_not_flex_m1_vec = rep(NA, N_part)
      R_oo_not_flex_m2_vec = rep(NA, N_part)
      R_oo_m1_mat =  matrix(NA, N_part, J_oo) # we need a matrix here since flex and not flex may have different R
      # cat("j: ", j, "\n")
      if(j > J & j < (J+J_oo+1)){  # first period choosing oo option
        if(flexible_oo_index[j-J]){ # first period choosing flex oo option
          #second period only expenses
          tmp = replicate(sum(flexible_oo_index), -inert_cost[n0:n1] + brand_value[2])
          tmp[,counter_oo_flex_index] = 0
          counter_oo_flex_index = counter_oo_flex_index + 1
          adj_flex_oo = tmp
          #second-to-third-period R and R_oo:
          R_m1_vec = R_choice[n0:n1,1]
          R_m2_vec = R_choice[n0:n1,2]
          R_oo_flex_m0_vec = renewal_R1
          R_oo_flex_m2_vec = renewal_R2
          R_oo_not_flex_m1_vec = R_choice[n0:n1,1]
          R_oo_not_flex_m2_vec = R_choice[n0:n1,2]
          R_oo_m1_mat[,flexible_oo_index] = do.call(cbind, replicate(sum(flexible_oo_index), renewal_R1, simplify = FALSE))
          if(min(flexible_oo_index) < 1)  {
            adj_not_flex_oo = replicate(sum(!flexible_oo_index),-firm_switch_cost[n0:n1])
            R_oo_m1_mat[,!flexible_oo_index] = do.call(cbind, replicate(sum(!flexible_oo_index), R_choice[n0:n1,1], simplify = FALSE))
          }
        } else { # first period choosing non_flex oo option
          #second period only expenses
          tmp = replicate(sum(!flexible_oo_index), -inert_cost[n0:n1])
          tmp[,counter_oo_non_flex_index] = 0
          counter_oo_non_flex_index = counter_oo_non_flex_index + 1
          adj_not_flex_oo = tmp
          adj_flex_oo = replicate(sum(flexible_oo_index),-firm_switch_cost[n0:n1]+brand_value[2])
          #second-to-third-period R and R_oo:
          R_m1_vec = R_choice[n0:n1,1]
          R_m2_vec = R_choice[n0:n1,2]
          R_oo_flex_m0_vec = R_choice[n0:n1,1]
          R_oo_flex_m2_vec = R_choice[n0:n1,2]
          R_oo_not_flex_m1_vec = renewal_R1
          R_oo_not_flex_m2_vec = renewal_R2
          R_oo_m1_mat[,!flexible_oo_index] = do.call(cbind, replicate(sum(!flexible_oo_index), renewal_R1, simplify = FALSE))
          R_oo_m1_mat[,flexible_oo_index] = do.call(cbind, replicate(sum(flexible_oo_index), R_choice[n0:n1,1], simplify = FALSE))
        } # close first period choosing flex or nonflex oo option
        # R_choice[n0:n1,1]
        adj_own = replicate(J,-firm_switch_cost[n0:n1]+brand_value[1])
        R_m1_mat = do.call(cbind, replicate(J, R_choice[n0:n1,1], simplify = FALSE))
      } else {  # first period choosing own firm option
        #second period only expenses
        tmp = replicate(J, - inert_cost[n0:n1] + brand_value[1])
        tmp[,round((j - 0.1) %% (J+J_oo))] = 0 #round((seq(J+J+J_oo) - 0.1) %% (J+J_oo))
        adj_own = tmp
        adj_flex_oo = replicate(sum(flexible_oo_index),-firm_switch_cost[n0:n1]+brand_value[2])
        adj_not_flex_oo = replicate(sum(!flexible_oo_index),-firm_switch_cost[n0:n1])
        if(exists("psi_oo")){
          if(!is.na(psi_oo)){
            adj_flex_oo = adj_flex_oo + psi_oo
            adj_not_flex_oo = adj_not_flex_oo + psi_oo
          }
        }
        #second-to-third-period R and R_oo:
        R_m1_vec = renewal_R1
        R_m2_vec = renewal_R2
        R_oo_flex_m1_vec = R_choice[n0:n1,1]
        R_oo_flex_m2_vec = R_choice[n0:n1,2]
        R_oo_not_flex_m1_vec = R_choice[n0:n1,1]
        R_oo_not_flex_m2_vec = R_choice[n0:n1,2]
        R_m1_mat = do.call(cbind, replicate(J, renewal_R1, simplify = FALSE))
        R_oo_m1_mat = do.call(cbind, replicate(J_oo, R_choice[n0:n1,1], simplify = FALSE))
      } # close first period choosing oo or own firm option
      ##### t=2 PROFITS #####
      ## there is no longer tm choice in second period, when j has tm in it, you simply get a different price_renw and/or price_oo_renw
      utils_oo_flex = out_model$functions$get_util_renw_w_joo(
          price_oo_renw[,flexible_oo_index],
          cbind(price_renw, price_oo_renw[,!flexible_oo_index]), #assuming that tm options unavailable in people's expectation about choosing oo plans
          clm_surcharge_ftr[n0:n1],
          limits_oo_base[flexible_oo_index],
          # c(limits_base,limits_oo_base[!flexible_oo_index]),
          adj_flex_oo, #adj
          lambda_choice_latter_period, # lambda_choice[n0:n1],
          lambda_severe_choice_latter_period, # lambda_severe_choice[n0:n1],
          pareto_alpha_choice_base[n0:n1],
          R_oo_flex_m1_vec,
          R_oo_flex_m2_vec,
          R_choice[n0:n1,1], #you will always get this as +1-period outside option renewal rate
          R_choice[n0:n1,2],
          zip_inc_norm[n0:n1],
          risk_aversion[n0:n1],
          firm_switch_cost[n0:n1],
          inert_cost[n0:n1],
          discount_factor,
          sigma_logit_renw1,
          sigma_logit_renw
        )

      if(min(flexible_oo_index) < 1)  {
        utils_oo_not_flex = out_model$functions$get_util_renw_w_joo(
          price_oo_renw[,!flexible_oo_index],
          cbind(price_renw, price_oo_renw[,flexible_oo_index]), #assuming that tm options unavailable in people's expectation about choosing oo plans
          clm_surcharge_ftr[n0:n1],
          limits_oo_base[!flexible_oo_index],
          # c(limits_base, limits_oo_base[flexible_oo_index]),
          adj_not_flex_oo, #adj
          lambda_choice_latter_period, # lambda_choice[n0:n1],
          lambda_severe_choice_latter_period, # lambda_severe_choice[n0:n1],
          pareto_alpha_choice_base[n0:n1],
          R_oo_not_flex_m1_vec,
          R_oo_not_flex_m2_vec,
          R_choice[n0:n1,1], #you will always get this as +1-period outside option renewal rate
          R_choice[n0:n1,2],
          zip_inc_norm[n0:n1],
          risk_aversion[n0:n1],
          firm_switch_cost[n0:n1],
          inert_cost[n0:n1],
          discount_factor,
          sigma_logit_renw1,
          sigma_logit_renw
        )

        utils_oo_renw = matrix(0, N_part, J_oo)
        utils_oo_renw[,flexible_oo_index] = utils_oo_flex
        utils_oo_renw[,!flexible_oo_index] = utils_oo_not_flex
      } else {
        utils_oo_renw = utils_oo_flex
      }

      utils_renw = out_model$functions$get_util_renw_w_joo(
          price_renw,
          price_oo_renw,
          clm_surcharge_ftr[n0:n1],
          limits_base,
          adj_own, # + matrix(-0.2, N_part, J)
          lambda_choice_latter_period, # lambda_choice[n0:n1],
          lambda_severe_choice_latter_period, # lambda_severe_choice[n0:n1],
          pareto_alpha_choice_base[n0:n1],
          R_m1_vec,
          R_m2_vec,
          R_choice[n0:n1,1], #you will always get this as +1-period outside option renewal rate
          R_choice[n0:n1,2],
          zip_inc_norm[n0:n1],
          risk_aversion[n0:n1],
          firm_switch_cost[n0:n1],
          inert_cost[n0:n1],
          discount_factor,
          sigma_logit_renw1,
          sigma_logit_renw
        )

      utils_renw <- cbind(utils_renw,utils_oo_renw)

      cp_renw = exp(ctf_model$functions$get_loglikelihood(
        utils_renw, rep(sigma_logit_renw1, N_part), 1
      ))

      ## calculate future period profits (assuming no switch after t2)
      pi_t2 = out_model$functions$get_profit(price_renw,
                limits_base,
                lambda_latter_period, #lambda[N_choice_to_N][n0:n1],
                lambda_severe_latter_period, #lambda_severe[N_choice_to_N][n0:n1],
                pareto_alpha_choice_base[n0:n1],
                sev_minor_mean_choice[n0:n1],
                sev_minor_sd) - cost_factors[1]
      # price_oo_renw = price_oo_renw_base
      pi_oo_t2 = out_model$functions$get_profit(
                   price_oo_renw,
                   limits_oo_base,
                   lambda_latter_period, #lambda[N_choice_to_N][n0:n1],
                   lambda_severe_latter_period, #lambda_severe[N_choice_to_N][n0:n1],
                   pareto_alpha_choice_base[n0:n1],
                   sev_minor_mean_choice[n0:n1],
                   sev_minor_sd)
      pi_oo_t2[,flexible_oo_index] = pi_oo_t2[,flexible_oo_index] - cost_factors[2]
      pi_oo_t2[,!flexible_oo_index] = pi_oo_t2[,!flexible_oo_index] - cost_factors[3]

      revs[,j] = revs[,j] + firm_discount_factor * pr_acc_scenario *
        rowSums(cp_renw[,1:J] * price_renw)
      profits[,j] = profits[,j] + firm_discount_factor * pr_acc_scenario *
        rowSums(cp_renw[,1:J] * pi_t2) #probabiliy of choosing own firm in renewal * profit

      oo_tmp = cp_renw[,(J+1):(J+J_oo)] * price_oo_renw
      revs_oo_flex[,j] = revs_oo_flex[,j] + firm_discount_factor * pr_acc_scenario *
        rowSums(oo_tmp[,flexible_oo_index])
      revs_oo[,j] = revs_oo[,j] + firm_discount_factor * pr_acc_scenario *
        rowSums(oo_tmp)

      oo_tmp = cp_renw[,(J+1):(J+J_oo)] * pi_oo_t2
      profits_oo_flex[,j] = profits_oo_flex[,j] + firm_discount_factor * pr_acc_scenario * rowSums(oo_tmp[,flexible_oo_index])
      profits_oo[,j] = profits_oo[,j] + firm_discount_factor * pr_acc_scenario * rowSums(oo_tmp)

      tmp_weight = replicate((J+J_oo), pr_acc_scenario * cp_first_period[,j])
      utils_second_period = utils_second_period + tmp_weight * utils_renw
      cp_second_period = cp_second_period + tmp_weight * cp_renw

      ##### t>2 PROFITS #####

      price_renw3 = matrix(0, N_part, J)
      price_renw4 = matrix(0, N_part, J)
      price_oo_renw3 = matrix(0, N_part, J_oo)
      price_oo_renw4 = matrix(0, N_part, J_oo)
      pi_t3 = matrix(0, N_part, J)
      pi_oo_t3 = matrix(0, N_part, J_oo)
      pi_t4 = matrix(0, N_part, J)
      pi_oo_t4 = matrix(0, N_part, J_oo)

      if(profit_horizon > 2){
        pr_acc = out_model$functions$get_acc_weight(1, lambda_choice[n0:n1])
        exp_srchg = clm_surcharge_ftr[n0:n1] * pr_acc + (1 - pr_acc)
        price_renw3 = price_renw * R_m1_mat * exp_srchg
        price_oo_renw3 = price_oo_renw * R_oo_m1_mat * exp_srchg

        pi_t3 = out_model$functions$get_profit(price_renw3,
                                               limits_base,
                                               lambda_latter_period, #lambda[N_choice_to_N][n0:n1],
                                               lambda_severe_latter_period, #lambda_severe[N_choice_to_N][n0:n1],
                                               pareto_alpha_choice_base[n0:n1],
                                               sev_minor_mean_choice[n0:n1],
                                               sev_minor_sd) - cost_factors[1]
        pi_oo_t3 = out_model$functions$get_profit(price_oo_renw3,
                                                  limits_oo_base,
                                                  lambda_latter_period, #lambda[N_choice_to_N][n0:n1],
                                                  lambda_severe_latter_period, #lambda_severe[N_choice_to_N][n0:n1],
                                                  pareto_alpha_choice_base[n0:n1],
                                                  sev_minor_mean_choice[n0:n1],
                                                  sev_minor_sd)
        pi_oo_t3[,flexible_oo_index] = pi_oo_t3[,flexible_oo_index] - cost_factors[2]
        pi_oo_t3[,!flexible_oo_index] = pi_oo_t3[,!flexible_oo_index] - cost_factors[3]

        if(profit_horizon > 3){
          price_renw4 = price_renw3 * R_m1_mat * exp_srchg
          price_oo_renw4 = price_oo_renw3 * R_oo_m1_mat * exp_srchg

          pi_t4 = out_model$functions$get_profit(price_renw4,
                                                 limits_base,
                                                 lambda_latter_period, #lambda[N_choice_to_N][n0:n1],
                                                 lambda_severe_latter_period, #lambda_severe[N_choice_to_N][n0:n1],
                                                 pareto_alpha_choice_base[n0:n1],
                                                 sev_minor_mean_choice[n0:n1],
                                                 sev_minor_sd) - cost_factors[1]
          pi_oo_t4 = out_model$functions$get_profit(price_oo_renw4,
                                                    limits_oo_base,
                                                    lambda_latter_period, #lambda[N_choice_to_N][n0:n1],
                                                    lambda_severe_latter_period, #lambda_severe[N_choice_to_N][n0:n1],
                                                    pareto_alpha_choice_base[n0:n1],
                                                    sev_minor_mean_choice[n0:n1],
                                                    sev_minor_sd)
          pi_oo_t4[,flexible_oo_index] = pi_oo_t4[,flexible_oo_index] - cost_factors[2]
          pi_oo_t4[,!flexible_oo_index] = pi_oo_t4[,!flexible_oo_index] - cost_factors[3]

          if(profit_horizon > 4){
            "error: can't calculate welfare horizon > 4"
          }
        } # close if profit_horizon > 3
      } # close if profit_horizon > 2

      cp_matrix_p2_plus = cp_renw

      revs[,j] = revs[,j] + firm_discount_factor^2 * pr_acc_scenario *
        rowSums(cp_matrix_p2_plus[,1:J] * (price_renw3 + firm_discount_factor * price_renw4))

      profits[,j] = profits[,j] + firm_discount_factor^2 * pr_acc_scenario *
        rowSums(cp_matrix_p2_plus[,1:J] * (pi_t3 + firm_discount_factor * pi_t4))

      oo_tmp = cp_matrix_p2_plus[,(J+1):(J+J_oo)] * (price_oo_renw3 + firm_discount_factor * price_oo_renw4)
      revs_oo_flex[,j] = revs_oo_flex[,j] + firm_discount_factor * pr_acc_scenario *
        rowSums(oo_tmp[,flexible_oo_index])
      revs_oo[,j] = revs_oo[,j] + firm_discount_factor * pr_acc_scenario *
        rowSums(oo_tmp)

      # probabiliy of choosing oo firm in renewal * profit_oo
      oo_tmp = cp_matrix_p2_plus[,(J+1):(J+J_oo)] * (pi_oo_t3 + firm_discount_factor * pi_oo_t4)
      profits_oo_flex[,j] = profits_oo_flex[,j] + firm_discount_factor * pr_acc_scenario * rowSums(oo_tmp[,flexible_oo_index])
      profits_oo[,j] = profits_oo[,j] + firm_discount_factor^2 * pr_acc_scenario * rowSums(oo_tmp)

    } # close first period j loop
  } # close acc scenario loop

  return(
    list(utils_combined = utils_combined * 2/welfare_horizon,
         utils_second_period = utils_second_period * 2/welfare_horizon,
         cp_first_period = cp_first_period,
         cp_second_period = cp_second_period,
         profits_first_period = profits_first_period,
         profits_oo_first_period = profits_oo_first_period,
         profits_oo_flex_first_period = profits_oo_flex_first_period,
         profits = profits * 2/profit_horizon,
         profits_oo = profits_oo * 2/profit_horizon,
         profits_oo_flex = profits_oo_flex * 2/profit_horizon,
         revs = revs * 2/profit_horizon,
         revs_oo = revs_oo * 2/profit_horizon,
         revs_oo_flex = revs_oo_flex * 2/profit_horizon,
         flexible_oo_index = flexible_oo_index
         )
    )
}
