//Jan 2025; Last edit: yizhou.jin@utoronto.ca
functions {
  vector log_sum_exp_v(matrix summable){
      int J = cols(summable);
      int N = rows(summable);
      vector[N] output;

      for (n in 1:N)
        output[n] = log_sum_exp(summable[n,:]);

      return output;
  }
  matrix get_loglikelihood(
    matrix util, vector sigma, int num_periods
  ){
    int J = cols(util);
    matrix[rows(util), J] u_over_sigma = util ./ rep_matrix(sigma * num_periods, J);
    return(u_over_sigma - rep_matrix(log_sum_exp_v(u_over_sigma), J));
  }
  
  matrix get_nested_loglikelihood(
   matrix util,
   vector sigma,
   int num_periods,
   real nesting_param,
   int J_std // Number of products in the standard (first) nest
 ){
   int J = cols(util);
   int N = rows(util);
   int J_tm = J - J_std;

   // 1. Scale all utilities by sigma (same as before)
   // This represents V_ni / sigma for all products i and consumers n
   matrix[N, J] u_over_sigma = util ./ rep_matrix(sigma * num_periods, J);

   // 2. Separate the scaled utilities into their respective nests
   matrix[N, J_std] u_std = u_over_sigma[:, 1:J_std];
   matrix[N, J_tm] u_tm = u_over_sigma[:, (J_std + 1):J];

   // 3. Calculate the Inclusive Value (IV) for each nest
   vector[N] iv_std = log_sum_exp_v(u_std);
   vector[N] iv_tm = log_sum_exp_v(u_tm);

   // 4. Calculate the log-sum-exp of the upper-level (nest choice)
   vector[N] top_level_lse = log_sum_exp_v(
                                 append_col(nesting_param * iv_std,
                                            nesting_param * iv_tm)
                               );

   // 5. Assemble the final log-likelihood for each product in each nest
   // LL = (V/sigma) + (lambda - 1)*IV - top_level_lse
   
   // For the standard nest
   matrix[N, J_std] ll_std = u_std + rep_matrix(
                                       (nesting_param - 1.0) * iv_std - top_level_lse,
                                       J_std
                                     );
                                     
   // For the telematics nest
   matrix[N, J_tm] ll_tm = u_tm + rep_matrix(
                                      (nesting_param - 1.0) * iv_tm - top_level_lse,
                                      J_tm
                                    );

   // 6. Combine the log-likelihood matrices and return
   return append_col(ll_std, ll_tm);
 }
 
  matrix get_hierarchical_loglikelihood(
   matrix utils_std,
   matrix utils_tm,
   matrix utils_oo,
   real rho
 ){
   int N = rows(utils_std);
   int J_std = cols(utils_std);
   int J_tm = cols(utils_tm);
   int J_oo = cols(utils_oo);

   // --- Part 1: Analyze the Lower-Level Choice ({std, tm} world) ---
   
   // 1a. Calculate inclusive values for the standard and telematics sub-nests.
   vector[N] iv_std = log_sum_exp_v(utils_std);
   vector[N] iv_tm = log_sum_exp_v(utils_tm);

   // 1b. Calculate the log-sum-exp for the lower-level choice.
   vector[N] lse_stage1 = log_sum_exp_v(
                              append_col(rho * iv_std, rho * iv_tm)
                            );

   // 1c. Calculate the log-likelihood for each std/tm plan *within Stage 1*.
   // This is log P(plan_i | super-alternative was chosen).
   matrix[N, J_std] ll_std_stage1 = utils_std + rep_matrix(
                                                  (rho - 1.0) * iv_std - lse_stage1,
                                                  J_std
                                                );
   matrix[N, J_tm] ll_tm_stage1 = utils_tm + rep_matrix(
                                                  (rho - 1.0) * iv_tm - lse_stage1,
                                                  J_tm
                                                );

   // --- Part 2: Analyze the Upper-Level Choice ({super-alternative, oo plans}) ---

   // 2a. Calculate the utility of the super-alternative. This is the expected
   // maximum utility (welfare) from the lower-level nested logit model.
   vector[N] util_super = (1.0 / rho) * lse_stage1;
   
   // 2b. Calculate the log-sum-exp for the upper-level standard logit choice.
   matrix[N, J_oo + 1] u_stage2 = append_col(util_super, utils_oo);
   vector[N] lse_stage2 = log_sum_exp_v(u_stage2);

   // --- Part 3: Assemble the Final Log-Likelihoods ---

   // 3a. The final LL for an 'oo' plan is its probability from Stage 2.
   matrix[N, J_oo] ll_oo = utils_oo - rep_matrix(lse_stage2, J_oo);

   // 3b. The final LL for a 'std' or 'tm' plan is:
   // log P(super-alternative in Stage 2) + log P(plan | Stage 1)
   vector[N] log_prob_super = util_super - lse_stage2;

   matrix[N, J_std] ll_std = ll_std_stage1 + rep_matrix(log_prob_super, J_std);
   matrix[N, J_tm] ll_tm = ll_tm_stage1 + rep_matrix(log_prob_super, J_tm);

   // 4. Combine all final log-likelihoods and return.
   // The order is standard, then telematics, then outside options.
   return append_col(ll_std, append_col(ll_tm, ll_oo));
 }
 
  vector get_welfare(
    matrix util, vector sigma, int num_periods
  ){
    return(num_periods * sigma .* log_sum_exp_v(util ./ rep_matrix(sigma * num_periods, cols(util))));
  }

 vector get_nested_welfare(
   matrix util,
   vector sigma,
   int num_periods,
   real nesting_param,
   int J_std // Number of products in the standard (first) nest
 ){
   int J = cols(util);
   int N = rows(util);

   // 1. Scale all utilities by sigma (same as before)
   matrix[N, J] u_over_sigma = util ./ rep_matrix(sigma * num_periods, J);

   // 2. Separate the scaled utilities into their respective nests
   matrix[N, J_std] u_std = u_over_sigma[:, 1:J_std];
   matrix[N, J - J_std] u_tm = u_over_sigma[:, (J_std + 1):J];

   // 3. Calculate the Inclusive Value (IV) for each nest
   vector[N] iv_std = log_sum_exp_v(u_std);
   vector[N] iv_tm = log_sum_exp_v(u_tm);

   // 4. Calculate the log-sum-exp of the upper-level (nest choice)
   vector[N] top_level_lse = log_sum_exp_v(
                                 append_col(nesting_param * iv_std,
                                            nesting_param * iv_tm)
                               );

   // 5. Calculate final welfare by scaling the top-level log-sum-exp
   // This converts the value back to the original utility units
   return ( (sigma * num_periods) ./ nesting_param .* top_level_lse );
 }
 
  // matrix get_loglikelihood_w_diff_sigma(
  //   matrix util,
  //   matrix util_tm,
  //   vector sigma,
  //   vector sigma_logit_tm_factor,
  //   int num_periods
  // ){
  //   int N = rows(util);
  //   int J = cols(util);
  //   int J_tm = cols(util_tm);
  //   matrix[N, J] u_over_s = util ./ rep_matrix(sigma * num_periods, J);
  //   matrix[N, J_tm] u_tm_over_s_tm = util_tm ./ rep_matrix((sigma * num_periods) .* sigma_logit_tm_factor, J_tm);
  //   matrix[N, (J + J_tm)] u_all = append_col(u_over_s, u_tm_over_s_tm);
  //   return(u_all - rep_matrix(log_sum_exp_v(u_all), (J + J_tm)));
  // }
  // 
  // vector get_welfare_w_diff_sigma(
  //   matrix util, 
  //   matrix util_tm,
  //   vector sigma,
  //   vector sigma_logit_tm_factor,
  //   int num_periods
  // ){
  //   int N = rows(util);
  //   int J = cols(util);
  //   int J_tm = cols(util_tm);
  //   matrix[N, J] u_over_s = util ./ rep_matrix(sigma * num_periods, J);
  //   matrix[N, J_tm] u_tm_over_s_tm = util_tm ./ rep_matrix((sigma * num_periods) .* sigma_logit_tm_factor, J_tm);
  //   matrix[N, (J + J_tm)] u_all = append_col(u_over_s, u_tm_over_s_tm);
  //   return(num_periods * sigma .* log_sum_exp_v(u_all));
  // }
  
  // vector add_eps( //this is assigning the per-I (person) private errors to the right choice observation
  //   int I,
  //   array[] int N_choice_to_I_choice,
  //   array[] int n_choice_regime_cutoffs,
  //   vector base,
  //   vector leps,
  //   array[] int d_by_block
  // ){
  //   int N_choice = num_elements(base);
  //   int I_leps = d_by_block[1] + d_by_block[2] + d_by_block[4];
  // 
  //   vector[N_choice] out = rep_vector(0,N_choice);
  //   out[1:d_by_block[1]] = leps[1:d_by_block[1]];
  //   out[n_choice_regime_cutoffs[2]:(n_choice_regime_cutoffs[2]-1+d_by_block[2])] = leps[(d_by_block[1]+1):(d_by_block[1] + d_by_block[2])];
  //   out[n_choice_regime_cutoffs[4]:(n_choice_regime_cutoffs[4]-1+d_by_block[4])] = leps[(d_by_block[1] + d_by_block[2] + 1):I_leps];
  //   
  //   for(n in n_choice_regime_cutoffs[5]:(n_choice_regime_cutoffs[5]-1+d_by_block[5])){
  //     out[n] = out[N_choice_to_I_choice[n]];
  //   }
  //   for(n in n_choice_regime_cutoffs[6]:(n_choice_regime_cutoffs[6]-1+d_by_block[6])){
  //     out[n] = out[N_choice_to_I_choice[n]];
  //   }
  //   
  //   return out + base;
  // }
  // vector add_eps_I (
  //   array[] int n_choice_regime_cutoffs,
  //   vector base,
  //   vector leps,
  //   array[] int d_by_block
  // ){
  //   int I = num_elements(base); 
  //   int I_leps = d_by_block[1] + d_by_block[2] + d_by_block[4];
  //   vector[I_leps] leps_cleaned = rep_vector(0, I_leps);
  //   if(num_elements(leps) < I_leps){ //this means that it's tm error that does not apply to block 1, unlike risk aversion or mm
  //     leps_cleaned[(d_by_block[1]+1):I_leps] = leps;
  //   } else {
  //     leps_cleaned = leps;
  //   }
  //   
  //   vector[I] out = base;
  //   out[1:d_by_block[1]] += leps_cleaned[1:d_by_block[1]];
  //   out[n_choice_regime_cutoffs[2]:(n_choice_regime_cutoffs[2]-1+d_by_block[2])] += leps_cleaned[(d_by_block[1]+1):(d_by_block[1] + d_by_block[2])];
  //   out[n_choice_regime_cutoffs[4]:(n_choice_regime_cutoffs[4]-1+d_by_block[4])] += leps_cleaned[(d_by_block[1] + d_by_block[2] + 1):I_leps];
  //   
  //   return(out);
  // }
  
  // vector pareto_lpdf_v(vector x, real a, vector b){
  //   int N = rows(b);
  //   vector[N] out;
  //   for(n in 1:N){
  //     out[n] = pareto_lpdf(x[n] | a, b[n]);
  //   }
  //   return out;
  // }
  // vector pareto_lccdf_v(vector x, real a, vector b){
  //   int N = rows(b);
  //   vector[N] out;
  //   for(n in 1:N){
  //     out[n] = pareto_lccdf(x[n] | a, b[n]);
  //   }
  //   return out;
  // }
  // vector lognormal_lpdf_v(vector x, vector mu, real sigma){
  //   int N = rows(mu);
  //   vector[N] out;
  //   for(n in 1:N){
  //     out[n] = lognormal_lpdf(x[n] | mu[n], sigma);
  //   }
  //   return out;
  // }
  // vector lognormal_lcdf_v(real x, vector mu, real sigma){
  //   int N = rows(mu);
  //   vector[N] out;
  //   for(n in 1:N){
  //     out[n] = lognormal_lcdf(x | mu[n], sigma);
  //   }
  //   return out;
  // }
  // vector normal_lpdf_v(vector x, vector mu, real sigma){
  //   int N = rows(mu);
  //   vector[N] out;
  //   for(n in 1:N){
  //     out[n] = normal_lpdf(x[n] | mu[n], sigma);
  //   }
  //   return out;
  // }
  // vector poisson_log_lpmf_v(array[] int x, vector lambda){
  //   int N = rows(lambda); 
  //   vector[N] out;
  //   for(n in 1:N){
  //     out[n] = poisson_log_lpmf(x[n] | lambda[n]);
  //   }
  //   return out;
  // }
    //   vector pareto_cdf_v(real y, real m, vector a){
  //   int N = rows(a); 
  //   vector[N] output;
  // 
  //   for(n in 1:N){
  //     output[n] = pareto_cdf(y | m, a[n]);
  //   }
  // 
  //   return output;
  // }
  //   matrix m2_accident_oop(  
  //     // θ, ℓ are pareto parameter; y is coverage limit (severity)
  //     vector alpha_pareto, // θ
  //     vector w, // w
  //     row_vector limits // y_j, J vector
  // ) {
  //     int N = rows(alpha_pareto); 
  //     int J = cols(limits); 
  //     matrix[N,J] moment_oop;
  //     //10K is the minimum of severe/major claims
  //     vector[N] tmp1 = (alpha_pareto * 10) ./ (alpha_pareto - 1); //temp first moment
  //     vector[N] tmp2 = (alpha_pareto * 100) ./ (alpha_pareto - 2); //temp second moment
  //     vector[N] tmp_total = 10.0 ./ w; //assuming that total loss cannot exceed annual income (previously tried max coverage of 500K)
  //     real tmp_claim;
  //     vector[N] m1_total_buffer = (
  //       1 - pow(tmp_total, (alpha_pareto - 1))) ./
  //        (1 - pow(tmp_total, alpha_pareto)); //this is before vector tmp1
  // 
  //     vector[N] m1_delta; 
  //     vector[N] m2_total; 
  //     vector[N] m2_claim; 
  //     vector[N] p;
  // 
  //     for (j in 1:J){ // (θ ℓ^k) / (θ - k) × (1 - (ℓ/y)^(θ - k)) / (1 - (ℓ/y)^(θ))
  //         tmp_claim = 10.0 / limits[j];
  //         m1_delta = tmp1 .* (m1_total_buffer
  //                         - (1 - pow(tmp_claim, (alpha_pareto - 1))) ./ (1 - pow(tmp_claim, alpha_pareto)));
  //         m2_total = tmp2 .* (1 - pow(tmp_total, (alpha_pareto - 2))) ./ (1 - pow(tmp_total, alpha_pareto));
  //         m2_claim = tmp2 .* (1 - pow(tmp_claim, (alpha_pareto - 2))) ./ (1 - pow(tmp_claim, alpha_pareto));
  //         p = pareto_cdf_v(limits[j], 10, alpha_pareto);
  // 
  //         //E[OOP^2] = E[(total - claim)^2] = E[total^2] + E[claim^2] - 2 * E[total * claim]
  //         //E[total * claim] = 
  //         //   Pr(total < cov) * E[claim^2] + 
  //         //   Pr(total > cov) * cov * E[total | total > cov]
  //         moment_oop[:,j] = m2_total + m2_claim 
  //                         - 2 * (p .* m2_claim + limits[j] * (1 - p) .* m1_delta); 
  //     }
  //     return moment_oop;
  // }
  // vector poisson_pmf_v(int x, vector l){
  //   int N = rows(l); 
  //   vector[N] output;
  //   for(n in 1:N){
  //     output[n] = exp(poisson_lpmf(x | l[n]));
  //   }
  //   return output;
  // }
  // vector pareto_cdf_v(real y, real m, vector a){
  //   int N = rows(a); 
  //   vector[N] output;
  // 
  //   for(n in 1:N){
  //     output[n] = pareto_cdf(y | m, a[n]);
  //   }
  // 
  //   return output;
  // }
  
} // close functions block

data {
  
}

model {
  
}

