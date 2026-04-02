################################################################################
## getX.R — Covariate variable lists for different model specifications
##
## Usage: getX(type)
##   type = "basic"      : driver observables for score selection
##   type = "reg"        : regression covariates (RF analysis)
##   type = "estimation" : structural estimation covariates (categorical + continuous)
##
## Returns: list of variable name vectors (+ label matrix for "estimation")
## Used by: c0_sum_stat.R, c3_demand_elast.R
################################################################################

getX <- function(t) {

  if (t == "basic") {
    X <- c('x_drvr_is_female', 'x_drvr_age_rated',
           'x_drvr_yr_edu', 'x_drvr_voc_edu_ind',
           'x_drvr_hm_own_ind', 'x_drvr_hm_own_vrfy_ind', 'x_drvr_lic_oos_ind',
           'x_cred_report_ind', 'x_cred_ind', 'x_cred_score',
           'x_veh_mdl_yr',
           'tier_pref_ind',
           'tier_pop_ind', 'tier_pop_lngth', 'tier_pop_lngth_rsd', 'tier_pop_prir_liab_lim',
           'tier_acci_drvr_pt_2',
           'tier_fctr_mkt',
           'tier_acci_tot_naf_cnt', 'tier_acci_tot_aaf_cnt',
           'pmt_nbr_of_pay')
    return(list(X, X))

  } else if (t == "reg") {
    X <- c('x_drvr_is_female', 'x_drvr_age_rated',
           'x_drvr_yr_edu', 'x_drvr_voc_edu_ind',
           'x_drvr_hm_own_ind', 'x_drvr_hm_own_vrfy_ind', 'x_drvr_lic_oos_ind',
           'x_cred_report_ind', 'x_cred_ind', 'x_cred_score',
           'x_veh_mdl_yr',
           'tier_pref_ind',
           'tier_pop_lngth', 'tier_pop_lngth_rsd',
           'tier_acci_drvr_pt_2',
           'tier_fctr_mkt',
           'tier_acci_tot_naf_cnt', 'tier_acci_tot_aaf_cnt',
           'pmt_nbr_of_pay',
           'x_cred_clue_ord_ind',
           'x_loc_grg_verify_ind',
           'x_loc_zipcd_ind',
           'x_loc_zipcd_lg_inc',
           'tier_pop_ind',
           'tier_pop_prir_liab_lim')
    return(list(X, X))

  } else if (t == "estimation") {
    X_cat <- c('x_drvr_is_female', 'x_drvr_yr_edu', 'x_drvr_lic_yr',
               'x_drvr_hm_own_ind', 'x_drvr_lic_oos_ind',
               'x_cred_clue_ord_ind', 'x_veh_ls_pay_ind',
               'x_veh_mdl_yr', 'x_veh_abs_ind', 'x_veh_sd_ind', 'x_veh_len_own', 'x_veh_class_C_ind',
               'x_loc_grg_verify_ind',
               'tier_pop_yes_ind', 'tier_pop_some_ind', 'tier_pref_ind',
               'tier_acci_tot_aaf_cnt', 'tier_acci_dui_cnt')

    X_cont <- c('x_drvr_age_rated',
                'x_cred_score',
                'x_loc_popltn_dens_pct', 'x_loc_zipcd_agi',
                'tier_pop_lngth',
                'tier_acci_drvr_pt')

    X_time_invariant <- c('tier_acci_dui_cnt', 'x_veh_ls_pay_ind',
                          'x_veh_abs_ind', 'x_veh_sd_ind', 'x_veh_len_own', 'x_veh_class_C_ind')

    X_cat_lab <- c('Female Ind.', 'Years of Edu.', 'License Year Cat.',
                   'Homeowner Ind.', 'Out-of-state Ind.',
                   'Credit Report Ind.', 'Vehicle on Lease Ind.',
                   'Vehicle Model Year', 'ABS Ind.',
                   'Safe Device Ind.', 'Ownership Length Cat.', 'Class C Vehicle Ind.',
                   'Garage Verification Ind.',
                   'Prior Insurance Ind.', 'Prior Insurance with Lapse Ind.', 'Preferred Customer Ind.',
                   'Record: At-Fault Accident Count', 'Record - DUI Count')

    X_cont_lab <- c('Driver Age',
                   'Driver Credit Tier',
                   'Population Density Percentile',
                   'Zipcode AGI',
                   'Length of Prior Insurance',
                   'Record: Accident Points')

    X_label_mat <- cbind.data.frame(c(X_cat, X_cont), c(X_cat_lab, X_cont_lab))
    colnames(X_label_mat) <- c("vars", "name")

    return(list(X_cat, X_cont, X_time_invariant, X_label_mat))

  } else {
    stop('Unknown type "', t, '". Use "basic", "reg", or "estimation".')
  }
}
