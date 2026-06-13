################################################################################
## panel_renw_clean.R — Clean and derive renewal quote variables
##
## Usage: panel_renw_clean(panel_renw_input, panel_input)
## Returns: cleaned data.frame with derived premium, tier, discount variables
## Used by: c3_demand_elast.R, c4_data_figures.R
################################################################################

panel_renw_clean <- function(panel_renw_input, panel_input)
{
  ########################## Last Edit: YJ 8/16/17 #################################################
  ##################################################################################################
  
  
  # Med_zip_inc = median(as.vector(panel_renw_input$zipcode_agi), na.rm = T)
  
  ##### 2. DATA CLEANING #####
    
  panel_renw_input <- panel_renw_input %>% 
    mutate(
  #### DATES ####
        date_pol_eff = as.Date(RENW_QT_POL_EFF_DT)
      , date_yr_char = substr(as.character(RENW_QT_POL_EFF_DT),1,4)
      , date_rt_rev = as.Date(RENW_QT_RT_REV_DT)
      , date_qt_iss = RENW_QT_QT_ISS_DT
      , date_qt_chg = as.Date(RENW_QT_QT_CHNG_DT)
      , date_prev_pol_expr = RENW_QT_PREV_POL_EXPR_DT
      , date_pol_expr = RENW_QT_POL_EXPR_DT
      , date_cust_since = RENW_QT_CUST_SINCE_DT
      , date_coh_incp = RENW_QT_COH_INCP_DT
      , date_qt_eff = RENW_QT_QT_EFF_DT
      , date_qt_eff_incp = RENW_QT_INIT_QT_EFF_DT
      , date_qt_acpt = RENW_QT_RENW_ACPT_DT
  #### RISK INFO - X ####
  # DRIVER INFO
    , x_drvr_is_female = (RENW_QT_DRVR_SEX_CD == 'F') * 1
    , x_drvr_age_rated = RENW_QT_DRVR_RATE_ON_AGE
    , x_drvr_married_ind = ifelse(is.na(RENW_QT_DRVR_MRTL_STAT_CD), 0, ifelse(RENW_QT_DRVR_MRTL_STAT_CD == "S", 0, 1))
    , RENW_QT_MAX_ED_CD = ifelse((RENW_QT_MAX_ED_CD == " " | RENW_QT_MAX_ED_CD == "X" | is.na(RENW_QT_MAX_ED_CD)), 0, RENW_QT_MAX_ED_CD)
    , x_drvr_edu = as.numeric(as.character(RENW_QT_MAX_ED_CD))
    # Education code → years: 1=some HS(9), 2-3=HS/vocational(12), 4-5=some college/assoc(14),
    #   6=bachelor(16), 7=graduate(18), else=default(14)
    , x_drvr_yr_edu = ifelse(x_drvr_edu == 1, 9,
                             ifelse(x_drvr_edu == 2, 12,
                                    ifelse(x_drvr_edu == 3, 12,
                                           ifelse(x_drvr_edu == 4, 14,
                                                  ifelse(x_drvr_edu == 5, 14,
                                                         ifelse(x_drvr_edu == 6, 16,
                                                                ifelse(x_drvr_edu == 7, 18, 14)))))))
    , x_drvr_voc_edu_ind = ifelse(x_drvr_edu == 3, 1, 0) # vocational school
    , x_drvr_hm_own_ind = (RENW_QT_OWN_HM_IND == "O") * 1
    , x_drvr_hm_own_vrfy_ind = ifelse(RENW_QT_HOP_VER_CD %in% c("Y", "V"), 0, 1)
    , x_drvr_lic_oos_ind = (as.character(RENW_QT_DRVR_DRVR_LIC_ST_CD) == as.character(RENW_QT_ALPHA_ST_CD))
    , x_drvr_lic_oos_ind = 1 - ifelse(is.na(x_drvr_lic_oos_ind), 1,  x_drvr_lic_oos_ind * 1)
    , x_ftr_drvr_lic_state = RENW_QT_DRVR_DRVR_LIC_ST_CD
    , x_ftr_drvr_occ_cd = RENW_QT_DRVR_OCC_CD
    , x_ftr_drvr_rel_ins = RENW_QT_DRVR_DRVR_REL_INS_CD
  # DRIVER CREDIT INFO
    , x_cred_report_ind = ifelse(RENW_QT_CRED_SCORE_STAT_CD == "O", 1, 0)
    , x_cred_ind = ifelse(is.na(RENW_QT_NEW_CRED_SCORE_NBR), 0, 
                        ifelse(RENW_QT_NEW_CRED_SCORE_NBR == 0 & RENW_QT_CRED_SCORE_STAT_CD != "O", 0, 
                               ifelse(RENW_QT_NEW_CRED_SCORE_NBR > 8999, 0, 1)))
    , x_cred_score = ifelse(x_cred_ind == 1, RENW_QT_NEW_CRED_SCORE_NBR, CRED_SCORE_PLACEHOLDER) # sentinel above all valid scores (valid range ~100-400)
    , x_fctr_cred_mkt_raw = RENW_QT_UPLD_FR_MKT
    , x_fctr_cred_mkt_pre_cred_raw = RENW_QT_UPLD_UND_MKT
    , x_fctr_cred_cd = as.factor(ifelse(is.na(RENW_QT_CRED_SCORE_CD), "XX", RENW_QT_CRED_SCORE_CD))
  # VEHICLE INFO
    , date_yr = as.numeric(date_yr_char)
    , x_veh_mdl_yr = ifelse(is.na(RENW_QT_VEH_MODL_YR_DT), 1980, ifelse(RENW_QT_VEH_MODL_YR_DT < 1900, 1980, RENW_QT_VEH_MODL_YR_DT))
    , x_veh_mdl_yr_missing_ind = ifelse(is.na(RENW_QT_VEH_MODL_YR_DT) | RENW_QT_VEH_MODL_YR_DT < 1900, 1, 0)
  # DRIVER HH INFO
    , x_hh_struct = as.numeric(ifelse(is.na(as.numeric(RENW_QT_HH_STRUCT_IND)), 0, as.numeric(RENW_QT_HH_STRUCT_IND)))
    , x_hh_avg_cd = RENW_QT_HSHLD_AVG_CD
    , x_ftr_hh_pers_occ_used = factor(ifelse(is.na(RENW_QT_PERS_USE_OCC_CD), 'X',RENW_QT_PERS_USE_OCC_CD))
  #### TIER INFO ####
  # Combined                                                                   
    , tier_pref_ind = ifelse(RENW_QT_MKT == "PR", 1, 0)
  # POP
    , tier_pop_tenure_extend = ifelse(is.na(RENW_QT_TENURE_CS_XPND)|RENW_QT_TENURE_CS_XPND<0, 0, RENW_QT_TENURE_CS_XPND)
    , tier_pop_lngth = ifelse(RENW_QT_LNGTH_PRIR_INS_CD %in% c("N","X", "Z"), 0, match(RENW_QT_LNGTH_PRIR_INS_CD, toupper(letters[1:4])))
    , tier_pop_lngth_rsd = ifelse(RENW_QT_LNGTH_RSD_CD %in% c("N","X", "Z"), 0, match(RENW_QT_LNGTH_RSD_CD, toupper(letters[1:2])))
    , tier_pop_prev_carir_standard_ind = ifelse(is.na(RENW_QT_PRIR_CARY_TYP), 1, ifelse(RENW_QT_PRIR_CARY_TYP == "S", 1, 0))
  # PRIOR ACCIDENTS
    , tier_acci_drvr_pt_2 = RENW_QT_DRVR_DRVR_PNT_CNT
    , tier_acci_drvr_pt_liab = RENW_QT_DRVR_DRVR_BIPD_PNT_CNT
    , tier_acci_drvr_pt_coll = RENW_QT_DRVR_DRVR_COLL_PNT_CNT
    , tier_acci_drvr_pt_comp = RENW_QT_DRVR_DRVR_COMP_PNT_CNT
    , tier_acci_drvr_pt_prop = tier_acci_drvr_pt_coll + tier_acci_drvr_pt_comp
  # MARKETING TIERS
    , tier_fctr_mkt = factor(ifelse(is.na(RENW_QT_MKT), 'X', RENW_QT_MKT))
    , tier_mkt_pref_ind = ifelse(RENW_QT_MKT == "PR", 1, 0)
    , tier_mkt_ultpref_ind = ifelse(RENW_QT_MKT == "UL", 1, 0)
    , tier_mkt_ns_ind = ifelse(RENW_QT_MKT == "NS", 1, 0)
    , tier_mkt_st_ind = ifelse(RENW_QT_MKT == "ST", 1, 0)
    , tier_mkt_mm_ind = ifelse(RENW_QT_MKT == "MM", 1, 0)
    , tier_acci_tot_naf_cnt = ifelse(is.na(RENW_QT_TOT_NAF_VIOL_CNT), 0, RENW_QT_TOT_NAF_VIOL_CNT)
    , tier_acci_tot_aaf_cnt = ifelse(is.na(RENW_QT_INCPT_AAF_CNT), 0, RENW_QT_INCPT_AAF_CNT)
    , tier_acci_nbr_mon_cln = RENW_QT_NBR_MO_CLN
    , tier_acci_aaf_nbr_mon_cln_5_yr = ifelse(is.na(RENW_QT_AAF_MO_CLN),99,RENW_QT_AAF_MO_CLN)
  # Premiums
    , prem_renw_amt = RENW_QT_RENW_QT_PREM_AMT
    , prem_renw_rate_chg_pct = RENW_QT_RATE_CHNG_PCT #what's the difference?
    , prem_renw_chg_amt = RENW_QT_RENW_PREM_CHNG_AMT
    , prem_renw_chg_pct = RENW_QT_ORGN_PREM_CHNG_PCT
    , prem_renw_chg_amt_orgn = RENW_QT_ORGN_PREM_CHNG_AMT
    , prem_renw_chg_pct_orgn = RENW_QT_ORGN_PREM_CHNG_PCT
    , panel_prem_expr_amt = RENW_QT_PREV_TOT_PREM_EXPR
  # Premium Cap, Stablization
    , prem_cap_ind = ifelse(is.na(RENW_QT_PREM_WAS_CAP_THIS_TERM), 0, RENW_QT_PREM_WAS_CAP_THIS_TERM)
    , prem_cap_pct = ifelse(is.na(RENW_QT_RT_CAP_PCT), 0, RENW_QT_RT_CAP_PCT)
    , prem_cap_comp_cd = ifelse(is.na(RENW_QT_RT_CAP_COMP_CD), 0, RENW_QT_RT_CAP_COMP_CD)
    , prem_cap_acq_pct = ifelse(is.na(RENW_QT_ACQST_EXP_CAP_PCT), 0, RENW_QT_ACQST_EXP_CAP_PCT)
    , prem_cap_opex_stbl_fctr = ifelse(is.na(RENW_QT_OPS_EXP_RT_STBL_FCT), 0, RENW_QT_OPS_EXP_RT_STBL_FCT)
    , prem_cap_stbl_convrg_fct = RENW_QT_CNVRG_RT_STBL_FCT
    , prem_cap_stbl_pol_fct = RENW_QT_POL_RT_STBL_FCT
  #### PMT ####   
    , pmt_nbr_of_pay = RENW_QT_NBR_OF_PAY
    , panel_pmt_req_down = ifelse(is.na(REQ_DWNPMT_PCT), 0, REQ_DWNPMT_PCT)
    , pmt_req_down = ifelse(is.na(RENW_QT_REQ_DWNPMT_PCT), 0, RENW_QT_REQ_DWNPMT_PCT)
    , pmt_eft_incp = ifelse(is.na(RENW_QT_EFT_INCP_IND), 0, (RENW_QT_EFT_INCP_IND == 'Y')*1)
    , pmt_eft = ifelse(is.na(RENW_QT_EFT_IND), 0, (RENW_QT_EFT_IND == 'Y')*1)
  #### DISCOUNT ####  
    , disc_ded_sav_ind = ifelse(is.na(RENW_QT_DED_SAV_BANK_IND), 0, ifelse(RENW_QT_DED_SAV_BANK_IND == "Y", 1, 0))
    , disc_ded_sav_amt = ifelse(is.na(RENW_QT_DED_SAV_AMT), 0, RENW_QT_DED_SAV_AMT)
  # system reference codes
    , admin_vin = RENW_QT_VEH_VIN_NBR
    , admin_ppro = ifelse(RENW_QT_PPRO_IND == "Y", 1, 0)
    , admin_init_qt_dt = RENW_QT_INIT_QT_EFF_DT
    , admin_last_cred_ord_dt = RENW_QT_LAST_CRED_ORDR_DT
    , admin_co_cd = RENW_QT_CO_CD
    , admin_pol_activation_dt = RENW_QT_LST_PM_TRANS_DT
    , admin_second_cnvrg_dt = RENW_QT_SCND_CNVRG_EVNT_DT
    , admin_cred_score_method = RENW_QT_SCORE_MTHD_CD
  # Location
    , admin_grg_zipcode = RENW_QT_VEH_GRG_PSTL_CD
    , panel_admin_grg_zipcode_prev_renw = GRG_PSTL_CD
    , admin_rewrite_cd = ifelse(is.na(RENW_QT_RWRT_RSN_CD), "N", RENW_QT_RWRT_RSN_CD)
    , admin_pol_term_nbr = RENW_QT_POL_TERM_NBR
  ) 

  cols <- colnames(panel_renw_input)
  X <- cols[((grepl("x_", cols))|(grepl("tier_", cols))|(grepl("disc_", cols))|(grepl("prem_cap_", cols))|(grepl("pmt_", cols)))
           &(!(grepl("panel_", cols)))]
  cols_to_keep <- c(X, cols[(((grepl("date_", cols))|(grepl("prem_renw_", cols))|
                              (grepl("admin_", cols))|(grepl("panel_", cols))))]
                    )
  
  
  panel_renw_cleaned <- panel_renw_input[c('POL_ID_CHAR', 'RENW_QT_POL_ID_CHAR', 'RENW_CNT'
                               , 'RENW_QT_ALPHA_ST_CD'
                               , 'RENW_QT_RATE_REV_ID'
                               , 'RENW_QT_RENW_ACPT_IND', 'RENW_QT_RENW_LPS_IND'
                               , cols_to_keep
                               , 'PRIOR_CARRIER_MIX_GROUP', 'PRIOR_CARRIER_GROUP', 'PRIOR_CARRIER_NAME'
                               )]
  
  invariant_X_cols <- c('POL_ID_CHAR', 'RENW_CNT',
                        'x_cred_clue_ord_ind',
                        'x_loc_grg_verify_ind',
                        'x_loc_grg_adrs_verify_ind',
                        'x_loc_zipcd_ind',
                        'x_loc_zipcd_agi',
                        'x_loc_zipcd_lg_inc',
                        'x_loc_popltn_dens_grp',
                        'x_loc_popltn_dens_pct',
                        'x_hh_hlth_ins_ind',
                        'tier_pop_ind',
                        'tier_pop_cont_ins_ind',
                        'tier_pop_no_ind',
                        'tier_pop_some_ind',
                        'tier_pop_yes_ind',
                        'tier_pop_not_over_min_ind',
                        'tier_pop_limit',
                        'tier_pop_prir_liab_lim',
                        'tier_pop_prir_liab_lim_missing',
                        'tier_pop_prog_lim')
  invariant_X_cols <- unique(intersect(invariant_X_cols, colnames(panel_input)))
  invariant_X_next_panel <- panel_input[invariant_X_cols] 
  # %>%
  #             mutate(RENW_CNT = RENW_CNT - 1)
  panel_renw_cleaned <- panel_renw_cleaned %>% left_join(invariant_X_next_panel)

  ##check for discrepancies
  source('code/functions/getX.R')
  vars_X <- getX('reg')[[1]] #; X <- c(X[X != "x_loc_zipcd_agi"], "lg_inc"); 
  print(paste("reg X not in panel_renw:", setdiff(vars_X, colnames(panel_renw_cleaned))))
  
return(panel_renw_cleaned)
}

#invariant: need to bring from panel:
# # , x_cred_clue_ord_ind = ifelse(CLU_ORD_IND == "O", 1, 0)
# 
# # , x_loc_grg_verify_ind = ifelse(CURR_GRG_VLD_SRC %in% c("A", "H", "U"), 0, 1) 
# # , x_loc_grg_adrs_verify_ind = ifelse(GRG_ADRS_VLD_CD %in% c("A", "H", "U"), 0, 1) 
# # , x_loc_zipcd_ind = !is.na(zipcode_agi)
# # , x_loc_zipcd_agi = ifelse(is.na(zipcode_agi), Med_zip_inc, zipcode_agi)
# # , x_loc_popltn_dens_grp = ifelse(is.na(DNSTY_POPLT_GRP),0,ifelse(is.na(as.numeric(DNSTY_POPLT_GRP)), 0, as.numeric(DNSTY_POPLT_GRP))) 
# # , x_loc_popltn_dens_pct = as.numeric(DNSTY_POPLT_PCT)
# # , x_hh_hlth_ins_ind = ifelse(is.na(HLTH_INS_IND), 0, ifelse(is.na(HLTH_INS_IND == "Y"), 1, 0))
# 
# # , tier_pop_ind = ifelse(POP_IND == "Y", 1, 0)
# # , tier_pop_cont_ins_ind = ifelse(CONT_INS_IND == "N", 0, 1)
# # , tier_pop_no_ind = ifelse(PRIR_INS_CD == "C", 1, 0)
# # , tier_pop_some_ind = ifelse(PRIR_INS_CD == "B", 1, 0)
# # , tier_pop_yes_ind = ifelse(PRIR_INS_CD == "A", 1, 0)
# # , tier_pop_not_over_min_ind = ifelse(is.na(OVER_MINM_LIM_CD), 0, ifelse(OVER_MINM_LIM_CD == "N", 1, 0))
# # , tier_pop_limit = ifelse(is.na(OVER_MINM_LIM_CD), 0, ifelse(OVER_MINM_LIM_CD == "N", 0, as.numeric(OVER_MINM_LIM_CD)))
# # , tier_pop_prir_liab_lim = ifelse(is.na(PRIR_BI_LIM_CD), 0, ifelse(as.numeric(PRIR_BI_LIM_CD)>0, as.numeric(PRIR_BI_LIM_CD), 0))
# # , tier_pop_prir_liab_lim = ifelse(is.na(tier_pop_prir_liab_lim), 0, ifelse(as.numeric(tier_pop_prir_liab_lim)>0, as.numeric(tier_pop_prir_liab_lim), 0)) ### May not be numeric
# # , tier_pop_prir_liab_lim_missing = ifelse(is.na(PRIR_BI_LIM_CD), 1, 0)
# # , tier_pop_prog_lim = ifelse(PROG_PRIR_LIM_CD %in% c("N", ""), 0, as.numeric(PROG_PRIR_LIM_CD))
# # , tier_pop_prog_lim = ifelse(is.na(tier_pop_prog_lim), 0, tier_pop_prog_lim)    


# , tier_pop_prev_carir_left_midterm_ind = ifelse(is.na(PREV_POL_CNCL_CD), 0, ifelse(PREV_POL_CNCL_CD == "Y", 1, 0))
# , tier_fctr_pop_prev_carir_cd_fctr = as.factor(ifelse(is.na(PREV_CARIR_CD), 9999,PREV_CARIR_CD)) 

# , tier_fctr_mkt_incp = factor(ifelse(is.na(ORGN_MKT_CD), 'X', ORGN_MKT_CD))



#need to find before categorizing whether invariant or uesless:
# , x_ftr_drvr_lic_stat = ifelse(is.na(DRVR_LIC_STAT_CD), 'ND', DRVR_LIC_STAT_CD)
# , x_drvr_lic_stat_valid_ind = ifelse(x_ftr_drvr_lic_stat %in% c("V", "B"), 1, 0)
# , x_drvr_lic_stat_permit_ind = ifelse(x_ftr_drvr_lic_stat == "P", 1, 0)
# , x_drvr_lic_stat_expr_ind = ifelse(x_ftr_drvr_lic_stat == "E", 1, 0)
# , x_cred_elig_ind = ifelse(DRVR_FIN_ELGBL_CD == "Y", 1, 0)
# , x_veh_abs_ind = ifelse(is.na(VEH_ABS_IND),0, ifelse(VEH_ABS_IND == 'S' | VEH_ABS_IND == 'Y', 1, 0))
# , x_veh_sd_ind = ifelse(VEH_SAFE_DEVC_IND == 'S', 1, 0)
# , x_veh_high_perf_ind = ifelse(VEH_HIGH_PFRM_IND == 'Y' | ISS_HIGH_PFRM_IND == "Y", 1, 0)
# , x_veh_len_own = ifelse(is.na(VEH_LEN_OF_OWN_CD), 0,
#                          ifelse(VEH_LEN_OF_OWN_CD == "A", 1, 
#                                 ifelse(VEH_LEN_OF_OWN_CD == "B", 2, 
#                                        ifelse(VEH_LEN_OF_OWN_CD == "C", 3, 
#                                               ifelse(VEH_LEN_OF_OWN_CD == "D", 4, 0)))))
# , x_veh_class_C_ind = ifelse(MDL_TYP_CD == "C", 1,0)
# , x_veh_lg_state_amt = ifelse(is.na(VEH_STATED_AMT)|as.numeric(VEH_STATED_AMT) <= 0, 0, log(as.numeric(VEH_STATED_AMT)))
# , x_veh_state_amt_missing = ifelse(is.na(VEH_STATED_AMT)|VEH_STATED_AMT < 10, 1, 0)
# , x_veh_ls_pay_ind = ifelse(LS_PAY_IND == "Y", 1 ,0)
# , x_ftr_veh_use_cd = as.factor(as.numeric(ifelse(is.na(veh_use_cd), 0, veh_use_cd)))
# , tier_fctr_mkt = factor(ifelse(is.na(CNSM_MKT_TIER), 'X', CNSM_MKT_TIER))
# , tier_fctr_mkt_incp = factor(ifelse(is.na(INCP_CNSM_MKT_TIER), 'X', INCP_CNSM_MKT_TIER))
# , pmt_late_pay_cnt = LATE_PAY_CNT
# , pmt_bad_pmt_cnt = ifelse(is.na(TOT_NONPAY_CNCL_NOTE_CNT_UW), 0, 1)
# , pmt_nsf_pay_cnt = NSF_PAY_CNT
# , pmt_bad_debt_fct = ifelse(is.na(BAD_DEBT_FCT), 1, BAD_DEBT_FCT)
# , pmt_outside_financing_ind = ifelse(OPF_IND == "Y", 1, 0)

# , disc_inet_qt_ind = ifelse(is.na(INET_QT_DISC_IND), 0, ifelse(INET_QT_DISC_IND == "Y", 1, 0))
# , disc_military_ind = ifelse(is.na(MLTRY_DISC_IND), 0, 1)
# , disc_e_sign_ind = ifelse(MTHD_SIGN_CD == "E", 1, 0)
# , disc_comm_umbrella_incpt = ifelse(is.na(MULTI_POL_RISK_OTHR), 0, ifelse(MULTI_POL_RISK_OTHR == "I", 1, 0))
# , disc_comm_umbrella_renw = ifelse(is.na(MULTI_POL_RISK_OTHR), 0, ifelse(MULTI_POL_RISK_OTHR == "R", 1, 0))
# , disc_home_incpt = ifelse(is.na(MULTI_POL_RISK_PROG_HO), 0, ifelse(MULTI_POL_RISK_PROG_HO == "I", 1, 0))
# , disc_home_renw = ifelse(is.na(MULTI_POL_RISK_PROG_HO), 0, ifelse(MULTI_POL_RISK_PROG_HO == "R", 1, 0))
# , disc_specline_incpt = ifelse(is.na(MULTI_POL_RISK_SL), 0, ifelse(MULTI_POL_RISK_SL == "I", 1, 0))
# , disc_specline_renw = ifelse(is.na(MULTI_POL_RISK_SL), 0, ifelse(MULTI_POL_RISK_SL == "R", 1, 0))
# , disc_paprls_mon_cnt = PAPRLS_MNTHS_CNT
# , disc_paprls_ind = ifelse(PAPRLS_STAT_CD == "E", 1, 0)
# , disc_home_ind = ifelse(is.na(PHA_HOME_CONDO_IND), 0, ifelse(PHA_HOME_CONDO_IND == "Y", 1, 0))
# , disc_ded_sav_ind_alt = ifelse(DSB_PKG_IND == "Y", 1, 0)
# , disc_email_subs_ind_incpt = ifelse(is.na(EMAIL_INCP_SBSCRB), 0, ifelse(EMAIL_INCP_SBSCRB == "Y", 1, 0))
# , disc_email_subs_ind = ifelse(is.na(EMAIL_SBSCRB_IND), 0, ifelse(EMAIL_SBSCRB_IND == "Y", 1, 0))

# , cov_ind_fullcov = ifelse(as.numeric(RENW_QT_VEH_VEH_FULL_COV_CD) > 0, 1 ,0) -- ALL "O"

# , admin_dma_cd = as.factor(ifelse(is.na(as.numeric(DMA_CD)), 0, as.character(DMA_CD)))
# , admin_region_mgmr_cd = ifelse(is.na(RGN_CD), 0, RGN_CD)
# , admin_zip_extend = EXTND_ZIP_CD
# , admin_zip_extend_last_4_digit = ZIP_CD_EXTN
# , admin_party_char = as.factor(ifelse(is.na(PNI_PARTYID), "0", as.character(PNI_PARTYID)))



# , rr_uw_score = ifelse(is.na(UW_TIER_CMBN_SCR), mean(UW_TIER_CMBN_SCR, na.rm = TRUE), UW_TIER_CMBN_SCR)
# , rr_uw_score_missing = ifelse(is.na(UW_TIER_CMBN_SCR), 1, 0)
# , rr_uw_bhvr_score = ifelse(is.na(UW_RNW_BHVR_SCR), mean(UW_RNW_BHVR_SCR, na.rm = TRUE), UW_RNW_BHVR_SCR)
# , rr_uw_bhvr_score_missing = ifelse(is.na(UW_RNW_BHVR_SCR), 1, 0)
# , rr_pre_cred_score = ifelse(is.na(PRE_CREDIT_SCORE), mean(PRE_CREDIT_SCORE, na.rm = TRUE), PRE_CREDIT_SCORE)
# , rr_pre_cred_score_missing = ifelse(is.na(PRE_CREDIT_SCORE), 1, 0)

# , admin_qt_srce = as.factor(ifelse(is.na(QT_SRCE), "0", as.character(QT_SRCE)))
# , admin_rpt_buiz_cd = RPT_BSNS_CD


#useless
# , x_ftr_drvr_occ_stat = RENW_QT_DRVR_EMP_STAT_CD
# , x_cred_drvr_pos = CRED_DRVR_POS ## irrelevant for single driver pol
# , x_cred_drvr_vers = CRED_DRVR_VRSN_NBR ## irrelevant for single driver pol
# , x_hh_sps_excl_ind = ifelse(is.na(EXCL_SPS_INCP_IND), 0, ifelse(EXCL_SPS_INCP_IND == "Y", 1, 0))
# , tier_acci_drvr_pt_3 = MAX_DRV_PT
# , tier_acci_drvr_pt = max(TOT_DRV_PT, RENW_QT_DRVR_DRVR_PNT_CNT)
# , tier_acci_maj_vio_cnt = MAJ_VIOL_CNT
# , tier_acci_dui_cnt = DUI_VIOL_CNT
# , tier_acci_viol_imnty_ind = ifelse(VIOL_IMNTY_IND == "Y", 1, 0)
# , tier_acci_num_acci_forgive = OMITTED_INCIDENT_CNT
# , tier_acci_omit_clm_cnt = ifelse(is.na(OMITTED_INCIDENT_CNT), 0, OMITTED_INCIDENT_CNT)
# , tier_acci_nbr_rcnt_clm = ifelse(is.na(NBR_OF_RECENT_CLM), 0, NBR_OF_RECENT_CLM)
# , tier_acci_thft_clm_cnt = ifelse(is.na(TOT_NBR_OF_THFT_VNDLSM_CMP_CLM), 0, TOT_NBR_OF_THFT_VNDLSM_CMP_CLM)
# , tier_acci_prir_thft_vndl_cnt = TOT_NBR_OF_THFT_VNDLSM_CMP_CLM
# , tier_acci_prir_pip_cnt = TOTAL_PRIOR_PIP_CLAIMS
# , tier_acci_otherfirm_cmp_cnt = ifelse(is.na(TOT_NPROG_CMP_CNT), 0, TOT_NPROG_CMP_CNT)
# , tier_acci_otherfirm_naf_cnt = ifelse(is.na(TOT_NPROG_NAF_CNT_NRT), 0, TOT_NPROG_NAF_CNT_NRT)
# , tier_acci_otherfirm_cmp_missing = ifelse(is.na(TOT_NPROG_CMP_CNT)|(TOT_NPROG_CMP_CNT<0), 1, 0)
# , tier_acci_otherfirm_naf_missing = ifelse(is.na(TOT_NPROG_NAF_CNT_NRT)|(TOT_NPROG_NAF_CNT_NRT<0), 1, 0)
# , tier_acci_ownfirm_aaf_cnt = ifelse(is.na(TOT_PROG_AAF_CNT_UW)|TOT_PROG_AAF_CNT_UW<0, 0, TOT_PROG_AAF_CNT_UW) 
# , tier_acci_ownfirm_cmp_cnt = ifelse(is.na(TOT_PROG_CMP_CNT_NRT)|TOT_PROG_CMP_CNT_NRT<0, 0, TOT_PROG_CMP_CNT_NRT) 
# , tier_acci_ownfirm_pun_cnt = ifelse(is.na(TOT_PROG_PUN_CNT_NRT)|TOT_PROG_PUN_CNT_NRT<0, 0, TOT_PROG_PUN_CNT_NRT) 
# , tier_acci_ownfirm_pua_cnt = ifelse(is.na(TOT_PROG_PUA_CNT_NRT)|TOT_PROG_PUA_CNT_NRT<0, 0, TOT_PROG_PUA_CNT_NRT) 
# , tier_acci_ownfirm_naf_cnt = ifelse(is.na(TOT_PROG_NAF_CNT_NRT)|TOT_PROG_NAF_CNT_NRT<0, 0, TOT_PROG_NAF_CNT_NRT) 
# # TOT_PRIR_COMP_CLM_CNT omitted
# , tier_mkt_pref_ind_incp = ifelse(ORGN_MKT_CD == "PR", 1, 0)
# , tier_mkt_ultpref_ind_incp = ifelse(ORGN_MKT_CD == "UL", 1, 0)
# , tier_mkt_ns_ind_incp = ifelse(ORGN_MKT_CD == "NS", 1, 0)
# , tier_mkt_st_ind_incp = ifelse(ORGN_MKT_CD == "ST", 1, 0)
# , tier_mkt_mm_ind_incp = ifelse(ORGN_MKT_CD == "MM", 1, 0)
# , tier_fctr_mkt_social_grp_cd = as.factor(ifelse(SCL_GRP_CD %in% c("??", ""), "Unknown", as.character(SCL_GRP_CD)))
# , tier_fctr_mkt_prizm_clstr_cd_char = as.factor(PRIZM_CLSTR_CD)
# , tier_fctr_mkt_urban_cd = as.factor(URBN_RSD_CD) # C = Second City/Metro Suburb; R = Rural; S = Metro Suburb; T = Rural / Small Towns; U = Metro Urban
# , tier_fctr_mkt_cmnty_cd = as.factor(CMNTY_CD) # community code (Boston, Community within MA vs within NH or VT) INTERNAL, no relations to prizm
# , pmt_pif_25_ind = ifelse(pct_down >= 0.25, 1, 0)
# , pmt_pif_25_ind = ifelse(is.na(pmt_pif_25_ind), 0, pmt_pif_25_ind)

# , prem_chg_poga_dif_pct = ifelse(is.na(POGA_DIF_PCT), 0, POGA_DIF_PCT)
# , prem_chg_poga_prem_chng_amt = ifelse(is.na(POGA_PREM_CHNG_AMT), 0, POGA_PREM_CHNG_AMT)
# , disc_incpt_pref_ind = ifelse(INCPT_PRFRD_IND == "P", 1, 0)
# , prir_cary_cd = as.character(PREV_CARIR_CD)

# , prem_cap_loading_pct = RENW_QT_ACQST_EXP_CAP_PCT

# , admin_convrg_dt = CNVRG_EVENT_DT
# , admin_prob_cd = as.factor(ifelse(is.na(PROB_STAT), "0", as.character(PROB_STAT)))
# , admin_pa_vers_incp = INCP_PROD_ALGN_VER
# , admin_pa_vers = PROD_ALGN_VRSN
# , admin_pol_stat_cd_fctr = as.factor(POL_STAT_CD)
# , admin_fin_bucket_for_mt = STD_FIN_BUCKT
# , admin_fin_tier_for_mt = STD_FIN_TIER
# , admin_symb_file_dt = SYM_FILE_DT
# , admin_symb_tbl_dt = SYM_TBL
# , admin_cred_vendor_cd = VEND_CD
# , admin_ubi_id = UBI_PARTCPNT_ID

# , disc_prem_capped_ind = ifelse(is.na(RENW_QT_PREM_WAS_CAP_THIS_TERM), 0, 1)
# , disc_prem_cap_pct = RENW_QT_RT_CAP_PCT
