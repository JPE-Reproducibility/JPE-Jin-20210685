################################################################################
## clean_choice_panel.R — Clean choice panel: remove premature dropouts, derive
##   monitoring indicators (ubi_groups, ubi_fin_ind, etc.)
##
## Usage: clean_choice_panel(choice_panel)
## Returns: cleaned data.frame with monitoring group variables
## Used by: c0_build_rf_data.R, c4_data_figures.R, c5_selection_figures.R
################################################################################

clean_choice_panel <- function(choice_panel){
  
  choice_panel <- choice_panel %>%
    mutate(premature_dropout_ind = is.na(prem_renw_qt_1_amt)) %>%
    filter(!premature_dropout_ind) %>%
    mutate(ubi_sum_val_final_max = ifelse(ubi_sum_val_final_max == 0, ifelse(is.na(UbiValueNbr), 0, UbiValueNbr), ubi_sum_val_final_max)
           , score_inaccurate = ifelse(!is.na(UbiScoreNbr) & ((ubi_sum_val_final_max == 0 & ubi_sum_disc_final_max == 0) ## neither val=0/tier=0 while disc=0 implies super different attrition patterns
           ), 1, 0) ## this gets rid of 40% obs | (ubi_sum_tier_final_max == 0 & ubi_sum_disc_final_max == 0)
           # State-specific score anomalies: these exact score values indicate data quality issues
           # where scores were assigned incorrectly despite 100% program completion
           , score_inaccurate = ifelse(((ST_CD == 42 & ubi_sum_val_final_max %in% c(4.084, 4.436)) | ## PA: stayed with 10% disc
                                          (ST_CD == 34 & ubi_sum_val_final_max %in% c(3.374, 4.378)) | ## NJ: stayed 100%, 12%/1% disc
                                          (ST_CD == 48 & ubi_sum_val_final_max %in% c(3.585))), ## TX: stayed with 10% disc
                                       1,score_inaccurate) ## this is about 6%
           , tier_inaccurate = ifelse(ubi_renw_1_tier_final > 350 | ubi_sum_tier_final_max > 350, 1, 0) ## negligible
           , tier_inaccurate = ifelse((ST_CD == 34 & ubi_sum_tier_final_max == 69), ## all retained at 1% ubi discount... why?
                                      1, tier_inaccurate) ## this is about 4%
           , ubi_attrit_score_ind = (ST_CD == 42 & ubi_sum_val_final_max == 1.447 & ubi_sum_tier_final_max == 1) ## this is all attrition, corresponding to the 1.447 score /  1.447 = attrit + disc 20%,
           , ubi_optin_ind = !is.na(ubi_renw_1_stat_fin) & !is.na(UbiScoreNbr) # na UbiScoreNbr means na UbiScore Dates
           , ubi_start_ind = ifelse(ubi_optin_ind == 1, ifelse(ubi_renw_1_stat_non_fin | tier_inaccurate | score_inaccurate | ubi_attrit_score_ind, 1, 0),0)
           , ubi_fin_ind = ifelse(ubi_optin_ind == 1, ifelse(ubi_renw_1_stat_fin & !(tier_inaccurate | score_inaccurate | ubi_attrit_score_ind), 1, 0),0)
           , ubi_val_fin = ifelse(ubi_fin_ind == 1, ubi_renw_1_val_final, NA)
           , ubi_val_zero = (ubi_val_fin < 0.1) * 1
           , ubi_val_poz = ifelse(ubi_val_fin < 0.1, NA, ubi_val_fin)
           , ubi_val_fin_quantile = cut(ubi_val_poz, breaks = c(quantile(ubi_val_poz, probs = seq(0, 1, by = 0.2), na.rm = T)), labels=c("20","40","60","80","100"), include.lowest=TRUE)
           , ubi_groups = ifelse(!is.na(ubi_val_fin_quantile), ubi_val_fin_quantile, ifelse(!is.na(ubi_val_zero) & ubi_val_zero, 0.75, ifelse(ubi_start_ind, 0.5, 0)))
           , ubi_groups = factor(ubi_groups, levels = c(0,0.5,0.75,1,2,3,4,5), labels = c("opt-out", "non-fin", "0", "1", "2", "3", "4","5"))
           , rc_liab = prem_bi_ern + prem_pd_ern
           , rc_prop = prem_coll_ern + prem_comp_ern
           , rc = rc_liab + rc_prop
           , rc_af = rc_liab + prem_coll_ern
           , rc_20 = floor((rc_liab + rc_prop)/20)*20
           , stages = ifelse(is.na(ubi_rate_rr_vers), 0, ifelse(ubi_rate_rr_vers >= 3, 3, ifelse(ubi_rate_rr_vers == 0, 0, 1)))
           , stages = factor(stages)
    )
  
  choice_panel$dup <- duplicated(paste(choice_panel$POL_ID_CHAR, choice_panel$RENW, choice_panel$admin_ubi_id))
  choice_panel <- choice_panel %>% filter(!dup)
  
  return(choice_panel)
}