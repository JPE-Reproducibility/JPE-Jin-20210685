################################################################################
## get_next_rrev.R — Calculate next rate revision date per state
##
## Usage: get_next_rrev(dataset, rrev_date_var_name, state_cd_var_name)
## Returns: dataset with rrev_order_asc, rrev_order_desc columns added
## Used by: c3_demand_elast.R
################################################################################

get_next_rrev <- function(dataset, rrev_date_var_name, state_cd_var_name){
  
  as_date_70 <- function(date){
    out = as.Date(date, origin = "1970-01-01")
    return(out)
  }
  
  dataset$ST_CD <- unlist(dataset[state_cd_var_name])
  temp = dataset
  temp$rrev_date_base <- unlist(dataset[rrev_date_var_name])
  
  
  sum_rrev_dates <- temp %>% group_by(ST_CD, rrev_date_base) %>% #summarise(  count = n()) %>% ungroup() %>%
    summarise(next_rrev_date = NA)
  # sum_rrev_dates$count <- NULL
  
  sum_rrev_dates_new <- data_frame()
  
  for (st in unique(temp$ST_CD))
  { sum_rrev_dates_st <- sum_rrev_dates %>% filter(ST_CD == st)
    sum_rrev_dates_st$rrev_date_base <- sum_rrev_dates_st$rrev_date_base[order(sum_rrev_dates_st$rrev_date_base)]
    sum_rrev_dates_st <- sum_rrev_dates_st %>%
      mutate(rrev_order_asc = row_number()
             , rrev_order_desc = rank(desc(row_number())))
    sum_rrev_dates_st$next_rrev_date[1:nrow(sum_rrev_dates_st)-1] <- sum_rrev_dates_st$rrev_date_base[2:nrow(sum_rrev_dates_st)]
    sum_rrev_dates_st$next_rrev_date <- as_date_70(sum_rrev_dates_st$next_rrev_date)
    sum_rrev_dates_new <- rbind.data.frame(sum_rrev_dates_new, sum_rrev_dates_st)
  }
  
  dataset$rrev_date_base <- unlist(dataset[rrev_date_var_name])
  dataset <- dataset %>% left_join(sum_rrev_dates_new, by = c("ST_CD", "rrev_date_base")) %>%
    rename( new_rrev_date_rank_asc = rrev_order_asc
          , new_rrev_date_rank_desc = rrev_order_desc) %>%
    select(-rrev_date_base)
  
  return(dataset) #dataset[c("rrev_date_next", "rrev_order_asc", "rrev_order_desc")]
} 