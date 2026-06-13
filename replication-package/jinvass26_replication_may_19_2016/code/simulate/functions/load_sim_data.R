################################################################################
## load_sim_data.R — Load panel data from consolidated data_rf.csv
##
## Reads data_rf.csv once, caches in memory, returns column subsets by panel_name.
## Supports: "panel", "panel_viol", "panel_exps", "panel_renw",
##           "panel_ubi_renw", "choice_panel", "df_mh", "st_ubi_dates"
################################################################################

if (!exists(".sim_env") || !is.environment(.sim_env)) {
  .sim_env <- new.env(parent = emptyenv())
}

load_sim_data <- function(panel_name, data_states = "us", dbug_pct = 1) {

  data_dir <- SIM_CACHE_DIR

  ## Read and cache data_rf.csv on first call
  if (!exists(".sim_cache", envir = .sim_env)) {
    rf_path <- file.path(data_dir, "data_rf.csv")
    cat("[load_sim_data] Loading", rf_path, "(first call, caching)...\n")
    data_rf <- read.csv(rf_path, stringsAsFactors = FALSE)
    data_rf$POL_ID_CHAR <- as.character(data_rf$POL_ID_CHAR)
    assign(".sim_cache", data_rf, envir = .sim_env)
  }
  data_rf <- get(".sim_cache", envir = .sim_env)

  ## Column exclusion patterns
  viol_cols <- grep("^VIOL_", colnames(data_rf), value = TRUE)
  renw_qt_cols <- grep("^RENW_QT_", colnames(data_rf), value = TRUE)
  ubi_device_cols <- c("UBI_ENROLL_IND", "UBI_FIN_IND", "UBI_VALUE_MAX",
                       "UBI_DISC_FINAL", "UBI_DISC_30DAY")
  df_mh_only_cols <- c("ubi_factor_0", "ubi_factor_1", "ubi_start_ind",
                        "ubi_end_due_to_claim",
                        "days_connect_raw", "clm_acci",
                        paste0("clm_acci_renw_", 1:5), "last_renewal_seen")
  cp_only_cols <- c("ubi_ind", "ubi_fin_ind", "ubi_group",
                    "prem_renw_qt_1_amt",
                    "ubi_sum_val_final_max", "UbiValueNbr", "UbiScoreNbr",
                    "ubi_sum_disc_final_max", "ubi_sum_tier_final_max",
                    "ubi_renw_1_tier_final", "ubi_renw_1_stat_fin",
                    "ubi_renw_1_stat_non_fin", "ubi_renw_1_val_final",
                    "admin_ubi_id", "RENW")
  renw_extra_cols <- c("RENW_SFX_NBR", "GRG_PSTL_CD",
                       "PRIOR_CARRIER_MIX_GROUP", "PRIOR_CARRIER_GROUP",
                       "PRIOR_CARRIER_NAME", "REQ_DWNPMT_PCT")

  if (panel_name == "panel") {
    exclude <- c(viol_cols, renw_qt_cols, ubi_device_cols, df_mh_only_cols,
                 cp_only_cols, renw_extra_cols, "DRVR_YR_LIC", "WRT_EXPS_CNT")
    cols <- setdiff(colnames(data_rf), exclude)
    dataset <- data_rf[, cols]
    if ("x_loc_zipcd_agi" %in% colnames(dataset) && !"x_loc_zipcd_lg_inc" %in% colnames(dataset))
      dataset$x_loc_zipcd_lg_inc <- log(dataset$x_loc_zipcd_agi)

  } else if (panel_name == "panel_viol") {
    viol_path <- file.path(data_dir, "data_rf_viol.csv")
    cat("[load_sim_data] Loading", viol_path, "\n")
    dataset <- read.csv(viol_path, stringsAsFactors = FALSE)
    dataset$POL_ID_CHAR <- as.character(dataset$POL_ID_CHAR)

  } else if (panel_name == "panel_exps") {
    cols <- c("POL_ID_CHAR", "RENW_CNT", "WRT_EXPS_CNT")
    dataset <- data_rf[, intersect(cols, colnames(data_rf))]

  } else if (panel_name == "panel_renw") {
    rrev_path <- file.path(data_dir, "data_rf_rrev.csv")
    cat("[load_sim_data] Loading", rrev_path, "\n")
    dataset <- read.csv(rrev_path, stringsAsFactors = FALSE)
    dataset$POL_ID_CHAR <- as.character(dataset$POL_ID_CHAR)
    data_states <- "IL"

  } else if (panel_name == "panel_ubi_renw") {
    ubi_cols <- c("POL_ID_CHAR", "RENW_CNT", "RENW_SFX_NBR",
                  "UBI_ENROLL_IND", "UBI_FIN_IND",
                  "UBI_VALUE_MAX", "UBI_DISC_FINAL", "UBI_DISC_30DAY")
    cols <- intersect(ubi_cols, colnames(data_rf))
    dataset <- data_rf[, cols]
    if ("UBI_ENROLL_IND" %in% colnames(dataset))
      dataset <- dataset[!is.na(dataset$UBI_ENROLL_IND), ]
    data_states <- "IL"

  } else if (panel_name == "choice_panel") {
    exclude <- c(viol_cols, renw_qt_cols, df_mh_only_cols, renw_extra_cols,
                 ubi_device_cols, "WRT_EXPS_CNT", "DRVR_YR_LIC",
                 "min_ubi_1", "min_ubi_3", "ubi_start_date")
    cols <- setdiff(colnames(data_rf), exclude)
    dataset <- data_rf[, cols]

  } else if (panel_name == "df_mh") {
    df_mh_cols <- c("POL_ID_CHAR", "state_alpha", "ST_CD",
                    "ubi_fin_ind", "ubi_start_ind",
                    "ubi_factor_0", "ubi_factor_1",
                    "ubi_end_due_to_claim", "days_connect_raw",
                    "clm_acci", paste0("clm_acci_renw_", 1:5),
                    "last_renewal_seen")
    if ("ubi_fin_ind_drv" %in% colnames(data_rf))
      df_mh_cols[df_mh_cols == "ubi_fin_ind"] <- "ubi_fin_ind_drv"
    cols <- intersect(df_mh_cols, colnames(data_rf))
    dataset <- data_rf[data_rf$RENW_CNT == 0, cols]
    dataset <- dataset[!duplicated(dataset$POL_ID_CHAR), ]
    if ("ubi_fin_ind_drv" %in% colnames(dataset))
      colnames(dataset)[colnames(dataset) == "ubi_fin_ind_drv"] <- "ubi_fin_ind"

  } else if (panel_name == "st_ubi_dates") {
    cols <- c("ST_CD", "state_alpha", "ubi_start_date", "min_ubi_1", "min_ubi_3")
    cols <- intersect(cols, colnames(data_rf))
    dataset <- unique(data_rf[, cols])
    dataset <- dataset[!is.na(dataset$ST_CD), ]
    return(dataset)

  } else {
    stop("[load_sim_data] Unknown panel_name: ", panel_name)
  }

  ## State filtering
  if (data_states != "us" && "state_alpha" %in% colnames(dataset)) {
    if (data_states == "3_state") {
      rf_prof <- jsonlite::fromJSON(file.path(SIM_DATA_DIR, "data_profile_rf.json"))
      dataset <- dataset[dataset$state_alpha %in% rf_prof$dgp_params$states, ]
    } else {
      dataset <- dataset[dataset$state_alpha == data_states, ]
    }
  }

  ## Debug subsampling
  if (dbug_pct < 1) {
    dataset <- dataset[sample(seq_len(nrow(dataset)),
                              floor(dbug_pct * nrow(dataset)),
                              replace = FALSE), ]
  }

  cat("[load_sim_data]", panel_name, ":", nrow(dataset), "rows,", ncol(dataset), "cols\n")
  return(dataset)
}
