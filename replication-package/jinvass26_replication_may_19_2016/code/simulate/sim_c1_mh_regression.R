################################################################################
## sim_c1_mh_regression.R — Moral hazard regressions on simulated data
##
## Mirrors codes/rf/c1_mh_regression.R from the main repo exactly.
##
## Produces CSV outputs in RF_CSV_DIR:
##   tab_2_coefs.csv + tab_2_meta.csv         (balanced panel, Table 2)
##   appendix/tab_c1_coefs.csv + tab_c1_meta.csv (unbalanced panel, Table C.1)
##   fig_4.csv                                 (unbalanced progression, Figure 4)
##   appendix/fig_c1.csv                       (balanced progression, Figure C.1)
##   appendix/fig_c2.csv + fig_c3.csv          (MH heterogeneity, Figures C.2-C.3)
##   tab_3_coefs.csv + tab_3_meta.csv          (selection, Table 3)
##
## Also produces JSON outputs in RF_REG_DIR (backward compatibility):
##   tab_2_regression.json, tab_3_regression.json, fig_4_regression.json
##   appendix/fig_c1_regression.json, appendix/tab_c1_regression.json
##   appendix/fig_c2_c3_regression.json
################################################################################

if (!exists("TABLES_DIR")) source("code/config.R")
source("code/functions/helper.R")
source("code/simulate/functions/load_sim_data.R")

ipak(c("fixest", "jsonlite"))

## ---- Helper: extract regression summary as list (for JSON) ------------------

extract_lm <- function(fit, dep_var_mean = NULL) {
  sm <- summary(fit)
  cf <- coef(sm)
  out <- list()
  out$n <- nobs(fit)
  out$r_squared <- sm$r.squared
  out$adj_r_squared <- sm$adj.r.squared
  if (!is.null(dep_var_mean)) out$dep_var_mean <- dep_var_mean

  coefficients <- list()
  for (i in seq_len(nrow(cf))) {
    nm <- rownames(cf)[i]
    coefficients[[nm]] <- list(
      estimate  = cf[i, "Estimate"],
      std_error = cf[i, "Std. Error"],
      t_value   = cf[i, "t value"],
      p_value   = cf[i, "Pr(>|t|)"]
    )
  }
  out$coefficients <- coefficients
  return(out)
}

extract_feols <- function(fit, dep_var_mean = NULL) {
  sm <- summary(fit)
  cf <- coeftable(fit)
  out <- list()
  out$n <- nobs(fit)
  out$r_squared <- fitstat(fit, "r2")[[1]]
  out$adj_r_squared <- fitstat(fit, "ar2")[[1]]
  if (!is.null(dep_var_mean)) out$dep_var_mean <- dep_var_mean

  coefficients <- list()
  for (i in seq_len(nrow(cf))) {
    nm <- rownames(cf)[i]
    coefficients[[nm]] <- list(
      estimate  = cf[i, "Estimate"],
      std_error = cf[i, "Std. Error"],
      t_value   = cf[i, "t value"],
      p_value   = cf[i, "Pr(>|t|)"]
    )
  }
  out$coefficients <- coefficients
  return(out)
}

## ---- 0.1 Load data (matching main repo) -------------------------------------

data_states <- "us"
df_mh <- load_sim_data("df_mh", data_states, 1)

# FIX: Use _norm.rds (not _prenorm.rds) to match main repo
X_mat <- as.data.frame(readRDS(file.path(SIM_PROCESSED_DIR, "X_mat_panel_us_norm.rds")))
Y_mat <- as.data.frame(readRDS(file.path(SIM_PROCESSED_DIR, "Y_mat_panel_us.rds")))

cat("  [SIM] df_mh:", nrow(df_mh), "rows\n")
cat("  [SIM] X_mat:", nrow(X_mat), "rows,", ncol(X_mat), "cols\n")
cat("  [SIM] Y_mat:", nrow(Y_mat), "rows,", ncol(Y_mat), "cols\n")

X <- colnames(X_mat)
X <- X[!X %in% c("POL_ID_CHAR", "RENW_CNT")]
Y <- colnames(Y_mat)
Y <- Y[!Y %in% c("POL_ID_CHAR", "RENW_CNT", "date_pol_eff", "ST_CD", "state_alpha")]

XY_mat <- X_mat %>% left_join(Y_mat)

df_mh <- df_mh %>%
  mutate(ubi_fin_ind   = na_to_zero(ubi_fin_ind)
         , ubi_start_ind = na_to_zero(ubi_start_ind)
         # "Finisher" = completed >=120 days of monitoring OR exited due to a claim
         , ubi_fin120_ind = (ubi_fin_ind>0 & (days_connect_raw >= 120 | ubi_end_due_to_claim > 0))*1
         , treat_int =  ubi_factor_0 - ubi_factor_1
         , last_renewal_seen = ifelse(is.na(clm_acci_renw_1), 0,
                                      ifelse(is.na(clm_acci_renw_2), 1,
                                             ifelse(is.na(clm_acci_renw_3), 2,
                                                    ifelse(is.na(clm_acci_renw_4), 3,
                                                           ifelse(is.na(clm_acci_renw_5), 4, 99)))))
  )

df_mh_save <- df_mh

## ---- Build panel: join XY_mat at MATCHING RENW_CNT for each period ----------
## (Main repo: df_reg_save built from RENW_CNT=0 base + RENW_CNT=q for each q)

mh_cols <- colnames(df_mh_save)
reg_cols <- c("POL_ID_CHAR", "treat_int", "ubi_start_ind", "ubi_fin_ind", "ubi_fin120_ind", "last_renewal_seen")
df_reg_save <- filter(df_mh_save, last_renewal_seen >= 2)[c(reg_cols, "clm_acci")] %>% left_join(filter(XY_mat, RENW_CNT == 0))
panel_renw <- data.frame()
for(q in 1:5){
  claim_varname <- paste0("clm_acci_renw_",q)
  temp <- df_mh[c(reg_cols, claim_varname)] %>% left_join(filter(XY_mat, RENW_CNT == q))
  colnames(temp)[colnames(temp) == claim_varname] <- "clm_acci"
  temp <- temp %>% filter(!is.na(clm_acci))
  panel_renw <- panel_renw %>% rbind.data.frame(temp)
}
df_reg_save <- rbind.data.frame(df_reg_save, panel_renw)

control_specifications <- list(c(), X, c(X, Y))
placebo_specifications <- c(1,2,3)

df_reg_save <- df_reg_save %>% mutate(ubi_ind = ubi_fin_ind*1)

mon_period_mean <- mean((df_reg_save %>% filter(RENW_CNT == 0 & ubi_ind > 0))$clm_acci, na.rm=T)
temp <- df_reg_save %>% filter(RENW_CNT < 1 & ubi_ind > 0) %>% left_join(df_mh_save[c("POL_ID_CHAR", "days_connect_raw")])
mean_mon_days <- mean(temp$days_connect_raw[is.finite(temp$days_connect_raw)], na.rm=T)

## ---- Helper: get_coef (matching main repo) ----------------------------------

get_coef <- function(coefs_temp, int_ind){
  all_names <- names(coefs_temp)
  name_ubi_post <- all_names[grepl("ubi_ind", all_names) & grepl("post_ind", all_names)]
  if(int_ind){
    name_treat_post <- all_names[grepl("treat_int", all_names) & grepl("post_ind", all_names)]
    out <- coefs_temp[c(name_ubi_post[1], name_treat_post[1])]
  } else {
    out <- coefs_temp[name_ubi_post[1]]
  }
  return(out)
}

## ---- TABLE 2 / TABLE C.1: balance_spec loop --------------------------------
## balanced (T) -> Tab 2; unbalanced (F) -> Tab C.1

dir.create(file.path(RF_CSV_DIR, "appendix"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(RF_REG_DIR, "appendix"), recursive = TRUE, showWarnings = FALSE)

balance_spec <- c(TRUE, FALSE)
for(b in 1:length(balance_spec)){
  filename <- paste(data_states, ifelse(balance_spec[b], "bal", "unbal"), sep="_")

  df_reg <- df_reg_save %>% mutate(post_ind = ifelse(RENW_CNT==0, 0, 1))
  if(balance_spec[b]){
    df_reg <- df_reg %>% filter(RENW_CNT %in% c(0,1,2))
  }
  df_reg_ind <- df_reg %>% filter(!(ubi_start_ind==1 & ubi_ind==0)) %>%
    mutate(ubi_ind = ubi_ind*1)
  df_reg_int <- df_reg %>% mutate(ubi_ind = ubi_start_ind*1)

  model_list_ind <- list()
  model_list_int <- list()
  model_list_placebo <- list()

  for(i in 1:length(control_specifications)) {
    control_varnames <- control_specifications[[i]]

    base_ind = "clm_acci ~ ubi_ind + post_ind + ubi_ind*post_ind"
    base_int = paste(base_ind, "treat_int + treat_int*post_ind", sep = " + ")
    if(length(control_varnames) > 0){
      base_ind = paste(base_ind, paste(control_varnames, collapse=" + "), sep = " + ")
      base_int = paste(base_int, paste(control_varnames, collapse=" + "), sep = " + ")
    }
    fit_ind <- lm(as.formula(base_ind), data = df_reg_ind)
    fit_int <- lm(as.formula(base_int), data = df_reg_int)
    model_list_ind[[i]] <- fit_ind
    model_list_int[[i]] <- fit_int
  }

  fe_ind <- "clm_acci ~ post_ind + ubi_ind*post_ind"
  fe_int <- paste(fe_ind, "treat_int + treat_int*post_ind", sep = " + ")

  fit_ind_fe <- feols(as.formula(paste(fe_ind, "| POL_ID_CHAR")), data = df_reg_ind)
  model_list_ind[[i+1]] <- fit_ind_fe

  fit_int_fe <- feols(as.formula(paste(fe_int, "| POL_ID_CHAR")), data = df_reg_int)
  model_list_int[[i+1]] <- fit_int_fe

  df_reg <- df_reg_save %>% filter(RENW_CNT > 0) %>%
    filter(!(ubi_start_ind==1 & ubi_ind==0))

  pt_ind = "clm_acci ~ ubi_ind + post_ind + ubi_ind*post_ind"
  pt_ind = paste(pt_ind, paste(c(X,Y), collapse=" + "), sep = " + ")
  for(p in placebo_specifications){
    df_reg_pt <- df_reg %>% mutate(post_ind = ifelse(RENW_CNT<=p,0,1))
    if(balance_spec[b]){
      df_reg_pt <- df_reg_pt %>% filter(RENW_CNT %in% c(p,(p+1)) & last_renewal_seen >= (p+1))
    }
    fit_placebo <- lm(as.formula(pt_ind), data = df_reg_pt)
    model_list_placebo[[p]] <- fit_placebo
  }

  all_models_flat <- c(model_list_ind, model_list_int, model_list_placebo)

  est_ind = c()
  est_int = matrix(0,length(model_list_ind),2)
  est_pt = c()

  for(i in 1:(length(control_specifications)+1)){
    est_ind[i] = get_coef(model_list_ind[[i]]$coefficients, F)
    est_int[i,] = get_coef(model_list_int[[i]]$coefficients, T)
  }
  for(i in 1:length(placebo_specifications)){
    est_pt[i] = get_coef(model_list_placebo[[i]]$coefficients, F)
  }

  pct_ind = est_ind / mon_period_mean / (mean_mon_days/180)
  pct_int = rowSums(est_int) / mon_period_mean
  pct_pt = est_pt / mon_period_mean / (mean_mon_days/180)
  mh_pct = c(pct_ind, pct_int, pct_pt)

  if(balance_spec[b]){
    v_ctrl_1d <- c("pre / post periods - ``1st diff''", c(rep("$t=0/1-2$", 8), c("$t=1/t=2$", "$t=2/t=3$", "$t=3/t=4$")))
  } else {
    v_ctrl_1d <- c("pre / post periods - ``1st diff''", c(rep("$t=0/1-5$", 8), c("$t=1/2-5$", "$t=1-2/3-5$", "$t=1-3/4-5$")))
  }

  controls_rows <- list(
    c("observables controls ($x$)", "No", "Yes", "Yes", "No", "No", "Yes", "Yes", "No",  "Yes", "Yes", "Yes"),
    c("coverage fixed effects",     "No", "No",  "Yes", "No", "No", "No",  "Yes", "No",  "Yes", "Yes", "Yes"),
    c("driver fixed effects",       "No", "No",  "No",  "Yes","No", "No",  "No",  "Yes", "No",  "No",  "No"),
    c("implied moral hazard effect (\\%)", paste0(round(mh_pct *100,2),"\\%")),
    v_ctrl_1d,
    c("treatment group - ``2nd diff''", rep("finishers", 4), rep("all monitored", 4), rep("finishers", 3))
  )
  controls_rows_df <- as.data.frame(do.call(rbind, controls_rows))

  midrule_row <- controls_rows_df[1,]
  midrule_row[1,] <- ""
  midrule_row$V1 <- "\\addlinespace\\hline"
  Ns <- vapply(all_models_flat, function(m) stats::nobs(m), integer(1))
  N_row <- c("$N$", format(Ns, big.mark = ",")); names(N_row) <- names(midrule_row)
  controls_df2 <- rbind.data.frame(midrule_row, controls_rows_df, N_row)
  for (j in 2:ncol(controls_df2)) {
    controls_df2[[j]] <- trimws(controls_df2[[j]])
  }

  ## ---- CSV output (matching main repo format) ----
  coef_rows <- list()
  for (mi in seq_along(all_models_flat)) {
    m <- all_models_flat[[mi]]
    co <- tryCatch({
      ct <- coeftable(m)  # fixest method
      data.frame(variable = rownames(ct),
                 model_id = mi,
                 estimate = ct[, 1],
                 std_error = ct[, 2],
                 stringsAsFactors = FALSE)
    }, error = function(e) {
      ct <- coef(summary(m))
      if (is.null(ct) || length(ct) == 0) return(NULL)
      if (is.matrix(ct)) {
        data.frame(variable = rownames(ct),
                   model_id = mi,
                   estimate = ct[, 1],
                   std_error = ct[, 2],
                   stringsAsFactors = FALSE)
      } else NULL
    })
    if (!is.null(co)) coef_rows[[mi]] <- co
  }
  coef_df <- do.call(rbind, coef_rows)
  meta_df <- controls_df2

  if (balance_spec[b]) {
    write.csv(coef_df, file.path(RF_CSV_DIR, "tab_2_coefs.csv"), row.names = FALSE)
    write.csv(meta_df, file.path(RF_CSV_DIR, "tab_2_meta.csv"), row.names = FALSE)
    cat("  Saved CSV: tab_2_coefs.csv, tab_2_meta.csv\n")
  } else {
    write.csv(coef_df, file.path(RF_CSV_DIR, "appendix", "tab_c1_coefs.csv"), row.names = FALSE)
    write.csv(meta_df, file.path(RF_CSV_DIR, "appendix", "tab_c1_meta.csv"), row.names = FALSE)
    cat("  Saved CSV: appendix/tab_c1_coefs.csv, appendix/tab_c1_meta.csv\n")
  }

  ## ---- JSON output (backward compatibility) ----
  if (balance_spec[b]) {
    # Build JSON structure for tab 2
    json_models <- list()
    model_names_json <- c("ind_no_ctrl", "ind_X_ctrl", "ind_XY_ctrl", "ind_driver_fe",
                          "int_no_ctrl", "int_X_ctrl", "int_XY_ctrl", "int_driver_fe",
                          "placebo_1", "placebo_2", "placebo_3")
    for (mi in seq_along(all_models_flat)) {
      m <- all_models_flat[[mi]]
      mn <- if (mi <= length(model_names_json)) model_names_json[mi] else paste0("model_", mi)
      if (inherits(m, "fixest")) {
        json_models[[mn]] <- extract_feols(m)
      } else {
        json_models[[mn]] <- extract_lm(m)
      }
    }
    tab2_out <- list(
      models = json_models,
      dep_var_mean_treated_monitoring = mon_period_mean,
      mean_monitoring_days = mean_mon_days,
      n_values = as.list(unname(Ns)),
      implied_mh_pct = as.list(unname(mh_pct * 100)),
      control_specs = controls_rows
    )
    write_json(tab2_out, file.path(RF_REG_DIR, "tab_2_regression.json"),
               pretty = TRUE, auto_unbox = TRUE, digits = 15)
    cat("  [SIM] tab_2_regression.json written\n")
  } else {
    # Build JSON structure for tab C.1 (unbalanced)
    json_models_c1 <- list()
    model_names_c1 <- c("ind_no_ctrl", "ind_X_ctrl", "ind_XY_ctrl", "ind_driver_fe",
                         "int_no_ctrl", "int_X_ctrl", "int_XY_ctrl", "int_driver_fe",
                         "placebo_1", "placebo_2", "placebo_3")
    for (mi in seq_along(all_models_flat)) {
      m <- all_models_flat[[mi]]
      mn <- if (mi <= length(model_names_c1)) model_names_c1[mi] else paste0("model_", mi)
      if (inherits(m, "fixest")) {
        json_models_c1[[mn]] <- extract_feols(m)
      } else {
        json_models_c1[[mn]] <- extract_lm(m)
      }
    }
    tab_c1_out <- list(
      models = json_models_c1,
      dep_var_mean_treated_monitoring = mon_period_mean,
      mean_monitoring_days = mean_mon_days,
      n_values = as.list(unname(Ns)),
      implied_mh_pct = as.list(unname(mh_pct * 100)),
      control_specs = controls_rows
    )
    write_json(tab_c1_out, file.path(RF_REG_DIR, "appendix", "tab_c1_regression.json"),
               pretty = TRUE, auto_unbox = TRUE, digits = 15)
    cat("  [SIM] appendix/tab_c1_regression.json written\n")
  }

  cat(filename, "\n")
}

## ---- FIGURE 4 / FIGURE C.1: balance_spec loop ------------------------------
## balanced (T) -> Fig C.1 (filter last_renewal_seen > 4)
## unbalanced (F) -> Fig 4 (NO last_renewal_seen filter)

balance_spec <- c(TRUE, FALSE)
for(b in 1:length(balance_spec)){
  filename <- paste(data_states, ifelse(balance_spec[b], "bal", "unbal"), sep="_")

  df_reg <- df_reg_save %>%
    mutate(renw_cnt_nbr = as.numeric(as.character(RENW_CNT))
         , RENW_CNT = factor(RENW_CNT)
         , ubi_ind = ubi_fin120_ind*1)

  if(balance_spec[b]){
    df_reg <- df_reg %>% filter(last_renewal_seen > 4)
  }

  by_period_ind = "clm_acci ~ ubi_ind + RENW_CNT + ubi_ind*RENW_CNT"
  by_period_ind = paste(by_period_ind, paste(c(X, Y), collapse=" + "), sep = " + ")
  lm_fit <- lm(as.formula(by_period_ind), data = df_reg)

  coefs <- summary(lm_fit)$coefficients[,1]
  ses <- summary(lm_fit)$coefficients[,2]
  index <- match(c("ubi_ind", "RENW_CNT1", "RENW_CNT2", "RENW_CNT3", "RENW_CNT4", "RENW_CNT5",
                   "ubi_ind:RENW_CNT1", "ubi_ind:RENW_CNT2", "ubi_ind:RENW_CNT3", "ubi_ind:RENW_CNT4", "ubi_ind:RENW_CNT5")
               , names(coefs))

  est <- coefs[index]
  se <- ses[index]

  vis_tbl <- as.data.frame(matrix(0,12,7))
  colnames(vis_tbl) <- c("period", "group", "est", "ub", "lb")
  vis_tbl$period <- rep(seq(6)-1, 2)
  vis_tbl$group <- c(rep("opt-in", 6),rep("opt-out", 6))
  est_unmon <- c(0, est[2:6])
  est_mon <- c(est[1], (est[1] + est[2:6] + est[7:11]))
  vis_tbl$est <- c(est_unmon,est_mon)
  vis_tbl$ub <- vis_tbl$est + 1.96*c(c(0,se[2:6]),c(se[1], se[7:11]))
  vis_tbl$lb <- vis_tbl$est - 1.96*c(c(0,se[2:6]),c(se[1], se[7:11]))

  ## CSV output
  if (balance_spec[b]) {
    write.csv(vis_tbl, file.path(RF_CSV_DIR, "appendix", "fig_c1.csv"), row.names = FALSE)
    cat("  Saved CSV: appendix/fig_c1.csv\n")
  } else {
    write.csv(vis_tbl, file.path(RF_CSV_DIR, "fig_4.csv"), row.names = FALSE)
    cat("  Saved CSV: fig_4.csv\n")
  }

  ## JSON output (backward compatibility)
  sm <- summary(lm_fit)
  cf <- coef(sm)
  coefficients_json <- list()
  for (ii in seq_len(nrow(cf))) {
    nm <- rownames(cf)[ii]
    coefficients_json[[nm]] <- list(
      estimate  = cf[ii, "Estimate"],
      std_error = cf[ii, "Std. Error"],
      t_value   = cf[ii, "t value"],
      p_value   = cf[ii, "Pr(>|t|)"]
    )
  }
  fig_json <- list(
    formula = paste(deparse(formula(lm_fit)), collapse = " "),
    n = nobs(lm_fit),
    r_squared = sm$r.squared,
    adj_r_squared = sm$adj.r.squared,
    residual_se = sm$sigma,
    dep_var_mean = mean(df_reg$clm_acci, na.rm = TRUE),
    sample_description = ifelse(balance_spec[b],
      "balanced panel (last_renewal_seen > 4)",
      "unbalanced panel, by-period MH"),
    coefficients = coefficients_json
  )

  if (balance_spec[b]) {
    write_json(fig_json, file.path(RF_REG_DIR, "appendix", "fig_c1_regression.json"),
               pretty = TRUE, auto_unbox = TRUE, digits = 15)
    cat("  [SIM] appendix/fig_c1_regression.json written\n")
  } else {
    write_json(fig_json, file.path(RF_REG_DIR, "fig_4_regression.json"),
               pretty = TRUE, auto_unbox = TRUE, digits = 15)
    cat("  [SIM] fig_4_regression.json written\n")
  }
}

## ---- MH HET GRAPHS (Figs C.2, C.3) -----------------------------------------

df_reg_save <- df_reg_save %>%
  left_join(filter(XY_mat[c("POL_ID_CHAR", "RENW_CNT", X, Y)], RENW_CNT == 0) %>%
              mutate(RENW_CNT = NULL) %>%
              rename_with(~ paste0(.x, "_0"), all_of(c(X,Y)))
  )

base_ind = "clm_acci ~ ubi_ind + post_ind + ubi_ind*post_ind"
het_XY = paste0("ubi_ind*post_ind*", paste(c(paste0(X,"_0"),paste0(Y,"_0")), collapse=" + ubi_ind*post_ind*"))
mh_het_ind = paste(base_ind, paste(c(X,Y), collapse=" + "), het_XY, sep = " + ")

df_reg <- df_reg_save %>% filter(as.numeric(RENW_CNT) <= 5) %>%
  mutate(ubi_ind = ubi_fin120_ind*1
       , post_ind = ifelse(RENW_CNT==0, 0, 1))

lm_het_fit <- lm(as.formula(mh_het_ind), data = df_reg)

varnames = rownames(summary(lm_het_fit)$coefficient)
vartarget = varnames[grepl("ubi_ind", varnames) & grepl("post_ind", varnames) & grepl("_0", varnames)];
vartarget_index = match(vartarget, varnames)
est_het_full = summary(lm_het_fit)$coefficient[vartarget_index,1];
est_het_full_se = summary(lm_het_fit)$coefficient[vartarget_index,2];

est_het_y = est_het_full[grepl("cov", names(est_het_full))]
est_het_x = est_het_full[!grepl("cov", names(est_het_full))]
est_het_se_y = est_het_full_se[grepl("cov", names(est_het_full_se))]
est_het_se_x = est_het_full_se[!grepl("cov", names(est_het_full_se))]
est_het_ub_y = est_het_y + 1.96 * est_het_se_y
est_het_ub_x = est_het_x + 1.96 * est_het_se_x
est_het_lb_y = est_het_y - 1.96 * est_het_se_y
est_het_lb_x = est_het_x - 1.96 * est_het_se_x

## CSV output for fig C.2 and C.3
df_c2 <- data.frame(
  dimension = gsub("1+$", "", gsub("_0", "", gsub("ubi_ind:post_ind:", "", names(est_het_x)))),
  est_het = est_het_x, est_het_lb = est_het_lb_x, est_het_ub = est_het_ub_x,
  stat_sig = abs(est_het_x) > 0 & est_het_lb_x * est_het_ub_x > 0
)
df_c3 <- data.frame(
  dimension = gsub("1+$", "", gsub("_0", "", gsub("ubi_ind:post_ind:", "", names(est_het_y)))),
  est_het = est_het_y, est_het_lb = est_het_lb_y, est_het_ub = est_het_ub_y,
  stat_sig = abs(est_het_y) > 0 & est_het_lb_y * est_het_ub_y > 0
)
write.csv(df_c2, file.path(RF_CSV_DIR, "appendix", "fig_c2.csv"), row.names = FALSE)
write.csv(df_c3, file.path(RF_CSV_DIR, "appendix", "fig_c3.csv"), row.names = FALSE)
cat("  Saved CSV: appendix/fig_c2.csv, appendix/fig_c3.csv\n")

## JSON output (backward compatibility)
sm_het <- summary(lm_het_fit)
cf_het <- coef(sm_het)
het_coefficients <- list()
for (ii in seq_len(nrow(cf_het))) {
  het_coefficients[[rownames(cf_het)[ii]]] <- list(
    estimate  = cf_het[ii, "Estimate"],
    std_error = cf_het[ii, "Std. Error"],
    t_value   = cf_het[ii, "t value"],
    p_value   = cf_het[ii, "Pr(>|t|)"]
  )
}
fig_c2c3_out <- list(
  formula = paste(deparse(formula(lm_het_fit)), collapse = " "),
  n = nobs(lm_het_fit),
  r_squared = sm_het$r.squared,
  adj_r_squared = sm_het$adj.r.squared,
  residual_se = sm_het$sigma,
  dep_var_mean = mean(df_reg$clm_acci, na.rm = TRUE),
  sample_description = "MH heterogeneity across observables and coverage",
  coefficients = het_coefficients
)
write_json(fig_c2c3_out, file.path(RF_REG_DIR, "appendix", "fig_c2_c3_regression.json"),
           pretty = TRUE, auto_unbox = TRUE, digits = 15)
cat("  [SIM] appendix/fig_c2_c3_regression.json written\n")

## ---- TABLE 3: Selection regression -----------------------------------------

post_period_mean <- mean((df_reg_save %>% filter(RENW_CNT > 0))$clm_acci, na.rm=T)

df_reg <- df_reg_save %>% filter(RENW_CNT == 0) %>%
  filter(!(ubi_start_ind==1 & ubi_fin120_ind==0)) %>%
  mutate(ubi_ind = ubi_fin120_ind*1)

select_ind_base <- "clm_acci ~ ubi_ind"
model_select_list <- list()
for(i in 1:length(control_specifications)){
  if(length(control_specifications[[i]]) > 0){
    select_ind = paste(select_ind_base, paste(control_specifications[[i]], collapse=" + "), sep = " + ")
  } else {
    select_ind = select_ind_base
  }
  lm_select_fit <- lm(as.formula(select_ind), data = df_reg)
  model_select_list[[i]] <- lm_select_fit
}

est_select = c()
for(i in 1:length(control_specifications)){
  est_select[i] = model_select_list[[i]]$coefficients["ubi_ind"]
}

select_pct = est_select / post_period_mean

## CSV output for Tab 3
coef_rows_3 <- list()
for (mi in seq_along(model_select_list)) {
  m <- model_select_list[[mi]]
  ct <- summary(m)$coefficients
  coef_rows_3[[mi]] <- data.frame(
    variable = rownames(ct),
    model_id = mi,
    estimate = ct[, 1],
    std_error = ct[, 2],
    stringsAsFactors = FALSE
  )
}
coef_df_3 <- do.call(rbind, coef_rows_3)
meta_df_3 <- data.frame(
  row_label = c("observables controls (x)", "coverage fixed effects", "risk reduction (%)",
                paste0("N_", seq_along(model_select_list))),
  value = c("No/Yes/Yes", "No/No/Yes",
            paste(round(select_pct * 100, 2), collapse = "/"),
            sapply(model_select_list, function(m) nobs(m)))
)
write.csv(coef_df_3, file.path(RF_CSV_DIR, "tab_3_coefs.csv"), row.names = FALSE)
write.csv(meta_df_3, file.path(RF_CSV_DIR, "tab_3_meta.csv"), row.names = FALSE)
cat("  Saved CSV: tab_3_coefs.csv, tab_3_meta.csv\n")

## JSON output (backward compatibility)
json_models_3 <- list()
model_names_3 <- c("no_ctrl", "X_ctrl", "XY_ctrl")
for (mi in seq_along(model_select_list)) {
  m <- model_select_list[[mi]]
  mn <- model_names_3[mi]
  json_models_3[[mn]] <- extract_lm(m, mean(df_reg$clm_acci, na.rm = TRUE))
}
tab3_out <- list(
  models = json_models_3,
  post_period_mean = post_period_mean,
  select_pct = mean(df_reg$ubi_ind, na.rm = TRUE)
)
write_json(tab3_out, file.path(RF_REG_DIR, "tab_3_regression.json"),
           pretty = TRUE, auto_unbox = TRUE, digits = 15)
cat("  [SIM] tab_3_regression.json written\n")

## ---- Cleanup ---------------------------------------------------------------

rm(list = intersect(ls(), c("df_mh", "df_mh_save", "df_reg", "df_reg_save",
                             "df_reg_ind", "df_reg_int", "df_reg_pt",
                             "XY_mat", "X_mat", "Y_mat", "panel_renw",
                             "lm_het_fit", "lm_fit", "lm_select_fit",
                             "model_list_ind", "model_list_int", "model_list_placebo",
                             "model_select_list", "all_models_flat",
                             "fit_ind", "fit_int", "fit_ind_fe", "fit_int_fe")))
gc()
cat("  [SIM] sim_c1_mh_regression.R done.\n")
