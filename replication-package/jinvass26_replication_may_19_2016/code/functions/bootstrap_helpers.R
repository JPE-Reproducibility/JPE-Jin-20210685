################################################################################
## bootstrap_helpers.R — Shared helpers for parsing bootstrap CSVs and
## generating LaTeX parameter tables from CmdStan output.
##
## Sourced by: codes/results/c1_param_tables.R
################################################################################

#' Parse a CmdStan CSV: skip comment lines, return named numeric vector
parse_cmdstan_csv <- function(path) {
  lines <- readLines(path)
  data_lines <- lines[!startsWith(lines, "#")]
  header <- strsplit(data_lines[1], ",")[[1]]
  values <- as.numeric(strsplit(data_lines[2], ",")[[1]])
  names(values) <- header
  values
}

#' Parse original bootstrap CSV but only extract non-gq columns (theta_* params).
#' This avoids parsing 627K gq_ values we don't need.
parse_original_csv_thetas <- function(path) {
  lines <- readLines(path)
  data_lines <- lines[!startsWith(lines, "#")]
  header <- strsplit(data_lines[1], ",")[[1]]

  # Find where gq_ columns start (everything before = theta/param columns)
  first_gq <- which(startsWith(header, "gq_"))[1]
  n_keep <- if (is.na(first_gq)) length(header) else first_gq - 1

  vals_all <- strsplit(data_lines[2], ",", fixed = TRUE)[[1]]
  vals <- as.numeric(vals_all[1:n_keep])
  names(vals) <- header[1:n_keep]
  vals
}

#' Capped SD: cap to [Q25, Q75] then compute sd (equivalent to sd_winsor(0.25))
sd_capped <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) <= 1) return(NA_real_)
  q25 <- quantile(x, 0.25, names = FALSE, type = 7)
  q75 <- quantile(x, 0.75, names = FALSE, type = 7)
  if (q25 == q75) return(sd(x))
  sd(pmin(pmax(x, q25), q75))
}

#' Extract a single (parameter, statistic) pair across bootstrap samples
extract_parameter_statistic <- function(parameter_name, statistic_name, df_list) {
  results <- map_dbl(df_list, ~{
    df <- .x
    if (parameter_name %in% df$parameter) {
      value <- df %>%
        filter(parameter == parameter_name) %>%
        pull(!!sym(statistic_name))
      # Take first match (may have duplicates from merged main+secondary)
      if (is.numeric(value) && length(value) >= 1) return(value[1])
    }
    NA_real_
  })

  # Bootstrap 0 = point estimate (full-sample MLE)
  bs0_df <- df_list[grep("id-0\\.csv$", names(df_list))]
  bs0_result <- NA_real_
  if (length(bs0_df) > 0 && is.data.frame(bs0_df[[1]])) {
    val <- bs0_df[[1]] %>%
      filter(parameter == parameter_name) %>%
      pull(!!sym(statistic_name))
    if (length(val) >= 1 && is.numeric(val)) bs0_result <- val[1]
  }

  tibble(
    estimate = bs0_result,
    mean = mean(results, na.rm = TRUE),
    sd = sd(results, na.rm = TRUE),
    q2.5 = quantile(results, probs = 0.025, na.rm = TRUE),
    q97.5 = quantile(results, probs = 0.975, na.rm = TRUE)
  )
}

#' Build a summary table from a list of bootstrap data frames
get_summary_table_from_list <- function(df_list, parameters, parameters_pretty,
                                        statistics, statistics_pretty) {
  results <- crossing(parameter = parameters, statistic = statistics) %>%
    mutate(result = map2(parameter, statistic,
                         ~extract_parameter_statistic(.x, .y, df_list))) %>%
    unnest(result)

  param_map <- setNames(parameters_pretty, parameters)
  stat_map <- setNames(statistics_pretty, statistics)
  results <- results %>%
    mutate(parameter_pretty = ifelse(parameter %in% names(param_map),
                                     param_map[parameter], parameter))
  results <- results %>%
    mutate(
      statistic_raw = statistic,
      statistic = ifelse(statistic %in% names(stat_map),
                         stat_map[statistic], statistic)
    )
  results
}

#' Generate a LaTeX table with estimate + SE rows
generate_latex_table_with_se <- function(
    results,
    custom_params = NULL,
    custom_stats = NULL,
    custom_rounding = NULL,
    first_col_header = "Parameter",
    use_pretty_param_names = TRUE,
    caption = "Model Parameter Summary",
    label = "tab:model_params_summary",
    notes = NULL,
    placement = "htbp"
) {
  params <- if (!is.null(custom_params)) custom_params else unique(results$parameter)
  stats <- if (!is.null(custom_stats)) custom_stats else unique(results$statistic)

  # Map raw -> pretty
  pretty_map <- results %>%
    distinct(parameter, parameter_pretty) %>%
    mutate(parameter_pretty = ifelse(is.na(parameter_pretty) | parameter_pretty == "",
                                     parameter, parameter_pretty)) %>%
    { setNames(.$parameter_pretty, .$parameter) }

  table_data <- results %>%
    filter(parameter %in% params, statistic %in% stats) %>%
    mutate(value = estimate, se = sd) %>%
    select(parameter, statistic, value, se)

  # Formatter
  pick_formatter <- function(param, stat) {
    if (!is.null(custom_rounding) && stat %in% names(custom_rounding)) {
      ri <- custom_rounding[[stat]]
    } else if (!is.null(custom_rounding) && param %in% names(custom_rounding)) {
      ri <- custom_rounding[[param]]
    } else {
      ri <- list(digits = 2, scientific = FALSE)
    }
    function(x) {
      if (is.na(x)) return("--")
      if (isTRUE(ri$scientific)) sprintf(paste0("%.", ri$digits, "e"), x)
      else sprintf(paste0("%.", ri$digits, "f"), x)
    }
  }

  # Build rows
  latex_rows <- vapply(params, function(param) {
    param_data <- filter(table_data, parameter == param)

    stat_cells <- vapply(stats, function(stat) {
      one <- param_data[param_data$statistic == stat, , drop = FALSE]
      if (nrow(one) == 0) return(c("--", "(--)"))
      fmt <- pick_formatter(param, stat)
      c(fmt(one$value), paste0("(", fmt(one$se), ")"))
    }, character(2))

    lbl <- if (use_pretty_param_names && !is.null(pretty_map[[param]]))
      pretty_map[[param]] else param

    paste0(
      sprintf("%s & %s \\\\", lbl, paste(stat_cells[1, ], collapse = " & ")),
      "\n",
      sprintf(" & %s \\\\", paste(stat_cells[2, ], collapse = " & ")),
      "\n\\\\[-1ex]"
    )
  }, character(1))

  body_rows <- paste(latex_rows, collapse = "\n")

  # Two-line column header
  raw_headers <- c(first_col_header, gsub("_", " ", stats))
  split_header <- function(header) {
    if (grepl("\n", header, fixed = TRUE)) {
      parts <- strsplit(header, "\n", fixed = TRUE)[[1]]
      if (length(parts) == 1) parts <- c(parts, "")
      return(parts[1:2])
    } else {
      words <- strsplit(header, " +")[[1]]
      if (length(words) <= 1) return(c(header, ""))
      mid <- ceiling(length(words) / 2)
      c(paste(words[1:mid], collapse = " "),
        paste(words[(mid + 1):length(words)], collapse = " "))
    }
  }
  header_parts <- lapply(raw_headers, split_header)
  header_row1 <- paste(vapply(header_parts, `[`, character(1), 1), collapse = " & ")
  header_row2 <- paste(vapply(header_parts, `[`, character(1), 2), collapse = " & ")

  if (is.null(notes)) {
    notes <- "This table reports the distributions of key parameters from our model. Columns are moments/correlations across individuals. Risk and choice frictions (default plan FE, switching costs, and monitoring disutility) are reported on a per period basis. Parentheses show bootstrap standard errors."
  }

  sprintf(
    "\\begin{table}[%s]
\\begin{centering}
\\caption{%s}
\\label{%s}
\\resizebox{\\columnwidth}{!}{
\\begin{tabular}{l%s}
\\\\[-1.8ex]\\hline
\\hline \\\\[-1.8ex]
%s \\\\
%s \\\\
\\hline \\\\[-1.8ex]
%s
\\\\[-1.8ex]\\hline
\\hline \\\\[-1.8ex]
\\end{tabular}
}
\\par\\end{centering}
\\vspace{0.1cm}
\\begin{singlespace}
{\\footnotesize \\emph{Notes:} %s\\par}
\\end{singlespace}
\\end{table}",
    placement, caption, label,
    paste(rep("c", length(stats)), collapse = ""),
    header_row1, header_row2,
    body_rows,
    notes
  )
}
