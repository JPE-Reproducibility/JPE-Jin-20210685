# Cycle detection in best-response dynamics for two-firm pricing games.
#
# When no pure strategy Nash equilibrium (PSNE) exists, firms' best responses
# can cycle. This module detects such cycles by simulating best-response
# dynamics from every strategy pair and tracking visited states.
#
# The main entry point is get_equi_cycle(), which:
#   1. Converts the payoff grid into strategy-ID pairs
#   2. Computes best-response maps for each firm
#   3. Simulates dynamics from all starting states
#   4. Returns any PSNE found and the cycle states (averaged as approximate equilibrium)

library(data.table)

find_all_cycles_simulation <- function(payoff_data,
                                       start_states_to_check = unique(payoff_data[, .(f1_strat_id, f2_strat_id)]),
                                       max_path_len = 100,
                                       tie_breaking = "first"
) {

  setDT(payoff_data)
  payoff_data[, payoff1 := as.numeric(payoff1)]
  payoff_data[, payoff2 := as.numeric(payoff2)]

  message("Calculating best responses...")
  # For each firm, find the best response to every opponent strategy
  br1_dt <- payoff_data[payoff_data[, .I[payoff1 == max(payoff1)], by = f2_strat_id]$V1]
  br1_dt <- br1_dt[, .SD[1], by = f2_strat_id]
  br1_map <- new.env(hash = TRUE); for (i in 1:nrow(br1_dt)) br1_map[[br1_dt$f2_strat_id[i]]] <- br1_dt$f1_strat_id[i]

  br2_dt <- payoff_data[payoff_data[, .I[payoff2 == max(payoff2)], by = f1_strat_id]$V1]
  br2_dt <- br2_dt[, .SD[1], by = f1_strat_id]
  br2_map <- new.env(hash = TRUE); for (i in 1:nrow(br2_dt)) br2_map[[br2_dt$f1_strat_id[i]]] <- br2_dt$f2_strat_id[i]

  message("Finding PSNE...")
  psne_map <- new.env(hash = TRUE)
  all_f1_strats <- unique(payoff_data$f1_strat_id)
  for (s1 in all_f1_strats) {
    s2_br = br2_map[[s1]]
    if (!is.null(s2_br)) {
      s1_br = br1_map[[s2_br]]
      if (!is.null(s1_br) && s1_br == s1) {
        state_key <- paste0("(", s1, ", ", s2_br, ")")
        psne_map[[state_key]] <- TRUE
      }
    }
  }
  psne_keys <- ls(psne_map)

  message("Detecting cycles via simulation...")
  found_cycles_global <- new.env(hash = TRUE)
  visited_states_overall <- new.env(hash = TRUE)

  num_start_states <- nrow(start_states_to_check)
  message(paste("Checking paths starting from", num_start_states, "unique states."))

  for (i in 1:num_start_states) {

    if (i %% 500 == 0) {
      message(paste("Processing starting state", i, "of", num_start_states))
    }

    start_s1 <- start_states_to_check$f1_strat_id[i]
    start_s2 <- start_states_to_check$f2_strat_id[i]
    start_state_key <- paste0("(", start_s1, ", ", start_s2, ")")

    if (!is.null(visited_states_overall[[start_state_key]])) {
      next
    }

    path <- list()
    visited_in_path <- new.env(hash = TRUE)
    current_s1 <- start_s1
    current_s2 <- start_s2
    state_key <- start_state_key

    for (step in 1:max_path_len) {
      if (!is.null(visited_states_overall[[state_key]])) {
        for(visited_state in path) {
          visited_states_overall[[paste0("(", visited_state$s1, ", ", visited_state$s2, ")")]] <- TRUE
        }
        break
      }

      path[[length(path) + 1]] <- list(s1 = current_s1, s2 = current_s2)
      visited_in_path[[state_key]] <- step

      next_s1 <- br1_map[[current_s2]]
      if (is.null(next_s1)) {warning(paste("Missing F1 BR for F2 state:", current_s2)); break}

      next_s2 <- br2_map[[next_s1]]
      if (is.null(next_s2)) {warning(paste("Missing F2 BR for F1 state:", next_s1)); break}

      next_state_key <- paste0("(", next_s1, ", ", next_s2, ")")

      if (!is.null(psne_map[[next_state_key]])) {
        for(visited_state in path) visited_states_overall[[paste0("(", visited_state$s1, ", ", visited_state$s2, ")")]] <- TRUE
        visited_states_overall[[next_state_key]] <- TRUE
        break
      }

      if (!is.null(visited_in_path[[next_state_key]])) {
        cycle_start_step <- visited_in_path[[next_state_key]]
        cycle <- path[cycle_start_step:length(path)]

        cycle_str_elements <- sapply(cycle, function(p) paste0("(", p$s1, ", ", p$s2, ")"))
        cycle_key <- digest::digest(sort(cycle_str_elements), algo="md5")

        if (is.null(found_cycles_global[[cycle_key]])) {
          found_cycles_global[[cycle_key]] <- cycle
        }
        for(visited_state in path) visited_states_overall[[paste0("(", visited_state$s1, ", ", visited_state$s2, ")")]] <- TRUE
        break
      }

      current_s1 <- next_s1
      current_s2 <- next_s2
      state_key <- next_state_key

      if(step == max_path_len){
        warning(paste("Path from", start_state_key, "exceeded max_path_len"))
        for(visited_state in path) visited_states_overall[[paste0("(", visited_state$s1, ", ", visited_state$s2, ")")]] <- TRUE
      }

    }
  }

  message("Analysis complete.")
  final_cycles_list <- as.list(found_cycles_global)
  final_cycles_list <- final_cycles_list[!sapply(final_cycles_list, is.null)]

  return(list(
    psne = psne_keys,
    cycles = final_cycles_list,
    num_states_explored = length(ls(visited_states_overall)),
    num_unique_cycles_found = length(final_cycles_list)
  ))
}

format_cycle_results <- function(results, original_data) {

  if (!is.data.table(original_data)) {
    setDT(original_data)
  }

  cycles_list <- results$cycles
  organized_cycles_list <- list()

  if (length(cycles_list) == 0) {
    message("No cycles found in the results.")
    expected_cols <- c("cycle_id", "sequence_in_cycle", names(original_data))
    return(data.table(matrix(ncol = length(expected_cols), nrow = 0,
                             dimnames = list(NULL, expected_cols))))
  }

  message(paste("Processing", length(cycles_list), "cycle(s)..."))

  cycle_ids <- 1:length(cycles_list)

  for (i in seq_along(cycles_list)) {
    cycle_id <- cycle_ids[i]
    current_cycle_states <- cycles_list[[i]]

    for (j in seq_along(current_cycle_states)) {
      sequence <- j
      state <- current_cycle_states[[j]]
      f1_id <- state$s1
      f2_id <- state$s2

      organized_cycles_list[[length(organized_cycles_list) + 1]] <- data.table(
        cycle_id = cycle_id,
        sequence_in_cycle = sequence,
        f1_strat_id = f1_id,
        f2_strat_id = f2_id
      )
    }
  }

  if (length(organized_cycles_list) > 0) {
    cycles_df <- rbindlist(organized_cycles_list)

    message("Merging cycle information with original data...")

    setkey(original_data, f1_strat_id, f2_strat_id)
    setkey(cycles_df, f1_strat_id, f2_strat_id)

    final_details_df <- original_data[cycles_df, nomatch = 0]

    id_cols <- c("cycle_id", "sequence_in_cycle", "f1_strat_id", "f2_strat_id")
    orig_data_cols <- setdiff(names(final_details_df), id_cols)
    setcolorder(final_details_df, c(id_cols, orig_data_cols))

    message("Merge complete.")
    return(final_details_df)

  } else {
    message("No states extracted from cycles.")
    expected_cols <- c("cycle_id", "sequence_in_cycle", names(original_data))
    return(data.table(matrix(ncol = length(expected_cols), nrow = 0,
                             dimnames = list(NULL, expected_cols))))
  }
}

# Detect cycles and PSNE in a payoff grid.
# Returns cycle states (as k-parameter rows) that can be averaged as an approximate equilibrium
# when no PSNE exists.
get_equi_cycle <- function(large_payoff_data, firm1_params, firm2_params) {
  params_all = c(firm1_params, firm2_params)

  if (!is.data.table(large_payoff_data)) {
    setDT(large_payoff_data)
  }

  large_payoff_data[, f1_strat_id := do.call(paste, c(.SD, sep = "_")), .SDcols = firm1_params]
  large_payoff_data[, f2_strat_id := do.call(paste, c(.SD, sep = "_")), .SDcols = firm2_params]
  large_payoff_data$payoff1 <- as.numeric(large_payoff_data$pi)
  large_payoff_data$payoff2 <- as.numeric(large_payoff_data$pi_oo)

  analysis_data <- large_payoff_data[, .(f1_strat_id, f2_strat_id, payoff1, payoff2)]
  analysis_data <- unique(analysis_data)

  all_unique_states <- unique(analysis_data[, .(f1_strat_id, f2_strat_id)])
  results_all <- find_all_cycles_simulation(analysis_data, start_states_to_check = all_unique_states)

  cycle_details_df_all <- format_cycle_results(results_all, large_payoff_data)
  equi_cycle <- cycle_details_df_all[, c(params_all, "pi", "pi_oo"), with = FALSE]

  print("PSNE Found:")
  print(results_all$psne)
  print("Cycles Found:")
  print(cycle_details_df_all)

  seq_of_play <- large_payoff_data[large_payoff_data$f1_strat_id %in% c(cycle_details_df_all$f1_strat_id) &
                                     large_payoff_data$f2_strat_id %in% c(cycle_details_df_all$f2_strat_id),] %>% arrange(pi + pi_oo)

  return(list(
    psne = results_all$psne,
    cycles_all = cycle_details_df_all,
    cycles = equi_cycle,
    seq_of_play = seq_of_play
  ))
}
