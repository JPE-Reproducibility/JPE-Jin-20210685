source('code/functions/helper.R')
ipak("data.table")

# Find Nash equilibria from a grid of pricing parameters and profits.
# Each row is a strategy profile (k0,k1,k2,k2s for firm 1; k3,k4,k4s for firm 2)
# with corresponding profits (pi for firm 1, pi_oo for firm 2).
# A profile is an equilibrium if neither firm can unilaterally deviate to increase profit.
find_equilibria <- function(cf_output, firm1_params, firm2_params) {
  dt <- as.data.table(cf_output)

  dt[, eq_firm1 := TRUE]
  dt[, eq_firm2 := TRUE]

  # Check deviations for Firm 1
  for (param in firm1_params) {
    fixed_params <- setdiff(firm1_params, param)
    dt[, max_pi := max(pi), by = c(fixed_params, firm2_params)]
    dt[pi < max_pi, eq_firm1 := FALSE]
    dt[, max_pi := NULL]
  }

  # Check deviations for Firm 2
  if(length(firm2_params) > 0){
    for (param in firm2_params) {
      fixed_params <- setdiff(firm2_params, param)
      dt[, max_pi_oo := max(pi_oo), by = c(firm1_params, fixed_params)]
      dt[pi_oo < max_pi_oo, eq_firm2 := FALSE]
      dt[, max_pi_oo := NULL]
    }
  }

  dt[, equilibrium := eq_firm1 & eq_firm2]
  equilibria <- dt[equilibrium == TRUE]

  return(equilibria)
}


# Same as find_equilibria but with a tolerance (tol/dollar_norm) for profitable deviations.
# A profile is an equilibrium if no firm can gain more than tol/dollar_norm by deviating.
find_equilibria_tol <- function(cf_output, firm1_params, firm2_params, tol) {
  dt <- as.data.table(cf_output)

  dt[, eq_firm1 := TRUE]
  dt[, eq_firm2 := TRUE]

  for (param in firm1_params) {
    fixed_params <- setdiff(firm1_params, param)
    dt[, max_pi := max(pi), by = c(fixed_params, firm2_params)]
    dt[pi - max_pi < -tol/dollar_norm, eq_firm1 := FALSE]
    dt[, max_pi := NULL]
  }

  if(length(firm2_params) > 0){
    for (param in firm2_params) {
      fixed_params <- setdiff(firm2_params, param)
      dt[, max_pi_oo := max(pi_oo), by = c(firm1_params, fixed_params)]
      dt[pi_oo - max_pi_oo < -tol/dollar_norm, eq_firm2 := FALSE]
      dt[, max_pi_oo := NULL]
    }
  }

  dt[, equilibrium := eq_firm1 & eq_firm2]
  equilibria <- dt[equilibrium == TRUE]

  return(equilibria)
}
