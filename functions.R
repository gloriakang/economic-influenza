
calc_subpop_cases_base <- function(num_cases, risk_group){
  subpop_cases_base <- num_cases * risk_group
  return(subpop_cases_base)
}

calc_subpop_cases_intv <- function(cases_after_vax, risk_group){
  subpop_cases_intv <- cases_after_vax * risk_group
  return(subpop_cases_intv)
}

# total cost of vaccination program
calc_vaccination_cost <- function(pop, risk_group, vax_cost, vax_comp){
  total_cost_vax = vax_cost * vax_comp * pop * risk_group
  return(total_cost_vax)
}

# adjusted cost per health outcome
cost_cpi_outcome <- function(death, hosp, out, rest){
  cost_per_death <- death * cpi
  cost_per_hosp <-  hosp * cpi
  cost_per_out <- out * cpi
  cost_per_rest <- rest * cpi
  # returns cost per health outcome case
  cost_outcome = c(cost_per_death, cost_per_hosp, cost_per_out, cost_per_rest)
  return(cost_outcome)
}

# number of cases for each health outcome (base)
calc_num_outcomes_base <- function(p_death, p_hosp, p_out, p_rest){
  subpop_cases_base = calc_subpop_cases_base(num_cases, risk_group)
  num_death = p_death * subpop_cases_base
  num_hosp = p_hosp * subpop_cases_base
  num_out = p_out * subpop_cases_base
  num_rest = p_rest * subpop_cases_base
  num_outcome_total = num_death + num_hosp + num_out + num_rest
  num_outcomes_base = c(num_death, num_hosp, num_out, num_rest)
  return(num_outcomes_base)
}

# number of cases for each health outcome (intervention)
calc_num_outcomes_intv <- function(p_death, p_hosp, p_out, p_rest){
  subpop_cases_intv = calc_subpop_cases_intv(cases_after_vax, risk_group)
  num_death = p_death * subpop_cases_intv
  num_hosp = p_hosp * subpop_cases_intv
  num_out = p_out * subpop_cases_intv
  num_rest = p_rest * subpop_cases_intv
  num_outcome_total = num_death + num_hosp + num_out + num_rest
  num_outcomes_intv = c(num_death, num_hosp, num_out, num_rest)
  return(num_outcomes_intv)
}

# total costs of each health outcome (base)
calc_total_cost_base <- function(death, hosp, out, rest){
  cost_outcome = cost_cpi_outcome(death, hosp, out, rest)
  total_cost_death <- cost_outcome[1] * num_outcomes_base[1]
  total_cost_hosp <- cost_outcome[2] * num_outcomes_base[2]
  total_cost_out <- cost_outcome[3] * num_outcomes_base[3]
  total_cost_rest <- cost_outcome[4] * num_outcomes_base[4]
  total_costs = total_cost_death + total_cost_hosp + total_cost_out + total_cost_rest
  return(c(total_cost_death, total_cost_hosp, total_cost_out, total_cost_rest,
           "total costs base", total_costs))
}

# total costs of each health outcome (intervention)
calc_total_cost_intv <- function(death, hosp, out, rest){
  cost_outcome = cost_cpi_outcome(death, hosp, out, rest)
  total_cost_death <- cost_outcome[1] * num_outcomes_intv[1]
  total_cost_hosp <- cost_outcome[2] * num_outcomes_intv[2]
  total_cost_out <- cost_outcome[3] * num_outcomes_intv[3]
  total_cost_rest <- cost_outcome[4] * num_outcomes_intv[4]
  total_costs = total_cost_death + total_cost_hosp + total_cost_out + total_cost_rest
  return(c(total_cost_death, total_cost_hosp, total_cost_out, total_cost_rest,
           "total costs intv", total_costs))
}

# ICER
calc_icer <- function(){
  total_cost_vax - 
}


