# functions

calc_subpop_cases_base <- function(num_cases, risk_group){
  subpop_cases_base <- num_cases * risk_group
  return(subpop_cases_base)
}

calc_subpop_cases_intv <- function(cases_after_vax, risk_group){
  subpop_cases_intv <- cases_after_vax * risk_group
  return(subpop_cases_intv)
}

# total cost of vaccination program
calc_vaccination_cost <- function(vax_comp){
  total_cost_vax = vax_cost * pop * risk_group * vax_comp
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
  print(sprintf("base num: deaths = %f, hosps = %f, outps = %f, rest = %f",
                num_death, num_hosp, num_out, num_rest))
  #print(sprintf("base total outcomes = %f", num_outcome_total))
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
  print(sprintf("intv num: deaths = %f, hosps = %f, outps = %f, rest = %f",
                num_death, num_hosp, num_out, num_rest))
  #print(sprintf("intv total outcomes = %f", num_outcome_total))
  return(num_outcomes_intv)
}

# total costs of each health outcome (base)
calc_total_cost_base <- function(death, hosp, out, rest){
  cost_outcome = cost_cpi_outcome(death, hosp, out, rest)
  total_cost_death = cost_outcome[1] * num_outcomes_base[1]
  total_cost_hosp = cost_outcome[2] * num_outcomes_base[2]
  total_cost_out = cost_outcome[3] * num_outcomes_base[3]
  total_cost_rest = cost_outcome[4] * num_outcomes_base[4]
  total_costs = total_cost_death + total_cost_hosp + total_cost_out + total_cost_rest
  print(sprintf("base cc: deaths = %f, hosps = %f, outps = %f, rest = %f",
                total_cost_death, total_cost_hosp, total_cost_out, total_cost_rest))
  print(sprintf("base total clin costs = %f", total_costs))
  return(c(total_cost_death, total_cost_hosp, total_cost_out, total_cost_rest, total_costs))
}

# total costs of each health outcome (intervention)
calc_total_cost_intv <- function(death, hosp, out, rest){
  cost_outcome = cost_cpi_outcome(death, hosp, out, rest)
  total_cost_death = cost_outcome[1] * num_outcomes_intv[1]
  total_cost_hosp = cost_outcome[2] * num_outcomes_intv[2]
  total_cost_out = cost_outcome[3] * num_outcomes_intv[3]
  total_cost_rest = cost_outcome[4] * num_outcomes_intv[4]
  total_costs = total_cost_death + total_cost_hosp + total_cost_out + total_cost_rest
  print(sprintf("intv cc: deaths = %f, hosps = %f, outps = %f, rest = %f",
                total_cost_death, total_cost_hosp, total_cost_out, total_cost_rest))
  print(sprintf("intv total clin costs = %f", total_costs))
  return(c(total_cost_death, total_cost_hosp, total_cost_out, total_cost_rest, total_costs))
}

# ICER CASES
calc_icer <- function(){
  cases_averted <- b_cases - i_cases
  net_costs_b = cc_base[5]+total_cost_vax_b
  net_costs_i = cc_intv[5]+total_cost_vax_i
  cost_diff = net_costs_b - net_costs_i
  icer = cost_diff / cases_averted
  print(sprintf("intv net costs = %f", net_costs_i))
  print(sprintf("cost difference = %f", cost_diff))
  print(sprintf("cases averted = %f", cases_averted))
  print(sprintf("icer = %s", icer))
  return(c(cost_diff, cases_averted, icer))
}

# DEATHS
calc_deaths_b <- function(){
  deaths_averted = num_outcomes_base[1]-num_outcomes_intv[1]
  # death_cost_b = cc_base[1]
  # death_cost_i = cc_intv[1]
  # cost_diff_d = death_cost_b - death_cost_i
  net_costs_b = cc_base[5]+total_cost_vax_b
  net_costs_i = cc_intv[5]+total_cost_vax_i
  cost_diff_d = net_costs_b - net_costs_i
  cost_per_death_averted = cost_diff_d / deaths_averted
  print(sprintf("deaths averted = %f", deaths_averted))
  print(sprintf("cost per death averted = %f", cost_per_death_averted))
  return(c(deaths_averted, cost_per_death_averted))
}

# DALY
# calc_daly <- function(){
#   dalys_averted = 
#   net_costs_b = cc_base[5]+total_cost_vax_b
#   net_costs_i = cc_intv[5]+total_cost_vax_i
#   cost_diff = net_costs_b - net_costs_i
#   daly = cost_diff / dalys_averted
#   return(daly)
# }









