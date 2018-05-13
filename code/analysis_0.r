rm(list = ls(all.names = TRUE))

library(knitr)
source("code/functions.R")
source("input/var.R")
source("input/input_base.R")

##### load input file #####
source("input/input_vaxbase_10.R")

##### save output RData #####
#save.image(file = "output/vaxbase_10.RData")




###### 0-4 years ######
pop <- pop_04
num_cases <- base_04
cases_after_vax <- intv_04
vax_comp_i <- vc_04


## 0-4 years, HIGH
risk_group <- high_019
prob <- c(p_04_high_death, p_04_high_hosp, p_04_high_out, p_04_high_rest)
r_cost <- rc_04_high

# total vaccination cost for age/risk
total_cost_vax_b <- calc_vaccination_cost(vax_comp_b)  # base
total_cost_vax_i <- calc_vaccination_cost(vax_comp_i)  # intv

# cost per health outcome
costs <- cost_cpi_outcome(r_cost[1], r_cost[2], r_cost[3], r_cost[4])

# base summary
b_cases <- calc_subpop_cases_base(num_cases, risk_group)  # base
num_outcomes_base <- calc_num_outcomes_base(prob[1], prob[2], prob[3], prob[4])  # base
cc_base <- calc_total_cost_base(r_cost[1],r_cost[2],r_cost[3],r_cost[4])  # base

# intv summary
i_cases <- calc_subpop_cases_intv(cases_after_vax, risk_group)  # intv
num_outcomes_intv <- calc_num_outcomes_intv(prob[1], prob[2], prob[3], prob[4])  # intv
cc_intv <- calc_total_cost_intv(r_cost[1],r_cost[2],r_cost[3],r_cost[4])  # intv

print(sprintf("total vax costs = %f", total_cost_vax_i))
print(sprintf("base cases = %f", b_cases))
print(sprintf("intv cases = %f", i_cases))

costs_04_high <- cc_intv[5]
cases_04_high <- i_cases
outcomes_04_high <- num_outcomes_intv[2:4]
icer_04_high <- calc_icer()
deaths_04_high <- calc_deaths_b()


## 0-4 years, low risk
risk_group <- 1-high_019
prob <- c(p_04_low_death, p_04_low_hosp, p_04_low_out, p_04_low_rest)
r_cost <- rc_04_low

# total vaccination cost for age/risk
total_cost_vax_b <- calc_vaccination_cost(vax_comp_b)  # base
total_cost_vax_i <- calc_vaccination_cost(vax_comp_i)  # intv

# cost per health outcome
costs <- cost_cpi_outcome(r_cost[1], r_cost[2], r_cost[3], r_cost[4])

# base summary
b_cases <- calc_subpop_cases_base(num_cases, risk_group)  # base
num_outcomes_base <- calc_num_outcomes_base(prob[1], prob[2], prob[3], prob[4])  # base
cc_base <- calc_total_cost_base(r_cost[1],r_cost[2],r_cost[3],r_cost[4])  # base

# intv summary
i_cases <- calc_subpop_cases_intv(cases_after_vax, risk_group)  # intv
num_outcomes_intv <- calc_num_outcomes_intv(prob[1], prob[2], prob[3], prob[4])  # intv
cc_intv <- calc_total_cost_intv(r_cost[1],r_cost[2],r_cost[3],r_cost[4])  # intv

print(sprintf("total vax costs = %f", total_cost_vax_i))
print(sprintf("base cases = %f", b_cases))
print(sprintf("intv cases = %f", i_cases))

costs_04_low <- cc_intv[5]
cases_04_low <- i_cases
outcomes_04_low <- num_outcomes_intv[2:4]
icer_04_low <- calc_icer()
deaths_04_low <- calc_deaths_b()



###### 5-19 years ######
pop <- pop_519
num_cases <- base_519
cases_after_vax <- intv_519
vax_comp_i <- vc_519


## 5-19 years, HIGH
risk_group <- high_019
prob <- c(p_519_high_death, p_519_high_hosp, p_519_high_out, p_519_high_rest)
r_cost <- rc_519_high

total_cost_vax_i <- calc_vaccination_cost(vax_comp_i)  # intv
costs <- cost_cpi_outcome(r_cost[1], r_cost[2], r_cost[3], r_cost[4])

b_cases <- calc_subpop_cases_base(num_cases, risk_group)  # base
num_outcomes_base <- calc_num_outcomes_base(prob[1], prob[2], prob[3], prob[4])  # base
cc_base <- calc_total_cost_base(r_cost[1],r_cost[2],r_cost[3],r_cost[4])  # base

i_cases <- calc_subpop_cases_intv(cases_after_vax, risk_group)  # intv
num_outcomes_intv <- calc_num_outcomes_intv(prob[1], prob[2], prob[3], prob[4])  # intv
cc_intv <- calc_total_cost_intv(r_cost[1],r_cost[2],r_cost[3],r_cost[4])  # intv

print(sprintf("total vax costs = %f", total_cost_vax_i))
print(sprintf("base cases = %f", b_cases))
print(sprintf("intv cases = %f", i_cases))

costs_519_high <- cc_intv[5]
cases_519_high <- i_cases
outcomes_519_high <- num_outcomes_intv[2:4]
icer_519_high <- calc_icer()
deaths_519_high <- calc_deaths_b()


## 5-19 years, low risk
risk_group <- 1-high_019
prob <- c(p_519_low_death, p_519_low_hosp, p_519_low_out, p_519_low_rest)
r_cost <- rc_519_low

total_cost_vax_i <- calc_vaccination_cost(vax_comp_i)  # intv
costs <- cost_cpi_outcome(r_cost[1], r_cost[2], r_cost[3], r_cost[4])

b_cases <- calc_subpop_cases_base(num_cases, risk_group)  # base
num_outcomes_base <- calc_num_outcomes_base(prob[1], prob[2], prob[3], prob[4])  # base
cc_base <- calc_total_cost_base(r_cost[1],r_cost[2],r_cost[3],r_cost[4])  # base

i_cases <- calc_subpop_cases_intv(cases_after_vax, risk_group)  # intv
num_outcomes_intv <- calc_num_outcomes_intv(prob[1], prob[2], prob[3], prob[4])  # intv
cc_intv <- calc_total_cost_intv(r_cost[1],r_cost[2],r_cost[3],r_cost[4])  # intv

print(sprintf("total vax costs = %f", total_cost_vax_i))
print(sprintf("base cases = %f", b_cases))
print(sprintf("intv cases = %f", i_cases))

costs_519_low <- cc_intv[5]
cases_519_low <- i_cases
outcomes_519_low <- num_outcomes_intv[2:4]
icer_519_low <- calc_icer()
deaths_519_low <- calc_deaths_b()



###### 20-64 years ######
pop <- pop_2064
num_cases <- base_2064
cases_after_vax <- intv_2064
vax_comp_i <- vc_2064


## 20-64 years, HIGH
risk_group <- high_2064
prob <- c(p_2064_high_death, p_2064_high_hosp, p_2064_high_out, p_2064_high_rest)
r_cost <- rc_2064_high

total_cost_vax_i <- calc_vaccination_cost(vax_comp_i)  # intv
costs <- cost_cpi_outcome(r_cost[1], r_cost[2], r_cost[3], r_cost[4])

b_cases <- calc_subpop_cases_base(num_cases, risk_group)  # base
num_outcomes_base <- calc_num_outcomes_base(prob[1], prob[2], prob[3], prob[4])  # base
cc_base <- calc_total_cost_base(r_cost[1],r_cost[2],r_cost[3],r_cost[4])  # base

i_cases <- calc_subpop_cases_intv(cases_after_vax, risk_group)  # intv
num_outcomes_intv <- calc_num_outcomes_intv(prob[1], prob[2], prob[3], prob[4])  # intv
cc_intv <- calc_total_cost_intv(r_cost[1],r_cost[2],r_cost[3],r_cost[4])  # intv

print(sprintf("total vax costs = %f", total_cost_vax_i))
print(sprintf("base cases = %f", b_cases))
print(sprintf("intv cases = %f", i_cases))

costs_2064_high <- cc_intv[5]
cases_2064_high <- i_cases
outcomes_2064_high <- num_outcomes_intv[2:4]
icer_2064_high <- calc_icer()
deaths_2064_high <- calc_deaths_b()


## 20-64 years, low risk
risk_group <- 1-high_2064
prob <- c(p_2064_low_death, p_2064_low_hosp, p_2064_low_out, p_2064_low_rest)
r_cost <- rc_2064_low

total_cost_vax_i <- calc_vaccination_cost(vax_comp_i)  # intv
costs <- cost_cpi_outcome(r_cost[1], r_cost[2], r_cost[3], r_cost[4])

b_cases <- calc_subpop_cases_base(num_cases, risk_group)  # base
num_outcomes_base <- calc_num_outcomes_base(prob[1], prob[2], prob[3], prob[4])  # base
cc_base <- calc_total_cost_base(r_cost[1],r_cost[2],r_cost[3],r_cost[4])  # base

i_cases <- calc_subpop_cases_intv(cases_after_vax, risk_group)  # intv
num_outcomes_intv <- calc_num_outcomes_intv(prob[1], prob[2], prob[3], prob[4])  # intv
cc_intv <- calc_total_cost_intv(r_cost[1],r_cost[2],r_cost[3],r_cost[4])  # intv

print(sprintf("total vax costs = %f", total_cost_vax_i))
print(sprintf("base cases = %f", b_cases))
print(sprintf("intv cases = %f", i_cases))

costs_2064_low <- cc_intv[5]
cases_2064_low <- i_cases
outcomes_2064_low <- num_outcomes_intv[2:4]
icer_2064_low <- calc_icer()
deaths_2064_low <- calc_deaths_b()



###### 65+ years ######
pop <- pop_65
num_cases <- base_65
cases_after_vax <- intv_65
vax_comp_i <- vc_65


## 65+ years, HIGH
risk_group <- high_65
prob <- c(p_65_high_death, p_65_high_hosp, p_65_high_out, p_65_high_rest)
r_cost <- rc_65_high

total_cost_vax_i <- calc_vaccination_cost(vax_comp_i)  # intv
costs <- cost_cpi_outcome(r_cost[1], r_cost[2], r_cost[3], r_cost[4])

b_cases <- calc_subpop_cases_base(num_cases, risk_group)  # base
num_outcomes_base <- calc_num_outcomes_base(prob[1], prob[2], prob[3], prob[4])  # base
cc_base <- calc_total_cost_base(r_cost[1],r_cost[2],r_cost[3],r_cost[4])  # base

i_cases <- calc_subpop_cases_intv(cases_after_vax, risk_group)  # intv
num_outcomes_intv <- calc_num_outcomes_intv(prob[1], prob[2], prob[3], prob[4])  # intv
cc_intv <- calc_total_cost_intv(r_cost[1],r_cost[2],r_cost[3],r_cost[4])  # intv

print(sprintf("total vax costs = %f", total_cost_vax_i))
print(sprintf("base cases = %f", b_cases))
print(sprintf("intv cases = %f", i_cases))

costs_65_high <- cc_intv[5]
cases_65_high <- i_cases
outcomes_65_high <- num_outcomes_intv[2:4]
icer_65_high <- calc_icer()
deaths_65_high <- calc_deaths_b()


## 65+ years, low risk
risk_group <- 1-high_65
prob <- c(p_65_low_death, p_65_low_hosp, p_65_low_out, p_65_low_rest)
r_cost <- rc_65_low

total_cost_vax_i <- calc_vaccination_cost(vax_comp_i)  # intv
costs <- cost_cpi_outcome(r_cost[1], r_cost[2], r_cost[3], r_cost[4])

b_cases <- calc_subpop_cases_base(num_cases, risk_group)  # base
num_outcomes_base <- calc_num_outcomes_base(prob[1], prob[2], prob[3], prob[4])  # base
cc_base <- calc_total_cost_base(r_cost[1],r_cost[2],r_cost[3],r_cost[4])  # base

i_cases <- calc_subpop_cases_intv(cases_after_vax, risk_group)  # intv
num_outcomes_intv <- calc_num_outcomes_intv(prob[1], prob[2], prob[3], prob[4])  # intv
cc_intv <- calc_total_cost_intv(r_cost[1],r_cost[2],r_cost[3],r_cost[4])  # intv

print(sprintf("total vax costs = %f", total_cost_vax_i))
print(sprintf("base cases = %f", b_cases))
print(sprintf("intv cases = %f", i_cases))

costs_65_low <- cc_intv[5]
cases_65_low <- i_cases
outcomes_65_low <- num_outcomes_intv[2:4]
icer_65_low <- calc_icer()
deaths_65_low <- calc_deaths_b()

