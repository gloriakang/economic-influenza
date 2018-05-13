# results

rm(list = ls(all.names = TRUE))
library(knitr)

## load RData
#load("output/base.RData")

load("output/vaxbase_10.RData")
#load("output/vaxbase_20.RData")
#load("output/vaxbase_30.RData")
#load("output/vaxbase_40.RData")
#load("output/vaxbase_50.RData")
#load("output/vaxbase_60.RData")

#load("output/vax70_10.RData")
#load("output/vax70_20.RData")
#load("output/vax70_30.RData")
#load("output/vax70_40.RData")
#load("output/vax70_50.RData")
#load("output/vax70_60.RData")



# Clinical costs (high, low, all)
costs_04_high; costs_04_low; costs_04_high + costs_04_low
costs_519_high; costs_519_low; costs_519_high + costs_519_low
costs_2064_high; costs_2064_low; costs_2064_high + costs_2064_low
costs_65_high; costs_65_low; costs_65_high + costs_65_low


# Number of cases (high, low, all)
cases_04 <- c(cases_04_high, cases_04_low, cases_04_high + cases_04_low)
cases_519 <- c(cases_519_high, cases_519_low, cases_519_high + cases_519_low)
cases_2064 <- c(cases_2064_high, cases_2064_low, cases_2064_high + cases_2064_low)
cases_65 <- c(cases_65_high, cases_65_low, cases_65_high + cases_65_low)

cases_04_high; cases_04_low; cases_04_high + cases_04_low
cases_519_high; cases_519_low; cases_519_high + cases_519_low
cases_2064_high; cases_2064_low; cases_2064_high + cases_2064_low
cases_65_high; cases_65_low; cases_65_high + cases_65_low


# Cost difference (high, low, all)
icer_04_high[1]; icer_04_low[1]; (cost_diff_04 <- icer_04_high[1] + icer_04_low[1])
icer_519_high[1]; icer_519_low[1]; (cost_diff_519 <- icer_519_high[1] + icer_519_low[1])
icer_2064_high[1]; icer_2064_low[1]; (cost_diff_2064 <- icer_2064_high[1] + icer_2064_low[1])
icer_65_high[1]; icer_65_low[1]; (cost_diff_65 <- icer_65_high[1] + icer_65_low[1])

## Total cost difference in scenario
(total_cost_difference <- cost_diff_04 + cost_diff_519 + cost_diff_2064 + cost_diff_65)


# Cases averted (high, low, all)
icer_04_high[2]; icer_04_low[2]; (case_avert_04 <- icer_04_high[2] + icer_04_low[2])
icer_519_high[2]; icer_519_low[2]; (case_avert_519 <- icer_519_high[2] + icer_519_low[2])
icer_2064_high[2]; icer_2064_low[2]; (case_avert_2064 <- icer_2064_high[2] + icer_2064_low[2])
icer_65_high[2]; icer_65_low[2]; (case_avert_65 <- icer_65_high[2] + icer_65_low[2])

## Total cases averted in scenario
(total_cases_averted <- case_avert_04 + case_avert_519 + case_avert_2064 + case_avert_65)


# ICER per case averted (high, low, all)
icer_04_high[3]; icer_04_low[3]; (icer_04_high[1] + icer_04_low[1]) / (icer_04_high[2] + icer_04_low[2])
icer_519_high[3]; icer_519_low[3]; (icer_519_high[1] + icer_519_low[1]) / (icer_519_high[2] + icer_519_low[2])
icer_2064_high[3]; icer_2064_low[3]; (icer_2064_high[1] + icer_2064_low[1]) / (icer_2064_high[2] + icer_2064_low[2])
icer_65_high[3]; icer_65_low[3]; (icer_65_high[1] + icer_65_low[1]) / (icer_65_high[2] + icer_65_low[2])

## Total ICER per case averted in scenario
total_cost_difference / total_cases_averted


# Deaths (high, low, all)
deaths_04 <- c(deaths_04_high[3], deaths_04_low[3], deaths_04_high[3] + deaths_04_low[3])
deaths_519 <- c(deaths_519_high[3], deaths_519_low[3], deaths_519_high[3] + deaths_519_low[3])
deaths_2064 <- c(deaths_2064_high[3], deaths_2064_low[3], deaths_2064_high[3] + deaths_2064_low[3])
deaths_65 <- c(deaths_65_high[3], deaths_65_low[3], deaths_65_high[3] + deaths_65_low[3])

deaths_04_high[3]; deaths_04_low[3]; deaths_04_high[3] + deaths_04_low[3]
deaths_519_high[3]; deaths_519_low[3]; deaths_519_high[3] + deaths_519_low[3]
deaths_2064_high[3]; deaths_2064_low[3]; deaths_2064_high[3] + deaths_2064_low[3]
deaths_65_high[3]; deaths_65_low[3]; deaths_65_high[3] + deaths_65_low[3]


# Deaths averted (high, low, all)
deaths_04_high[1]; deaths_04_low[1]; (death_avert_04 <- deaths_04_high[1] + deaths_04_low[1])
deaths_519_high[1]; deaths_519_low[1]; (death_avert_519 <- deaths_519_high[1] + deaths_519_low[1])
deaths_2064_high[1]; deaths_2064_low[1]; (death_avert_2064 <- deaths_2064_high[1] + deaths_2064_low[1])
deaths_65_high[1]; deaths_65_low[1]; (death_avert_65 <- deaths_65_high[1] + deaths_65_low[1])

## Total deaths averted in scenario
(total_deaths_averted <- death_avert_04 + death_avert_519 + death_avert_2064 + death_avert_65)


# ICER per death averted (high, low, all)
deaths_04_high[2]; deaths_04_low[2]; (icer_04_high[1] + icer_04_low[1]) / (deaths_04_high[1] + deaths_04_low[1])
deaths_519_high[2]; deaths_519_low[2]; (icer_519_high[1] + icer_519_low[1]) / (deaths_519_high[1] + deaths_519_low[1])
deaths_2064_high[2]; deaths_2064_low[2]; (icer_2064_high[1] + icer_2064_low[1]) / (deaths_2064_high[1] + deaths_2064_low[1])
deaths_65_high[2]; deaths_65_low[2]; (icer_65_high[1] + icer_65_low[1]) / (deaths_65_high[1] + deaths_65_low[1])

## Total ICER per death averted in scenario
total_cost_difference / total_deaths_averted


# Other health outcomes
outcomes_04_high; outcomes_04_low
outcomes_519_high; outcomes_519_low
outcomes_2064_high; outcomes_2064_low
outcomes_65_high; outcomes_65_low


# calculating DALYs
yll_04 <- 79.4-2
yll_519 <- 79.5-12
yll_2064 <- 80.9-42
yll_65 <- 86.3-72

calc_daly <- function(cases, deaths, yll){
  daly_high <- (cases[1] * 0.051 * (7/365)) + (yll * deaths[1])
  daly_low <- (cases[2] * 0.051 * (7/365)) + (yll * deaths[2])
  daly_all <- (cases[3] * 0.051 * (7/365)) + (yll * deaths[3])
  return(c(daly_high, daly_low, daly_all))
}
# (2463.194 * 0.051 * (7/365)) + (77.4 * 16.0260)

# DALYs (high, low, all)
(daly_04 <- calc_daly(cases_04, deaths_04, yll_04))
(daly_519 <- calc_daly(cases_519, deaths_519, yll_519))
(daly_2064 <- calc_daly(cases_2064, deaths_2064, yll_2064))
(daly_65 <- calc_daly(cases_65, deaths_65, yll_65))


# DALYs averted (high, low, all)
source("input/input_base.R")
(daly_avert_04 <- (daly_b_04 - daly_04))
(daly_avert_519 <- (daly_b_519 - daly_519))
(daly_avert_2064 <- (daly_b_2064 - daly_2064))
(daly_avert_65 <- (daly_b_65 - daly_65))

## Total DALYs averted in scenario
(total_dalys_averted <- daly_avert_04[3] + daly_avert_519[3] + daly_avert_2064[3] + daly_avert_65[3])


# ICER per DALY averted (high, low, all)
icer_04_high[1] / daly_avert_04[1]; icer_04_low[1] / daly_avert_04[2]; cost_diff_04 / daly_avert_04[3]
icer_519_high[1] / daly_avert_519[1]; icer_519_low[1] / daly_avert_519[2]; cost_diff_519 / daly_avert_519[3]
icer_2064_high[1] / daly_avert_2064[1]; icer_2064_low[1] / daly_avert_2064[2]; cost_diff_2064 / daly_avert_2064[3]
icer_65_high[1] / daly_avert_65[1]; icer_65_low[1] / daly_avert_65[2]; cost_diff_65 / daly_avert_65[3]

# ICER per DALY averted in scenario
total_cost_difference / total_dalys_averted

