rm(list = ls(all.names = TRUE))

# load RData
load("output/vaxbase-40.RData")

#load("output/vaxbase-30.RData")
#load("output/vaxbase-50.RData")
#load("output/vaxbase-60.RData")

# total clinical costs
costs_04_high
costs_04_low
costs_519_high
costs_519_low
costs_2064_high
costs_2064_low
costs_65_high
costs_65_low


# number of cases
cases_04_high
cases_04_low
cases_519_high
cases_519_low
cases_2064_high
cases_2064_low
cases_65_high
cases_65_low


# cost difference (base - intv)
icer_04_high[1]
icer_04_low[1]
icer_519_high[1]
icer_519_low[1]
icer_2064_high[1]
icer_2064_low[1]
icer_65_high[1]
icer_65_low[1]


# cases averted (base - intv)
icer_04_high[2]
icer_04_low[2]
icer_519_high[2]
icer_519_low[2]
icer_2064_high[2]
icer_2064_low[2]
icer_65_high[2]
icer_65_low[2]


# ICER per case averted
icer_04_high[3]
icer_04_low[3]
icer_519_high[3]
icer_519_low[3]
icer_2064_high[3]
icer_2064_low[3]
icer_65_high[3]
icer_65_low[3]


# number of deaths
deaths_04_high[3]
deaths_04_low[3]
deaths_519_high[3]
deaths_519_low[3]
deaths_2064_high[3]
deaths_2064_low[3]
deaths_65_high[3]
deaths_65_low[3]


# deaths averted
deaths_04_high[1]
deaths_04_low[1]
deaths_519_high[1]
deaths_519_low[1]
deaths_2064_high[1]
deaths_2064_low[1]
deaths_65_high[1]
deaths_65_low[1]


# ICER per death averted
deaths_04_high[2]
deaths_04_low[2]
deaths_519_high[2]
deaths_519_low[2]
deaths_2064_high[2]
deaths_2064_low[2]
deaths_65_high[2]
deaths_65_low[2]


# health outcomes
outcomes_04_high
outcomes_04_low
outcomes_519_high
outcomes_519_low
outcomes_2064_high
outcomes_2064_low
outcomes_65_high
outcomes_65_low

