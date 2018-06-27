# inputs = base case

## base vaccine compliance = 0%
#vax_comp_b <- 0
bc_04 <- 0
bc_519 <- 0
bc_2064 <- 0
bc_65 <- 0


## total = 1248308.16 (36.64%)
base_04 <- 74205.56 * 0.67 #33.19%
base_519 <- 419788.08 * 0.67 #65.63%
base_2064 <- 685774.72 * 0.67 #30.68%
base_65 <- 68539.8 * 0.67 #22.21%


# base case DALYs (high, low, all)
daly_b_04 <- c(2396.2599, 797.6505, 3193.9104)
daly_b_519 <- c(11847.968 , 3987.216, 15835.183)
daly_b_2064 <- c(49711.938, 7485.782, 57197.719)
daly_b_65 <- c(13243.099, 1686.065, 14929.164)
total_dalys_base <- daly_b_04 + daly_b_519 + daly_b_2064 + daly_b_65