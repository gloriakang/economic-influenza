# inputs = reference case: vaxbase 20% VE

## base vaccine compliance = 
#vax_comp_b <- c(0.51, 0.51, 0.33, 0.63)
bc_04 <- 0.51
bc_519 <- 0.51
bc_2064 <- 0.33
bc_65 <- 0.63

## total = 924779.36 (27.14%)
base_04 <- 51751.64 #23.14%
base_519 <- 313924.8 #49.08%
base_2064 <- 512276.44 #22.92%
base_65 <- 46826.48 #15.18%

# base case DALYs (high, low, all)
daly_b_04 <- c(1671.17, 556.29, 2227.46)
daly_b_519 <- c(8860.12, 2981.71, 11841.82)
daly_b_2064 <- c(37135.02, 5591.91, 42726.92)
daly_b_65 <- c(9047.70, 1151.92, 10199.62)
total_dalys_base <- daly_b_04 + daly_b_519 + daly_b_2064 + daly_b_65