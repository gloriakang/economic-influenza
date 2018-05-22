# inputs = reference case: vaxbase 10% VE

## base vaccine compliance = 
vax_comp_b <- c(0.51, 0.51, 0.33, 0.63)

## total = 1089539.4 (31.98%)
base_04 <- 62948.88 #28.15%
base_519 <- 368519.4 #57.61%
base_2064 <- 600589.08 #26.87%
base_65 <- 57482.04 #18.63%

# base case DALYs (high, low, all)
daly_b_04 <- c(2032.76, 676.65, 2709.41)
daly_b_519 <- c(10400.98, 3500.26, 13901.23)
daly_b_2064 <- c(43536.82, 6555.91, 50092.73)
daly_b_65 <- c(11106.54, 1414.05, 12520.59)
total_dalys_base <- daly_b_04 + daly_b_519 + daly_b_2064 + daly_b_65