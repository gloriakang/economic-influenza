# set-up

library(mc2d)

# population sizes
total_pop <- 3406876
pop_04 <- 223608  #(0.0656)
pop_519 <- 639661  #(0.187)
pop_2064 <- 2235049  #(0.656)
pop_65 <- 308558  #(0.0905)

# high risk group proportion
high_019 <- 0.064  # meltzer 1999
#high_04 <- 0.052  # molinari 2007
#high_517 <- 0.106  # molinari 2007
high_2064 <- 0.144  # meltzer 1999
high_65 <- 0.512  # molinari 2007


# for costs
cpi <- 513.135/379.516
vax_cost <- 28.62

# unadjusted health outcome costs (Carias et al. 2015)
rc_04_high <- c(46017, 20928, 603, 4)
rc_04_low <- c(49339, 9390, 339, 4)
rc_519_high <- c(188071, 40310, 843, 4)
rc_519_low <- c(167086, 19258, 277, 4)
rc_2064_high <- c(71826, 29528, 665, 4)
rc_2064_low <- c(66174, 22556, 422, 4)
rc_65_high <- c(44806, 19099, 2592, 4)
rc_65_low <- c(33011, 12689, 922, 4)

# health outcome probabilities
set.seed(1000)

# 0-4 high
p_04_high_death <- mean(rtriang(10000, min = 0.00036, mode = 0.001, max = 0.01821))
p_04_high_hosp <- mean(runif(10000, min = 0.006, max = 0.02143))
p_04_high_out <- mean(runif(10000, min = 0.82571, max = 0.95952))
p_04_high_rest <- 1-(p_04_high_death + p_04_high_hosp + p_04_high_out)
#(p_04_high_death, p_04_high_hosp, p_04_high_out, p_04_high_rest)

# 0-4 low
p_04_low_death <- mean(rtriang(10000, min = 0.00004, mode = 0.07/1000, max = 0.0003))
p_04_low_hosp <- mean(runif(10000, min = 0.00057, max = 0.0069))
p_04_low_out <- mean(runif(10000, min = 0.47143, max = 0.54762))
p_04_low_rest <- 1-(p_04_low_death + p_04_low_hosp + p_04_low_out)
#(p_04_low_death, p_04_low_hosp, p_04_low_out, p_04_low_rest)

# 5-19 high
p_519_high_death <- mean(rtriang(10000, min = 0.00036, mode = 0.001, max = 0.01821))
p_519_high_hosp <- mean(runif(10000, min = 0.006, max = 0.02143))
p_519_high_out <- mean(runif(10000, min = 0.082571, max = 0.95952))
p_519_high_rest <- 1-(p_519_high_death + p_519_high_hosp + p_519_high_out)
#(p_519_high_death, p_519_high_hosp, p_519_high_out, p_519_high_rest)

# 5-19 low
p_519_low_death <- mean(rtriang(10000, min = 0.00004, mode = 0.07/1000, max = 0.0003))
p_519_low_hosp <- mean(runif(10000, min = 0.00057, max = 0.0069))
p_519_low_out <- mean(runif(10000, min = 0.47143, max = 0.54762))
p_519_low_rest <- 1-(p_519_low_death + p_519_low_hosp + p_519_low_out)
#(p_519_low_death, p_519_low_hosp, p_519_low_out, p_519_low_rest)

# 20-64 high
p_2064_high_death <- mean(runif(10000, min = 0.00083, max = 0.02487))
p_2064_high_hosp <- mean(runif(10000, min = 0.00692, max = 0.02235))
p_2064_high_out <- mean(runif(10000, min = 0.58333, max = 0.64786))
p_2064_high_rest <- 1-(p_2064_high_death + p_2064_high_hosp + p_2064_high_out)
#(p_2064_high_death, p_2064_high_hosp, p_2064_high_out, p_2064_high_rest)

# 20-64 low
p_2064_low_death <- mean(rtriang(10000, min = 0.00021, mode = 0.31/1000, max = 0.00039))
p_2064_low_hosp <- mean(runif(10000, min = 0.0015, max = 0.01196))
p_2064_low_out <- mean(runif(10000, min = 0.33333, max = 0.36957))
p_2064_low_rest <- 1-(p_2064_low_death + p_2064_low_hosp + p_2064_low_out)
#(p_2064_low_death, p_2064_low_hosp, p_2064_low_out, p_2064_low_rest)

# 65+ high
p_65_high_death <- mean(runif(10000, min = 0.023, max = 0.02963))
p_65_high_hosp <- mean(runif(10000, min = 0.03333, max = 0.06842))
p_65_high_out <- mean(runif(10000, min = 0.65833, max = 0.68421))
p_65_high_rest <- 1-(p_65_high_death + p_65_high_hosp + p_65_high_out)
#(p_65_high_death, p_65_high_hosp, p_65_high_out, p_65_high_rest)

# 65+ low
p_65_low_death <- mean(rtriang(10000, min = 0.00233, mode = 3.51/1000, max = 4.52/1000))
p_65_low_hosp <- mean(runif(10000, min = 0.0125, max = 0.01579))
p_65_low_out <- mean(runif(10000, min = 0.375, max = 0.38947))
p_65_low_rest <- 1-(p_65_low_death + p_65_low_hosp + p_65_low_out)
#(p_65_low_death, p_65_low_hosp, p_65_low_out, p_65_low_rest)
