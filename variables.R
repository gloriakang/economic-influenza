# variables

library(mc2d)

# population sizes
total_pop <- 3406876
pop_04 <- 223608  #(0.0656)
pop_519 <- 639661  #(0.187)
pop_2064 <- 2235049  #(0.656)
pop_65 <- 308558  #(0.0905)

# risk group proportion (high)
high_019 <- 0.064  # meltzer 1999
#high_04 <- 0.052  # molinari 2007
#high_517 <- 0.106  # molinari 2007
high_2064 <- 0.144  # meltzer 1999
high_65 <- 0.512  # molinari 2007

cpi <- 513.135/379.516
vax_cost <- 28.62
vax_comp <- 0.4

# health outcome probabilities
set.seed(1000)
p_04_high_death <- mean(rtriang(10000, min = 0.00036, mode = 0.001, max = 0.01821))
p_04_high_hosp <- mean(runif(10000, min = 0.006, max = 0.02143))
p_04_high_out <- mean(runif(10000, min = 0.8257, max = 0.9595))
p_04_high_rest <- 1-(p_04_high_death + p_04_high_hosp + p_04_high_out)

#p_04_low_death <- mean(rtriang(10000, min = , mode = , max =))
#p_04_low_hosp <- mean(runif(10000, min = , max = ))
#p_04_low_out <- mean(runif(10000, min = , max = ))
#p_04_low_rest <- 1-(p_04_low_death + p_04_low_hosp + p_04_low_out)

# p_519_high_death <- mean(rtriang(10000, min = , mode = , max = ))
# p_519_high_hosp <- mean(runif(10000, min = , max = ))
# p_519_high_out <- mean(runif(10000, min = , max = ))
# p_519_high_rest <- 1-(p_519_high_death + p_519_high_hosp + p_519_high_out)
# 
# p_519_low_death <- mean(rtriang(10000, min = , mode = , max =))
# p_519_low_hosp <- mean(runif(10000, min = , max =))
# p_519_low_out <- mean(runif(10000, min = , max =))
# p_519_low_rest <- 1-(p_519_low_death + p_519_low_hosp + p_519_low_out)
# 
# p_2064_high_death <- mean(rtriang(10000, min = , mode = , max =) )
# p_2064_high_hosp <- mean(runif(10000, min = , max =))
# p_2064_high_out <- mean(runif(10000, min = , max =))
# p_2064_high_rest <- 1-(p_2064_high_death + p_2064_high_hosp + p_2064_high_out)
# 
# p_2064_low_death <- mean(rtriang(10000, min = , mode = , max =))
# p_2064_low_hosp <- mean(runif(10000, min = , max =))
# p_2064_low_out <- mean(runif(10000, min = , max =))
# p_2064_low_rest <- 1-(p_2064_low_death + p_2064_low_hosp + p_2064_low_out)
# 
# p_65_high_death <- mean(rtriang(10000, min = , mode = , max =))
# p_65_high_hosp <- mean(runif(10000, min = , max =))
# p_65_high_out <- mean(runif(10000, min = , max =))
# p_65_high_rest <- 1-(p_65_high_death + p_65_high_hosp + p_65_high_out)
# 
# p_65_low_death <- mean(rtriang(10000, min = , mode = , max = ))
# p_65_low_hosp <- mean(runif(10000, min = , max = ))
# p_65_low_out <- mean(runif(10000, min = , max = ))
# p_65_low_rest <- 1-(p_65_low_death + p_65_low_hosp + p_65_low_out)


