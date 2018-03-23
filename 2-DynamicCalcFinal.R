# Calculations

rm(list = ls(all.names = TRUE))
library(mc2d)

########## Define variables ##########
# risk.group
highrisk0_19 <- 0.064  # meltzer 1999
highrisk0_4 <- 0.052  # molinari 2007
highrisk5_17 <- 0.106  # molinari 2007
highrisk20_64 <- 0.144  # meltzer 1999
highrisk65 <- 0.512  # molinari 2007

# consumer price adjustment index
cpi <- 513.135/379.516

# vaccine cost
vax.cost <- 28.62

# population = 3406876
# 0-4 = 223608 (0.0656)
# 5-19 = 639661 (0.187)
# 20-64 = 2235049 (0.656)
# 65+ = 308558 (0.0905)

# AR = attack rate of age group
# vax.comp = compliance for age group
# tot.pop = size of age group?
# totalpopulation?
# num.cases = number of (base) cases for age group
# case.after.vax = number of cases after vaccination


########## 0-4 years, High risk ##########
num.cases <- 18302.76
case.after.vax <- 360.2

num.cases - case.after.vax

#children.high.risk <- function(AR, vax.comp, tot.pop, totalpopulation, num.cases, case.after.vax){
  #risk.group = highrisk0_19
  risk.group = highrisk0_4  ##
  subpop.ar = num.cases * risk.group
  
  ### Health outcome probability, 0-4, high risk ###
  set.seed(1000)
  prob.outpatient = mean(runif(10000, min = 0.8257, max = 0.9595))
  prob.hospitalization = mean(runif(10000, min = 0.006, max = 0.02143))
  prob.death = mean(rtriang(10000, min = 0.00036, mode = 0.001, max = 0.01821))
  rest = 1-(prob.death + prob.hospitalization + prob.outpatient)
  
  # Number of each health outcome, 0-4 yrs, high risk
  num.outpatient = prob.outpatient * subpop.ar
  num.hospitalization = prob.hospitalization * subpop.ar
  num.death = prob.death * subpop.ar
  num.rest = rest * subpop.ar
  num.case.ch = num.outpatient + num.hospitalization + num.death + num.rest
  num.case.ch  # total number of cases = subpop.ar
  
  ### Cost of each health outcome, 0-4 yrs, high risk ###
  cost.death.ch =  46017 * cpi
  cost.hospitalization.ch = 20928 * cpi
  cost.outpatient.ch = 603 * cpi
  cost.rest.ch = 4 * cpi
  
  # Total cost, 0-4 yrs, high risk
  tot.cost.death.ch = cost.death.ch * num.death
  tot.cost.hospitalization.ch = cost.hospitalization.ch * num.hospitalization
  tot.cost.outpatient.ch = cost.outpatient.ch * num.outpatient
  tot.cost.rest.ch = cost.rest.ch * num.rest
  tot.cost.ch = tot.cost.death.ch + tot.cost.hospitalization.ch + tot.cost.outpatient.ch + tot.cost.rest.ch
  tot.cost.ch  ## total cost in base case
  
  # Vaccination, 0-4 yrs, high risk
  # vax.cost = 28.62
  vax.comp <- 0.4
  tot.pop <- 223608  ### pop size of 0-4 ###
  vaccination.cost = vax.cost * (vax.comp) * tot.pop * risk.group
  tot.cost.ch.vax = vaccination.cost
  tot.cost.ch.vax  ## vaccination cost
  
  # Non-vaccinated, 0-4 yrs, high risk
  subpop.ar.nonvax = case.after.vax * risk.group
  num.outpatient.nonvax = prob.outpatient * subpop.ar.nonvax
  num.hospitalization.nonvax = prob.hospitalization * subpop.ar.nonvax
  num.death.nonvax = prob.death * subpop.ar.nonvax
  num.rest.nonvax = rest * subpop.ar.nonvax
  num.case.ch.nonvax = num.outpatient.nonvax + num.hospitalization.nonvax + num.death.nonvax + num.rest.nonvax
  num.case.ch.nonvax  ## number of cases after vax = subpop.ar.nonvax
  
  # Total cost, 0-4 yrs, high risk, non-vax
  cost.death.ch.nonvax = cost.death.ch * num.death.nonvax
  cost.hospitalization.ch.nonvax = cost.hospitalization.ch * num.hospitalization.nonvax
  cost.outpatient.ch.nonvax = cost.outpatient.ch * num.outpatient.nonvax
  cost.rest.ch.nonvax = cost.rest.ch * num.rest.nonvax
  tot.cost.ch.nonvax = cost.death.ch.nonvax + cost.hospitalization.ch.nonvax + cost.outpatient.ch.nonvax + cost.rest.ch.nonvax
  tot.cost.ch.nonvax  # total cost in vaccination
  
#  res <- c(tot.cost.ch, tot.cost.ch.vax + tot.cost.ch.nonvax, tot.cost.ch - (tot.cost.ch.vax + tot.cost.ch.nonvax),
#           (tot.cost.ch - (tot.cost.ch.vax + tot.cost.ch.nonvax))/(totalpopulation * risk.group),
#           (tot.cost.ch - (tot.cost.ch.vax + tot.cost.ch.nonvax))/(vax.comp * tot.pop * risk.group),
#           (tot.cost.ch - (tot.cost.ch.vax + tot.cost.ch.nonvax))/(vax.comp * tot.pop * risk.group * vax.cost),
#           num.case.ch, num.case.ch.nonvax, num.death, num.death/(totalpopulation * risk.group))
#  return(res)
#}


########## 0-4 years, Low risk ##########

#children.low.risk <- function(AR, vax.comp, tot.pop, totalpopulation, num.cases, case.after.vax){
  #risk.group = 1 - highrisk0_19  
  risk.group = 1 - highrisk0_4
  subpop.ar = num.cases * risk.group
  
  # Health outcome probability, 0-4, low risk
  set.seed(1000)
  prob.outpatient = mean(runif(10000, min = 0.471, max = 0.548))
  prob.hospitalization = mean(runif(10000, min = 0.57/1000, max = 6.9/1000))
  prob.death = mean(rtriang(10000, min = 0.041/1000, mode = 0.07/1000, max = 0.3/1000))
  rest = 1 - (prob.death + prob.hospitalization + prob.outpatient)  
  
  # Number of each health outcome, 0-4 yrs, low risk
  num.outpatient = prob.outpatient * subpop.ar
  num.hospitalization = prob.hospitalization * subpop.ar
  num.death = prob.death * subpop.ar
  num.rest = rest * subpop.ar
  num.case.ch = num.outpatient + num.hospitalization + num.death + num.rest
  
  ### Cost of health outcomes, US 2018$, 0-4 yrs, low risk ###
  cost.death.ch = 1479040 * cpi
  cost.hospitalization.ch = 15224 * cpi
  cost.outpatient.ch = 458 * cpi
  cost.rest.ch = 116.5 * cpi
  
  # Total cost, 0-4 years, low risk
  tot.cost.death.ch = cost.death.ch * num.death
  tot.cost.hospitalization.ch = cost.hospitalization.ch * num.hospitalization
  tot.cost.outpatient.ch = cost.outpatient.ch * num.outpatient
  tot.cost.rest.ch = cost.rest.ch * num.rest
  tot.cost.ch = tot.cost.death.ch + tot.cost.hospitalization.ch + tot.cost.outpatient.ch + tot.cost.rest.ch
  tot.cost.ch
  
  # Vaccination, 0-4 years, low risk
  #vax.comp = 0.4
  #vax.cost = 28.62
  vaccination.cost = vax.cost * (vax.comp) * tot.pop * risk.group
  tot.cost.ch.vax = vaccination.cost
  
  # Non-vaccinated, 0-4 years, low risk
  subpop.ar.nonvax = case.after.vax * risk.group
  num.outpatient.nonvax = prob.outpatient * subpop.ar.nonvax
  num.hospitalization.nonvax = prob.hospitalization * subpop.ar.nonvax
  num.death.nonvax = prob.death * subpop.ar.nonvax
  num.rest.nonvax = rest * subpop.ar.nonvax
  num.case.ch.nonvax = num.outpatient.nonvax + num.hospitalization.nonvax + num.death.nonvax + num.rest.nonvax
  
  # Total cost, 0-4 yrs, low risk, non-vax
  cost.death.ch.nonvax = cost.death.ch * num.death.nonvax
  cost.hospitalization.ch.nonvax = cost.hospitalization.ch * num.hospitalization.nonvax
  cost.outpatient.ch.nonvax = cost.outpatient.ch * num.outpatient.nonvax
  cost.rest.ch.nonvax = cost.rest.ch * num.rest.nonvax
  tot.cost.ch.nonvax = cost.death.ch.nonvax + cost.hospitalization.ch.nonvax + cost.outpatient.ch.nonvax + cost.rest.ch.nonvax
  tot.cost.ch.nonvax
  
  res <- c(tot.cost.ch, tot.cost.ch.vax + tot.cost.ch.nonvax, tot.cost.ch - (tot.cost.ch.vax + tot.cost.ch.nonvax),
           (tot.cost.ch - (tot.cost.ch.vax + tot.cost.ch.nonvax))/(totalpopulation * risk.group),
           (tot.cost.ch - (tot.cost.ch.vax + tot.cost.ch.nonvax))/(vax.comp * tot.pop * risk.group),
           (tot.cost.ch - (tot.cost.ch.vax + tot.cost.ch.nonvax))/(vax.comp * tot.pop * risk.group * vax.cost),
           num.case.ch, num.case.ch.nonvax, num.death, num.death/(totalpopulation * risk.group))
  return(res)
  
  #return(paste("Totalcost.lowrisk.children.beforevax=", tot.cost.ch, "Number of cases before vax", subpop.ar, "Total cost of vaccinated =", tot.cost.ch.vax, "Number of vaccinated cases=", num.case.ch.vax, "Number of averted cases =", num.averted.case, "Total cost of unvaccinated =", tot.cost.ch.nonvax, "Number of cases unvaccinated =", num.case.ch.nonvax))
  
}



########## 5-19 years, High risk ########## 

########## Cost of each health outcome, 5-19 yrs, high risk


# 5-19 years, high risk
children.high.risk <- function(AR, vax.comp, tot.pop, totalpopulation, num.cases, case.after.vax){
  risk.group = highrisk0_19
  subpop.ar = num.cases * risk.group
  
  ### Health outcome probability, 5-19, high risk ###
  set.seed(1000)
  prob.outpatient = mean(runif(10000, min = 0.825, max = 0.958))
  prob.hospitalization = mean(runif(10000, min = 6/1000, max = 21.4/1000))
  prob.death = mean(rtriang(10000, min = 0.4/1000, mode = 0.6/1000, max = 21.9/1000))
  rest = 1-(prob.death + prob.hospitalization + prob.outpatient)  
  
  # Number of each health outcome, 5-19 yrs, high risk
  num.outpatient = prob.outpatient * subpop.ar
  num.hospitalization = prob.hospitalization * subpop.ar
  num.death = prob.death * subpop.ar
  num.rest = rest * subpop.ar
  num.case.ch = num.outpatient + num.hospitalization + num.death + num.rest
  
  ### Cost of each health outcome, 5-19 yrs, high risk ###
  cost.death.ch =  1487872 * cpi 
  cost.hospitalization.ch = 31894 * cpi
  cost.outpatient.ch = 948 * cpi
  cost.rest.ch = 116.5 * cpi
  
  # Total cost, 5-19 yrs, high risk
  tot.cost.death.ch = cost.death.ch * num.death
  tot.cost.hospitalization.ch = cost.hospitalization.ch * num.hospitalization
  tot.cost.outpatient.ch = cost.outpatient.ch * num.outpatient
  tot.cost.rest.ch = cost.rest.ch * num.rest
  tot.cost.ch = tot.cost.death.ch + tot.cost.hospitalization.ch + tot.cost.outpatient.ch + tot.cost.rest.ch
  tot.cost.ch
  
  # Vaccination, 5-19 yrs, high risk
  # vax.cost = 28.62
  vaccination.cost = vax.cost * (vax.comp) * tot.pop * risk.group
  tot.cost.ch.vax = vaccination.cost
  tot.cost.ch.vax
  
  # Non-vaccinated, 5-19 yrs, high risk
  subpop.ar.nonvax = case.after.vax * risk.group
  num.outpatient.nonvax = prob.outpatient * subpop.ar.nonvax
  num.hospitalization.nonvax = prob.hospitalization * subpop.ar.nonvax
  num.death.nonvax = prob.death * subpop.ar.nonvax
  num.rest.nonvax = rest * subpop.ar.nonvax
  num.case.ch.nonvax = num.outpatient.nonvax + num.hospitalization.nonvax + num.death.nonvax + num.rest.nonvax
  
  # Total cost, 5-19 yrs, high risk, non-vax
  cost.death.ch.nonvax = cost.death.ch * num.death.nonvax
  cost.hospitalization.ch.nonvax = cost.hospitalization.ch * num.hospitalization.nonvax
  cost.outpatient.ch.nonvax = cost.outpatient.ch * num.outpatient.nonvax
  cost.rest.ch.nonvax = cost.rest.ch * num.rest.nonvax
  tot.cost.ch.nonvax = cost.death.ch.nonvax + cost.hospitalization.ch.nonvax + cost.outpatient.ch.nonvax + cost.rest.ch.nonvax
  tot.cost.ch.nonvax
  
  res <- c(tot.cost.ch, tot.cost.ch.vax + tot.cost.ch.nonvax, tot.cost.ch - (tot.cost.ch.vax + tot.cost.ch.nonvax),
           (tot.cost.ch - (tot.cost.ch.vax + tot.cost.ch.nonvax))/(totalpopulation * risk.group),
           (tot.cost.ch - (tot.cost.ch.vax + tot.cost.ch.nonvax))/(vax.comp * tot.pop * risk.group),
           (tot.cost.ch - (tot.cost.ch.vax + tot.cost.ch.nonvax))/(vax.comp * tot.pop * risk.group * vax.cost),
           num.case.ch, num.case.ch.nonvax, num.death, num.death/(totalpopulation * risk.group))
  return(res)
}



########## 5-19 years, Low risk ##########

########## Cost of health outcomes, US 2018$, 5-19 yrs, low risk
cost.death.ch = 1479040 * cpi
cost.hospitalization.ch = 15224 * cpi
cost.outpatient.ch = 458 * cpi
cost.rest.ch = 116.5 * cpi

# 5-19 years, low risk
children.low.risk <- function(AR, vax.comp, tot.pop, totalpopulation, num.cases, case.after.vax){
  risk.group = 1 - highrisk0_19
  subpop.ar = num.cases * risk.group
  
  # Health outcome probability, 5-19, low risk
  set.seed(1000)
  prob.outpatient = mean(runif(10000, min = 0.471, max = 0.548))
  prob.hospitalization = mean(runif(10000, min = 0.57/1000, max = 6.9/1000))
  prob.death = mean(rtriang(10000, min = 0.041/1000, mode = 0.07/1000, max = 0.3/1000))
  rest = 1 - (prob.death + prob.hospitalization + prob.outpatient)  
  
  # Number of each health outcome, 5-19 yrs, low risk
  num.outpatient = prob.outpatient * subpop.ar
  num.hospitalization = prob.hospitalization * subpop.ar
  num.death = prob.death * subpop.ar
  num.rest = rest * subpop.ar
  num.case.ch = num.outpatient + num.hospitalization + num.death + num.rest
  
  ## Cost of health outcomes, US 2018$, 5-19 yrs, low risk
  # cost.death.ch = 1479040 * cpi
  # cost.hospitalization.ch = 15224 * cpi
  # cost.outpatient.ch = 458 * cpi
  # cost.rest.ch = 116.5 * cpi
  
  # Total cost, 5-19 years, low risk
  tot.cost.death.ch = cost.death.ch * num.death
  tot.cost.hospitalization.ch = cost.hospitalization.ch * num.hospitalization
  tot.cost.outpatient.ch = cost.outpatient.ch * num.outpatient
  tot.cost.rest.ch = cost.rest.ch * num.rest
  tot.cost.ch = tot.cost.death.ch + tot.cost.hospitalization.ch + tot.cost.outpatient.ch + tot.cost.rest.ch
  tot.cost.ch
  
  # Vaccination, 5-19 years, low risk
  #vax.comp = 0.4
  #vax.cost = 28.62
  vaccination.cost = vax.cost * (vax.comp) * tot.pop * risk.group
  tot.cost.ch.vax = vaccination.cost
  
  # Non-vaccinated, 5-19 years, low risk
  subpop.ar.nonvax = case.after.vax * risk.group
  num.outpatient.nonvax = prob.outpatient * subpop.ar.nonvax
  num.hospitalization.nonvax = prob.hospitalization * subpop.ar.nonvax
  num.death.nonvax = prob.death * subpop.ar.nonvax
  num.rest.nonvax = rest * subpop.ar.nonvax
  num.case.ch.nonvax = num.outpatient.nonvax + num.hospitalization.nonvax + num.death.nonvax + num.rest.nonvax
  
  # Total cost, 5-19 yrs, low risk, non-vax
  cost.death.ch.nonvax = cost.death.ch * num.death.nonvax
  cost.hospitalization.ch.nonvax = cost.hospitalization.ch * num.hospitalization.nonvax
  cost.outpatient.ch.nonvax = cost.outpatient.ch * num.outpatient.nonvax
  cost.rest.ch.nonvax = cost.rest.ch * num.rest.nonvax
  tot.cost.ch.nonvax = cost.death.ch.nonvax + cost.hospitalization.ch.nonvax + cost.outpatient.ch.nonvax + cost.rest.ch.nonvax
  tot.cost.ch.nonvax
  
  res <- c(tot.cost.ch, tot.cost.ch.vax + tot.cost.ch.nonvax, tot.cost.ch - (tot.cost.ch.vax + tot.cost.ch.nonvax),
           (tot.cost.ch - (tot.cost.ch.vax + tot.cost.ch.nonvax))/(totalpopulation * risk.group),
           (tot.cost.ch - (tot.cost.ch.vax + tot.cost.ch.nonvax))/(vax.comp * tot.pop * risk.group),
           (tot.cost.ch - (tot.cost.ch.vax + tot.cost.ch.nonvax))/(vax.comp * tot.pop * risk.group * vax.cost),
           num.case.ch, num.case.ch.nonvax, num.death, num.death/(totalpopulation * risk.group))
  return(res)
  
  #return(paste("Totalcost.lowrisk.childlren.beforevax=", tot.cost.ch, "Numberof num.cases before vax", subpop.ar, "Total cost of vaxinated=", tot.cost.ch.vax, "Number of num.cases in vaxinated=", num.case.ch.vax, "Number of averted num.cases=", num.averted.case, "Total cost of unvaccinated=", tot.cost.ch.nonvax, "Number of num.cases in unvaccinated=", num.case.ch.nonvax))
  
}



########## 20-64 years, High risk ##########

########## Cost of each health outcome, 20-64 years, high risk
cost.death.ad =  848692 * cpi 
cost.hospitalization.ad = 31328 * cpi
cost.outpatient.ad = 815 * cpi
cost.rest.ad = 79 * cpi

# 20-64 years, high risk
adult.high.risk <- function(AR, vax.comp, tot.pop, totalpopulation, num.cases, case.after.vax){
  risk.group = highrisk20_64
  subpop.ar = num.cases * risk.group
  
  # Health outcome probability, 20-64 years, high risk
  set.seed(1000)
  prob.outpatient = mean(runif(10000, min = 0.583, max = 0.647))
  prob.hospitalization = mean(runif(10000, min = 6.9/1000, max = 22.3/1000))
  #prob.death = mean(rtriang(1000, min=0.21/1000, mode=0.31/1000, max=0.41/1000))
  prob.death = mean(runif(10000, min = 0.8/1000, max = 24.9/1000))
  rest = 1 - (prob.death + prob.hospitalization + prob.outpatient)  
  
  # Number of each health outcome, 20-64 years, high risk
  num.outpatient = prob.outpatient * subpop.ar
  num.hospitalization = prob.hospitalization * subpop.ar
  num.death = prob.death * subpop.ar
  num.rest = rest * subpop.ar
  num.case.ad = num.outpatient + num.hospitalization + num.death + num.rest
  
  ## Cost of each health outcome, 20-64 years, high risk
  # cost.death.ad =  848692 * cpi 
  # cost.hospitalization.ad = 31328 * cpi
  # cost.outpatient.ad = 815 * cpi
  # cost.rest.ad = 79 * cpi
  
  # Total cost, 20-64 years, high risk
  tot.cost.death.ad = cost.death.ad * num.death
  tot.cost.hospitalization.ad = cost.hospitalization.ad * num.hospitalization
  tot.cost.outpatient.ad = cost.outpatient.ad * num.outpatient
  tot.cost.rest.ad = cost.rest.ad * num.rest
  tot.cost.ad = tot.cost.death.ad + tot.cost.hospitalization.ad + tot.cost.outpatient.ad + tot.cost.rest.ad
  tot.cost.ad
  
  # Vaccination, 20-64 years, high risk
  vax.cost = 28.62
  vaccination.cost = vax.cost * (vax.comp) * tot.pop * risk.group
  tot.cost.ad.vax = vaccination.cost
  tot.cost.ad.vax
  
  # Non-vaccinated, 20-64 years, high risk
  subpop.ar.nonvax = case.after.vax * risk.group
  num.outpatient.nonvax = prob.outpatient * subpop.ar.nonvax
  num.hospitalization.nonvax = prob.hospitalization * subpop.ar.nonvax
  num.death.nonvax = prob.death * subpop.ar.nonvax
  num.rest.nonvax = rest * subpop.ar.nonvax
  num.case.ad.nonvax = num.outpatient.nonvax + num.hospitalization.nonvax + num.death.nonvax + num.rest.nonvax
  
  # Total cost, 20-64 years, high risk, non-vax
  cost.death.ad.nonvax = cost.death.ad * num.death.nonvax
  cost.hospitalization.ad.nonvax = cost.hospitalization.ad * num.hospitalization.nonvax
  cost.outpatient.ad.nonvax = cost.outpatient.ad * num.outpatient.nonvax
  cost.rest.ad.nonvax = cost.rest.ad * num.rest.nonvax
  tot.cost.ad.nonvax = cost.death.ad.nonvax + cost.hospitalization.ad.nonvax + cost.outpatient.ad.nonvax + cost.rest.ad.nonvax
  tot.cost.ad.nonvax
  
  res <- c(tot.cost.ad, tot.cost.ad.vax + tot.cost.ad.nonvax, tot.cost.ad - (tot.cost.ad.vax + tot.cost.ad.nonvax),
           (tot.cost.ad - (tot.cost.ad.vax + tot.cost.ad.nonvax))/(totalpopulation * risk.group),
           (tot.cost.ad - (tot.cost.ad.vax + tot.cost.ad.nonvax))/(vax.comp * tot.pop * risk.group),
           (tot.cost.ad - (tot.cost.ad.vax + tot.cost.ad.nonvax))/(vax.comp * tot.pop * risk.group * vax.cost),
           num.case.ad, num.case.ad.nonvax, num.death, num.death/(totalpopulation * risk.group))
  return(res)
}



########## 20-64 years, Low risk ##########

########## Cost of each health outcome, 20-64 years, low risk
cost.death.ad =  848692 * cpi 
cost.hospitalization.ad = 31328 * cpi
cost.outpatient.ad = 815 * cpi
cost.rest.ad = 79 * cpi

# 20-64 years, low risk
adult.low.risk <- function(AR, vax.comp, tot.pop, totalpopulation, num.cases, case.after.vax){
  risk.group = 1 - highrisk20_64
  subpop.ar = num.cases * risk.group
  
  # Health outcome probability, 20-64 years, low risk
  set.seed(1000)
  prob.outpatient = mean(runif(10000,min = 0.333, max = 0.370))
  prob.hospitalization = mean(runif(10000, min = 1.5/1000, max = 12/1000))
  prob.death = mean(rtriang(10000, min = 0.21/1000, mode = 0.31/1000, max = 0.41/1000))
  rest = 1 - (prob.death + prob.hospitalization + prob.outpatient)  
  
  # Number of each health outcome, 20-64 years, low risk
  num.outpatient = prob.outpatient * subpop.ar
  num.hospitalization = prob.hospitalization * subpop.ar
  num.death = prob.death * subpop.ar
  num.rest = rest * subpop.ar
  num.case.ad = num.outpatient + num.hospitalization + num.death + num.rest
  
  ## Cost of each health outcome, 20-64 years, low risk
  # cost.death.ad =  848692 * cpi 
  # cost.hospitalization.ad = 31328 * cpi
  # cost.outpatient.ad = 815 * cpi
  # cost.rest.ad = 79 * cpi
  
  # Total cost, 20-64 years, low risk
  tot.cost.death.ad = cost.death.ad * num.death
  tot.cost.hospitalization.ad = cost.hospitalization.ad * num.hospitalization
  tot.cost.outpatient.ad = cost.outpatient.ad * num.outpatient
  tot.cost.rest.ad = cost.rest.ad * num.rest
  tot.cost.ad = tot.cost.death.ad + tot.cost.hospitalization.ad + tot.cost.outpatient.ad + tot.cost.rest.ad
  tot.cost.ad
  
  # Vaccination, 20-64 years, low risk
  #vax.cost = 28.62
  vaccination.cost = vax.cost * (vax.comp) * tot.pop * risk.group
  tot.cost.ad.vax = vaccination.cost
  tot.cost.ad.vax
  
  # Non-vaccinated, 20-64 years, low risk
  subpop.ar.nonvax = case.after.vax * risk.group
  num.outpatient.nonvax = prob.outpatient * subpop.ar.nonvax
  num.hospitalization.nonvax = prob.hospitalization * subpop.ar.nonvax
  num.death.nonvax = prob.death * subpop.ar.nonvax
  num.rest.nonvax = rest * subpop.ar.nonvax
  num.case.ad.nonvax = num.outpatient.nonvax + num.hospitalization.nonvax + num.death.nonvax + num.rest.nonvax
  
  # Total cost, 20-64 yrs, low risk, non-vax
  cost.death.ad.nonvax = cost.death.ad * num.death.nonvax
  cost.hospitalization.ad.nonvax = cost.hospitalization.ad * num.hospitalization.nonvax
  cost.outpatient.ad.nonvax = cost.outpatient.ad * num.outpatient.nonvax
  cost.rest.ad.nonvax = cost.rest.ad * num.rest.nonvax
  tot.cost.ad.nonvax = cost.death.ad.nonvax + cost.hospitalization.ad.nonvax + cost.outpatient.ad.nonvax + cost.rest.ad.nonvax
  tot.cost.ad.nonvax
  
  res <- c(tot.cost.ad, tot.cost.ad.vax + tot.cost.ad.nonvax, tot.cost.ad - (tot.cost.ad.vax + tot.cost.ad.nonvax),
           (tot.cost.ad - (tot.cost.ad.vax + tot.cost.ad.nonvax))/(totalpopulation * risk.group),
           (tot.cost.ad - (tot.cost.ad.vax + tot.cost.ad.nonvax))/(vax.comp * tot.pop * risk.group),
           (tot.cost.ad - (tot.cost.ad.vax + tot.cost.ad.nonvax))/(vax.comp * tot.pop * risk.group * vax.cost),
           num.case.ad, num.case.ad.nonvax, num.death, num.death/(totalpopulation * risk.group))
  return(res)
}



########## 65+ years, High risk ##########

########## Cost of each health outcome, 65+ years, high risk
cost.death.elder =  261544 * cpi 
cost.hospitalization.elder = 20269 * cpi
cost.outpatient.elder = 2826 * cpi
cost.rest.elder = 121 * cpi

# 65+, high risk
senior.high.risk <- function(AR, vax.comp, tot.pop, totalpopulation, num.cases, case.after.vax){
  risk.group = highrisk65
  subpop.ar = num.cases * risk.group
  
  # Health outcome probability, 65+ years, high risk
  set.seed(1000)
  prob.outpatient = mean(runif(10000, min = 0.656, max = 0.682))
  prob.hospitalization = mean(runif(10000, min = 33.3/1000, max = 68.4/1000))
  #prob.death = mean(rtriang(1000, min=2.3/1000, mode=3.51/1000, max=4.52/1000))
  prob.death = mean(runif(10000, min = 23/1000, max = 29.6/1000))
  rest = 1 - (prob.death + prob.hospitalization + prob.outpatient)  
  
  # Number of each health outcome, 65+ years, high risk
  num.outpatient = prob.outpatient * subpop.ar
  num.hospitalization = prob.hospitalization * subpop.ar
  num.death = prob.death * subpop.ar
  num.rest = rest * subpop.ar
  num.case.eld = num.outpatient + num.hospitalization + num.death + num.rest
  
  # # Cost of each health outcome, 65+ years, high risk
  # cost.death.elder =  261544 * cpi 
  # cost.hospitalization.elder = 20269 * cpi
  # cost.outpatient.elder = 2826 * cpi
  # cost.rest.elder = 121 * cpi
  
  # Total cost, 65+ years, high risk
  tot.cost.death.elder = cost.death.elder * num.death
  tot.cost.hospitalization.elder = cost.hospitalization.elder * num.hospitalization
  tot.cost.outpatient.elder = cost.outpatient.elder * num.outpatient
  tot.cost.rest.elder = cost.rest.elder * num.rest
  tot.cost.elder = tot.cost.death.elder + tot.cost.hospitalization.elder + tot.cost.outpatient.elder + tot.cost.rest.elder
  tot.cost.elder
  
  # Vaccination, 65+ years, high risk
  vax.cost = 28.62
  vaccination.cost = vax.cost * (vax.comp) * tot.pop * risk.group
  tot.cost.elder.vax = vaccination.cost
  tot.cost.elder.vax

  # Nonvaccinated, 65+ years, high risk
  subpop.ar.nonvax = case.after.vax * risk.group
  num.outpatient.nonvax = prob.outpatient * subpop.ar.nonvax
  num.hospitalization.nonvax = prob.hospitalization * subpop.ar.nonvax
  num.death.nonvax = prob.death * subpop.ar.nonvax
  num.rest.nonvax = rest * subpop.ar.nonvax
  num.case.eld.nonvax = num.outpatient.nonvax + num.hospitalization.nonvax + num.death.nonvax + num.rest.nonvax
  
  # Total cost, 65+ years, high risk, non-vax
  cost.death.elder.nonvax = cost.death.elder * num.death.nonvax
  cost.hospitalization.elder.nonvax = cost.hospitalization.elder * num.hospitalization.nonvax
  cost.outpatient.elder.nonvax = cost.outpatient.elder * num.outpatient.nonvax
  cost.rest.elder.nonvax = cost.rest.elder * num.rest.nonvax
  tot.cost.elder.nonvax = cost.death.elder.nonvax + cost.hospitalization.elder.nonvax + cost.outpatient.elder.nonvax + cost.rest.elder.nonvax
  tot.cost.elder.nonvax
  
  res <- c(tot.cost.elder, tot.cost.elder.vax + tot.cost.elder.nonvax, tot.cost.elder - (tot.cost.elder.vax + tot.cost.elder.nonvax),
           (tot.cost.elder - (tot.cost.elder.vax + tot.cost.elder.nonvax))/(totalpopulation * risk.group),
           (tot.cost.elder - (tot.cost.elder.vax + tot.cost.elder.nonvax))/(vax.comp * tot.pop * risk.group),
           (tot.cost.elder - (tot.cost.elder.vax + tot.cost.elder.nonvax))/(vax.comp * tot.pop * risk.group*vax.cost),
           num.case.eld, num.case.eld.nonvax, num.death, num.death/(totalpopulation * risk.group))  
  return(res)
}



########## 65+ years, Low risk ########## 

########## Cost of each health outcome, 65+ years, low risk
cost.death.elder = 249749 * cpi 
cost.hospitalization.elder = 13508 * cpi
cost.outpatient.elder = 1156 * cpi
cost.rest.elder = 121 * cpi

# 65+ years, low risk
senior.low.risk <- function(AR, vax.comp, tot.pop, totalpopulation, num.cases, case.after.vax){
  risk.group = 1 - highrisk65
  subpop.ar = num.cases * risk.group

  # Health outcome probability, 65+ years, low risk
  set.seed(1000)
  prob.outpatient = mean(runif(10000, min = 0.375, max = 0.389))
  prob.hospitalization = mean(runif(10000, min = 12.5/1000, max = 15.8/1000))
  prob.death = mean(rtriang(10000, min = 2.3/1000, mode = 3.51/1000, max = 4.52/1000))
  #prob.death = mean(runif(1000, min=0.8/1000, max=24.9/1000))
  rest = 1 - (prob.death + prob.hospitalization + prob.outpatient)  
  
  # Number of each health outcome, 65+ years, low risk
  num.outpatient = prob.outpatient * subpop.ar
  num.hospitalization = prob.hospitalization * subpop.ar
  num.death = prob.death * subpop.ar
  num.rest = rest * subpop.ar
  num.case.eld = num.outpatient + num.hospitalization + num.death + num.rest
  
  # # Cost of each health outcome, 65+ years, low risk
  # cost.death.elder = 249749 * cpi 
  # cost.hospitalization.elder = 13508 * cpi
  # cost.outpatient.elder = 1156 * cpi
  # cost.rest.elder = 121 * cpi
  
  # Total cost, 65+ years, low risk
  tot.cost.death.elder = cost.death.elder * num.death
  tot.cost.hospitalization.elder = cost.hospitalization.elder * num.hospitalization
  tot.cost.outpatient.elder = cost.outpatient.elder * num.outpatient
  tot.cost.rest.elder = cost.rest.elder * num.rest
  tot.cost.elder = tot.cost.death.elder + tot.cost.hospitalization.elder + tot.cost.outpatient.elder + tot.cost.rest.elder
  tot.cost.elder
  
  # Vaccination, 65+ years, low risk
  #vax.cost = 28.62
  vaccination.cost = vax.cost * (vax.comp) * tot.pop * risk.group
  tot.cost.elder.vax = vaccination.cost
  tot.cost.elder.vax
  
  # Non-vaccinated, 65+ years, low risk
  subpop.ar.nonvax = case.after.vax * risk.group
  num.outpatient.nonvax = prob.outpatient * subpop.ar.nonvax
  num.hospitalization.nonvax = prob.hospitalization * subpop.ar.nonvax
  num.death.nonvax = prob.death * subpop.ar.nonvax
  num.rest.nonvax = rest * subpop.ar.nonvax
  num.case.eld.nonvax = num.outpatient.nonvax + num.hospitalization.nonvax + num.death.nonvax + num.rest.nonvax
  
  # Total cost, 65+, low risk, non-vax
  cost.death.elder.nonvax = cost.death.elder * num.death.nonvax
  cost.hospitalization.elder.nonvax = cost.hospitalization.elder * num.hospitalization.nonvax
  cost.outpatient.elder.nonvax = cost.outpatient.elder * num.outpatient.nonvax
  cost.rest.elder.nonvax = cost.rest.elder * num.rest.nonvax
  tot.cost.elder.nonvax = cost.death.elder.nonvax + cost.hospitalization.elder.nonvax + cost.outpatient.elder.nonvax + cost.rest.elder.nonvax
  tot.cost.elder.nonvax
  
  res <- c(tot.cost.elder, tot.cost.elder.vax + tot.cost.elder.nonvax, tot.cost.elder - (tot.cost.elder.vax + tot.cost.elder.nonvax),
           (tot.cost.elder - (tot.cost.elder.vax + tot.cost.elder.nonvax))/(totalpopulation * risk.group),
           (tot.cost.elder - (tot.cost.elder.vax + tot.cost.elder.nonvax))/(vax.comp * tot.pop * risk.group),
           (tot.cost.elder - (tot.cost.elder.vax + tot.cost.elder.nonvax))/(vax.comp * tot.pop * risk.group * vax.cost),
           num.case.eld, num.case.eld.nonvax, num.death, num.death/(totalpopulation * risk.group))
  return(res)
}
