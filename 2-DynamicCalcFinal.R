#

library(mc2d)

# risk.group
highrisk0_19 <- 0.064  # meltzer 1999
highrisk0_4 <- 0.052  # molinari 2007
highrisk5_17 <- 0.106  # molinari 2007
highrisk20_64 <- 0.144  # meltzer 1999
highrisk65 <- 0.512  # molinari 2007

# consumer price adjustment index
cpi <- ((513.135 - 379.516)/379.516) + 1


##### 0-19 years - Low risk #####

Children.low.risk <- function(AR, vax.comp, tot.pop, totalpopulation, num.cases, case.after.vax){
  risk.group = 1 - highrisk0_19
  subpop.ar = num.cases * risk.group
  
  set.seed(1000)
  prob.outpatient = mean(runif(10000, min = 0.471, max = 0.548))
  prob.hospitalization = mean(runif(10000, min = 0.57/1000, max = 6.9/1000))
  prob.death = mean(rtriang(10000, min = 0.041/1000, mode = 0.07/1000, max = 0.3/1000))
  rest = 1 - (prob.death + prob.hospitalization + prob.outpatient)  
  
  # Number of each health outcome
  num.outpatient = prob.outpatient * subpop.ar
  num.hospitalization = prob.hospitalization * subpop.ar
  num.death = prob.death * subpop.ar
  num.rest = rest * subpop.ar
  num.case.ch = num.outpatient + num.hospitalization + num.death + num.rest
  
  # Cost of health outcomes (adjusted to 2018$)
  cost.death.ch = 1479040 * cpi
  cost.hospitalization.ch = 15224 * cpi
  cost.outpatient.ch = 458 * cpi
  cost.rest.ch = 116.5 * cpi
  
  # Total cost for 0-19 years, low risk
  tot.cost.death.ch = cost.death.ch * num.death
  tot.cost.hospitalization.ch = cost.hospitalization.ch * num.hospitalization
  tot.cost.outpatient.ch = cost.outpatient.ch * num.outpatient
  tot.cost.rest.ch = cost.rest.ch * num.rest
  tot.cost.ch = tot.cost.death.ch + tot.cost.hospitalization.ch + tot.cost.outpatient.ch + tot.cost.rest.ch
  tot.cost.ch
  
  # Vaccination
  #vax.comp = 0.4
  vax.cost = 28.62
  vaccination.cost = vax.cost * (vax.comp) * tot.pop * risk.group
  tot.cost.ch.vax = vaccination.cost
  
  # Non-vaccinated
subpop.ar.nonvax = case.after.vax * risk.group
  num.outpatient.nonvax = prob.outpatient * high.risk.ch.nonvax
  num.hospitalization.nonvax = prob.hospitalization * high.risk.ch.nonvax
  num.death.nonvax = prob.death * high.risk.ch.nonvax
  num.rest.nonvax = rest * high.risk.ch.nonvax
  num.case.ch.nonvax = num.outpatient.nonvax + num.hospitalization.nonvax + num.death.nonvax + num.rest.nonvax
  
  # Total cost for 0-19 yrs, low risk, non-vax
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
  
  #return(paste("Totalcost.lowrisk.childlren.beforevax=",tot.cost.ch,"Numberof num.cases before vax",subpop.ar,
  #             "Total cost of vaxinated=",tot.cost.ch.vax,"NUmber of num.cases in vaxinated=",num.case.ch.vax,
  #             "NUmber of averted num.cases=",num.averted.case,"Total cost of unvaccinated=",tot.cost.ch.nonvax,"Number of num.cases in unvaccinated=",num.case.ch.nonvax))
  
}



##### 0-19 years - High risk #####

Children.high.risk <- function(AR, vax.comp, tot.pop, totalpopulation, num.cases, case.after.vax){
  risk.group = highrisk0_19
  subpop.ar = num.cases * risk.group
  
  set.seed(1000)
  prob.outpatient = mean(runif(10000, min = 0.825, max = 0.958))
  prob.hospitalization = mean(runif(10000, min = 6/1000, max = 21.4/1000))
  prob.death = mean(rtriang(10000, min = 0.4/1000, mode = 0.6/1000, max = 21.9/1000))
  rest = 1-(prob.death + prob.hospitalization + prob.outpatient)  
  
  # Number of each health outcome
  num.outpatient = prob.outpatient * subpop.ar
  num.hospitalization = prob.hospitalization * subpop.ar
  num.death = prob.death * subpop.ar
  num.rest = rest * subpop.ar
  num.case.ch = num.outpatient + num.hospitalization + num.death + num.rest
  
  # Cost of each health outcome - high risk
  cost.death.ch =  1487872 * cpi 
  cost.hospitalization.ch = 31894 * cpi
  cost.outpatient.ch = 948 * cpi
  cost.rest.ch = 116.5 * cpi
  
  # Total cost
  tot.cost.death.ch = cost.death.ch * num.death
  tot.cost.hospitalization.ch = cost.hospitalization.ch * num.hospitalization
  tot.cost.outpatient.ch = cost.outpatient.ch * num.outpatient
  tot.cost.rest.ch = cost.rest.ch * num.rest
  tot.cost.ch = tot.cost.death.ch + tot.cost.hospitalization.ch + tot.cost.outpatient.ch + tot.cost.rest.ch
  tot.cost.ch
  
  # Vaccination
  vax.cost = 28.62
  vaccination.cost = vax.cost * (vax.comp) * tot.pop * risk.group
  tot.cost.ch.vax = vaccination.cost
  tot.cost.ch.vax
  
  # Health outcome probability
  
  # Non-vaccinated
  subpop.ar.nonvax = case.after.vax * risk.group
  num.outpatient.nonvax = prob.outpatient * high.risk.ch.nonvax
  num.hospitalization.nonvax = prob.hospitalization * high.risk.ch.nonvax
  num.death.nonvax = prob.death * high.risk.ch.nonvax
  num.rest.nonvax = rest * high.risk.ch.nonvax
  num.case.ch.nonvax = num.outpatient.nonvax + num.hospitalization.nonvax + num.death.nonvax + num.rest.nonvax
  
  # Total cost for 0-19 yrs, high risk, non-vax
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



##### Adult 20-64 - Low risk #####

adult.low.risk <- function(AR, vax.comp, tot.pop, totalpopulation, num.cases, case.after.vax){
  risk.group = 1 - highrisk20_64
  subpop.ar = num.cases * risk.group
  
  set.seed(1000)
  prob.outpatient = mean(runif(10000,min = 0.333, max = 0.370))
  prob.hospitalization = mean(runif(10000, min = 1.5/1000, max = 12/1000))
  prob.death = mean(rtriang(10000, min = 0.21/1000, mode = 0.31/1000, max = 0.41/1000))
  rest = 1 - (prob.death + prob.hospitalization + prob.outpatient)  
  
  # Number of each health outcome
  num.outpatient = prob.outpatient * subpop.ar
  num.hospitalization = prob.hospitalization * subpop.ar
  num.death = prob.death * subpop.ar
  num.rest = rest * subpop.ar
  num.case.ch = num.outpatient + num.hospitalization + num.death + num.rest
  
  # Cost of each health outcome for adults 20-64 yrs, low risk
  cost.death.ch =  848692 * cpi 
  cost.hospitalization.ch = 31328 * cpi
  cost.outpatient.ch = 815 * cpi
  cost.rest.ch = 79 * cpi
  
  # Total cost
  tot.cost.death.ch = cost.death.ch * num.death
  tot.cost.hospitalization.ch = cost.hospitalization.ch * num.hospitalization
  tot.cost.outpatient.ch = cost.outpatient.ch * num.outpatient
  tot.cost.rest.ch = cost.rest.ch * num.rest
  tot.cost.ch = tot.cost.death.ch + tot.cost.hospitalization.ch + tot.cost.outpatient.ch + tot.cost.rest.ch
  tot.cost.ch
  
  # Vaccination
  vax.cost = 28.62
  vaccination.cost = vax.cost * (vax.comp) * tot.pop * risk.group
  tot.cost.ch.vax = vaccination.cost
  tot.cost.ch.vax
  
  # Non-vaccinated
  
  # Health outcome probability
  subpop.ar.nonvax = case.after.vax * risk.group
  num.outpatient.nonvax = prob.outpatient * high.risk.ch.nonvax
  num.hospitalization.nonvax = prob.hospitalization * high.risk.ch.nonvax
  num.death.nonvax = prob.death * high.risk.ch.nonvax
  num.rest.nonvax = rest * high.risk.ch.nonvax
  num.case.ch.nonvax = num.outpatient.nonvax + num.hospitalization.nonvax + num.death.nonvax + num.rest.nonvax
  
  # Total cost for adults 20-64 yrs, low risk, non-vax
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


##### Adults 20-64 - High risk #####

adult.high.risk <- function(AR, vax.comp, tot.pop, totalpopulation, num.num.cases, case.after.vax){
  risk.group = highrisk20_64
  subpop.ar = num.cases * risk.group
  
  set.seed(1000)
  prob.outpatient = mean(runif(10000, min = 0.583, max = 0.647))
  prob.hospitalization = mean(runif(10000, min = 6.9/1000, max = 22.3/1000))
  #prob.death = mean(rtriang(1000, min=0.21/1000, mode=0.31/1000, max=0.41/1000))
  prob.death = mean(runif(10000, min = 0.8/1000, max = 24.9/1000))
  rest = 1 - (prob.death + prob.hospitalization + prob.outpatient)  
  
  # Number of each health outcome
  num.outpatient = prob.outpatient * subpop.ar
  num.hospitalization = prob.hospitalization * subpop.ar
  num.death = prob.death * subpop.ar
  num.rest = rest * subpop.ar
  num.case.ch = num.outpatient + num.hospitalization + num.death + num.rest
  
  # Cost of each health outcome for adults 20-64, high risk
  cost.death.ch =  848692 * cpi 
  cost.hospitalization.ch = 31328 * cpi
  cost.outpatient.ch = 815 * cpi
  cost.rest.ch = 79 * cpi
  
  # Total cost
  tot.cost.death.ch = cost.death.ch * num.death
  tot.cost.hospitalization.ch = cost.hospitalization.ch * num.hospitalization
  tot.cost.outpatient.ch = cost.outpatient.ch * num.outpatient
  tot.cost.rest.ch = cost.rest.ch * num.rest
  tot.cost.ch = tot.cost.death.ch + tot.cost.hospitalization.ch + tot.cost.outpatient.ch + tot.cost.rest.ch
  tot.cost.ch
  
  # Vaccination
  vax.cost = 28.62
  vaccination.cost = vax.cost * (vax.comp) * tot.pop * risk.group
  tot.cost.ch.vax = vaccination.cost
  tot.cost.ch.vax
  
  # Health outcome probability
  
  # Non-vaccinated
  subpop.ar.nonvax = case.after.vax * risk.group
  num.outpatient.nonvax = prob.outpatient * high.risk.ch.nonvax
  num.hospitalization.nonvax = prob.hospitalization * high.risk.ch.nonvax
  num.death.nonvax = prob.death * high.risk.ch.nonvax
  num.rest.nonvax = rest * high.risk.ch.nonvax
  num.case.ch.nonvax = num.outpatient.nonvax + num.hospitalization.nonvax + num.death.nonvax + num.rest.nonvax
  
  # Total cost for adults 20-64, high risk, non-vax
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



##### Senior - Low risk #####

senior.low.risk <- function(AR, vax.comp, tot.pop, totalpopulation, num.cases, case.after.vax){
  risk.group = 1 - highrisk65
  subpop.ar = num.cases * risk.group
  
  set.seed(1000)
  prob.outpatient = mean(runif(10000, min = 0.375, max = 0.389))
  prob.hospitalization = mean(runif(10000, min = 12.5/1000, max = 15.8/1000))
  prob.death = mean(rtriang(10000, min = 2.3/1000, mode = 3.51/1000, max = 4.52/1000))
  #prob.death = mean(runif(1000, min=0.8/1000, max=24.9/1000))
  rest = 1 - (prob.death + prob.hospitalization + prob.outpatient)  
  
  # Number of each health outcome
  num.outpatient = prob.outpatient * subpop.ar
  num.hospitalization = prob.hospitalization * subpop.ar
  num.death = prob.death * subpop.ar
  num.rest = rest * subpop.ar
  num.case.ch = num.outpatient + num.hospitalization + num.death + num.rest
  
  # Cost of each health outcome for elderly 65+, low risk
  cost.death.ch = 249749 * cpi 
  cost.hospitalization.ch = 13508 * cpi
  cost.outpatient.ch = 1156 * cpi
  cost.rest.ch = 121 * cpi
  
  # Total cost for elderly 65+, low risk
  tot.cost.death.ch = cost.death.ch * num.death
  tot.cost.hospitalization.ch = cost.hospitalization.ch * num.hospitalization
  tot.cost.outpatient.ch = cost.outpatient.ch * num.outpatient
  tot.cost.rest.ch = cost.rest.ch * num.rest
  tot.cost.ch = tot.cost.death.ch + tot.cost.hospitalization.ch + tot.cost.outpatient.ch + tot.cost.rest.ch
  tot.cost.ch
  
  # Vaccination
  vax.cost = 28.62
  vaccination.cost = vax.cost * (vax.comp) * tot.pop * risk.group
  tot.cost.ch.vax = vaccination.cost
  tot.cost.ch.vax
  
  # Health outcome probability
  
  # Non-vaccinated
  
subpop.ar.nonvax = case.after.vax * risk.group
  num.outpatient.nonvax = prob.outpatient * high.risk.ch.nonvax
  num.hospitalization.nonvax = prob.hospitalization * high.risk.ch.nonvax
  num.death.nonvax = prob.death * high.risk.ch.nonvax
  num.rest.nonvax = rest * high.risk.ch.nonvax
  num.case.ch.nonvax = num.outpatient.nonvax + num.hospitalization.nonvax + num.death.nonvax + num.rest.nonvax
  
  # Total cost for elderly 65+, low risk, non-vax
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



##### Senior - High risk #####

senior.high.risk <- function(AR, vax.comp, tot.pop, totalpopulation, num.cases, case.after.vax){
  risk.group = highrisk65
  subpop.ar = num.cases * risk.group
  
  set.seed(1000)
  prob.outpatient = mean(runif(10000, min = 0.656, max = 0.682))
  prob.hospitalization = mean(runif(10000, min = 33.3/1000, max = 68.4/1000))
  #prob.death = mean(rtriang(1000, min=2.3/1000, mode=3.51/1000, max=4.52/1000))
  prob.death = mean(runif(10000, min = 23/1000, max = 29.6/1000))
  rest = 1 - (prob.death + prob.hospitalization + prob.outpatient)  
  
  # Number of patient with each health outcome
  num.outpatient = prob.outpatient * subpop.ar
  num.hospitalization = prob.hospitalization * subpop.ar
  num.death = prob.death * subpop.ar
  num.rest = rest * subpop.ar
  num.case.ch = num.outpatient + num.hospitalization + num.death + num.rest
  
  # Cost of each health outcome for elderly 65+, high risk
  cost.death.ch =  261544 * cpi 
  cost.hospitalization.ch = 20269 * cpi
  cost.outpatient.ch = 2826 * cpi
  cost.rest.ch = 121 * cpi
  
  # Total cost for elderly 65+, high risk
  tot.cost.death.ch = cost.death.ch * num.death
  tot.cost.hospitalization.ch = cost.hospitalization.ch * num.hospitalization
  tot.cost.outpatient.ch = cost.outpatient.ch * num.outpatient
  tot.cost.rest.ch = cost.rest.ch * num.rest
  tot.cost.ch = tot.cost.death.ch + tot.cost.hospitalization.ch + tot.cost.outpatient.ch + tot.cost.rest.ch
  tot.cost.ch
  
  # Vaccination
  vax.cost = 28.62
  vaccination.cost = vax.cost * (vax.comp) * tot.pop * risk.group
  tot.cost.ch.vax = vaccination.cost
  tot.cost.ch.vax
  
  # Health outcome probability
  
  # Nonvaccinated
  subpop.ar.nonvax = case.after.vax * risk.group
  num.outpatient.nonvax = prob.outpatient * high.risk.ch.nonvax
  num.hospitalization.nonvax = prob.hospitalization * high.risk.ch.nonvax
  num.death.nonvax = prob.death * high.risk.ch.nonvax
  num.rest.nonvax = rest * high.risk.ch.nonvax
  num.case.ch.nonvax = num.outpatient.nonvax + num.hospitalization.nonvax + num.death.nonvax + num.rest.nonvax
  
  # Total cost for elderly 65+, high risk, non-vax
  cost.death.ch.nonvax = cost.death.ch * num.death.nonvax
  cost.hospitalization.ch.nonvax = cost.hospitalization.ch * num.hospitalization.nonvax
  cost.outpatient.ch.nonvax = cost.outpatient.ch * num.outpatient.nonvax
  cost.rest.ch.nonvax = cost.rest.ch * num.rest.nonvax
  tot.cost.ch.nonvax = cost.death.ch.nonvax + cost.hospitalization.ch.nonvax + cost.outpatient.ch.nonvax + cost.rest.ch.nonvax
  tot.cost.ch.nonvax
  
  res <- c(tot.cost.ch, tot.cost.ch.vax + tot.cost.ch.nonvax, tot.cost.ch - (tot.cost.ch.vax + tot.cost.ch.nonvax),
           (tot.cost.ch - (tot.cost.ch.vax + tot.cost.ch.nonvax))/(totalpopulation * risk.group),
           (tot.cost.ch - (tot.cost.ch.vax + tot.cost.ch.nonvax))/(vax.comp * tot.pop * risk.group),
           (tot.cost.ch - (tot.cost.ch.vax + tot.cost.ch.nonvax))/(vax.comp * tot.pop * risk.group*vax.cost),
           num.case.ch, num.case.ch.nonvax, num.death, num.death/(totalpopulation * risk.group))  
  return(res)
}
