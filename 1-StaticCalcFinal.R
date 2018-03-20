#install.packages("mc2d")
library(mc2d)

##### Children low risk
Children.low.risk <- function(AR, vax.comp, efficacy, tot.pop, totalPopulation, Children.case, infvaxday){
  risk.group = 0.936
  high.risk.ch.ar = Children.case * risk.group
  
  set.seed(1000)
  
  prob.outpatient = mean(runif(10000, min = 0.471, max = 0.548))
  prob.hospitalization = mean(runif(10000, min = 0.57/1000, max = 6.9/1000))
  prob.death = mean(rtriang(10000, min = 0.041/1000, mode = 0.07/1000, max = 0.3/1000))
  rest = 1 - (prob.death + prob.hospitalization + prob.outpatient)  
  
  #Number of patient with each health outcome
  num.outpatient = prob.outpatient * high.risk.ch.ar
  num.hospitalization = prob.hospitalization * high.risk.ch.ar
  num.death = prob.death * high.risk.ch.ar
  num.rest = rest * high.risk.ch.ar
  
  num.case.ch = num.outpatient + num.hospitalization + num.death + num.rest
  
  #Cost of each health outcome for 0-19
  
  cost.death.ch = 1479040 * (1+0.109)
  cost.hospitalization.ch = 15224 * (1+0.109)
  cost.outpatient.ch = 458 * (1+0.109)
  cost.rest.ch = 116.5 * (1+0.109)
  
  #Total cost for 0-19 low risk group
  
  tot.cost.death.ch = cost.death.ch * num.death
  tot.cost.hospitalization.ch = cost.hospitalization.ch * num.hospitalization
  tot.cost.outpatient.ch = cost.outpatient.ch * num.outpatient
  tot.cost.rest.ch = cost.rest.ch * num.rest
  
  tot.cost.ch = tot.cost.death.ch + tot.cost.hospitalization.ch + tot.cost.outpatient.ch + tot.cost.rest.ch
  tot.cost.ch
  
  
  #Vaccination
  
  high.risk.ch.vax = (vax.comp) * tot.pop * risk.group * efficacy
  vax.cost = 28.62
  vaccination.cost = vax.cost * (vax.comp) * tot.pop * risk.group
  
  
  tot.cost.ch.vax = vaccination.cost
  tot.cost.ch.vax
  
  
  #Health outcome probability
  
  ###
  #Nonvaccinated
  #high.risk.ch.nonvax=(tot.pop*risk.group-high.risk.ch.vax)*AR
  #high.risk.ch.nonvax=((AR*totalPopulation-infvaxday)*(1-efficacy*vax.comp)*AR+infvaxday)*risk.group
  high.risk.ch.nonvax = ((Children.case - infvaxday) * (1 - efficacy * vax.comp) + infvaxday) * risk.group
  

  num.outpatient.nonvax = prob.outpatient * high.risk.ch.nonvax
  num.hospitalization.nonvax = prob.hospitalization * high.risk.ch.nonvax
  num.death.nonvax = prob.death * high.risk.ch.nonvax
  num.rest.nonvax = rest * high.risk.ch.nonvax
  
  num.case.ch.nonvax = num.outpatient.nonvax + num.hospitalization.nonvax + num.death.nonvax + num.rest.nonvax
  
  
  
  #Total cost for 0-19 low risk group non vax
  
  cost.death.ch.nonvax = cost.death.ch * num.death.nonvax
  cost.hospitalization.ch.nonvax = cost.hospitalization.ch * num.hospitalization.nonvax
  cost.outpatient.ch.nonvax = cost.outpatient.ch * num.outpatient.nonvax
  cost.rest.ch.nonvax = cost.rest.ch * num.rest.nonvax
  
  tot.cost.ch.nonvax = cost.death.ch.nonvax + cost.hospitalization.ch.nonvax + cost.outpatient.ch.nonvax + cost.rest.ch.nonvax
  tot.cost.ch.nonvax
  
  res <- c(tot.cost.ch, tot.cost.ch.vax + tot.cost.ch.nonvax, tot.cost.ch - (tot.cost.ch.vax + tot.cost.ch.nonvax),
           (tot.cost.ch-(tot.cost.ch.vax + tot.cost.ch.nonvax))/(totalPopulation * risk.group),
           (tot.cost.ch-(tot.cost.ch.vax + tot.cost.ch.nonvax))/(vax.comp * tot.pop * risk.group),
           (tot.cost.ch-(tot.cost.ch.vax + tot.cost.ch.nonvax))/(vax.comp * tot.pop * risk.group * vax.cost),
           num.case.ch, num.case.ch.nonvax, num.death, num.death/(totalPopulation * risk.group))
  return(res)
  
  #return(paste("Totalcost.lowrisk.childlren.beforevax=",tot.cost.ch,"Numberof cases before vax",high.risk.ch.ar,
  #             "Total cost of vaxinated=",tot.cost.ch.vax,"NUmber of cases in vaxinated=",num.case.ch.vax,
  #             "NUmber of averted cases=",num.averted.case,"Total cost of unvaccinated=",tot.cost.ch.nonvax,"Number of cases in unvaccinated=",num.case.ch.nonvax))
}




##### Children high risk


Children.high.risk <- function(AR,vax.comp,efficacy,tot.pop,totalPopulation,Children.case,infvaxday){
  
  risk.group = 1-0.936
  high.risk.ch.ar=Children.case*risk.group
  
  set.seed(1000)
  
  prob.outpatient = mean(runif(10000,min=0.825,max=0.958))
  prob.hospitalization = mean(runif(10000, min=6/1000,max=21.4/1000))
  prob.death = mean(rtriang(10000, min=0.4/1000, mode=0.6/1000, max=21.9/1000))
  rest = 1-(prob.death + prob.hospitalization + prob.outpatient)  
  
  #Number of patient with each health outcome
  num.outpatient = prob.outpatient*high.risk.ch.ar
  num.hospitalization = prob.hospitalization*high.risk.ch.ar
  num.death = prob.death*high.risk.ch.ar
  num.rest = rest*high.risk.ch.ar
  
  num.case.ch=num.outpatient+num.hospitalization+num.death+num.rest
  
  #Cost of each health outcome for 0-19
  
  cost.death.ch =  1487872*(1+0.109) 
  cost.hospitalization.ch = 31894*(1+0.109)
  cost.outpatient.ch = 948*(1+0.109)
  cost.rest.ch = 116.5*(1+0.109)
  
  #Total cost for 0-19 low risk group
  
  tot.cost.death.ch = cost.death.ch*num.death
  tot.cost.hospitalization.ch = cost.hospitalization.ch*num.hospitalization
  tot.cost.outpatient.ch = cost.outpatient.ch*num.outpatient
  tot.cost.rest.ch = cost.rest.ch*num.rest
  
  tot.cost.ch=tot.cost.death.ch+tot.cost.hospitalization.ch+tot.cost.outpatient.ch+tot.cost.rest.ch
  tot.cost.ch
  
  
  
  #Vaccination
  high.risk.ch.vax=vax.comp*tot.pop*risk.group*efficacy
  vax.cost=28.62
  vaccination.cost=vax.cost*(vax.comp)*tot.pop*risk.group
  
  tot.cost.ch.vax=vaccination.cost
  tot.cost.ch.vax
  
  
  #Health outcome probability
  
  ###
  #Nonvaccinated
  
  #high.risk.ch.nonvax=(tot.pop*risk.group-high.risk.ch.vax)*AR
  high.risk.ch.nonvax=((Children.case-infvaxday)*(1-efficacy*vax.comp)+infvaxday)*risk.group
  
  
  num.outpatient.nonvax = prob.outpatient*high.risk.ch.nonvax
  num.hospitalization.nonvax = prob.hospitalization*high.risk.ch.nonvax
  num.death.nonvax = prob.death*high.risk.ch.nonvax
  num.rest.nonvax = rest*high.risk.ch.nonvax
  
  num.case.ch.nonvax=num.outpatient.nonvax+num.hospitalization.nonvax+num.death.nonvax+num.rest.nonvax
  
  
  
  #Total cost for 0-19 low risk group non vax
  
  cost.death.ch.nonvax = cost.death.ch*num.death.nonvax
  cost.hospitalization.ch.nonvax = cost.hospitalization.ch*num.hospitalization.nonvax
  cost.outpatient.ch.nonvax = cost.outpatient.ch*num.outpatient.nonvax
  cost.rest.ch.nonvax = cost.rest.ch*num.rest.nonvax
  
  tot.cost.ch.nonvax=cost.death.ch.nonvax+cost.hospitalization.ch.nonvax+cost.outpatient.ch.nonvax+cost.rest.ch.nonvax
  tot.cost.ch.nonvax
  
  res <- c(tot.cost.ch,tot.cost.ch.vax+tot.cost.ch.nonvax,tot.cost.ch-(tot.cost.ch.vax+tot.cost.ch.nonvax),
           (tot.cost.ch-(tot.cost.ch.vax+tot.cost.ch.nonvax))/(totalPopulation*risk.group),
           (tot.cost.ch-(tot.cost.ch.vax+tot.cost.ch.nonvax))/(vax.comp*tot.pop*risk.group),
           (tot.cost.ch-(tot.cost.ch.vax+tot.cost.ch.nonvax))/(vax.comp*tot.pop*risk.group*vax.cost),
           num.case.ch,num.case.ch.nonvax,num.death,num.death/(totalPopulation*risk.group))
  return(res)
}



##### Adult low risk


adult.low.risk <- function(AR,vax.comp,efficacy,tot.pop,totalPopulation,Children.case,infvaxday){

  risk.group = 1-0.144  
  high.risk.ch.ar=Children.case*risk.group
  
  set.seed(1000)
  
  prob.outpatient = mean(runif(10000,min=0.333,max=0.370))
  prob.hospitalization = mean(runif(10000, min=1.5/1000,max=12/1000))
  prob.death = mean(rtriang(10000, min=0.21/1000, mode=0.31/1000, max=0.41/1000))
  rest = 1-(prob.death + prob.hospitalization + prob.outpatient)  
  
  #Number of patient with each health outcome
  num.outpatient = prob.outpatient*high.risk.ch.ar
  num.hospitalization = prob.hospitalization*high.risk.ch.ar
  num.death = prob.death*high.risk.ch.ar
  num.rest = rest*high.risk.ch.ar
  
  num.case.ch=num.outpatient+num.hospitalization+num.death+num.rest
  
  #Cost of each health outcome for 0-19
  
  cost.death.ch =  848692*(1+0.109) 
  cost.hospitalization.ch = 31328*(1+0.109)
  cost.outpatient.ch = 815*(1+0.109)
  cost.rest.ch = 79*(1+0.109)
  
  
  #Total cost for 0-19 low risk group
  
  tot.cost.death.ch = cost.death.ch*num.death
  tot.cost.hospitalization.ch = cost.hospitalization.ch*num.hospitalization
  tot.cost.outpatient.ch = cost.outpatient.ch*num.outpatient
  tot.cost.rest.ch = cost.rest.ch*num.rest
  
  tot.cost.ch=tot.cost.death.ch+tot.cost.hospitalization.ch+tot.cost.outpatient.ch+tot.cost.rest.ch
  tot.cost.ch
  
  
  
  #Vaccination
  high.risk.ch.vax=vax.comp*tot.pop*risk.group*efficacy
  
  vax.cost=28.62
  vaccination.cost=vax.cost*(vax.comp)*tot.pop*risk.group
  
  tot.cost.ch.vax=vaccination.cost
  tot.cost.ch.vax
  
  ###
  #Nonvaccinated
  
  #Health outcome probability
  
  
  #high.risk.ch.nonvax=(tot.pop*risk.group-high.risk.ch.vax)*AR
  high.risk.ch.nonvax=((Children.case-infvaxday)*(1-efficacy*vax.comp)+infvaxday)*risk.group
  
  
  num.outpatient.nonvax = prob.outpatient*high.risk.ch.nonvax
  num.hospitalization.nonvax = prob.hospitalization*high.risk.ch.nonvax
  num.death.nonvax = prob.death*high.risk.ch.nonvax
  num.rest.nonvax = rest*high.risk.ch.nonvax
  
  num.case.ch.nonvax=num.outpatient.nonvax+num.hospitalization.nonvax+num.death.nonvax+num.rest.nonvax
  
  
  
  #Total cost for 0-19 low risk group non vax
  
  cost.death.ch.nonvax = cost.death.ch*num.death.nonvax
  cost.hospitalization.ch.nonvax = cost.hospitalization.ch*num.hospitalization.nonvax
  cost.outpatient.ch.nonvax = cost.outpatient.ch*num.outpatient.nonvax
  cost.rest.ch.nonvax = cost.rest.ch*num.rest.nonvax
  
  tot.cost.ch.nonvax=cost.death.ch.nonvax+cost.hospitalization.ch.nonvax+cost.outpatient.ch.nonvax+cost.rest.ch.nonvax
  tot.cost.ch.nonvax
  
  res <- c(tot.cost.ch,tot.cost.ch.vax+tot.cost.ch.nonvax,tot.cost.ch-(tot.cost.ch.vax+tot.cost.ch.nonvax),
           (tot.cost.ch-(tot.cost.ch.vax+tot.cost.ch.nonvax))/(totalPopulation*risk.group),
           (tot.cost.ch-(tot.cost.ch.vax+tot.cost.ch.nonvax))/(vax.comp*tot.pop*risk.group),
           (tot.cost.ch-(tot.cost.ch.vax+tot.cost.ch.nonvax))/(vax.comp*tot.pop*risk.group*vax.cost),
           num.case.ch,num.case.ch.nonvax,num.death,num.death/(totalPopulation*risk.group))
  return(res)
}



##### Adult high risk


adult.high.risk <- function(AR,vax.comp,efficacy,tot.pop,totalPopulation,Children.case,infvaxday){
  
  risk.group = 0.144
  high.risk.ch.ar=Children.case*risk.group
  
  set.seed(1000)
  
  prob.outpatient = mean(runif(10000,min=0.583,max=0.647))
  prob.hospitalization = mean(runif(10000, min=6.9/1000,max=22.3/1000))
  #prob.death = mean(rtriang(1000, min=0.21/1000, mode=0.31/1000, max=0.41/1000))
  prob.death = mean(runif(10000, min=0.8/1000, max=24.9/1000))
  rest = 1-(prob.death + prob.hospitalization + prob.outpatient)  
  
  #Number of patient with each health outcome
  num.outpatient = prob.outpatient*high.risk.ch.ar
  num.hospitalization = prob.hospitalization*high.risk.ch.ar
  num.death = prob.death*high.risk.ch.ar
  num.rest = rest*high.risk.ch.ar
  
  num.case.ch=num.outpatient+num.hospitalization+num.death+num.rest
  
  #Cost of each health outcome for 0-19
  
  cost.death.ch =  848692*(1+0.109) 
  cost.hospitalization.ch = 31328*(1+0.109)
  cost.outpatient.ch = 815*(1+0.109)
  cost.rest.ch = 79*(1+0.109)
  
  #Total cost for 0-19 low risk group
  
  tot.cost.death.ch = cost.death.ch*num.death
  tot.cost.hospitalization.ch = cost.hospitalization.ch*num.hospitalization
  tot.cost.outpatient.ch = cost.outpatient.ch*num.outpatient
  tot.cost.rest.ch = cost.rest.ch*num.rest
  
  tot.cost.ch=tot.cost.death.ch+tot.cost.hospitalization.ch+tot.cost.outpatient.ch+tot.cost.rest.ch
  tot.cost.ch
  
  
  
  #Vaccination
  high.risk.ch.vax=(vax.comp)*tot.pop*risk.group*efficacy
  vax.cost=28.62
  vaccination.cost=vax.cost*(vax.comp)*tot.pop*risk.group
  
  tot.cost.ch.vax=vaccination.cost
  tot.cost.ch.vax
  
  
  #Health outcome probability
  
  ###
  #Nonvaccinated
  
  #high.risk.ch.nonvax=(tot.pop*risk.group-high.risk.ch.vax)*AR
  high.risk.ch.nonvax=((Children.case-infvaxday)*(1-efficacy*vax.comp)+infvaxday)*risk.group
  
  
  num.outpatient.nonvax = prob.outpatient*high.risk.ch.nonvax
  num.hospitalization.nonvax = prob.hospitalization*high.risk.ch.nonvax
  num.death.nonvax = prob.death*high.risk.ch.nonvax
  num.rest.nonvax = rest*high.risk.ch.nonvax
  
  num.case.ch.nonvax=num.outpatient.nonvax+num.hospitalization.nonvax+num.death.nonvax+num.rest.nonvax
  
  
  
  #Total cost for 0-19 low risk group non vax
  
  cost.death.ch.nonvax = cost.death.ch*num.death.nonvax
  cost.hospitalization.ch.nonvax = cost.hospitalization.ch*num.hospitalization.nonvax
  cost.outpatient.ch.nonvax = cost.outpatient.ch*num.outpatient.nonvax
  cost.rest.ch.nonvax = cost.rest.ch*num.rest.nonvax
  
  tot.cost.ch.nonvax=cost.death.ch.nonvax+cost.hospitalization.ch.nonvax+cost.outpatient.ch.nonvax+cost.rest.ch.nonvax
  tot.cost.ch.nonvax
  
  res <- c(tot.cost.ch,tot.cost.ch.vax+tot.cost.ch.nonvax,tot.cost.ch-(tot.cost.ch.vax+tot.cost.ch.nonvax),
           (tot.cost.ch-(tot.cost.ch.vax+tot.cost.ch.nonvax))/(totalPopulation*risk.group),
           (tot.cost.ch-(tot.cost.ch.vax+tot.cost.ch.nonvax))/(vax.comp*tot.pop*risk.group),
           (tot.cost.ch-(tot.cost.ch.vax+tot.cost.ch.nonvax))/(vax.comp*tot.pop*risk.group*vax.cost),
           num.case.ch,num.case.ch.nonvax,num.death,num.death/(totalPopulation*risk.group))
  return(res)
}



##### Senior low risk



senior.low.risk <- function(AR,vax.comp,efficacy,tot.pop,totalPopulation,Children.case,infvaxday){
  risk.group = 1-0.4
  high.risk.ch.ar = Children.case * risk.group
  
  set.seed(1000)
  
  prob.outpatient = mean(runif(10000, min = 0.375, max = 0.389))
  prob.hospitalization = mean(runif(10000, min = 12.5/1000, max = 15.8/1000))
  prob.death = mean(rtriang(10000, min = 2.3/1000, mode = 3.51/1000, max = 4.52/1000))
  #prob.death = mean(runif(1000, min=0.8/1000, max=24.9/1000))
  rest = 1-(prob.death + prob.hospitalization + prob.outpatient)  
  
  #Number of patient with each health outcome
  num.outpatient = prob.outpatient*high.risk.ch.ar
  num.hospitalization = prob.hospitalization*high.risk.ch.ar
  num.death = prob.death*high.risk.ch.ar
  num.rest = rest*high.risk.ch.ar
  
  num.case.ch=num.outpatient+num.hospitalization+num.death+num.rest
  
  #Cost of each health outcome for 0-19
  
  cost.death.ch =  249749*(1+0.109) 
  cost.hospitalization.ch = 13508*(1+0.109)
  cost.outpatient.ch = 1156*(1+0.109)
  cost.rest.ch = 121*(1+0.109)
  
  #Total cost for 0-19 low risk group
  
  tot.cost.death.ch = cost.death.ch*num.death
  tot.cost.hospitalization.ch = cost.hospitalization.ch*num.hospitalization
  tot.cost.outpatient.ch = cost.outpatient.ch*num.outpatient
  tot.cost.rest.ch = cost.rest.ch*num.rest
  
  tot.cost.ch=tot.cost.death.ch+tot.cost.hospitalization.ch+tot.cost.outpatient.ch+tot.cost.rest.ch
  tot.cost.ch
  
  
  
  #Vaccination
  high.risk.ch.vax=vax.comp*tot.pop*risk.group*efficacy
  vax.cost=28.62
  vaccination.cost=vax.cost*(vax.comp)*tot.pop*risk.group
  
  tot.cost.ch.vax=vaccination.cost
  tot.cost.ch.vax
  
  
  #Health outcome probability
  
  ###
  #Nonvaccinated
  
  #high.risk.ch.nonvax=(tot.pop*risk.group-high.risk.ch.vax)*AR
  high.risk.ch.nonvax=((Children.case-infvaxday)*(1-efficacy*vax.comp)+infvaxday)*risk.group
  
  
  num.outpatient.nonvax = prob.outpatient*high.risk.ch.nonvax
  num.hospitalization.nonvax = prob.hospitalization*high.risk.ch.nonvax
  num.death.nonvax = prob.death*high.risk.ch.nonvax
  num.rest.nonvax = rest*high.risk.ch.nonvax
  
  num.case.ch.nonvax=num.outpatient.nonvax+num.hospitalization.nonvax+num.death.nonvax+num.rest.nonvax
  
  
  
  #Total cost for 0-19 low risk group non vax
  
  cost.death.ch.nonvax = cost.death.ch*num.death.nonvax
  cost.hospitalization.ch.nonvax = cost.hospitalization.ch*num.hospitalization.nonvax
  cost.outpatient.ch.nonvax = cost.outpatient.ch*num.outpatient.nonvax
  cost.rest.ch.nonvax = cost.rest.ch*num.rest.nonvax
  
  tot.cost.ch.nonvax=cost.death.ch.nonvax+cost.hospitalization.ch.nonvax+cost.outpatient.ch.nonvax+cost.rest.ch.nonvax
  tot.cost.ch.nonvax
  
  res <- c(tot.cost.ch,tot.cost.ch.vax+tot.cost.ch.nonvax,tot.cost.ch-(tot.cost.ch.vax+tot.cost.ch.nonvax),
           (tot.cost.ch-(tot.cost.ch.vax+tot.cost.ch.nonvax))/(totalPopulation*risk.group),
           (tot.cost.ch-(tot.cost.ch.vax+tot.cost.ch.nonvax))/(vax.comp*tot.pop*risk.group),
           (tot.cost.ch-(tot.cost.ch.vax+tot.cost.ch.nonvax))/(vax.comp*tot.pop*risk.group*vax.cost),
           num.case.ch,num.case.ch.nonvax,num.death,num.death/(totalPopulation*risk.group))
  return(res)
}




##### Senior high risk




senior.high.risk <- function(AR,vax.comp,efficacy,tot.pop,totalPopulation,Children.case,infvaxday){
  risk.group = 0.4
  high.risk.ch.ar=Children.case*risk.group
  
  set.seed(1000)
  
  prob.outpatient = mean(runif(10000,min=0.656,max=0.682))
  prob.hospitalization = mean(runif(10000, min=33.3/1000,max=68.4/1000))
  #prob.death = mean(rtriang(1000, min=2.3/1000, mode=3.51/1000, max=4.52/1000))
  prob.death = mean(runif(10000, min=23/1000, max=29.6/1000))
  rest = 1-(prob.death + prob.hospitalization + prob.outpatient)  
  
  #Number of patient with each health outcome
  num.outpatient = prob.outpatient*high.risk.ch.ar
  num.hospitalization = prob.hospitalization*high.risk.ch.ar
  num.death = prob.death*high.risk.ch.ar
  num.rest = rest*high.risk.ch.ar
  
  num.case.ch=num.outpatient+num.hospitalization+num.death+num.rest
  
  #Cost of each health outcome for 0-19
  
  cost.death.ch =  261544*(1+0.109) 
  cost.hospitalization.ch = 20269*(1+0.109)
  cost.outpatient.ch = 2826*(1+0.109)
  cost.rest.ch = 121*(1+0.109)
  
  #Total cost for 0-19 low risk group
  
  tot.cost.death.ch = cost.death.ch*num.death
  tot.cost.hospitalization.ch = cost.hospitalization.ch*num.hospitalization
  tot.cost.outpatient.ch = cost.outpatient.ch*num.outpatient
  tot.cost.rest.ch = cost.rest.ch*num.rest
  
  tot.cost.ch=tot.cost.death.ch+tot.cost.hospitalization.ch+tot.cost.outpatient.ch+tot.cost.rest.ch
  tot.cost.ch
  
  
  
  #Vaccination
  high.risk.ch.vax=vax.comp*tot.pop*risk.group*efficacy
  vax.cost=28.62
  vaccination.cost=vax.cost*(vax.comp)*tot.pop*risk.group
  
  tot.cost.ch.vax=vaccination.cost
  tot.cost.ch.vax
  
  
  #Health outcome probability
  
  ###
  #Nonvaccinated
  
  #high.risk.ch.nonvax=(tot.pop*risk.group-high.risk.ch.vax)*AR
  high.risk.ch.nonvax=((Children.case-infvaxday)*(1-efficacy*vax.comp)+infvaxday)*risk.group
  
  
  num.outpatient.nonvax = prob.outpatient*high.risk.ch.nonvax
  num.hospitalization.nonvax = prob.hospitalization*high.risk.ch.nonvax
  num.death.nonvax = prob.death*high.risk.ch.nonvax
  num.rest.nonvax = rest*high.risk.ch.nonvax
  
  num.case.ch.nonvax=num.outpatient.nonvax+num.hospitalization.nonvax+num.death.nonvax+num.rest.nonvax
  
  
  
  #Total cost for 0-19 low risk group non vax
  
  cost.death.ch.nonvax = cost.death.ch*num.death.nonvax
  cost.hospitalization.ch.nonvax = cost.hospitalization.ch*num.hospitalization.nonvax
  cost.outpatient.ch.nonvax = cost.outpatient.ch*num.outpatient.nonvax
  cost.rest.ch.nonvax = cost.rest.ch*num.rest.nonvax
  
  tot.cost.ch.nonvax=cost.death.ch.nonvax+cost.hospitalization.ch.nonvax+cost.outpatient.ch.nonvax+cost.rest.ch.nonvax
  tot.cost.ch.nonvax
  
  res <- c(tot.cost.ch,tot.cost.ch.vax+tot.cost.ch.nonvax,tot.cost.ch-(tot.cost.ch.vax+tot.cost.ch.nonvax),
           (tot.cost.ch-(tot.cost.ch.vax+tot.cost.ch.nonvax))/(totalPopulation*risk.group),
           (tot.cost.ch-(tot.cost.ch.vax+tot.cost.ch.nonvax))/(vax.comp*tot.pop*risk.group),
           (tot.cost.ch-(tot.cost.ch.vax+tot.cost.ch.nonvax))/(vax.comp*tot.pop*risk.group*vax.cost),
           num.case.ch,num.case.ch.nonvax,num.death,num.death/(totalPopulation*risk.group))  
  return(res)
}

