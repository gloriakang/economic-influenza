

###Basic input

####################
compliance <- 0.4
####################
efficacy <- 0.4
####################
vaccine.cost <- 28.62 
total.population = c(2652690, 5439493, 955391)

###Catastrophic Flu

base.case.catastrophic <- c(1282633.92, 1302458.28, 142433.4)
#######################################################
vaccine.catastrophic <- c(741981.76,666084.8,70090.08)
#Day15 vaccine.catastrophic <- c(741981.76,666084.8,70090.08)
#Day30 vaccine.catastrophic <- c(881830.64,816118.72,86750.4)
#Day60 vaccine.catastrophic <- c(1102402.24,1070896.4,115433.84)
#Day90 vaccine.catastrophic <- c(1243833.04*0.93,1249987.24*0.93,136211.84*0.93)
#######################################################
inf.Vaxday.Cat <- c(536,493,56)
#Day15 inf.Vaxday.Cat <- c(536,493,56)
#Day30 inf.Vaxday.Cat <- c(3186.24,2595,277.28)
#Day60 inf.Vaxday.Cat <- c(72589.36,57965.88,6019.88)
#Day90 inf.Vaxday.Cat <- c(725365.88,654251.6,69453.2)

#######################################################

###Strong Flu

base.case.strong <- c(975188.68,914708.88,97283.84)
##################################################
vaccine.strong <- c(178906.96,141039.36,14476.28)
#Day15 vaccine.strong <- c(178906.96,141039.36,14476.28)
#Day30 vaccine.strong <- c(308168.76,250474,25805.8)
#Day60 vaccine.strong <- c(558585.76,480327.36,50070.64)
#Day90 vaccine.strong <- c(775510.96,696683.12,73376.32)
##################################################
inf.Vaxday.Str <- c(393,366,43)
#Day15 inf.Vaxday.Str <- c(393,366,43)
#Day30 inf.Vaxday.Str <- c(1524.12,,1263.48,135)
#Day60 inf.Vaxday.Str <- c(15021.92,11788.84,1205.84)
#Day90 inf.Vaxday.Str <- c(121753.92,96208.8,9952.8)
##################################################

###Moderate Flu

base.case.moderate <- c(547645.2,465329.8,48200.28)
###################################################
vaccine.moderate <- c(7955.08,5910.64,611.4)
#Day15 vaccine.moderate(7955.08,5910.64,611.4)
#Day30 vaccine.moderate(13849.8,10340.12,1072)
#Day60 vaccine.moderate(42573.24,31821.36,3246.32)
#Day90 vaccine.moderate(104013.92,80254.36,8202.36)
###################################################
inf.Vaxday.Mod <- c(279,279,35)
#Day15 inf.Vaxday.Mod <-c(279,279,35)
#Day30 inf.Vaxday.Mod <- c(744.84,648.88,73.4)
#Day60 inf.Vaxday.Mod <- c(3130.68,2407.6,249.16)
#Day90 inf.vaxday.Mod <- c(10663.24,8034.72,822.56)
###################################################

##Attack rate

cat.AR <- 0.3014649
strong.AR <- 0.2196369
mod.AR <- 0.1172884

###Catastrophic Flu

Catastrophic.dynamic <-data.frame(rbind(Children.low.risk(cat.AR,compliance,total.population[1]-inf.Vaxday.Cat[1],total.population[1],base.case.catastrophic[1],vaccine.catastrophic[1]),
                                Children.high.risk(cat.AR,compliance,total.population[1]-inf.Vaxday.Cat[1],total.population[1],base.case.catastrophic[1],vaccine.catastrophic[1]),
                                adult.low.risk(cat.AR,compliance,total.population[2]-inf.Vaxday.Cat[2],total.population[2],base.case.catastrophic[2],vaccine.catastrophic[2]),
                                adult.high.risk(cat.AR,compliance,total.population[2]-inf.Vaxday.Cat[2],total.population[2],base.case.catastrophic[2],vaccine.catastrophic[2]),
                                senior.low.risk(cat.AR,compliance,total.population[3]-inf.Vaxday.Cat[3],total.population[3],base.case.catastrophic[3],vaccine.catastrophic[3]),
                                senior.high.risk(cat.AR,compliance,total.population[3]-inf.Vaxday.Cat[3],total.population[3],base.case.catastrophic[3],vaccine.catastrophic[3])))
catastrophic.total.dynamic <- colSums(Catastrophic.dynamic)
colnames(Catastrophic.dynamic) <- c("Totalcost basecase","total cost after tax",
                            "Net return","Net return per capita",
                            "Net return per vaccinated","Net return per dollar spent",
                            "number of cases basecase","number of casses after vax","total death","risk of death")
rownames(Catastrophic.dynamic) <- c("children low risk","Children low risk","Adult low risk","Adult high risk","Senior low risk","Senior high risk")
names(catastrophic.total.dynamic) <- c("Totalcost basecase","total cost after tax",
                               "Net return","Net return per capita",
                               "Net return per vaccinated","Net return per dollar spent",
                               "number of cases basecase","number of casses after vax","total death","risk of death")

###Strong Flu

Strong.dynamic <-data.frame(rbind(Children.low.risk(strong.AR,compliance,total.population[1]-inf.Vaxday.Str[1],total.population[1],base.case.strong[1],vaccine.strong[1]),
                          Children.high.risk(strong.AR,compliance,total.population[1]-inf.Vaxday.Str[1],total.population[1],base.case.strong[1],vaccine.strong[1]),
                          adult.low.risk(strong.AR,compliance,total.population[2]-inf.Vaxday.Str[2],total.population[2],base.case.strong[2],vaccine.strong[2]),
                          adult.high.risk(strong.AR,compliance,total.population[2]-inf.Vaxday.Str[2],total.population[2],base.case.strong[2],vaccine.strong[2]),
                          senior.low.risk(strong.AR,compliance,total.population[3]-inf.Vaxday.Str[3],total.population[3],base.case.strong[3],vaccine.strong[3]),
                          senior.high.risk(strong.AR,compliance,total.population[3]-inf.Vaxday.Str[3],total.population[3],base.case.strong[3],vaccine.strong[3])))
strong.total.dynamic <- colSums(Strong.dynamic)
colnames(Strong.dynamic) <- c("Totalcost basecase","total cost after tax",
                                    "Net return","Net return per capita",
                                    "Net return per vaccinated","Net return per dollar spent",
                                    "number of cases basecase","number of casses after vax","total death","risk of death")
names(strong.total.dynamic) <- c("Totalcost basecase","total cost after tax",
                                       "Net return","Net return per capita",
                                       "Net return per vaccinated","Net return per dollar spent",
                                       "number of cases basecase","number of casses after vax","total death","risk of death")

###Moderate Flu

Moderate.dynamic <-data.frame(rbind(Children.low.risk(mod.AR,compliance,total.population[1]-inf.Vaxday.Mod[1],total.population[1],base.case.moderate[1],vaccine.moderate[1]),
                            Children.high.risk(mod.AR,compliance,total.population[1]-inf.Vaxday.Mod[1],total.population[1],base.case.moderate[1],vaccine.moderate[1]),
                            adult.low.risk(mod.AR,compliance,total.population[2]-inf.Vaxday.Mod[2],total.population[2],base.case.moderate[2],vaccine.moderate[2]),
                            adult.high.risk(mod.AR,compliance,total.population[2]-inf.Vaxday.Mod[2],total.population[2],base.case.moderate[2],vaccine.moderate[2]),
                            senior.low.risk(mod.AR,compliance,total.population[3]-inf.Vaxday.Mod[3],total.population[3],base.case.moderate[3],vaccine.moderate[3]),
                            senior.high.risk(mod.AR,compliance,total.population[3]-inf.Vaxday.Mod[3],total.population[3],base.case.moderate[3],vaccine.moderate[3])))
moderate.total.dynamic <- colSums(Moderate.dynamic)
colnames(Moderate.dynamic) <- c("Totalcost basecase","total cost after tax",
                              "Net return","Net return per capita",
                              "Net return per vaccinated","Net return per dollar spent",
                              "number of cases basecase","number of casses after vax","total death","risk of death")
names(moderate.total.dynamic) <- c("Totalcost basecase","total cost after tax",
                                 "Net return","Net return per capita",
                                 "Net return per vaccinated","Net return per dollar spent",
                                 "number of cases basecase","number of casses after vax","total death","risk of death")




##Table 2- dynamic model

column.catastrophic.dynamic <- c(
  catastrophic.total.dynamic[2]/sum(total.population),
  (catastrophic.total.dynamic[1] - catastrophic.total.dynamic[2])/sum(total.population),
  (catastrophic.total.dynamic[1] - catastrophic.total.dynamic[2])/((sum(total.population) - sum(inf.Vaxday.Cat)) * compliance),
  catastrophic.total.dynamic[8]/sum(total.population),
  est.R0.AR(pop.size = sum(total.population), AR = catastrophic.total.dynamic[8]/sum(total.population))[2],
  est.R0.AR(pop.size = sum(total.population), AR = catastrophic.total.dynamic[8]/sum(total.population))[4],
  (catastrophic.total.dynamic[1] - catastrophic.total.dynamic[2])/((sum(total.population) - sum(inf.Vaxday.Cat)) * compliance * vaccine.cost))


column.strong.dynamic <- c(
  strong.total.dynamic[2]/sum(total.population),
  (strong.total.dynamic[1]-strong.total.dynamic[2])/sum(total.population), 
  (strong.total.dynamic[1]-strong.total.dynamic[2])/((sum(total.population)-sum(inf.Vaxday.Str))*compliance),
  strong.total.dynamic[8]/sum(total.population),
  est.R0.AR(pop.size=sum(total.population), AR=strong.total.dynamic[8]/sum(total.population))[2],
  est.R0.AR(pop.size=sum(total.population), AR=strong.total.dynamic[8]/sum(total.population))[4],
  (strong.total.dynamic[1]-strong.total.dynamic[2])/((sum(total.population)-sum(inf.Vaxday.Str))*compliance*vaccine.cost))


column.moderate.dynamic <- c(moderate.total.dynamic[2]/sum(total.population),
                             (moderate.total.dynamic[1]-moderate.total.dynamic[2])/sum(total.population),
                             (moderate.total.dynamic[1]-moderate.total.dynamic[2])/((sum(total.population) - sum(inf.Vaxday.Mod))*compliance),
                             moderate.total.dynamic[8]/sum(total.population),
                             est.R0.AR(pop.size=sum(total.population), AR=moderate.total.dynamic[8]/sum(total.population))[2],
                             est.R0.AR(pop.size=sum(total.population), AR=moderate.total.dynamic[8]/sum(total.population))[4],
                             (moderate.total.dynamic[1]-moderate.total.dynamic[2])/((sum(total.population)-sum(inf.Vaxday.Cat))*compliance*vaccine.cost))


table2.dynamic <- cbind(column.catastrophic.dynamic, column.strong.dynamic, column.moderate.dynamic)
rownames(table2.dynamic) <- c("Pandemic cost per capita", "Return per capita", "return per vaccinate person", "Attack rate", "Reproductive number", "95% confidence interval", "Return on investment of vaccine")
colnames(table2.dynamic)<- c("Catastrophic flu", "Strong flu", "Moderate flu")


#Table 5- dynamic model

table5.catastrophic.dynamic <- c(catastrophic.total.dynamic[1]/1e6,
                                 (catastrophic.total.dynamic[2]-(sum(total.population)-sum(inf.Vaxday.Cat))*compliance*vaccine.cost)/1e6,
                                 compliance*vaccine.cost*(sum(total.population)-sum(inf.Vaxday.Cat))/1e6,
                                 (catastrophic.total.dynamic[1]-catastrophic.total.dynamic[2])/1e6,
                         (catastrophic.total.dynamic[1]-catastrophic.total.dynamic[2])/sum(total.population),
                         (catastrophic.total.dynamic[1]-catastrophic.total.dynamic[2])/((sum(total.population)-sum(inf.Vaxday.Cat))*compliance),
                         (catastrophic.total.dynamic[1]-catastrophic.total.dynamic[2])/((sum(total.population)-sum(inf.Vaxday.Cat))*compliance*vaccine.cost))


table5.strong.dynamic <- c(strong.total.dynamic[1]/1e6,
                           (strong.total.dynamic[2]-(sum(total.population)-sum(inf.Vaxday.Str))*compliance*vaccine.cost)/1e6,
                   (compliance*vaccine.cost*(sum(total.population)-sum(inf.Vaxday.Str)))/1e6,
                   (strong.total.dynamic[1]-strong.total.dynamic[2])/1e6,
                   (strong.total.dynamic[1]-strong.total.dynamic[2])/sum(total.population),
                   (strong.total.dynamic[1]-strong.total.dynamic[2])/((sum(total.population)-sum(inf.Vaxday.Str))*compliance),
                   (strong.total.dynamic[1]-strong.total.dynamic[2])/((sum(total.population)-sum(inf.Vaxday.Str))*compliance*vaccine.cost))


table5.moderate.dynamic <- c(moderate.total.dynamic[1]/1e6,(moderate.total.dynamic[2]-(sum(total.population)-sum(inf.Vaxday.Mod))*compliance*vaccine.cost)/1e6,
                     (compliance*vaccine.cost*(sum(total.population)-sum(inf.Vaxday.Mod)))/1e6,(moderate.total.dynamic[1]-moderate.total.dynamic[2])/1e6,
                     (moderate.total.dynamic[1]-moderate.total.dynamic[2])/sum(total.population),
                     (moderate.total.dynamic[1]-moderate.total.dynamic[2])/((sum(total.population)-sum(inf.Vaxday.Mod))*compliance),
                     (moderate.total.dynamic[1]-moderate.total.dynamic[2])/((sum(total.population)-sum(inf.Vaxday.Mod))*compliance*vaccine.cost))


table5.dynamic <- rbind(table5.catastrophic.dynamic,table5.strong.dynamic,table5.moderate.dynamic)
colnames(table5.dynamic) <- c("Pandemic cost before vax","Pandemic cost after  vax","vax cost","net return","Return per capita","return per vaccinate person","return per dollar spent")
rownames(table5.dynamic)<- c("Catastrophic flu","Strong flu","Moderate flu")
