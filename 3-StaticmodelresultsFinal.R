library("R0")

###Basic input

##################
complianceSA<-c(0.1,0.4,0.6,0.8)
compliance <- 0.4
##################
EfficacySA <- c(0.1,0.2,0.3,0.4,0.5,0.6)
efficacy <- 0.4

##################

vaccine.cost <- 28.62 
total.population = c(2652690,5439493,955391)


###Catastrophic Flu

 base.case.catastrophic <- c(1282633.92,1302458.28,142433.4)
 #vaccine.catastrophic <- c(741981.76,666084.8,70090.08)
#############################
 inf.Vaxday.Cat <- c(536,493,56)
#Day15 inf.Vaxday.Cat <- c(536,493,56)
#Day30 inf.Vaxday.Cat <- c(3186.24,2595,277.28)
#Day60 inf.Vaxday.Cat <- c(72589.36,57965.88,6019.88)
#Day90 inf.Vaxday.Cat <- c(725365.88,654251.6,69453.2)
 
 
 #############################
###Strong Flu

 base.case.strong <- c(975188.68,914708.88,97283.84)
 #vaccine.strong <- c(178906.96,141039.36,14476.28)
 ############################
 inf.Vaxday.Str <- c(393,366,43)
 #Day15 inf.Vaxday.Str <- c(393,366,43)
 #Day30 inf.Vaxday.Str <- c(1524.12,,1263.48,135)
 #Day60 inf.Vaxday.Str <- c(15021.92,11788.84,1205.84)
 #Day90 inf.Vaxday.Str <- c(121753.92,96208.8,9952.8)

 ############################

###Moderate Flu

 base.case.moderate <- c(547645.2,465329.8,48200.28)
 #vaccine.moderate <- c(7955.08,5910.64,611.4)
 ############################
 inf.Vaxday.Mod <- c(279,279,35)
 #Day15 inf.Vaxday.Mod <-c(279,279,35)
 #Day30 inf.Vaxday.Mod <- c(744.84,648.88,73.4)
 #Day60 inf.Vaxday.Mod <- c(3130.68,2407.6,249.16)
 #Day90 inf.vaxday.Mod <- c(10663.24,8034.72,822.56)
 
 ############################

##Attack rate

cat.AR <- 0.3014649
strong.AR <- 0.2196369
mod.AR <- 0.1172884
###Catastrophic Flu

Catastrophic <-data.frame(rbind(Children.low.risk(cat.AR,compliance,efficacy,total.population[1]-inf.Vaxday.Cat[1],total.population[1],base.case.catastrophic[1],inf.Vaxday.Cat[1]),
               Children.high.risk(cat.AR,compliance,efficacy,total.population[1]-inf.Vaxday.Cat[1],total.population[1],base.case.catastrophic[1],inf.Vaxday.Cat[1]),
               adult.low.risk(cat.AR,compliance,efficacy,total.population[2]-inf.Vaxday.Cat[2],total.population[2],base.case.catastrophic[2],inf.Vaxday.Cat[2]),
               adult.high.risk(cat.AR,compliance,efficacy,total.population[2]-inf.Vaxday.Cat[2],total.population[2],base.case.catastrophic[2],inf.Vaxday.Cat[2]),
               senior.low.risk(cat.AR,compliance,efficacy,total.population[3]-inf.Vaxday.Cat[3],total.population[3],base.case.catastrophic[3],inf.Vaxday.Cat[3]),
               senior.high.risk(cat.AR,compliance,efficacy,total.population[3]-inf.Vaxday.Cat[3],total.population[3],base.case.catastrophic[3],inf.Vaxday.Cat[3])))
catastrophic.total <- colSums(Catastrophic)
colnames(Catastrophic) <- c("Totalcost basecase","total cost after tax",
                      "Net return","Net return per capita",
                      "Net return per vaccinated","Net return per dollar spent",
                      "number of cases basecase","number of casses after vax","total death","risk of death")
names(catastrophic.total) <- c("Totalcost basecase","total cost after tax",
                            "Net return","Net return per capita",
                            "Net return per vaccinated","Net return per dollar spent",
                            "number of cases basecase","number of casses after vax","total death","risk of death")

###Strong Flu

Strong <-data.frame(rbind(Children.low.risk(strong.AR,compliance,efficacy,total.population[1]-inf.Vaxday.Str[1],total.population[1],base.case.strong[1],inf.Vaxday.Str[1]),
                          Children.high.risk(strong.AR,compliance,efficacy,total.population[1]-inf.Vaxday.Str[1],total.population[1],base.case.strong[1],inf.Vaxday.Str[1]),
                          adult.low.risk(strong.AR,compliance,efficacy,total.population[2]-inf.Vaxday.Str[2],total.population[2],base.case.strong[2],inf.Vaxday.Str[2]),
                          adult.high.risk(strong.AR,compliance,efficacy,total.population[2]-inf.Vaxday.Str[2],total.population[2],base.case.strong[2],inf.Vaxday.Str[2]),
                          senior.low.risk(strong.AR,compliance,efficacy,total.population[3]-inf.Vaxday.Str[3],total.population[3],base.case.strong[3],inf.Vaxday.Str[3]),
                          senior.high.risk(strong.AR,compliance,efficacy,total.population[3]-inf.Vaxday.Str[3],total.population[3],base.case.strong[3],inf.Vaxday.Str[3])))
strong.total <- colSums(Strong)
colnames(Strong) <- c("Totalcost basecase","total cost after tax",
                              "Net return","Net return per capita",
                              "Net return per vaccinated","Net return per dollar spent",
                              "number of cases basecase","number of casses after vax","total death","risk of death")
names(strong.total) <- c("Totalcost basecase","total cost after tax",
                                 "Net return","Net return per capita",
                                 "Net return per vaccinated","Net return per dollar spent",
                                 "number of cases basecase","number of casses after vax","total death","risk of death")

###Moderate Flu

Moderate <-data.frame(rbind(Children.low.risk(mod.AR,compliance,efficacy,total.population[1]-inf.Vaxday.Mod[1],total.population[1],base.case.moderate[1],inf.Vaxday.Mod[1]),
               Children.high.risk(mod.AR,compliance,efficacy,total.population[1]-inf.Vaxday.Mod[1],total.population[1],base.case.moderate[1],inf.Vaxday.Mod[1]),
               adult.low.risk(mod.AR,compliance,efficacy,total.population[2]-inf.Vaxday.Mod[2],total.population[2],base.case.moderate[2],inf.Vaxday.Mod[2]),
               adult.high.risk(mod.AR,compliance,efficacy,total.population[2]-inf.Vaxday.Mod[2],total.population[2],base.case.moderate[2],inf.Vaxday.Mod[2]),
               senior.low.risk(mod.AR,compliance,efficacy,total.population[3]-inf.Vaxday.Mod[3],total.population[3],base.case.moderate[3],inf.Vaxday.Mod[3]),
               senior.high.risk(mod.AR,compliance,efficacy,total.population[3]-inf.Vaxday.Mod[3],total.population[3],base.case.moderate[3],inf.Vaxday.Mod[3])))
moderate.total <- colSums(Moderate)
colnames(Moderate) <- c("Totalcost basecase","total cost after tax",
                      "Net return","Net return per capita",
                      "Net return per vaccinated","Net return per dollar spent",
                      "number of cases basecase","number of casses after vax","total death","risk of death")
names(moderate.total) <- c("Totalcost basecase","total cost after tax",
                         "Net return","Net return per capita",
                         "Net return per vaccinated","Net return per dollar spent",
                         "number of cases basecase","number of casses after vax","total death","risk of death")




##Table 1

column1 <- c(catastrophic.total[1]/sum(total.population),est.R0.AR(pop.size=sum(total.population), AR=catastrophic.total[7]/sum(total.population))[7],
             est.R0.AR(pop.size=sum(total.population), AR=catastrophic.total[7]/sum(total.population))[2],est.R0.AR(pop.size=sum(total.population), AR=catastrophic.total[7]/sum(total.population))[4])
column2 <- c(strong.total[1]/sum(total.population),est.R0.AR(pop.size=sum(total.population), AR=strong.total[7]/sum(total.population))[7],
                        est.R0.AR(pop.size=sum(total.population), AR=strong.total[7]/sum(total.population))[2],est.R0.AR(pop.size=sum(total.population), AR=strong.total[7]/sum(total.population))[4])
column3 <- c(moderate.total[1]/sum(total.population),est.R0.AR(pop.size=sum(total.population), AR=moderate.total[7]/sum(total.population))[7],
                          est.R0.AR(pop.size=sum(total.population), AR=moderate.total[7]/sum(total.population))[2],est.R0.AR(pop.size=sum(total.population), AR=moderate.total[7]/sum(total.population))[4])
table1<-cbind(column1,column2,column3)             
rownames(table1) <- c("Pandemic cost per capita","Attack rate","Reproductive number","95% confidence interval")
colnames(table1)<- c("Catastrophic flu","Strong flu","Moderate flu")




##Table 2- Static model

column.catastrophic<- c(catastrophic.total[2]/sum(total.population),(catastrophic.total[1]-catastrophic.total[2])/sum(total.population), 
                        (catastrophic.total[1]-catastrophic.total[2])/((sum(total.population)-sum(inf.Vaxday.Mod))*compliance),catastrophic.total[8]/sum(total.population),
                        est.R0.AR(pop.size=sum(total.population), AR=catastrophic.total[8]/sum(total.population))[2],
                        est.R0.AR(pop.size=sum(total.population), AR=catastrophic.total[8]/sum(total.population))[4],
                        (catastrophic.total[1]-catastrophic.total[2])/(sum(total.population)*compliance*vaccine.cost))

column.strong <- c(strong.total[2]/sum(total.population),(strong.total[1]-strong.total[2])/sum(total.population), 
                        (strong.total[1]-strong.total[2])/((sum(total.population)-sum(inf.Vaxday.Str))*compliance),strong.total[8]/sum(total.population),
                        est.R0.AR(pop.size=sum(total.population), AR=strong.total[8]/sum(total.population))[2],
                        est.R0.AR(pop.size=sum(total.population), AR=strong.total[8]/sum(total.population))[4],
                        (strong.total[1]-strong.total[2])/(sum(total.population)*compliance*vaccine.cost))

column.moderate <- c(moderate.total[2]/sum(total.population),(moderate.total[1]-moderate.total[2])/sum(total.population), 
                        (moderate.total[1]-moderate.total[2])/((sum(total.population)-sum(inf.Vaxday.Mod))*compliance),moderate.total[8]/sum(total.population),
                        est.R0.AR(pop.size=sum(total.population), AR=moderate.total[8]/sum(total.population))[2],
                        est.R0.AR(pop.size=sum(total.population), AR=moderate.total[8]/sum(total.population))[4],
                        (moderate.total[1]-moderate.total[2])/(sum(total.population)*compliance*vaccine.cost))
table2.static <- cbind(column.catastrophic,column.strong,column.moderate)
rownames(table2.static) <- c("Pandemic cost per capita","Return per capita","return per vaccinate person","Attack rate","Reproductive number","95% confidence interval","Retrun on investment")
colnames(table2.static)<- c("Catastrophic flu","Strong flu","Moderate flu")


#Table 5- Static model

table5.catastrophic <- c(catastrophic.total[1]/1e6,(catastrophic.total[2]-(sum(total.population)-sum(inf.Vaxday.Cat))*compliance*vaccine.cost)/1e6,
                         compliance*vaccine.cost*(sum(total.population)-sum(inf.Vaxday.Cat))/1e6,(catastrophic.total[1]-catastrophic.total[2])/1e6,
                         (catastrophic.total[1]-catastrophic.total[2])/sum(total.population),
                         (catastrophic.total[1]-catastrophic.total[2])/((sum(total.population)-sum(inf.Vaxday.Cat))*compliance),
                         (catastrophic.total[1]-catastrophic.total[2])/((sum(total.population)-sum(inf.Vaxday.Cat))*compliance*vaccine.cost))

table5.strong <- c(strong.total[1]/1e6,(strong.total[2]-(sum(total.population)-sum(inf.Vaxday.Str))*compliance*vaccine.cost)/1e6,
                         (compliance*vaccine.cost*(sum(total.population)-sum(inf.Vaxday.Str)))/1e6,(strong.total[1]-strong.total[2])/1e6,
                         (strong.total[1]-strong.total[2])/sum(total.population),
                         (strong.total[1]-strong.total[2])/((sum(total.population)-sum(inf.Vaxday.Str))*compliance),
                         (strong.total[1]-strong.total[2])/((sum(total.population)-sum(inf.Vaxday.Str))*compliance*vaccine.cost))

table5.moderate <- c(moderate.total[1]/1e6,(moderate.total[2]-(sum(total.population)-sum(inf.Vaxday.Mod))*compliance*vaccine.cost)/1e6,
                         (compliance*vaccine.cost*(sum(total.population)-sum(inf.Vaxday.Str)))/1e6,(moderate.total[1]-moderate.total[2])/1e6,
                         (moderate.total[1]-moderate.total[2])/sum(total.population),
                         (moderate.total[1]-moderate.total[2])/((sum(total.population)-sum(inf.Vaxday.Mod))*compliance),
                         (moderate.total[1]-moderate.total[2])/((sum(total.population)-sum(inf.Vaxday.Mod))*compliance*vaccine.cost))

table5.static <- rbind(table5.catastrophic,table5.strong,table5.moderate)
colnames(table5.static) <- c("Pandemic cost before vax","Pandemic cost after  vax","vax cost","net return","Return per capita","return per vaccinate person","return per dollar spent")
rownames(table5.static)<- c("Catastrophic flu","Strong flu","Moderate flu")

