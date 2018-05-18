# Figures

## Alternatively:
# icer per case averted
ggplot(df3[(df3$risk=="All"),], aes(x = age, y = icer.case.averted, group = v.eff, color = v.eff)) +
  geom_point() + geom_line(linetype = "dotted") + labs(color = "Vaccine \nefficacy") +
  ylab("$ saved per case averted") + facet_grid(~ scenario) + ggtitle("ICER per case averted")

ggplot(df3, aes(x = age, y = icer.case.averted, group = risk, color = risk)) +
  geom_point() + geom_line(linetype = "dotted") + labs(color = "risk") +
  ylab("$ saved per case averted") + facet_grid(scenario ~ v.eff) + ggtitle("ICER per case averted")

# icer per death averted
ggplot(df3[(df3$risk=="All"),], aes(x = age, y = icer.death.averted, group = v.eff, color = v.eff)) +
  geom_point() + geom_line(linetype = "dotted") + labs(color = "Vaccine \nefficacy") +
  ylab("$ saved per death averted") + facet_grid(~ scenario) + ggtitle("ICER per death averted")

ggplot(df3, aes(x = age, y = icer.death.averted, group = risk, color = risk)) +
  geom_point() + geom_line(linetype = "dotted") + labs(color = "risk") +
  ylab("$ saved per death averted") + facet_grid(scenario ~ v.eff) + ggtitle("ICER per death averted")

# icer per DALY averted
ggplot(df3[(df3$risk=="All"),], aes(x = age, y = icer.daly.averted, group = v.eff, color = v.eff)) +
  geom_point() + geom_line(linetype = "dotted") + labs(color = "Vaccine \nefficacy") +
  ylab("$ saved per DALY averted") + facet_grid(~ scenario) + ggtitle("ICER per DALY averted")

ggplot(df3, aes(x = age, y = icer.daly.averted, group = risk, color = risk)) +
  geom_point() + geom_line(linetype = "dotted") + labs(color = "risk") +
  ylab("$ saved per DALY averted") + facet_grid(scenario ~ v.eff) + ggtitle("ICER per DALY averted")


####################
## Fig. Cases, cases averted
# age groups
ggplot(age_df_base, aes(x = age, y = cases, color = v.eff, group = v.eff)) +
  geom_point() + geom_line(linetype = "dotted") + labs(color = "Vaccine \nefficacy") + 
  ylab("Cases") + ggtitle("Base vaccination: Cases")
ggplot(age_df_base, aes(x = age, y = cases.per100k, group = v.eff, color = v.eff)) +
  geom_point() + geom_line(linetype = "dotted") + labs(color = "Vaccine \nefficacy") + 
  ylab("Cases per 100k") + ggtitle("Base vaccination: Cases per 100k")

ggplot(age_df_base, aes(x = age, y = cases.averted, color = v.eff, group = v.eff)) +
  geom_point() + geom_line(linetype = "dotted") + labs(color = "Vaccine \nefficacy") + 
  ylab("Averted cases") + ggtitle("Base vaccination: Averted cases")
ggplot(age_df_base, aes(x = age, y = cases.averted.per100k, group = v.eff, color = v.eff)) +
  geom_point() + geom_line(linetype = "dotted") + labs(color = "Vaccine \nefficacy") + 
  ylab("Averted cases per 100k") + ggtitle("Base vaccination: Averted cases per 100k")

# risk groups
ggplot(df_base, aes(x = age, y = cases, group = risk, color = risk)) +
  geom_point() + geom_line(linetype = "dotted") + labs(color = "Risk group") + 
  ylab("Cases") + ggtitle("Base vaccination: Cases") + facet_grid(~ v.eff)
ggplot(df_base, aes(x = age, y = cases.per100k, group = risk, color = risk)) +
  geom_point() + geom_line(linetype = "dotted") + labs(color = "Risk group") + 
  ylab("Cases per 100k") + ggtitle("Base vaccination: Cases per 100k") + facet_grid(~ v.eff)

ggplot(df_base, aes(x = age, y = cases.averted, group = risk, color = risk)) +
  geom_point() + geom_line(linetype = "dotted") + labs(color = "Risk group") + 
  ylab("Averted cases") + ggtitle("Base vaccination: Averted cases") + facet_grid(~ v.eff)
ggplot(df_base, aes(x = age, y = cases.averted.per100k, group = risk, color = risk)) +
  geom_point() + geom_line(linetype = "dotted") + labs(color = "Risk group") + 
  ylab("Averted cases per 100k") + ggtitle("Base vaccination: Averted cases per 100k") + facet_grid(~ v.eff)


####################
# Alternatively
## BASEVAX Figures: Cases
ggplot(df_base, aes(x = age, y = cases, color = risk, group = risk)) + labs(color = "Risk \ngroup") +
  geom_point() + geom_line(linetype = "dotted") + ylab("cases") +
  facet_grid(~v.eff) + ggtitle("Number of cases in each age and risk group by vaccine efficacy")


####################
### Cases per 100k
ggplot(age_group_df, aes(x = age, y = cases.per100k, group = v.eff, color = v.eff)) +
  geom_point() + geom_line(linetype = "dotted") + labs(color = "Vaccine \nefficacy") +
  ylab("Cases per 100k") + ggtitle("Number of cases per 100k") + facet_grid(~ scenario)

ggplot(df3, aes(x = age, y = cases.per100k, color = risk, group = risk)) + labs(color = "Risk \ngroup") +
  geom_point() + geom_line(linetype = "dotted") + ylab("cases") +
  facet_grid(v.eff~scenario) + ggtitle("Number of cases per 100k")

ggplot(age_group_df, aes(x = age, y = cases.per100k, color = v.eff, group = v.eff)) + labs(color = "Vaccine \nefficacy") +
  geom_point() + geom_line(linetype = "dotted") + ylab("cases") +
  facet_grid(scenario~v.eff) + ggtitle("Number of cases per 100k")

ggplot(df3, aes(x = age, y = cases, color = risk, group = risk)) + labs(color = "Vaccine \nefficacy") +
  geom_point() + geom_line(linetype = "dotted") + ylab("cases") +
  facet_grid(scenario~v.eff) + ggtitle("Number of cases per 100k")


### Deaths per 100k
# deaths
ggplot(age_group_df, aes(x = age, y = deaths.per100k, color = v.eff, group = v.eff)) + geom_point() + geom_line(linetype = "dotted") + ggtitle("Deaths per 100k in each age group by vaccine efficacy") + facet_grid(~scenario)

ggplot(age_group_df, aes(x = age, y = deaths.per100k, color = v.eff, group = v.eff)) + labs(color = "Vaccine \nefficacy") +
  geom_point() + geom_line(linetype = "dotted") + ylab("deaths per 100k") +
  facet_grid(v.eff~scenario) + ggtitle("Deaths per 100k in each age group by vaccine efficacy")

ggplot(df3, aes(x = age, y = deaths.per100k, color = risk, group = risk)) + labs(color = "Risk \ngroup") +
  geom_point() + geom_line(linetype = "dotted") + ylab("deaths per 100k") +
  facet_grid(v.eff~scenario) + ggtitle("Deaths per 100k in each age and risk group by vaccine efficacy")

ggplot(age_group_df, aes(x = age, y = deaths.per100k, color = v.eff, group = v.eff)) + labs(color = "Vaccine \nefficacy") +
  geom_point() + geom_line(linetype = "dotted") + ylab("deaths per 100k") +
  facet_grid(scenario~v.eff) + ggtitle("Deaths per 100k in each age group by vaccine efficacy")

ggplot(df3, aes(x = age, y = deaths.per100k, color = risk, group = risk)) + labs(color = "Vaccine \nefficacy") +
  geom_point() + geom_line(linetype = "dotted") + ylab("deaths per 100k") +
  facet_grid(scenario~v.eff) + ggtitle("Deaths per 100k in each age group by vaccine efficacy")

##################
ggplot(vaxbase_df_age, aes(x = age, y = deaths.per100k, color = v.eff, group = v.eff)) + geom_point() + geom_line(linetype = "dotted") + ggtitle("Deaths per 100k in each age group by vaccine efficacy")

ggplot(vaxbase_df_age, aes(x = age, y = deaths.per100k, color = v.eff, group = v.eff)) + labs(color = "Vaccine \nefficacy") +
  geom_point() + geom_line(linetype = "dotted") + ylab("deaths") +
  facet_grid(~v.eff) + ggtitle("Deaths per 100k in each age group by vaccine efficacy")

ggplot(vaxbase_df, aes(x = age, y = deaths.per100k, color = risk, group = risk)) + labs(color = "Risk \ngroup") +
  geom_point() + geom_line(linetype = "dotted") + ylab("deaths") +
  facet_grid(~v.eff) + ggtitle("Deaths per 100k in each age and risk group by vaccine efficacy")

ggplot(vaxbase_df, aes(x = age, y = deaths.per100k, color = risk, group = risk)) + labs(color = "Risk \ngroup") +
  geom_point() + geom_line(linetype = "dotted") + ylab("deaths") +
  facet_grid(v.eff~risk) + ggtitle("Deaths per 100k in each age and risk group by vaccine efficacy")
##################

### DALYs per 100k
# DALYs per 100k
ggplot(age_group_df, aes(x = age, y = dalys.per100k, color = v.eff, group = v.eff)) + geom_point() + geom_line(linetype = "dotted") + ggtitle("DALYs per 100k in each age group by vaccine efficacy") + facet_grid(~scenario)

ggplot(age_group_df, aes(x = age, y = dalys.per100k, color = v.eff, group = v.eff)) + labs(color = "Vaccine \nefficacy") +
  geom_point() + geom_line(linetype = "dotted") + ylab("DALYs per 100k") +
  facet_grid(v.eff~scenario) + ggtitle("DALYs per 100k in each age group by vaccine efficacy")

ggplot(df3, aes(x = age, y = dalys.per100k, color = risk, group = risk)) + labs(color = "Risk \ngroup") +
  geom_point() + geom_line(linetype = "dotted") + ylab("DALYs per 100k") +
  facet_grid(v.eff~scenario) + ggtitle("DALYs per 100k in each age and risk group by vaccine efficacy")

ggplot(age_group_df, aes(x = age, y = dalys.per100k, color = v.eff, group = v.eff)) + labs(color = "Vaccine \nefficacy") +
  geom_point() + geom_line(linetype = "dotted") + ylab("DALYs per 100k") +
  facet_grid(scenario~v.eff) + ggtitle("DALYs per 100k in each age group by vaccine efficacy")

ggplot(df3, aes(x = age, y = dalys.per100k, color = risk, group = risk)) + labs(color = "Vaccine \nefficacy") +
  geom_point() + geom_line(linetype = "dotted") + ylab("DALYs per 100k") +
  facet_grid(scenario~v.eff) + ggtitle("DALYs per 100k in each age group by vaccine efficacy")

########## Fig. DALYs, DALYs averted
# age groups
ggplot(age_df_base, aes(x = age, y = dalys.per100k, group = v.eff, color = v.eff)) +
  geom_point() + geom_line(linetype = "dotted") + labs(color = "Vaccine \nefficacy") + 
  ylab("DALYs per 100k") + ggtitle("Base vaccination: DALYs per 100k")
ggplot(age_df_base, aes(x = age, y = dalys.averted.per100k, group = v.eff, color = v.eff)) +
  geom_point() + geom_line(linetype = "dotted") + labs(color = "Vaccine \nefficacy") + 
  ylab("DALYs per 100k") + ggtitle("Base vaccination: Averted DALYs per 100k")

# risk groups
ggplot(risk_df_base, aes(x = age, y = dalys.per100k, group = risk, color = risk)) +
  geom_point() + geom_line(linetype = "dotted") + labs(color = "Risk group") + 
  ylab("DALYs per 100k") + ggtitle("Base vaccination: DALYs per 100k") + facet_grid(~ v.eff)
ggplot(risk_df_base, aes(x = age, y = dalys.averted.per100k, group = risk, color = risk)) +
  geom_point() + geom_line(linetype = "dotted") + labs(color = "Risk group") + 
  ylab("DALYs per 100k") + ggtitle("Base vaccination: Averted DALYs per 100k") + facet_grid(~ v.eff)

###############
ggplot(vaxbase_df_age, aes(x = age, y = dalys.per100k, color = v.eff, group = v.eff)) + geom_point() + geom_line(linetype = "dotted") +  ggtitle("DALYs per 100k in each age group by vaccine efficacy")

ggplot(vaxbase_df_age, aes(x = age, y = dalys.per100k, color = v.eff, group = v.eff)) + labs(color = "Vaccine \nefficacy") +
  geom_point() + geom_line(linetype = "dotted") + ylab("DALYs") +
  facet_grid(~v.eff) + ggtitle("DALYs per 100k in each age group by vaccine efficacy")

ggplot(vaxbase_df, aes(x = age, y = dalys.per100k, color = risk, group = risk)) + labs(color = "Risk \ngroup") +
  geom_point() + geom_line(linetype = "dotted") + ylab("DALYs") +
  facet_grid(~v.eff) + ggtitle("DALYs per 100k in each age and risk group by vaccine efficacy")

ggplot(vaxbase_df, aes(x = age, y = dalys.per100k, color = risk, group = risk)) + labs(color = "Risk \ngroup") +
  geom_point() + geom_line(linetype = "dotted") + ylab("DALYs") +
  facet_grid(v.eff~risk) + ggtitle("DALYs per 100k in each age and risk group by vaccine efficacy")


#########################################################
# FIGURES. 70% Vaccination Scenario

## Fig. ICERs
```{r}
## Figure. 70% Vaccination: ICERs.
g <- ggplot(df_vax70[(df_vax70$age=="All"),])
t1 <- labs(y = "$ saved per case averted", title = "70% vaccination: ICER per case averted")
t2 <- labs(y = "$ saved per death averted", title = "70% vaccination:ICER per death averted")
t3 <- labs(y = "$ saved per DALY averted", title = "70% vaccination: ICER per DALY averted")

# icer per case averted
ggplot(age_df_vax70, aes(x = age, y = icer.case.averted, color = v.eff, group = v.eff)) +
  geom_point() + geom_line(linetype = "dotted") + labs(color = "Vaccine \nefficacy") + t1
g + aes(x = v.eff, y = icer.case.averted, group = risk, color = risk) + 
  geom_point() + geom_line(linetype = "dotted") + t1

# icer per death averted
ggplot(age_df_vax70, aes(x = age, y = icer.death.averted, color = v.eff, group = v.eff)) +
  geom_point() + geom_line(linetype = "dotted") + labs(color = "Vaccine \nefficacy") + t2
g + aes(x = v.eff, y = icer.death.averted, group = risk, color = risk) + 
  geom_point() + geom_line(linetype = "dotted") + t2

# icer per daly averted
ggplot(age_df_vax70, aes(x = age, y = icer.daly.averted, color = v.eff, group = v.eff)) +
  geom_point() + geom_line(linetype = "dotted") + labs(color = "Vaccine \nefficacy") + t3
g + aes(x = v.eff, y = icer.daly.averted, group = risk, color = risk) + 
  geom_point() + geom_line(linetype = "dotted") + t3
```

## Fig. Cases, cases averted
```{r}
# age groups
ggplot(age_df_vax70, aes(x = age, y = cases.per100k, group = v.eff, color = v.eff)) +
  geom_point() + geom_line(linetype = "dotted") + labs(color = "Vaccine \nefficacy") + 
  ylab("Cases per 100k") + ggtitle("70% vaccination: Number of cases per 100k")
ggplot(age_df_vax70, aes(x = age, y = cases.averted.per100k, group = v.eff, color = v.eff)) +
  geom_point() + geom_line(linetype = "dotted") + labs(color = "Vaccine \nefficacy") + 
  ylab("Cases per 100k") + ggtitle("70% vaccination: Averted cases per 100k")

# risk groups
ggplot(df_vax70, aes(x = age, y = cases.per100k, group = risk, color = risk)) +
  geom_point() + geom_line(linetype = "dotted") + labs(color = "Risk group") + 
  ylab("Cases per 100k") + ggtitle("70% vaccination: Number of cases per 100k") + facet_grid(~ v.eff)
ggplot(df_vax70, aes(x = age, y = cases.averted.per100k, group = risk, color = risk)) +
  geom_point() + geom_line(linetype = "dotted") + labs(color = "Risk group") + 
  ylab("Cases per 100k") + ggtitle("70% vaccination: Averted cases per 100k") + facet_grid(~ v.eff)
```

```{r eval=FALSE, include=FALSE}
## Vax 70% FIGURES: Cases
vax70_df <- df3[(df3$scenario=="vax70"),]
vax70_df_risk <- risk_group_df[(risk_group_df$scenario=="vax70"),]
vax70_df_age <- age_group_df[(age_group_df$scenario=="vax70"),]

ggplot(vax70_df_age, aes(x = age, y = cases, color = v.eff, group = v.eff)) + geom_point() + geom_line(linetype = "dotted") +
  ggtitle("Number of cases in each age group by vaccine efficacy")

ggplot(vax70_df_age, aes(x = age, y = cases, color = v.eff, group = v.eff)) + labs(color = "Vaccine \nefficacy") +
  geom_point() + geom_line(linetype = "dotted") + ylab("cases") +
  facet_grid(~v.eff) + ggtitle("Number of cases in each age group by vaccine efficacy")

ggplot(vax70_df, aes(x = age, y = cases, color = risk, group = risk)) + labs(color = "Risk \ngroup") +
  geom_point() + geom_line(linetype = "dotted") + ylab("cases") +
  facet_grid(~v.eff) + ggtitle("Number of cases in each age and risk group by vaccine efficacy")

ggplot(vax70_df, aes(x = age, y = cases, color = risk, group = risk)) + labs(color = "Risk \ngroup") +
  geom_point() + geom_line(linetype = "dotted") + ylab("cases") +
  facet_grid(v.eff~risk) + ggtitle("Number of cases in each age and risk group by vaccine efficacy")
```

## Fig. Deaths, deaths averted
```{r}
# age groups
ggplot(age_df_vax70, aes(x = age, y = deaths.per100k, group = v.eff, color = v.eff)) +
  geom_point() + geom_line(linetype = "dotted") + labs(color = "Vaccine \nefficacy") + 
  ylab("Deaths per 100k") + ggtitle("70% vaccination: Deaths per 100k")
ggplot(age_df_vax70, aes(x = age, y = deaths.averted.per100k, group = v.eff, color = v.eff)) +
  geom_point() + geom_line(linetype = "dotted") + labs(color = "Vaccine \nefficacy") + 
  ylab("Deaths per 100k") + ggtitle("70% vaccination: Averted deaths per 100k")

# risk groups
ggplot(risk_df_vax70, aes(x = age, y = deaths.per100k, group = risk, color = risk)) +
  geom_point() + geom_line(linetype = "dotted") + labs(color = "Risk group") + 
  ylab("Deaths per 100k") + ggtitle("70% vaccination: Deaths per 100k") + facet_grid(~ v.eff)
ggplot(risk_df_vax70, aes(x = age, y = deaths.averted.per100k, group = risk, color = risk)) +
  geom_point() + geom_line(linetype = "dotted") + labs(color = "Risk group") + 
  ylab("Deaths per 100k") + ggtitle("70% vaccination: Averted deaths per 100k") + facet_grid(~ v.eff)
```

```{r eval=FALSE, include=FALSE}
## Vax 70% FIGURES: Deaths
ggplot(vax70_df_age, aes(x = age, y = deaths.per100k, color = v.eff, group = v.eff)) + geom_point() + geom_line(linetype = "dotted") + ggtitle("Deaths per 100k in each age group by vaccine efficacy")

ggplot(vax70_df_age, aes(x = age, y = deaths.per100k, color = v.eff, group = v.eff)) + labs(color = "Vaccine \nefficacy") +
  geom_point() + geom_line(linetype = "dotted") + ylab("deaths") +
  facet_grid(~v.eff) + ggtitle("Deaths per 100k in each age group by vaccine efficacy")

ggplot(vax70_df, aes(x = age, y = deaths.per100k, color = risk, group = risk)) + labs(color = "Risk \ngroup") +
  geom_point() + geom_line(linetype = "dotted") + ylab("deaths") +
  facet_grid(~v.eff) + ggtitle("Deaths per 100k in each age and risk group by vaccine efficacy")

ggplot(vax70_df, aes(x = age, y = deaths.per100k, color = risk, group = risk)) + labs(color = "Risk \ngroup") +
  geom_point() + geom_line(linetype = "dotted") + ylab("deaths") +
  facet_grid(v.eff~risk) + ggtitle("Deaths per 100k in each age and risk group by vaccine efficacy")
```

## Fig. DALYs, DALYs averted
```{r}
# age groups
ggplot(age_df_vax70, aes(x = age, y = dalys.per100k, group = v.eff, color = v.eff)) +
  geom_point() + geom_line(linetype = "dotted") + labs(color = "Vaccine \nefficacy") + 
  ylab("DALYs per 100k") + ggtitle("70% vaccination: DALYs per 100k")
ggplot(age_df_vax70, aes(x = age, y = dalys.averted.per100k, group = v.eff, color = v.eff)) +
  geom_point() + geom_line(linetype = "dotted") + labs(color = "Vaccine \nefficacy") + 
  ylab("DALYs per 100k") + ggtitle("70% vaccination: Averted DALYs per 100k")

# risk groups
ggplot(risk_df_vax70, aes(x = age, y = dalys.per100k, group = risk, color = risk)) +
  geom_point() + geom_line(linetype = "dotted") + labs(color = "Risk group") + 
  ylab("DALYs per 100k") + ggtitle("70% vaccination: DALYs per 100k") + facet_grid(~ v.eff)
ggplot(risk_df_vax70, aes(x = age, y = dalys.averted.per100k, group = risk, color = risk)) +
  geom_point() + geom_line(linetype = "dotted") + labs(color = "Risk group") + 
  ylab("DALYs per 100k") + ggtitle("70% vaccination: Averted DALYs per 100k") + facet_grid(~ v.eff)
```

```{r eval=FALSE, include=FALSE}
## Vax 70% FIGURES: DALYs
ggplot(vax70_df_age, aes(x = age, y = dalys.per100k, color = v.eff, group = v.eff)) + geom_point() + geom_line(linetype = "dotted") +
  ggtitle("DALYs per 100k in each age group by vaccine efficacy")

ggplot(vax70_df_age, aes(x = age, y = dalys.per100k, color = v.eff, group = v.eff)) + labs(color = "Vaccine \nefficacy") +
  geom_point() + geom_line(linetype = "dotted") + ylab("DALYs") +
  facet_grid(~v.eff) + ggtitle("DALYs per 100k in each age group by vaccine efficacy")

ggplot(vax70_df, aes(x = age, y = dalys.per100k, color = risk, group = risk)) + labs(color = "Risk \ngroup") +
  geom_point() + geom_line(linetype = "dotted") + ylab("DALYs") +
  facet_grid(~v.eff) + ggtitle("DALYs per 100k in each age and risk group by vaccine efficacy")

ggplot(vax70_df, aes(x = age, y = dalys.per100k, color = risk, group = risk)) + labs(color = "Risk \ngroup") +
  geom_point() + geom_line(linetype = "dotted") + ylab("DALYs") +
  facet_grid(v.eff~risk) + ggtitle("DALYs per 100k in each age and risk group by vaccine efficacy")
```




