---
title: "figures"
output: 
  html_document: 
    fig_caption: yes
    keep_md: yes
    number_sections: yes
    theme: cosmo
    toc: yes
  html_notebook: 
    fig_caption: yes
    number_sections: yes
    theme: cosmo
    toc: yes
editor_options: 
  chunk_output_type: inline
---




```r
rm(list = ls(all.names = TRUE))
library(ggplot2); library(tidyr); library(dplyr)
library(knitr); library(rmarkdown)
```


```r
data <- read.csv("df/icer-all.csv", as.is = TRUE)
df <- data[(data$scenario!="base"),]

df$cases.averted <- as.numeric(df$cases.averted)
df$cases.averted.per100k <- as.numeric(df$cases.averted.per100k)
df$icer.case.averted <- as.numeric(df$icer.case.averted)
df$deaths.averted <- as.numeric(df$deaths.averted)
df$deaths.averted.per100k <- as.numeric(df$ deaths.averted.per100k)
df$icer.death.averted <- as.numeric(df$icer.death.averted)
df$dalys <- as.numeric(df$dalys)
df$dalys.per100k <- as.numeric(df$dalys.per100k)
df$dalys.averted <- as.numeric(df$dalys.averted)
df$dalys.averted.per100k <- as.numeric(df$dalys.averted.per100k)
df$icer.daly.averted <- as.numeric(df$icer.daly.averted)
```


```r
# subset relevant efficacies
df2 <- df[!(df$scenario == 'vaxbase' & df$v.eff %in% c(50, 60)),]
#table(df2$v.eff, useNA = 'always')

df3 <- df2[!(df2$scenario == 'vax70' & df2$v.eff %in% c(30,40)), ]
#table(df3$scenario, df3$v.eff, useNA = 'always')

# factors
df3$age <- factor(df3$age, levels = c("0-4 yrs", "5-19 yrs", "20-64 yrs", "65+ yrs", "All"))
df3$risk <- factor(df3$risk, levels = c("High", "Non-high", "All"))
df3$scenario <- factor(df3$scenario, levels = c("vaxbase", "vax70"))
df3$v.eff <- factor(df3$v.eff)
```

# TABLES



Subset data.

```r
# subset scenario
df_base <- df3[(df3$scenario == 'vaxbase'),]
df_vax70 <- df3[(df3$scenario == 'vax70'),]

# age groups
age_df_base <- df_base[(df_base$risk == "All"),]
age_df_vax70 <- df_vax70[(df_vax70$risk == "All"),]

# risk groups
risk_df_base <- df_base[!(df_base$age == 'All'),]
risk_df_vax70 <- df_vax70[!(df_vax70$age == 'All'),]
```

# FIGURES. Base Vaccination Scenario

## Fig. ICERs

```r
# icer per case averted
ggplot(age_df_base, aes(x = age, y = icer.case.averted, color = v.eff, group = v.eff)) +
  geom_point() + geom_line(linetype = "dotted") + labs(color = "Vaccine \nefficacy") +
  ylab("$ saved per case averted") + ggtitle("Base vaccination: ICER per case averted")
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

```r
ggplot(df_base[(df_base$age=="All"),], aes(x = v.eff, y = icer.case.averted, group = risk, color = risk)) +
  geom_point() + geom_line(linetype = "dotted") + labs(color = "Risk") +
  ylab("$ saved per case averted") + ggtitle("Base vaccination: ICER per case averted")
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-6-2.png)<!-- -->

```r
ggplot(df_base, aes(x = age, y = icer.case.averted, group = risk, color = risk)) +
  geom_point() + geom_line(linetype = "dotted") + labs(color = "Risk group") + 
  ylab("$ saved per case averted") + ggtitle("Base vaccination: ICER per case averted") + facet_grid(~ v.eff)
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-6-3.png)<!-- -->

```r
# icer per death averted
ggplot(age_df_base, aes(x = age, y = icer.death.averted, color = v.eff, group = v.eff)) +
  geom_point() + geom_line(linetype = "dotted") + labs(color = "Vaccine \nefficacy") + 
  ylab("$ saved per death averted") + ggtitle("Base vaccination: ICER per death averted")
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-6-4.png)<!-- -->

```r
ggplot(df_base[(df_base$age=="All"),], aes(x = v.eff, y = icer.death.averted, group = risk, color = risk)) +
  geom_point() + geom_line(linetype = "dotted") + labs(color = "Risk") +
  ylab("$ saved per death averted") + ggtitle("Base vaccination: ICER per death averted")
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-6-5.png)<!-- -->

```r
ggplot(df_base, aes(x = age, y = icer.death.averted, group = risk, color = risk)) +
  geom_point() + geom_line(linetype = "dotted") + labs(color = "Risk group") + 
  ylab("$ saved per death averted") + ggtitle("Base vaccination: ICER per death averted") + facet_grid(~ v.eff)
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-6-6.png)<!-- -->

```r
# icer per daly averted
ggplot(age_df_base, aes(x = age, y = icer.daly.averted, color = v.eff, group = v.eff)) +
  geom_point() + geom_line(linetype = "dotted") + labs(color = "Vaccine \nefficacy") + 
  ylab("$ saved per DALY averted") + ggtitle("Base vaccination: ICER per DALY averted")
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-6-7.png)<!-- -->

```r
ggplot(df_base[(df_base$age=="All"),], aes(x = v.eff, y = icer.daly.averted, group = risk, color = risk)) +
  geom_point() + geom_line(linetype = "dotted") + labs(color = "Risk") +
  ylab("$ saved per DALY averted") + ggtitle("Base vaccination: ICER per DALY averted")
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-6-8.png)<!-- -->

```r
ggplot(df_base, aes(x = age, y = icer.daly.averted, group = risk, color = risk)) +
  geom_point() + geom_line(linetype = "dotted") + labs(color = "Risk group") + 
  ylab("$ saved per DALY averted") + ggtitle("Base vaccination: ICER per DALY averted") + facet_grid(~ v.eff)
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-6-9.png)<!-- -->


```r
# age
g1 <- ggplot(age_df_base, aes(x = age, group = v.eff, color = v.eff)) +
  geom_point() + geom_line(linetype = "dotted") + labs(x = "Age group", color = "Vaccine \nefficacy")
# risk
g2 <- ggplot(df_base[(df_base$age=="All"),], aes(x = v.eff, group = risk, color = risk)) +
  geom_point() + geom_line(linetype = "dotted") + labs(x = "Vaccine efficacy", color = "Risk group")

g3 <- ggplot(risk_df_base, aes(x = age, group = risk, color = risk)) +
  geom_point() + geom_line(linetype = "dotted") + labs(x = "Age group", color = "Risk group") +
  facet_grid(~ v.eff)
```

## Fig. Cases, cases averted

```r
# age groups
g1 + aes(y = cases) + labs(y = "Cases", title = "Base vaccination: Cases")
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

```r
g1 + aes(y = cases.per100k) + labs(y = "Cases per 100k", title = "Base vaccination: Cases per 100k")
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-8-2.png)<!-- -->

```r
g1 + aes(y = cases.averted) + labs(y = "Cases averted", title = "Base vaccination: Cases averted")
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-8-3.png)<!-- -->

```r
g1 + aes(y = cases.averted.per100k) + labs(y = "Cases averted per 100k", title = "Base vaccination: Cases averted per 100k")
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-8-4.png)<!-- -->

```r
# risk groups
g2 + aes(y = cases) + labs(y = "Cases", title = "Base vaccination: Cases")
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-8-5.png)<!-- -->

```r
g2 + aes(y = cases.per100k) + labs(y = "Cases per 100k", title = "Base vaccination: Cases per 100k")
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-8-6.png)<!-- -->

```r
g2 + aes(y = cases.averted) + labs(y = "Cases averted", title = "Base vaccination: Cases averted")
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-8-7.png)<!-- -->

```r
g2 + aes(y = cases.averted.per100k) + labs(y = "Cases averted per 100k", title = "Base vaccination: Cases averted per 100k")
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-8-8.png)<!-- -->

```r
g3 + aes(y = cases) + labs(y = "Cases", title = "Base vaccination: Cases")
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-8-9.png)<!-- -->

```r
g3 + aes(y = cases.per100k) + labs(y = "Cases per 100k", title = "Base vaccination: Cases per 100k")
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-8-10.png)<!-- -->

```r
g3 + aes(y = cases.averted) + labs(y = "Cases averted", title = "Base vaccination: Cases averted")
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-8-11.png)<!-- -->

```r
g3 + aes(y = cases.averted.per100k) + labs(y = "Cases averted per 100k", title = "Base vaccination: Cases averted per 100k")
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-8-12.png)<!-- -->

## Fig. Deaths, deaths averted

```r
# age groups
g1 + aes(y = deaths) + labs(y = "Deaths", title = "Base vaccination: Deaths")
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

```r
g1 + aes(y = deaths.per100k) + labs(y = "Deaths per 100k", title = "Base vaccination: Deaths per 100k")
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-9-2.png)<!-- -->

```r
g1 + aes(y = deaths.averted) + labs(y = "Deaths averted", title = "Base vaccination: Deaths averted")
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-9-3.png)<!-- -->

```r
g1 + aes(y = deaths.averted.per100k) + labs(y = "Deaths averted per 100k", title = "Base vaccination: Deaths averted per 100k")
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-9-4.png)<!-- -->

```r
# risk groups
g2 + aes(y = deaths) + labs(y = "Deaths", title = "Base vaccination: Deaths")
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-9-5.png)<!-- -->

```r
g2 + aes(y = deaths.per100k) + labs(y = "Deaths per 100k", title = "Base vaccination: Deaths per 100k")
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-9-6.png)<!-- -->

```r
g2 + aes(y = deaths.averted) + labs(y = "Deaths averted", title = "Base vaccination: Deaths averted")
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-9-7.png)<!-- -->

```r
g2 + aes(y = deaths.averted.per100k) + labs(y = "Deaths averted per 100k", title = "Base vaccination: Deaths averted per 100k")
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-9-8.png)<!-- -->

```r
g3 + aes(y = deaths) + labs(y = "Deaths", title = "Base vaccination: Deaths")
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-9-9.png)<!-- -->

```r
g3 + aes(y = deaths.per100k) + labs(y = "Deaths per 100k", title = "Base vaccination: Deaths per 100k")
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-9-10.png)<!-- -->

```r
g3 + aes(y = deaths.averted) + labs(y = "Deaths averted", title = "Base vaccination: Deaths averted")
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-9-11.png)<!-- -->

```r
g3 + aes(y = deaths.averted.per100k) + labs(y = "Deaths averted per 100k", title = "Base vaccination: Deaths averted per 100k")
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-9-12.png)<!-- -->

## Fig. DALYs, DALYs averted

```r
# age groups
g1 + aes(y = dalys) + labs(y = "DALYs", title = "Base vaccination: DALYs")
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

```r
g1 + aes(y = dalys.per100k) + labs(y = "DALYs per 100k", title = "Base vaccination: DALYs per 100k")
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-10-2.png)<!-- -->

```r
g1 + aes(y = dalys.averted) + labs(y = "DALYs averted", title = "Base vaccination: DALYs averted")
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-10-3.png)<!-- -->

```r
g1 + aes(y = dalys.averted.per100k) + labs(y = "DALYs averted per 100k", title = "Base vaccination: DALYs averted per 100k")
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-10-4.png)<!-- -->

```r
# risk groups
g2 + aes(y = dalys) + labs(y = "DALYs", title = "Base vaccination: DALYs")
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-10-5.png)<!-- -->

```r
g2 + aes(y = dalys.per100k) + labs(y = "DALYs per 100k", title = "Base vaccination: DALYs per 100k")
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-10-6.png)<!-- -->

```r
g2 + aes(y = dalys.averted) + labs(y = "DALYs averted", title = "Base vaccination: DALYs averted")
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-10-7.png)<!-- -->

```r
g2 + aes(y = dalys.averted.per100k) + labs(y = "DALYs averted per 100k", title = "Base vaccination: DALYs averted per 100k")
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-10-8.png)<!-- -->

```r
g3 + aes(y = dalys) + labs(y = "DALYs", title = "Base vaccination: DALYs")
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-10-9.png)<!-- -->

```r
g3 + aes(y = dalys.per100k) + labs(y = "DALYs per 100k", title = "Base vaccination: DALYs per 100k")
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-10-10.png)<!-- -->

```r
g3 + aes(y = dalys.averted) + labs(y = "DALYs averted", title = "Base vaccination: DALYs averted")
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-10-11.png)<!-- -->

```r
g3 + aes(y = dalys.averted.per100k) + labs(y = "DALYs averted per 100k", title = "Base vaccination: DALYs averted per 100k")
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-10-12.png)<!-- -->

# FIGURES. 70% Vaccination Scenario

## Fig. ICERs

```r
## Figure. 70% Vaccination: ICERs.
g <- ggplot(df_vax70[(df_vax70$age=="All"),])
t1 <- labs(y = "$ saved per case averted", title = "70% vaccination: ICER per case averted")
t2 <- labs(y = "$ saved per death averted", title = "70% vaccination:ICER per death averted")
t3 <- labs(y = "$ saved per DALY averted", title = "70% vaccination: ICER per DALY averted")

# icer per case averted
ggplot(age_df_vax70, aes(x = age, y = icer.case.averted, color = v.eff, group = v.eff)) +
  geom_point() + geom_line(linetype = "dotted") + labs(color = "Vaccine \nefficacy") + t1
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

```r
g + aes(x = v.eff, y = icer.case.averted, group = risk, color = risk) + 
  geom_point() + geom_line(linetype = "dotted") + t1
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-11-2.png)<!-- -->

```r
# icer per death averted
ggplot(age_df_vax70, aes(x = age, y = icer.death.averted, color = v.eff, group = v.eff)) +
  geom_point() + geom_line(linetype = "dotted") + labs(color = "Vaccine \nefficacy") + t2
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-11-3.png)<!-- -->

```r
g + aes(x = v.eff, y = icer.death.averted, group = risk, color = risk) + 
  geom_point() + geom_line(linetype = "dotted") + t2
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-11-4.png)<!-- -->

```r
# icer per daly averted
ggplot(age_df_vax70, aes(x = age, y = icer.daly.averted, color = v.eff, group = v.eff)) +
  geom_point() + geom_line(linetype = "dotted") + labs(color = "Vaccine \nefficacy") + t3
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-11-5.png)<!-- -->

```r
g + aes(x = v.eff, y = icer.daly.averted, group = risk, color = risk) + 
  geom_point() + geom_line(linetype = "dotted") + t3
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-11-6.png)<!-- -->

```r
# age
h1 <- ggplot(age_df_vax70, aes(x = age, group = v.eff, color = v.eff)) +
  geom_point() + geom_line(linetype = "dotted") + labs(x = "Age group", color = "Vaccine \nefficacy")
# risk
h2 <- ggplot(df_vax70[(df_vax70$age=="All"),], aes(x = v.eff, group = risk, color = risk)) +
  geom_point() + geom_line(linetype = "dotted") + labs(x = "Vaccine efficacy", color = "Risk group")

h3 <- ggplot(risk_df_vax70, aes(x = age, group = risk, color = risk)) +
  geom_point() + geom_line(linetype = "dotted") + labs(x = "Age group", color = "Risk group") +
  facet_grid(~ v.eff)
```

## Fig. Cases, cases averted

```r
# age groups
h1 + aes(y = cases) + labs(y = "Cases", title = "70% vaccination: Cases")
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-13-1.png)<!-- -->

```r
h1 + aes(y = cases.per100k) + labs(y = "Cases per 100k", title = "70% vaccination: Cases per 100k")
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-13-2.png)<!-- -->

```r
h1 + aes(y = cases.averted) + labs(y = "Cases averted", title = "70% vaccination: Cases averted")
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-13-3.png)<!-- -->

```r
h1 + aes(y = cases.averted.per100k) + labs(y = "Cases averted per 100k", title = "70% vaccination: Cases averted per 100k")
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-13-4.png)<!-- -->

```r
# risk groups
h2 + aes(y = cases) + labs(y = "Cases", title = "70% vaccination: Cases")
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-13-5.png)<!-- -->

```r
h2 + aes(y = cases.per100k) + labs(y = "Cases per 100k", title = "70% vaccination: Cases per 100k")
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-13-6.png)<!-- -->

```r
h2 + aes(y = cases.averted) + labs(y = "Cases averted", title = "70% vaccination: Cases averted")
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-13-7.png)<!-- -->

```r
h2 + aes(y = cases.averted.per100k) + labs(y = "Cases averted per 100k", title = "70% vaccination: Cases averted per 100k")
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-13-8.png)<!-- -->

```r
h3 + aes(y = cases) + labs(y = "Cases", title = "70% vaccination: Cases")
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-13-9.png)<!-- -->

```r
h3 + aes(y = cases.per100k) + labs(y = "Cases per 100k", title = "70% vaccination: Cases per 100k")
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-13-10.png)<!-- -->

```r
h3 + aes(y = cases.averted) + labs(y = "Cases averted", title = "70% vaccination: Cases averted")
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-13-11.png)<!-- -->

```r
h3 + aes(y = cases.averted.per100k) + labs(y = "Cases averted per 100k", title = "70% vaccination: Cases averted per 100k")
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-13-12.png)<!-- -->

## Fig. Deaths, deaths averted

```r
# age groups
h1 + aes(y = deaths) + labs(y = "Deaths", title = "70% vaccination: Deaths")
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-14-1.png)<!-- -->

```r
h1 + aes(y = deaths.per100k) + labs(y = "Deaths per 100k", title = "70% vaccination: Deaths per 100k")
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-14-2.png)<!-- -->

```r
h1 + aes(y = deaths.averted) + labs(y = "Deaths averted", title = "70% vaccination: Deaths averted")
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-14-3.png)<!-- -->

```r
h1 + aes(y = deaths.averted.per100k) + labs(y = "Deaths averted per 100k", title = "70% vaccination: Deaths averted per 100k")
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-14-4.png)<!-- -->

```r
# risk groups
h2 + aes(y = deaths) + labs(y = "Deaths", title = "70% vaccination: Deaths")
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-14-5.png)<!-- -->

```r
h2 + aes(y = deaths.per100k) + labs(y = "Deaths per 100k", title = "70% vaccination: Deaths per 100k")
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-14-6.png)<!-- -->

```r
h2 + aes(y = deaths.averted) + labs(y = "Deaths averted", title = "70% vaccination: Deaths averted")
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-14-7.png)<!-- -->

```r
h2 + aes(y = deaths.averted.per100k) + labs(y = "Deaths averted per 100k", title = "70% vaccination: Deaths averted per 100k")
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-14-8.png)<!-- -->

```r
h3 + aes(y = deaths) + labs(y = "Deaths", title = "70% vaccination: Deaths")
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-14-9.png)<!-- -->

```r
h3 + aes(y = deaths.per100k) + labs(y = "Deaths per 100k", title = "70% vaccination: Deaths per 100k")
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-14-10.png)<!-- -->

```r
h3 + aes(y = deaths.averted) + labs(y = "Deaths averted", title = "70% vaccination: Deaths averted")
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-14-11.png)<!-- -->

```r
h3 + aes(y = deaths.averted.per100k) + labs(y = "Deaths averted per 100k", title = "70% vaccination: Deaths averted per 100k")
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-14-12.png)<!-- -->

## Fig. DALYs, DALYs averted

```r
# age groups
h1 + aes(y = dalys) + labs(y = "DALYs", title = "70% vaccination: DALYs")
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-15-1.png)<!-- -->

```r
h1 + aes(y = dalys.per100k) + labs(y = "DALYs per 100k", title = "70% vaccination: DALYs per 100k")
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-15-2.png)<!-- -->

```r
h1 + aes(y = dalys.averted) + labs(y = "DALYs averted", title = "70% vaccination: DALYs averted")
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-15-3.png)<!-- -->

```r
h1 + aes(y = dalys.averted.per100k) + labs(y = "DALYs averted per 100k", title = "70% vaccination: DALYs averted per 100k")
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-15-4.png)<!-- -->

```r
# risk groups
h2 + aes(y = dalys) + labs(y = "DALYs", title = "70% vaccination: DALYs")
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-15-5.png)<!-- -->

```r
h2 + aes(y = dalys.per100k) + labs(y = "DALYs per 100k", title = "70% vaccination: DALYs per 100k")
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-15-6.png)<!-- -->

```r
h2 + aes(y = dalys.averted) + labs(y = "DALYs averted", title = "70% vaccination: DALYs averted")
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-15-7.png)<!-- -->

```r
h2 + aes(y = dalys.averted.per100k) + labs(y = "DALYs averted per 100k", title = "70% vaccination: DALYs averted per 100k")
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-15-8.png)<!-- -->

```r
h3 + aes(y = dalys) + labs(y = "DALYs", title = "70% vaccination: DALYs")
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-15-9.png)<!-- -->

```r
h3 + aes(y = dalys.per100k) + labs(y = "DALYs per 100k", title = "70% vaccination: DALYs per 100k")
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-15-10.png)<!-- -->

```r
h3 + aes(y = dalys.averted) + labs(y = "DALYs averted", title = "70% vaccination: DALYs averted")
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-15-11.png)<!-- -->

```r
h3 + aes(y = dalys.averted.per100k) + labs(y = "DALYs averted per 100k", title = "70% vaccination: DALYs averted per 100k")
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-15-12.png)<!-- -->

