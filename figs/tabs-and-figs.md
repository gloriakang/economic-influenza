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
library(ggplot2)
library(tidyr)
library(dplyr)
library(knitr)
library(rmarkdown)
library(formatR)
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

# Tables



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

# Base Vaccination Scenario

## Fig. ICER per case averted

```r
ggplot(age_df_base, aes(x = age, y = icer.case.averted, color = v.eff, group = v.eff)) + 
    geom_point() + geom_line(linetype = "dotted") + labs(x = "Age group", color = "Vaccine \nefficacy") + 
    ylab("$ saved per case averted") + ggtitle("Base Vaccination: ICER per case averted")
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

```r
ggplot(df_base[(df_base$age == "All"), ], aes(x = v.eff, y = icer.case.averted, 
    group = risk, color = risk)) + geom_point() + geom_line(linetype = "dotted") + 
    labs(x = "Vaccine efficacy (%)", color = "Risk") + ylab("$ saved per case averted") + 
    ggtitle("Base Vaccination: ICER per case averted")
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-6-2.png)<!-- -->

```r
ggplot(df_base, aes(x = age, y = icer.case.averted, group = risk, color = risk)) + 
    geom_point() + geom_line(linetype = "dotted") + labs(x = "Age group", color = "Risk group") + 
    ylab("$ saved per case averted") + ggtitle("Base Vaccination: ICER per case averted") + 
    facet_grid(~v.eff)
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-6-3.png)<!-- -->

## Fig. ICER per death averted

```r
ggplot(age_df_base, aes(x = age, y = icer.death.averted, color = v.eff, group = v.eff)) + 
    geom_point() + geom_line(linetype = "dotted") + labs(x = "Age group", color = "Vaccine \nefficacy") + 
    ylab("$ saved per death averted") + ggtitle("Base Vaccination: ICER per death averted")
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

```r
ggplot(df_base[(df_base$age == "All"), ], aes(x = v.eff, y = icer.death.averted, 
    group = risk, color = risk)) + geom_point() + geom_line(linetype = "dotted") + 
    labs(x = "Vaccine efficacy (%)", color = "Risk") + ylab("$ saved per death averted") + 
    ggtitle("Base Vaccination: ICER per death averted")
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-7-2.png)<!-- -->

```r
ggplot(df_base, aes(x = age, y = icer.death.averted, group = risk, color = risk)) + 
    geom_point() + geom_line(linetype = "dotted") + labs(x = "Age group", color = "Risk group") + 
    ylab("$ saved per death averted") + ggtitle("Base Vaccination: ICER per death averted") + 
    facet_grid(~v.eff)
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-7-3.png)<!-- -->

## Fig. ICER per DALY averted

```r
ggplot(age_df_base, aes(x = age, y = icer.daly.averted, color = v.eff, group = v.eff)) + 
    geom_point() + geom_line(linetype = "dotted") + labs(x = "Age group", color = "Vaccine \nefficacy") + 
    ylab("$ saved per DALY averted") + ggtitle("Base Vaccination: ICER per DALY averted")
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

```r
ggplot(df_base[(df_base$age == "All"), ], aes(x = v.eff, y = icer.daly.averted, 
    group = risk, color = risk)) + geom_point() + geom_line(linetype = "dotted") + 
    labs(x = "Vaccine efficacy (%)", color = "Risk") + ylab("$ saved per DALY averted") + 
    ggtitle("Base Vaccination: ICER per DALY averted")
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-8-2.png)<!-- -->

```r
ggplot(df_base, aes(x = age, y = icer.daly.averted, group = risk, color = risk)) + 
    geom_point() + geom_line(linetype = "dotted") + labs(x = "Age group", color = "Risk group") + 
    ylab("$ saved per DALY averted") + ggtitle("Base Vaccination: ICER per DALY averted") + 
    facet_grid(~v.eff)
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-8-3.png)<!-- -->

Set up plots.

```r
# age
g1 <- ggplot(age_df_base, aes(x = age, group = v.eff, color = v.eff)) + geom_point() + 
    geom_line(linetype = "dotted") + labs(x = "Age group", color = "Vaccine \nefficacy")
# risk
g2 <- ggplot(df_base[(df_base$age == "All"), ], aes(x = v.eff, group = risk, 
    color = risk)) + geom_point() + geom_line(linetype = "dotted") + labs(x = "Vaccine efficacy", 
    color = "Risk group")

g3 <- ggplot(risk_df_base, aes(x = age, group = risk, color = risk)) + geom_point() + 
    geom_line(linetype = "dotted") + labs(x = "Age group", color = "Risk group") + 
    facet_grid(~v.eff)
```

## Fig. Cases, cases averted

```r
# age groups
g1 + aes(y = cases) + labs(y = "Cases", title = "Base Vaccination: Cases")
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

```r
g1 + aes(y = cases.per100k) + labs(y = "Cases per 100k", title = "Base Vaccination: Cases per 100k")
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-10-2.png)<!-- -->

```r
g1 + aes(y = cases.averted) + labs(y = "Cases averted", title = "Base Vaccination: Cases averted")
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-10-3.png)<!-- -->

```r
g1 + aes(y = cases.averted.per100k) + labs(y = "Cases averted per 100k", title = "Base Vaccination: Cases averted per 100k")
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-10-4.png)<!-- -->

```r
# risk groups
g2 + aes(y = cases) + labs(y = "Cases", title = "Base Vaccination: Cases")
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-10-5.png)<!-- -->

```r
g2 + aes(y = cases.per100k) + labs(y = "Cases per 100k", title = "Base Vaccination: Cases per 100k")
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-10-6.png)<!-- -->

```r
g2 + aes(y = cases.averted) + labs(y = "Cases averted", title = "Base Vaccination: Cases averted")
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-10-7.png)<!-- -->

```r
g2 + aes(y = cases.averted.per100k) + labs(y = "Cases averted per 100k", title = "Base Vaccination: Cases averted per 100k")
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-10-8.png)<!-- -->

```r
g3 + aes(y = cases) + labs(y = "Cases", title = "Base Vaccination: Cases")
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-10-9.png)<!-- -->

```r
g3 + aes(y = cases.per100k) + labs(y = "Cases per 100k", title = "Base Vaccination: Cases per 100k")
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-10-10.png)<!-- -->

```r
g3 + aes(y = cases.averted) + labs(y = "Cases averted", title = "Base Vaccination: Cases averted")
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-10-11.png)<!-- -->

```r
g3 + aes(y = cases.averted.per100k) + labs(y = "Cases averted per 100k", title = "Base Vaccination: Cases averted per 100k")
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-10-12.png)<!-- -->

## Fig. Deaths, deaths averted

```r
# age groups
g1 + aes(y = deaths) + labs(y = "Deaths", title = "Base Vaccination: Deaths")
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

```r
g1 + aes(y = deaths.per100k) + labs(y = "Deaths per 100k", title = "Base Vaccination: Deaths per 100k")
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-11-2.png)<!-- -->

```r
g1 + aes(y = deaths.averted) + labs(y = "Deaths averted", title = "Base Vaccination: Deaths averted")
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-11-3.png)<!-- -->

```r
g1 + aes(y = deaths.averted.per100k) + labs(y = "Deaths averted per 100k", title = "Base Vaccination: Deaths averted per 100k")
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-11-4.png)<!-- -->

```r
# risk groups
g2 + aes(y = deaths) + labs(y = "Deaths", title = "Base Vaccination: Deaths")
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-11-5.png)<!-- -->

```r
g2 + aes(y = deaths.per100k) + labs(y = "Deaths per 100k", title = "Base Vaccination: Deaths per 100k")
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-11-6.png)<!-- -->

```r
g2 + aes(y = deaths.averted) + labs(y = "Deaths averted", title = "Base Vaccination: Deaths averted")
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-11-7.png)<!-- -->

```r
g2 + aes(y = deaths.averted.per100k) + labs(y = "Deaths averted per 100k", title = "Base Vaccination: Deaths averted per 100k")
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-11-8.png)<!-- -->

```r
g3 + aes(y = deaths) + labs(y = "Deaths", title = "Base Vaccination: Deaths")
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-11-9.png)<!-- -->

```r
g3 + aes(y = deaths.per100k) + labs(y = "Deaths per 100k", title = "Base Vaccination: Deaths per 100k")
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-11-10.png)<!-- -->

```r
g3 + aes(y = deaths.averted) + labs(y = "Deaths averted", title = "Base Vaccination: Deaths averted")
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-11-11.png)<!-- -->

```r
g3 + aes(y = deaths.averted.per100k) + labs(y = "Deaths averted per 100k", title = "Base Vaccination: Deaths averted per 100k")
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-11-12.png)<!-- -->

## Fig. DALYs, DALYs averted

```r
# age groups
g1 + aes(y = dalys) + labs(y = "DALYs", title = "Base Vaccination: DALYs")
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-12-1.png)<!-- -->

```r
g1 + aes(y = dalys.per100k) + labs(y = "DALYs per 100k", title = "Base Vaccination: DALYs per 100k")
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-12-2.png)<!-- -->

```r
g1 + aes(y = dalys.averted) + labs(y = "DALYs averted", title = "Base Vaccination: DALYs averted")
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-12-3.png)<!-- -->

```r
g1 + aes(y = dalys.averted.per100k) + labs(y = "DALYs averted per 100k", title = "Base Vaccination: DALYs averted per 100k")
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-12-4.png)<!-- -->

```r
# risk groups
g2 + aes(y = dalys) + labs(y = "DALYs", title = "Base Vaccination: DALYs")
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-12-5.png)<!-- -->

```r
g2 + aes(y = dalys.per100k) + labs(y = "DALYs per 100k", title = "Base Vaccination: DALYs per 100k")
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-12-6.png)<!-- -->

```r
g2 + aes(y = dalys.averted) + labs(y = "DALYs averted", title = "Base Vaccination: DALYs averted")
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-12-7.png)<!-- -->

```r
g2 + aes(y = dalys.averted.per100k) + labs(y = "DALYs averted per 100k", title = "Base Vaccination: DALYs averted per 100k")
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-12-8.png)<!-- -->

```r
g3 + aes(y = dalys) + labs(y = "DALYs", title = "Base Vaccination: DALYs")
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-12-9.png)<!-- -->

```r
g3 + aes(y = dalys.per100k) + labs(y = "DALYs per 100k", title = "Base Vaccination: DALYs per 100k")
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-12-10.png)<!-- -->

```r
g3 + aes(y = dalys.averted) + labs(y = "DALYs averted", title = "Base Vaccination: DALYs averted")
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-12-11.png)<!-- -->

```r
g3 + aes(y = dalys.averted.per100k) + labs(y = "DALYs averted per 100k", title = "Base Vaccination: DALYs averted per 100k")
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-12-12.png)<!-- -->


# 70% Vaccination Scenario

## Fig. ICER per case averted

```r
g <- ggplot(df_vax70[(df_vax70$age == "All"), ], aes(x = v.eff, color = risk, 
    group = risk))
t1 <- labs(y = "$ saved per case averted", title = "70% Vaccination: ICER per case averted")
t2 <- labs(y = "$ saved per death averted", title = "70% Vaccination: ICER per death averted")
t3 <- labs(y = "$ saved per DALY averted", title = "70% Vaccination: ICER per DALY averted")

ggplot(age_df_vax70, aes(x = age, y = icer.case.averted, color = v.eff, group = v.eff)) + 
    geom_point() + geom_line(linetype = "dotted") + labs(x = "Age group", color = "Vaccine \nefficacy") + 
    t1
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-13-1.png)<!-- -->

```r
g + aes(y = icer.case.averted) + geom_point() + geom_line(linetype = "dotted") + 
    t1 + labs(x = "Vaccine efficacy (%)", color = "Risk group")
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-13-2.png)<!-- -->

```r
ggplot(df_vax70, aes(x = age, y = icer.case.averted, group = risk, color = risk)) + 
    geom_point() + geom_line(linetype = "dotted") + labs(color = "Risk group") + 
    ylab("$ saved per case averted") + ggtitle("70% Vaccination: ICER per case averted") + 
    facet_grid(~v.eff)
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-13-3.png)<!-- -->

## Fig. ICER per death averted

```r
ggplot(age_df_vax70, aes(x = age, y = icer.death.averted, color = v.eff, group = v.eff)) + 
    geom_point() + geom_line(linetype = "dotted") + labs(x = "Age group", color = "Vaccine \nefficacy") + 
    t2
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-14-1.png)<!-- -->

```r
g + aes(y = icer.death.averted) + geom_point() + geom_line(linetype = "dotted") + 
    t2 + labs(x = "Vaccine efficacy (%)", color = "Risk group")
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-14-2.png)<!-- -->

```r
ggplot(df_vax70, aes(x = age, y = icer.death.averted, group = risk, color = risk)) + 
    geom_point() + geom_line(linetype = "dotted") + labs(color = "Risk group") + 
    ylab("$ saved per death averted") + ggtitle("70% Vaccination: ICER per death averted") + 
    facet_grid(~v.eff)
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-14-3.png)<!-- -->

## Fig. ICER per DALY averted

```r
ggplot(age_df_vax70, aes(x = age, y = icer.daly.averted, color = v.eff, group = v.eff)) + 
    geom_point() + geom_line(linetype = "dotted") + labs(x = "Age group", color = "Vaccine \nefficacy") + 
    t3
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-15-1.png)<!-- -->

```r
g + aes(y = icer.daly.averted) + geom_point() + geom_line(linetype = "dotted") + 
    t3 + labs(x = "Vaccine efficacy (%)", color = "Risk group")
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-15-2.png)<!-- -->

```r
ggplot(df_vax70, aes(x = age, y = icer.daly.averted, group = risk, color = risk)) + 
    geom_point() + geom_line(linetype = "dotted") + labs(color = "Risk group") + 
    ylab("$ saved per DALY averted") + ggtitle("70% Vaccination: ICER per DALY averted") + 
    facet_grid(~v.eff)
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-15-3.png)<!-- -->

Set up plots.

```r
# age
h1 <- ggplot(age_df_vax70, aes(x = age, group = v.eff, color = v.eff)) + geom_point() + 
    geom_line(linetype = "dotted") + labs(x = "Age group", color = "Vaccine \nefficacy")
# risk
h2 <- ggplot(df_vax70[(df_vax70$age == "All"), ], aes(x = v.eff, group = risk, 
    color = risk)) + geom_point() + geom_line(linetype = "dotted") + labs(x = "Vaccine efficacy", 
    color = "Risk group")

h3 <- ggplot(risk_df_vax70, aes(x = age, group = risk, color = risk)) + geom_point() + 
    geom_line(linetype = "dotted") + labs(x = "Age group", color = "Risk group") + 
    facet_grid(~v.eff)
```

## Fig. Cases, cases averted

```r
# age groups
h1 + aes(y = cases) + labs(y = "Cases", title = "70% Vaccination: Cases")
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-17-1.png)<!-- -->

```r
h1 + aes(y = cases.per100k) + labs(y = "Cases per 100k", title = "70% Vaccination: Cases per 100k")
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-17-2.png)<!-- -->

```r
h1 + aes(y = cases.averted) + labs(y = "Cases averted", title = "70% Vaccination: Cases averted")
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-17-3.png)<!-- -->

```r
h1 + aes(y = cases.averted.per100k) + labs(y = "Cases averted per 100k", title = "70% Vaccination: Cases averted per 100k")
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-17-4.png)<!-- -->

```r
# risk groups
h2 + aes(y = cases) + labs(y = "Cases", title = "70% Vaccination: Cases")
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-17-5.png)<!-- -->

```r
h2 + aes(y = cases.per100k) + labs(y = "Cases per 100k", title = "70% Vaccination: Cases per 100k")
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-17-6.png)<!-- -->

```r
h2 + aes(y = cases.averted) + labs(y = "Cases averted", title = "70% Vaccination: Cases averted")
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-17-7.png)<!-- -->

```r
h2 + aes(y = cases.averted.per100k) + labs(y = "Cases averted per 100k", title = "70% Vaccination: Cases averted per 100k")
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-17-8.png)<!-- -->

```r
h3 + aes(y = cases) + labs(y = "Cases", title = "70% Vaccination: Cases")
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-17-9.png)<!-- -->

```r
h3 + aes(y = cases.per100k) + labs(y = "Cases per 100k", title = "70% Vaccination: Cases per 100k")
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-17-10.png)<!-- -->

```r
h3 + aes(y = cases.averted) + labs(y = "Cases averted", title = "70% Vaccination: Cases averted")
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-17-11.png)<!-- -->

```r
h3 + aes(y = cases.averted.per100k) + labs(y = "Cases averted per 100k", title = "70% Vaccination: Cases averted per 100k")
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-17-12.png)<!-- -->

## Fig. Deaths, deaths averted

```r
# age groups
h1 + aes(y = deaths) + labs(y = "Deaths", title = "70% Vaccination: Deaths")
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-18-1.png)<!-- -->

```r
h1 + aes(y = deaths.per100k) + labs(y = "Deaths per 100k", title = "70% Vaccination: Deaths per 100k")
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-18-2.png)<!-- -->

```r
h1 + aes(y = deaths.averted) + labs(y = "Deaths averted", title = "70% Vaccination: Deaths averted")
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-18-3.png)<!-- -->

```r
h1 + aes(y = deaths.averted.per100k) + labs(y = "Deaths averted per 100k", title = "70% Vaccination: Deaths averted per 100k")
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-18-4.png)<!-- -->

```r
# risk groups
h2 + aes(y = deaths) + labs(y = "Deaths", title = "70% Vaccination: Deaths")
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-18-5.png)<!-- -->

```r
h2 + aes(y = deaths.per100k) + labs(y = "Deaths per 100k", title = "70% Vaccination: Deaths per 100k")
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-18-6.png)<!-- -->

```r
h2 + aes(y = deaths.averted) + labs(y = "Deaths averted", title = "70% Vaccination: Deaths averted")
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-18-7.png)<!-- -->

```r
h2 + aes(y = deaths.averted.per100k) + labs(y = "Deaths averted per 100k", title = "70% Vaccination: Deaths averted per 100k")
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-18-8.png)<!-- -->

```r
h3 + aes(y = deaths) + labs(y = "Deaths", title = "70% Vaccination: Deaths")
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-18-9.png)<!-- -->

```r
h3 + aes(y = deaths.per100k) + labs(y = "Deaths per 100k", title = "70% Vaccination: Deaths per 100k")
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-18-10.png)<!-- -->

```r
h3 + aes(y = deaths.averted) + labs(y = "Deaths averted", title = "70% Vaccination: Deaths averted")
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-18-11.png)<!-- -->

```r
h3 + aes(y = deaths.averted.per100k) + labs(y = "Deaths averted per 100k", title = "70% Vaccination: Deaths averted per 100k")
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-18-12.png)<!-- -->

## Fig. DALYs, DALYs averted

```r
# age groups
h1 + aes(y = dalys) + labs(y = "DALYs", title = "70% Vaccination: DALYs")
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-19-1.png)<!-- -->

```r
h1 + aes(y = dalys.per100k) + labs(y = "DALYs per 100k", title = "70% Vaccination: DALYs per 100k")
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-19-2.png)<!-- -->

```r
h1 + aes(y = dalys.averted) + labs(y = "DALYs averted", title = "70% Vaccination: DALYs averted")
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-19-3.png)<!-- -->

```r
h1 + aes(y = dalys.averted.per100k) + labs(y = "DALYs averted per 100k", title = "70% Vaccination: DALYs averted per 100k")
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-19-4.png)<!-- -->

```r
# risk groups
h2 + aes(y = dalys) + labs(y = "DALYs", title = "70% Vaccination: DALYs")
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-19-5.png)<!-- -->

```r
h2 + aes(y = dalys.per100k) + labs(y = "DALYs per 100k", title = "70% Vaccination: DALYs per 100k")
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-19-6.png)<!-- -->

```r
h2 + aes(y = dalys.averted) + labs(y = "DALYs averted", title = "70% Vaccination: DALYs averted")
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-19-7.png)<!-- -->

```r
h2 + aes(y = dalys.averted.per100k) + labs(y = "DALYs averted per 100k", title = "70% Vaccination: DALYs averted per 100k")
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-19-8.png)<!-- -->

```r
h3 + aes(y = dalys) + labs(y = "DALYs", title = "70% Vaccination: DALYs")
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-19-9.png)<!-- -->

```r
h3 + aes(y = dalys.per100k) + labs(y = "DALYs per 100k", title = "70% Vaccination: DALYs per 100k")
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-19-10.png)<!-- -->

```r
h3 + aes(y = dalys.averted) + labs(y = "DALYs averted", title = "70% Vaccination: DALYs averted")
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-19-11.png)<!-- -->

```r
h3 + aes(y = dalys.averted.per100k) + labs(y = "DALYs averted per 100k", title = "70% Vaccination: DALYs averted per 100k")
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-19-12.png)<!-- -->

