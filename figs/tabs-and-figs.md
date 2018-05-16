---
title: "figures"
output: 
  html_document: 
    css: ~/git/economic-influenza/css/default.css
    fig_caption: yes
    keep_md: yes
    theme: cosmo
  html_notebook: 
    css: ~/git/economic-influenza/css/default.css
    theme: cosmo
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
df2 <- df[!df$v.eff %in% c(60), ]
table(df2$v.eff, useNA = 'always')
```

```
## 
##   10   20   30   40   50 <NA> 
##   30   30   30   30   30    0
```

```r
df3 <- df2[!(df2$scenario == 'vax70' & df2$v.eff %in% c(40,50)), ]
table(df3$scenario, df3$v.eff, useNA = 'always')
```

```
##          
##           10 20 30 40 50 <NA>
##   vax70   15 15 15  0  0    0
##   vaxbase 15 15 15 15 15    0
##   <NA>     0  0  0  0  0    0
```

```r
# factors
df3$age <- factor(df3$age, levels = c("0-4 yrs", "5-19 yrs", "20-64 yrs", "65+ yrs", "All"))
df3$risk <- factor(df3$risk, levels = c("High", "Non-high", "All"))
df3$scenario <- factor(df3$scenario, levels = c("vaxbase", "vax70"))
df3$v.eff <- factor(df3$v.eff)
```

# Table: ICERs.

```r
t1 <- df3[c("scenario", "age", "risk", "v.eff", "icer.case.averted", "icer.death.averted", "icer.daly.averted")] %>%
  gather("icer.case.averted", "icer.death.averted", "icer.daly.averted", key = "icer", value = "value") %>%
  spread(risk, value) %>%
  arrange(scenario, v.eff, icer)
head(t1)
```

```
##   scenario       age v.eff              icer    High Non-high     All
## 1  vaxbase   0-4 yrs    10 icer.case.averted 1230.87     2.86   81.45
## 2  vaxbase  5-19 yrs    10 icer.case.averted 2823.01   139.30  311.06
## 3  vaxbase 20-64 yrs    10 icer.case.averted 2147.21   187.54  469.73
## 4  vaxbase   65+ yrs    10 icer.case.averted 4757.78   373.85 2618.42
## 5  vaxbase       All    10 icer.case.averted 2892.18   163.75  540.61
## 6  vaxbase   0-4 yrs    10 icer.daly.averted 2439.47   248.94 1892.40
```

```r
#write.csv(t1, "table-icer.csv")
```

# Table: Cases, deaths, DALYs per 100,000 population.

```r
t2 <- df3[c("scenario", "age", "risk", "v.eff", "cases.per100k", "deaths.per100k", "dalys.per100k")] %>%
  gather("cases.per100k", "deaths.per100k", "dalys.per100k", key = "metric", value = "value") %>%
  spread(risk, value) %>%
  arrange(scenario, v.eff, metric)
head(t2)
```

```
##   scenario       age v.eff        metric     High Non-high      All
## 1  vaxbase   0-4 yrs    10 cases.per100k 28151.45 28151.44 28151.44
## 2  vaxbase  5-19 yrs    10 cases.per100k 57611.67 57611.68 57611.67
## 3  vaxbase 20-64 yrs    10 cases.per100k 26871.41 26871.41 26871.41
## 4  vaxbase   65+ yrs    10 cases.per100k 18629.25 18629.25 18629.25
## 5  vaxbase       All    10 cases.per100k 26824.05 32941.38 31980.59
## 6  vaxbase   0-4 yrs    10 dalys.per100k 14204.25   323.30  1211.68
```

```r
#write.csv(t2, "table-rates.csv")
```

# Table: Cases, deaths, DALYs averted per 100,000

```r
t3 <- df3[c("scenario", "age", "risk", "v.eff", "cases.averted.per100k", "deaths.averted.per100k", "dalys.averted.per100k")] %>%
  gather("cases.averted.per100k", "deaths.averted.per100k", "dalys.averted.per100k", key = "metric", value = "value") %>%
  spread(risk, value) %>%
  arrange(scenario, v.eff, metric)
head(t3)
```

```
##   scenario       age v.eff                metric    High Non-high     All
## 1  vaxbase   0-4 yrs    10 cases.averted.per100k 5034.12  5034.11 5034.11
## 2  vaxbase  5-19 yrs    10 cases.averted.per100k 8014.99  8014.96 8014.98
## 3  vaxbase 20-64 yrs    10 cases.averted.per100k 3811.35  3811.35 3811.35
## 4  vaxbase   65+ yrs    10 cases.averted.per100k 3583.69  3583.68 3583.69
## 5  vaxbase       All    10 cases.averted.per100k 4098.46  4764.92 4660.26
## 6  vaxbase   0-4 yrs    10 dalys.averted.per100k 2540.04    57.81  216.68
```

```r
#write.csv(t3, "table-averted-rates.csv")
```


# FIGURES


```r
# subset by age group
age_group_df <- df3[(df3$risk=="All"),]

# subset by risk group
risk_group_df <- df3[(df3$risk!="All"),]
```

# Figure: ICERs.

```r
# icer per case averted
ggplot(age_group_df, aes(x = age, y = icer.case.averted, color = v.eff, group = v.eff)) + labs(color = "Vaccine \nefficacy") +
  geom_point() + geom_line(linetype = "dotted") + ylab("$ saved per case averted") +
  facet_grid(~ scenario) + ggtitle("ICER per case averted")
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

```r
# icer per death averted
ggplot(age_group_df, aes(x = age, y = icer.death.averted, color = v.eff, group = v.eff)) + labs(color = "Vaccine \nefficacy") +
  geom_point() + geom_line(linetype = "dotted") + ylab("$ saved per death averted") +
  facet_grid(~ scenario)+ ggtitle("ICER per death averted")
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-8-2.png)<!-- -->

```r
# icer per daly averted
ggplot(age_group_df, aes(x = age, y = icer.daly.averted, color = v.eff, group = v.eff)) + labs(color = "Vaccine \nefficacy") +
  geom_point() + geom_line(linetype = "dotted") + ylab("$ saved per DALY averted") +
  facet_grid(~ scenario) + ggtitle("ICER per DALY averted")
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-8-3.png)<!-- -->

```r
## Alternatively:
# icer per case averted
ggplot(age_group_df, aes(x = age, y = icer.case.averted, color = v.eff, group = v.eff)) + labs(color = "Vaccine \nefficacy") +
  geom_point() + geom_line(linetype = "dotted") + ylab("$ saved per case averted") +
  facet_grid(v.eff ~ scenario) + ggtitle("ICER per case averted")
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-8-4.png)<!-- -->

```r
# icer per death averted
ggplot(age_group_df, aes(x = age, y = icer.death.averted, color = v.eff, group = v.eff)) + labs(color = "Vaccine \nefficacy") +
  geom_point() + geom_line(linetype = "dotted") + ylab("$ saved per death averted") +
  facet_grid(v.eff ~ scenario) + ggtitle("ICER per death averted")
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-8-5.png)<!-- -->

```r
# icer per DALY averted
ggplot(age_group_df, aes(x = age, y = icer.daly.averted, color = v.eff, group = v.eff)) + labs(color = "Vaccine \nefficacy") +
  geom_point() + geom_line(linetype = "dotted") + ylab("$ saved per DALY averted") +
  facet_grid(v.eff ~ scenario) + ggtitle("ICER per DALY averted")
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-8-6.png)<!-- -->

# Figures: Cases.

```r
# cases
ggplot(age_group_df, aes(x = age, y = cases, color = v.eff, group = v.eff)) + geom_point() + geom_line(linetype = "dotted") +
  ggtitle("Number of cases in each age group by vaccine efficacy") + facet_grid(~scenario)
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

```r
ggplot(age_group_df, aes(x = age, y = cases, color = v.eff, group = v.eff)) + labs(color = "Vaccine \nefficacy") +
  geom_point() + geom_line(linetype = "dotted") + ylab("cases") +
  facet_grid(v.eff~scenario) + ggtitle("Number of cases in each age group by vaccine efficacy")
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-9-2.png)<!-- -->

```r
ggplot(df3, aes(x = age, y = cases, color = risk, group = risk)) + labs(color = "Risk \ngroup") +
  geom_point() + geom_line(linetype = "dotted") + ylab("cases") +
  facet_grid(v.eff~scenario) + ggtitle("Number of cases in each age and risk group by vaccine efficacy")
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-9-3.png)<!-- -->

```r
ggplot(age_group_df, aes(x = age, y = cases, color = v.eff, group = v.eff)) + labs(color = "Vaccine \nefficacy") +
  geom_point() + geom_line(linetype = "dotted") + ylab("cases") +
  facet_grid(scenario~v.eff) + ggtitle("Number of cases in each age group by vaccine efficacy")
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-9-4.png)<!-- -->

```r
ggplot(df3, aes(x = age, y = cases, color = risk, group = risk)) + labs(color = "Vaccine \nefficacy") +
  geom_point() + geom_line(linetype = "dotted") + ylab("cases") +
  facet_grid(scenario~v.eff) + ggtitle("Number of cases in each age group by vaccine efficacy")
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-9-5.png)<!-- -->

# Figures: Deaths per 100k

```r
# deaths
ggplot(age_group_df, aes(x = age, y = deaths.per100k, color = v.eff, group = v.eff)) + geom_point() + geom_line(linetype = "dotted") + ggtitle("Deaths per 100k in each age group by vaccine efficacy") + facet_grid(~scenario)
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

```r
ggplot(age_group_df, aes(x = age, y = deaths.per100k, color = v.eff, group = v.eff)) + labs(color = "Vaccine \nefficacy") +
  geom_point() + geom_line(linetype = "dotted") + ylab("deaths per 100k") +
  facet_grid(v.eff~scenario) + ggtitle("Deaths per 100k in each age group by vaccine efficacy")
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-10-2.png)<!-- -->

```r
ggplot(df3, aes(x = age, y = deaths.per100k, color = risk, group = risk)) + labs(color = "Risk \ngroup") +
  geom_point() + geom_line(linetype = "dotted") + ylab("deaths per 100k") +
  facet_grid(v.eff~scenario) + ggtitle("Deaths per 100k in each age and risk group by vaccine efficacy")
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-10-3.png)<!-- -->

```r
ggplot(age_group_df, aes(x = age, y = deaths.per100k, color = v.eff, group = v.eff)) + labs(color = "Vaccine \nefficacy") +
  geom_point() + geom_line(linetype = "dotted") + ylab("deaths per 100k") +
  facet_grid(scenario~v.eff) + ggtitle("Deaths per 100k in each age group by vaccine efficacy")
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-10-4.png)<!-- -->

```r
ggplot(df3, aes(x = age, y = deaths.per100k, color = risk, group = risk)) + labs(color = "Vaccine \nefficacy") +
  geom_point() + geom_line(linetype = "dotted") + ylab("deaths per 100k") +
  facet_grid(scenario~v.eff) + ggtitle("Deaths per 100k in each age group by vaccine efficacy")
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-10-5.png)<!-- -->

# Figures: DALYs per 100k

```r
# DALYs per 100k
ggplot(age_group_df, aes(x = age, y = dalys.per100k, color = v.eff, group = v.eff)) + geom_point() + geom_line(linetype = "dotted") + ggtitle("DALYs per 100k in each age group by vaccine efficacy") + facet_grid(~scenario)
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

```r
ggplot(age_group_df, aes(x = age, y = dalys.per100k, color = v.eff, group = v.eff)) + labs(color = "Vaccine \nefficacy") +
  geom_point() + geom_line(linetype = "dotted") + ylab("DALYs per 100k") +
  facet_grid(v.eff~scenario) + ggtitle("DALYs per 100k in each age group by vaccine efficacy")
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-11-2.png)<!-- -->

```r
ggplot(df3, aes(x = age, y = dalys.per100k, color = risk, group = risk)) + labs(color = "Risk \ngroup") +
  geom_point() + geom_line(linetype = "dotted") + ylab("DALYs per 100k") +
  facet_grid(v.eff~scenario) + ggtitle("DALYs per 100k in each age and risk group by vaccine efficacy")
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-11-3.png)<!-- -->

```r
ggplot(age_group_df, aes(x = age, y = dalys.per100k, color = v.eff, group = v.eff)) + labs(color = "Vaccine \nefficacy") +
  geom_point() + geom_line(linetype = "dotted") + ylab("DALYs per 100k") +
  facet_grid(scenario~v.eff) + ggtitle("DALYs per 100k in each age group by vaccine efficacy")
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-11-4.png)<!-- -->

```r
ggplot(df3, aes(x = age, y = dalys.per100k, color = risk, group = risk)) + labs(color = "Vaccine \nefficacy") +
  geom_point() + geom_line(linetype = "dotted") + ylab("DALYs per 100k") +
  facet_grid(scenario~v.eff) + ggtitle("DALYs per 100k in each age group by vaccine efficacy")
```

![](tabs-and-figs_files/figure-html/unnamed-chunk-11-5.png)<!-- -->








