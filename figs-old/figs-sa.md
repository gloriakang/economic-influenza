---
title: "figures"
output: 
  html_document: 
    css: ~/git/economic-influenza/css/default.css
    fig_caption: yes
    fig_width: 7.5
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
library(ggplot2)
library(tidyr)
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

# subset by age group
age_group_df <- df3[(df3$risk=="All"),]

# subset by risk group
risk_group_df <- df3[(df3$risk!="All"),]
```

# ICER sensitivity analysis


```r
# icer per case averted
ggplot(age_group_df, aes(x = age, y = icer.case.averted, color = v.eff, group = v.eff)) + labs(color = "Vaccine \nefficacy") +
  geom_point() + geom_line(linetype = "dotted") + ylab("$ saved per case averted") +
  facet_grid(~ scenario) + ggtitle("ICER per case averted")
```

![](figs-sa_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
# icer per death averted
ggplot(age_group_df, aes(x = age, y = icer.death.averted, color = v.eff, group = v.eff)) + labs(color = "Vaccine \nefficacy") +
  geom_point() + geom_line(linetype = "dotted") + ylab("$ saved per death averted") +
  facet_grid(~ scenario)+ ggtitle("ICER per death averted")
```

![](figs-sa_files/figure-html/unnamed-chunk-4-2.png)<!-- -->

```r
# icer per daly averted
ggplot(age_group_df, aes(x = age, y = icer.daly.averted, color = v.eff, group = v.eff)) + labs(color = "Vaccine \nefficacy") +
  geom_point() + geom_line(linetype = "dotted") + ylab("$ saved per DALY averted") +
  facet_grid(~ scenario) + ggtitle("ICER per DALY averted")
```

![](figs-sa_files/figure-html/unnamed-chunk-4-3.png)<!-- -->


```r
# icer per case averted
ggplot(age_group_df, aes(x = age, y = icer.case.averted, color = v.eff, group = v.eff)) + labs(color = "Vaccine \nefficacy") +
  geom_point() + geom_line(linetype = "dotted") + ylab("$ saved per case averted") +
  facet_grid(v.eff ~ scenario) + ggtitle("ICER per case averted")
```

![](figs-sa_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

```r
# icer per death averted
ggplot(age_group_df, aes(x = age, y = icer.death.averted, color = v.eff, group = v.eff)) + labs(color = "Vaccine \nefficacy") +
  geom_point() + geom_line(linetype = "dotted") + ylab("$ saved per death averted") +
  facet_grid(v.eff ~ scenario) + ggtitle("ICER per death averted")
```

![](figs-sa_files/figure-html/unnamed-chunk-5-2.png)<!-- -->

```r
# icer per DALY averted
ggplot(age_group_df, aes(x = age, y = icer.daly.averted, color = v.eff, group = v.eff)) + labs(color = "Vaccine \nefficacy") +
  geom_point() + geom_line(linetype = "dotted") + ylab("$ saved per DALY averted") +
  facet_grid(v.eff ~ scenario) + ggtitle("ICER per DALY averted")
```

![](figs-sa_files/figure-html/unnamed-chunk-5-3.png)<!-- -->


# Vax base scenario


```r
# subset vax base data
vaxbase_df <- df3[(df3$scenario=="vaxbase"),]
vaxbase_df_risk <- risk_group_df[(risk_group_df$scenario=="vaxbase"),]
vaxbase_df_age <- age_group_df[(age_group_df$scenario=="vaxbase"),]
```

## Cases


```r
ggplot(vaxbase_df_age, aes(x = age, y = cases, color = v.eff, group = v.eff)) + geom_point() + geom_line(linetype = "dotted") +
  ggtitle("Number of cases in each age group by vaccine efficacy")
```

![](figs-sa_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

```r
ggplot(vaxbase_df_age, aes(x = age, y = cases, color = v.eff, group = v.eff)) + labs(color = "Vaccine \nefficacy") +
  geom_point() + geom_line(linetype = "dotted") + ylab("cases") +
  facet_grid(~v.eff) + ggtitle("Number of cases in each age group by vaccine efficacy")
```

![](figs-sa_files/figure-html/unnamed-chunk-7-2.png)<!-- -->

```r
ggplot(vaxbase_df, aes(x = age, y = cases, color = risk, group = risk)) + labs(color = "Risk \ngroup") +
  geom_point() + geom_line(linetype = "dotted") + ylab("cases") +
  facet_grid(~v.eff) + ggtitle("Number of cases in each age and risk group by vaccine efficacy")
```

![](figs-sa_files/figure-html/unnamed-chunk-7-3.png)<!-- -->

```r
ggplot(vaxbase_df, aes(x = age, y = cases, color = risk, group = risk)) + labs(color = "Risk \ngroup") +
  geom_point() + geom_line(linetype = "dotted") + ylab("cases") +
  facet_grid(v.eff~risk) + ggtitle("Number of cases in each age and risk group by vaccine efficacy")
```

![](figs-sa_files/figure-html/unnamed-chunk-7-4.png)<!-- -->

## Deaths per 100k


```r
ggplot(vaxbase_df_age, aes(x = age, y = deaths.per100k, color = v.eff, group = v.eff)) + geom_point() + geom_line(linetype = "dotted") +
  ggtitle("Deaths per 100k in each age group by vaccine efficacy")
```

![](figs-sa_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

```r
ggplot(vaxbase_df_age, aes(x = age, y = deaths.per100k, color = v.eff, group = v.eff)) + labs(color = "Vaccine \nefficacy") +
  geom_point() + geom_line(linetype = "dotted") + ylab("deaths") +
  facet_grid(~v.eff) + ggtitle("Deaths per 100k in each age group by vaccine efficacy")
```

![](figs-sa_files/figure-html/unnamed-chunk-8-2.png)<!-- -->

```r
ggplot(vaxbase_df, aes(x = age, y = deaths.per100k, color = risk, group = risk)) + labs(color = "Risk \ngroup") +
  geom_point() + geom_line(linetype = "dotted") + ylab("deaths") +
  facet_grid(~v.eff) + ggtitle("Deaths per 100k in each age and risk group by vaccine efficacy")
```

![](figs-sa_files/figure-html/unnamed-chunk-8-3.png)<!-- -->

```r
ggplot(vaxbase_df, aes(x = age, y = deaths.per100k, color = risk, group = risk)) + labs(color = "Risk \ngroup") +
  geom_point() + geom_line(linetype = "dotted") + ylab("deaths") +
  facet_grid(v.eff~risk) + ggtitle("Deaths per 100k in each age and risk group by vaccine efficacy")
```

![](figs-sa_files/figure-html/unnamed-chunk-8-4.png)<!-- -->

## DALYs per 100k


```r
ggplot(vaxbase_df_age, aes(x = age, y = dalys.per100k, color = v.eff, group = v.eff)) + geom_point() + geom_line(linetype = "dotted") +
  ggtitle("DALYs per 100k in each age group by vaccine efficacy")
```

![](figs-sa_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

```r
ggplot(vaxbase_df_age, aes(x = age, y = dalys.per100k, color = v.eff, group = v.eff)) + labs(color = "Vaccine \nefficacy") +
  geom_point() + geom_line(linetype = "dotted") + ylab("DALYs") +
  facet_grid(~v.eff) + ggtitle("DALYs per 100k in each age group by vaccine efficacy")
```

![](figs-sa_files/figure-html/unnamed-chunk-9-2.png)<!-- -->

```r
ggplot(vaxbase_df, aes(x = age, y = dalys.per100k, color = risk, group = risk)) + labs(color = "Risk \ngroup") +
  geom_point() + geom_line(linetype = "dotted") + ylab("DALYs") +
  facet_grid(~v.eff) + ggtitle("DALYs per 100k in each age and risk group by vaccine efficacy")
```

![](figs-sa_files/figure-html/unnamed-chunk-9-3.png)<!-- -->

```r
ggplot(vaxbase_df, aes(x = age, y = dalys.per100k, color = risk, group = risk)) + labs(color = "Risk \ngroup") +
  geom_point() + geom_line(linetype = "dotted") + ylab("DALYs") +
  facet_grid(v.eff~risk) + ggtitle("DALYs per 100k in each age and risk group by vaccine efficacy")
```

![](figs-sa_files/figure-html/unnamed-chunk-9-4.png)<!-- -->


# Vax 70% scenario


```r
# subset vax70% data
vax70_df <- df3[(df3$scenario=="vax70"),]
vax70_df_risk <- risk_group_df[(risk_group_df$scenario=="vax70"),]
vax70_df_age <- age_group_df[(age_group_df$scenario=="vax70"),]
```

## Cases


```r
ggplot(vax70_df_age, aes(x = age, y = cases, color = v.eff, group = v.eff)) + geom_point() + geom_line(linetype = "dotted") +
  ggtitle("Number of cases in each age group by vaccine efficacy")
```

![](figs-sa_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

```r
ggplot(vax70_df_age, aes(x = age, y = cases, color = v.eff, group = v.eff)) + labs(color = "Vaccine \nefficacy") +
  geom_point() + geom_line(linetype = "dotted") + ylab("cases") +
  facet_grid(~v.eff) + ggtitle("Number of cases in each age group by vaccine efficacy")
```

![](figs-sa_files/figure-html/unnamed-chunk-11-2.png)<!-- -->

```r
ggplot(vax70_df, aes(x = age, y = cases, color = risk, group = risk)) + labs(color = "Risk \ngroup") +
  geom_point() + geom_line(linetype = "dotted") + ylab("cases") +
  facet_grid(~v.eff) + ggtitle("Number of cases in each age and risk group by vaccine efficacy")
```

![](figs-sa_files/figure-html/unnamed-chunk-11-3.png)<!-- -->

```r
ggplot(vax70_df, aes(x = age, y = cases, color = risk, group = risk)) + labs(color = "Risk \ngroup") +
  geom_point() + geom_line(linetype = "dotted") + ylab("cases") +
  facet_grid(v.eff~risk) + ggtitle("Number of cases in each age and risk group by vaccine efficacy")
```

![](figs-sa_files/figure-html/unnamed-chunk-11-4.png)<!-- -->

## Deaths per 100k


```r
ggplot(vax70_df_age, aes(x = age, y = deaths.per100k, color = v.eff, group = v.eff)) + geom_point() + geom_line(linetype = "dotted") +
  ggtitle("Deaths per 100k in each age group by vaccine efficacy")
```

![](figs-sa_files/figure-html/unnamed-chunk-12-1.png)<!-- -->

```r
ggplot(vax70_df_age, aes(x = age, y = deaths.per100k, color = v.eff, group = v.eff)) + labs(color = "Vaccine \nefficacy") +
  geom_point() + geom_line(linetype = "dotted") + ylab("deaths") +
  facet_grid(~v.eff) + ggtitle("Deaths per 100k in each age group by vaccine efficacy")
```

![](figs-sa_files/figure-html/unnamed-chunk-12-2.png)<!-- -->

```r
ggplot(vax70_df, aes(x = age, y = deaths.per100k, color = risk, group = risk)) + labs(color = "Risk \ngroup") +
  geom_point() + geom_line(linetype = "dotted") + ylab("deaths") +
  facet_grid(~v.eff) + ggtitle("Deaths per 100k in each age and risk group by vaccine efficacy")
```

![](figs-sa_files/figure-html/unnamed-chunk-12-3.png)<!-- -->

```r
ggplot(vax70_df, aes(x = age, y = deaths.per100k, color = risk, group = risk)) + labs(color = "Risk \ngroup") +
  geom_point() + geom_line(linetype = "dotted") + ylab("deaths") +
  facet_grid(v.eff~risk) + ggtitle("Deaths per 100k in each age and risk group by vaccine efficacy")
```

![](figs-sa_files/figure-html/unnamed-chunk-12-4.png)<!-- -->

# DALYs per 100k


```r
ggplot(vax70_df_age, aes(x = age, y = dalys.per100k, color = v.eff, group = v.eff)) + geom_point() + geom_line(linetype = "dotted") +
  ggtitle("DALYs per 100k in each age group by vaccine efficacy")
```

![](figs-sa_files/figure-html/unnamed-chunk-13-1.png)<!-- -->

```r
ggplot(vax70_df_age, aes(x = age, y = dalys.per100k, color = v.eff, group = v.eff)) + labs(color = "Vaccine \nefficacy") +
  geom_point() + geom_line(linetype = "dotted") + ylab("DALYs") +
  facet_grid(~v.eff) + ggtitle("DALYs per 100k in each age group by vaccine efficacy")
```

![](figs-sa_files/figure-html/unnamed-chunk-13-2.png)<!-- -->

```r
ggplot(vax70_df, aes(x = age, y = dalys.per100k, color = risk, group = risk)) + labs(color = "Risk \ngroup") +
  geom_point() + geom_line(linetype = "dotted") + ylab("DALYs") +
  facet_grid(~v.eff) + ggtitle("DALYs per 100k in each age and risk group by vaccine efficacy")
```

![](figs-sa_files/figure-html/unnamed-chunk-13-3.png)<!-- -->

```r
ggplot(vax70_df, aes(x = age, y = dalys.per100k, color = risk, group = risk)) + labs(color = "Risk \ngroup") +
  geom_point() + geom_line(linetype = "dotted") + ylab("DALYs") +
  facet_grid(v.eff~risk) + ggtitle("DALYs per 100k in each age and risk group by vaccine efficacy")
```

![](figs-sa_files/figure-html/unnamed-chunk-13-4.png)<!-- -->




