---
title: "Prevalent vax vs. No vax (40% VE)"
output: 
  html_document: 
    fig_caption: yes
    keep_md: yes
    number_sections: yes
    theme: cosmo
    toc: yes
  html_notebook: 
    code_folding: hide
    fig_caption: yes
    number_sections: yes
    theme: cosmo
    toc: yes
editor_options: 
  chunk_output_type: inline
---




```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```





# Tables



# Subset data.



## 40% VE: ICER per case averted
![](tabs-and-figs-v1_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

## 40% VE: ICER per death averted
![](tabs-and-figs-v1_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

## 40% VE: ICER per DALY averted
![](tabs-and-figs-v1_files/figure-html/unnamed-chunk-8-1.png)<!-- -->


## 40% VE: Cases, cases averted
![](tabs-and-figs-v1_files/figure-html/unnamed-chunk-9-1.png)<!-- -->![](tabs-and-figs-v1_files/figure-html/unnamed-chunk-9-2.png)<!-- -->![](tabs-and-figs-v1_files/figure-html/unnamed-chunk-9-3.png)<!-- -->![](tabs-and-figs-v1_files/figure-html/unnamed-chunk-9-4.png)<!-- -->

## 40% VE: Deaths, deaths averted
![](tabs-and-figs-v1_files/figure-html/unnamed-chunk-10-1.png)<!-- -->![](tabs-and-figs-v1_files/figure-html/unnamed-chunk-10-2.png)<!-- -->![](tabs-and-figs-v1_files/figure-html/unnamed-chunk-10-3.png)<!-- -->![](tabs-and-figs-v1_files/figure-html/unnamed-chunk-10-4.png)<!-- -->

## 40% VE: DALYs, DALYs averted
![](tabs-and-figs-v1_files/figure-html/unnamed-chunk-11-1.png)<!-- -->![](tabs-and-figs-v1_files/figure-html/unnamed-chunk-11-2.png)<!-- -->![](tabs-and-figs-v1_files/figure-html/unnamed-chunk-11-3.png)<!-- -->![](tabs-and-figs-v1_files/figure-html/unnamed-chunk-11-4.png)<!-- -->

## Fig. Cost-effectiveness plane (40% VE)

![](tabs-and-figs-v1_files/figure-html/unnamed-chunk-12-1.png)<!-- -->![](tabs-and-figs-v1_files/figure-html/unnamed-chunk-12-2.png)<!-- -->


# Prevalent vax vs. No vax (All VE)
## Fig. ICER per case averted

![](tabs-and-figs-v1_files/figure-html/unnamed-chunk-13-1.png)<!-- -->![](tabs-and-figs-v1_files/figure-html/unnamed-chunk-13-2.png)<!-- -->![](tabs-and-figs-v1_files/figure-html/unnamed-chunk-13-3.png)<!-- -->

## Fig. ICER per death averted

![](tabs-and-figs-v1_files/figure-html/unnamed-chunk-14-1.png)<!-- -->![](tabs-and-figs-v1_files/figure-html/unnamed-chunk-14-2.png)<!-- -->![](tabs-and-figs-v1_files/figure-html/unnamed-chunk-14-3.png)<!-- -->

## Fig. ICER per DALY averted

![](tabs-and-figs-v1_files/figure-html/unnamed-chunk-15-1.png)<!-- -->![](tabs-and-figs-v1_files/figure-html/unnamed-chunk-15-2.png)<!-- -->![](tabs-and-figs-v1_files/figure-html/unnamed-chunk-15-3.png)<!-- -->


## Set up plots.



## Fig. Cases, cases averted

![](tabs-and-figs-v1_files/figure-html/unnamed-chunk-17-1.png)<!-- -->![](tabs-and-figs-v1_files/figure-html/unnamed-chunk-17-2.png)<!-- -->![](tabs-and-figs-v1_files/figure-html/unnamed-chunk-17-3.png)<!-- -->![](tabs-and-figs-v1_files/figure-html/unnamed-chunk-17-4.png)<!-- -->![](tabs-and-figs-v1_files/figure-html/unnamed-chunk-17-5.png)<!-- -->![](tabs-and-figs-v1_files/figure-html/unnamed-chunk-17-6.png)<!-- -->![](tabs-and-figs-v1_files/figure-html/unnamed-chunk-17-7.png)<!-- -->![](tabs-and-figs-v1_files/figure-html/unnamed-chunk-17-8.png)<!-- -->![](tabs-and-figs-v1_files/figure-html/unnamed-chunk-17-9.png)<!-- -->![](tabs-and-figs-v1_files/figure-html/unnamed-chunk-17-10.png)<!-- -->![](tabs-and-figs-v1_files/figure-html/unnamed-chunk-17-11.png)<!-- -->![](tabs-and-figs-v1_files/figure-html/unnamed-chunk-17-12.png)<!-- -->

## Fig. Deaths, deaths averted

![](tabs-and-figs-v1_files/figure-html/unnamed-chunk-18-1.png)<!-- -->![](tabs-and-figs-v1_files/figure-html/unnamed-chunk-18-2.png)<!-- -->![](tabs-and-figs-v1_files/figure-html/unnamed-chunk-18-3.png)<!-- -->![](tabs-and-figs-v1_files/figure-html/unnamed-chunk-18-4.png)<!-- -->![](tabs-and-figs-v1_files/figure-html/unnamed-chunk-18-5.png)<!-- -->![](tabs-and-figs-v1_files/figure-html/unnamed-chunk-18-6.png)<!-- -->![](tabs-and-figs-v1_files/figure-html/unnamed-chunk-18-7.png)<!-- -->![](tabs-and-figs-v1_files/figure-html/unnamed-chunk-18-8.png)<!-- -->![](tabs-and-figs-v1_files/figure-html/unnamed-chunk-18-9.png)<!-- -->![](tabs-and-figs-v1_files/figure-html/unnamed-chunk-18-10.png)<!-- -->![](tabs-and-figs-v1_files/figure-html/unnamed-chunk-18-11.png)<!-- -->![](tabs-and-figs-v1_files/figure-html/unnamed-chunk-18-12.png)<!-- -->

## Fig. DALYs, DALYs averted

![](tabs-and-figs-v1_files/figure-html/unnamed-chunk-19-1.png)<!-- -->![](tabs-and-figs-v1_files/figure-html/unnamed-chunk-19-2.png)<!-- -->![](tabs-and-figs-v1_files/figure-html/unnamed-chunk-19-3.png)<!-- -->![](tabs-and-figs-v1_files/figure-html/unnamed-chunk-19-4.png)<!-- -->![](tabs-and-figs-v1_files/figure-html/unnamed-chunk-19-5.png)<!-- -->![](tabs-and-figs-v1_files/figure-html/unnamed-chunk-19-6.png)<!-- -->![](tabs-and-figs-v1_files/figure-html/unnamed-chunk-19-7.png)<!-- -->![](tabs-and-figs-v1_files/figure-html/unnamed-chunk-19-8.png)<!-- -->![](tabs-and-figs-v1_files/figure-html/unnamed-chunk-19-9.png)<!-- -->![](tabs-and-figs-v1_files/figure-html/unnamed-chunk-19-10.png)<!-- -->![](tabs-and-figs-v1_files/figure-html/unnamed-chunk-19-11.png)<!-- -->![](tabs-and-figs-v1_files/figure-html/unnamed-chunk-19-12.png)<!-- -->

## Fig. Cost-effectiveness plane (All VE)

![](tabs-and-figs-v1_files/figure-html/unnamed-chunk-20-1.png)<!-- -->![](tabs-and-figs-v1_files/figure-html/unnamed-chunk-20-2.png)<!-- -->
