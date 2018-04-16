---
title: "scratch"
output: 
  html_document: 
    fig_caption: yes
    keep_md: yes
    self_contained: no
editor_options: 
  chunk_output_type: inline
---




```r
rm(list = ls(all.names = TRUE))
```


```r
source("code/functions.R")
source("input/var.R")
```

```
## Loading required package: mvtnorm
```

```
## 
## Attaching package: 'mc2d'
```

```
## The following objects are masked from 'package:base':
## 
##     pmax, pmin
```

```r
source("input/input_base.R")
```


```r
## select list of parameters
source("input/input_vaxbase_40.R")  # base vax 51,51,33,63
#source("input/input_6667_40.R")  # vax 60,60,60,70
#source("input/input_7777_40.R")  # vax 70,70,70,70
#source("input/input_7778_40.R")  # vax 70,70,70,80

## sensitivity analysis
#source("input/input_vaxbase_30.R")
#source("input/input_6667_30.R")
#source("input/input_7777_30.R")
#source("input/input_7778_30.R")
```

###### 0-4 years ######


```r
pop <- pop_04
num_cases <- base_04
cases_after_vax <- intv_04
vax_comp_i <- vc_04
```

## 0-4 years, HIGH

```r
risk_group <- high_019
prob <- c(p_04_high_death, p_04_high_hosp, p_04_high_out, p_04_high_rest)
r_cost <- rc_04_high

# total vaccination cost for age/risk
total_cost_vax_b <- calc_vaccination_cost(vax_comp_b)  # base
total_cost_vax_i <- calc_vaccination_cost(vax_comp_i)  # intv

# cost per health outcome
costs <- cost_cpi_outcome(r_cost[1], r_cost[2], r_cost[3], r_cost[4])

# base summary
b_cases <- calc_subpop_cases_base(num_cases, risk_group)  # base
num_outcomes_base <- calc_num_outcomes_base(prob[1], prob[2], prob[3], prob[4])  # base
```

```
## [1] "base num: deaths = 16.026268, hosps = 33.748173, outps = 2198.969617, rest = 214.449542"
```

```r
cc_base <- calc_total_cost_base(r_cost[1],r_cost[2],r_cost[3],r_cost[4])  # base
```

```
## [1] "base cc: deaths = 997131.072105, hosps = 954947.598040, outps = 1792825.781730, rest = 1159.809504"
## [1] "base total clin costs = 3746064.261380"
```

```r
# intv summary
i_cases <- calc_subpop_cases_intv(cases_after_vax, risk_group)  # intv
num_outcomes_intv <- calc_num_outcomes_intv(prob[1], prob[2], prob[3], prob[4])  # intv
```

```
## [1] "intv num: deaths = 8.074337, hosps = 17.002968, outps = 1107.882480, rest = 108.043735"
```

```r
cc_intv <- calc_total_cost_intv(r_cost[1],r_cost[2],r_cost[3],r_cost[4])  # intv
```

```
## [1] "intv cc: deaths = 502373.492032, hosps = 481120.660018, outps = 903259.535050, rest = 584.333963"
## [1] "intv total clin costs = 1887338.021063"
```

```r
print(sprintf("total vax costs = %f", total_cost_vax_i))
```

```
## [1] "total vax costs = 208884.933734"
```

```r
print(sprintf("base cases = %f", b_cases))
```

```
## [1] "base cases = 2463.193600"
```

```r
print(sprintf("intv cases = %f", i_cases))
```

```
## [1] "intv cases = 1241.003520"
```

```r
icer_04_high <- calc_icer()
```

```
## [1] "intv net costs = 2096222.954797"
## [1] "cost difference = 1649841.306583"
## [1] "cases averted = 1222.190080"
## [1] "icer = 1349.90565999576"
```

```r
deaths_04_high <- calc_deaths_b()
```

```
## [1] "deaths averted = 7.951931"
## [1] "cost per death averted = 207476.812047"
```

## 0-4 years, low risk

```r
risk_group <- 1-high_019
prob <- c(p_04_low_death, p_04_low_hosp, p_04_low_out, p_04_low_rest)
r_cost <- rc_04_low

# total vaccination cost for age/risk
total_cost_vax_b <- calc_vaccination_cost(vax_comp_b)  # base
total_cost_vax_i <- calc_vaccination_cost(vax_comp_i)  # intv

# cost per health outcome
costs <- cost_cpi_outcome(r_cost[1], r_cost[2], r_cost[3], r_cost[4])

# base summary
b_cases <- calc_subpop_cases_base(num_cases, risk_group)  # base
num_outcomes_base <- calc_num_outcomes_base(prob[1], prob[2], prob[3], prob[4])  # base
```

```
## [1] "base num: deaths = 4.889847, hosps = 134.861363, outps = 18358.876704, rest = 17525.578485"
```

```r
cc_base <- calc_total_cost_base(r_cost[1],r_cost[2],r_cost[3],r_cost[4])  # base
```

```
## [1] "base cc: deaths = 326202.429510, hosps = 1712200.756789, outps = 8414868.846155, rest = 94783.753160"
## [1] "base total clin costs = 10548055.785614"
```

```r
# intv summary
i_cases <- calc_subpop_cases_intv(cases_after_vax, risk_group)  # intv
num_outcomes_intv <- calc_num_outcomes_intv(prob[1], prob[2], prob[3], prob[4])  # intv
```

```
## [1] "intv num: deaths = 2.463598, hosps = 67.945705, outps = 9249.549290, rest = 8829.717888"
```

```r
cc_intv <- calc_total_cost_intv(r_cost[1],r_cost[2],r_cost[3],r_cost[4])  # intv
```

```
## [1] "intv cc: deaths = 164346.953181, hosps = 862639.122691, outps = 4239570.068068, rest = 47753.847408"
## [1] "intv total clin costs = 5314309.991347"
```

```r
print(sprintf("total vax costs = %f", total_cost_vax_i))
```

```
## [1] "total vax costs = 3054942.155866"
```

```r
print(sprintf("base cases = %f", b_cases))
```

```
## [1] "base cases = 36024.206400"
```

```r
print(sprintf("intv cases = %f", i_cases))
```

```
## [1] "intv cases = 18149.676480"
```

```r
icer_04_low <- calc_icer()
```

```
## [1] "intv net costs = 8369252.147213"
## [1] "cost difference = 2178803.638401"
## [1] "cases averted = 17874.529920"
## [1] "icer = 121.894318236741"
```

```r
deaths_04_low <- calc_deaths_b()
```

```
## [1] "deaths averted = 2.426250"
## [1] "cost per death averted = 898012.919271"
```


###### 5-19 years ######


```r
pop <- pop_519
num_cases <- base_519
cases_after_vax <- intv_519
vax_comp_i <- vc_519
```

## 5-19 years, HIGH

```r
risk_group <- high_019
prob <- c(p_519_high_death, p_519_high_hosp, p_519_high_out, p_519_high_rest)
r_cost <- rc_519_high

total_cost_vax_i <- calc_vaccination_cost(vax_comp_i)  # intv

costs <- cost_cpi_outcome(r_cost[1], r_cost[2], r_cost[3], r_cost[4])

b_cases <- calc_subpop_cases_base(num_cases, risk_group)  # base
num_outcomes_base <- calc_num_outcomes_base(prob[1], prob[2], prob[3], prob[4])  # base
```

```
## [1] "base num: deaths = 110.403573, hosps = 232.133991, outps = 8885.630004, rest = 7708.088432"
```

```r
cc_base <- calc_total_cost_base(r_cost[1],r_cost[2],r_cost[3],r_cost[4])  # base
```

```
## [1] "base cc: deaths = 28074143.098881, hosps = 12651822.345067, outps = 10127852.041018, rest = 41687.728131"
## [1] "base total clin costs = 50895505.213097"
```

```r
i_cases <- calc_subpop_cases_intv(cases_after_vax, risk_group)  # intv
num_outcomes_intv <- calc_num_outcomes_intv(prob[1], prob[2], prob[3], prob[4])  # intv
```

```
## [1] "intv num: deaths = 57.298181, hosps = 120.474865, outps = 4611.539527, rest = 4000.409027"
```

```r
cc_intv <- calc_total_cost_intv(r_cost[1],r_cost[2],r_cost[3],r_cost[4])  # intv
```

```
## [1] "intv cc: deaths = 14570156.591736, hosps = 6566149.929819, outps = 5256238.441751, rest = 21635.450268"
## [1] "intv total clin costs = 26414180.413574"
```

```r
print(sprintf("total vax costs = %f", total_cost_vax_i))
```

```
## [1] "total vax costs = 597543.672845"
```

```r
print(sprintf("base cases = %f", b_cases))
```

```
## [1] "base cases = 16936.256000"
```

```r
print(sprintf("intv cases = %f", i_cases))
```

```
## [1] "intv cases = 8789.721600"
```

```r
icer_519_high <- calc_icer()
```

```
## [1] "intv net costs = 27011724.086419"
## [1] "cost difference = 23883781.126678"
## [1] "cases averted = 8146.534400"
## [1] "icer = 2931.77196019431"
```

```r
deaths_519_high <- calc_deaths_b()
```

```
## [1] "deaths averted = 53.105391"
## [1] "cost per death averted = 449743.058607"
```

## 5-19 years, low risk

```r
risk_group <- 1-high_019
prob <- c(p_519_low_death, p_519_low_hosp, p_519_low_out, p_519_low_rest)
r_cost <- rc_519_low

total_cost_vax_i <- calc_vaccination_cost(vax_comp_i)  # intv

costs <- cost_cpi_outcome(r_cost[1], r_cost[2], r_cost[3], r_cost[4])

b_cases <- calc_subpop_cases_base(num_cases, risk_group)  # base
num_outcomes_base <- calc_num_outcomes_base(prob[1], prob[2], prob[3], prob[4])  # base
```

```
## [1] "base num: deaths = 33.647786, hosps = 924.931066, outps = 126226.782983, rest = 120507.382165"
```

```r
cc_base <- calc_total_cost_base(r_cost[1],r_cost[2],r_cost[3],r_cost[4])  # base
```

```
## [1] "base cc: deaths = 7601479.052977, hosps = 24083638.341882, outps = 47275140.808910, rest = 651741.223529"
## [1] "base total clin costs = 79611999.427299"
```

```r
i_cases <- calc_subpop_cases_intv(cases_after_vax, risk_group)  # intv
num_outcomes_intv <- calc_num_outcomes_intv(prob[1], prob[2], prob[3], prob[4])  # intv
```

```
## [1] "intv num: deaths = 17.462813, hosps = 480.028559, outps = 65510.245056, rest = 62541.941972"
```

```r
cc_intv <- calc_total_cost_intv(r_cost[1],r_cost[2],r_cost[3],r_cost[4])  # intv
```

```
## [1] "intv cc: deaths = 3945079.988393, hosps = 12499130.630774, outps = 24535253.028244, rest = 338246.180860"
## [1] "intv total clin costs = 41317709.828271"
```

```r
print(sprintf("total vax costs = %f", total_cost_vax_i))
```

```
## [1] "total vax costs = 8739076.215355"
```

```r
print(sprintf("base cases = %f", b_cases))
```

```
## [1] "base cases = 247692.744000"
```

```r
print(sprintf("intv cases = %f", i_cases))
```

```
## [1] "intv cases = 128549.678400"
```

```r
icer_519_low <- calc_icer()
```

```
## [1] "intv net costs = 50056786.043626"
## [1] "cost difference = 29555213.383673"
## [1] "cases averted = 119143.065600"
## [1] "icer = 248.064906126375"
```

```r
deaths_519_low <- calc_deaths_b()
```

```
## [1] "deaths averted = 16.184973"
## [1] "cost per death averted = 1826089.756857"
```


###### 20-64 years ######


```r
pop <- pop_2064
num_cases <- base_2064
cases_after_vax <- intv_2064
vax_comp_i <- vc_2064
```

## 20-64 years, HIGH

```r
risk_group <- high_2064
prob <- c(p_2064_high_death, p_2064_high_hosp, p_2064_high_out, p_2064_high_rest)
r_cost <- rc_2064_high

total_cost_vax_i <- calc_vaccination_cost(vax_comp_i)  # intv

costs <- cost_cpi_outcome(r_cost[1], r_cost[2], r_cost[3], r_cost[4])

b_cases <- calc_subpop_cases_base(num_cases, risk_group)  # base
num_outcomes_base <- calc_num_outcomes_base(prob[1], prob[2], prob[3], prob[4])  # base
```

```
## [1] "base num: deaths = 668.385850, hosps = 758.230165, outps = 31874.194807, rest = 18448.521018"
```

```r
cc_base <- calc_total_cost_base(r_cost[1],r_cost[2],r_cost[3],r_cost[4])  # base
```

```
## [1] "base cc: deaths = 64909830.684107, hosps = 30271687.985590, outps = 28659091.298496, rest = 99775.312058"
## [1] "base total clin costs = 123940385.280251"
```

```r
i_cases <- calc_subpop_cases_intv(cases_after_vax, risk_group)  # intv
num_outcomes_intv <- calc_num_outcomes_intv(prob[1], prob[2], prob[3], prob[4])  # intv
```

```
## [1] "intv num: deaths = 337.982505, hosps = 383.414058, outps = 16117.816111, rest = 9328.859006"
```

```r
cc_intv <- calc_total_cost_intv(r_cost[1],r_cost[2],r_cost[3],r_cost[4])  # intv
```

```
## [1] "intv cc: deaths = 32822937.836764, hosps = 15307476.887445, outps = 14492035.524274, rest = 50453.357077"
## [1] "intv total clin costs = 62672903.605559"
```

```r
print(sprintf("total vax costs = %f", total_cost_vax_i))
```

```
## [1] "total vax costs = 3039716.705098"
```

```r
print(sprintf("base cases = %f", b_cases))
```

```
## [1] "base cases = 51749.331840"
```

```r
print(sprintf("intv cases = %f", i_cases))
```

```
## [1] "intv cases = 26168.071680"
```

```r
icer_2064_high <- calc_icer()
```

```
## [1] "intv net costs = 65712620.310657"
## [1] "cost difference = 58227764.969594"
## [1] "cases averted = 25581.260160"
## [1] "icer = 2276.18829586205"
```

```r
deaths_2064_high <- calc_deaths_b()
```

```
## [1] "deaths averted = 330.403344"
## [1] "cost per death averted = 176232.371589"
```

## 20-64 years, low risk

```r
risk_group <- 1-high_2064
prob <- c(p_2064_low_death, p_2064_low_hosp, p_2064_low_out, p_2064_low_rest)
r_cost <- rc_2064_low

total_cost_vax_i <- calc_vaccination_cost(vax_comp_i)  # intv

costs <- cost_cpi_outcome(r_cost[1], r_cost[2], r_cost[3], r_cost[4])

b_cases <- calc_subpop_cases_base(num_cases, risk_group)  # base
num_outcomes_base <- calc_num_outcomes_base(prob[1], prob[2], prob[3], prob[4])  # base
```

```
## [1] "base num: deaths = 93.108929, hosps = 2060.273709, outps = 108114.562482, rest = 197353.083040"
```

```r
cc_base <- calc_total_cost_base(r_cost[1],r_cost[2],r_cost[3],r_cost[4])  # base
```

```
## [1] "base cc: deaths = 8330676.395766, hosps = 62833109.782118, outps = 61687645.474822, rest = 1067346.559996"
## [1] "base total clin costs = 133918778.212702"
```

```r
i_cases <- calc_subpop_cases_intv(cases_after_vax, risk_group)  # intv
num_outcomes_intv <- calc_num_outcomes_intv(prob[1], prob[2], prob[3], prob[4])  # intv
```

```
## [1] "intv num: deaths = 47.082369, hosps = 1041.818091, outps = 54670.263752, rest = 99795.484109"
```

```r
cc_intv <- calc_total_cost_intv(r_cost[1],r_cost[2],r_cost[3],r_cost[4])  # intv
```

```
## [1] "intv cc: deaths = 4212571.048092, hosps = 31772802.898005, outps = 31193576.248415, rest = 539724.867864"
## [1] "intv total clin costs = 67718675.062376"
```

```r
print(sprintf("total vax costs = %f", total_cost_vax_i))
```

```
## [1] "total vax costs = 18069427.080302"
```

```r
print(sprintf("base cases = %f", b_cases))
```

```
## [1] "base cases = 307621.028160"
```

```r
print(sprintf("intv cases = %f", i_cases))
```

```
## [1] "intv cases = 155554.648320"
```

```r
icer_2064_low <- calc_icer()
```

```
## [1] "intv net costs = 85788102.142678"
## [1] "cost difference = 48130676.070024"
## [1] "cases averted = 152066.379840"
## [1] "icer = 316.510961335869"
```

```r
deaths_2064_low <- calc_deaths_b()
```

```
## [1] "deaths averted = 46.026560"
## [1] "cost per death averted = 1045715.258080"
```


###### 65+ years ######


```r
pop <- pop_65
num_cases <- base_65
cases_after_vax <- intv_65
vax_comp_i <- vc_65
```

## 65+ years, HIGH

```r
risk_group <- high_65
prob <- c(p_65_high_death, p_65_high_hosp, p_65_high_out, p_65_high_rest)
r_cost <- rc_65_high

total_cost_vax_i <- calc_vaccination_cost(vax_comp_i)  # intv

costs <- cost_cpi_outcome(r_cost[1], r_cost[2], r_cost[3], r_cost[4])

b_cases <- calc_subpop_cases_base(num_cases, risk_group)  # base
num_outcomes_base <- calc_num_outcomes_base(prob[1], prob[2], prob[3], prob[4])  # base
```

```
## [1] "base num: deaths = 460.017590, hosps = 887.942212, outps = 11733.710656, rest = 4395.080902"
```

```r
cc_base <- calc_total_cost_base(r_cost[1],r_cost[2],r_cost[3],r_cost[4])  # base
```

```
## [1] "base cc: deaths = 27868408.066869, hosps = 22929621.149117, outps = 41121781.385163, rest = 23769.957933"
## [1] "base total clin costs = 91943580.559082"
```

```r
i_cases <- calc_subpop_cases_intv(cases_after_vax, risk_group)  # intv
num_outcomes_intv <- calc_num_outcomes_intv(prob[1], prob[2], prob[3], prob[4])  # intv
```

```
## [1] "intv num: deaths = 231.031677, hosps = 445.945509, outps = 5892.946068, rest = 2207.313226"
```

```r
cc_intv <- calc_total_cost_intv(r_cost[1],r_cost[2],r_cost[3],r_cost[4])  # intv
```

```
## [1] "intv cc: deaths = 13996171.421417, hosps = 11515796.218469, outps = 20652327.899024, rest = 11937.833159"
## [1] "intv total clin costs = 46176233.372070"
```

```r
print(sprintf("total vax costs = %f", total_cost_vax_i))
```

```
## [1] "total vax costs = 2848504.767898"
```

```r
print(sprintf("base cases = %f", b_cases))
```

```
## [1] "base cases = 17476.751360"
```

```r
print(sprintf("intv cases = %f", i_cases))
```

```
## [1] "intv cases = 8777.236480"
```

```r
icer_65_high <- calc_icer()
```

```
## [1] "intv net costs = 49024738.139967"
## [1] "cost difference = 42918842.419115"
## [1] "cases averted = 8699.514880"
## [1] "icer = 4933.47537318255"
```

```r
deaths_65_high <- calc_deaths_b()
```

```
## [1] "deaths averted = 228.985913"
## [1] "cost per death averted = 187430.055487"
```

## 65+ years, low risk

```r
risk_group <- 1-high_65
prob <- c(p_65_low_death, p_65_low_hosp, p_65_low_out, p_65_low_rest)
r_cost <- rc_65_low

total_cost_vax_i <- calc_vaccination_cost(vax_comp_i)  # intv

costs <- cost_cpi_outcome(r_cost[1], r_cost[2], r_cost[3], r_cost[4])

b_cases <- calc_subpop_cases_base(num_cases, risk_group)  # base
num_outcomes_base <- calc_num_outcomes_base(prob[1], prob[2], prob[3], prob[4])  # base
```

```
## [1] "base num: deaths = 57.580693, hosps = 235.875597, outps = 6367.183782, rest = 9996.888568"
```

```r
cc_base <- calc_total_cost_base(r_cost[1],r_cost[2],r_cost[3],r_cost[4])  # base
```

```
## [1] "base cc: deaths = 2570023.633156, hosps = 4046802.023687, outps = 7937429.018781, rest = 54066.267723"
## [1] "base total clin costs = 14608320.943347"
```

```r
i_cases <- calc_subpop_cases_intv(cases_after_vax, risk_group)  # intv
num_outcomes_intv <- calc_num_outcomes_intv(prob[1], prob[2], prob[3], prob[4])  # intv
```

```
## [1] "intv num: deaths = 28.918381, hosps = 118.462285, outps = 3197.749777, rest = 5020.673077"
```

```r
cc_intv <- calc_total_cost_intv(r_cost[1],r_cost[2],r_cost[3],r_cost[4])  # intv
```

```
## [1] "intv cc: deaths = 1290726.446966, hosps = 2032399.364045, outps = 3986363.947507, rest = 27153.354054"
## [1] "intv total clin costs = 7336643.112573"
```

```r
print(sprintf("total vax costs = %f", total_cost_vax_i))
```

```
## [1] "total vax costs = 2714981.106902"
```

```r
print(sprintf("base cases = %f", b_cases))
```

```
## [1] "base cases = 16657.528640"
```

```r
print(sprintf("intv cases = %f", i_cases))
```

```
## [1] "intv cases = 8365.803520"
```

```r
icer_65_low <- calc_icer()
```

```
## [1] "intv net costs = 10051624.219475"
## [1] "cost difference = 4556696.723872"
## [1] "cases averted = 8291.725120"
## [1] "icer = 549.547489566517"
```

```r
deaths_65_low <- calc_deaths_b()
```

```
## [1] "deaths averted = 28.662312"
## [1] "cost per death averted = 158978.688005"
```
