# Project repository for economic evaluation of influenza vaccination in Seattle
http://rpubs.com/gkang

1. Run `analysis.Rmd`
    - Select input file: `input/input_<scenario>_<efficacy>.R`
    - Select save file: `output/<scenario>-<efficacy>.RData`

2. Run `reports/results-<scenario>-<efficacy>.Rmd`



---
## Files:

- `input/`: contains compliance levels, number of cases
  - `var.R` contains global variables
- `output/`: contains `.RData` saved from `analysis.Rmd`
- `reports/`: contains `.Rmd` files of `.RData` results
- `code/`: contains `functions.r`
- `df/`: contains dataframe of results



---
## Notes:

Updating costs to US 2018$ values using the medical cost component of the Consumer Price Index:
- Unadjusted index Jan 2018 = 513.135
- Unadjusted index Dec 2009 = 379.516
- CPI adjustment = (new index / old index)

Total Population = 3406876 (proportion)
- 0-4 years = 223608 (0.0656)
- 5-19 years = 639661 (0.187)
- 20-64 years = 2235049 (0.656)
- 65+ years 308558 (0.0905)