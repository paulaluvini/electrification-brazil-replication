# Brazil Electrification Replication

This repository contains an R Markdown replication of selected results from Lipscomb, Mobarak, and Barham (2013), *Development Effects of Electrification: Evidence from the Topographic Placement of Hydropower Plants in Brazil*.

## Contents

- `Brazil_Electricity_replication_R.Rmd`: main replication script.
- `data/`: replication datasets and original Stata do-file.
- `output/`: generated summary tables.

## Running the replication

Open `Brazil_Electricity_replication_R.Rmd` and run or knit the file from the repository root. The script reads the `.dta` files in `data/`, estimates the weighted OLS, fixed-effects, and IV specifications with `fixest`, and writes output tables to `output/`.

Required R packages:

```r
install.packages(c("haven", "fixest", "rmarkdown", "knitr"))
```

## Notes

The public replication dataset does not include the GDP per capita and industrial GDP per capita variables reported in the published descriptive table. These variables are therefore not reconstructed unless the original county-level GDP source and harmonization crosswalk are added separately.
