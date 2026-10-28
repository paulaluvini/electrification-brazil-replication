#### SCRIPT for the replication of tables 6 and 7 ####
## Author: Rodrigo Lugo, Paula Luvini
## Date: 28.10.2026
## Output: tables 6 and 7 as .png in /output

# Libraries
library(haven) # To read stata .dta file
library(fixest)
library(tidyverse)
library(stringr)
library(tinytex) # Needed for export argument in etable to work
library(pdftools) # Needed for export argument in etable to work

# Creating dataframe # 
df_brazil <- read_dta("./data/Brazil_Electricity_data.dta") |>
  as.data.frame() |>
  rename(county_id = AMC60ID) # County code variable

# Create lead variable for totalrescap_house and HDI
df_brazil <- df_brazil |>   
  arrange(county_id, decade) |> 
  group_by(county_id) |>
  mutate(F_totalrescap_house = lead(totalrescap_house),
         F_hdi = lead(HDI)) |>
  ungroup() |>
  as.data.frame()

# Create junglexyear interaction
df_brazil <- df_brazil |>
  mutate(jungle_yr = decade*jungle)

## Running regressions ##
# TABLE 6: DV = average value of housing #

# On the basic format of feols(): https://www.tilburgsciencehub.com/topics/analyze/causal-inference/panel-data/fixest/ 

# OLS 1
tbl6_ols1 <- feols(
  F_totalrescap_house ~electricity_infrastructure | decade, 
  data = df_brazil,
  weights = ~areakm2_, 
  cluster = ~county_id # clustered by county
)

# OLS 2
tbl6_ols2 <- feols(
  F_totalrescap_house ~electricity_infrastructure | decade + county_id, 
  data = df_brazil,
  weights = ~areakm2_, 
  cluster = ~county_id
)

# OLS 3
tbl6_ols3 <- feols(
  F_totalrescap_house ~electricity_infrastructure | decade + county_id + jungle_yr,
  data = df_brazil,
  weights = ~areakm2_, 
  cluster = ~county_id
)

# IV 1
# General feols() formula for IV estimations: 
#feols(y ~ exogenous_controls | fixed_effects | endogenous ~ instrument, data = ...)
tbl6_iv1 <- feols(
  F_totalrescap_house ~ 1 | decade | electricity_infrastructure ~ matlab_instrument,
  # the ~ 1 syntax is necessary for feols to compute; it indicates that there are no exogeneous controls
  data = df_brazil,
  weights = ~areakm2_, 
  cluster = ~county_id 
)

# IV 2
tbl6_iv2 <- feols(
  F_totalrescap_house ~ 1 | decade + county_id | electricity_infrastructure ~ matlab_instrument,
  data = df_brazil,
  weights = ~areakm2_, 
  cluster = ~county_id 
)

# IV 3
tbl6_iv3 <- feols(
  F_totalrescap_house ~ 1 | decade + county_id + jungle_yr | electricity_infrastructure ~ matlab_instrument,
  data = df_brazil,
  weights = ~areakm2_, 
  cluster = ~county_id 
)

# Table 7: DV = HDI #
# OLS 1
tbl7_ols1 <- feols(
  F_hdi ~electricity_infrastructure | decade, 
  data = df_brazil,
  weights = ~areakm2_, 
  cluster = ~county_id 
)

# OLS 2
tbl7_ols2 <- feols(
  F_hdi ~electricity_infrastructure | decade + county_id, 
  data = df_brazil,
  weights = ~areakm2_,
  cluster = ~county_id 
)

# OLS 3
tbl7_ols3 <- feols(
  F_hdi ~electricity_infrastructure | decade + county_id + jungle_yr,
  data = df_brazil,
  weights = ~areakm2_, 
  cluster = ~county_id 
)

# IV 1
tbl7_iv1 <- feols(
  F_hdi ~ 1 | decade | electricity_infrastructure ~ matlab_instrument,
  data = df_brazil,
  weights = ~areakm2_, 
  cluster = ~county_id 
)

# IV 2
tbl7_iv2 <- feols(
  F_hdi ~ 1 | decade + county_id | electricity_infrastructure ~ matlab_instrument,
  data = df_brazil,
  weights = ~areakm2_, 
  cluster = ~county_id 
)

# IV 3
tbl7_iv3 <- feols(
  F_hdi ~ 1 | decade + county_id + jungle_yr | electricity_infrastructure ~ matlab_instrument,
  data = df_brazil,
  weights = ~areakm2_, 
  cluster = ~county_id 
)

## Exporting results ##

# Set etable dictionary, to change vars name output #
setFixest_dict(c(F_totalrescap_house = "Average value of housing", 
                 F_hdi = "Human development index",
                 electricity_infrastructure = "Lagged electricity infrastructure",
                 decade = "Decade", 
                 county_id = "County", 
                 jungle_yr = "Amazon x decade"))

# Final table outputs #

# On exporting etables: https://lrberge.github.io/fixest/articles/exporting_tables.html

# Table 6
etable(tbl6_ols1, tbl6_ols2, tbl6_ols3, tbl6_iv1, tbl6_iv2, tbl6_iv3,
       fitstat = ~ r2 + n,
       style.tex = style.tex("aer", yesNo = "Yes", var.title = "\\midrule",
                             fixef.title = "\\midrule", stats.title = "\\midrule"),
       tex = T,
       export = "./output/table6-replication.png")

# Table 7
etable(tbl7_ols1, tbl7_ols2, tbl7_ols3, tbl7_iv1, tbl7_iv2, tbl7_iv3,
       fitstat = ~ r2 + n,
       style.tex = style.tex("aer", yesNo = "Yes", var.title = "\\midrule",
                             fixef.title = "\\midrule", stats.title = "\\midrule"),
       tex = T,
       export = "./output/table7-replication.png")


#### AI TESTING ####
## NOTE: this is fully generated with Claude; it is an attempt to solve the issue of large R2
# statistics in the IV models, which appears to be a result of differences under the hood of Stata
# and R fixest. 
# FOR MORE: see this chat -> https://claude.ai/share/068366a7-594b-4eda-a17a-40170d80b1c2 

fitstat_register("wr2_stata", function(x) {
  w   <- x$weights
  y   <- model.matrix(x, type = "lhs")
  fe  <- model.matrix(x, type = "fixef")            # the model's FE variables, estimation sample
  yd  <- demean(y, fe[["county_id"]], weights = w)  # demean by county only
  1 - sum(w * resid(x)^2) / sum(w * yd^2)
}, alias = "R2 (county-within)")

etable(tbl6_iv2, tbl6_iv3, fitstat = ~ r2 + wr2_stata + n) # works with these, but
etable(tbl6_iv1, tbl6_iv2, tbl6_iv3, fitstat = ~ r2 + wr2_stata + n) # breaks when adding other specifications that did not have this issue
