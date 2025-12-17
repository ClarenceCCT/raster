## code to import aggregated CBSA data and calculate % Canadian Citizens by country and year
## CCT
## 2025-12-16

###############################################################################
## LOAD LIBRARIES
###############################################################################
require(tidyverse)
require(lubridate)

###############################################################################
## IMPORT AGGREGATED CBSA DATA
###############################################################################
cbsa_file <- "W:/DCAP_PACD/PUBLIC/CBSA/Data/CSV_iapi_combined/cbsa_agr_iapi_2019_2025.csv"
cbsa <- read_csv(cbsa_file)

###############################################################################
## FORMAT DATA
###############################################################################
## for suppressed values (<10 travellers), replace with 5
cbsa <- cbsa |> 
  mutate(
    npass = if_else(travellers_c == "*", 5, travellers_n)
  )

###############################################################################
## COUNTS BY COUNTRY AND CITIZENSHIP STATUS
###############################################################################
## Annual
cbsa_ccfn <- cbsa |> 
  group_by(year = wk_start_yr, country = embark_country, status = citizenship_country) |> 
  summarise(npass = sum(npass)) |> 
  ungroup() |> 
  pivot_wider(id_cols = c(country, year), names_from = "status", values_from = npass) |> 
  rename(cc = "CA - Canada", fn = "FN - Foreign National") |> 
  tidyr::replace_na(list(cc = 0, fn = 0)) |> 
  mutate(
    total = cc + fn,
    cc_p = cc / total
  ) 

## Monthly - to adjust for weeks that straddle consecutive months
## first convert to daily data and aggregate over months
cbsa_ccfn_monthly <- cbsa |> 
  # Create sequence of dates for each week
  mutate(date = map2(as.Date(wk_start, "%m/%d/%Y"), as.Date(wk_end, "%m/%d/%Y"), ~seq(.x, .y, by = "day"))) |> 
  unnest(date) |> 
  mutate(
    month_start = floor_date(date, "month"),  # Extract month
    month = month(month_start), # Numerical month
    daily_n = npass / 7 # Allocate daily proportion
  ) |> 
  # Aggregate to monthly
  group_by(year = wk_start_yr, country = embark_country, month, month_start, status = citizenship_country) |> 
  summarise(npass = as.integer(round(sum(daily_n), 0))) 

###############################################################################
## SAVE
###############################################################################
saveRDS(cbsa_ccfn, file = "W:/DCAP_PACD/Group/RAD_DER/travel_data/cbsa/cbsa_cc_prop_by_country.rds")
saveRDS(cbsa_ccfn_monthly, file = "W:/DCAP_PACD/Group/RAD_DER/travel_data/cbsa/cbsa_cc_prop_by_country_monthly.rds")
