## code to import aggregated CBSA data and calculate % Canadian Citizens by country and year
## CCT
## 2025-12-16

###############################################################################
## LOAD LIBRARIES
###############################################################################
require(tidyverse)

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
## COUNTS BY YEAR, COUNTRY AND CITIZENSHIP STATUS
###############################################################################
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

###############################################################################
## SAVE
###############################################################################
saveRDS(cbsa_ccfn, file = "W:/DCAP_PACD/Group/RAD_DER/travel_data/cbsa/cbsa_cc_prop_by_country.rds")  