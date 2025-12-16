## Calculate % of arrivals that are Canadian Citizens vs Foreign Nationals by country
## CCT
## 2025-12-16

require(tidyverse)
require(here)
require(patchwork)

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
## PLOT COUNTS FOR SELECT COUNTRIES
###############################################################################
my_countries <- c("Thailand|Mexico|Cuba|Brazil")
  
dat <- cbsa_ccfn |> 
  filter(str_detect(country, my_countries)) |> 
  arrange(country, year)

count_plot <- dat |> 
  ggplot(aes(x = year, y = cc)) +
  geom_col(fill = scico::scico(1, palette = "lipari", begin = 0.2)) +
  #scale_y_continuous(limits = c(0,100)) +
  scale_x_continuous(breaks = seq(2019, 2025, 1)) +
  facet_wrap(~country) +
  labs(
    x = "Year",
    y = "Canadian Citizens (N)"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )

percent_plot <- dat |> 
  ggplot(aes(x = year, y = cc_p * 100)) +
  geom_col(fill = scico::scico(1, palette = "lipari", begin = 0.7)) +
  scale_y_continuous(limits = c(0,100)) +
  scale_x_continuous(breaks = seq(2019, 2025, 1)) +
  facet_wrap(~country) +
  labs(
    x = "Year",
    y = "Canadian Citizens (%)"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )

count_plot + percent_plot
