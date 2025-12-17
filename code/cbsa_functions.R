## Functions to get estimates from CBSA data
## CCT
## 2025-12-16

###############################################################################
## GET DATAFRAME OF COUNTS AND PERCENTAGES BY YEAR FOR SELECT COUNTRIES
###############################################################################

get_cc_prop <- function(...) { #can be a named vector or user-defined country names
  
  require(tidyverse)
  
  countries <- c(...)
  countries <- paste(countries, collapse = "|")
  
  d <- readRDS("W:/DCAP_PACD/Group/RAD_DER/travel_data/cbsa/cbsa_cc_prop_by_country.rds")
  
  d <- d |> 
    filter(str_detect(country, countries)) |> 
    arrange(country, year)
  
  return(d)
  
}

###############################################################################
## PLOT COUNTS AND PERCENTAGES BY YEAR FOR SELECT COUNTRIES
###############################################################################

plot_cc_prop <- function(...) {
  
  require(tidyverse)
  
  countries <- c(...)
  countries <- paste(countries, collapse = "|")
  
  d <- readRDS("W:/DCAP_PACD/Group/RAD_DER/travel_data/cbsa/cbsa_cc_prop_by_country.rds")
  
  d <- d |> 
    filter(str_detect(country, countries)) |> 
    arrange(country, year)
  
  ratio <- max(d$cc)
  
  d |> 
    ggplot() +
    geom_col(aes(x = year, y = cc), fill = "#3B5378") +
    geom_line(aes(x = year, y = cc_p * ratio), size = 1, col = "#E88768") +
    geom_point(aes(x = year, y = cc_p * ratio), size = 1, col = "black") +
    scale_y_continuous(
      labels = scales::comma_format(),
      "Canadian Citizens (N)",
      sec.axis = sec_axis(~ . / ratio * 100, name = "Canadian Citizens (%)")
    ) +
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
  
}
  

