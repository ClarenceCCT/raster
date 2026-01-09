## code to get airport-specific travel information for Cuba (IATA)
## CCT
## 2026-01-09

##################################################################################
## PRELIMINARIES
##################################################################################
## load libraries
require(tidyverse)
require(here)

## the following functions are automatically loaded into the project .Rprofile
#source(here("code", "cbsa_functions.R"))

##################################################################################
## DEFINE AIRPORTS FOR SOURCE AND DESTINATION COUNTRIES OF INTEREST
##################################################################################
## load airports file from RAD IATA folder
airports_file <- paste(iata_path_rad, "airports.rds", sep = "/")
airports <- readRDS(airports_file)

## define origin and destination countries of interest
my_orig_country <- c("CA") # using 2-letter country code
my_dest_country <- c("CU")

## get vector of airport codes
my_orig_airports <- airports |> 
  filter(country_code == my_orig_country) |> 
  select(airport_code) |> 
  pull()

my_dest_airports <- airports |> 
  filter(country_code == my_dest_country) |> 
  select(airport_code) |> 
  pull()

##################################################################################
## DEFINE IATA FILES TO IMPORT (THESE ARE MONTHLY CSV FILES)
##################################################################################
## list all files in all sub-folders
all_files <- list.files(iata_path_dcap, pattern = "*.csv", full.names = TRUE, recursive = TRUE)

## define number of months of data to keep (from last file available)
my_months <- 24
start_file <- length(all_files) - my_months + 1
end_file <- length(all_files)

## subset files
sub_files <- all_files[start_file:end_file]
#test_files <- tail(sub_files, n = 2)

##################################################################################
## IMPORT DATA
##################################################################################
cu_iata <- map_dfr(sub_files, ~ import_iata(.x, origcodes = my_orig_airports, destcodes = my_dest_airports))
cu_iata <- process_iata(cu_iata)

##################################################################################
## SAVE DATA
##################################################################################
save_filename <- paste(save_path_rad, "cu_iata.rds", sep = "/")
saveRDS(cu_iata, file = save_filename)
