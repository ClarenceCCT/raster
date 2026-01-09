## test code to estimate sub-national travel by Canadian Citizens, method 2 (including relative airport share and distance from airports)
## CCT
## 2026-01-09

###############################################################################
## LOAD LIBRARIES
###############################################################################
require(tidyverse)
require(here)
require(patchwork)
require(tidyterra)

options(scipen = 999)

###############################################################################
## LOAD FUNCTIONS
###############################################################################
## note that these are included in the .Rprofile for the project
#source(here("code", "osm_functions.R"))
#source(here("code", "cbsa_functions.R"))
source(here("code", "airport_functions.R"))

###############################################################################
## LOAD IATA TRAVEL DATA FOR CUBA
###############################################################################
file_path <- "W:/DCAP_PACD/Group/RAD_DER/chikv/cu_iata.rds"
cu_iata <- readRDS(file_path)

## get list of airport codes
my_airport_codes <- cu_iata |> 
  distinct(dest) |> 
  arrange(dest) |> 
  pull()

## create a list, with each element containing a single airport and its coordinates
my_airports_list <- airportr::airports |> 
  janitor::clean_names() |> 
  filter(iata %in% my_airport_codes) |> 
  group_by(iata) |>
  group_split(.keep = TRUE)

## generate a list of SpatVector objects (coordinate vectors for each airport)
my_coords_list <- map(my_airports_list, ~vect(cbind(.x$longitude, .x$latitude), crs = crs(cuba_index)))

###############################################################################
## GET DISTANCES OF EACH CELL FROM AIRPORTS
###############################################################################
## get distances and output to multi-layer raster
airport_dist_raster <- calculate_individual_airport_distances(
  cuba_index, my_coords_list, my_airport_codes, units = "km"
)                      

#plot(airport_dist_raster[["HOG"]])

## get inverse distance
airport_dist_raster_inv <- 1 / (airport_dist_raster)

###############################################################################
## FOR EACH AIRPORT, CALCULATE ATTRACTIVENESS OF OTHER CELLS WEIGHTED BY INVERSE DISTANCE
###############################################################################
## multiply attractiveness index by inverse distance layers
attractXdist <- cuba_index * airport_dist_raster_invsq
names(attractXdist) <- names(airport_dist_raster)

## normalize cell values in each layer by layer sum
layer_sums <- global(attractXdist, "sum", na.rm = TRUE)
attractXdist_norm <- attractXdist / as.numeric(layer_sums[[1]])

## verify values sum to 1 for each layer
global(attractXdist_norm, "sum", na.rm = TRUE) |> print()

plot(attractXdist_norm)

###############################################################################
## ESTIMATE TRAVEL VOLUME TO INDIVIDUAL AIRPORTS (Canadian Citizens)
###############################################################################
## get overall travel volume by Canadian citizens from CBSA data
cuba_cc <- get_cc_prop("Cuba")
#plot_cc_prop("Cuba")

cuba_2024 <- cuba_cc$cc[which(cuba_cc$year == 2024)]

## estimate proportion travelling to each airport in Cuba from IATA data
airport_arrivals_p <- cu_iata |> 
  group_by(dest) |> 
  summarise(n = sum(npass)) |>
  mutate(
    p = n / sum(n)
  ) |> 
  select(p) |> 
  pull()

## estimate number of CC travellers to each airport
airport_arrivals <- cuba_2024 * airport_arrivals

###############################################################################
## DISTRIBUTE TRAVELLERS ACROSS GRID BASED ON ARRIVALS AT EACH AIRPORT
###############################################################################
## multiply travellers to each airport by propensity to travel to each cell
r_traveln <- attractXdist_norm * airport_arrivals

## verify product sums to expected number of travellers
global(r_traveln, "sum", na.rm = TRUE)
