## test code to estimate sub-national travel by Canadian Citizens
## CCT
## 2025-12-17

###############################################################################
## LOAD LIBRARIES
###############################################################################
require(tidyverse)
require(here)
require(patchwork)

###############################################################################
## LOAD FUNCTIONS
###############################################################################
## note that these are included in the .Rprofile for the project
#source(here("code", "osm_functions.R"))
#source(here("code", "cbsa_functions.R"))

###############################################################################
## CALCULATE ATTRACTIVENESS INDEX FOR CUBA
###############################################################################
## note that current function calculates index for grid cells of 0.09 degrees (~10km^2 at the equator)
cuba_index <- get_attract_index("Cuba")

cuba <- rnaturalearth::ne_countries(country = "Cuba", returnclass = "sf")
ggplot() +
  geom_sf(data = cuba) +
  geom_spatraster(data = cuba_index) +
  scale_fill_viridis_c(name = "Attractiveness index",
                       option = "plasma",
                       na.value = "transparent") +
  labs(title = "Attractions per cell\n(normalised)") +
  theme_minimal()

# index expressed as proportion of sum for country
cuba_p <- cuba_index / global(cuba_index, "sum", na.rm = TRUE)[[1]]

ggplot() +
  geom_spatraster(data = cuba_p) +
  scale_fill_viridis_c(name = "Attractiveness index",
                       option = "plasma",
                       na.value = "transparent") +
  labs(title = "Attractions per cell\n(normalised)") +
  theme_minimal()

###############################################################################
## GET TRAVEL VOLUME FROM CBSA DATA
###############################################################################
cuba_cc <- get_cc_prop("Cuba")
plot_cc_prop("Cuba")

cuba_2024 <- cuba_cc$cc[which(cuba_cc$year == 2024)]

###############################################################################
## DISTRIBUTE TRAVELLERS ACROSS GRID
###############################################################################
cuba_travel <- cuba_p * cuba_2024

ggplot() +
  geom_spatraster(data = cuba_travel) +
  scale_fill_viridis_c(name = "Estimated travel volume (CC)",
                       option = "plasma",
                       na.value = "transparent") +
  labs(title = "Travellers per cell\n(2024)") +
  theme_minimal()

###############################################################################
## IMPORT AE. AEGYPTI DENGUE TRANSMISSION POTENTIAL DATA
###############################################################################
# Load vector suitability data
cu_aegypti <- rast(here("data/Cuba", "Cuba_indexP_yearly_mean_rasters.tif"))
cu_aegypti_monthly <-  rast(here("data/Cuba", "Cuba_indexP_typical_year_median_rasters.tif"))
plot(cu_aegypti_monthly)

# Extract 2022 layer
cu_aegypti_2022 <- cu_aegypti$X2022
#plot(cu_aegypti_2022)

###############################################################################
## AGGREGATE TRAVELLER ESTIMATES TO SAME GRID CELL DIMENSIONS AS TRANSMISSION POTENTIAL DATA
###############################################################################
# Re-sample traveller raster to same dimensions as vector data
#cuba_travel2 <- resample(cuba_travel, cu_aegypti_2022, method = "sum")
#compareGeom(cuba_travel2, cu_aegypti_2022)

# Re-sample to common grid
my_res <- 10 / 111 # 0.09 degrees, ~10km^2 at the equator
common_grid <- rast(cuba, resolution = my_res)
cuba_travel_common <- resample(cuba_travel, common_grid, method = "sum")
cu_aegypti_2022_common <- resample(cu_aegypti_2022, common_grid, method = "near")

# Mask to country boundary
cuba_travel_common <- mask(cuba_travel, cuba)
cu_aegypti_2022_common <- mask(cu_aegypti_2022_common, cuba)

###############################################################################
## PLOTS
###############################################################################
# traveller volume 
cuba_travel_plot <- ggplot() +
  geom_spatraster(data = cuba_travel_common) +
  #geom_sf(data = cuba) +
  scale_fill_viridis_c(name = "Estimated travel volume (CC)",
                       option = "plasma",
                       na.value = "transparent") +
  labs(title = "Travellers per cell\n(2024)") +
  theme_minimal()

# Ae.aegypti dengue transmission potential
aedes_plot <- ggplot() +
  geom_spatraster(data = cu_aegypti_2022_common) +
  #geom_sf(data = cuba) +
  scale_fill_viridis_c(name = "Ae. aegypti transmission potential\nfor dengue", 
                       option = "plasma",
                       na.value = "transparent") +
  labs(title = "Index P (2022 mean)") +
  theme_minimal()

aedes_plot + cuba_travel_plot


###############################################################################
## FOR FURTHER DEVELOPMENT...
###############################################################################
## IATA data has information about which airports within a country are used most often
## could consider which airports travellers are most likely to arrive into
## and probability of visiting different cells based on distance and attractiveness
## note that this method does not account for individual travellers visiting several locations
## could assume that collective travel time spent by Canadian Citizens in different locations 
## is proportional to attractiveness index

## need to consider travel volume during times of year with exposure risk
## e.g. months during which vector IndexP is estimated to be >1