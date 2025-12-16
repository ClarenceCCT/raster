## code to extract features from OSM and plot against vector suitability
## CCT
## 2025-12-12

rm(list = ls())

require(tidyverse)
require(terra)
require(tidyterra)
require(sf)
require(osmextract)
require(patchwork)
require(scico)
require(here)

source(here("code", "functions.R"))

###############################################################################
## GET OSM DATA AND FILTER FOR RELEVANT TAGS
###############################################################################
# Download OSM data (one-time download, then cached)
country_osm <- oe_get(
  place = "Mexico",
  layer = "points",
  extra_tags = c("tourism", "historic", "leisure", "natural", "name"),
  quiet = FALSE
)

accommodation_tags <- c("hotel", "hostel", "guest_house", "motel", "resort",
                        "apartment", "chalet", "camp_site")
tourism_tags <- c("attraction", "museum", "gallery", "theme_park", "zoo",
                  "aquarium", "viewpoint", "artwork", "information")
historic_tags <- c("monument", "memorial", "castle", "ruins", "archaeological_site",
                   "battlefield", "fort", "manor", "palace")
leisure_tags <- c("park", "nature_reserve", "beach_resort", "water_park",
                  "sports_centre", "stadium")
natural_tags <- c("beach", "waterfall", "peak")

# filter accommodation
hotels <- country_osm %>%
  filter(tourism %in% accommodation_tags) %>%
  select(osm_id, name, tourism, geometry)

# filter attractions
attractions <- country_osm |> 
  filter(tourism %in% tourism_tags |
           historic %in% historic_tags |
           leisure %in% leisure_tags |
           natural %in% natural_tags)

# Convert to spatial points for raster operations
hotels_vect <- vect(hotels)
attractions_vect <- vect(attractions)

###############################################################################
## PLOT HOTEL AND ATTRACTION LOCATIONS
###############################################################################
# Simple plot
plot(st_geometry(hotels))

# Add country boundary for context
country <- st_as_sf(rnaturalearth::ne_countries(country = "Mexico", returnclass = "sf"))
plot(st_geometry(country))
plot(st_geometry(hotels), add = TRUE, col = "red", pch = 20)

###############################################################################
## IMPORT AE. AEGYPTI DENGUE TRANSMISSION POTENTIAL DATA
###############################################################################
# Load vector suitability data
#mx_aegypti <- rast(here("data/Thailand", "Thailand_indexP_typical_year_mean_rasters.tif"))
mx_aegypti <- rast(here("data/Mexico", "Mexico_indexP_yearly_mean_rasters.tif"))

# Extract 2022 layer
mx_aegypti_2022 <- mx_aegypti$X2022
plot(mx_aegypti_2022)

###############################################################################
## INTERSECT HOTEL/ATTRACTIONS DATA AND COUNT
###############################################################################
# Count hotels and attractions per grid cell
# Count hotels per grid cell
hotel_count <- rasterize(hotels_vect, mx_aegypti_2022, fun = "count")
hotel_count <- subst(hotel_count, NA, 0)
#summary(hotel_count)

# Mask to country boundary
hotel_count <- mask(hotel_count, country)
#plot(hotel_count)

# Attractions
attractions_count <- rasterize(attractions_vect, mx_aegypti_2022, fun = "count")
attractions_count <- subst(attractions_count, NA, 0)
attractions_count <- mask(attractions_count, country)
#plot(attractions_count)

# Normalize counts
hotel_norm <- (hotel_count - global(hotel_count, "min", na.rm = TRUE)[[1]]) / 
  (global(hotel_count, "max", na.rm = TRUE)[[1]] - global(hotel_count, "min", na.rm = TRUE)[[1]])

attractions_norm <- (attractions_count - global(attractions_count, "min", na.rm = TRUE)[[1]]) / 
  (global(attractions_count, "max", na.rm = TRUE)[[1]] - global(attractions_count, "min", na.rm = TRUE)[[1]])

# composite index
hotel_wt <- 0.5
attraction_wt <- 0.5

attract_index <- hotel_wt * hotel_norm + attraction_wt * attractions_norm # additive index
attract_index2 <- hotel_norm * attractions_norm # multiplicative index

###############################################################################
## PLOT MAPS
###############################################################################
# hotel index
hotel_plot <- ggplot() +
  geom_sf(data = country) +
  geom_spatraster(data = hotel_norm) +
  #scale_fill_scico(palette= "acton", direction = 1, begin = 0) +
  scale_fill_viridis_c(name = "Hotel index",
                       option = "plasma",
                       na.value = "transparent") +
  labs(title = "Hotels per cell\n(normalised)") +
  theme_minimal()
hotel_plot

# attraction index
attraction_plot <- ggplot() +
  geom_sf(data = country) +
  geom_spatraster(data = attractions_norm) +
  scale_fill_viridis_c(name = "Attractions index",
                       option = "plasma",
                       na.value = "transparent") +
  labs(title = "Attractions per cell\n(normalised)") +
  theme_minimal()
attraction_plot

# attractiveness index - additive 
attract_index_plot <- ggplot() +
  geom_sf(data = country) +
  geom_spatraster(data = attract_index) +
  scale_fill_viridis_c(name = "Attractiveness index",
                       option = "plasma",
                       na.value = "transparent") +
  labs(title = "Normalised attractiveness index\n(weighted average of hotels and attractions)") +
  theme_minimal()

# attractiveness index - multiplicative 
attract_index_plot2 <- ggplot() +
  geom_sf(data = country) +
  geom_spatraster(data = attract_index2) +
  scale_fill_viridis_c(name = "Attractiveness index",
                       option = "plasma",
                       na.value = "transparent") +
  labs(title = "Normalised attractiveness index\n(hotels * attractions)") +
  theme_minimal()

hotel_plot + attraction_plot + attract_index_plot
hotel_plot + attraction_plot + attract_index_plot2

attract_index_plot + attract_index_plot2

# Ae.aegypti dengue transmission potential
aedes_plot <- ggplot() +
  geom_spatraster(data = mx_aegypti_2022) +
  scale_fill_viridis_c(name = "Ae. aegypti transmission potential\nfor dengue", 
                       option = "plasma",
                       na.value = "transparent") +
  labs(title = "Index P") +
  theme_minimal()

aedes_plot + attract_index_plot

ggsave(here("plots", "mx_den_indexp_tourism.png"), device = "png", width = 16, height = 9, dpi = 300)
