## test code to plot Thailand Aedes aegypti suitability and hotels/attractions
## CCT
## 2025-12-09

###############################################################################
## LOAD LIBRARIES
###############################################################################
library(osmdata)
library(sf)
library(here)
library(tidyverse)
library(tidyterra)

###############################################################################
## EXTRACT HOTEL AND ATTRACTIONS DATA FROM OPEN STREET MAP
###############################################################################
# Define your area of interest (bounding box)
# Example: Thailand
bbox <- getbb("Thailand")

# Query for tourist attractions
attractions <- opq(bbox) %>%
  add_osm_feature(key = "tourism", value = c("attraction", "museum", 
                                             "viewpoint", "artwork",
                                             "gallery", "zoo", "theme_park")) %>%
  osmdata_sf()

# Extract points and polygons
attractions_points <- attractions$osm_points
attractions_polygons <- attractions$osm_polygons

# Query for hotels
hotels <- opq(bbox) %>%
  add_osm_feature(key = "tourism", value = c("hotel", "hostel", 
                                             "guest_house", "motel",
                                             "resort", "apartment")) %>%
  osmdata_sf()

hotels_points <- hotels$osm_points
hotels_polygons <- hotels$osm_polygons

# If you have polygons, convert to centroids
if(!is.null(hotels_polygons)) {
  hotels_centroids <- st_centroid(hotels_polygons)
  hotels_all <- rbind(hotels_points[,c("osm_id", "name", "geometry")], 
                      hotels_centroids[,c("osm_id", "name", "geometry")])
}

# Save as shapefile or GeoPackage
st_write(attractions_points, here("data", "attractions.gpkg"), delete_dsn = TRUE)
st_write(hotels_points, here("data", "hotels.gpkg"), delete_dsn = TRUE)

# Convert to spatial points for raster operations
library(terra)
hotels_vect <- vect(hotels_points)
attractions_vect <- vect(attractions_points)

###############################################################################
## PLOT HOTEL AND ATTRACTION LOCATIONS
###############################################################################
# Simple plot
plot(st_geometry(hotels_points))

# Add country boundary for context
country <- st_as_sf(rnaturalearth::ne_countries(country = "Thailand", returnclass = "sf"))
plot(st_geometry(country))
plot(st_geometry(hotels_points), add = TRUE, col = "red", pch = 20)

###############################################################################
## IMPORT VECTOR SUITABILITY DATA
###############################################################################
# Load vector suitability data
th_aegypti <- rast(here("data/Thailand", "Thailand_indexP_typical_year_mean_rasters.tif"))
plot(th_aegypti$X2022)

# Extract 2022 layer
th_aegypti_2022 <- th_aegypti$X2022

###############################################################################
## INTERSECT HOTEL/ATTRACTIONS DATA AND COUNT
###############################################################################
# Count hotels and attractions per grid cell
# Count hotels per grid cell
hotel_count <- rasterize(hotels_vect, th_aegypti_2022, fun = "count")
plot(hotel_count)
attractions_count <- rasterize(attractions_vect, th_aegypti_2022, fun = "count")
plot(attractions_count)

###############################################################################
## CALCULATE CORRELATION BETWEEN HOTEL AND ATTRACTIONS COUNTS
###############################################################################
# Correlation between count of hotels and attractions
# Stack the two rasters
combined <- c(hotel_count, attractions_count)
names(combined) <- c("hotels", "attractions")

# Calculate correlation
correlation <- layerCor(combined, fun = "pearson")
print(correlation)

# Spearman
# Extract values from both rasters
hotel_values <- values(hotel_count, na.rm = FALSE)
attraction_values <- values(attractions_count, na.rm = FALSE)

# Remove NA values (cells where both have data)
valid_cells <- complete.cases(hotel_values, attraction_values)
hotel_clean <- hotel_values[valid_cells]
attraction_clean <- attraction_values[valid_cells]

# Calculate correlation
cor_pearson <- cor(hotel_clean, attraction_clean, method = "pearson")
cor_spearman <- cor(hotel_clean, attraction_clean, method = "spearman")

print(paste("Pearson correlation:", round(cor_pearson, 3)))
print(paste("Spearman correlation:", round(cor_spearman, 3)))

# Statistical test
cor_test <- cor.test(hotel_clean, attraction_clean, method = "pearson")
print(cor_test)

# Create dataframe for plotting
df <- data.frame(
  hotels = hotel_clean,
  attractions = attraction_clean
)

# Scatterplot
ggplot(df, aes(x = attractions, y = hotels)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", color = "red") +
  labs(
    title = "Relationship between Hotels and Attractions",
    x = "Number of Attractions per Grid Cell",
    y = "Number of Hotels per Grid Cell"
  ) +
  theme_minimal() +
  annotate("text", x = Inf, y = Inf, 
           label = paste("r =", round(cor_pearson, 3)),
           hjust = 1.1, vjust = 1.5, size = 5)

# Hexbin plot (better for many points)
ggplot(df, aes(x = attractions, y = hotels)) +
  geom_hex() +
  scale_fill_viridis_c() +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Hotels vs Attractions Density") +
  theme_minimal()

###############################################################################
## PLOTS OF VECTOR SUITABILITY VS HOTELS/ATTRACTIONS
###############################################################################
# plot Aedes suitability and hotels
# Create ggplot objects
aedes_plot <- ggplot() +
  geom_spatraster(data = th_aegypti_2022) +
  scale_fill_viridis_c(name = "Ae. aegypti\nsuitability", 
                       option = "plasma",
                       na.value = "transparent") +
  labs(title = "Ae. aegypti Suitability") +
  theme_minimal()

hotels_plot <- ggplot() +
  geom_spatraster(data = hotel_count) +
  scale_fill_viridis_c(name = "Hotel\nCount",
                       option = "plasma",
                       na.value = "transparent") +
  labs(title = "Hotels per Grid") +
  theme_minimal()

attractions_plot <- ggplot() +
  geom_spatraster(data = attractions_count) +
  scale_fill_viridis_c(name = "Attractions\nCount",
                       option = "plasma",
                       na.value = "transparent") +
  labs(title = "Attractions per Grid") +
  theme_minimal()

# Combine
aedes_plot + hotels_plot + attractions_plot

###############################################################################
## ATTRACTIVENESS INDEX (HOTELS * ATTRACTIONS)
###############################################################################
# Attractions * hotels
attractionsXhotels <- attractions_count * hotel_count

attractionsXhotels_norm <- (attractionsXhotels - global(attractionsXhotels, "min", na.rm = TRUE)[[1]]) / 
  (global(attractionsXhotels, "max", na.rm = TRUE)[[1]] - 
     global(attractionsXhotels, "min", na.rm = TRUE)[[1]])

attrXhot_plot <- ggplot() +
  geom_spatraster(data = attractionsXhotels_norm) +
  scale_fill_viridis_c(name = "Attraction index",
                       option = "plasma",
                       na.value = "transparent") +
  labs(title = "Attractions X Hotels per grid\n(normalised)") +
  theme_minimal()

aedes_plot + attrXhot_plot
