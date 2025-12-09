library(osmdata)
library(sf)
library(here)

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

# Simple plot
plot(st_geometry(hotels_points))

# Add country boundary for context
country <- st_as_sf(rnaturalearth::ne_countries(country = "Thailand", returnclass = "sf"))
plot(st_geometry(country))
plot(st_geometry(hotels_points), add = TRUE, col = "red", pch = 20)
