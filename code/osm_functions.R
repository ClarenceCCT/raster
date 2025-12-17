## functions to calculate country-specific tourism attractiveness index
## CCT
## 2025-12-16

## function to retrieve OSM data and calculate index

get_attract_index <- function(...) {
  
  require(tidyverse)
  require(terra)
  require(sf)
  require(osmextract)  
  
  country <- c(...)
  
  # Download OSM data (one-time download, then cached)
  country_osm <- oe_get(
    place = country,
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
  
  # convert to spatial points for raster operations
  hotels_vect <- vect(hotels)
  attractions_vect <- vect(attractions)
  
  # get country boundary
  boundary <- rnaturalearth::ne_countries(country = country, returnclass = "sv")
  
  # Approximate 10km in degrees
  # At the equator: 1 degree ≈ 111 km
  # So 10km ≈ 10/111 ≈ 0.09 degrees (note that grid cells decrease in area further away from the equator)
  resolution_deg <- 10 / 111
  
  # create grid
  grid <- rast(boundary, resolution = resolution_deg)
  
  # count hotels and attractions and mask to boundary
  hotels_count <- rasterize(hotels_vect, grid, fun = "count")
  hotels_count <- subst(hotels_count, NA, 0) # replace NAs with 0
  
  attractions_count <- rasterize(attractions_vect, grid, fun = "count")
  attractions_count <- subst(attractions_count, NA, 0)
  
  hotels_count <- mask(hotels_count, boundary)
  attractions_count <- mask(attractions_count, boundary)
  
  # Normalize counts
  hotel_norm <- (hotels_count - global(hotels_count, "min", na.rm = TRUE)[[1]]) / 
    (global(hotels_count, "max", na.rm = TRUE)[[1]] - global(hotels_count, "min", na.rm = TRUE)[[1]])
  
  attractions_norm <- (attractions_count - global(attractions_count, "min", na.rm = TRUE)[[1]]) / 
    (global(attractions_count, "max", na.rm = TRUE)[[1]] - global(attractions_count, "min", na.rm = TRUE)[[1]])
  
  # define weights for hotels and attractions
  # could change these or allow user-defined weights
  hotel_wt <- 0.5 
  attraction_wt <- 0.5
  
  attract_index <- hotel_wt * hotel_norm + attraction_wt * attractions_norm
  
  return(attract_index)
  
}
