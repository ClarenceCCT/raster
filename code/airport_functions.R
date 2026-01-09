## function to calculate distances to each airport
calculate_individual_airport_distances <- function(raster_object, airport_list, 
                                                   airport_names = NULL,
                                                   units = "m") {
  
  require(terra)
  
  # Combine airports
  all_airports <- do.call(rbind, airport_list)
  
  # Create names if not provided
  if (is.null(airport_names)) {
    airport_names <- paste0("Airport_", 1:nrow(all_airports))
  }
  
  # Calculate distance to each airport separately
  distance_layers <- list()
  
  for (i in 1:nrow(all_airports)) {
    message(paste("Processing airport", i, "of", nrow(all_airports)))
    
    single_airport <- all_airports[i, ]
    distance_layers[[i]] <- distance(raster_object, single_airport)
    
    if (units == "km") {
      
      distance_layers[[i]] <- distance_layers[[i]] / 1000
    }
    
  }
  
  # Combine into multi-layer raster
  distance_raster <- rast(distance_layers)
  names(distance_raster) <- airport_names
  
  return(distance_raster)
}