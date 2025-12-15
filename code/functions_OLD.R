query_osm_grid <- function(bbox_or_sf, key, value, grid_size = 5, timeout = 60, 
                           keep_columns = c("osm_id", "name")) {
  library(osmdata)
  library(sf)
  library(dplyr)
  
  # Handle different input types
  if(is.matrix(bbox_or_sf)) {
    bbox_full <- c(
      xmin = bbox_or_sf[1, 1],
      ymin = bbox_or_sf[2, 1],
      xmax = bbox_or_sf[1, 2],
      ymax = bbox_or_sf[2, 2]
    )
    class(bbox_full) <- "bbox"
    attr(bbox_full, "crs") <- st_crs(4326)
    country_geom <- st_as_sfc(bbox_full)
    
  } else if(inherits(bbox_or_sf, "bbox")) {
    bbox_full <- bbox_or_sf
    country_geom <- st_as_sfc(bbox_full)
    
  } else if(inherits(bbox_or_sf, "sf") || inherits(bbox_or_sf, "sfc")) {
    country_geom <- bbox_or_sf
    bbox_full <- st_bbox(country_geom)
    
  } else {
    stop("Input must be a bbox, matrix (from getbb), sf, or sfc object")
  }
  
  # Create grid
  grid <- st_make_grid(country_geom, n = c(grid_size, grid_size))
  grid_sf <- st_sf(id = 1:length(grid), geometry = grid)
  
  results <- list()
  successful <- 0
  failed <- 0
  
  for(i in 1:nrow(grid_sf)) {
    message(sprintf("Processing grid %d of %d...", i, nrow(grid_sf)))
    
    bbox_i <- st_bbox(grid_sf[i,])
    
    if(anyNA(bbox_i) || bbox_i["xmax"] <= bbox_i["xmin"] || bbox_i["ymax"] <= bbox_i["ymin"]) {
      failed <- failed + 1
      next
    }
    
    tryCatch({
      result <- opq(bbox_i, timeout = timeout) %>%
        add_osm_feature(key = key, value = value) %>%
        osmdata_sf()
      
      if(!is.null(result$osm_points) && nrow(result$osm_points) > 0) {
        # Keep only specified columns (and geometry)
        cols_to_keep <- intersect(keep_columns, names(result$osm_points))
        results[[i]] <- result$osm_points %>% 
          select(all_of(cols_to_keep), geometry)
        
        successful <- successful + 1
        message("  Success: ", nrow(result$osm_points), " features found")
      }
    }, error = function(e) {
      message("  Error: ", e$message)
      failed <- failed + 1
    })
    
    Sys.sleep(2)
  }
  
  message(sprintf("\nCompleted: %d successful, %d failed", successful, failed))
  
  # Combine using bind_rows (handles mismatched columns)
  if(length(results) > 0) {
    results <- results[!sapply(results, is.null)]
    
    if(length(results) > 0) {
      combined <- bind_rows(results)
      
      if(nrow(combined) > 0) {
        combined <- combined[!duplicated(combined$osm_id), ]
        message(sprintf("Total unique features: %d", nrow(combined)))
        return(combined)
      }
    }
  }
  
  message("No results found")
  return(NULL)
}