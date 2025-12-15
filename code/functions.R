## function to query OSM for tags (value parameter) under a specific feature type (key parameter)
## accepts a single value character for key and a vector of tags for value
## accepts spatial boundaries as bounding boxes or country boundaries from rnaturalearth
## to avoid timeout errors, the area is split into smaller grids based on a user defined grid size
## the function returns a dataframe of features with the osm_id, name if available and coordinates

query_osm_grid <- function(bbox_or_sf, key, value, grid_size = 5, timeout = 60, 
                           keep_columns = c("osm_id", "name", "geometry")) {
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
    
    # More thorough bbox validation
    if(anyNA(bbox_i)) {
      message("  Skipping: bbox contains NA")
      failed <- failed + 1
      next
    }
    
    if(bbox_i["xmax"] <= bbox_i["xmin"] || bbox_i["ymax"] <= bbox_i["ymin"]) {
      message("  Skipping: invalid bbox extent")
      failed <- failed + 1
      next
    }
    
    # Check bbox isn't too small
    if(abs(bbox_i["xmax"] - bbox_i["xmin"]) < 0.001 || 
       abs(bbox_i["ymax"] - bbox_i["ymin"]) < 0.001) {
      message("  Skipping: bbox too small")
      failed <- failed + 1
      next
    }
    
    # Try the query with better error handling
    attempt <- 1
    max_attempts <- 3
    success <- FALSE
    
    while(attempt <= max_attempts && !success) {
      tryCatch({
        # Build query step by step to catch errors
        query <- opq(bbox_i, timeout = timeout)
        
        if(is.null(query)) {
          stop("opq() returned NULL")
        }
        
        query <- add_osm_feature(query, key = key, value = value)
        
        if(is.null(query)) {
          stop("add_osm_feature() returned NULL")
        }
        
        result <- osmdata_sf(query)
        
        if(!is.null(result) && !is.null(result$osm_points) && nrow(result$osm_points) > 0) {
          # Keep only specified columns
          cols_to_keep <- intersect(keep_columns, names(result$osm_points))
          
          if(length(cols_to_keep) > 0) {
            results[[i]] <- result$osm_points %>% 
              select(all_of(cols_to_keep), geometry)
            
            successful <- successful + 1
            message("  Success: ", nrow(result$osm_points), " features found")
            success <- TRUE
          } else {
            # If no requested columns exist, just keep osm_id and geometry
            results[[i]] <- result$osm_points %>%
              select(osm_id, geometry)
            successful <- successful + 1
            message("  Success: ", nrow(result$osm_points), " features (minimal columns)")
            success <- TRUE
          }
        } else {
          message("  No features found")
          success <- TRUE  # Not an error, just no data
        }
        
      }, error = function(e) {
        message(sprintf("  Attempt %d failed: %s", attempt, e$message))
        
        if(attempt < max_attempts) {
          message(sprintf("  Retrying in %d seconds...", attempt * 3))
          Sys.sleep(attempt * 3)  # Increasing delay
        } else {
          message("  All attempts failed, moving to next grid")
          failed <- failed + 1
        }
        
        attempt <<- attempt + 1
      })
    }
    
    # Longer delay between grid cells
    if(i < nrow(grid_sf)) {
      Sys.sleep(5)  # 5 second delay
    }
  }
  
  message(sprintf("\nCompleted: %d successful, %d failed", successful, failed))
  
  # Combine results
  if(length(results) > 0) {
    results <- results[!sapply(results, is.null)]
    
    if(length(results) > 0) {
      tryCatch({
        combined <- bind_rows(results)
        
        if(nrow(combined) > 0) {
          combined <- combined[!duplicated(combined$osm_id), ]
          message(sprintf("Total unique features: %d", nrow(combined)))
          return(combined)
        }
      }, error = function(e) {
        message("Error combining results: ", e$message)
        message("Returning NULL")
        return(NULL)
      })
    }
  }
  
  message("No results found")
  return(NULL)
}

## this function accepts a list of features that includes multiple feature types (key) and several tags (value) per feature type
query_osm_grid_multi <- function(bbox_or_sf, features_list, grid_size = 5, 
                                 timeout = 60, keep_columns = c("osm_id", "name", "geometry")) {
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
  
  # Validate and format features_list
  # Accept both single key-value or list of key-values
  if(!is.list(features_list)) {
    stop("features_list must be a list")
  }
  
  # Check if it's a single feature (has 'key' and 'value' elements)
  if(!is.null(names(features_list)) && all(c("key", "value") %in% names(features_list))) {
    features_list <- list(features_list)
  }
  
  # Validate each feature has key and value
  for(i in seq_along(features_list)) {
    if(!all(c("key", "value") %in% names(features_list[[i]]))) {
      stop(sprintf("Feature %d must have 'key' and 'value' elements", i))
    }
  }
  
  message(sprintf("Querying %d feature type(s) across %dx%d grid", 
                  length(features_list), grid_size, grid_size))
  
  # Create grid
  grid <- st_make_grid(country_geom, n = c(grid_size, grid_size))
  grid_sf <- st_sf(id = 1:length(grid), geometry = grid)
  
  # Store results for each feature type
  all_feature_results <- list()
  
  # Loop through each feature type
  for(feat_idx in seq_along(features_list)) {
    key <- features_list[[feat_idx]]$key
    value <- features_list[[feat_idx]]$value
    
    message(sprintf("\n=== Feature Type %d/%d: key='%s', value='%s' ===", 
                    feat_idx, length(features_list), 
                    key, paste(value, collapse=", ")))
    
    results <- list()
    successful <- 0
    failed <- 0
    
    # Loop through each grid cell
    for(i in 1:nrow(grid_sf)) {
      message(sprintf("  Grid %d/%d...", i, nrow(grid_sf)))
      
      bbox_i <- st_bbox(grid_sf[i,])
      
      # Thorough bbox validation
      if(anyNA(bbox_i)) {
        message("    Skipping: bbox contains NA")
        failed <- failed + 1
        next
      }
      
      if(bbox_i["xmax"] <= bbox_i["xmin"] || bbox_i["ymax"] <= bbox_i["ymin"]) {
        message("    Skipping: invalid bbox extent")
        failed <- failed + 1
        next
      }
      
      # Check bbox isn't too small
      if(abs(bbox_i["xmax"] - bbox_i["xmin"]) < 0.001 || 
         abs(bbox_i["ymax"] - bbox_i["ymin"]) < 0.001) {
        message("    Skipping: bbox too small")
        failed <- failed + 1
        next
      }
      
      # Try the query with retries
      attempt <- 1
      max_attempts <- 3
      success <- FALSE
      
      while(attempt <= max_attempts && !success) {
        tryCatch({
          # Build query step by step
          query <- opq(bbox_i, timeout = timeout)
          
          if(is.null(query)) {
            stop("opq() returned NULL")
          }
          
          query <- add_osm_feature(query, key = key, value = value)
          
          if(is.null(query)) {
            stop("add_osm_feature() returned NULL")
          }
          
          result <- osmdata_sf(query)
          
          if(!is.null(result) && !is.null(result$osm_points) && 
             nrow(result$osm_points) > 0) {
            
            # Add feature type info
            result$osm_points$feature_key <- key
            result$osm_points$feature_value <- paste(value, collapse=",")
            
            # Keep specified columns plus feature info
            cols_to_keep <- intersect(keep_columns, names(result$osm_points))
            cols_to_keep <- unique(c(cols_to_keep, "feature_key", "feature_value"))
            
            if(length(cols_to_keep) > 0) {
              results[[i]] <- result$osm_points %>% 
                select(all_of(cols_to_keep), geometry)
            } else {
              # Minimum columns
              results[[i]] <- result$osm_points %>%
                select(osm_id, feature_key, feature_value, geometry)
            }
            
            successful <- successful + 1
            message(sprintf("    Success: %d features found", nrow(result$osm_points)))
            success <- TRUE
            
          } else {
            message("    No features found")
            success <- TRUE  # Not an error, just no data
          }
          
        }, error = function(e) {
          message(sprintf("    Attempt %d failed: %s", attempt, e$message))
          
          if(attempt < max_attempts) {
            wait_time <- attempt * 3
            message(sprintf("    Retrying in %d seconds...", wait_time))
            Sys.sleep(wait_time)
          } else {
            message("    All attempts failed, moving to next grid")
            failed <- failed + 1
          }
          
          attempt <<- attempt + 1
        })
      }
      
      # Delay between grid cells
      if(i < nrow(grid_sf)) {
        Sys.sleep(5)
      }
    }
    
    message(sprintf("  Feature type completed: %d successful, %d failed", 
                    successful, failed))
    
    # Combine results for this feature type
    if(length(results) > 0) {
      results <- results[!sapply(results, is.null)]
      
      if(length(results) > 0) {
        tryCatch({
          combined <- bind_rows(results)
          
          if(nrow(combined) > 0) {
            combined <- combined[!duplicated(combined$osm_id), ]
            all_feature_results[[feat_idx]] <- combined
            message(sprintf("  Total unique features for this type: %d", nrow(combined)))
          }
        }, error = function(e) {
          message(sprintf("  Error combining results: %s", e$message))
        })
      }
    }
    
    # Delay between feature types
    if(feat_idx < length(features_list)) {
      message("\n  Pausing before next feature type...")
      Sys.sleep(10)
    }
  }
  
  # Combine all feature types
  message("\n=== Combining all feature types ===")
  
  if(length(all_feature_results) > 0) {
    tryCatch({
      final_combined <- bind_rows(all_feature_results)
      
      if(nrow(final_combined) > 0) {
        # Remove duplicates (same feature might be in multiple categories)
        final_combined <- final_combined[!duplicated(final_combined$osm_id), ]
        message(sprintf("Total unique features across all types: %d", nrow(final_combined)))
        return(final_combined)
      }
    }, error = function(e) {
      message(sprintf("Error in final combination: %s", e$message))
      message("Returning list of results by feature type instead")
      return(all_feature_results)
    })
  }
  
  message("No results found")
  return(NULL)
}
