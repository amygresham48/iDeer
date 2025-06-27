extract_raster_to_wood_polygons <- function(risk_map, tiles, wood_polys) {
  
  #risk_map <- terra::rast(risk_map)
  
  # List to store extracted data from all tiles
  extraction_results <- list()
  problematic_tiles <- c()  # Initialize a vector to store problematic tiles
  
  # Get the name of the risk map
  raster_name <- deparse(substitute(risk_map))
  
  # Initialize a progress bar
  pb <- progress_bar$new(
    format = "  Extracting [:bar] :percent | Elapsed: :elapsed | ETA: :eta",
    total = nrow(tiles), 
    clear = FALSE, 
    width = 60
  )
  
  # Loop through each 10km tile
  for (i in seq_len(nrow(tiles))) {
    pb$tick()  # Update the progress bar
    
    tile <- tiles[i, ]  # Extract the ith row (tile) as an sf object
    
    #buffer tile by 200m
    
    tile_buf <- st_buffer(tile, 2000)
    
    # Crop the risk raster to the current tile
    cropped_raster <- tryCatch(
      crop(risk_map, tile_buf),
      error = function(e) {
        message(paste("Skipping tile", i, "- extents do not overlap:", e$message))
        return(NULL)
      }
    )
    
    # Skip if the cropped raster is empty
    if (is.null(cropped_raster)) {
      next
    }
    
    # Filter woodland polygons within the current tile
    wood_polys_tile <- tryCatch(
      st_filter(wood_polys, tile),
      error = function(e) {
        message(paste("Skipping tile", i, "- error during st_filter:", e$message))
        return(NULL)
      }
    )
    
    # Skip if no woodland polygons in the tile
    if (is.null(wood_polys_tile) || nrow(wood_polys_tile) == 0) {
      next
    }
    # Extract raster values for polygons
    risk_map_poly_vals <- tryCatch(
      raster::extract(cropped_raster, wood_polys_tile, fun=mean, na.rm=TRUE,df=TRUE),
      error = function(e) {
        message(paste("Tile", i, "caused an error during extraction:", e$message))
        problematic_tiles <<- c(problematic_tiles, i)
        return(NULL)
      }
    )
    
    # Skip if extraction fails
    if (is.null(risk_map_poly_vals)) {
      next
    }
    
    # risk_map_poly_vals <- risk_map_poly_vals %>%
    #   rename(patch_ID = ID,
    #          mean_risk = layer) #this should be category for small deer - need to figure out why there is a difference
    
    colnames(risk_map_poly_vals)[1:2] <- c("patch_ID", "mean_risk")
    wood_polys_tile <- wood_polys_tile %>% rename(patch_ID = OBJECTID)
    
    risk_map_poly_vals$patch_ID <- wood_polys_tile$patch_ID
    
    # # Prepare patch_ID mapping
    # wood_tiles_IDs <- data.frame(
    #   ID = seq_len(nrow(wood_polys_tile)),
    #   patch_ID = wood_polys_tile$patch_ID
    # )
    
    # # Create a data frame with extracted values and patch_IDs
    # vals <- data.frame(
    #   ID = risk_map_poly_vals$ID,
    #   raster_values = risk_map_poly_vals[, 2]  # Second column contains raster values
    # ) %>%
    #   left_join(wood_tiles_IDs, by = "ID") %>%
    #   dplyr::select(-ID) %>%
    #   filter(!is.na(raster_values))  # Filter out NA values
    # 
    # # Summarize: Calculate mean raster values per patch_ID
    # summary_df <- vals %>%
    #   group_by(patch_ID) %>%
    #   summarise(mean_value = mean(raster_values, na.rm = TRUE), .groups = "drop")
    
    
    
    # Get geometry for the current tile
    #geometry_tiles <- st_filter(wood_polys, tile)
    
    # Join geometry with summarized data
    merged_df <- left_join(risk_map_poly_vals, wood_polys_tile, by = "patch_ID") %>%
      filter(!st_is_empty(geometry)) %>%
      st_as_sf()
    
    # # Rename the mean value column dynamically
    # names(merged_df)[names(merged_df) == "mean_value"] <- paste0("mean_", raster_name)
    
    # Append the results to the overall list
    extraction_results[[i]] <- merged_df
  }
  
  # Combine all results into a single data frame
  result_df <- bind_rows(extraction_results)
  
  # Print problematic tiles if any
  if (length(problematic_tiles) > 0) {
    message("Problematic tiles:", paste(problematic_tiles, collapse = ", "))
  }
  
  return(list(result = result_df, problematic_tiles = problematic_tiles))
  
}
