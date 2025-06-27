extract_raster_to_wood_polygons <- function(risk_map, tiles, wood_polys_chunked) {
  
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
    
    # Buffer tile by 2000m
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
    
    # Retrieve pre-chunked woodland polygons for this tile
    wood_polys_tile <- wood_polys_chunked[[i]]
    
    # Skip if no woodland polygons in the tile
    if (is.null(wood_polys_tile) || nrow(wood_polys_tile) == 0) {
      next
    }
    
    # Extract raster values for polygons
    risk_map_poly_vals <- tryCatch(
      raster::extract(cropped_raster, wood_polys_tile, fun = mean, na.rm = TRUE, df = TRUE),
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
    
    # Add patch_IDs based on the extract() ID mapping back to wood_polys_tile
    risk_map_poly_vals <- risk_map_poly_vals %>%
      mutate(patch_ID = wood_polys_tile$patch_ID[ID]) %>%
      dplyr::select(patch_ID, mean_risk = 2)
    
    # Join geometry with summarized data
    merged_df <- left_join(risk_map_poly_vals, wood_polys_tile, by = "patch_ID") %>%
      filter(!st_is_empty(geometry)) %>%
      st_as_sf()
    
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
