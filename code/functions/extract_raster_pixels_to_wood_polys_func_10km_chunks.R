extract_raster_to_wood_polygons <- function(risk_map, tiles, wood_polys) {
  
  # List to store extracted data from all tiles
  extraction_results <- list()
  
  # Initialize a progress bar with estimated time remaining
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

    # crop risk raster current tile using tryCatch
    cropped_raster <- tryCatch(
      crop(risk_map, tile),
      error = function(e) {
        if (grepl("extents do not overlap", e$message)) {
          message(paste("Skipping tile", i, ": extents do not overlap."))
          return(NULL)
        } else {
          stop(e)  # For other errors, stop execution
        }
      }
    )
    
    # If raster empty, skip
    if (is.null(cropped_raster)) {
      next
    }
    
    # get woodland polygons within current tile using tryCatch
    wood_polys_tile <- tryCatch(
      st_filter(wood_polys, tile),
      error = function(e) {
        if (grepl("extents do not overlap", e$message)) {
          message(paste("Skipping tile", i, ": extents do not overlap."))
          return(NULL)
        } else {
          stop(e)  # For other errors, stop execution
        }
      }
    )
    
    # If no woods in this tile, skip
    if (is.null(wood_polys_tile)) {
      next
    }
    
    #Matt's code:
    #will tell me how many of each pixel is in each polygon
    risk_map_poly_vals <- raster::extract(cropped_raster, wood_polys_tile, df = TRUE)
    
    #get row number and patch_ID from wood_poly_tiles
    wood_tiles_IDs <- data.frame(ID = rep(seq_len(nrow(wood_polys_tile))),
                                patch_ID = wood_polys_tile$patch_ID)
    
    # If no pixel values, skip the tile
    if (is.null(cropped_raster)) {
      next
    }
    
    # Get name of map
    raster_name <- deparse(substitute(value))
    
    # Create a data frame with the extracted values and patch_IDs
    
    colnames(risk_map_poly_vals)[2] <- "value"
    
    vals <- data.frame(
      ID = risk_map_poly_vals$ID,
      raster_values = risk_map_poly_vals$value  # Extracted raster values
    )
    
    #left_join the patch_IDs to this df
    
    vals <- left_join(vals, wood_tiles_IDs, by = c("ID"))
    vals <- vals %>% dplyr::select(-c("ID"))
    
    # Filter out NA values
    vals <- vals[!is.na(vals$raster_values), ]
    
    #Summarise dataframe: get mean Focal1000_wood_area for each polygon
    
    summary_df <- vals %>%
      group_by(patch_ID) %>%
      summarise(mean_value = mean(raster_values, na.rm = TRUE))
      
    #Join geometry to extracted values
    geometry_tiles <- st_filter(GB_polys_merged, ew10k[i,])
    merged_df <- left_join(summary_df, geometry_tiles, by = c("patch_ID"))
    merged_df <- merged_df %>% filter(!st_is_empty(geometry))
    merged_df <- st_as_sf(merged_df)
    
    # Rename raster_values column to the map name
    names(merged_df)[names(merged_df) == "mean_value"] <- paste0("mean_", raster_name)
    
    # Append the current tile's results to the overall list
    extraction_results[[i]] <- merged_df
  }
  
  # Combine all results into a single data frame
  result_df <- bind_rows(extraction_results)
  
  return(result_df)
}
