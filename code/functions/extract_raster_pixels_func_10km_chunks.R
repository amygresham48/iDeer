extract_raster <- function(map, lcm, tiles) {
  
  # List to store extracted data from all tiles
  extraction_results <- list()
  
  # Loop through each 10km tile
  for (i in seq_len(nrow(tiles))) {
    tile <- tiles[i, ]  # Extract the ith row (tile) as an sf object
    
    # Get the extent of the current tile geometry
    tile_extent <- st_bbox(tile)
    
    # Convert tile_extent to a raster extent
    raster_extent <- extent(tile_extent[c("xmin", "xmax", "ymin", "ymax")])
    
    # Crop the raster to the extent of the current tile using tryCatch
    cropped_raster <- tryCatch(
      crop(map, raster_extent),
      error = function(e) {
        if (grepl("extents do not overlap", e$message)) {
          message(paste("Skipping tile", i, ": extents do not overlap."))
          return(NULL)
        } else {
          stop(e)  # For other errors, stop execution
        }
      }
    )
    
    # If the cropped raster is empty, skip this tile
    if (is.null(cropped_raster)) {
      next
    }
    
    # Get the coordinates for the cropped raster cells
    cell_coords <- as.data.frame(xyFromCell(cropped_raster, 1:ncell(cropped_raster)))
    
    # Extract pixel values for the cropped raster
    pixel_values <- getValues(cropped_raster)
    
    # If no pixel values, skip the tile
    if (is.null(pixel_values)) {
      next
    }
    
    # Get name of map
    raster_name <- deparse(substitute(map))
    
    # Create a data frame with the extracted values and cell indices
    vals <- data.frame(
      raster_values = pixel_values,  # Extracted raster values
      x = cell_coords$x,
      y = cell_coords$y
    )
    
    # Filter out NA values
    vals <- vals[!is.na(vals$raster_values), ]
    
    # Rename raster_values column to the map name
    names(vals)[names(vals) == "raster_values"] <- raster_name
    
    # Append the current tile's results to the overall list
    extraction_results[[i]] <- vals
  }
  
  # Combine all results into a single data frame
  result_df <- bind_rows(extraction_results)
  
  return(result_df)
}