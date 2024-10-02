extract_raster <- function(map_reclass, lcm, tiles) {
  
  # List to store extracted data from all tiles
  extraction_results <- list()
  
  # Loop through each 10km tile
  for (i in seq_along(tiles)) {
    tile <- tiles[i, ]  # Extract the ith row, which is an sf object
    
    # Get the extent of the current tile geometry
    tile_extent <- st_bbox(tile)
    
    # Convert tile_extent to a raster extent
    raster_extent <- extent(tile_extent[c("xmin", "xmax", "ymin", "ymax")])
    
    # Get the cell numbers within the extent of the current tile
    pixel_ID <- cellsFromExtent(map_reclass, raster_extent)
    
    # Get the coordinates for these cells
    cell_coords <- as.data.frame(xyFromCell(map_reclass, pixel_ID))
    
    # Extract pixel values for the current tile
    pixel_values <- extract(map_reclass, cell_coords)
    
    # Get name of map_reclass
    raster_name <- deparse(substitute(map_reclass))
    
    # Create a data frame with the extracted values and cell indices
    vals <- data.frame(raster_values = pixel_values, # Raster values
                       pixel_ID = pixel_ID,
                       x = cell_coords$x,
                       y = cell_coords$y)
    
    # Filter out NA values
    vals <- vals[!is.na(vals$raster_values), ]
    
    # Rename raster_values column to raster_name
    names(vals)[names(vals) == "raster_values"] <- raster_name
    
    # Append the current tile's results to the overall list
    extraction_results[[i]] <- vals
  }
  
  # Combine all results into a single dataframe
  result_df <- bind_rows(extraction_results)
  
  return(result_df)
}