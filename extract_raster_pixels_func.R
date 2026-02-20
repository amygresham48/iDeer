#EXTRACT PIXEL VALUES FROM RASTER LAYER PIXELS ####

extract_raster <- function(map_reclass, lcm) {
  
  # List to store extracted data
  extraction_results <- list()
  
  # Get the cell numbers within the extent
  pixel_ID <- cellsFromExtent(map_reclass, extent(st_bbox(lcm)))
  
  # Get the coordinates for these cells
  cell_coords <- as.data.frame(xyFromCell(map_reclass, pixel_ID))
  
  # Extract pixel values
  pixel_values <- raster::extract(map_reclass, cell_coords)
  
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
  
  # Append to the results list
  extraction_results <- list(vals)
  
  # Combine all results into a single dataframe
  result_df <- bind_rows(extraction_results)
  
  return(result_df)
}