#EXTRACT PIXEL VALUES FROM RASTER LAYER CONTAINING MAX DAMS SCORES WITHIN 1KM OF EACH PIXEL ####

extract_dams_raster <- function(fivekm_buffer, raster_layer, land_cover_raster) {
  
  # List to store extracted data
  extraction_results <- list()
  # Loop through each buffered buffer and crop all rasters
  buffer_geometry <- fivekm_buffer$geometry

  # Convert buffer geometry to a spatial object that raster can use
  buffer_extent <- as(extent(st_bbox(buffer_geometry)), "Extent")
  
 # Get the cell numbers within the extent
pixel_ID <- cellsFromExtent(land_cover_raster, buffer_extent)

# Get the coordinates for these cells
cell_coords <- as.data.frame(xyFromCell(land_cover_raster, pixel_ID))

#Extract pixel values
pixel_values <- extract(raster_layer, cell_coords)

    # Create a data frame with the extracted values and cell indices
    vals <- data.frame(max_dams = pixel_values, #The max dams score
                       pixel_ID = pixel_ID,
                       x = cell_coords$x,
                       y = cell_coords$y)
    
    # Filter out NA values
    vals <- vals[!is.na(vals$max_dams), ]
    
    # Append to the results list
    extraction_results<- vals
  
  
  # Combine all results into a single dataframe
  result_df <- bind_rows(extraction_results)
  
  return(result_df)
}
