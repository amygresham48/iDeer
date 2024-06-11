{r}
#ROAD DENSITY EXTRACTION AT PIXEL LEVEL ####
extract_road_raster <- function(buffered_woods_5km, raster_layer, land_cover_raster) {
  
  # Initialize progress bar
  pb <- progress_bar$new(
    format = "[:bar] :current/:total (:percent) in :elapsed, eta: :eta",
    total = nrow(buffered_woods_5km),
    clear = FALSE,   # Keep the progress bar on the screen
    width = 60
  )
  
  # List to store extracted data
  extraction_results <- list()
  
  # Loop through each buffered buffer and crop all rasters
  for (i in seq_along(buffered_woods_5km$geometry)) {
    buffer_geometry <- buffered_woods_5km$geometry[i]
    pb$tick()  # Update the progress bar

    # Convert buffer geometry to a spatial object that raster can use
    buffer_extent <- as(extent(st_bbox(buffer_geometry)), "Extent")
    
     # Get the cell numbers within the extent
pixel_ID <- cellsFromExtent(land_cover_raster, buffer_extent)

# Get the coordinates for these cells
cell_coords <- as.data.frame(xyFromCell(land_cover_raster, pixel_ID))

#Extract pixel values
pixel_values <- extract(raster_layer, cell_coords)

    # Create a data frame with the extracted values and cell indices
    vals <- data.frame(roads = pixel_values, 
                       patch_ID = buffered_woods_5km$patch_ID[i],
                       pixel_ID = pixel_ID,
                       x = cell_coords$x,
                       y = cell_coords$y)
    
    # Filter out NA values
    vals <- vals[!is.na(vals$roads), ]
    
    # Append to the results list
    extraction_results[[i]] <- vals
  }
  
  # Combine all results into a single dataframe
  result_df <- bind_rows(extraction_results)
  
  return(result_df)
}

# Use the function and save the results
road_extraction <- extract_road_raster(buffered_woods_5km=buffered_woods_5km, 
raster_layer=all_roads, 
land_cover_raster=map)

write.csv(road_extraction, here("data/derived-data/Tool-extract-function-test/road_density_extraction_250_pixelIDs_5km.csv"))