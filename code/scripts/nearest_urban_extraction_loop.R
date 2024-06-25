#NEAREST URBAN EXTRACTION ####

extract_urban_rasters <- function(buffered_woods_5km, raster_layer, land_cover_raster) {
  
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
    vals <- data.frame(nearest_urban = pixel_values, 
                       patch_ID = buffered_woods_5km$patch_ID[i],
                       pixel_ID = pixel_ID,
                       x = cell_coords$x,
                       y = cell_coords$y)
    
    # Filter out NA values
    vals <- vals[!is.na(vals$nearest_urban), ]
    
    # Append to the results list
    extraction_results[[i]] <- vals
  }
  
  # Combine all results into a single dataframe
  result_df <- bind_rows(extraction_results)
  
  return(result_df)
}

nearest_urban_extraction <- extract_urban_rasters(buffered_woods_5km=buffered_woods_5km, 
raster_layer=nearest_urban,
land_cover_raster=map)

write.csv(nearest_urban_extraction, here("data/derived-data/Tool-extract-function-test/nearest_urban_extraction_pixelIDs_5km.csv"))