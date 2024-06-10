#MAX DAMS EXTRACTION ####
#At patch level, as this is just the maximum DAMS in the surrounding landscape
#The pixel level dams extraction is not lining up with the rest of the extractions - pixel IDs don't match
extract_dams_raster <- function(buffered_woods_5km, raster_layer, woodlands) {
  
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
  
  # Crop raster to the current buffer extent
  dams_cropped <- crop(dams, buffer_extent)
  
  #Convert to spatraster
  dams_spatraster <- terra::rast(dams_cropped)
  
  #Filter for woodland patches that intersect buffer
  
  woods_buffer <- st_filter(woodlands, buffered_woods_5km[i, ], .predicate = st_intersects)
  
    # Extract the average maximum dams for all woodland polygons within the current buffer
    extraction <- terra::extract(dams_spatraster, vect(woods_buffer), fun = mean, na.rm = TRUE)
    
    #cbind to the original woods_buffer columns
    
    extraction_cbind <- cbind(woods_buffer, extraction)
    
    # Store the extracted data in the results list
    extraction_results[[paste("Buffer", i)]] <- extraction_cbind
  }
  
  # Combine all extracted data into a single data frame
 dams_extraction_results <- bind_rows(extraction_results)
  
  return(dams_extraction_results) # Return the combined data frame
}


dams_extraction <- extract_dams_raster(
  buffered_woods_5km=buffered_woods_5km, 
  raster_layer=dams, 
  woodlands=woods_buffers)

#ggplot(dams_extraction) +
  #geom_sf(aes(fill = focal.dams.1000.max))

write.csv(dams_extraction, here("data/derived-data/Tool-extract-function-test/DAMS_MAX_extraction_1000_patch_level_5km.csv"))