#For the focal woodland AND each woodland in the 5km landscape
#Identify woodlands in the 5km landscape
#For each woodland polygon, buffer by 1km
#For each pixel within that 1km, identify the mean forage quality value
#Stick the results into one dataframe

polyFragStats <- function(buffered_woods_10km, hab_patches_all, veg, buffer_width = 1000) {
  # Initialize a list to store results for each polygon
  extraction_results <- list()
  
  # Initialize progress bar
  pb <- progress_bar$new(
    format = "[:bar] :current/:total (:percent) in :elapsed, eta: :eta",
    total = length(buffered_woods_10km),
    clear = FALSE,
    width = 60
  )
  
  # Loop through each buffered polygon
  for (i in seq_len(nrow(buffered_woods_10km))) {
    pb$tick()  # Update the progress bar
    
    # Filter for woodland patches that intersect buffer
    woods_buffers <- st_filter(hab_patches_all, st_geometry(buffered_woods_10km[i, ]), .predicate = st_intersects)
    
    # Convert to spatial object if there are any intersecting patches
    if (nrow(woods_buffers) > 0) {
      POLY <- as(woods_buffers, "Spatial")
      
      for (j in seq_len(length(POLY))) {
        # Create a buffer around the polygon
        buffer_poly <- buffer(POLY[j, ], width = buffer_width, byid = TRUE)
        
        # Cut out the original polygon from the buffer
        buffer_area <- erase(buffer_poly, POLY[j, ])
        
        # Crop land cover map by the buffer area
        fcrop <- crop(veg, buffer_area)
        
        # Mask the cropped land cover map
        fmask <- mask(fcrop, buffer_area)
        
        # Get the cell numbers within the extent
        buffer_extent <- extent(fmask)
        pixel_ID <- cellsFromExtent(veg, buffer_extent)
        
        # Get the coordinates for these cells
        cell_coords <- as.data.frame(xyFromCell(veg, pixel_ID))
        
        # Extract pixel values
        pixel_values <- extract(fmask, cell_coords)
        
        # Ensure that there are pixel values before proceeding
        if (!is.null(pixel_values) && length(pixel_values) == nrow(cell_coords)) {
          # Create a data frame with the extracted values and cell indices
          vals <- data.frame(pixel_ID = pixel_ID,
                             forage_q = pixel_values,
                             patch_ID = POLY@data$patch_ID[j],
                             x = cell_coords$x,
                             y = cell_coords$y)
          
          # Filter out NA values
          vals <- vals[!is.na(vals$lc_class), ]
          
          # Append to the results list
          extraction_results[[length(extraction_results) + 1]] <- vals
        }
      }
    }
  }
  
  # Combine all results into a single dataframe
  if (length(extraction_results) > 0) {
    result_df <- do.call(rbind, extraction_results)
  } else {
    result_df <- data.frame()
  }
  
  return(result_df)
}

respoly <- polyFragStats(buffered_woods_10km=buffered_woods_10km, hab_patches_all=hab_patches_all, veg=map)

write.csv(respoly, here("data/derived-data/Tool-extract-function-test/landscapemetrics_1000_pixelIDs.csv"))
