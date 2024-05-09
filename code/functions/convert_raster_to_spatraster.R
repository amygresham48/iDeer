# Function to convert 'RasterLayer' to 'SpatRaster'
convert_to_spatraster <- function(raster) {
  if (!inherits(raster, "SpatRaster")) {  # If not already 'SpatRaster'
    terra::rast(raster)  # Convert to 'SpatRaster'
  } else {
    raster  # If it's already a 'SpatRaster'
  }
}

# Function to apply conversion to all elements in a sub-list
convert_sublist <- function(sublist) {
  lapply(sublist, convert_to_spatraster)  # Apply conversion to each item in the sub-list
}

# Convert each sub-list in the main list
buffered_cropped_rasters_spat <- lapply(buffered_cropped_rasters, convert_sublist)