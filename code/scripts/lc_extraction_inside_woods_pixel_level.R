#WOODLAND PIXEL EXTRACTION ####
#Do this at the pixel level

woodpixels <- function(POLY, veg, i) {
  # Crop land cover map by the woodland polygon
  fcrop <- crop(veg, extent(POLY[i, ]))
  
  # Mask the cropped land cover map
  fmask <- mask(fcrop, POLY[i, ])
  
  # Get extent of fmask
  fmask_extent <- extent(fmask)
  
  # Get the cell numbers within the extent
  pixel_ID <- cellsFromExtent(veg, fmask_extent)
  
  # Get the coordinates for these cells
  cell_coords <- as.data.frame(xyFromCell(veg, pixel_ID))
  
  # Extract pixel values
  pixel_values <- extract(fmask, cell_coords)
  
  # Ensure that there are pixel values before proceeding
  if (length(pixel_values) > 0) {
    # Create a data frame with the extracted values and cell indices
    vals <- data.frame(pixel_ID = pixel_ID,
                       lc_class = pixel_values,
                       patch_ID = POLY@data$patch_ID[i],
                       x = cell_coords$x,
                       y = cell_coords$y)
    
    # Filter out NA values
    vals <- vals[!is.na(vals$lc_class), ]
  } else {
    # If no values, create an empty data frame with the same structure
    vals <- data.frame(pixel_ID = integer(0),
                       lc_class = integer(0),
                       patch_ID = integer(0),
                       x = numeric(0),
                       y = numeric(0))
  }
  
  return(vals)
}

# Filter for woodland patches that intersect buffers
woods_buffers <- st_filter(hab_patches_all, buffered_woods_10km, .predicate = st_intersects)

# Convert to spatial object
POLY <- as(woods_buffers[1:50, ], "Spatial")
veg <- map

# Initialize progress bar
pb <- progress_bar$new(
  format = "[:bar] :current/:total (:percent) in :elapsed, eta: :eta",
  total = length(POLY),
  clear = FALSE,
  width = 60
)

# Process each polygon and collect results
woodpix <- ldply(1:length(POLY), function(i) {  # for each polygon
  pb$tick()  # Update the progress bar
  out <- woodpixels(POLY, veg = veg, i = i)  # compute metrics
  return(out)
})

write.csv(woodpix, here("data/derived-data/Tool-extract-function-test/woodland_type_pixelIDs.csv"))