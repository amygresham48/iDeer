#WOODLAND PIXEL EXTRACTION ####
#Do this at the pixel level

# Filter for woodland patches that intersect buffers
woods_buffers <- st_filter(wood_polys, buffered_woods_5km, .predicate = st_intersects)
plot(woods_buffers$geometry)

# Initialize progress bar
pb <- progress_bar$new(
  format = "[:bar] :current/:total (:percent) in :elapsed, eta: :eta",
  total = length(woods_buffers),
  clear = FALSE,
  width = 60
)

      for (j in nrow(woods_buffers)) {

  # Crop land cover map by the woodland polygon
  fcrop <- crop(map, woods_buffers[j, ])
  
  # Mask the cropped land cover map
  fmask <- mask(fcrop, woods_buffers[j, ])
  
  # Get extent of fmask
  fmask_extent <- as(extent(st_bbox(fmask)), "Extent")
  
  # Get the cell numbers within the extent
  pixel_ID <- cellsFromExtent(map, fmask_extent)
  
  # Get the coordinates for these cells
  cell_coords <- as.data.frame(xyFromCell(map, pixel_ID))
  
  # Extract pixel values
  pixel_values <- extract(fmask, cell_coords)
  
  # Ensure that there are pixel values before proceeding
  if (length(pixel_values) > 0) {
    # Create a data frame with the extracted values and cell indices
    vals <- data.frame(pixel_ID = pixel_ID,
                       class = pixel_values,
                       patch_ID = woods_buffers$patch_ID[j],
                       x = cell_coords$x,
                       y = cell_coords$y)
    
    # Filter out NA values
    vals <- vals[!is.na(vals$class), ]
  } else {
    # If no values, create an empty data frame with the same structure
    vals <- data.frame(pixel_ID = integer(0),
                       class = integer(0),
                       patch_ID = integer(0),
                       x = numeric(0),
                       y = numeric(0))
  }
}
  
write.csv(vals, here("data/derived-data/Tool-extract-function-test/woodland_type_pixelIDs_5km.csv"))

#Plot
ggplot(vals) +
  geom_tile(aes(x = x, y = y, fill = class)) +
  scale_fill_viridis_c(option = "plasma") +  # Apply a continuous viridis color scale
  theme_minimal() +
  guides(fill = guide_colorbar(barwidth = 1, barheight = 10)) +  # Customize the colorbar appearance
  labs(title = "Land cover",
       fill = "Pixel class")  # Adjust the legend title to be more descriptive