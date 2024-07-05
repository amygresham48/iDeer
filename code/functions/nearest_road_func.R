#NEAREST MAIN ROAD FUNCTION ####

#This function produces a raster layer.
#Pixels with values are inside woodlands (no other land cover type)
#Pixel value = nearest main road (A road, B road, motorway)

nearest_road <- function(lcm, wood_binary_rast, roads, uk10k, EW) {
  
bng=27700

#make template raster
template_raster <- raster(ext = extent(lcm), res = res(lcm), crs = bng)

#Filter roads for motorways, A roads and B roads
roads_subset <- roads %>%
  mutate(unique_ID = id,
         class = road_classification) %>%
  dplyr::select(unique_ID, class) %>%
  filter(class %in% c("Motorway","A Road", "B Road"))

# NEAREST MAIN ROAD FEATURE LOOP ####

tiles <- ew10k

# Create a progress bar
pb <- progress_bar$new(
  format = "[:bar] :percent Elapsed: :elapsed Time remaining: :eta",
  total = nrow(tiles)
)

# Make empty list to store raster tiles
nearest_road_list <- list()

for (i in 1:length(tiles$tile_name)) { 
  tile <- tiles[i,]
  chunked.wood <- crop(nfi.lcm.2015, tile)
  chunked.wood[chunked.wood == 0] <- NA #make zeros NAs
  chunked.wood.points <- rasterToPoints(chunked.wood, spatial = TRUE) #Raster to point
  chunked.wood.points <- st_as_sf(chunked.wood.points, crs = bng)
  chunked.wood.points <- sf::st_transform(chunked.wood.points, crs = bng)
  
  if (nrow(chunked.wood.points) > 0) {
    # Find the minimum distance from each point to the nearest road parcel
    nearest_distances <- st_nearest_feature(chunked.wood.points, road_subset, check_crs = TRUE)
    # Extract nearest distances
    nearest <- st_distance(chunked.wood.points, road_subset[nearest_distances,], by_element = TRUE)
    # Add to chunked.wood.points
    chunked.wood.points$nearest_road <- nearest
    # Make template raster
    template_raster <- crop(lcm2015.rast, tile)
    # Rasterize these data
    road_dist_raster <- rasterize(chunked.wood.points, template_raster, field = "nearest_road")
    # Save to list
    nearest_road_list[[i]] <- road_dist_raster
    
  } else {
    nearest_road_list[[i]] <- NA
    
  }
  
  pb$tick()  # Increment the progress bar
  
}

pb$close()  # Close the progress bar

print("road raster list complete")

# Save list
saveRDS(nearest_road_list, file = here("outputs/nearest_road_raster_list_nfi_lcm_tiles.rds"))


# Label elements that are not RasterLayers or are empty
valid_road_rasters <- lapply(nearest_road_list, function(raster_layer) {
  if (inherits(raster_layer, "RasterLayer") && any(!is.na(values(raster_layer)))) {
    return(raster_layer)
  } else {
    return(NULL)
  }
})

# Remove NULL elements from the list
valid_road_rasters <- Filter(function(x) !is.null(x), valid_road_rasters)

# Mosaic raster
road_distance_mosaic <- do.call(mosaic, c(valid_road_rasters, fun = mean))

#ensure crs is bng

crs(road_distance_mosaic) <- bng

# Save raster
writeRaster(road_distance_mosaic, here("outputs/nearest_road_raster_all_tiles.tif"))
print("road raster mosaic complete and saved")
#print("road raster list saved")

}