#Define British National Grid crs
bng=27700

# England and Wales 10k squares
ew10k <- sf::st_read(here("data/raw-data/os_bng_grids.gpkg"),layer="10km_grid")

lcm_vector <- sf::st_read("data/raw-data/lcm-2023-vector/lcm-2023-vec_5670267.gpkg")

#Get urban/suburban parcels
urb_suburb <- lcm_vector
#filter out everything except urban and suburban
urb_suburb[urb_suburb$category < 20] <- NA
#save
st_write(urb_suburb,here("output/urban_suburban_parcels_GB_2023.shp"))

urban_polygons <- st_as_sf(urban_polygons)
#urban_polygons<-sf::st_transform(urban_polygons,crs=bng)
urban_parcels <- urban_polygons

#Binary woodland raster
wood_binary_rast <- raster("data/derived-data/LCM2023WOODSGB.tif")
crs(wood_binary_rast) <- bng

nearest_urb_suburb <- function(wood_binary_rast, urban_parcels, tiles) {

#-------------------------------------------------

# NEAREST URBAN FEATURE LOOP ####
  
tiles <- ew10k[7501,]

#tiles <- tiles[1:2,]

# Make empty list to store raster tiles
nearest_urban_suburban_list <- list()

for (i in 1:length(tiles$tile_name)) { 
  tile <- tiles[i,] #select tile
  
  #Make points from woodland raster pixels
  chunked.wood <- crop(wood_binary_rast, tile)
  chunked.wood[chunked.wood == 0] <- NA #make zeros NAs
  chunked.wood.points <- rasterToPoints(chunked.wood, spatial = TRUE) #Raster to point
  chunked.wood.points <- st_as_sf(chunked.wood.points, crs = bng)
  chunked.wood.points <- sf::st_transform(chunked.wood.points, crs = bng)
  
  if (nrow(chunked.wood.points) > 0) {
    # Find the minimum distance from each point to the nearest urban parcel
    nearest_distances <- st_nearest_feature(chunked.wood.points, urban_parcels, check_crs = TRUE)
    # Extract nearest distances
    nearest <- st_distance(chunked.wood.points, urban_parcels[nearest_distances,], by_element = TRUE)
    # Add to chunked.wood.points
    chunked.wood.points$nearest_urban <- nearest
    # Make template raster
    template_raster <- crop(nfi_lcm_map, tile)
    # Rasterize these data
    urban_dist_raster <- rasterize(chunked.wood.points, template_raster, field = "nearest_urban")
    # Save to list
    nearest_urban_list[[i]] <- urban_dist_raster
    
  } else {
    nearest_urban_suburban_list[[i]] <- NA
    
  }
  
}


print("Urban raster list complete")

# Save list
saveRDS(nearest_urban_list, file = here("output/nearest_urban_suburban_raster_tile_list_EW_2022.rds"))
print("Urban raster list saved")

# Label elements that are not RasterLayers or are empty
valid_urban_rasters <- lapply(nearest_urban_list, function(raster_layer) {
  if (inherits(raster_layer, "RasterLayer") && any(!is.na(values(raster_layer)))) {
    return(raster_layer)
  } else {
    return(NULL)
  }
})

# Remove NULL elements from the list
valid_urban_rasters <- Filter(function(x) !is.null(x), valid_urban_rasters)

# Mosaic raster
urban_distance_mosaic <- do.call(mosaic, c(valid_urban_rasters, fun = mean))

#ensure crs is bng

crs(urban_distance_mosaic) <- bng

# Save raster
writeRaster(urban_distance_mosaic, here("output/nearest_urban_suburban_raster_2022_EW.tif"),overwrite=TRUE)
print("Urban raster mosaic complete and saved")

}