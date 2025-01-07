#Extract deer damage risk scores to woodland polygons #####----------------------

#DON'T NEED TO DO THIS FOR WHOLE MAP - DO THIS WITHIN TOOL FOR SELECTED AREA ####

#read
r_rast <- terra::rast("C:/Users/ik929086/Documents/iDeer/output/small_deer_risk_raster.tif")

#replace 0 with NA
#values(r_rast)[values(r_rast) <= 0] = NA

# Convert the stars object to a RasterLayer
raster_layer <- as(r_rast, "Raster")
crs(raster_layer) <- bng

plot(raster_layer)

#Round all values to an integer
#rounded_raster <- round(raster_layer)
#rounded_raster <- raster::as.factor(rounded_raster)
#plot(rounded_raster)

#Make zero in rounded_raster NAs
#rounded_raster_no_zeroes <- reclassify(rounded_raster, cbind(0, NA))
#plot(rounded_raster_no_zeroes)
#raster_layer_no_zeroes <- reclassify(raster_layer, cbind(0,NA))

#Extract raster values to woodland polygons
#Use custom function to loop over 10k tiles - takes too long unchunked
small_deer_risk_poly_vals <- extract_raster_to_wood_polygons(risk_map = raster_layer,
                                                             wood_polys = wood_polys_EW,
                                                             tiles = ew10k)

# Get polygon IDs
poly_ids <- unique(small_deer_risk_poly_vals$patch_ID)

# # Group by patch_ID and calculate the counts for each raster_layer category
deer_risk_landcover <- small_deer_risk_poly_vals %>%
  filter(!is.na(raster_layer)) %>% # Remove NA values
  group_by(patch_ID) %>%
  summarise(
    risk_count_1 = sum(raster_layer == 1, na.rm = TRUE),
    risk_count_2 = sum(raster_layer == 2, na.rm = TRUE),
    risk_count_3 = sum(raster_layer == 3, na.rm = TRUE),
    risk_count_4 = sum(raster_layer == 4, na.rm = TRUE),
    risk_count_5 = sum(raster_layer == 5, na.rm = TRUE)
  )

# # Combine results into a matrix, where each column corresponds to a polygon
deer_risk_landcover_matrix <- do.call(rbind, deer_risk_landcover)
colnames(deer_risk_landcover_matrix) <- deer_risk_landcover_matrix[1,]

#Add polygon IDs to the matrix
deer_risk_landcover_df <- data.frame(
  poly_ID = poly_ids,
  deer_risk_landcover_matrix
)

# Convert to a matrix and assign column names based on polygon IDs
deer_risk_landcover <- matrix(deer_risk_landcover, nrow = 5, byrow = TRUE)
colnames(deer_risk_landcover) <- poly_ids

#Remove the first row (patch_ID) from the matrix
deer_risk_landcover_matrix_no_patch <- deer_risk_landcover_matrix[-1, ]

#Convert the matrix to a long format
deer_risk_landcover_long <- reshape2::melt(
  deer_risk_landcover_matrix_no_patch,
  varnames = c("risk_category", "patch_ID"),  # Specify the correct column names
  value.name = "value"  # Name for the values column
)

mean_risk <- deer_risk_landcover_long %>%
  mutate(
    risk_category = as.numeric(gsub("[^0-9]", "", risk_category))  # Remove non-numeric characters and convert to numeric
  ) %>%
  group_by(patch_ID) %>%
  filter(value != 0) %>%  # Exclude rows with zero values
  summarise(
    Weighted_Mean = sum(risk_category * value) / sum(value),
    #Weighted SD calculation
    sd = sqrt(sum(value * (risk_category - Weighted_Mean)^2) / sum(value)),
    # Weighted SE calculation
    se = sd / sqrt(sum(value)),
    .groups = 'drop'
  )


#Append the mean risk categories to the polygon dataset
polygon_with_mean_risk <- wood_polys_EW %>%
  left_join(mean_risk, by = "patch_ID")

#Map the mean woodland polygon risk scores ####

#Convert to leaflet projection

polygon_with_mean_risk <- st_transform(polygon_with_mean_risk, crs = '+proj=longlat +datum=WGS84')
polygon_with_mean_risk <- st_as_sf(polygon_with_mean_risk)

#Convert to leaflet projection

polygon_with_mean_risk <- st_transform(polygon_with_mean_risk, crs = '+proj=longlat +datum=WGS84')
polygon_with_mean_risk <- st_as_sf(polygon_with_mean_risk)

# Convert polygons to Web Mercator (EPSG:3857) for compatibility with OSM tiles
crs_mercator <- "EPSG:3857"

# Fetch OpenStreetMap tiles for the bounding box region
map_tiles <- maptiles::get_tiles(sitebuf_merc, provider = "OpenStreetMap", zoom = 15)

# Plotting the polygons with OpenStreetMap tiles as base layer

updated_small_deer_risk_map<- ggplot() +
  tidyterra::geom_spatraster_rgb(data = map_tiles) +  # OSM tiles as base
  geom_sf(data = polygon_with_mean_risk,
          aes(fill = Weighted_Mean),
          color = "black") +
  #geom_sf(data = polygon_with_mean_risk %>% filter(patch_ID %in% new_wood_IDs),
  #fill = NA,                   # No fill for the outlined polygons
  #size = 1.5) +               # Adjust size as needed
  scale_fill_viridis_c(name = "Small deer impact risk",option = "viridis", direction = -1,
                       limits = c(1, 5)) +
  coord_sf(crs = st_crs(polygon_with_mean_risk)) +
  labs(title = "Updated small deer impact risk map (Mean impact risk per parcel)",
       x = "Longitude",
       y = "Latitude") +
  theme_minimal() +
  theme(legend.position = "right")

#Plot the standard deviation

updated_small_deer_risk_map_sd <- ggplot() +
  tidyterra::geom_spatraster_rgb(data = map_tiles) +  # OSM tiles as base
  geom_sf(data = polygon_with_mean_risk,
          aes(fill = sd),
          color = "black") +
  #geom_sf(data = polygon_with_mean_risk %>% filter(patch_ID %in% new_wood_IDs),
  #fill = NA,                   # No fill for the outlined polygons
  #size = 1.5) +               # Adjust size as needed
  scale_fill_viridis_c(name = "Small deer impact risk",option = "viridis", direction = -1) +
  coord_sf(crs = st_crs(polygon_with_mean_risk)) +
  labs(title = "Updated small deer impact risk map (Standard deviation)",
       x = "Longitude",
       y = "Latitude") +
  theme_minimal() +
  theme(legend.position = "right")
