#sum_perennial <- function(wood_binary_rast, nfi_lcm_map,tiles, GB_woods_merged){

bng <- 27700

#GB land cover map

lcm <- raster("./data/raw-data/lcm-2023/gblcm2023_25m.tif")
#GB_polys_unmerged <- st_read("C:/Users/ik929086/Documents/iDeer/data/derived-data/LCM2023_GB_WOODS_polygons.shp")
crs(lcm) <- bng
#st_crs(GB_polys_unmerged) <- bng
GB_polys_merged <- st_read("C:/Users/ik929086/Documents/iDeer/data/derived-data/LCM2023_GB_WOOD_ExportFeature.shp")
st_crs(GB_polys_merged) <- bng

#Add a patch_ID column to GB_polys_merged
GB_polys_merged$patch_ID <- 1:nrow(GB_polys_merged)

ew10k <- st_read("./data/derived-data/10k_tiles_EW.shp")

source("./code/functions/extract_raster_pixels_to_wood_polys_func_10km_chunks_mean_poly_values.R")

#land cover map
#This layer does NOT include woodland edge type
#nfi_lcm_map <- raster(here("output/nfi_lcm_2022_overlaid.tif"))

# reclass woodland

reclass_woods <- data.frame(is = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,
                                    18,19,20,21), becomes = c(1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0))

woods_raster_GB <- reclassify(lcm, reclass_woods)
crs(woods_raster_GB) <- bng

writeRaster(woods_raster_GB, "C:/Users/ik929086/Documents/iDeer/output/GB_datasets_2023/woods_only_raster_GB_2023.tif",overwrite=TRUE)

#focal statistics, moving window
#Sum of woodland up to 1km away

#if na.rm = F, edges become cropped.

circle.buff = raster::focalWeight(woods_raster_GB, d=1000, type="circle",fillNA=T)#Create buffer
circle.buff[circle.buff > 0] <- 1   # replacing weights by 1
Focal1000_WOOD= raster::focal(x=woods_raster_GB, w=circle.buff, fun=sum, na.rm=T, pad=TRUE, padValue=NA)

plot(Focal1000_WOOD)
crs(Focal1000_WOOD) <- bng

#Get woodland area
Focal1000_WOOD_AREA <- Focal1000_WOOD*25

writeRaster(Focal1000_WOOD_AREA, "C:/Users/ik929086/Documents/iDeer/output/GB_datasets_2023/large_deer/sum_woodland_area_1000m_large_deer_GB_2023.tif",overwrite=TRUE)

# Extract values from the cropped raster for the woodland polygons #--------------
Focal1000_WOOD_AREA <- raster("C:/Users/ik929086/Documents/iDeer/output/GB_datasets_2023/large_deer/sum_woodland_area_1000m_large_deer_GB_2023.tif")

#Divide number of pixels by the total number of 25m2 pixels in a buffer*100 = percentage woodland
Focal1000_WOOD_PERC <- (Focal1000_WOOD/5027)*100

writeRaster(Focal1000_WOOD_PERC, "C:/Users/ik929086/Documents/iDeer/output/GB_datasets_2023/large_deer/percentage_woodland_area_1000m_large_deer_GB_2023.tif",overwrite=TRUE)


#Focal_1000_WOOD_AREA_TEST <- crop(Focal1000_WOOD_AREA, extent(df_sf))
#crs(Focal_1000_WOOD_AREA_TEST) <- bng
#GB_polys_TEST <- st_filter(GB_polys_merged, df_sf)

wood_map_poly_vals <- extract_raster_to_wood_polygons(risk_map = Focal1000_WOOD_PERC,
                                                      tiles = ew10k,
                                                      wood_polys = GB_polys_merged)

# wood_map_poly_vals <- terra::extract(Focal_1000_WOOD_AREA_TEST, GB_polys_TEST, df = TRUE, fun = modal, na.rm=TRUE)
# 
# # Get row numbers and patch IDs from the wood_polys_site
# wood_site_IDs <- data.frame(row_ID = seq_len(nrow(GB_polys_TEST)))#,
#                             #patch_ID = GB_polys_TEST$patch_ID)
# 
# # Create a data frame with the extracted values and patch IDs
# vals <- data.frame(
#   row_ID = wood_site_IDs$row_ID,
#   wood_area_1000m = wood_map_poly_vals$sum_woodland_area_1000m_large_deer_GB_2023 # Extracted raster values
# )
# 
# # Join the patch IDs to the extracted values
# vals <- left_join(vals, wood_site_IDs)
# vals <- vals %>% dplyr::select(-c("row_ID"))
# 
# # Filter out NA values
# #vals <- vals[!is.na(vals$raster_values), ]
# 
# #left_join geometry to vals
# #merged_df <- left_join(vals, GB_polys_TEST)
#merged_df <- cbind(vals, GB_polys_TEST)
# merged_df <- st_as_sf(merged_df)
# st_crs(merged_df) <- 27700

# #Summarise dataframe: get mean Focal1000_wood_area for each polygon
# 
# summary_df <- wood_map_poly_vals %>%
#   group_by(patch_ID) %>%
#   summarise(mean_Focal1000_WOOD_AREA = mean(Focal1000_WOOD_AREA, na.rm = TRUE)) %>%
# 
# #Join geometry to extracted values
# geometry_tiles <- st_filter(GB_polys_merged, ew10k[1:10,])
# merged_df <- left_join(summary_df, geometry_tiles, by = c("patch_ID"))
# merged_df <- merged_df %>% filter(!st_is_empty(geometry))
# merged_df <- st_as_sf(merged_df)

wood_map_poly_vals_df <- wood_map_poly_vals[[1]]

saveRDS(wood_map_poly_vals_df, "C:/Users/ik929086/Documents/iDeer/output/GB_datasets_2023/large_deer/focal_area_woods_extracted_to_polys_1000m.RDS")

wood_map_poly_vals_df <- st_as_sf(wood_map_poly_vals_df)
woods_area_1000m_tif <- fasterize::fasterize(sf = wood_map_poly_vals_df, 
                                             raster = Focal1000_WOOD_PERC,
                                             field = "mean_Focal1000_WOOD_AREA",
                                             fun = "sum",
                                             background = NA)
crs(woods_area_1000m_tif) <- bng
writeRaster(woods_area_1000m_tif, "C:/Users/ik929086/Documents/iDeer/output/GB_datasets_2023/large_deer/focal_area_woods_extracted_to_polys_1000m.tif",
            overwrite=TRUE)

# # Plot the geometry colored by raster_values
# ggplot(data = result_df) +
#   geom_sf(aes(fill = mean_value), color = NA) + # Use fill for raster_values and no border
#   scale_fill_viridis_c(option = "viridis", direction = -1, name = "Raster Values") +
#   theme_minimal() +
#   labs(
#     title = "Geometry Plot Colored by Raster Values",
#     x = "Easting",
#     y = "Northing"
#   )


#}

