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

writeRaster(Focal1000_WOOD, "C:/Users/ik929086/Documents/iDeer/output/GB_datasets_2023/large_deer/sum_woodland_1000m_large_deer_GB_2023.tif",overwrite=TRUE)

#Divide number of pixels by the total number of 25m2 pixels in a buffer*100 = percentage woodland
Focal1000_WOOD_PERC <- (Focal1000_WOOD/5027)*100

writeRaster(Focal1000_WOOD_PERC, "C:/Users/ik929086/Documents/iDeer/output/GB_datasets_2023/large_deer/percentage_woodland_area_1000m_large_deer_GB_2023.tif",overwrite=TRUE)

Focal1000_WOOD_PERC <- raster("C:/Users/ik929086/Documents/iDeer/output/GB_datasets_2023/large_deer/percentage_woodland_area_1000m_large_deer_GB_2023.tif")

# Extract values from the cropped raster for the woodland polygons #--------------

#Focal_1000_WOOD_AREA_TEST <- crop(Focal1000_WOOD_AREA, extent(df_sf))
#crs(Focal_1000_WOOD_AREA_TEST) <- bng
#GB_polys_TEST <- st_filter(GB_polys_merged, df_sf)

# wood_map_poly_vals <- extract_raster_to_wood_polygons(risk_map = Focal1000_WOOD_PERC,
#                                                       tiles = ew10k,
#                                                       wood_polys = GB_polys_merged)

#Get average woodland cover within 1000m for each merged woodland polygon
#Accounts for patch size, but also woods around each patch.
mean_wood_map_poly_vals  <- raster::extract(Focal1000_WOOD_PERC, GB_polys_merged, fun=mean, na.rm=TRUE, df=TRUE)

saveRDS(mean_wood_map_poly_vals, "C:/Users/ik929086/Documents/iDeer/output/GB_datasets_2023/large_deer/mean_focal_area_woods_extracted_to_polys_1000m.RDS")

#left_join GB_merged_polys
mean_wood_map_poly_vals <- mean_wood_map_poly_vals %>% rename(patch_ID = ID)

mean_wood_perc_polys <- left_join(GB_polys_merged, mean_wood_map_poly_vals, by = c("patch_ID"))
mean_wood_perc_polys <- st_as_sf(mean_wood_perc_polys)

woods_area_1000m_tif <- fasterize::fasterize(sf = mean_wood_perc_polys, 
                                            raster = lcm,
                                            field = "percentage_woodland_area_1000m_large_deer_GB_2023",
                                            fun = "sum",
                                            background = NA)
crs(woods_area_1000m_tif) <- bng
plot(woods_area_1000m_tif)

writeRaster(woods_area_1000m_tif, "C:/Users/ik929086/Documents/iDeer/output/GB_datasets_2023/large_deer/mean_percentage_focal_area_woods_extracted_to_polys_1000m.tif",
            overwrite=TRUE)



# wood_map_poly_vals_df <- wood_map_poly_vals[[1]]
# 
# saveRDS(wood_map_poly_vals_df, "C:/Users/ik929086/Documents/iDeer/output/GB_datasets_2023/large_deer/focal_area_woods_extracted_to_polys_1000m.RDS")
# 
# wood_map_poly_vals_df <- st_as_sf(wood_map_poly_vals_df)
# woods_area_1000m_tif <- fasterize::fasterize(sf = wood_map_poly_vals_df, 
#                                              raster = Focal1000_WOOD_PERC,
#                                              field = "mean_Focal1000_WOOD_AREA",
#                                              fun = "sum",
#                                              background = NA)
# crs(woods_area_1000m_tif) <- bng
# writeRaster(woods_area_1000m_tif, "C:/Users/ik929086/Documents/iDeer/output/GB_datasets_2023/large_deer/percentage_focal_area_woods_extracted_to_polys_1000m.tif",
#             overwrite=TRUE)

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

