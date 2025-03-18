#sum_perennial <- function(wood_binary_rast, nfi_lcm_map,tiles){

bng <- 27700

library(raster)
library(sf)
library(tidyverse)

#GB land cover map

lcm <- raster("./data/raw-data/lcm-2023/gblcm2023_25m.tif")

woods_raster_GB <- raster("C:/Users/ik929086/Documents/iDeer/output/GB_datasets_2023/woods_only_raster_GB_2023.tif")

GB_polys_merged <- st_read("C:/Users/ik929086/Documents/iDeer/data/derived-data/LCM2023_GB_WOOD_ExportFeature.shp")
st_crs(GB_polys_merged) <- bng

#Add a patch_ID column to GB_polys_merged
GB_polys_merged$patch_ID <- 1:nrow(GB_polys_merged)

ew10k <- st_read("./data/derived-data/10k_tiles_EW.shp")

source("./code/functions/extract_raster_pixels_to_wood_polys_func_10km_chunks_mean_poly_values.R")

#create circular buffers around every pixel of 400 metres
circle.buff = raster::focalWeight (woods_raster_GB, d=400, type="circle",fillNA=T )#Create buffer
circle.buff[circle.buff > 0] <- 1   # replacing weights by 1
Focal400_WOODS= raster::focal(woods_raster_GB, w=circle.buff, fun=sum, na.rm=T,pad=TRUE, padValue=NA)

plot(Focal400_WOODS)
crs(Focal400_WOODS) <- bng

writeRaster(Focal400_WOODS, "C:/Users/ik929086/Documents/iDeer/output/GB_datasets_2023/small_deer/sum_woodland_400m_small_deer_GB_2023.tif",overwrite=TRUE)

#Divide number of pixels by the total number of 25m2 pixels in a 400m buffer*100 = percentage woodland
Focal400_WOODS_PERC <- (Focal400_WOODS/804)*100

writeRaster(Focal400_WOODS_PERC, "C:/Users/ik929086/Documents/iDeer/output/GB_datasets_2023/small_deer/percentage_woodland_area_400m_small_deer_GB_2023.tif",overwrite=TRUE)

#Extract to polygons
#wood_map_poly_vals <- extract_raster_to_wood_polygons(risk_map = Focal400_WOODS_AREA,
                                                      #tiles = ew10k,
                                                      #wood_polys = GB_polys_merged)
#wood_map_poly_vals_df <- wood_map_poly_vals[[1]]

Focal400_WOODS_PERC <- raster("C:/Users/ik929086/Documents/iDeer/output/GB_datasets_2023/small_deer/percentage_woodland_area_400m_small_deer_GB_2023.tif")

#Get average woodland cover within 400m for each merged woodland polygon
#Accounts for patch size, but also woods around each patch.
mean_wood_map_poly_vals  <- raster::extract(Focal400_WOODS_PERC, GB_polys_merged, fun=mean, na.rm=TRUE, df=TRUE)
mean_wood_map_poly_vals <- mean_wood_map_poly_vals %>% rename(patch_ID = ID)

saveRDS(mean_wood_map_poly_vals, "C:/Users/ik929086/Documents/iDeer/output/GB_datasets_2023/small_deer/mean_focal_area_woods_extracted_to_polys_400m.RDS")

#left_join GB_merged_polys
mean_wood_perc_polys <- left_join(GB_polys_merged, mean_wood_map_poly_vals, by = c("patch_ID"))
mean_wood_perc_polys <- st_as_sf(mean_wood_perc_polys)

woods_area_400m_tif <- fasterize::fasterize(sf = mean_wood_perc_polys, 
                                             raster = lcm,
                                             field = "percentage_woodland_area_400m_small_deer_GB_2023",
                                             fun = "sum",
                                             background = NA)
crs(woods_area_400m_tif) <- bng
plot(woods_area_400m_tif)

writeRaster(woods_area_400m_tif, "C:/Users/ik929086/Documents/iDeer/output/GB_datasets_2023/small_deer/mean_percentage_focal_area_woods_extracted_to_polys_400m.tif",
            overwrite=TRUE)

#}

