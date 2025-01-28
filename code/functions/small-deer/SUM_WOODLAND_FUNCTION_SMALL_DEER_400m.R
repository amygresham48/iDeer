#sum_perennial <- function(wood_binary_rast, nfi_lcm_map,tiles){

bng <- 27700

library(raster)
library(sf)
library(tidyverse)

#GB land cover map

lcm <- raster("./data/raw-data/lcm-2023/gblcm2023_25m.tif")

woods_raster_GB <- raster("C:/Users/ik929086/Documents/iDeer/output/GB_datasets_2023/woods_only_raster_GB_2023.tif")

#st_crs(GB_polys_unmerged) <- bng
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

Focal400_WOODS_AREA <- Focal400_WOODS*25

plot(Focal400_WOODS_AREA)
crs(Focal400_WOODS_AREA) <- bng

writeRaster(Focal400_WOODS_AREA, "C:/Users/ik929086/Documents/iDeer/output/GB_datasets_2023/small_deer/sum_woodland_area_400m_small_deer_GB_2023.tif",overwrite=TRUE)

Focal400_WOODS_AREA <- raster("C:/Users/ik929086/Documents/iDeer/output/GB_datasets_2023/small_deer/sum_woodland_area_400m_small_deer_GB_2023.tif")

#Extract to polygons
wood_map_poly_vals <- extract_raster_to_wood_polygons(risk_map = Focal400_WOODS_AREA,
                                                      tiles = ew10k,
                                                      wood_polys = GB_polys_merged)


wood_map_poly_vals_df <- wood_map_poly_vals[[1]]

saveRDS(wood_map_poly_vals_df, "C:/Users/ik929086/Documents/iDeer/output/GB_datasets_2023/small_deer/focal_area_woods_extracted_to_polys_400m.RDS")

wood_map_poly_vals_df <- st_as_sf(wood_map_poly_vals_df)
woods_area_400m_tif <- fasterize::fasterize(sf = wood_map_poly_vals_df, 
                                             raster = Focal400_WOOD_AREA,
                                             field = "mean_Focal400_WOOD_AREA",
                                             fun = "sum",
                                             background = NA)
crs(woods_area_1000m_tif) <- bng
writeRaster(woods_area_400m_tif, "C:/Users/ik929086/Documents/iDeer/output/GB_datasets_2023/small_deer/focal_area_woods_extracted_to_polys_400m.tif",
            overwrite=TRUE)

#}

