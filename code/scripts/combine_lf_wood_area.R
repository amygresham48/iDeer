#Combine wood area within 200m and lf length within 200m for small deer model ####

#woodland area within 200m

Focal200_WOOD <- raster(here("output/EW_datasets_2022/small_deer/sum_woodland_area_200m_small_deer_EW.tif"))

#linear feature density within 200m
Focal200_LF <- raster(here("output/EW_datasets_2022/small_deer/lf_density_200m_2022.tif"))

#Read in England-Wales 10k tiles

ew10k <- st_read(here("data/derived-data/10k_tiles_EW.shp"))

extent(Focal200_WOOD)
extent(Focal200_LF)
extent(ew10k)

#crop and mask lf to ew10k ####
Focal200_LF_cropped <- crop(Focal200_LF, ew10k)
#mask
Focal200_LF_mask <- mask(Focal200_LF_cropped, ew10k)
#project
Focal200_LF_mask_project <- projectRaster(Focal200_LF_mask, Focal200_WOOD)

#Add the rasters together as a brick ####
Focal_200_WOOD_LF_AREA_BRICK <- brick(Focal200_WOOD, Focal200_LF_mask_project)
plot(Focal_200_WOOD_LF_AREA_BRICK)

Focal_200_WOOD_LF_AREA_SUM <- calc(Focal_200_WOOD_LF_AREA_BRICK, sum, na.rm = TRUE)
plot(Focal_200_WOOD_LF_AREA_SUM)

#save
writeRaster(Focal_200_WOOD_LF_AREA_SUM, here("output/EW_datasets_2022/small_deer/sum_woodland_and_LF_area_200m_small_deer_EW.tif"))
