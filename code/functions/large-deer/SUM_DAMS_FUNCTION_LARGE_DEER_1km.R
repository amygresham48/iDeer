max_dams <- function(wood_binary_rast, dams, nfi_lcm_map,tiles){

bng <- 27700

#DAMS
#Interpolated from 50m resolution to 25m resolution
#To match the resolution of other raster layers
#Interpolation method = bilinear
dams <- raster(here("data/raw-data/DAMS/dams_25m_bng.tif"))
crs(dams) <- bng

#GB land cover map
lcm <- raster("./data/raw-data/lcm-2023/gblcm2023_25m.tif")

# Crop dams to match the extent of lcm
dams_crop <- crop(dams, extent(lcm))

#Reclass nfi/lcm land cover map so all woodland habitat set to 0
#All non-woodland habitat set to 1

reclass <- data.frame(is = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,
                             18,19,20,21), becomes = c(0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1))

open_raster_GB <- reclassify(lcm, reclass)

#Multiply DAMs and binary LCM together - woodlands will be set to 0 DAMS

DAMS_open_raster <- dams*open_raster_GB

plot(DAMS_open_raster)

#Crop map to England and Wales

#DAMS_open_raster_EW <- crop(DAMS_open_raster,uk10k_EW)
#DAMS_open_raster_EW <- mask(DAMS_open_raster_EW,uk10k_EW)
#crs(DAMS_open_raster_EW) <- bng

writeRaster(DAMS_open_raster, here("output/GB_datasets_2023/DAMS_open_raster_GB_2023.tif"),overwrite=TRUE)

#-------------------------------------

#focal statistics, moving window
#Calculate maximum DAMS up to 200m away from each woodland pixel

#if na.rm = F, edges become cropped.

#test

circle.buff = raster::focalWeight(DAMS_open_raster, d=1000, type="circle",fillNA=T )#Create buffer
circle.buff[circle.buff > 0] <- 1   # replacing weights by 1
Focal1000= raster::focal(x=DAMS_open_raster, w=circle.buff, fun=sum, na.rm=T, pad=TRUE, padValue=NA,
                         filename = here("output/GB_datasets_2023/large-deer/sum_dams_1000_non_wood_2023.tif"), 
                         overwrite=T)#do focal window analysis with weights matrix

plot(Focal1000)
print("MAX DAMS 1km raster saved")



}

