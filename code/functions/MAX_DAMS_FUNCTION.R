max_dams <- function(wood_binary_rast, dams, nfi_lcm_map,tiles){

bng <- 27700

nfi_lcm_map <- crop(nfi_lcm_map,tiles[1:2,])
  
# Crop dams to match the extent of nfi_lcm_map
dams_crop <- crop(dams, extent(nfi_lcm_map))

#Reclass nfi/lcm land cover map so all woodland habitat set to 0
#All non-woodland habitat set to 1

reclass <- data.frame(is = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,
                             18,19,20,21,22,23), becomes = c(0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1))

lcm_mask <- reclassify(nfi_lcm_map, reclass)

#Multiply DAMs and binary LCM together - woodlands will be set to 0 DAMS

DAMS_open_raster <- dams_crop*lcm_mask

plot(DAMS_open_raster)

#Crop map to England and Wales

DAMS_open_raster_EW <- crop(DAMS_open_raster,tiles[1:2,])
DAMS_open_raster_EW <- mask(DAMS_open_raster_EW,tiles[1:2,])
crs(DAMS_open_raster_EW) <- bng

#writeRaster(DAMS_open_raster_EW, here("output/DAMS_open_raster_EW_2022.tif"),overwrite=TRUE)

DAMS_open_raster_EW <- raster(here("output/DAMS_open_raster_EW_2022.tif"))

#-------------------------------------

#focal statistics, moving window
#Calculate maximum DAMS up to 1km away from each woodland pixel

#if na.rm = F, edges become cropped.

circle.buff = raster::focalWeight (DAMS_open_raster_EW, d=1000, type="circle",fillNA=T )#Create buffer
circle.buff[circle.buff > 0] <- 1   # replacing weights by 1
Focal1000= raster::focal(x=DAMS_open_raster_EW, w=circle.buff, fun=max, na.rm=T, pad=TRUE, padValue=NA,filename = here("output/max_dams_1km_non_wood_2022.tif"), overwrite=T)#do focal window analysis with weights matrix

plot(Focal1000)
print("MAX DAMS 1km raster saved")


}

