sum_arable <- function(wood_binary_rast, nfi_lcm_map,tiles){

bng <- 27700

#make arable map only

reclass_arable <- data.frame(is = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,
                                    18,19,20,21), becomes = c(0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0))

arable_raster_GB <- reclassify(nfi_lcm_map, reclass_arable)
crs(arable_raster_GB) <- bng

writeRaster(arable_raster_GB, here("output/EW_datasets_2022/arable_raster_GB_2022.tif"),overwrite=TRUE)

#-------------------------------------

arable_raster_GB <- raster(here("output/EW_datasets_2022/arable_raster_GB_2022.tif"))

#focal statistics, moving window
#Sum of arable land up to 1km away

#if na.rm = F, edges become cropped.

#test
#arable_raster_GB_test <- crop(arable_raster_GB, uk10k[8586,])

circle.buff = raster::focalWeight(arable_raster_GB, d=1000, type="circle",fillNA=T )#Create buffer
circle.buff[circle.buff > 0] <- 1   # replacing weights by 1
Focal1000= raster::focal(x=arable_raster_GB, w=circle.buff, fun=sum, na.rm=T, pad=TRUE, padValue=NA,
                         filename = here("output/EW_datasets_2022/sum_arable_1000_non_wood_2022.tif"), 
                         overwrite=T)#do focal window analysis with weights matrix

plot(Focal1000)
print("MAX DAMS 200m raster saved")



}

