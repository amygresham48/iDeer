sum_peren <- function(wood_binary_rast, nfi_lcm_map,tiles){

bng <- 27700

#GB land cover map

lcm <- raster("./data/raw-data/lcm-2023/gblcm2023_25m.tif")

crs(lcm) <- bng

#make peren map only

reclass_peren <- data.frame(is = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,
                                    18,19,20,21), becomes = c(0,0,0,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,1,0,0))
peren_raster_GB <- reclassify(lcm, reclass_peren)
crs(peren_raster_GB) <- bng

writeRaster(peren_raster_GB, here("./output/GB_datasets_2023/peren_raster_GB_2023.tif"),overwrite=TRUE)

#-------------------------------------

peren_raster_GB <- raster("./output/GB_datasets_2023/peren_raster_GB_2023.tif")

#focal statistics, moving window
#Sum of peren land up to 1km away

#if na.rm = F, edges become cropped.

#test
#peren_raster_GB_test <- crop(peren_raster_GB, ew10k[1300,])

circle.buff = raster::focalWeight(peren_raster_GB, d=1000, type="circle",fillNA=T )#Create buffer
circle.buff[circle.buff > 0] <- 1   # replacing weights by 1
Focal1000_peren= raster::focal(x=peren_raster_GB, w=circle.buff, fun=sum, na.rm=T, pad=TRUE, padValue=NA)
                       
#Divide number of pixels by the total number of 25m2 pixels in a buffer*100 = percentage peren
Focal1000_peren_PERC <- (Focal1000_peren/5027)*100
#save
writeRaster(Focal1000_peren_PERC, "./output/GB_datasets_2023/large_deer/percentage_cover_perennial_1000m_2023.tif")


}

