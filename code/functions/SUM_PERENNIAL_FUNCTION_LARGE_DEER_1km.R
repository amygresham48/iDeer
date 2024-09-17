sum_perennial <- function(wood_binary_rast, nfi_lcm_map,tiles){

bng <- 27700

#make perennial map only
#semi-natural vegetation

#This will be grasslands, heathland, fenland, saltmarsh
#suburban and urban not included

reclass_peren <- data.frame(is = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,
                                    18,19,20,21), becomes = c(0,0,0,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,1,0,0))

peren_raster_GB <- reclassify(nfi_lcm_map, reclass_peren)
crs(peren_raster_GB) <- bng

writeRaster(peren_raster_GB, here("output/EW_datasets_2022/perennial_raster_GB_2022.tif"),overwrite=TRUE)

#-------------------------------------

#focal statistics, moving window
#Sum of perennial land up to 1km away

#if na.rm = F, edges become cropped.

#test
#peren_raster_GB_test <- crop(peren_raster_GB, uk10k[8586,])

circle.buff = raster::focalWeight(peren_raster_GB, d=1000, type="circle",fillNA=T)#Create buffer
circle.buff[circle.buff > 0] <- 1   # replacing weights by 1
Focal1000= raster::focal(x=peren_raster_GB, w=circle.buff, fun=sum, na.rm=T, pad=TRUE, padValue=NA,
                         filename = here("output/sum_perennial_1000_2022.tif"), 
                         overwrite=T)#do focal window analysis with weights matrix

plot(Focal1000)
print("MAX DAMS 200m raster saved")



}

