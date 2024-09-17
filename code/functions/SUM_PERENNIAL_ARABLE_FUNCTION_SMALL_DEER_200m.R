sum_perennial <- function(wood_binary_rast, nfi_lcm_map,tiles){

bng <- 27700

#perennial vegetation AND arable

#This will be grasslands, heathland, fenland, saltmarsh
#arable land included - small deer forager over a smaller area
#therefore the effect of attracting high densities to the area will not be very influential
#compared to the effect on far-ranging large species
#suburban and urban not included

reclass_peren_arable <- data.frame(is = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,
                                    18,19,20,21), becomes = c(0,0,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,1,0,0))

peren_arable_raster_GB <- reclassify(nfi_lcm_map, reclass_peren_arable)
crs(peren_raster_GB) <- bng

writeRaster(peren_arable_raster_GB, here("output/EW_datasets_2022/perennial_arable_raster_GB_2022.tif"),overwrite=TRUE)

#-------------------------------------

peren_arable_raster_GB <- raster(here("output/EW_datasets_2022/perennial_arable_raster_GB_2022.tif"))

#focal statistics, moving window
#Sum of perennial land up to 1km away

#if na.rm = F, edges become cropped.

#test
#peren_raster_GB_test <- crop(peren_raster_GB, uk10k[8586,])

circle.buff = raster::focalWeight(peren_arable_raster_GB, d=200, type="circle",fillNA=T)#Create buffer
circle.buff[circle.buff > 0] <- 1   # replacing weights by 1
Focal200= raster::focal(x=peren_arable_raster_GB, w=circle.buff, fun=sum, na.rm=T, pad=TRUE, padValue=NA,
                         filename = here("output/sum_perennial_arable_200m_2022.tif"), 
                         overwrite=T)#do focal window analysis with weights matrix

plot(Focal200)
print("MAX DAMS 200m raster saved")



}

