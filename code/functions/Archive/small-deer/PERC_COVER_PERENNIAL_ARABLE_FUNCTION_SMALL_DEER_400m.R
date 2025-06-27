#sum_perennial <- function(wood_binary_rast, nfi_lcm_map,tiles){

bng <- 27700

library(raster)
library(sf)
library(tidyverse)

#GB land cover map

lcm <- raster("./data/raw-data/lcm-2023/gblcm2023_25m.tif")

crs(lcm) <- bng

#land cover map
#This layer does NOT include woodland edge type
#nfi_lcm_map <- raster(here("output/nfi_lcm_2022_overlaid.tif"))

#perennial vegetation AND arable AND woodland

#This will be grasslands, heathland, fenland, saltmarsh
#arable land included - small deer forager over a smaller area
#therefore the effect of attracting high densities to the area will not be very influential
#compared to the effect on far-ranging large species
#suburban and urban not included

reclass_peren_arable <- data.frame(is = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,
                                    18,19,20,21), becomes = c(0,0,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,1,0,0))

peren_arable_raster_GB <- reclassify(lcm, reclass_peren_arable)
crs(peren_arable_raster_GB) <- bng

writeRaster(peren_arable_raster_GB, "C:/Users/ik929086/Documents/iDeer/output/GB_datasets_2023/perennial_arable_raster_binary_GB_2023.tif",overwrite=TRUE)

#-------------------------------------

peren_arable_raster_GB <- raster("C:/Users/ik929086/Documents/iDeer/output/GB_datasets_2023/perennial_arable_raster_binary_GB_2023.tif")

#create circular buffers around every pixel of 400 metres
circle.buff = raster::focalWeight (peren_arable_raster_GB, d=400, type="circle",fillNA=T)#Create buffer
circle.buff[circle.buff > 0] <- 1   # replacing weights by 1
Focal400_PEREN_ARABLE= raster::focal(peren_arable_raster_GB, w=circle.buff, fun=sum, na.rm=T,pad=TRUE, padValue=NA,)

#Divide number of pixels by the total number of 25m2 pixels in a 400m buffer*100 = percentage perennial or arable
Focal400_PERC_PEREN_ARABLE <- (Focal400_PEREN_ARABLE/804)*100

writeRaster(Focal400_PERC_PEREN_ARABLE, "C:/Users/ik929086/Documents/iDeer/output/GB_datasets_2023/small_deer/percentage_cover_perennial_arable_400m_GB_2023.tif")


#}

