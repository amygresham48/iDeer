sum_perennial <- function(wood_binary_rast, nfi_lcm_map,tiles){

bng <- 27700

#GB binary woodland raster
#Made in ArcGIS from CEH LCM 2023

wood_binary <- raster("./data/derived-data/LCM2023WOODSGB.tif")

#create circular buffers around every pixel of 200 metres
circle.buff = raster::focalWeight (wood_binary, d=200, type="circle",fillNA=T )#Create buffer
circle.buff[circle.buff > 0] <- 1   # replacing weights by 1
Focal200_WOOD_AREA= raster::focal(wood_binary, w=circle.buff, fun=sum, na.rm=T,pad=TRUE, padValue=NA)

#plot(Focal200_WOOD_AREA)
crs(Focal200_WOOD_AREA) <- bng

#Assign all pixels with woodland a forage score of 10
Focal200_WOOD_QUALITY <- Focal200_WOOD_AREA*10
#plot(Focal200_WOOD_AREA)

writeRaster(Focal200_WOOD_QUALITY, "C:/Users/ik929086/Documents/iDeer/output/GB_datasets_2023/small_deer/sum_woodland_quality_200m_small_deer_GB_2023.tif",overwrite=TRUE)

}

Focal200_WOOD_AREA <- raster("C:/Users/ik929086/Documents/iDeer/output/GB_datasets_2023/small_deer/sum_woodland_area_200m_small_deer_GB_2023.tif")
