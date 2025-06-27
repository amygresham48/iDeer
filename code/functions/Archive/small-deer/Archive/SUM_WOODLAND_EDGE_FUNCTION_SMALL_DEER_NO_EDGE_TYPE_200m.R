sum_edge <- function(wood_binary_rast, nfi_lcm_map,tiles){

bng <- 27700

#land cover map cropped to England and Wales
#lcm_EW <- raster("output/EW_datasets_2023/LCM2023_EW_masked.tif")

#GB land cover map

lcm <- raster("./data/raw-data/lcm-2023/gblcm2023_25m.tif")

crs(lcm) <- bng

#Get wood boundaries in tile buffer
# make binary edge raster. 1 if edge, 0 if not ####
#Filter for woodland only, make everything else NA
wood <- lcm
wood[wood[] >= 3] = NA
boundaries_wood = boundaries(wood, type='inner') # edge raster
#Make woodland boundaries = 1000
#boundaries_wood <- boundaries*1000
# need to make NAs 0
boundaries_wood[is.na(boundaries_wood[])] <- 0 
plot(boundaries_wood)
#save boundaries raster
writeRaster(boundaries_wood, "./output/GB_datasets_2023/LCMWOOD2023_GB_EDGES.tif",overwrite=TRUE)

#create circular buffers around every pixel of 200 metres
circle.buff = raster::focalWeight (boundaries_wood, d=200, type="circle",fillNA=T )#Create buffer
circle.buff[circle.buff > 0] <- 1   # replacing weights by 1
Focal200_EDGES= raster::focal(boundaries_wood, w=circle.buff, fun=sum, na.rm=T,pad=TRUE, padValue=NA)

#plot(Focal200_EDGES)
crs(Focal200_EDGES) <- bng

#201 25x25m pixels can fit in a 200m radius buffer
#Therefore get n pixels containing woodland edges as a % of 201
Focal200_EDGES_PERC <- (Focal200_EDGES/201)*100

#writeRaster(Focal200_EDGES, "./output/GB_datasets_2023/small_deer/sum_woodland_edges_200m_2023_small_deer_GB.tif")
writeRaster(Focal200_EDGES_PERC, "./output/GB_datasets_2023/small_deer/percentage_cover_woodland_edges_200m_2023_small_deer_GB.tif")



}
