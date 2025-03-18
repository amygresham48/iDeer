sum_arable <- function(wood_binary_rast, nfi_lcm_map,tiles){

bng <- 27700

# England and Wales 10k squares
ew10k <- sf::st_read("data/derived-data/10k_tiles_EW.shp")

#GB land cover map

lcm <- raster("./data/raw-data/lcm-2023/gblcm2023_25m.tif")

crs(lcm) <- bng

#make arable map only

reclass_arable <- data.frame(is = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,
                                    18,19,20,21), becomes = c(0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0))

arable_raster_GB <- reclassify(lcm, reclass_arable)
crs(arable_raster_GB) <- bng

writeRaster(arable_raster_GB, here("output/GB_datasets_2023/2023/arable_raster_GB_2023.tif"),overwrite=TRUE)

arable_raster_GB <- raster("output/GB_datasets_2023/arable_raster_GB_2023.tif")

#-------------------------------------

#WOODLAND EDGES ####

bng <- 27700

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
writeRaster(boundaries_wood, "./output/GB_datasets_2023/LCMWOOD2023_GB_EDGES.tif")

#Read
boundaries_wood <- raster("./output/GB_datasets_2023/LCMWOOD2023_GB_EDGES.tif")

#Overlay rasters, giving priority to woodland edges
combined_arable_wood_edges <- overlay(boundaries_wood, arable_raster_GB, fun = function(x, y) {
  ifelse(!is.na(x) & x != 0, x, y)  # Prioritize `boundaries_wood` only if non-zero
})


#focal statistics, moving window
#Sum of arable land + woodland edges up to 1km away

#if na.rm = F, edges become cropped.

#test
#combined_arable_wood_edges_test <- crop(combined_arable_wood_edges, ew10k[1300,])

circle.buff = raster::focalWeight(combined_arable_wood_edges, d=1000, type="circle",fillNA=T )#Create buffer
circle.buff[circle.buff > 0] <- 1   # replacing weights by 1
Focal1000= raster::focal(x=combined_arable_wood_edges, w=circle.buff, fun=sum, na.rm=T, pad=TRUE, padValue=NA)
writeRaster(Focal1000, "./output/GB_datasets_2023/large_deer/sum_woodland_edge_arable_1000_2023.tif")                   
Focal1000_WOOD_EDGE_ARABLE <- raster("./output/GB_datasets_2023/large_deer/sum_woodland_edge_arable_1000_2023.tif")

#Divide number of pixels by the total number of 25m2 pixels in a buffer*100 = percentage arable
Focal1000_WOOD_EDGE_ARABLE_PERC <- (Focal1000_WOOD_EDGE_ARABLE/5027)*100
#save
writeRaster(Focal1000_WOOD_EDGE_ARABLE_PERC, "./output/GB_datasets_2023/large_deer/percentage_cover_woodland_edge_arable_1000_2023.tif")


}

