#sum_edge <- function(wood_binary_rast, nfi_lcm_map,tiles){

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

#edges_test <- crop(boundaries_wood, ew10k[1300,])

#create circular buffers around every pixel of 1000 metres
circle.buff = raster::focalWeight (boundaries_wood, d=1000, type="circle",fillNA=T )#Create buffer
circle.buff[circle.buff > 0] <- 1   # replacing weights by 1
Focal1000_EDGES= raster::focal(boundaries_wood, w=circle.buff, fun=sum, na.rm=F,pad=TRUE, padValue=NA)

#plot(Focal1000_EDGES)
crs(Focal1000_EDGES) <- bng

writeRaster(Focal1000_EDGES, here("output/GB_datasets_2023/large_deer/sum_woodland_edge_1000m_2023_large_deer_GB.tif"))

Focal1000_EDGES <- raster("./output/GB_datasets_2023/large_deer/sum_woodland_edge_area_1000m_2023_large_deer_GB.tif")
#Divide number of pixels by the total number of 25m2 pixels in a buffer*100 = percentage woodland edge
Focal1000_EDGES_PERC <- (Focal1000_EDGES/5027)*100
writeRaster(Focal1000_EDGES_PERC, here("output/GB_datasets_2023/large_deer/percentage_cover_woodland_edge_1000m_2023_large_deer_GB.tif"))

#}

