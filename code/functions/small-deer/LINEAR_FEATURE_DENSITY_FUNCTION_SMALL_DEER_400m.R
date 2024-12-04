#LINEAR FEATURE DENSITY WITHIN 400m FOR SMALL DEER MODEL ####

#Read in linear feature raster
#Created in ArcGIS
#Original dataset: CEH linear features (2016)
#The dataset has been rasterized
#Maintained full GB extent so as to not miss off hedgerows on the Scottish border

bng <- 27700

lf <- raster(here("data/derived-data/WLF_GB_raster_.tif"))
crs(lf) <- bng

#focal statistics, moving window
#Count pixels that contain linear features up to 400m away from each woodland pixel

#if na.rm = F, edges become cropped.

circle.buff = raster::focalWeight(lf, d=400, type="circle",fillNA=T )#Create buffer
circle.buff[circle.buff > 0] <- 1   # replacing weights by 1
Focal400= raster::focal(x=lf, w=circle.buff, fun=sum, na.rm=T, pad=TRUE, padValue=NA)

plot(Focal400)

# Save raster
writeRaster(Focal400, here("output/GB_datasets_2023/small_deer/lf_density_400m_raster_2023_GB.tif"),overwrite=TRUE)
