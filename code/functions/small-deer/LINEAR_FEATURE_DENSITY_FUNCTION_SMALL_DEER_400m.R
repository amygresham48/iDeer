#LINEAR FEATURE DENSITY WITHIN 400m FOR SMALL DEER MODEL ####

bng <- 27700

#Read in linear feature raster
#Created in ArcGIS
#Original dataset: CEH linear features (2016)
#The dataset has been rasterized
#Maintained full GB extent so as to not miss off hedgerows on the Scottish border
#lf <- raster(here("data/derived-data/WLF_GB_raster_.tif"))
#crs(lf) <- bng

#Read in linear feature raster
#Created in ArcGIS
#Original dataset: CEH linear features (2016)
#The dataset has been rasterized, snapped to gblcm2023_25m.tif
#Then masked to the original GB LF vector dataset to remove weird background values
#Maintained full GB extent so as to not miss off hedgerows on the Scottish border
lf <- raster("data/derived-data/WLF_GB_raster_LCM_matched_masked.tif")
crs(lf) <- bng

#focal statistics, moving window
#Count pixels that contain linear features up to 400m away from each woodland pixel

#if na.rm = F, edges become cropped.

circle.buff = raster::focalWeight(lf, d=400, type="circle",fillNA=T )#Create buffer
circle.buff[circle.buff > 0] <- 1   # replacing weights by 1
Focal400= raster::focal(x=lf, w=circle.buff, fun=sum, na.rm=T, pad=TRUE, padValue=NA)

plot(Focal400)

# Save raster
writeRaster(Focal400, "./output/GB_datasets_2023/small_deer/lf_density_400m_raster_2023_GB.tif")

#Divide number of pixels by the total number of 25m2 pixels in a 400m buffer*100 = percentage lf
Focal400_LF_PERC <- (Focal400/804)*100

#save raster
writeRaster(Focal400_LF_PERC, "C:/Users/ik929086/Documents/iDeer/output/GB_datasets_2023/small_deer/percentage_LF_area_400m_small_deer_GB_2023.tif",overwrite=TRUE)
