#LINEAR FEATURE DENSITY WITHIN 200M FOR SMALL DEER MODEL ####

#Read in linear feature raster
#Created in ArcGIS (see notes in make_LF_raster.R)
#Original dataset: CEH linear features (2016)
#This raster has been trimmed to England-Wales 10k tiles and erased so that parts of LFs overlapping with
#woodland polygons from nfi/lcm 2022 combined dataset are removed

bng <- 27700

lf <- raster(here("data/derived-data/WLF_EW_RASTER.tif"))
crs(lf) <- bng

#focal statistics, moving window
#Count pixels that contain linear features up to 200m away from each woodland pixel

#if na.rm = F, edges become cropped.

circle.buff = raster::focalWeight(lf, d=200, type="circle",fillNA=T )#Create buffer
circle.buff[circle.buff > 0] <- 1   # replacing weights by 1
Focal200= raster::focal(x=lf, w=circle.buff, fun=sum, na.rm=T, pad=TRUE, padValue=NA)

plot(Focal200)

# Save raster
writeRaster(Focal200, here("output/lf_density_200m_raster_2022_EW.tif"),overwrite=TRUE)