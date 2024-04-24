#Mosaic lcm and nfi together

library(raster)
library(fasterize)
library(sf)
library(dplyr)
library(here)

#Set project directory
project_dir <- here::here("iDeer_spatial_data")

# Set the output directory
output_dir <- here(project_dir, "outputs")

#Import datasets ####

bng <- 27700

# CEH land cover map 2015 raster layer
lcm2015.rast <- raster::raster(here("inputs", "lcm-2015-tif", "lcm2015gb25m.tif"))
lcm2015.rast <- setMinMax(lcm2015.rast)
crs(lcm2015.rast) <- bng

#Import tidied nfi vector

nfi <- sf::st_read(here("inputs", "nfi-2015-tidied", "nfi_2015_GB_woodlands_only_shapefile.shp")) %>%
  sf::st_transform(., bng)

#make template raster
template_raster <- raster(ext = extent(lcm2015.rast), res = res(lcm2015.rast), crs = bng)

#rasterize nfi ####
wood.r <- fasterize::fasterize(nfi, template_raster, field = "wood_vals")

#At the moment, values 3 and 4 in wood.r are mixed woodland.
#Need to give them different values so they don't clash with lcm
#in lcm, 3 = arable and 4 = improved grassland

#In wood.r, change 3 --> 22 and 4 --> 23 ####
reclass_matrix <- matrix(c(3, 22, 4, 23), ncol = 2, byrow = TRUE)
wood_raster_reclass <- reclassify(wood.r, reclass_matrix)

#Mosaic the two rasters together using the overlay function ####
#This should give a raster with values from 1-23, where 1 and 2 are BL and conif woodland (same for lcm and NFI)
#Values 22 and 23 will be the mixed woodland types which are not included in the lcm but are included in the NFI dataset.
#All other remaining land cover types will be present

#Give priority to the first raster except where first raster has NAs ####
priority_function <- function(x, y) {
  
  ifelse(is.na(x), y, x)
}

nfi_lcm_mosaic <- raster::overlay(wood_raster_reclass, lcm2015.rast, fun = priority_function)

#export
writeRaster(nfi_lcm_mosaic, here(output_dir, "nfi_lcm_2015_overlaid.tif"))
