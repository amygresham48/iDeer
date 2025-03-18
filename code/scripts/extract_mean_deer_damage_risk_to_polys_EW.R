library(raster)
library(sf)
library(terra)
library(tidyverse)

bng <- 27700

#unmerged woodland polygons
GB_polys_unmerged <- st_read("C:/Users/ik929086/Documents/iDeer/data/derived-data/LCM2023_GB_WOODS_polygons.shp")

source("C:/Users/ik929086/Documents/iDeer/code/functions/extract_raster_pixels_to_wood_polys_func_10km_chunks_mean_poly_values.R")

#SMALL DEER ####

#read
r_rast_small <- raster("C:/Users/ik929086/Documents/iDeer/output/small_deer_risk_raster.tif")

crs(r_rast_small) <- bng
plot(r_rast_small)

#Extract raster values to woodland polygons
#Use custom function to loop over 10k tiles - takes too long unchunked
small_deer_risk_poly_vals <- extract_raster_to_wood_polygons(
  risk_map = r_rast_small,
  wood_polys = GB_polys_unmerged,
  tiles = ew10k)

small_deer_risk_polys <- small_deer_risk_poly_vals$result

#LARGE DEER ####

r_rast_large <- raster("C:/Users/ik929086/Documents/iDeer/output/large_deer_risk_raster.tif")
crs(r_rast_large) <- bng
plot(r_rast_large)

#Extract raster values to woodland polygons
#Use custom function to loop over 10k tiles - takes too long unchunked
large_deer_risk_poly_vals <- extract_raster_to_wood_polygons(
  risk_map = r_rast_large,
  wood_polys = GB_polys_unmerged,
  tiles = ew10k)

large_deer_risk_polys <- large_deer_risk_poly_vals$result