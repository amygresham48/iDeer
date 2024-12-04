#22/10/2024
#Decided not to use NFI dataset - just use CEH land cover map
#Then it will line up with the other land cover types
#Don't need to combine NFI and LCM.

#Import CEH 2022 polygons

bng <- 27700

library(here)
library(sf)
library(dplyr)

lcm2022 <- st_read("C:/Users/ik929086/Documents/iDeer/data/raw-data/lcm-2022-vector/lcm-2022-vec_5644691.gpkg",layer = "lcm_2022")

#x_mode = modal land cover category

#subset lcm2022 polygons for woodlands only
#mode = 1 = broadleaved woodland
#mode = 2 = coniferous woodland

lcm_woods <- lcm2022 %>%
  dplyr::filter(X_mode %in% c("1", "2"))

#Subset the polygons that overlap England and Wales only

ew <- st_read(here("data/derived-data/10k_tiles_EW.shp"))

#using st_filter will prevent the polygons being cut
lcm_woods_EW <- st_filter(lcm_woods,ew)

#write shapefiles

st_write(lcm_woods, here("data/derived-data/LCM2022_GB_WOODS.shp"),append=FALSE)
st_write(lcm_woods_EW, here("data/derived-data/LCM2022_EW_WOODS.shp"),append=FALSE)

#Remaining processing done in ArcGIS Pro as follows:

#Need two versions:

#1. All adjacent polygons of same type merged using "Dissolve function" on polygons in England & Wales

#filepath = st_read("./data/derived-data/LCM2022_EW_WOODS_ExportFeature.shp")

#2. All adjacent polygons merged regardless of type for polygons in GB (for connectivity analysis)

#filepath = st_read("./data/derived-data/LCM2022_GB_WOODS_ExportFeature.shp")

#3. Binary woodland raster of England-Wales

#filepath = raster("./data/derived-data/LCM2022WOODEW.tif)
