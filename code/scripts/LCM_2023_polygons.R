#22/10/2024
#Decided not to use NFI dataset - just use CEH land cover map
#Then it will line up with the other land cover types
#Don't need to combine NFI and LCM.
#Use most recent CEH dataset (2023)

#Import CEH 2023 polygons

bng <- 27700

library(here)
library(sf)
library(dplyr)

lcm2023 <- st_read("C:/Users/ik929086/Documents/iDeer/data/raw-data/lcm-2023-vector/lcm-2023-vec_5670267.gpkg",layer = "lcm_2023", promote_to_multi = FALSE)

#X_mode = modal land cover category

#subset lcm2023 polygons for woodlands only
#mode = 1 = broadleaved woodland
#mode = 2 = coniferous woodland

lcm_woods <- lcm2023 %>%
  dplyr::filter(X_mode %in% c("1", "2"))
lcm_woods <- st_transform(lcm_woods, bng)

unique(st_geometry_type(lcm_woods))

#Subset the polygons that overlap England and Wales only

ew <- st_read(here("data/derived-data/10k_tiles_EW.shp"))

#using st_filter will prevent the polygons being cut
lcm_woods_EW <- st_filter(lcm_woods,ew)
#st_write(lcm_woods_EW, here("data/derived-data/LCM2023_EW_WOODS.shp"),append=FALSE)

#Some geometries invalid in lcm_woods
#Check if they are now all valid
invalid_geometries <- st_is_valid(lcm_woods)
print(all(invalid_geometries))  # Should return TRUE if all geometries are valid
invalid_indices <- which(!invalid_geometries)  # Indices of invalid geometries
invalid_polygons <- lcm_woods[invalid_indices, ]  # Extract invalid geometries
#make them valid
invalid_polygons_valid <- st_make_valid(invalid_polygons)
lcm_woods_valid <- lcm_woods
lcm_woods_valid[!invalid_geometries, ] <- invalid_polygons_valid
#Check if they are now all valid
all_valid <- st_is_valid(lcm_woods_valid)
print(all(all_valid))  # Should return TRUE if all geometries are valid
#This says the geometries are all valid but they are still broken when read into ArcGIS Pro

#cast to polygon, not multipolygon
lcm_woods <- st_cast(lcm_woods, "POLYGON")

lcm_woods <- lcm_woods %>%
  select(-c(X_conf, X_hist, X_mode, X_n, X_purity, X_stdev, X_agg, gid)) %>%
  mutate(patch_ID = row_number(),
         Shape_Area = st_area(geometry))%>%
  mutate(Shape_Area = as.numeric(Shape_Area))

#save as polygons
st_write(lcm_woods, here("data/derived-data/LCM2023_GB_WOODS_polygons.shp"),append=FALSE)

#Remaining processing done in ArcGIS Pro as follows:

#1. Repair geometries using the Repair Geometry tool for LCM2023_GB_WOODS_polygons

#2. All adjacent polygons merged regardless of type for polygons in GB (for connectivity analysis)

#filepath = st_read("./data/derived-data/LCM2023_GB_WOODS_ExportFeature.shp")

#2. Binary woodland raster of GB - rasterize LCM2023_GB_WOODS.shp

#filepath = raster("./data/derived-data/LCM2023WOODGB.tif)
