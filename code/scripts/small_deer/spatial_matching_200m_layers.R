

library(sf)
library(raster)
library(fasterize)
library(dplyr)
library(here)
library(progress)
library(ggplot2)
library(stringr)
library(here)
library(pbapply)
library(ggplot2)
library(reshape2)

#British National Grid
bng <- 27700

#Import current_risk datasets #--------------
#Make sure they are all of the same extent (England & Wales)

#England-Wales 10k tiles
ew10k <- st_read(here("data/derived-data/10k_tiles_EW.shp"))
extent(ew10k)

#GB shapefile
GB <- st_read("C:/Users/ik929086/OneDrive - University of Reading/Documents/Spatial datasets/GB shapefile/Countries_December_2022_GB_BFC_-8802398211591794926/CTRY_DEC_2022_GB_BFC.shp")
#Remove Scotland
EW <- GB[!grepl("Scotland", GB$CTRY22NM),]

#This layer does NOT include woodland edge type
#Combined NFI/LCM land cover dataset
nfi_lcm_map <- raster(here("output/nfi_lcm_2022_overlaid.tif"))
crs(nfi_lcm_map) <- bng
extent(nfi_lcm_map)

#sum of DAMS within 200m
Focal200_DAMS <- raster(here("output/EW_datasets_2022/small_deer/sum_dams_200_non_wood_2022.tif"))

Focal200_DAMS_crop <- crop(Focal200_DAMS, ew10k)
Focal200_DAMS_mask <- mask(Focal200_DAMS_crop, ew10k)

writeRaster(Focal200_DAMS_mask, here("output/EW_datasets_2022/small_deer/sum_dams_200_non_wood_2022_EW_masked.tif"))

#woodland connectivity within 200m
Focal200_WOOD_CONNECT <- raster(here("output/EW_datasets_2022/small_deer/woodland_connectivity_200m_2022_GB.tif"))
extent(Focal200_WOOD_CONNECT)

Focal200_WOOD_CONNECT_crop <- crop(Focal200_WOOD_CONNECT, ew10k)
Focal200_WOOD_CONNECT_mask <- mask(Focal200_WOOD_CONNECT_crop, ew10k)
extent(Focal200_WOOD_CONNECT_mask)
extent(ew10k)

writeRaster(Focal200_WOOD_CONNECT_mask, here("output/EW_datasets_2022/small_deer/woodland_connectivity_200m_2022_EW_masked.tif"))

#edge area within 200m ####

#Read focal dams raster to act as reproject template
#cropping and masking is not working on this raster for some reason
#maybe because it does not contain zeros, only NAs

Focal200_DAMS_mask <- raster(here("output/EW_datasets_2022/small_deer/sum_dams_200_non_wood_2022_EW_masked.tif"))
crs(Focal200_DAMS_mask) <- bng

Focal200_EDGE_AREA <- raster(here("output/EW_datasets_2022/small_deer/sum_woodland_edges_200m_small_deer_EW.tif"))
extent(Focal200_EDGE_AREA)
plot(Focal200_EDGE_AREA)

Focal200_EDGE_AREA_reproject <- projectRaster(Focal200_EDGE_AREA, Focal200_DAMS_mask)
writeRaster(Focal200_EDGE_AREA_reproject, here("output/EW_datasets_2022/small_deer/sum_woodland_edges_200m_small_deer_EW_reprojected.tif"),overwrite=TRUE)

#Focal200_EDGE_AREA_crop <- crop(Focal200_EDGE_AREA, ew10k)
#Focal200_EDGE_AREA_mask <- mask(Focal200_EDGE_AREA_crop, ew10k)
#writeRaster(Focal200_EDGE_AREA_mask, here("output/EW_datasets_2022/small_deer/sum_woodland_edges_200m_small_deer_EW_masked.tif"),overwrite=TRUE)

#linear feature density within 200m ####
Focal200_LF <- raster(here("output/EW_datasets_2022/small_deer/lf_density_200m_2022.tif"))

Focal200_LF_crop <- crop(Focal200_LF, ew10k)
Focal200_LF_mask <- mask(Focal200_LF_crop, ew10k)

writeRaster(Focal200_LF_mask, here("output/EW_datasets_2022/small_deer/lf_density_200m_2022_EW_masked.tif"))

#woodland and linear feature area within 200m ####
#made using script combine_lf_wood_area.R
Focal200_WOOD_LF <- raster(here("output/EW_datasets_2022/small_deer/sum_woodland_and_LF_area_200m_small_deer_EW.tif"))

Focal200_WOOD_LF_crop <- crop(Focal200_WOOD_LF, ew10k)
Focal200_WOOD_LF_mask <- mask(Focal200_WOOD_LF_crop, ew10k)

writeRaster(Focal200_WOOD_LF_mask, here("output/EW_datasets_2022/small_deer/sum_woodland_and_LF_area_200m_small_deer_EW_masked.tif"))

#perennial/arable forage quality map ####
Focal200_PEREN_ARABLE_FORAGEQ <- raster(here("output/EW_datasets_2022/small_deer/sum_perennial_arable_forage_q_200m_small_deer_GB.tif"))

Focal200_PEREN_ARABLE_FORAGEQ_crop <- crop(Focal200_PEREN_ARABLE_FORAGEQ, ew10k)
Focal200_PEREN_ARABLE_FORAGEQ_mask <- mask(Focal200_PEREN_ARABLE_FORAGEQ_crop, ew10k)

writeRaster(Focal200_PEREN_ARABLE_FORAGEQ_mask, here("output/EW_datasets_2022/small_deer/sum_perennial_arable_forage_q_200m_small_deer_EW_masked.tif"))


#Binary woodland raster####
#Read focal dams raster to act as reproject template
#cropping and masking is not working on this raster for some reason
#maybe because it does not contain zeros, only NAs

Focal200_DAMS_mask <- raster(here("output/EW_datasets_2022/small_deer/sum_dams_200_non_wood_2022_EW_masked.tif"))
crs(Focal200_DAMS_mask) <- bng

NFI_LCM_woods_only <- raster(here("output/NFILCM_2022_binary_woodland_all_tiles_EW.tif"))

#NFI_LCM_woods_only_crop <- crop(NFI_LCM_woods_only, ew10k)
#NFI_LCM_woods_only_mask <- mask(NFI_LCM_woods_only_crop, ew10k)

NFI_LCM_woods_only_reproject <- projectRaster(NFI_LCM_woods_only, Focal200_DAMS_mask)

writeRaster(NFI_LCM_woods_only_reproject, here("output/EW_datasets_2022/small_deer/NFI_LCM_woods_only_2022_EW_reprojected.tif"),overwrite=TRUE)

##Read in woodland polygons from ArcGIS pro:
wood_polys <- st_read(here("data/derived-data/NFILCM_2022_GB_polys_arcgis.shp"))
st_crs(wood_polys) <- bng

hab_patches_all <- wood_polys %>%
  dplyr::select(-c(SHAPE_Leng, SHAPE_Area)) %>%
  mutate(patch_ID = dplyr::row_number(),
         Shape_Area = st_area(geometry))%>%
  mutate(Shape_Area = as.numeric(Shape_Area))

#Filter by ew10k tiles - DONE IN ARCGIS
#FILE NAME = NFILCM_2022_filtered_EW_arcgis.shp
