

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

#This layer does NOT include woodland edge type
#Combined NFI/LCM land cover dataset
#nfi_lcm_map <- raster(here("output/nfi_lcm_2022_overlaid.tif"))
#crs(nfi_lcm_map) <- bng
#extent(nfi_lcm_map)

#arable area within 1000m
Focal1000_ARABLE_AREA <- raster(here("output/EW_datasets_2022/large_deer/sum_arable_area_1000m_2022_GB.tif"))
extent(Focal1000_ARABLE_AREA)

Focal1000_ARABLE_AREA_crop <- crop(Focal1000_ARABLE_AREA, ew10k)
Focal1000_ARABLE_AREA_mask <- mask(Focal1000_ARABLE_AREA_crop, ew10k)
extent(Focal1000_ARABLE_AREA_mask)
extent(ew10k)

writeRaster(Focal1000_ARABLE_AREA_mask, here("output/EW_datasets_2022/large_deer/sum_arable_area_1000m_2022_EW_masked.tif"))

#edge area within 1000m
Focal1000_EDGE_AREA <- raster(here("output/EW_datasets_2022/large_deer/sum_woodland_edges_1000m_large_deer_GB.tif"))
extent(Focal1000_EDGE_AREA)

Focal1000_EDGE_AREA_crop <- crop(Focal1000_EDGE_AREA, ew10k)
Focal1000_EDGE_AREA_mask <- mask(Focal1000_EDGE_AREA_crop, ew10k)

writeRaster(Focal1000_EDGE_AREA_mask, here("output/EW_datasets_2022/large_deer/sum_woodland_edges_1000m_large_deer_EW_masked.tif"))

#woodland area within 1000m
Focal1000_WOOD <- raster(here("output/EW_datasets_2022/large_deer/sum_woodland_area_1000m_large_deer_EW.tif"))

Focal1000_WOOD_crop <- crop(Focal1000_WOOD, ew10k)
Focal1000_WOOD_mask <- mask(Focal1000_WOOD_crop, ew10k)

writeRaster(Focal1000_WOOD_LF_mask, here("output/EW_datasets_2022/large_deer/sum_woodland_and_LF_area_1000m_large_deer_EW_masked.tif"))

#perennial forage quality map
Focal1000_PERENNIAL <- raster(here("output/EW_datasets_2022/large_deer/sum_perennial_forage_q_1000m_large_deer_GB.tif"))

Focal1000_PERENNIAL_crop <- crop(Focal1000_PERENNIAL, ew10k)
Focal1000_PERENNIAL_mask <- mask(Focal1000_PERENNIAL_crop, ew10k)

writeRaster(Focal1000_PERENNIAL_mask, here("output/EW_datasets_2022/large_deer/sum_perennial_forage_q_1000m_large_deer_EW_masked.tif"))

#sum of DAMS within 1000m
Focal1000_DAMS <- raster(here("output/EW_datasets_2022/large_deer/sum_dams_1000_non_wood_2022.tif"))

Focal1000_DAMS_crop <- crop(Focal1000_DAMS, ew10k)
Focal1000_DAMS_mask <- mask(Focal1000_DAMS_crop, ew10k)

writeRaster(Focal1000_DAMS_mask, here("output/EW_datasets_2022/large_deer/sum_dams_1000_non_wood_2022_EW_masked.tif"))

#Urban proximity 

URBAN_PROX <- raster(here("output/EW_datasets_2022/large_deer/urban_prox_2022.tif"))

URBAN_PROX_crop <- crop(URBAN_PROX, ew10k)
URBAN_PROX_mask <- mask(URBAN_PROX_crop, ew10k)

writeRaster(URBAN_PROX_mask, here("output/EW_datasets_2022/large_deer/URBAN_PROX_mask_2022_EW_masked.tif"))

#Binary woodland raster
#Done in spatial_matching_200m_layers
#Same as used for small deer model
#NFI_LCM_woods_only <- raster(here("output/NFILCM_2022_binary_woodland_all_tiles_EW.tif"))
#NFI_LCM_woods_only_crop <- crop(NFI_LCM_woods_only, ew10k)
#NFI_LCM_woods_only_mask <- mask(NFI_LCM_woods_only_crop, ew10k)
#writeRaster(Focal1000_DAMS_mask, here("output/EW_datasets_2022/large_deer/NFI_LCM_woods_only_2022_EW_masked.tif"))

