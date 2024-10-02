

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
nfi_lcm_map <- raster(here("output/nfi_lcm_2022_overlaid.tif"))
crs(nfi_lcm_map) <- bng
extent(nfi_lcm_map)

#woodland connectivity within 200m
Focal200_WOOD_CONNECT <- raster(here("output/EW_datasets_2022/small_deer/woodland_connectivity_200m_2022_GB.tif"))
extent(Focal200_WOOD_CONNECT)

Focal200_WOOD_CONNECT_crop <- crop(Focal200_WOOD_CONNECT, ew10k)
Focal200_WOOD_CONNECT_mask <- mask(Focal200_WOOD_CONNECT_crop, ew10k)
extent(Focal200_WOOD_CONNECT_mask)
extent(ew10k)

writeRaster(Focal200_WOOD_CONNECT_mask, here("output/EW_datasets_2022/small_deer/woodland_connectivity_200m_2022_EW_masked.tif"))

#edge area within 200m
Focal200_EDGE_AREA <- raster(here("output/EW_datasets_2022/small_deer/sum_woodland_edges_200m_small_deer_GB.tif"))
extent(Focal200_EDGE_AREA)

Focal200_EDGE_AREA_crop <- crop(Focal200_EDGE_AREA, ew10k)
Focal200_EDGE_AREA_mask <- mask(Focal200_EDGE_AREA_crop, ew10k)

writeRaster(Focal200_EDGE_AREA_mask, here("output/EW_datasets_2022/small_deer/sum_woodland_edges_200m_small_deer_EW_masked.tif"))

#linear feature density within 200m
Focal200_LF <- raster(here("output/EW_datasets_2022/small_deer/lf_density_200m_2022.tif"))

Focal200_LF_crop <- crop(Focal200_LF, ew10k)
Focal200_LF_mask <- mask(Focal200_LF_crop, ew10k)

writeRaster(Focal200_LF_mask, here("output/EW_datasets_2022/small_deer/lf_density_200m_2022_EW_masked.tif"))

#woodland and linear feature area within 200m
#made using script combine_lf_wood_area.R
Focal200_WOOD_LF <- raster(here("output/EW_datasets_2022/small_deer/sum_woodland_and_LF_area_200m_small_deer_EW.tif"))

Focal200_WOOD_LF_crop <- crop(Focal200_WOOD_LF, ew10k)
Focal200_WOOD_LF_mask <- mask(Focal200_WOOD_LF_crop, ew10k)

writeRaster(Focal200_WOOD_LF_mask, here("output/EW_datasets_2022/small_deer/sum_woodland_and_LF_area_200m_small_deer_EW_masked.tif"))

#perennial/arable forage quality map
Focal200_PEREN_ARABLE_FORAGEQ <- raster(here("output/EW_datasets_2022/small_deer/sum_perennial_arable_forage_q_200m_small_deer_GB.tif"))

Focal200_PEREN_ARABLE_FORAGEQ_crop <- crop(Focal200_PEREN_ARABLE_FORAGEQ, ew10k)
Focal200_PEREN_ARABLE_FORAGEQ_mask <- mask(Focal200_PEREN_ARABLE_FORAGEQ_crop, ew10k)

writeRaster(Focal200_PEREN_ARABLE_FORAGEQ_mask, here("output/EW_datasets_2022/small_deer/sum_perennial_arable_forage_q_200m_small_deer_EW_masked.tif"))

#sum of DAMS within 200m
Focal200_DAMS <- raster(here("output/EW_datasets_2022/small_deer/sum_dams_200_non_wood_2022.tif"))

Focal200_DAMS_crop <- crop(Focal200_DAMS, ew10k)
Focal200_DAMS_mask <- mask(Focal200_DAMS_crop, ew10k)

writeRaster(Focal200_DAMS_mask, here("output/EW_datasets_2022/small_deer/sum_dams_200_non_wood_2022_EW_masked.tif"))


