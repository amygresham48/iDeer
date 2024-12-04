

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
#ew10k <- st_read(here("data/derived-data/10k_tiles_EW.shp"))
#extent(ew10k)
#GB shapefile
#GB <- st_read("C:/Users/ik929086/OneDrive - University of Reading/Documents/Spatial datasets/GB shapefile/Countries_December_2022_GB_BFC_-8802398211591794926/CTRY_DEC_2022_GB_BFC.shp")
#Remove Scotland
#EW <- GB[!grepl("Scotland", GB$CTRY22NM),]
#This layer does NOT include woodland edge type
#Raw CEH LCM 2023 cropped to England and Wales in ArcGIS Pro
#lcm_EW <- raster("./data/derived-data/LCM2023_EW.tif")crs(nfi_lcm_map) <- bng
#extent(lcm_EW)
#lcm_EW_crop <- crop(lcm_EW, ew10k)
#lcm_EW_mask <- mask(lcm_EW_crop, ew10k)
#extent(lcm_EW_mask)
#DONE
#writeRaster(lcm_EW_mask, here("output/EW_datasets_2023/LCM2023_EW_masked.tif"))

#GB land cover map
lcm <- raster("C:/Users/ik929086/Documents/iDeer/data/raw-data/lcm-2023/gblcm2023_25m.tif")
#sum of DAMS within 200m
Focal200_DAMS <- raster("output/GB_datasets_2023/small_deer/sum_dams_200_non_wood_2023.tif")
Focal200_DAMS_project <- projectRaster(Focal200_DAMS, lcm)
writeRaster(Focal200_DAMS_project, here("output/GB_datasets_2023/small_deer/sum_dams_200_non_wood_2023_GB_projected.tif"))

#woodland connectivity within 200m
Focal200_WOOD_CONNECT <- raster("output/GB_datasets_2023/small_deer/woodland_connectivity_2023_400m_EW.tif")
#Already correct extent, don't need to reproject

#edge area within 200m ####
Focal200_EDGE_AREA <- raster("output/GB_datasets_2023/small_deer/sum_woodland_edges_200m_2023_small_deer_GB.tif")
#Already correct extent, don't need to reproject

#linear feature density within 200m ####
Focal200_LF <- raster("output/GB_datasets_2023/small_deer/lf_density_200m_quality_raster_2023_GB.tif")
Focal200_LF_project <- projectRaster(Focal200_LF, lcm)
writeRaster(Focal200_LF_project, here("output/GB_datasets_2023/small_deer/lf_density_200m_quality_2023_GB_projected.tif"))

#linear features within 400m
Focal400LF <- raster("C:/Users/ik929086/Documents/iDeer/output/GB_datasets_2023/small_deer/lf_density_400m_raster_2023_GB.tif")
Focal400_LF_project <- projectRaster(Focal400LF, lcm)
writeRaster(Focal400_LF_project, here("output/GB_datasets_2023/small_deer/lf_density_400m_2023_GB_projected.tif"))

#perennial/arable forage quality map ####
peren_arable_wood_200m <- raster("C:/Users/ik929086/Documents/iDeer/output/GB_datasets_2023/small_deer/sum_perennial_arable_woodland_forage_q_200m_small_deer_GB_2023.tif")

#Binary woodland raster####
woods_binary <- raster("data/derived-data/LCM2023WOODSGB.tif")
#Already correct extent, don't need to reproject


# Focal200_DAMS_mask <- raster(here("output/EW_datasets_2022/small_deer/sum_dams_200_non_wood_2022_EW_masked.tif"))
# crs(Focal200_DAMS_mask) <- bng
# 
# NFI_LCM_woods_only <- raster(here("output/NFILCM_2022_binary_woodland_all_tiles_EW.tif"))
# 
# #NFI_LCM_woods_only_crop <- crop(NFI_LCM_woods_only, ew10k)
# #NFI_LCM_woods_only_mask <- mask(NFI_LCM_woods_only_crop, ew10k)
# 
# NFI_LCM_woods_only_reproject <- projectRaster(NFI_LCM_woods_only, Focal200_DAMS_mask)
# 
# writeRaster(NFI_LCM_woods_only_reproject, here("output/EW_datasets_2022/small_deer/NFI_LCM_woods_only_2022_EW_reprojected.tif"),overwrite=TRUE)

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
