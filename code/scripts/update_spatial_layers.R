#UPDATE iDEER SPATIAL LAYERS FOR BAYESIAN BELIEF NETWORK MODEL ####
#Author: Amy Gresham
#Date: 05/07/2024

#The purpose of this script is to update spatial layers for the Bayesian
#Belief Network model that produces the initial deer damage risk map presented
#in the RShiny iDeer tool, prior to users inserting new woodlands

#Each update requires a separate function for each spatial layer

library(sf)
library(raster)
library(fasterize)
library(dplyr)
library(here)
library(progress)

#Import datasets

#Latest NFI dataset
nfi <- st_read(here("data/raw-data/nfi-2022-raw/NATIONAL_FOREST_INVENTORY_GB_2022.shp"))

#Latest CEH LCM dataset
lcm <- raster(here("data/raw-data/lcm-2022/gblcm2022_25m.tif"))

#England and Wales 10k squares
ew10k <- sf::st_read(here("data/clean-data/10k_tiles_EW.shp"))

#OS Open Roads
roads <- sf::st_read(here("data/raw-data/open_roads.gpkg"), layer = "road_link") %>%
  st_transform(.,bng)

#Linear feature layer
#Dataset = CEH Woody Linear Feature Framework (2016)
#Need to modify to remove linear features within woodlands
lf <- raster(here("data/raw-data/linear_features/lf_raster.tif"))


#####################################################

#RUN FUNCTIONS ####

#1. The combined NFI/LCM land cover map
#This function produces a modified land cover raster with woodland edges
#classified by modal land cover type
#also produces a binary woodland raster to be used for subsequent spatial layers

#Call function
source(here("code/functions/NFI_CEHLCM_woodland_edge_function.R"))

update_map(nfi=nfi, 
           lcm=lcm,
           uk10k=uk10k,
           EW=EW)

#The following require on the update_map() function to be run first
#As this generates the updated woodland map

#Import binary raster and combined CEH/LCM land cover maps

cehlcm_map <- raster(here("output/edge_core_raster_all_tiles_EW.tif"))
NFI_LCM_woods_only <- raster(here("NFILCM_binary_woodland.tif"))

#Import shapefile of binary woodland map for connectivity analysis

hab_patches_all <- st_read(here("output/NFILCM_binary_woodland.shp"))

#2. The nearest main road (A road, B road or motorway)

source(here("code/functions/nearest_road_func.R"))

nearest_road(wood_binary_rast = NFI_LCM_woods_only, #woodland binary raster
            lcm=lcm, #Latest CEH LCM
            ew10k=ew10k, #BNG 10km tiles within England and Wales
            roads=roads)

#3. The nearest urban and suburban features

source(here("code/functions/nearest_urb_suburb_func.R"))

nearest_urb_suburb(wood_binary_rast = NFI_LCM_woods_only,
                   lcm = lcm,
                   tiles=ew10k)

#4. MAXIMUM DAMS (metric for landscape exposure outside of woodlands)
#Need to ensure this is using the combined NFI/LCM when identifying the open habitats

source(here("code/functions/max_dams_func.R"))

max_dams(wood_binary_rast = NFI_LCM_woods_only,
         dams_map = dams)

#5. Linear feature density (hedgerows and treelines)

source(here("code/functions/linear_feature_func.R"))

linear_feature_density(wood_binary_rast = NFI_LCM_woods_only,
                       tiles=ew10k,
                       lcm=lcm
                       )
#6. Connectivity

source(here("code/functions/connectivity_func.R"))

connectivity(hab_patches_all = nfi_lcm_overlaid_shapefile_woods_export)


