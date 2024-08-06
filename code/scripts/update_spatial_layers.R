#UPDATING iDEER SPATIAL LAYERS FOR BAYESIAN BELIEF NETWORK MODEL ####
#Author: Amy Gresham, July 2024

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

#British National Grid
bng <- 27700

#Import raw datasets ####

#Latest NFI dataset
nfi <- st_read(here("data/raw-data/nfi-2022-raw/NATIONAL_FOREST_INVENTORY_GB_2022.shp"))

#Latest CEH LCM dataset
lcm <- raster(here("data/raw-data/lcm-2022/gblcm2022_25m.tif"))

#uk10k
uk10k <-sf::st_read(dsn = "C:/Users/ik929086/OneDrive - University of Reading/Documents/Spatial datasets/os_bng_grids.gpkg", layer = "10km_grid")

#Import UK shapefile

#GB shapefile
GB <- st_read("C:/Users/ik929086/OneDrive - University of Reading/Documents/Spatial datasets/GB shapefile/Countries_December_2022_GB_BFC_-8802398211591794926/CTRY_DEC_2022_GB_BFC.shp")
#Remove Scotland
EW <- GB[!grepl("Scotland", GB$CTRY22NM),]

#st_filter to keep all 10k tiles that overlap EW
#using st_filter instead of st_intersection ensures that edges of tiles are not cut off

uk10k_EW <- sf::st_filter(uk10k, EW)

#OS Open Roads
roads <- sf::st_read(here("data/raw-data/open_roads.gpkg"), layer = "road_link") %>%
  st_transform(.,bng)

#Linear feature layer
#Dataset = CEH Woody Linear Feature Framework (2016)
#Need to modify to remove linear features within woodlands

lf <- st_read(here("data/raw-data/linear_features/GB_WLF_V1_0.gdb"),layer="GB_WLF_V1_0")
st_crs(lf) <- bng

#dams (Direct Aspect Method Scoring) dataset from Forest Research
#This layer has been pre-processed in ArcGIS Pro as follows: 

#1. CRS unknown, therefore assigned British National Grid
#Original dams layer had no defined crs, but the pixels line up with other BNG rasters

#2. Interpolated from 50m resolution to 25m resolution
#To match the resolution of other raster layers used in the functions below
#Interpolation method = bilinear

dams <- raster(here("data/raw-data/DAMS/dams_25m_bng.tif"))
crs(dams) <- bng

#-------------------------------------------------

#RUN FUNCTIONS ####

#1. The combined NFI/LCM land cover map
#This function produces a modified land cover raster with woodland edges
#classified by modal land cover type
#also produces a binary woodland raster to be used for subsequent spatial layers
#!WARNING! THIS FUNCTION MAY TAKE SEVERAL DAYS TO CLASSIFY WOODLAND EDGES DEPENDING ON SIZE OF AREA.
#FOR ENGLAND AND WALES, WILL TAKE 2-3 DAYS

#Call function
source(here("code/functions/NFI_LCM_RASTER_FUNCTION.R"))

update_map(nfi=nfi, #latest NFI dataset 
           lcm=lcm, # latest CEH LCM dataset
           tiles=uk10k_EW) #10k tiles for England and Wales

#-------------------------------------

#The following require the update_map() function to be run first
#As this generates the updated woodland map and binary map

#Import woodland raster and combined CEH/LCM land cover maps
#Generated from update_map() function

#This layer does NOT include woodland edge type
nfi_lcm_map <- raster(here("output/EW_datasets_2022/nfi_lcm_2022_overlaid.tif"))
crs(nfi_lcm_map) <- bng
NFI_LCM_woods_only <- raster(here("output/EW_datasets_2022/NFILCM_2022_binary_woodland_all_tiles_EW.tif"))
crs(NFI_LCM_woods_only) <- bng
#Ensure binary raster is 0/1, not 1/2
# Define the reclassification matrix
reclass_matrix <- matrix(c(1,0,  # From 1 to 0
                           2,1), # From 2 to 1
                         ncol=2, byrow=TRUE)
NFI_LCM_woods_only <- reclassify(NFI_LCM_woods_only, reclass_matrix)



#Make shapefile of binary woodland raster (0/1)
#convert to spatraster
spat_raster <- terra::rast(NFI_LCM_woods_only)
woodland_polys <- terra::as.polygons(spat_raster, values = FALSE)
woodland_polys <- st_as_sf(woodland_polys)
#Explode multipolygon into non-adjoining polygons
woodland_polys<-st_cast(woodland_polys,"POLYGON")

hab_patches_all <- woodland_polys %>%
  mutate(Id = row_number(),
         Shape_Area = st_area(geometry))%>%
  mutate(Shape_Area = as.numeric(Shape_Area))

#save hab_patches_all

st_write(hab_patches_all,here("outputs/NFI_LCM_2022_polys_made_in_R.shp"))
saveRDS(hab_patches_all, here("outputs/NFI_LCM_2022_polys_made_in_R.rds"))

#This takes AGES in R, so I did it in ArcGIS Pro instead using Raster to Polygon
#Converted NFILCM_2022_binary_woodland_all_tiles_EW.tif into a raster
#Ticked "Simplify Polygons" to smooth the edges.
#Then, merged adjoining polygons using "Dissolve Boundaries" function.
#Read in woodland polygons from ArcGIS pro:
#woodland_polys <- st_read(here("data/derived-data/NFI_LCM_woods_2022_raster_to_polygon.shp"))
#st_crs(woodland_polys) <- bng

hab_patches_all <- readRDS(here("data/derived-data/NFI_LCM_2022_polys_made_in_R.rds"))
hab_patches_all <- st_as_sf(hab_patches_all)

#hab_patches_all <- hab_patches_all %>%
#  select(-c(SHAPE_Leng, SHAPE_Area)) %>%
#  mutate(Id = row_number(),
#         Shape_Area = st_area(geometry))%>%
#  mutate(Shape_Area = as.numeric(Shape_Area))
  

#-------------------------------------

#2. The nearest main road (A road, B road or motorway)

source(here("code/functions/NEAREST_ROAD_RASTER_FUNCTION.R"))

nearest_road(wood_binary_rast = NFI_LCM_woods_only, #woodland binary raster
            lcm=lcm, #Latest CEH LCM to act as template raster
            tiles=uk10k_EW, #BNG 10km tiles within England and Wales
            roads=roads)

#------------------------------------

#3. The nearest urban and suburban features

source(here("code/functions/NEAREST_URBAN_SUBURBAN_RASTER_FUNCTION.R"))

nearest_urb_suburb(wood_binary_rast = NFI_LCM_woods_only, #woodland binary raster
                   nfi_lcm_map = nfi_lcm_map, #combined nfi/lcm raster
                   tiles=uk10k_EW)

#------------------------------------

#4. MAXIMUM DAMS (metric for landscape exposure outside of woodlands)
#Need to ensure this is using the combined NFI/LCM when identifying the open habitats

#THIS FUNCTION NEEDS FIXING!!
#Extent of output for focal statistics step does not match input

source(here("code/functions/MAX_DAMS_FUNCTION.R"))

max_dams(wood_binary_rast = NFI_LCM_woods_only,
         dams = dams,
         nfi_lcm_map = nfi_lcm_map,
         tiles=uk10k_EW)

#------------------------------------

#5. Linear feature density (hedgerows and treelines)

source(here("code/functions/LINEAR_FEATURE_DENSITY_FUNCTION.R"))

linear_feature_density(wood_binary_rast = NFI_LCM_woods_only,
                       tiles=uk10k_EW,
                       lcm=lcm,
                       hab_patches_all=hab_patches_all
                       )

#------------------------------------

#6. Connectivity

source(here("code/functions/WOODLAND_CONNECTIVITY_RASTER_FUNCTION.R"))

incoming_connectivity(hab_patches_all = hab_patches_all,
                      nfi_lcm_map = nfi_lcm_map,
                      )


