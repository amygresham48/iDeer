#THIS TAKES AGES IN R!!! SO I USED ARCGIS PRO
#1. SELECT LAYER BY LOCATION: REMOVED ALL LINEAR FEATURES WHICH DID NOT OVERLAP WITH THE ENGLAND-WALES 10K TILES
#arcgis file = GB_WLF_v1_0_ENGLAND_WALES
#2. PAIRWISE CLIP: CLIP LINEAR FEATURES TO REMOVE PARTS THAT OVERLAP WITH LCM2022_EW_WOODS_ExportFeature.shp POLYGONS
#arcgis file = GB_WLF_v1_0_ENGLAND_WALES_ERASED
#3. RASTERIZE FEATURES
#arcgis file = WLF_EW_RASTER

#RESULT: RASTER OF LINEAR FEATURES IN ENGLAND AND WALES OUTSIDE OF WOODLANDS
#CAN THEN EXTRACT LENGTH OF LINEAR FEATURES WITHIN CERTAIN RADIUS OF WOODLAND PIXELS
#USED IN SMALL DEER BBN TO PREDICT DAMAGE RISK

#FILE NAME: 


##################################################################################
#Make linear feature raster layer ####

#This will allow us to perform focal statistics on the linear feature dataset and speed up data processing for the iDeer tool 


#Function to erase linear features inside woodland geometry
st_erase = function(x, y)st_difference(x, st_union(y))

#Linear feature layer
#Dataset = CEH Woody Linear Feature Framework (2016)
#Need to modify to remove linear features within woodlands

lf <- st_read(here("data/raw-data/linear_features/GB_WLF_V1_0.gdb"),layer="GB_WLF_V1_0")
st_crs(lf) <- bng

#Read in woodland polygons from ArcGIS pro:
wood_polys <- st_read(here("data/derived-data/NFILCM_2022_filtered_EW_arcgis.shp"))
st_crs(wood_polys) <- bng

hab_patches_all <- wood_polys %>%
  dplyr::select(-c(SHAPE_Leng, Shape_Area, Shape_Le_1)) %>%
  mutate(patch_ID = dplyr::row_number(),
         Shape_Area = st_area(geometry))%>%
  mutate(Shape_Area = as.numeric(Shape_Area))

#Get linear features outside of woodlands:
lf_erase <- st_erase(lf, hab_patches_all) #st_erase the lf that overlap woodlands
lf_erase <- st_as_sf(lf_erase)
