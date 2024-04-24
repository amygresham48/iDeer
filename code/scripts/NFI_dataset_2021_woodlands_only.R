bng =27700 #coordinate system EPSG code (British National Grid = epsg:27700) 
crs2 = "+init=epsg:27700"#coordinate system (British National Grid = "+init=epsg:27700")

library("sf")
library("dplyr")
library(raster)

#Process NFI GB data for 2021 ####

#Import NFI data 
nfi <- st_read("C:/Users/ik929086/Documents/iDeer_Local/iDeer_spatial_data/inputs/nfi-2021-raw/National_Forest_Inventory_Woodland_GB_2021.shp")
#nfi_polys <- st_read("C:/Users/ik929086/OneDrive - University of Reading/Documents/Spatial datasets/NFI GB/National_Forest_Inventory_Woodland_GB_2020.shp", quiet = TRUE, as = "POLYGON")
#nfi <- st_as_sf(nfi)
st_crs(nfi) <- bng

#convert dataset to polygons only if needed
unique(st_geometry_type(nfi))
#wood_polys <- nfi %>% 
#  dplyr::group_by(OBJECTID_1) %>% 
#  dplyr::summarise() %>%
#  st_cast("POLYGON")

#ASK LAND COVER MAP WHERE WOODLANDS ARE TO CHECK UNCERTAINTY IN NFI LAYER ####

#subset NFI data by: assumed woodland, young trees, low density, shrub, windblow, uncertain, felled, failed, cloud \\ shadow, ground prep

ifts <- unique(nfi$IFT_IOA)
unknowns <- ifts[c(11,14,15,16,17,18,21,24,25,26)]
unknown_woodlands <- nfi %>%filter(IFT_IOA %in% unknowns)
ifts_unknown <- unique(unknown_woodlands$IFT_IOA)
ifts_unknown

#calculate area of land cover classes within these ?woodland? polygons ####

#CEH land cover map 2021 raster layer
lcm2021 <- raster::raster("C:/Users/ik929086/OneDrive - University of Reading/Documents/Spatial datasets/Land use/land_use_2021/data/gblcm25m2021.tif")

# Create a data frame to store the results
unknown_df <- data.frame(unknown_df = 1:length(unknown_woodlands$OBJECTID_1))
unknown_df$OBJECTID_1 <- unknown_woodlands$OBJECTID_1

# Loop through each polygon and calculate the area of land cover classes
for (i in 1:length(unknown_woodlands)) {
  # Extract values of raster pixels that intersect with the current polygon
  extracted_values <- raster::extract(lcm2021, unknown_woodlands[i,])
  
  # Calculate the area of land cover classes
  class_areas <- table(unlist(extracted_values)) * res(lcm2021)[1] * res(lcm2021)[2]
  
  # Add the results to the data frame
  unknown_df[i, names(class_areas)] <- as.vector(class_areas)
}


#Only 4 of the unknown polygons in whole of UK contain woodland (Broadleaved = 1)
unknown_df_woodlands <- unknown_df %>%
  filter(!is.na(.[, 5]))

#Add NFI polygon areas

unknown_df_woodlands <- merge(unknown_df_woodlands, nfi, by = "OBJECTID_1")
unknown_df_woodlands$Area_m2 <- unknown_df_woodlands$Area_ha*10000

#The majority of polygons 30355, 30336 and 30338 are made up of broadleaved woodland (category 1)
#Therefore, change IFT_IOA category for these three polygons to BL woodland.

nfi$CATEGORY <- ifelse(nfi$OBJECTID_1 %in% c("30355","30336","30338"), "Woodland", nfi$CATEGORY)
nfi$IFT_IOA <- ifelse(nfi$OBJECTID_1 %in% c("30355","30336","30338"), "Broadleaved", nfi$IFT_IOA)

#Filter NFI data for woodlands only ####

#select for woodlands only

ifts <- unique(nfi$IFT_IOA)
ifts <- ifts[c(12,13,19,20,22,23)]
ifts
#[1] "Broadleaved"              "Conifer"                  "Mixed mainly broadleaved"
#[4] "Mixed mainly conifer"     "Coppice"                  "Coppice with standards"  

wood <- nfi %>%filter(IFT_IOA %in% ifts)

# Recategorize polygons labelled as "Coppice" to "Broadleaved"
wood <- wood %>%
  mutate(IFT_IOA = ifelse(IFT_IOA == "Coppice", "Broadleaved", IFT_IOA))

wood <- wood %>%
  mutate(IFT_IOA = ifelse(IFT_IOA == "Coppice with standards", "Broadleaved", IFT_IOA))

#Make a dataframe containing numbers for each IFT category

wood_vals <- c(1,2,3,4)
wood_names <- c("Broadleaved","Conifer","Mixed mainly broadleaved","Mixed mainly conifer")
wood_df <- as.data.frame(wood_vals)
wood_df$IFT_IOA <- wood_names

#Add wood_vals column to nfi data

wood <- left_join(wood, wood_df, by = c("IFT_IOA" = "IFT_IOA"))

#save the modified nfi dataframe

saveRDS(wood, "./iDeer_spatial_data/inputs/nfi-2021-tidied/nfi_2021_GB_woodlands_only.rds")
wood_rds <- readRDS("./iDeer_spatial_data/inputs/nfi-2021-tidied/nfi_2021_GB_woodlands_only.rds")
sf::st_write(wood_rds, "./iDeer_spatial_data/inputs/nfi-2021-tidied/nfi_2021_GB_woodlands_only_shapefile.shp", append=FALSE)

############################

#Spare code

#convert dataset to polygons only
unique(st_geometry_type(wood))

wood_polys <- wood %>% 
  dplyr::group_by(OBJECTID_1) %>% 
  dplyr::summarise() %>%
  st_cast("POLYGON")

unique(st_geometry_type(wood_polys))

#Add info columns to polygon dataframe
#Need to remove geometry of wood df to do this
wood_no_geom <- wood %>% st_drop_geometry()

wood_polys_info <- wood_polys %>% left_join(wood_no_geom)

