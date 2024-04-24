bng =27700 #coordinate system EPSG code (British National Grid = epsg:27700) 

library("sf")
library("dplyr")
library(raster)

#Process NFI GB data for 2015 ####

#Import NFI data 
nfi <- st_read("C:/Users/ik929086/Documents/iDeer_Local/iDeer_spatial_data/inputs/nfi-2015-raw/NATIONAL_FOREST_INVENTORY_GB_2015.shp")
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
ifts
unknowns <- ifts[c(4,7,8,9,12,13,18,20,23,27)]
unknown_woodlands <- nfi %>%filter(IFT_IOA %in% unknowns)
ifts_unknown <- unique(unknown_woodlands$IFT_IOA)
ifts_unknown

#calculate area of land cover classes within these ?woodland? polygons ####

#CEH land cover map 2015 raster layer
lcm2015 <- raster::raster("C:/Users/ik929086/Documents/iDeer_Local/iDeer_spatial_data/inputs/lcm-2015-tif/lcm2015gb25m.tif")
lcm2015 <- setMinMax(lcm2015$lcm2015gb25m_1)

# Create a data frame to store the results
unknown_df <- data.frame(unknown_df = 1:length(unknown_woodlands$OBJECTID))
unknown_df$OBJECTID <- unknown_woodlands$OBJECTID

# Loop through each polygon and calculate the area of land cover classes
for (i in 1:length(unknown_woodlands)) {
  # Extract values of raster pixels that intersect with the current polygon
  extracted_values <- raster::extract(lcm2015, unknown_woodlands[i,])
  
  # Calculate the area of land cover classes
  class_areas <- table(unlist(extracted_values)) * res(lcm2015)[1] * res(lcm2015)[2]
  
  # Add the results to the data frame
  unknown_df[i, names(class_areas)] <- as.vector(class_areas)
}


#Only 4 of the unknown polygons in whole of UK contain woodland (Broadleaved = 1)
#Polygon OBJECTIDs = 16, 18, 17, 19
unknown_df_woodlands <- unknown_df %>%
  filter(!is.na(.[, 5]))

#Add NFI polygon areas

unknown_df_woodlands <- merge(unknown_df_woodlands, nfi, by = "OBJECTID")

#When comparing the area of the unidentified land with the patch size,
#The majority of all four polygons are made up of broadleaved woodland (category 1)
#Therefore, change IFT_IOA category for these three polygons to BL woodland.

nfi$Category <- ifelse(nfi$OBJECTID %in% c("16","18","17","19"), "Woodland", nfi$Category)
nfi$IFT_IOA <- ifelse(nfi$OBJECTID %in% c("16","18","17","19"), "Broadleaved", nfi$IFT_IOA)

#Filter NFI data for woodlands only ####

#select for woodlands only

ifts <- unique(nfi$IFT_IOA)
ifts
ifts <- ifts[c(5,6,10,11,19,24)]
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

saveRDS(wood, "./iDeer_spatial_data/inputs/nfi-2015-tidied/nfi_2015_GB_woodlands_only.rds")
wood_rds <- readRDS("./iDeer_spatial_data/inputs/nfi-2015-tidied/nfi_2015_GB_woodlands_only.rds")
sf::st_write(wood_rds, "./iDeer_spatial_data/inputs/nfi-2015-tidied/nfi_2015_GB_woodlands_only_shapefile.shp", append=FALSE)

#END ####
