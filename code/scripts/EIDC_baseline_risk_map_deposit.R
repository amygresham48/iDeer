library(sf)
library(dplyr)

#load in datasets
large <- st_read("C:/Users/ik929086/OneDrive - University of Southampton/iDeer_Manuscripts/iDeer_Tool/Risk_maps_eidc_deposit/large_deer_impact_risk.gpkg")
small <- st_read("C:/Users/ik929086/OneDrive - University of Southampton/iDeer_Manuscripts/iDeer_Tool/Risk_maps_eidc_deposit/small_deer_impact_risk.gpkg")

#Remove tile_ID columns

large <- large %>% select(-c("tile_ID"))
small <- small %>% select(-c("tile_ID"))

#Reset patch_ID to 1:nrow

large$patch_ID <- 1:nrow(large)
small$patch_ID <- 1:nrow(small)

#Rename area columns

large <- large %>% rename(area_m2 = Wood_Area_m2)
small <- small %>% rename(area_m2 = Wood_Area_m2)

#Save (overwrite)
st_write(large, "C:/Users/ik929086/OneDrive - University of Southampton/iDeer_Manuscripts/iDeer_Tool/Risk_maps_eidc_deposit/large_deer_impact_risk.gpkg")
st_write(small, "C:/Users/ik929086/OneDrive - University of Southampton/iDeer_Manuscripts/iDeer_Tool/Risk_maps_eidc_deposit/small_deer_impact_risk.gpkg")
