#Crop uk10k tiles to England and Wales ####

#Import uk10k shapefile

bng <- 

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

#save cropped uk10k file

write_sf(uk10k_EW, "C:/Users/ik929086/Documents/iDeer_Local/iDeer_spatial_data/intermediate_outputs/10k_tiles_EW.shp")

