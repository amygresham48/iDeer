sum_perennial <- function(wood_binary_rast, nfi_lcm_map,tiles){

bng <- 27700

#land cover map with edges
map <- raster(here("C:/Users/ik929086/Documents/iDeer/output/EW_datasets_2022/edge_core_raster_2022_all_tiles_EW.tif"))

#This layer does NOT include woodland edge type
nfi_lcm_map <- raster(here("output/EW_datasets_2022/nfi_lcm_2022_overlaid.tif"))
crs(nfi_lcm_map) <- bng
nfi_lcm_map

# England and Wales 10k squares
ew10k <- sf::st_read(here("C:/Users/ik929086/Documents/iDeer/data/raw-data/os_bng_grids.gpkg"),layer="10km_grid")

#make perennial map only
#semi-natural vegetation

#This will be grasslands, heathland, fenland, saltmarsh
#suburban and urban not included

reclass_peren <- data.frame(is = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,
                                    18,19,20,21), becomes = c(0,0,0,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,1,0,0))

peren_raster_GB <- reclassify(nfi_lcm_map, reclass_peren)
crs(peren_raster_GB) <- bng

writeRaster(peren_raster_GB, here("output/EW_datasets_2022/perennial_raster_GB_2022.tif"),overwrite=TRUE)

#-------------------------------------

peren_raster_GB <- raster(here("C:/Users/ik929086/Documents/iDeer/output/EW_datasets_2022/perennial_raster_GB_2022.tif"))

#UPDATE FORAGE QUALITY MAP ####

#LAND COVER EXTRACTION AT 200m AROUND WOODLAND PIXELS

forage.q.vals <- read.csv(here("C:/Users/ik929086/Documents/iDeer/data/raw-data/Expert_Qnaire/Forage_Q_medians_large_small_deer.csv"))

land.cover.classes <- read.csv(here("C:/Users/ik929086/Documents/iDeer/data/raw-data/Expert_Qnaire/Expert_derived_quality_ranks_edge_types_with_water_no_mixed_woods.csv"))

land.cover.classes <- land.cover.classes %>%
  dplyr::select(c(Land.cover, class))

#Filter for large deer only

forage.q.vals <- forage.q.vals %>% filter(species_group %in% c("large"))

forage.q.vals <- left_join(forage.q.vals, land.cover.classes, by = "Land.cover")

#Filter for perennial land cover types

valid_land_cover <- c("Calcareous grassland","Neutral grassland","Acid grassland","Fenland",
                      "Heather grassland","Heather","Improved grassland","Peatland bog","Saltmarsh")

#Reclassify land cover map using forage quality scores

# Create the is-becomes matrix with conditional NA setting
reclass_vals <- forage.q.vals %>%
  dplyr::select(Median, class, Land.cover) %>%
  rename(becomes = Median, is = class) %>%
  relocate(is) %>%
  # Set becomes to NA for rows where Land.cover does not match the valid types
  mutate(becomes = if_else(Land.cover %in% valid_land_cover, becomes, NA_real_)) %>%
  filter(!is.na(is)) %>%
  select(-c(Land.cover))

reclass_vals

#Reclassify
map_reclass <- raster::reclassify(map, reclass_vals)

#focal statistics, moving window
#Sum of perennial land up to 1km away

#if na.rm = F, edges become cropped.

circle.buff = raster::focalWeight(map_reclass, d=1000, type="circle",fillNA=T)#Create buffer
circle.buff[circle.buff > 0] <- 1   # replacing weights by 1
Focal1000_PEREN= raster::focal(x=map_reclass, w=circle.buff, fun=sum, na.rm=T, pad=TRUE, padValue=NA)

plot(Focal1000_PEREN)
crs(Focal1000_PEREN) <- bng

writeRaster(Focal1000_PEREN, here("output/EW_datasets_2022/large_deer/sum_perennial_quality_1000m_large_deer_EW.tif"))




}

