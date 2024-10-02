sum_perennial <- function(wood_binary_rast, nfi_lcm_map,tiles){

bng <- 27700

#land cover map with edges
map <- raster(here("output/EW_datasets_2022/edge_core_raster_2022_all_tiles_EW.tif"))

#land cover map
#This layer does NOT include woodland edge type
#nfi_lcm_map <- raster(here("output/nfi_lcm_2022_overlaid.tif"))

#perennial vegetation AND arable

#This will be grasslands, heathland, fenland, saltmarsh
#arable land included - small deer forager over a smaller area
#therefore the effect of attracting high densities to the area will not be very influential
#compared to the effect on far-ranging large species
#suburban and urban not included

reclass_peren_arable <- data.frame(is = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,
                                    18,19,20,21), becomes = c(0,0,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,1,0,0))

peren_arable_raster_EW <- reclassify(nfi_lcm_map, reclass_peren_arable)
crs(peren_raster_GB) <- bng

writeRaster(peren_arable_raster_GB, here("output/EW_datasets_2022/perennial_arable_raster_GB_2022.tif"),overwrite=TRUE)

#-------------------------------------

peren_arable_raster_GB <- raster(here("output/EW_datasets_2022/perennial_arable_raster_GB_2022.tif"))

#UPDATE FORAGE QUALITY MAP ####

#LAND COVER EXTRACTION AT 200m AROUND WOODLAND PIXELS

forage.q.vals <- read.csv(here("data/raw-data/Expert_Qnaire/Forage_Q_medians_large_small_deer.csv"))

land.cover.classes <- read.csv(here("data/raw-data/Expert_Qnaire/Expert_derived_quality_ranks_edge_types_with_water_no_mixed_woods.csv"))

land.cover.classes <- land.cover.classes %>%
  dplyr::select(c(Land.cover, class))

#Filter for small deer only

forage.q.vals <- forage.q.vals %>% filter(species_group %in% c("small"))

forage.q.vals <- left_join(forage.q.vals, land.cover.classes, by = "Land.cover")

#Filter for perennial land cover types and arable

valid_land_cover <- c("Arable land","Calcareous grassland","Neutral grassland","Acid grassland","Fenland",
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

#create circular buffers around every pixel of 200 metres
circle.buff = raster::focalWeight (map_reclass, d=200, type="circle",fillNA=T )#Create buffer
circle.buff[circle.buff > 0] <- 1   # replacing weights by 1
Focal200_FORAGE_QUAL= raster::focal(map_reclass, w=circle.buff, fun=sum, na.rm=T,pad=TRUE, padValue=NA,)

plot(Focal200_FORAGE_QUAL)
crs(Focal200_FORAGE_QUAL) <- bng

writeRaster(Focal200_FORAGE_QUAL, here("output/EW_datasets_2022/small_deer/sum_perennial_arable_forage_q_200m_small_deer_GB.tif"))


}

