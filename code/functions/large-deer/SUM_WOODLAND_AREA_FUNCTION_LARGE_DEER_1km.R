sum_perennial <- function(wood_binary_rast, nfi_lcm_map,tiles){

bng <- 27700

#land cover map with edges
map <- raster(here("output/EW_datasets_2022/edge_core_raster_2022_all_tiles_EW.tif"))

forage.q.vals <- read.csv(here("data/raw-data/Expert_Qnaire/Forage_Q_medians_large_small_deer.csv"))

land.cover.classes <- read.csv(here("data/raw-data/Expert_Qnaire/Expert_derived_quality_ranks_edge_types_with_water_no_mixed_woods.csv"))

land.cover.classes <- land.cover.classes %>%
  dplyr::select(c(Land.cover, class))

#Filter for small deer only

forage.q.vals <- forage.q.vals %>% filter(species_group %in% c("large"))

forage.q.vals <- left_join(forage.q.vals, land.cover.classes, by = "Land.cover")

#Filter for edges

#Reclassify land cover map using forage quality scores

# Create the is-becomes matrix with woodlands set to 1 and all else set to 0
reclass_vals <- forage.q.vals %>%
  dplyr::select(Median, class, Land.cover) %>%
  rename(is = class) %>%
  relocate(is) %>%
  # Set becomes to 1 for rows where Land.cover is woodland, else set to 0
  mutate(becomes = ifelse(grepl("BL|Broadleaved|Conif", Land.cover), 1, 0)) %>%  
  filter(!is.na(is)) %>%
  select(-c(Land.cover,Median))

reclass_vals$is <- as.numeric(reclass_vals$is)
reclass_vals$becomes <- as.numeric(reclass_vals$becomes)

# create classification matrix
#This will classify 2.9 and under as 1 (woodland)
## all values > 0 and <= 2.9 become 1
#Any value >2.9 will be 0
reclass_matrix <- c(0, 2.9, 1,
                2.9, Inf, 0)
reclass_matrix

#Reclassify
map_reclass <- raster::reclassify(map, reclass_matrix)
map_reclass
plot(map_reclass)

#create circular buffers around every pixel of 1000 metres
circle.buff = raster::focalWeight (map_reclass, d=1000, type="circle",fillNA=T )#Create buffer
circle.buff[circle.buff > 0] <- 1   # replacing weights by 1
Focal1000_WOOD_AREA= raster::focal(map_reclass, w=circle.buff, fun=sum, na.rm=T,pad=TRUE, padValue=NA)

plot(Focal1000_WOOD_AREA)
crs(Focal1000_WOOD_AREA) <- bng

Focal1000_WOOD_AREA <- Focal1000_WOOD_AREA*25
plot(Focal1000_WOOD_AREA)

writeRaster(Focal1000_WOOD_AREA, here("output/EW_datasets_2022/large_deer/sum_woodland_area_1km_large_deer_GB.tif"),overwrite=TRUE)

}

