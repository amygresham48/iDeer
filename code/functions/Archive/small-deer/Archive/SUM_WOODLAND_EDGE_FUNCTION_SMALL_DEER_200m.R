sum_perennial <- function(wood_binary_rast, nfi_lcm_map,tiles){

bng <- 27700

#land cover map with edges
map <- raster(here("output/EW_datasets_2022/edge_core_raster_2022_all_tiles_EW.tif"))

forage.q.vals <- read.csv(here("data/raw-data/Expert_Qnaire/Forage_Q_medians_large_small_deer.csv"))

land.cover.classes <- read.csv(here("data/raw-data/Expert_Qnaire/Expert_derived_quality_ranks_edge_types_with_water_no_mixed_woods.csv"))

land.cover.classes <- land.cover.classes %>%
  dplyr::select(c(Land.cover, class))

#Filter for small deer only

forage.q.vals <- forage.q.vals %>% filter(species_group %in% c("small"))

forage.q.vals <- left_join(forage.q.vals, land.cover.classes, by = "Land.cover")

# Reclassify based on the presence of "edge" in Land.cover
forage.q.vals$class_reclassified <- ifelse(grepl("edge", forage.q.vals$Land.cover), 1, 0)

# Create the is-becomes matrix 
reclass_vals <- forage.q.vals %>%
  dplyr::select(class, class_reclassified) %>%
  rename(is = class, becomes = class_reclassified)
reclass_vals
#Reclassify
map_reclass <- raster::reclassify(map, reclass_vals)
unique(map_reclass)
#[1] 0.000 1.000 1.201 1.202 1.300 1.600 1.700 1.800 1.900 2.201 2.202 2.300 2.600 2.700 2.800 2.900

#For some reason, can't directly reclassify decimal values...
#Need to do it the long way:
#Reclassify 1.00 (Broadleaved woodland) as 0
reclass_vals <- data.frame(is = 1, becomes = 0)
#Reclassify
map_reclass <- raster::reclassify(map_reclass, reclass_vals)
unique(map_reclass)
#[1] 0.000 1.201 1.202 1.300 1.600 1.700 1.800 1.900 2.201 2.202 2.300 2.600 2.700 2.800 2.900

#Reclassify all remaining values >0 as 1
# Reclassify values: set all values greater than 0 to 1, and others to 0
map_reclass_final <- calc(map_reclass, fun = function(x) {
  ifelse(x > 0, 1, 0) # Set values > 0 to 1, and others to 0
})
unique(map_reclass_final)
#[1] 0 1

#create circular buffers around every pixel of 200 metres
circle.buff = raster::focalWeight (map_reclass_final, d=200, type="circle",fillNA=T )#Create buffer
circle.buff[circle.buff > 0] <- 1   # replacing weights by 1
Focal200_EDGES= raster::focal(map_reclass_final, w=circle.buff, fun=sum, na.rm=T,pad=TRUE, padValue=NA)

plot(Focal200_EDGES)
crs(Focal200_EDGES) <- bng

Focal200_EDGES <- Focal200_EDGES*25

writeRaster(Focal200_EDGES, here("output/EW_datasets_2022/small_deer/sum_woodland_edges_200m_small_deer_EW.tif"))

}
