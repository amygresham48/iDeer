#sum_perennial <- function(wood_binary_rast, nfi_lcm_map,tiles){

bng <- 27700

#GB land cover map

lcm <- raster("./data/raw-data/lcm-2023/gblcm2023_25m.tif")

crs(lcm) <- bng

#-------------------------------------

#UPDATE FORAGE QUALITY MAP ####

#LAND COVER EXTRACTION AT 1000m AROUND WOODLAND PIXELS

forage.q.vals <- read.csv("C:/Users/ik929086/Documents/iDeer/data/raw-data/Expert_Qnaire/Forage_Q_medians_large_small_deer.csv")

land.cover.classes <- read.csv("C:/Users/ik929086/Documents/iDeer/data/raw-data/Expert_Qnaire/Expert_derived_quality_ranks_edge_types_with_water_no_mixed_woods.csv")

land.cover.classes <- land.cover.classes %>%
  dplyr::select(c(Land.cover, class))

#Filter for large deer only

forage.q.vals <- forage.q.vals %>% filter(species_group %in% c("large"))

forage.q.vals <- left_join(forage.q.vals, land.cover.classes, by = "Land.cover")

#Filter for perennial land cover types and woodland

valid_land_cover <- c("Broadleaved woodland","Coniferous woodland","Arable land","Calcareous grassland","Neutral grassland","Acid grassland","Fenland",
                      "Heather grassland","Heather","Improved grassland","Peatland bog","Saltmarsh", "Suburban areas","Urban areas","Inland_rock",
                      "Littoral_rock","Littoral_sediment","Saltwater","Freshwater","Supralittoral_rock","Supralittoral_sediment")


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

#Make sure woods are scored as 10!
#where is = 1.000 and 2.000, becomes = 10
reclass_vals$becomes[reclass_vals$is == 1.000] <- 10
reclass_vals$becomes[reclass_vals$is == 2.000] <- 10

reclass_vals

#Reclassify
map_reclass <- raster::reclassify(lcm, reclass_vals)

#Export to inspect
writeRaster(map_reclass, "C:/Users/ik929086/Documents/iDeer/output/GB_datasets_2023/large_deer/alt_forage_quality_2023_large_deer.tif",overwrite=TRUE)

#focal statistics, moving window
#Sum of perennial land up to 1km away

#if na.rm = F, edges become cropped.

#Get percentage forage quality
#5027 25x25m pixels can fit inside a 1000m radius buffer
#Therefore max quality score = 50270 (5027 x 10)
#Divide summed quality by max possible forage score (50270) then *100 to get percentage

circle.buff = raster::focalWeight(map_reclass, d=1000, type="circle",fillNA=T)#Create buffer
circle.buff[circle.buff > 0] <- 1   # replacing weights by 1
Focal1000_ALT_FORAGE= raster::focal(x=map_reclass, w=circle.buff, fun=sum, na.rm=T, pad=TRUE, padValue=NA)

writeRaster(Focal1000_ALT_FORAGE, ("./output/GB_datasets_2023/large_deer/sum_alt_forage_quality_1000m_large_deer_GB_2023.tif"))

Focal1000_ALT_FORAGE <- raster("./output/GB_datasets_2023/large_deer/sum_alt_forage_quality_1000m_large_deer_GB_2023.tif")

plot(Focal1000_ALT_FORAGE)
crs(Focal1000_ALT_FORAGE) <- bng

Forage1000_FORAGE_QUAL_PERC <- (Focal1000_ALT_FORAGE/50270)*100

# Normalize the raster values from -100% to 100% → 0% to 100%
Forage1000_FORAGE_QUAL_PERC_NORMALIZED <- (Forage1000_FORAGE_QUAL_PERC + 100) / 2

writeRaster(Forage1000_FORAGE_QUAL_PERC_NORMALIZED, "./output/GB_datasets_2023/large_deer/percentage_alt_forage_quality_1000m_large_deer_GB_2023.tif")

#}

