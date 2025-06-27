sum_perennial <- function(wood_binary_rast, nfi_lcm_map,tiles){

bng <- 27700

library(raster)
library(sf)
library(tidyverse)

#GB land cover map

lcm <- raster("./data/raw-data/lcm-2023/gblcm2023_25m.tif")
crs(lcm) <- bng


#Read in linear feature raster
#Created in ArcGIS
#Original dataset: CEH linear features (2016)
#The dataset has been rasterized, snapped to gblcm2023_25m.tif
#Then masked to the original GB LF vector dataset to remove weird background values
#Maintained full GB extent so as to not miss off hedgerows on the Scottish border
lf <- raster("data/derived-data/WLF_GB_raster_LCM_matched_masked.tif")
crs(lf) <- bng
lf_quality <- lf*10
#make zero values NAs
lf_quality[lf_quality == 0] <- NA

#Read in woods only raster
#Binary - Broadleaf & conifer are both 1
woods_only <- raster("C:/Users/ik929086/Documents/iDeer/output/GB_datasets_2023/woods_only_raster_GB_2023.tif")
crs(woods_only) <- bng
#Reclassify binary LF map as forage quality score of 10
woods_quality <- woods_quality*10
woods_quality[woods_quality == 0] <- NA


#UPDATE FORAGE QUALITY MAP ####

#LAND COVER EXTRACTION AT 200m AROUND WOODLAND PIXELS

forage.q.vals <- read.csv("C:/Users/ik929086/Documents/iDeer/data/raw-data/Expert_Qnaire/Forage_Q_medians_large_small_deer.csv")

land.cover.classes <- read.csv("C:/Users/ik929086/Documents/iDeer/data/raw-data/Expert_Qnaire/Expert_derived_quality_ranks_edge_types_with_water_no_mixed_woods.csv")

land.cover.classes <- land.cover.classes %>%
  dplyr::select(c(Land.cover, class))

#Filter for small deer only

forage.q.vals <- forage.q.vals %>% filter(species_group %in% c("small"))

forage.q.vals <- left_join(forage.q.vals, land.cover.classes, by = "Land.cover")

#Filter for perennial land cover types and arable

valid_land_cover <- c("Arable land","Calcareous grassland","Neutral grassland","Acid grassland","Fenland",
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
  dplyr::select(-c(Land.cover))

reclass_vals

#Make sure woods are scored as 10!
#where is = 1.000 and 2.000, becomes = 10
reclass_vals$becomes[reclass_vals$is == 1.000] <- 10
reclass_vals$becomes[reclass_vals$is == 2.000] <- 10

reclass_vals

#Reclassify
map_reclass <- raster::reclassify(lcm, reclass_vals)

#Combine alt_forage_map, lf_quality and woodland quality maps.
#Order of priority for overlay:
#Woodlands > Hedgerows > alt_forage

# Combine lf and peren_arable giving priority to lf_quality where it has non-zero and non-NA values
combined_lf_alt_forage <- overlay(lf_quality, map_reclass, fun = function(x, y) {
  ifelse(!is.na(x) & x != 0, x, y)  # Prioritize `lf_quality` only if non-zero
})

#Combine woods_quality with lf and alt_forage, giving priority to woods where it has non-zero and non-NA values
#This will erase any linear features present in woods
combined_lf_alt_forage_woods <- overlay(woods_quality, combined_lf_alt_forage, fun = function(x, y) {
  ifelse(!is.na(x) & x != 0, x, y)  # Prioritize `woods_quality` only if non-zero
})

#Export to inspect
writeRaster(combined_lf_alt_forage_woods, "C:/Users/ik929086/Documents/iDeer/output/GB_datasets_2023/small_deer/combined_lf_alt_forage_woods_quality_2023_small_deer.tif",overwrite=TRUE)

#Get percentage forage quality
#201 25x25m pixels can fit inside a 200m radius buffer
#Therefore max quality score = 2010 (201 x 10)
#Divide summed quality by max possible forage score (2010) then *100 to get percentage

#create circular buffers around every pixel of 200 metres
circle.buff = raster::focalWeight (combined_lf_alt_forage_woods, d=200, type="circle",fillNA=T )#Create buffer
circle.buff[circle.buff > 0] <- 1   # replacing weights by 1
Focal200_FORAGE_QUAL= raster::focal(combined_lf_alt_forage_woods, w=circle.buff, fun=sum, na.rm=T,pad=TRUE, padValue=NA,)

plot(Focal200_FORAGE_QUAL)
crs(Focal200_FORAGE_QUAL) <- bng

Forage200_FORAGE_QUAL_PERC <- (Focal200_FORAGE_QUAL/2010)*100

# Normalize the raster values from -100% to 100% → 0% to 100%
Forage200_FORAGE_QUAL_PERC_NORMALIZED <- (Forage200_FORAGE_QUAL_PERC + 100) / 2

writeRaster(Forage200_FORAGE_QUAL_PERC_NORMALIZED, "C:/Users/ik929086/Documents/iDeer/output/GB_datasets_2023/small_deer/percentage_alt_forage_quality_200m_small_deer_GB_2023.tif")


}

