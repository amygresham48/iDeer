sum_perennial <- function(wood_binary_rast, nfi_lcm_map,tiles){

bng <- 27700

#GB land cover map

lcm <- raster("./data/raw-data/lcm-2023/gblcm2023_25m.tif")

crs(lcm) <- bng

#land cover map
#This layer does NOT include woodland edge type
#nfi_lcm_map <- raster(here("output/nfi_lcm_2022_overlaid.tif"))

#perennial vegetation AND arable AND woodland

#This will be grasslands, heathland, fenland, saltmarsh
#arable land included - small deer forager over a smaller area
#therefore the effect of attracting high densities to the area will not be very influential
#compared to the effect on far-ranging large species
#suburban and urban not included

reclass_peren_arable_woods <- data.frame(is = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,
                                    18,19,20,21), becomes = c(1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,1,0,0))

peren_arable_woods_raster_GB <- reclassify(lcm, reclass_peren_arable_woods)
crs(peren_arable_woods_raster_GB) <- bng

writeRaster(peren_arable_woods_raster_GB, "C:/Users/ik929086/Documents/iDeer/output/GB_datasets_2023/perennial_arable_woods_raster_GB_2023.tif",overwrite=TRUE)

#-------------------------------------

#peren_arable_woods_raster_GB <- raster(here("C:/Users/ik929086/Documents/iDeer/output/GB_datasets_2023/perennial_arable_raster_GB_2023.tif"))

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

valid_land_cover <- c("Broadleaved woodland","Coniferous woodland","Arable land","Calcareous grassland","Neutral grassland","Acid grassland","Fenland",
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

#Make sure woods are scored as 10!
#where is = 1.000 and 2.000, becomes = 10
reclass_vals$becomes[reclass_vals$is == 1.000] <- 10
reclass_vals$becomes[reclass_vals$is == 2.000] <- 10

reclass_vals

#Reclassify
map_reclass <- raster::reclassify(lcm, reclass_vals)

#create circular buffers around every pixel of 200 metres
circle.buff = raster::focalWeight (map_reclass, d=200, type="circle",fillNA=T )#Create buffer
circle.buff[circle.buff > 0] <- 1   # replacing weights by 1
Focal200_FORAGE_QUAL= raster::focal(map_reclass, w=circle.buff, fun=sum, na.rm=T,pad=TRUE, padValue=NA,)

plot(Focal200_FORAGE_QUAL)
crs(Focal200_FORAGE_QUAL) <- bng

writeRaster(Focal200_FORAGE_QUAL, "C:/Users/ik929086/Documents/iDeer/output/GB_datasets_2023/small_deer/sum_perennial_arable_woodland_forage_q_200m_small_deer_GB_2023.tif")


}

