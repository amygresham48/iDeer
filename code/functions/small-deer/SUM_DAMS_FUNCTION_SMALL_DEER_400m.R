#sum_dams <- function(wood_binary_rast, dams, nfi_lcm_map,tiles){

library(here)

bng <- 27700

#DAMS
#Interpolated from 50m resolution to 25m resolution
#To match the resolution of other raster layers
#Interpolation method = bilinear
dams <- raster("./data/raw-data/DAMS/dams_25m_bng.tif")
crs(dams) <- bng

#Merged woodland polygons
GB_polys_merged <- st_read("C:/Users/ik929086/Documents/iDeer/data/derived-data/LCM2023_GB_WOOD_ExportFeature.shp")
st_crs(GB_polys_merged) <- bng
GB_polys_merged$patch_ID <- 1:nrow(GB_polys_merged)

#GB land cover map
lcm <- raster("./data/raw-data/lcm-2023/gblcm2023_25m.tif")

# Crop dams to match the extent of lcm
dams_crop <- crop(dams, extent(lcm))

#Reclass nfi/lcm land cover map so all woodland habitat set to 0
#All non-woodland habitat set to 1

reclass <- data.frame(is = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,
                             18,19,20,21), becomes = c(0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1))

open_raster_GB <- reclassify(lcm, reclass)

#Multiply DAMs and binary LCM together - woodlands will be set to 0 DAMS

DAMS_open_raster <- dams*open_raster_GB

plot(DAMS_open_raster)

#Crop map to England and Wales

#DAMS_open_raster_EW <- crop(DAMS_open_raster,uk10k_EW)
#DAMS_open_raster_EW <- mask(DAMS_open_raster_EW,uk10k_EW)
#crs(DAMS_open_raster_EW) <- bng

#writeRaster(DAMS_open_raster, here("output/GB_datasets_2023/DAMS_open_raster_GB_2023.tif"),overwrite=TRUE)

#-------------------------------------

#DAMS_open_raster <- raster("./output/GB_datasets_2023/DAMS_open_raster_GB_2023.tif")

#focal statistics, moving window
#Calculate Summed DAMS up to 400m away from each woodland pixel
#if na.rm = F, edges become cropped.

circle.buff = raster::focalWeight(DAMS_open_raster, d=400, type="circle",fillNA=T )#Create buffer
circle.buff[circle.buff > 0] <- 1   # replacing weights by 1
Focal400_SUM_DAMS= raster::focal(x=DAMS_open_raster, w=circle.buff, fun=sum, na.rm=T, pad=TRUE, padValue=NA,
                         filename = here("output/GB_datasets_2023/small_deer/sum_dams_400_non_wood_2023.tif"), 
                         overwrite=T)#do focal window analysis with weights matrix

Focal400_SUM_DAMS <- raster("./output/GB_datasets_2023/small_deer/sum_dams_400_non_wood_2023.tif")

#RASTER EXTRACT METHOD
#Aggregate to patch, where the score for each patch is the normalised maximum summed DAMS score for any pixel in patch
#Assuming you have a patch identifier, e.g., 'patch_id' in your woodland polygons
patch_agg <- raster::extract(Focal400_SUM_DAMS, GB_polys_merged, fun=max, na.rm=TRUE, df=TRUE)
patch_agg <- patch_agg %>% rename(patch_ID = ID,
                                  max = sum_dams_400_non_wood_2023)
patch_agg$patch_ID <- GB_polys_merged$patch_ID

#EXACTEXTRACTR METHOD KEEPS patch_ID CONSISTENT
# Extract, keeping all attributes
#THE TWO METHODS GIVE SLIGHTLY DIFFERENT RESULTS
#SOME SMALLER WOODLANDS GET BUMPED INTO HIGHER CATEGORY WITH EXTRACTR METHOD
# patch_agg <- exactextractr::exact_extract(Focal400_normalized, GB_polys_merged, 'max', append_cols = TRUE) # include_cols keeps the original attributes
# patch_agg <- patch_agg %>%
#   select(-c(Shape_Area, Shape_Leng))

#left_join the max summed DAMS score to GB_polys_merged

GB_polys_merged_max_summed_DAMS <- left_join(patch_agg, GB_polys_merged, by = c("patch_ID"))
GB_polys_merged_max_summed_DAMS <- st_as_sf(GB_polys_merged_max_summed_DAMS)
st_write(GB_polys_merged_max_summed_DAMS, "./output/GB_datasets_2023/small_deer/max_sum_dams_400_2023_patch_agg.shp",append=FALSE)

#Rasterize

GB_polys_merged_max_summed_DAMS_raster <- fasterize::fasterize(sf = GB_polys_merged_max_summed_DAMS,
                                                                raster = lcm,
                                                                field = "max")

writeRaster(GB_polys_merged_max_summed_DAMS_raster,"./output/GB_datasets_2023/small_deer/max_summed_DAMS_400m_small_deer_GB_2023_.tif",overwrite=TRUE)

#DO THE NORMALIZATION AFTER EXTRACTING TO PATCH ####

summed_dams_min <- GB_polys_merged_max_summed_DAMS_raster@data@min
summed_dams_max <- GB_polys_merged_max_summed_DAMS_raster@data@max

#Normalise summed DAMS scores from 0-1
GB_polys_merged_max_summed_DAMS_raster_normalised <- (GB_polys_merged_max_summed_DAMS_raster - summed_dams_min) / 
  (summed_dams_max - summed_dams_min)

#save raster
writeRaster(GB_polys_merged_max_summed_DAMS_raster_normalised, "./output/GB_datasets_2023/small_deer/max_summed_DAMS_400m_small_deer_GB_2023_normalized.tif",overwrite=TRUE)
#writeRaster(GB_polys_merged_max_summed_DAMS_raster, "./output/GB_datasets_2023/small_deer/max_summed_DAMS_400m_small_deer_GB_2023.tif")

#}
