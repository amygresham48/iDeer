#This polygon set needs to include all polygons from England Scotland and Wales
#So that the connectivity is correctly calculated for polygons on the Scottish borders

#Import CEH 2022 polygons

bng <- 27700

library(here)
library(sf)
library(dplyr)

lcm2022 <- st_read("C:/Users/ik929086/Documents/iDeer/data/raw-data/lcm-2022-vector/lcm-2022-vec_5644691.gpkg",layer = "lcm_2022")

#x_mode = modal land cover category

#subset lcm2022 polygons for woodlands only
#mode = 1 = broadleaved woodland
#mode = 2 = coniferous woodland

lcm_woods <- lcm2022 %>%
  dplyr::filter(X_mode %in% c("1", "2"))

#Import NFI polygons

NFI2022 <- st_read(here("data/raw-data/nfi-2022-raw/National_Forest_Inventory_GB_2022.shp"))
unique(NFI2022$CATEGORY)

#subset for "Woodland" in "CATEGORY"

#Rename col
lcm_woods <- lcm_woods %>% rename(ift_vals = X_mode)
#subset
lcm_woods_cols <- lcm_woods %>% select(ift_vals, geom)

#Get unique nfi categories
ifts <- unique(NFI2022$IFT_IOA)
ifts
n_cat <- length(ifts)

#Assign values to nfi, ensuring that the woodland polygon values match those of the LCM
ift_vals <- 50:(50 + n_cat - 1)
ift_vals

#Make dataframe

ift_df <- as.data.frame(ift_vals)
ift_df$IFT_IOA <- ifts

#Make values for Broadleaved = 1, Coniferous = 2
ift_df$ift_vals[ift_df$IFT_IOA == "Broadleaved"] <- 1
ift_df$ift_vals[ift_df$IFT_IOA == "Conifer"] <- 2
ift_df$ift_vals[ift_df$IFT_IOA == "Mixed mainly broadleaved"] <- 1 #classify as Broadleaved
ift_df$ift_vals[ift_df$IFT_IOA == "Mixed mainly conifer"] <- 2 #classify as Coniferous

#Add values to NFI dataset
nfi_vals <- dplyr::left_join(NFI2022, ift_df, by = c("IFT_IOA" = "IFT_IOA"))

#Filter all other ift_vals out
NFI2022_WOODS <- nfi_vals %>% dplyr::filter(ift_vals %in% c(1,2))
unique(NFI2022_WOODS$ift_vals)

#filter columns out of nfi to match lcm

nfi_val_cols <- nfi_vals %>% select(c("geometry","ift_vals"))

#write shapefiles

st_write(NFI2022_WOODS,here("data/derived-data/NFI2022_GB_WOODS.shp"))
st_write(lcm_woods, here("data/derived-data/LCM2022_GB_WOODS.shp"))

#Performed the remaining spatial processing in ArcGIS Pro as follows:

#1. in the LCM 2022 dataset, delete all features that overlap with features in the NFI 2022 dataset using the
# Select by Location tool --> select intersecting features --> delete intersecting features.

#2. Merge the modified LCM 2022 dataset with the NFI 2022 dataset

#3. Dissolve all woodland polygons contained within others (regardless of type) in the merged dataset
#for connectivity analysis
#using the Dissolve boundaries tool

#

#Read in: