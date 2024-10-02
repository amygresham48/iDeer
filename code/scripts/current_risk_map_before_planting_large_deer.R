#UPDATING iDEER SPATIAL LAYERS FOR BAYESIAN BELIEF NETWORK MODEL - LARGE DEER ####
#Author: Amy Gresham, July 2024

#The purpose of this script is to update spatial layers for the Bayesian
#Belief Network model that produces the initial deer damage risk map presented
#in the RShiny iDeer tool after new woodlands are inserted into landscape

library(sf)
library(raster)
library(fasterize)
library(dplyr)
library(here)
library(progress)
library(ggplot2)
library(stringr)
library(here)
library(pbapply)

#British National Grid
bng <- 27700

#Import current_risk datasets #--------------

#edge area within 1km

Focal1000_EDGE_AREA <- raster(here("output/EW_datasets_2022/large_deer/"))

#Area of arable land within 1km

Focal1000_ARABLE_AREA <- raster(here("output/EW_datasets_2022/large_deer/sum_arable_1000_2022.tif"))

#Sum of quality of perennial fodder (without arable) within 1km

Focal1000_PEREN_FORAGEQ  <- raster(here("output/EW_datasets_2022/large_deer/"))

#Combined urban/suburban proximity

URBAN_PROX <- raster(here("output/EW_datasets_2022/large_deer/"))

#Sum of DAMS within 1km

Focal1000_DAMS <- raster(here("output/EW_datasets_2022/large_deer/sum_dams_1000_non_wood_2022.tif"))

#Woodland area within 1km

Focal1000_WOOD <- raster(here("output/EW_datasets_2022/large_deer/sum_woodland_area_1km_large_deer_GB"))

#Ensure all rasters have same extent and projection#------------------------------



#Get code from current_deer_impact_risk_EW_small_deer to extract raster values#-----------------------------------
#Create a dataframe containing all extracted raster values within user's landscape

#EXTRACT LENGTH OF WOODLAND EDGE WITHIN 1KM

edgelen <- extract_raster(map_reclass = Focal1000_EDGE_AREA,
                          lcm = nfi_lcm_map)

ggplot(edgelen) +
  geom_tile(aes(x = x, y = y, fill = Focal1000_EDGE_AREA)) +
  scale_fill_viridis_c(option = "plasma") +  # Apply a continuous viridis color scale
  theme_minimal() +
  guides(fill = guide_colorbar(barwidth = 1, barheight = 10)) +  # Customize the colorbar appearance
  labs(title = "Edge area within 1000m",
       fill = "") 

#EXTRACT WOODLAND AREA WITHIN 1KM

woodpix <- extract_raster(map_reclass = Focal1000_WOOD,
                          lcm = nfi_lcm_map)

ggplot(woodpix) +
  geom_tile(aes(x = x, y = y, fill = Focal1000_WOOD)) +
  scale_fill_viridis_c(option = "plasma") +  # Apply a continuous viridis color scale
  theme_minimal() +
  guides(fill = guide_colorbar(barwidth = 1, barheight = 10)) +  # Customize the colorbar appearance
  labs(title = "Summed woodland area within 200m",
       fill = "") 


#EXTRACT SUMMED DAMS WITHIN 1KM
damspix <- extract_raster(map_reclass = Focal1000_SUM_DAMS,
                          lcm = nfi_lcm_map)

ggplot(damspix) +
  geom_tile(aes(x = x, y = y, fill = Focal1000_SUM_DAMS)) +
  scale_fill_viridis_c(option = "plasma") +  # Apply a continuous viridis color scale
  theme_minimal() +
  guides(fill = guide_colorbar(barwidth = 1, barheight = 10)) +  # Customize the colorbar appearance
  labs(title = "Summed DAMS within 1000m",
       fill = "") 

#EXTRACT PERENNIAL FORAGE QUALITY

foragepix <- extract_raster(map_reclass = Focal1000_PEREN_FORAGEQ,
                            lcm = nfi_lcm_map)

ggplot(foragepix) +
  geom_tile(aes(x = x, y = y, fill = Focal1000_PEREN_FORAGEQ)) +
  scale_fill_viridis_c(option = "plasma") +  # Apply a continuous viridis color scale
  theme_minimal() +
  guides(fill = guide_colorbar(barwidth = 1, barheight = 10)) +  # Customize the colorbar appearance
  labs(title = "Perennial and arable quality",
       fill = "") 

#EXTRACT ARABLE AREA

arablepix <- extract_raster(map_reclass = Focal1000_ARABLE_AREA,
                          lcm = nfi_lcm_map)

ggplot(arablepix) +
  geom_tile(aes(x = x, y = y, fill = Focal1000_ARABLE_AREA)) +
  scale_fill_viridis_c(option = "plasma") +  # Apply a continuous viridis color scale
  theme_minimal() +
  guides(fill = guide_colorbar(barwidth = 1, barheight = 10)) +  # Customize the colorbar appearance
  labs(title = "Edge area within 1000m",
       fill = "") 


#EXTRACT URBAN PROXIMIYY

urbanpix <- extract_raster(map_reclass = URBAN_PROX,
                          lcm = nfi_lcm_map)

ggplot(urbanpix) +
  geom_tile(aes(x = x, y = y, fill = URBAN_PROX)) +
  scale_fill_viridis_c(option = "plasma") +  # Apply a continuous viridis color scale
  theme_minimal() +
  guides(fill = guide_colorbar(barwidth = 1, barheight = 10)) +  # Customize the colorbar appearance
  labs(title = "Urban proximity",
       fill = "") 


#left_join the datasets together #---------------

df <- left_join(edgelen, connectpix, by = c("pixel_ID","x","y"))
df <- left_join(df,woodpix, by = c("pixel_ID","x","y"))
df <- left_join(df,damspix, by = c("pixel_ID","x","y"))
df <- left_join(df, foragepix, by = c("pixel_ID","x","y"))
df <- left_join(df, arablepix, by = c("pixel_ID","x","y"))
df <- left_join(df, urbanpix, by = c("pixel_ID","x","y"))


#Remove the NAs

df <- na.omit(df)


#plot the maps


#woodland edge area
ggplot(df) +
  geom_tile(aes(x = x, y = y, fill = Focal1000_EDGE_AREA)) +
  scale_fill_viridis_c(option = "plasma") +  # Apply a continuous viridis color scale
  theme_minimal() +
  guides(fill = guide_colorbar(barwidth = 1, barheight = 10)) +  # Customize the colorbar appearance
  labs(title = "woodland edge area within 1000m",
       fill = "") 

#woodland area
ggplot(df) +
  geom_tile(aes(x = x, y = y, fill = Focal1000_WOOD)) +
  scale_fill_viridis_c(option = "plasma") +  # Apply a continuous viridis color scale
  theme_minimal() +
  guides(fill = guide_colorbar(barwidth = 1, barheight = 10)) +  # Customize the colorbar appearance
  labs(title = "woodland edge area within 1000m",
       fill = "") 

#dams
ggplot(df) +
  geom_tile(aes(x = x, y = y, fill = Focal1000_DAMS)) +
  scale_fill_viridis_c(option = "plasma") +  # Apply a continuous viridis color scale
  theme_minimal() +
  guides(fill = guide_colorbar(barwidth = 1, barheight = 10)) +  # Customize the colorbar appearance
  labs(title = "summed dams within 1000m",
       fill = "") 

#arable area
perennial quality
ggplot(df) +
  geom_tile(aes(x = x, y = y, fill = Focal1000_ARABLE_AREA)) +
  scale_fill_viridis_c(option = "plasma") +  # Apply a continuous viridis color scale
  theme_minimal() +
  guides(fill = guide_colorbar(barwidth = 1, barheight = 10)) +  # Customize the colorbar appearance
  labs(title = "perennial and arable quality within 1000m",
       fill = "") 

#perennial quality
ggplot(df) +
  geom_tile(aes(x = x, y = y, fill = Focal1000_PEREN_FORAGEQ)) +
  scale_fill_viridis_c(option = "plasma") +  # Apply a continuous viridis color scale
  theme_minimal() +
  guides(fill = guide_colorbar(barwidth = 1, barheight = 10)) +  # Customize the colorbar appearance
  labs(title = "perennial and arable quality within 1000m",
       fill = "") 

#urban proximity
ggplot(df) +
  geom_tile(aes(x = x, y = y, fill = URBAN_PROX)) +
  scale_fill_viridis_c(option = "plasma") +  # Apply a continuous viridis color scale
  theme_minimal() +
  guides(fill = guide_colorbar(barwidth = 1, barheight = 10)) +  # Customize the colorbar appearance
  labs(title = "woodland edge area within 1000m",
       fill = "") 

#------------------------------------

#Reclassify continuous data into pre-specified LOW,MED,HIGH categories

#Need to look at overall dataset to identify these boundaries