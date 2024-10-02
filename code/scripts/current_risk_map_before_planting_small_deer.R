#UPDATING iDEER SPATIAL LAYERS FOR BAYESIAN BELIEF NETWORK MODEL - SMALL DEER ####
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
library(ggplot2)
library(reshape2)

#British National Grid
bng <- 27700

#function to extract pixel values from raster layers
source(here("code/functions/extract_raster_pixels_func_10km_chunks.R"))

#Import current_risk datasets #--------------

#This layer does NOT include woodland edge type
nfi_lcm_map <- raster(here("output/nfi_lcm_2022_overlaid.tif"))
crs(nfi_lcm_map) <- bng
extent(nfi_lcm_map)

#The following have all had their extents matched in the script spatial_matching_200m_layers.R

#woodland connectivity within 200m
Focal200_WOOD_CONNECT <- raster(here("output/EW_datasets_2022/small_deer/woodland_connectivity_200m_2022_EW_masked.tif"))

#edge area within 200m
Focal200_EDGE_AREA <- raster(here("output/EW_datasets_2022/small_deer/sum_woodland_edges_200m_small_deer_EW_masked.tif"))

#linear feature density within 200m
Focal200_LF <- raster(here("output/EW_datasets_2022/small_deer/lf_density_200m_2022.tif"))

#woodland and linear feature area within 200m
#made using script combine_lf_wood_area.R, then spatial_matching_200m_layers.R
Focal200_WOOD_LF <- raster(here("output/EW_datasets_2022/small_deer/sum_woodland_and_LF_area_200m_small_deer_EW_masked.tif"))

#perennial/arable forage quality map
Focal200_PEREN_ARABLE_FORAGEQ <- raster(here("output/EW_datasets_2022/small_deer/sum_perennial_arable_forage_q_200m_small_deer_EW_masked.tif"))

#sum of DAMS within 200m
Focal200_DAMS <- raster(here("output/EW_datasets_2022/small_deer/sum_dams_200_non_wood_2022_EW_masked.tif"))



#Update layers #####-------------------------------------------------

#Extract raster values
#Create a dataframe containing all extracted raster values

#EXTRACT LENGTH OF WOODLAND EDGE

#try with a few tiles

ew10k_test_tiles <- ew10k[866:888,]
ew10k_test_tiles <- st_as_sf(ew10k_test_tiles)

edgelen <- extract_raster(tiles = 
                            ew10k_test_tiles,
                          map_reclass = Focal200_EDGE_AREA,
                          lcm = nfi_lcm_map)

ggplot(edgelen) +
  geom_tile(aes(x = x, y = y, fill = Focal200_EDGE_AREA)) +
  scale_fill_viridis_c(option = "plasma") +  # Apply a continuous viridis color scale
  theme_minimal() +
  guides(fill = guide_colorbar(barwidth = 1, barheight = 10)) +  # Customize the colorbar appearance
  labs(title = "Edge area within 200m",
       fill = "") 

#EXTRACT CONNECTIVITY

connectpix <- extract_raster(map_reclass = Focal200_WOOD_CONNECT,
                             lcm = nfi_lcm_map)

ggplot(connectpix) +
  geom_tile(aes(x = x, y = y, fill = Focal200_WOOD_CONNECT)) +
  scale_fill_viridis_c(option = "plasma") +  # Apply a continuous viridis color scale
  theme_minimal() +
  guides(fill = guide_colorbar(barwidth = 1, barheight = 10)) +  # Customize the colorbar appearance
  labs(title = "Total incoming connectivity from within 200m",
       fill = "") 


#EXTRACT LF DENSITY WITHIN 200M

LFpix <- extract_raster(map_reclass = Focal200_LF_AREA,
                          lcm = nfi_lcm_map)

ggplot(LFpix) +
  geom_tile(aes(x = x, y = y, fill = Focal200_LF_AREA)) +
  scale_fill_viridis_c(option = "plasma") +  # Apply a continuous viridis color scale
  theme_minimal() +
  guides(fill = guide_colorbar(barwidth = 1, barheight = 10)) +  # Customize the colorbar appearance
  labs(title = "Summed LF area within 200m",
       fill = "") 

#EXTRACT WOODLAND AND LF AREA WITHIN 200M

woodpix <- extract_raster(map_reclass = Focal_200_WOOD_LF_AREA_SUM,
                          lcm = nfi_lcm_map)

ggplot(woodpix) +
  geom_tile(aes(x = x, y = y, fill = Focal_200_WOOD_LF_AREA_SUM)) +
  scale_fill_viridis_c(option = "plasma") +  # Apply a continuous viridis color scale
  theme_minimal() +
  guides(fill = guide_colorbar(barwidth = 1, barheight = 10)) +  # Customize the colorbar appearance
  labs(title = "Summed woodland area within 200m",
       fill = "") 


#EXTRACT SUMMED DAMS
damspix <- extract_raster(map_reclass = Focal200_DAMS,
                          lcm = nfi_lcm_map)

ggplot(damspix) +
  geom_tile(aes(x = x, y = y, fill = Focal200_DAMS)) +
  scale_fill_viridis_c(option = "plasma") +  # Apply a continuous viridis color scale
  theme_minimal() +
  guides(fill = guide_colorbar(barwidth = 1, barheight = 10)) +  # Customize the colorbar appearance
  labs(title = "Summed DAMS within 200m",
       fill = "") 

#EXTRACT PERENNIAL/ARABLE FORAGE QUALITY

foragepix <- extract_raster(map_reclass = Focal200_PEREN_ARABLE_FORAGEQ,
                          lcm = nfi_lcm_map)

ggplot(foragepix) +
  geom_tile(aes(x = x, y = y, fill = Focal200_PEREN_ARABLE_FORAGEQ)) +
  scale_fill_viridis_c(option = "plasma") +  # Apply a continuous viridis color scale
  theme_minimal() +
  guides(fill = guide_colorbar(barwidth = 1, barheight = 10)) +  # Customize the colorbar appearance
  labs(title = "Perennial and arable quality",
       fill = "") 

#left_join the datasets together #---------------

df <- left_join(edgelen, connectpix, by = c("pixel_ID","x","y"))
df <- left_join(df,LFpix, by = c("pixel_ID","x","y"))
df <- left_join(df,woodpix, by = c("pixel_ID","x","y"))
df <- left_join(df,damspix, by = c("pixel_ID","x","y"))
df <- left_join(df, foragepix, by = c("pixel_ID","x","y"))

#Remove the NAs

df <- na.omit(df)


#plot the maps

#woodland + linear feature area
ggplot(df) +
  geom_tile(aes(x = x, y = y, fill = Focal_200_WOOD_LF_AREA_SUM)) +
  scale_fill_viridis_c(option = "plasma") +  # Apply a continuous viridis color scale
  theme_minimal() +
  guides(fill = guide_colorbar(barwidth = 1, barheight = 10)) +  # Customize the colorbar appearance
  labs(title = "woodland + linear feature area within 200m",
       fill = "") 

#woodland edge area
ggplot(df) +
  geom_tile(aes(x = x, y = y, fill = Focal200_EDGE_AREA)) +
  scale_fill_viridis_c(option = "plasma") +  # Apply a continuous viridis color scale
  theme_minimal() +
  guides(fill = guide_colorbar(barwidth = 1, barheight = 10)) +  # Customize the colorbar appearance
  labs(title = "woodland edge area within 200m",
       fill = "") 

#woodland connectivity
ggplot(df) +
  geom_tile(aes(x = x, y = y, fill = Focal200_WOOD_CONNECT)) +
  scale_fill_viridis_c(option = "plasma") +  # Apply a continuous viridis color scale
  theme_minimal() +
  guides(fill = guide_colorbar(barwidth = 1, barheight = 10)) +  # Customize the colorbar appearance
  labs(title = "woodland edge area within 200m",
       fill = "") 

#linear feature density
ggplot(df) +
  geom_tile(aes(x = x, y = y, fill = Focal200_LF)) +
  scale_fill_viridis_c(option = "plasma") +  # Apply a continuous viridis color scale
  theme_minimal() +
  guides(fill = guide_colorbar(barwidth = 1, barheight = 10)) +  # Customize the colorbar appearance
  labs(title = "linear feature density within 200m",
       fill = "") 

#dams
ggplot(df) +
  geom_tile(aes(x = x, y = y, fill = Focal200_DAMS)) +
  scale_fill_viridis_c(option = "plasma") +  # Apply a continuous viridis color scale
  theme_minimal() +
  guides(fill = guide_colorbar(barwidth = 1, barheight = 10)) +  # Customize the colorbar appearance
  labs(title = "summed dams within 200m",
       fill = "") 

#perennial and arable quality
ggplot(df) +
  geom_tile(aes(x = x, y = y, fill = Focal200_PEREN_ARABLE_FORAGEQ)) +
  scale_fill_viridis_c(option = "plasma") +  # Apply a continuous viridis color scale
  theme_minimal() +
  guides(fill = guide_colorbar(barwidth = 1, barheight = 10)) +  # Customize the colorbar appearance
  labs(title = "perennial and arable quality within 200m",
       fill = "") 

#------------------------------------

#Reclassify continuous data into pre-specified LOW,MED,HIGH categories

#Need to look at overall dataset to identify these boundaries

#For now, just use this example dataset

df_cat <- df

#Linear features
hist(df_cat$Focal200_LF_AREA)
# Replace numeric values with cat labels
df_cat$Focal200_LF_AREA <- ifelse(df_cat$Focal200_LF_AREA<= 500, "LOW",
                ifelse(df_cat$Focal200_LF_AREA> 500 & df_cat$Focal200_LF_AREA <= 1000, "MED",
                       ifelse(df_cat$Focal200_LF_AREA> 1000, "HIGH", NA)))
unique(df_cat$Focal200_LF_AREA)

#Woodland connectivity

hist(df_cat$connect_raster)
df_cat$connect_raster <- ifelse(df_cat$connect_raster <= 50000, "LOW",
                                        ifelse(df_cat$connect_raster > 50000 & df_cat$connect_raster <=100000, "MED",
                                               ifelse(df_cat$connect_raster > 100000, "HIGH", NA)))
unique(df_cat$connect_raster)

#Summed woodland linear feature area

hist(df_cat$Focal_200_WOOD_LF_AREA_SUM)
df_cat$Focal_200_WOOD_LF_AREA_SUM <- ifelse(df_cat$Focal_200_WOOD_LF_AREA_SUM <= 1500, "LOW",
                                ifelse(df_cat$Focal_200_WOOD_LF_AREA_SUM > 1500 & df_cat$Focal_200_WOOD_LF_AREA_SUM <=3000, "MED",
                                       ifelse(df_cat$Focal_200_WOOD_LF_AREA_SUM > 3000, "HIGH", NA)))
unique(df_cat$Focal_200_WOOD_LF_AREA_SUM)

#DAMS

hist(df_cat$Focal200_SUM_DAMS)
df_cat$Focal200_SUM_DAMS <- ifelse(df_cat$Focal200_SUM_DAMS <= 1500, "LOW",
                                            ifelse(df_cat$Focal200_SUM_DAMS > 1500 & df_cat$Focal200_SUM_DAMS <=3000, "MED",
                                                   ifelse(df_cat$Focal200_SUM_DAMS > 3000, "HIGH", NA)))
unique(df_cat$Focal200_SUM_DAMS)

#Alternative forage quality

hist(df_cat$Focal200_FORAGE_QUAL)
df_cat$Focal200_FORAGE_QUAL <- ifelse(df_cat$Focal200_FORAGE_QUAL <= -50, "LOW",
                                   ifelse(df_cat$Focal200_FORAGE_QUAL > -50 & df_cat$Focal200_FORAGE_QUAL <=50, "MED",
                                          ifelse(df_cat$Focal200_FORAGE_QUAL > 50, "HIGH", NA)))
unique(df_cat$Focal200_FORAGE_QUAL)

#Edge area

hist(df_cat$Focal200_EDGE_AREA)
df_cat$Focal200_EDGE_AREA <- ifelse(df_cat$Focal200_EDGE_AREA <= 400, "LOW",
                                      ifelse(df_cat$Focal200_EDGE_AREA > 400 & df_cat$Focal200_EDGE_AREA <=800, "MED",
                                             ifelse(df_cat$Focal200_EDGE_AREA > 800, "HIGH", NA)))
unique(df_cat$Focal200_EDGE_AREA)
#------------------------------------

#Set up conditional probability tables for BBN

#CPTS for measured nodes ####

#woodland + linear feature area
wood_lf_cpt<-matrix(c(0.333333,0.333333,0.333333),ncol = 3,dimnames = list(NULL, c("LOW", "MED","HIGH")))
#woodland edge area
edge_cpt <-matrix(c(0.333333,0.333333,0.333333),ncol = 3,dimnames = list(NULL, c("LOW", "MED","HIGH")))
#linear feature area
lf_cpt <-matrix(c(0.333333,0.333333,0.333333),ncol = 3,dimnames = list(NULL, c("LOW", "MED","HIGH")))
#dams
dams_cpt<-matrix(c(0.333333,0.333333,0.333333),ncol = 3,dimnames = list(NULL, c("LOW", "MED","HIGH")))
#perennial and arable quality
alt_forage_cpt <- matrix(c(0.333333,0.333333,0.333333),ncol = 3,dimnames = list(NULL, c("LOW", "MED","HIGH")))
#woodland connectivity
wood_connect_cpt <- matrix(c(0.333333,0.333333,0.333333),ncol = 3,dimnames = list(NULL, c("LOW", "MED","HIGH")))

#CPT for Connectivity Index ####

connect_index_cpt <- array(
  0,  # Default probability for each cell
  dim = c(3, 3, 3),  # Shape of the array (3x3x3)
  dimnames = list(
    connectivity_index = c("LOW", "MED", "HIGH"),
    lf_length_200m = c("LOW", "MED", "HIGH"),
    wood_connectivity_200m = c("LOW", "MED", "HIGH")
  )
)
#order = connectivity_index, lf_length_200m, wood_connectivity_200m

#table1: lf_length_200m = LOW, wood_connectivity_200m = LOW
connect_index_cpt["LOW","LOW","LOW"] <- 0.95
connect_index_cpt["MED","LOW","LOW"] <- 0.05
connect_index_cpt["HIGH","LOW","LOW"] <- 0

#table3: lf_length_200m = MED, wood_connectivity_200m = LOW
connect_index_cpt["LOW","MED","LOW"] <- 0.15
connect_index_cpt["MED","MED","LOW"] <- 0.7
connect_index_cpt["HIGH","MED","LOW"] <- 0.15

#table3: lf_length_200m = HIGH, wood_connectivity_200m = LOW
connect_index_cpt["LOW","HIGH","LOW"] <- 0.2
connect_index_cpt["MED","HIGH","LOW"] <- 0.5
connect_index_cpt["HIGH","HIGH","LOW"] <- 0.3

#table4: lf_length_200m = LOW, wood_connectivity_200m = MED
connect_index_cpt["LOW","LOW","MED"] <- 0.1
connect_index_cpt["MED","LOW","MED"] <- 0.8
connect_index_cpt["HIGH","LOW","MED"] <- 0.1

#table5: lf_length_200m = LOW, wood_connectivity_200m = HIGH
connect_index_cpt["LOW","LOW","HIGH"] <- 0
connect_index_cpt["MED","LOW","HIGH"] <- 0.3
connect_index_cpt["HIGH","LOW","HIGH"] <- 0.7

#table6: lf_length_200m = MED, wood_connectivity_200m = MED
connect_index_cpt["LOW","MED","MED"] <- 0.025
connect_index_cpt["MED","MED","MED"] <- 0.95
connect_index_cpt["HIGH","MED","MED"] <- 0.025

#table7: lf_length_200m = HIGH, wood_connectivity_200m = HIGH
connect_index_cpt["LOW","HIGH","HIGH"] <- 0
connect_index_cpt["MED","HIGH","HIGH"] <- 0.1
connect_index_cpt["HIGH","HIGH","HIGH"] <- 0.9

#table8: lf_length_200m = HIGH, wood_connectivity_200m = MED
connect_index_cpt["LOW","HIGH","MED"] <- 0
connect_index_cpt["MED","HIGH","MED"] <- 0.2
connect_index_cpt["HIGH","HIGH","MED"] <- 0.8

#table9: lf_length_200m = MED, wood_connectivity_200m = HIGH
connect_index_cpt["LOW","MED","HIGH"] <- 0
connect_index_cpt["MED","MED","HIGH"] <- 0.1
connect_index_cpt["HIGH","MED","HIGH"] <- 0.9


connect_index_cpt

# Convert the CPT to a data frame for plotting
connect_index_df <- as.data.frame(as.table(connect_index_cpt))

# Rename the columns for clarity
colnames(connect_index_df) <- c("connect_index","lf_density_200m", "woodland_connect_200m","probability")

# Create the plot
p <- ggplot(connect_index_df, aes(x = connect_index, y = probability, group = woodland_connect_200m)) +
  geom_line(size = 1) +  # Line plot for probability distribution
  geom_point(size = 2) +  # Points on the curves
  facet_grid(rows = vars(woodland_connect_200m), cols = vars(lf_density_200m),labeller="label_both") +  
  theme_bw()+
  labs(
    x = "Connectivity Index",
    y = "Probability")

# Display the plot
print(p)

#CPT for Foraging pressure index ------------------------------------------------####

forage_pressure_index_cpt <- array(
  0,  # Default probability for each cell
  dim = c(3, 3, 3, 3),  # Shape of the array (3x3x3)
  dimnames = list(
    forage_pressure_index = c("LOW", "MED", "HIGH"),
    alt_forage_qual_200m = c("LOW", "MED", "HIGH"),
    wood_lf_area_200m = c("LOW", "MED", "HIGH"),
    wood_edge_area_200m = c("LOW", "MED", "HIGH")
  )
)

#order = forage_pressure_index, alt_forage_qual_200m, wood_lf_area_200m,wood_edge_area_200m

# Updated CPT for forage_pressure_index to ensure at most one 1 per row

forage_pressure_index_cpt["LOW","LOW","LOW","LOW"] <- 0.1
forage_pressure_index_cpt["MED","LOW","LOW","LOW"] <- 0.8
forage_pressure_index_cpt["HIGH","LOW","LOW","LOW"] <- 0.1

forage_pressure_index_cpt["LOW","MED","LOW","LOW"] <- 0.2
forage_pressure_index_cpt["MED","MED","LOW","LOW"] <- 0.7
forage_pressure_index_cpt["HIGH","MED","LOW","LOW"] <- 0.1

forage_pressure_index_cpt["LOW","HIGH","LOW","LOW"] <- 0.5
forage_pressure_index_cpt["MED","HIGH","LOW","LOW"] <- 0.3
forage_pressure_index_cpt["HIGH","HIGH","LOW","LOW"] <- 0.2

forage_pressure_index_cpt["LOW","LOW","MED","LOW"] <- 0.7
forage_pressure_index_cpt["MED","LOW","MED","LOW"] <- 0.2
forage_pressure_index_cpt["HIGH","LOW","MED","LOW"] <- 0.1

forage_pressure_index_cpt["LOW","MED","MED","LOW"] <- 0.8
forage_pressure_index_cpt["MED","MED","MED","LOW"] <- 0.15
forage_pressure_index_cpt["HIGH","MED","MED","LOW"] <- 0.05

forage_pressure_index_cpt["LOW","HIGH","MED","LOW"] <- 0.7
forage_pressure_index_cpt["MED","HIGH","MED","LOW"] <- 0.2
forage_pressure_index_cpt["HIGH","HIGH","MED","LOW"] <- 0.1

forage_pressure_index_cpt["LOW","LOW","HIGH","LOW"] <- 0.8
forage_pressure_index_cpt["MED","LOW","HIGH","LOW"] <- 0.15
forage_pressure_index_cpt["HIGH","LOW","HIGH","LOW"] <- 0.05

forage_pressure_index_cpt["LOW","MED","HIGH","LOW"] <- 0.8
forage_pressure_index_cpt["MED","MED","HIGH","LOW"] <- 0.15
forage_pressure_index_cpt["HIGH","MED","HIGH","LOW"] <- 0.05

forage_pressure_index_cpt["LOW","HIGH","HIGH","LOW"] <- 0.8
forage_pressure_index_cpt["MED","HIGH","HIGH","LOW"] <- 0.15
forage_pressure_index_cpt["HIGH","HIGH","HIGH","LOW"] <- 0.05

forage_pressure_index_cpt["LOW","LOW","LOW","MED"] <- 0.1
forage_pressure_index_cpt["MED","LOW","LOW","MED"] <- 0.7
forage_pressure_index_cpt["HIGH","LOW","LOW","MED"] <- 0.2

forage_pressure_index_cpt["LOW","MED","LOW","MED"] <- 0.1
forage_pressure_index_cpt["MED","MED","LOW","MED"] <- 0.5
forage_pressure_index_cpt["HIGH","MED","LOW","MED"] <- 0.4

forage_pressure_index_cpt["LOW","HIGH","LOW","MED"] <- 0.1
forage_pressure_index_cpt["MED","HIGH","LOW","MED"] <- 0.3
forage_pressure_index_cpt["HIGH","HIGH","LOW","MED"] <- 0.6

forage_pressure_index_cpt["LOW","LOW","MED","MED"] <- 0.3
forage_pressure_index_cpt["MED","LOW","MED","MED"] <- 0.5
forage_pressure_index_cpt["HIGH","LOW","MED","MED"] <- 0.2

forage_pressure_index_cpt["LOW","MED","MED","MED"] <- 0.1
forage_pressure_index_cpt["MED","MED","MED","MED"] <- 0.8
forage_pressure_index_cpt["HIGH","MED","MED","MED"] <- 0.1

forage_pressure_index_cpt["LOW","HIGH","MED","MED"] <- 0.6
forage_pressure_index_cpt["MED","HIGH","MED","MED"] <- 0.2
forage_pressure_index_cpt["HIGH","HIGH","MED","MED"] <- 0.2

forage_pressure_index_cpt["LOW","LOW","HIGH","MED"] <- 0.5
forage_pressure_index_cpt["MED","LOW","HIGH","MED"] <- 0.3
forage_pressure_index_cpt["HIGH","LOW","HIGH","MED"] <- 0.2

forage_pressure_index_cpt["LOW","MED","HIGH","MED"] <- 0.15
forage_pressure_index_cpt["MED","MED","HIGH","MED"] <- 0.7
forage_pressure_index_cpt["HIGH","MED","HIGH","MED"] <- 0.15

forage_pressure_index_cpt["LOW","HIGH","HIGH","MED"] <- 0.4
forage_pressure_index_cpt["MED","HIGH","HIGH","MED"] <- 0.4
forage_pressure_index_cpt["HIGH","HIGH","HIGH","MED"] <- 0.2

forage_pressure_index_cpt["LOW","LOW","LOW","HIGH"] <- 0.1
forage_pressure_index_cpt["MED","LOW","LOW","HIGH"] <- 0.6
forage_pressure_index_cpt["HIGH","LOW","LOW","HIGH"] <- 0.3

forage_pressure_index_cpt["LOW","MED","LOW","HIGH"] <- 0.2
forage_pressure_index_cpt["MED","MED","LOW","HIGH"] <- 0.4
forage_pressure_index_cpt["HIGH","MED","LOW","HIGH"] <- 0.4

forage_pressure_index_cpt["LOW","HIGH","LOW","HIGH"] <- 0.1
forage_pressure_index_cpt["MED","HIGH","LOW","HIGH"] <- 0.3
forage_pressure_index_cpt["HIGH","HIGH","LOW","HIGH"] <- 0.6

forage_pressure_index_cpt["LOW","LOW","MED","HIGH"] <- 0.25
forage_pressure_index_cpt["MED","LOW","MED","HIGH"] <- 0.5
forage_pressure_index_cpt["HIGH","LOW","MED","HIGH"] <- 0.25

forage_pressure_index_cpt["LOW","MED","MED","HIGH"] <- 0.25
forage_pressure_index_cpt["MED","MED","MED","HIGH"] <- 0.5
forage_pressure_index_cpt["HIGH","MED","MED","HIGH"] <- 0.25

forage_pressure_index_cpt["LOW","HIGH","MED","HIGH"] <- 0.2
forage_pressure_index_cpt["MED","HIGH","MED","HIGH"] <- 0.3
forage_pressure_index_cpt["HIGH","HIGH","MED","HIGH"] <- 0.5

forage_pressure_index_cpt["LOW","LOW","HIGH","HIGH"] <- 0.1
forage_pressure_index_cpt["MED","LOW","HIGH","HIGH"] <- 0.7
forage_pressure_index_cpt["HIGH","LOW","HIGH","HIGH"] <- 0.2

forage_pressure_index_cpt["LOW","MED","HIGH","HIGH"] <- 0.1
forage_pressure_index_cpt["MED","MED","HIGH","HIGH"] <- 0.2
forage_pressure_index_cpt["HIGH","MED","HIGH","HIGH"] <- 0.7

forage_pressure_index_cpt["LOW","HIGH","HIGH","HIGH"] <- 0.2
forage_pressure_index_cpt["MED","HIGH","HIGH","HIGH"] <- 0.3
forage_pressure_index_cpt["HIGH","HIGH","HIGH","HIGH"] <- 0.5

forage_pressure_index_cpt

# Convert the CPT to a data frame for plotting
forage_pressure_index_df <- as.data.frame(as.table(forage_pressure_index_cpt))

# Rename the columns for clarity
colnames(forage_pressure_index_df) <- c("forage_pressure_index","alt_forage_quality_200m", "woodland_LF_area_200m","wood_edge_area_200m","probability")

#subset by wood_edge_area_200m levels

edges_low <- subset(forage_pressure_index_df, wood_edge_area_200m %in% c("LOW"))
edges_med <- subset(forage_pressure_index_df, wood_edge_area_200m %in% c("MED"))
edges_high <- subset(forage_pressure_index_df, wood_edge_area_200m %in% c("HIGH"))

p_edges_low <- ggplot(edges_low, aes(x = forage_pressure_index, y = probability, group = alt_forage_quality_200m)) +
  geom_line(size = 1) +  # Line plot for probability distribution
  geom_point(size = 2) +  # Points on the curves
  facet_grid(rows = vars(alt_forage_quality_200m), 
             cols = vars(woodland_LF_area_200m), 
             labeller = label_both) +  # Facet by both alt_forage_quality_200m and wood_edge_area_200m in rows
  theme_bw() +
  labs(
    x = "Forage Pressure Index",
    y = "Probability"
  )

p_edges_med <- ggplot(edges_med, aes(x = forage_pressure_index, y = probability, group = alt_forage_quality_200m)) +
  geom_line(size = 1) +  # Line plot for probability distribution
  geom_point(size = 2) +  # Points on the curves
  facet_grid(rows = vars(alt_forage_quality_200m), 
             cols = vars(woodland_LF_area_200m), 
             labeller = label_both) +  # Facet by both alt_forage_quality_200m and wood_edge_area_200m in rows
  theme_bw() +
  labs(
    x = "Forage Pressure Index",
    y = "Probability"
  )

p_edges_high <- ggplot(edges_high, aes(x = forage_pressure_index, y = probability, group = alt_forage_quality_200m)) +
  geom_line(size = 1) +  # Line plot for probability distribution
  geom_point(size = 2) +  # Points on the curves
  facet_grid(rows = vars(alt_forage_quality_200m), 
             cols = vars(woodland_LF_area_200m), 
             labeller = label_both) +  # Facet by both alt_forage_quality_200m and wood_edge_area_200m in rows
  theme_bw() +
  labs(
    x = "Forage Pressure Index",
    y = "Probability"
  )

library(gridExtra)

grid.arrange(p_edges_low,p_edges_med,p_edges_high)


#CPT for thermoregulation index ----------------------------------------####

thermoreg_index_cpt <- array(
  0,  # Default probability for each cell
  dim = c(3, 3),  # Shape of the array (3x3x3)
  dimnames = list(
    thermoreg_index = c("LOW", "MED", "HIGH"),
    sum_dams_200m = c("LOW", "MED", "HIGH")
  )
)

#order = thermoreg_index, sum_dams_200m

#table1: 
thermoreg_index_cpt["LOW","LOW"] <- 0.6
thermoreg_index_cpt["MED","LOW"] <- 0.3
thermoreg_index_cpt["HIGH","LOW"] <- 0.1

#table2:
thermoreg_index_cpt["LOW","MED"] <- 0.2
thermoreg_index_cpt["MED","MED"] <- 0.6
thermoreg_index_cpt["HIGH","MED"] <- 0.2

#table3:
thermoreg_index_cpt["LOW","HIGH"] <- 0.1
thermoreg_index_cpt["MED","HIGH"] <- 0.3
thermoreg_index_cpt["HIGH","HIGH"] <- 0.6

thermoreg_index_cpt

# Convert the CPT to a data frame for plotting
thermoreg_index_df <- as.data.frame(as.table(thermoreg_index_cpt))

# Rename the columns for clarity
colnames(thermoreg_index_df) <- c("thermoreg_index", "sum_dams_200m", "probability")

# Create the plot
p <- ggplot(thermoreg_index_df, aes(x = thermoreg_index, y = probability, group = sum_dams_200m, color = sum_dams_200m)) +
  geom_line(size = 1) +  # Line plot for probability distribution
  geom_point(size = 2) +  # Points on the curves
  facet_wrap(~ sum_dams_200m, ncol = 3) +  # Create a panel of plots for each sum_dams_200m
  labs(title = "Probability Distributions of Thermoreg Index",
       x = "Thermoreg Index",
       y = "Probability") +
  theme_minimal()

# Display the plot
print(p)

#Final CPT: deer damage risk -----------------------------------------------####

damage_risk_cpt <- array(
  0,  # Default probability for each cell
  dim = c(5, 3, 3, 3),  # Shape of the array
  dimnames = list(
    damage_risk = c("LOW","LOW-MED","MED","MED-HIGH","HIGH"),
    forage_pressure_index = c("LOW", "MED", "HIGH"),
    connectivity_index = c("LOW", "MED", "HIGH"),
    thermoreg_index = c("LOW", "MED", "HIGH")
  )
)

#order = damage_risk, forage_pressure_index, connectivity_index, thermoreg_index

damage_risk_cpt["LOW", "LOW", "LOW", "LOW"] <- 0.8
damage_risk_cpt["LOW-MED", "LOW", "LOW", "LOW"] <- 0.1
damage_risk_cpt["MED", "LOW", "LOW", "LOW"] <- 0.1
damage_risk_cpt["MED-HIGH", "LOW", "LOW", "LOW"] <- 0
damage_risk_cpt["HIGH", "LOW", "LOW", "LOW"] <- 0

damage_risk_cpt["LOW", "MED", "LOW", "LOW"] <- 0.2
damage_risk_cpt["LOW-MED", "MED", "LOW", "LOW"] <- 0.7
damage_risk_cpt["MED", "MED", "LOW", "LOW"] <- 0.1
damage_risk_cpt["MED-HIGH", "MED", "LOW", "LOW"] <- 0
damage_risk_cpt["HIGH", "MED", "LOW", "LOW"] <- 0

damage_risk_cpt["LOW", "HIGH", "LOW", "LOW"] <- 0
damage_risk_cpt["LOW-MED", "HIGH", "LOW", "LOW"] <- 0
damage_risk_cpt["MED", "HIGH", "LOW", "LOW"] <- 0.2
damage_risk_cpt["MED-HIGH", "HIGH", "LOW", "LOW"] <- 0.7
damage_risk_cpt["HIGH", "HIGH", "LOW", "LOW"] <- 0.1

damage_risk_cpt["LOW", "LOW", "LOW", "MED"] <- 0.9
damage_risk_cpt["LOW-MED", "LOW", "LOW", "MED"] <- 0.1
damage_risk_cpt["MED", "LOW", "LOW", "MED"] <- 0
damage_risk_cpt["MED-HIGH", "LOW", "LOW", "MED"] <- 0
damage_risk_cpt["HIGH", "LOW", "LOW", "MED"] <- 0

damage_risk_cpt["LOW", "MED", "LOW", "MED"] <- 0
damage_risk_cpt["LOW-MED", "MED", "LOW", "MED"] <- 0.2
damage_risk_cpt["MED", "MED", "LOW", "MED"] <- 0.6
damage_risk_cpt["MED-HIGH", "MED", "LOW", "MED"] <- 0.2
damage_risk_cpt["HIGH", "MED", "LOW", "MED"] <- 0

damage_risk_cpt["LOW", "HIGH", "LOW", "MED"] <- 0
damage_risk_cpt["LOW-MED", "HIGH", "LOW", "MED"] <- 0
damage_risk_cpt["MED", "HIGH", "LOW", "MED"] <- 0.2
damage_risk_cpt["MED-HIGH", "HIGH", "LOW", "MED"] <- 0.8
damage_risk_cpt["HIGH", "HIGH", "LOW", "MED"] <- 0.2

damage_risk_cpt["LOW", "LOW", "LOW", "HIGH"] <- 0.8
damage_risk_cpt["LOW-MED", "LOW", "LOW", "HIGH"] <- 0.2
damage_risk_cpt["MED", "LOW", "LOW", "HIGH"] <- 0
damage_risk_cpt["MED-HIGH", "LOW", "LOW", "HIGH"] <- 0
damage_risk_cpt["HIGH", "LOW", "LOW", "HIGH"] <- 0

damage_risk_cpt["LOW", "MED", "LOW", "HIGH"] <- 0
damage_risk_cpt["LOW-MED", "MED", "LOW", "HIGH"] <- 0.2
damage_risk_cpt["MED", "MED", "LOW", "HIGH"] <- 0.2
damage_risk_cpt["MED-HIGH", "MED", "LOW", "HIGH"] <- 0.6
damage_risk_cpt["HIGH", "MED", "LOW", "HIGH"] <- 1

damage_risk_cpt["LOW", "HIGH", "LOW", "HIGH"] <- 0
damage_risk_cpt["LOW-MED", "HIGH", "LOW", "HIGH"] <- 0
damage_risk_cpt["MED", "HIGH", "LOW", "HIGH"] <- 0.1
damage_risk_cpt["MED-HIGH", "HIGH", "LOW", "HIGH"] <- 0.8
damage_risk_cpt["HIGH", "HIGH", "LOW", "HIGH"] <- 0.1

damage_risk_cpt["LOW", "LOW", "MED", "LOW"] <- 0.2
damage_risk_cpt["LOW-MED", "LOW", "MED", "LOW"] <- 0.7
damage_risk_cpt["MED", "LOW", "MED", "LOW"] <- 0.1
damage_risk_cpt["MED-HIGH", "LOW", "MED", "LOW"] <- 0
damage_risk_cpt["HIGH", "LOW", "MED", "LOW"] <- 0

damage_risk_cpt["LOW", "MED", "MED", "LOW"] <- 1
damage_risk_cpt["LOW-MED", "MED", "MED", "LOW"] <- 0
damage_risk_cpt["MED", "MED", "MED", "LOW"] <- 0
damage_risk_cpt["MED-HIGH", "MED", "MED", "LOW"] <- 0
damage_risk_cpt["HIGH", "MED", "MED", "LOW"] <- 0

damage_risk_cpt["LOW", "HIGH", "MED", "LOW"] <- 0
damage_risk_cpt["LOW-MED", "HIGH", "MED", "LOW"] <- 0.1
damage_risk_cpt["MED", "HIGH", "MED", "LOW"] <- 0.8
damage_risk_cpt["MED-HIGH", "HIGH", "MED", "LOW"] <- 0.1
damage_risk_cpt["HIGH", "HIGH", "MED", "LOW"] <- 1

damage_risk_cpt["LOW", "LOW", "MED", "MED"] <- 0.2
damage_risk_cpt["LOW-MED", "LOW", "MED", "MED"] <- 0.8
damage_risk_cpt["MED", "LOW", "MED", "MED"] <- 0.2
damage_risk_cpt["MED-HIGH", "LOW", "MED", "MED"] <- 0
damage_risk_cpt["HIGH", "LOW", "MED", "MED"] <- 0

damage_risk_cpt["LOW", "MED", "MED", "MED"] <- 0
damage_risk_cpt["LOW-MED", "MED", "MED", "MED"] <- 0
damage_risk_cpt["MED", "MED", "MED", "MED"] <- 0.9
damage_risk_cpt["MED-HIGH", "MED", "MED", "MED"] <- 0.1
damage_risk_cpt["HIGH", "MED", "MED", "MED"] <- 0

damage_risk_cpt["LOW", "HIGH", "MED", "MED"] <- 0
damage_risk_cpt["LOW-MED", "HIGH", "MED", "MED"] <- 0
damage_risk_cpt["MED", "HIGH", "MED", "MED"] <- 0
damage_risk_cpt["MED-HIGH", "HIGH", "MED", "MED"] <- 0.8
damage_risk_cpt["HIGH", "HIGH", "MED", "MED"] <- 0.2

damage_risk_cpt["LOW", "LOW", "MED", "HIGH"] <- 0
damage_risk_cpt["LOW-MED", "LOW", "MED", "HIGH"] <- 0.9
damage_risk_cpt["MED", "LOW", "MED", "HIGH"] <- 0.1
damage_risk_cpt["MED-HIGH", "LOW", "MED", "HIGH"] <- 0
damage_risk_cpt["HIGH", "LOW", "MED", "HIGH"] <- 0

damage_risk_cpt["LOW", "MED", "MED", "HIGH"] <- 0
damage_risk_cpt["LOW-MED", "MED", "MED", "HIGH"] <- 0
damage_risk_cpt["MED", "MED", "MED", "HIGH"] <- 0.1
damage_risk_cpt["MED-HIGH", "MED", "MED", "HIGH"] <- 0.8
damage_risk_cpt["HIGH", "MED", "MED", "HIGH"] <- 0.1

damage_risk_cpt["LOW", "HIGH", "MED", "HIGH"] <- 0
damage_risk_cpt["LOW-MED", "HIGH", "MED", "HIGH"] <- 0
damage_risk_cpt["MED", "HIGH", "MED", "HIGH"] <- 0
damage_risk_cpt["MED-HIGH", "HIGH", "MED", "HIGH"] <- 0.1
damage_risk_cpt["HIGH", "HIGH", "MED", "HIGH"] <- 0.9

damage_risk_cpt["LOW", "LOW", "HIGH", "LOW"] <- 0
damage_risk_cpt["LOW-MED", "LOW", "HIGH", "LOW"] <- 0
damage_risk_cpt["MED", "LOW", "HIGH", "LOW"] <- 0.2
damage_risk_cpt["MED-HIGH", "LOW", "HIGH", "LOW"] <- 0.7
damage_risk_cpt["HIGH", "LOW", "HIGH", "LOW"] <- 0.1

damage_risk_cpt["LOW", "MED", "HIGH", "LOW"] <- 0
damage_risk_cpt["LOW-MED", "MED", "HIGH", "LOW"] <- 0.1
damage_risk_cpt["MED", "MED", "HIGH", "LOW"] <- 0.7
damage_risk_cpt["MED-HIGH", "MED", "HIGH", "LOW"] <- 0.2
damage_risk_cpt["HIGH", "MED", "HIGH", "LOW"] <- 0

damage_risk_cpt["LOW", "HIGH", "HIGH", "LOW"] <- 0
damage_risk_cpt["LOW-MED", "HIGH", "HIGH", "LOW"] <- 0
damage_risk_cpt["MED", "HIGH", "HIGH", "LOW"] <- 0.1
damage_risk_cpt["MED-HIGH", "HIGH", "HIGH", "LOW"] <- 0.8
damage_risk_cpt["HIGH", "HIGH", "HIGH", "LOW"] <- 0.1

damage_risk_cpt["LOW", "LOW", "HIGH", "MED"] <- 0
damage_risk_cpt["LOW-MED", "LOW", "HIGH", "MED"] <- 0.2
damage_risk_cpt["MED", "LOW", "HIGH", "MED"] <- 0.6
damage_risk_cpt["MED-HIGH", "LOW", "HIGH", "MED"] <- 0.2
damage_risk_cpt["HIGH", "LOW", "HIGH", "MED"] <- 0

damage_risk_cpt["LOW", "MED", "HIGH", "MED"] <- 0
damage_risk_cpt["LOW-MED", "MED", "HIGH", "MED"] <- 0
damage_risk_cpt["MED", "MED", "HIGH", "MED"] <- 0.2
damage_risk_cpt["MED-HIGH", "MED", "HIGH", "MED"] <- 0.6
damage_risk_cpt["HIGH", "MED", "HIGH", "MED"] <- 0.2

damage_risk_cpt["LOW", "HIGH", "HIGH", "MED"] <- 0
damage_risk_cpt["LOW-MED", "HIGH", "HIGH", "MED"] <- 0
damage_risk_cpt["MED", "HIGH", "HIGH", "MED"] <- 0
damage_risk_cpt["MED-HIGH", "HIGH", "HIGH", "MED"] <- 0.1
damage_risk_cpt["HIGH", "HIGH", "HIGH", "MED"] <- 0.9

damage_risk_cpt["LOW", "LOW", "HIGH", "HIGH"] <- 0
damage_risk_cpt["LOW-MED", "LOW", "HIGH", "HIGH"] <- 0.2
damage_risk_cpt["MED", "LOW", "HIGH", "HIGH"] <- 0.6
damage_risk_cpt["MED-HIGH", "LOW", "HIGH", "HIGH"] <- 0.2
damage_risk_cpt["HIGH", "LOW", "HIGH", "HIGH"] <- 0

damage_risk_cpt["LOW", "MED", "HIGH", "HIGH"] <- 0
damage_risk_cpt["LOW-MED", "MED", "HIGH", "HIGH"] <- 0
damage_risk_cpt["MED", "MED", "HIGH", "HIGH"] <- 0.1
damage_risk_cpt["MED-HIGH", "MED", "HIGH", "HIGH"] <- 0.8
damage_risk_cpt["HIGH", "MED", "HIGH", "HIGH"] <- 0.1

damage_risk_cpt["LOW", "HIGH", "HIGH", "HIGH"] <- 0
damage_risk_cpt["LOW-MED", "HIGH", "HIGH", "HIGH"] <- 0
damage_risk_cpt["MED", "HIGH", "HIGH", "HIGH"] <- 0
damage_risk_cpt["MED-HIGH", "HIGH", "HIGH", "HIGH"] <- 0.1
damage_risk_cpt["HIGH", "HIGH", "HIGH", "HIGH"] <- 0.9


damage_risk_cpt

# Convert the CPT to a data frame for plotting
damage_risk_df <- as.data.frame(as.table(damage_risk_cpt))

# Rename the columns for clarity
colnames(damage_risk_df)[5] <- "probability"

#subset by thermoreg_index levels

thermoreg_low <- subset(damage_risk_df, thermoreg_index %in% c("LOW"))
thermoreg_med <- subset(damage_risk_df, thermoreg_index %in% c("MED"))
thermoreg_high <- subset(damage_risk_df, thermoreg_index %in% c("HIGH"))

p_thermoreg_low <- ggplot(thermoreg_low, aes(x = damage_risk, y = probability, group = forage_pressure_index)) +
  geom_line(size = 1) +  # Line plot for probability distribution
  geom_point(size = 2) +  # Points on the curves
  facet_grid(rows = vars(forage_pressure_index), 
             cols = vars(connectivity_index), 
             labeller = label_both) +  # Facet by both forage_pressure_index and thermoreg_index in rows
  theme_bw() +
  labs(
    x = "Damage risk",
    y = "Probability"
  )

p_thermoreg_med <- ggplot(thermoreg_med, aes(x = damage_risk, y = probability, group = forage_pressure_index)) +
  geom_line(size = 1) +  # Line plot for probability distribution
  geom_point(size = 2) +  # Points on the curves
  facet_grid(rows = vars(forage_pressure_index), 
             cols = vars(connectivity_index), 
             labeller = label_both) +  # Facet by both forage_pressure_index and thermoreg_index in rows
  theme_bw() +
  labs(
    x = "Damage risk",
    y = "Probability"
  )

p_thermoreg_high <- ggplot(thermoreg_high, aes(x = damage_risk, y = probability, group = forage_pressure_index)) +
  geom_line(size = 1) +  # Line plot for probability distribution
  geom_point(size = 2) +  # Points on the curves
  facet_grid(rows = vars(forage_pressure_index), 
             cols = vars(connectivity_index), 
             labeller = label_both) +  # Facet by both forage_pressure_index and thermoreg_index in rows
  theme_bw() +
  labs(
    x = "Damage risk",
    y = "Probability"
  )

library(gridExtra)

grid.arrange(p_thermoreg_low,p_thermoreg_med,p_thermoreg_high)


#Create BBN structure#------------------------------------

# Step 1: Explicitly define the nodes in the network
nodes <- c(
  "wood_connectivity_200m",
  "lf_length_200m",
  "alt_forage_qual_200m",
  "wood_lf_area_200m",
  "wood_edge_area_200m",
  "sum_dams_200m",
  "forage_pressure_index",
  "thermoreg_index",
  "connectivity_index",
  "damage_risk"
)

#empty graph
library(bnlearn)
e = empty.graph(nodes)

arc.set = matrix(c("wood_connectivity_200m", "connectivity_index",
                   "lf_length_200m", "connectivity_index", 
                   "alt_forage_qual_200m", "forage_pressure_index",
                   "wood_lf_area_200m","forage_pressure_index",
                   "wood_edge_area_200m","forage_pressure_index",
                   "sum_dams_200m","thermoreg_index",
                   "thermoreg_index","damage_risk",
                   "connectivity_index","damage_risk",
                   "forage_pressure_index","damage_risk"),
                 ncol = 2, byrow = TRUE,
                 dimnames = list(NULL, c("from", "to")))

arcs(e) <- arc.set

model_string <- modelstring(e) 

net<-model2network(model_string) 
# You dont need to repeat the names of the nodes with parents (child nodes) 

# Custom fitting network (matching up the nodes to their CPTs)
dfit = custom.fit(net, dist = list(wood_connectivity_200m = wood_connect_cpt, 
                                   lf_length_200m = lf_cpt,
                                   alt_forage_qual_200m = alt_forage_cpt,
                                   wood_lf_area_200m = wood_lf_cpt,
                                   wood_edge_area_200m = edge_cpt,
                                   sum_dams_200m = dams_cpt,
                                   connectivity_index =connect_index_cpt,
                                   thermoreg_index =thermoreg_index_cpt,
                                   forage_pressure_index =forage_pressure_index_cpt,
                                   damage_risk=damage_risk_cpt))

#Plot BN structure ####
graphviz.plot(net)

#Check parameters ####
dfit

#Predict latent indices for BBN (Thermoreg Index, Connectivity Index and Foraging pressure index) ####--------------------------------

#TO BE EDITED:

#wood_connectivity_200m
#lf_length_200m
#alt_forage_qual_200m
#wood_lf_area_200m
#wood_edge_area_200m
#sum_dams_200m
#connectivity_index
#thermoreg_index
#forage_pressure_index
#damage_risk

predict_dat <- df_cat %>%st_drop_geometry() %>% #drop geometry
  mutate(across(where(is.character), toupper)) %>% #convert characters to upper case
  dplyr::select(-c("pixel_ID","x","y"))
#Ensure variable names match those in BBN
predict_dat<-predict_dat%>%rename(
  wood_edge_area_200m=Focal200_EDGE_AREA,
  lf_length_200m=Focal200_LF_AREA,
  wood_lf_area_200m=Focal_200_WOOD_LF_AREA_SUM,
  alt_forage_qual_200m=Focal200_FORAGE_QUAL,
  sum_dams_200m=Focal200_SUM_DAMS,
  wood_connectivity_200m=connect_raster)

#Make empty columns for the latent (unobserved) variables

predict_dat$forage_pressure_index <- NA
predict_dat$connectivity_index <- NA
predict_dat$thermoreg_index <- NA
predict_dat$damage_risk <- NA

#Ensure all columns are factors, not characters
predict_dat <- predict_dat %>% mutate_all(as.factor)

#Ensure factor levels are in correct order
levels(predict_dat$forage_pressure_index)<-c("LOW", "MED", "HIGH")
levels(predict_dat$connectivity_index)<-c("LOW", "MED", "HIGH")
levels(predict_dat$wood_connectivity_200m)<-c("LOW", "MED", "HIGH")
levels(predict_dat$alt_forage_qual_200m)<-c("LOW", "MED", "HIGH")
levels(predict_dat$lf_length_200m)<-c("LOW", "MED", "HIGH")
levels(predict_dat$sum_dams_200m)<-c("LOW", "MED", "HIGH")
levels(predict_dat$wood_edge_area_200m)<-c("LOW", "MED", "HIGH")
levels(predict_dat$thermoreg_index)<-c("LOW", "MED", "HIGH")
levels(predict_dat$wood_lf_area_200m)<-c("LOW", "MED", "HIGH")
levels(predict_dat$damage_risk)<-c("LOW","LOW-MED","MED","MED-HIGH","HIGH")

#Ensure predict_data is a data.frame
predict_dat <- as.data.frame(predict_dat)

#Predict values for latent variables
pred_thermoreg = predict(object=dfit,node="thermoreg_index",data=predict_dat, method = "exact")
pred_fpi = predict(dfit,node="forage_pressure_index",data=predict_dat, method = "exact")
pred_connect = predict(dfit,node="connectivity_index",data=predict_dat, method = "exact")

#fill the columns
predict_dat$thermoreg_index <- pred_thermoreg
predict_dat$forage_pressure_index <- pred_fpi
predict_dat$connectivity_index <- pred_connect


#Predict damage
pred_damage = predict(dfit,node = "damage_risk",data = predict_dat,method = "exact")
predict_dat$damage_risk <- pred_damage

#Add damage risk back into spatial dataset

df_cat$pred_damage <- pred_damage
df_cat$pred_thermoreg <- pred_thermoreg
df_cat$pred_forage_pressure_index <- pred_fpi
df_cat$pred_connect <- pred_connect

#plot maps #------------------------------

# Make sf object
df_sf <- st_as_sf(df_cat, coords = c("x", "y"), crs = st_crs(bng))
df_sf <- st_transform(df_sf, crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

# Define the value mapping for pred_damage
damage_values <- c("LOW" = 1, "LOW-MED" = 2, "MED" = 3, "MED-HIGH" = 4, "HIGH" = 5)
df_sf$value <- damage_values[df_sf$pred_damage]

# Set up the colors
val <- 1:5
pal <- c("yellow", "#FED976", "#FD8D3C", "#FC4E2A", "#E31A1C")

#Convert to raster
r <- df_sf %>% dplyr::select(geometry, value) %>% stars::st_rasterize()

# Convert the raster to a data frame for ggplot
r_df <- as.data.frame(r, xy = TRUE)

# Create a factor for the value with all levels
r_df$value <- factor(r_df$value, levels = val, labels = names(damage_values))

# Add missing levels to the data
missing_levels <- data.frame(x = NA, y = NA, value = factor(val, labels = names(damage_values)))
r_df <- rbind(r_df, missing_levels)

# Plot raster using the color palette with custom labels
ggplot(r_df) +
  geom_raster(aes(x = x, y = y, fill = value)) +
  scale_fill_manual(values = pal, 
                    breaks = names(damage_values),
                    labels = names(damage_values),
                    na.value = "transparent") +
  theme_minimal() +
  labs(title = "Predicted Damage Raster",
       fill = "Damage Level") +
  guides(fill = guide_legend(override.aes = list(alpha = 1))) # Ensure all levels are shown in the legend

# Define the color palette function for leaflet
color_pal <- colorNumeric(palette = pal, domain = val, na.color = "transparent")

# Convert the stars object to a RasterLayer
raster_layer <- as(r, "Raster")

#Round all values to an integer
rounded_raster <- round(raster_layer)
rounded_raster <- raster::as.factor(rounded_raster)

#Project for leaflet map
rounded_raster <- projectRasterForLeaflet(rounded_raster, method="ngb")

# Create a color palette for the raster
color_fact <- colorFactor(
  palette = c("yellow", "#FED976", "#FD8D3C", "#FC4E2A", "#E31A1C"),
  domain = damage_values,
  na.color = "transparent"  # Set the color for NA values to transparent
  
)

# Create a reversed mapping for labels
damage_labels <- names(damage_values)
names(damage_labels) <- damage_values

# Create the Leaflet map
leaflet() %>%
  addProviderTiles(providers$Esri.WorldImagery) %>%
  addRasterImage(rounded_raster, colors = color_fact, opacity = 0.9, project = FALSE) %>%
  addLegend(
    pal = color_fact,
    values = damage_values,  # Use the correct domain of values
    title = "Deer impact risk",
    labFormat = labelFormat(
      transform = function(x) {
        # Use the label mapping to get the labels
        damage_labels[as.character(x)]
      }
    )
  )

#Crop final map to England-Wales shapefile to make the edges smooth (no jagged edges from the 10k tiles)