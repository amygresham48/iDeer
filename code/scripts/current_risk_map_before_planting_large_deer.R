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

Focal1000_EDGE_AREA <- raster(here("output/EW_datasets_2022/large_deer/sum_woodland_edges_1000m_large_deer_GB.tif"))

#Area of arable land within 1km

Focal1000_ARABLE_AREA <- raster(here("output/EW_datasets_2022/large_deer/sum_arable_1000_2022.tif"))

#Sum of quality of perennial fodder (without arable) within 1km

Focal1000_PEREN_FORAGEQ  <- raster(here("output/EW_datasets_2022/large_deer/sum_perennial_quality_1000m_large_deer_EW.tif"))

#Combined urban/suburban proximity

URBAN_PROX <- raster(here("output/EW_datasets_2022/large_deer/sum_dams_1000_non_wood_2022.tif"))

#Sum of DAMS within 1km

Focal1000_DAMS <- raster(here("output/EW_datasets_2022/large_deer/sum_dams_1000_non_wood_2022.tif"))

#Woodland area within 1km

Focal1000_WOOD <- raster(here("output/EW_datasets_2022/large_deer/sum_woodland_area_1km_large_deer_GB.tif"))

#Ensure all rasters have same extent and projection#------------------------------



#Get code from current_deer_impact_risk_EW_small_deer to extract raster values#-----------------------------------
#Create a dataframe containing all extracted raster values within user's landscape

#EXTRACT LENGTH OF WOODLAND EDGE WITHIN 1KM

edgelen <- extract_raster(map_reclass = Focal1000_EDGE_AREA,
                          lcm = nfi_lcm_map)

#ggplot(edgelen) +
#  geom_tile(aes(x = x, y = y, fill = Focal1000_EDGE_AREA)) +
#  scale_fill_viridis_c(option = "plasma") +  # Apply a continuous viridis color scale
#  theme_minimal() +
#  guides(fill = guide_colorbar(barwidth = 1, barheight = 10)) +  # Customize the colorbar appearance
#  labs(title = "Edge area within 1000m",
#       fill = "") 

#EXTRACT WOODLAND AREA WITHIN 1KM

woodpix <- extract_raster(map_reclass = Focal1000_WOOD,
                          lcm = nfi_lcm_map)

#ggplot(woodpix) +
#  geom_tile(aes(x = x, y = y, fill = Focal1000_WOOD)) +
#  scale_fill_viridis_c(option = "plasma") +  # Apply a continuous viridis color scale
#  theme_minimal() +
#  guides(fill = guide_colorbar(barwidth = 1, barheight = 10)) +  # Customize the colorbar appearance
#  labs(title = "Summed woodland area within 200m",
#       fill = "") 


#EXTRACT SUMMED DAMS WITHIN 1KM
damspix <- extract_raster(map_reclass = Focal1000_SUM_DAMS,
                          lcm = nfi_lcm_map)

#ggplot(damspix) +
#  geom_tile(aes(x = x, y = y, fill = Focal1000_SUM_DAMS)) +
#  scale_fill_viridis_c(option = "plasma") +  # Apply a continuous viridis color scale
#  theme_minimal() +
#  guides(fill = guide_colorbar(barwidth = 1, barheight = 10)) +  # Customize the colorbar appearance
#  labs(title = "Summed DAMS within 1000m",
#       fill = "") 

#EXTRACT PERENNIAL FORAGE QUALITY

foragepix <- extract_raster(map_reclass = Focal1000_PEREN_FORAGEQ,
                            lcm = nfi_lcm_map)

#ggplot(foragepix) +
#  geom_tile(aes(x = x, y = y, fill = Focal1000_PEREN_FORAGEQ)) +
#  scale_fill_viridis_c(option = "plasma") +  # Apply a continuous viridis color scale
#  theme_minimal() +
#  guides(fill = guide_colorbar(barwidth = 1, barheight = 10)) +  # Customize the colorbar appearance
#  labs(title = "Perennial and arable quality",
#       fill = "") 

#EXTRACT ARABLE AREA

arablepix <- extract_raster(map_reclass = Focal1000_ARABLE_AREA,
                          lcm = nfi_lcm_map)

#ggplot(arablepix) +
#  geom_tile(aes(x = x, y = y, fill = Focal1000_ARABLE_AREA)) +
#  scale_fill_viridis_c(option = "plasma") +  # Apply a continuous viridis color scale
#  theme_minimal() +
#  guides(fill = guide_colorbar(barwidth = 1, barheight = 10)) +  # Customize the colorbar appearance
#  labs(title = "Edge area within 1000m",
#       fill = "") 


#EXTRACT URBAN PROXIMIYY

urbanpix <- extract_raster(map_reclass = URBAN_PROX,
                          lcm = nfi_lcm_map)

#ggplot(urbanpix) +
#  geom_tile(aes(x = x, y = y, fill = URBAN_PROX)) +
#  scale_fill_viridis_c(option = "plasma") +  # Apply a continuous viridis color scale
#  theme_minimal() +
#  guides(fill = guide_colorbar(barwidth = 1, barheight = 10)) +  # Customize the colorbar appearance
#  labs(title = "Urban proximity",
#       fill = "") 


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

#For now, just use this example dataset

df_cat <- df

#Urban proximity
hist(df_cat$URBAN_PROX)
# Replace numeric values with cat labels
df_cat$URBAN_PROX <- ifelse(df_cat$URBAN_PROX<= 100, "HIGH",
                                  ifelse(df_cat$URBAN_PROX> 100 & df_cat$URBAN_PROX <= 500, "MED",
                                         ifelse(df_cat$URBAN_PROX> 500, "LOW", NA)))
unique(df_cat$URBAN_PROX)

#Summed woodland area

hist(df_cat$Focal1000_WOOD)
df_cat$Focal1000_WOOD <- ifelse(df_cat$Focal1000_WOOD <= 1500, "LOW",
                                            ifelse(df_cat$Focal1000_WOOD > 1500 & df_cat$Focal1000_WOOD <=3000, "MED",
                                                   ifelse(df_cat$Focal1000_WOOD > 3000, "HIGH", NA)))
unique(df_cat$Focal1000_WOOD)

#DAMS

hist(df_cat$Focal1000_SUM_DAMS)
df_cat$Focal1000_SUM_DAMS <- ifelse(df_cat$Focal1000_SUM_DAMS <= 1500, "LOW",
                                   ifelse(df_cat$Focal1000_SUM_DAMS > 1500 & df_cat$Focal1000_SUM_DAMS <=3000, "MED",
                                          ifelse(df_cat$Focal1000_SUM_DAMS > 3000, "HIGH", NA)))
unique(df_cat$Focal1000_SUM_DAMS)

#Perennial forage quality

hist(df_cat$Focal1000_PEREN_FORAGEQ)
df_cat$Focal1000_PEREN_FORAGEQ <- ifelse(df_cat$Focal1000_PEREN_FORAGEQ <= -50, "LOW",
                                      ifelse(df_cat$Focal1000_PEREN_FORAGEQ > -50 & df_cat$Focal1000_PEREN_FORAGEQ <=50, "MED",
                                             ifelse(df_cat$Focal1000_PEREN_FORAGEQ > 50, "HIGH", NA)))
unique(df_cat$Focal1000_PEREN_FORAGEQ)

#Arable forage

hist(df_cat$Focal1000_ARABLE_AREA)
df_cat$Focal1000_ARABLE_AREA <- ifelse(df_cat$Focal1000_ARABLE_AREA <= -50, "LOW",
                                         ifelse(df_cat$Focal1000_ARABLE_AREA > -50 & df_cat$Focal1000_ARABLE_AREA <=50, "MED",
                                                ifelse(df_cat$Focal1000_ARABLE_AREA > 50, "HIGH", NA)))

#Edge area

hist(df_cat$Focal1000_EDGE_AREA)
df_cat$Focal1000_EDGE_AREA <- ifelse(df_cat$Focal1000_EDGE_AREA <= 400, "LOW",
                                    ifelse(df_cat$Focal1000_EDGE_AREA > 400 & df_cat$Focal1000_EDGE_AREA <=800, "MED",
                                           ifelse(df_cat$Focal1000_EDGE_AREA > 800, "HIGH", NA)))
unique(df_cat$Focal1000_EDGE_AREA)

#------------------------------------

#Set up conditional probability tables for BBN

#CPTS for measured nodes ####

#woodland + linear feature area
wood_cpt<-matrix(c(0.333333,0.333333,0.333333),ncol = 3,dimnames = list(NULL, c("LOW", "MED","HIGH")))
#woodland edge area
edge_cpt <-matrix(c(0.333333,0.333333,0.333333),ncol = 3,dimnames = list(NULL, c("LOW", "MED","HIGH")))
#urban proximity
urban_cpt <-matrix(c(0.333333,0.333333,0.333333),ncol = 3,dimnames = list(NULL, c("LOW", "MED","HIGH")))
#dams
dams_cpt<-matrix(c(0.333333,0.333333,0.333333),ncol = 3,dimnames = list(NULL, c("LOW", "MED","HIGH")))
#perennial quality
peren_forage_cpt <- matrix(c(0.333333,0.333333,0.333333),ncol = 3,dimnames = list(NULL, c("LOW", "MED","HIGH")))
#Arable area
arable_cpt <- matrix(c(0.333333,0.333333,0.333333),ncol = 3,dimnames = list(NULL, c("LOW", "MED","HIGH")))

#Indices: Disturbance Index, Thermoregulation Index, Relative foraging pressure Index

#CPT for Disturbance Index ----------------------------------- ####

disturb_index_cpt <- array(
  0,  # Default probability for each cell
  dim = c(3, 3, 3, 3),  # Shape of the array (3x3x3)
  dimnames = list(
    disturb_index = c("LOW", "MED", "HIGH"),
    urban_prox = c("LOW","MED","HIGH")
 )
)
#order = disturb_index, urban_prox

#table1: urban_prox = LOW, connectivity = LOW
disturb_index_cpt["LOW","LOW"] <- 0.8
disturb_index_cpt["LOW","MED"] <- 0.15
disturb_index_cpt["LOW","HIGH"] <- 0.05
#table2:  urban_prox = MED, connectivity = LOW
disturb_index_cpt["MED","LOW"] <- 0.1
disturb_index_cpt["MED","MED"] <- 0.8
disturb_index_cpt["MED","HIGH"] <- 0.1
#table3: urban_prox = HIGH, connectivity = LOW
disturb_index_cpt["HIGH","LOW"] <- 0.05
disturb_index_cpt["HIGH","MED"] <- 0.15
disturb_index_cpt["HIGH","HIGH"] <- 0.8

disturb_index_cpt

# Convert the CPT to a data frame for plotting
disturb_index_df <- as.data.frame(as.table(disturb_index_cpt))

# Rename the columns for clarity
colnames(disturb_index_df) <- c("disturb_index", "urban_prox", "probability")

# Create the plot
p <- ggplot(thermoreg_index_df, aes(x = disturb_index, y = probability, group = urban_prox, col = urban_prox)) +
  geom_line(size = 1) +  # Line plot for probability distribution
  geom_point(size = 2) +  # Points on the curves
  facet_wrap(~ urban_prox, ncol = 3) +  # Create a panel of plots for each sum_dams_200m
  labs(title = "Probability Distributions of Thermoreg Index",
       x = "Thermoreg Index",
       y = "Probability") +
  theme_minimal()

# Display the plot
print(p)


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


#CPT for Foraging pressure index ------------------------------------------------####

forage_pressure_index_cpt <- array(
  0,  # Default probability for each cell
  dim = c(3, 3, 3, 3),  # Shape of the array (3x3x3)
  dimnames = list(
    forage_pressure_index = c("LOW", "MED", "HIGH"),
    arable_1000m = c("LOW", "MED", "HIGH"),
    perennial_1000m = c("LOW", "MED", "HIGH"),
    wood_edge_area_1000m = c("LOW", "MED", "HIGH")
  )
)

#order = forage_pressure_index, arable_1000m , perennial_1000m,wood_edge_area_1000m

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
colnames(forage_pressure_index_df) <- c("forage_pressure_index","arable_1000m", "perennial_1000m","wood_edge_area_1000m","probability")

#subset by wood_edge_area_1000m levels

edges_low <- subset(forage_pressure_index_df, wood_edge_area_1000m %in% c("LOW"))
edges_med <- subset(forage_pressure_index_df, wood_edge_area_1000m %in% c("MED"))
edges_high <- subset(forage_pressure_index_df, wood_edge_area_1000m %in% c("HIGH"))

p_edges_low <- ggplot(edges_low, aes(x = forage_pressure_index, y = probability, group = perennial_1000m)) +
  geom_line(size = 1) +  # Line plot for probability distribution
  geom_point(size = 2) +  # Points on the curves
  facet_grid(rows = vars(perennial_1000m), 
             cols = vars(arable_1000m), 
             labeller = label_both) +  # Facet by both perennial_1000m and wood_edge_area_1000m in rows
  theme_bw() +
  labs(
    x = "Forage Pressure Index",
    y = "Probability"
  )

p_edges_med <- ggplot(edges_med, aes(x = forage_pressure_index, y = probability, group = perennial_1000m)) +
  geom_line(size = 1) +  # Line plot for probability distribution
  geom_point(size = 2) +  # Points on the curves
  facet_grid(rows = vars(perennial_1000m), 
             cols = vars(arable_1000m), 
             labeller = label_both) +  # Facet by both perennial_1000m and wood_edge_area_1000m in rows
  theme_bw() +
  labs(
    x = "Forage Pressure Index",
    y = "Probability"
  )

p_edges_high <- ggplot(edges_high, aes(x = forage_pressure_index, y = probability, group = perennial_1000m)) +
  geom_line(size = 1) +  # Line plot for probability distribution
  geom_point(size = 2) +  # Points on the curves
  facet_grid(rows = vars(perennial_1000m), 
             cols = vars(arable_1000m), 
             labeller = label_both) +  # Facet by both perennial_1000m and wood_edge_area_1000m in rows
  theme_bw() +
  labs(
    x = "Forage Pressure Index",
    y = "Probability"
  )

library(gridExtra)

grid.arrange(p_edges_low,p_edges_med,p_edges_high)

#Final CPT: deer damage risk -----------------------------------------------####

damage_risk_cpt <- array(
  0,  # Default probability for each cell
  dim = c(5, 3, 3, 3),  # Shape of the array
  dimnames = list(
    damage_risk = c("LOW","LOW-MED","MED","MED-HIGH","HIGH"),
    forage_pressure_index = c("LOW", "MED", "HIGH"),
    disturb_index = c("LOW", "MED", "HIGH"),
    thermoreg_index = c("LOW", "MED", "HIGH")
  )
)

#order = damage_risk, forage_pressure_index, disturb_index, thermoreg_index

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
             cols = vars(disturb_index), 
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
             cols = vars(disturb_index), 
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
             cols = vars(disturb_index), 
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
  "wood_1000m",
  "arable_1000m",
  "perennial_1000m",
  "wood_edge_area_1000m",
  "sum_dams_1000m",
  "urban_prox",
  "forage_pressure_index",
  "thermoreg_index",
  "disturb_index",
  "damage_risk"
)

#empty graph
library(bnlearn)
e = empty.graph(nodes)

arc.set = matrix(c("wood_1000m", "forage_pressure_index",
                   "arable_1000m", "forage_pressure_index",
                   "perennial_1000m","forage_pressure_index",
                   "wood_edge_area_1000m","forage_pressure_index",
                   "sum_dams_1000m","thermoreg_index",
                   "urban_prox","disturb_index",
                   "thermoreg_index","damage_risk",
                   "disturb_index","damage_risk",
                   "forage_pressure_index","damage_risk"),
                 ncol = 2, byrow = TRUE,
                 dimnames = list(NULL, c("from", "to")))

arcs(e) <- arc.set

model_string <- modelstring(e) 

net<-model2network(model_string) 
# You dont need to repeat the names of the nodes with parents (child nodes) 

# Custom fitting network (matching up the nodes to their CPTs)
dfit = custom.fit(net, dist = list(perennial_1000m = lf_cpt,
                                   arable_1000m = arable_cpt,
                                   wood_1000m = wood_cpt,
                                   wood_edge_area_1000m = edge_cpt,
                                   sum_dams_1000m = dams_cpt,
                                   urban_prox = urban_prox_cpt,
                                   thermoreg_index =thermoreg_index_cpt,
                                   forage_pressure_index =forage_pressure_index_cpt,
                                   damage_risk=damage_risk_cpt))

#Plot BN structure ####
graphviz.plot(net)

#Check parameters ####
dfit

#Predict latent indices for BBN (Thermoreg Index, Disturb Index and Foraging pressure index) ####--------------------------------

predict_dat <- df_cat %>%st_drop_geometry() %>% #drop geometry
  mutate(across(where(is.character), toupper)) %>% #convert characters to upper case
  dplyr::select(-c("pixel_ID","x","y"))
#Ensure variable names match those in BBN
predict_dat<-predict_dat%>%rename(
  wood_edge_area_1000m=Focal1000_EDGE_AREA,
  urban_prox = URBAN_PROX,
  wood_1000m=Focal_1000_WOOD_AREA,
  arable_1000m=Focal1000_ARABLE_AREA,
  sum_dams_1000m=Focal1000_SUM_DAMS,
  perennial_1000 = Focal1000_PEREN_FORAGEQ)

#Make empty columns for the latent (unobserved) variables

predict_dat$forage_pressure_index <- NA
predict_dat$disturb_index <- NA
predict_dat$thermoreg_index <- NA
predict_dat$damage_risk <- NA

#Ensure all columns are factors, not characters
predict_dat <- predict_dat %>% mutate_all(as.factor)

#Ensure factor levels are in correct order
levels(predict_dat$urban_prox)<-c("HIGH", "MED", "LOW")
levels(predict_dat$arable_1000m)<-c("LOW", "MED", "HIGH")
levels(predict_dat$perennial_1000m)<-c("LOW", "MED", "HIGH")
levels(predict_dat$sum_dams_200m)<-c("LOW", "MED", "HIGH")
levels(predict_dat$wood_edge_area_1000m)<-c("LOW", "MED", "HIGH")
levels(predict_dat$thermoreg_index)<-c("LOW", "MED", "HIGH")

levels(predict_dat$forage_pressure_index)<-c("LOW", "MED", "HIGH")
levels(predict_dat$disturb_index)<-c("LOW", "MED", "HIGH")
levels(predict_dat$damage_risk)<-c("LOW","LOW-MED","MED","MED-HIGH","HIGH")

#Ensure predict_data is a data.frame
predict_dat <- as.data.frame(predict_dat)

#Predict values for latent variables
pred_thermoreg = predict(object=dfit,node="thermoreg_index",data=predict_dat, method = "exact")
pred_fpi = predict(dfit,node="forage_pressure_index",data=predict_dat, method = "exact")
pred_connect = predict(dfit,node="disturb_index",data=predict_dat, method = "exact")

#fill the columns
predict_dat$thermoreg_index <- pred_thermoreg
predict_dat$forage_pressure_index <- pred_fpi
predict_dat$disturb_index <- pred_connect

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
