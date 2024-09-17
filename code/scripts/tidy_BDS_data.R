library(here)
library(readr)
library(tidyr)
library(reshape2)
library(data.table)

#Import data
bds <- read_csv(here("data/clean-data/BDS-data/BDS distribution coordinates 2005_2010_2016_2022_tidied.csv"))

#Import uk10k tiles

#uk10k geometry
uk10k <-sf::st_read(dsn = "C:/Users/ik929086/OneDrive - University of Reading/Documents/Spatial datasets/os_bng_grids.gpkg", layer = "10km_grid") %>%
  st_transform(.,bng)
#GB shapefile
GB <- st_read("C:/Users/ik929086/OneDrive - University of Reading/Documents/Spatial datasets/GB shapefile/Countries_December_2022_GB_BFC_-8802398211591794926/CTRY_DEC_2022_GB_BFC.shp")
uk10k_GB <- sf::st_filter(uk10k, GB)

#Import northern Ireland tiles
ni10k <- sf::st_read("C:/Users/ik929086/OneDrive - University of Reading/Documents/Spatial datasets/OSNI_Open_Data_-_Coverage_Grid_-_10K.shp")
ni10k <- ni10k %>%
  rename(OS_Tile = NAME)

#Make UK 10k tile sf object ####

#Convert to bng

# Define the CRS for Irish Grid and British National Grid
# EPSG code for Irish Grid is 29902
# EPSG code for British National Grid is 27700
irish_crs <- 29902
bng <- 27700

# Transform the CRS from Irish Grid to British National Grid
ni10k_bng <- st_transform(ni10k, crs = bng)

#Get tile names and geometry for NI

ni10k_bng_cols <- ni10k_bng %>%
  select(c(OS_Tile, geometry)) %>%
  rename(tile_name = OS_Tile) %>%
  rename(geom = geometry)

#rbind to uk10k

ukni10k <- rbind(uk10k_GB, ni10k_bng_cols)

st_write(ukni10k, here("output/UK_NI_10K_tiles.shp"))

#Get centroid of tiles to get BNG coordinates
tiles_centroids <- st_centroid(ukni10k)
# Extract the X and Y coordinates of the centroids
centroid_coords <- st_coordinates(tiles_centroids)
# Combine the original data with the centroid coordinates (optional)
tiles_with_coords <- cbind(ukni10k, centroid_coords)
#Rename coord cols
tiles_with_coords <- tiles_with_coords %>%
  rename(X_Cent = X) %>%
  rename(Y_Cent = Y) %>%
  rename(OS_Tile = tile_name)

#Reshape the BDS data ####

bds <- as.data.table(bds)

#Extract unique years from columns

years <- unique(sub("X_Cent|Y_Cent", "", names(bds)[-(1)]))

# Create a mapping of indices to years
year_map <- setNames(years, seq_along(years))

# Correct melt function
reshaped_data <- melt(bds, 
                      id.vars = "Species", 
                      measure.vars = patterns("X_Cent", "Y_Cent"),
                      variable.name = "Year", 
                      value.name = c("X_Cent", "Y_Cent"))

# Replace the numeric Year values with actual year names
reshaped_data[, Year := year_map[as.integer(Year)]]

#Add the tile geometry into the dataset
bds_tiles <- left_join(reshaped_data, tile_coords, by = c("X_Cent","Y_Cent"))
bds_tiles <- left_join(bds_tiles, uk10k, by = c("OS_Tile" = "tile_name"))

bds_tiles <- bds_tiles %>%
  select(-c("Country","Grid_Type","FID"))

#Add col to bds_tiles where Presence = 1

bds_tiles$Presence = 1

#Find tiles which are absent from BDS dataset
missing_tiles <- tiles_with_coords %>%
  anti_join(bds_tiles, by = "OS_Tile")

#Create a dataframe with all combinations of species, years, and tiles
species_list <- unique(bds_tiles$Species)
years_list <- unique(bds_tiles$Year)
tiles_list <- unique(ukni10k$tile_name)

# Create a dataframe of all combinations of species and years
combinations <- expand.grid(
  Species = species_list,
  Year = years_list,
  OS_Tile = missing_tiles$OS_Tile,
  Presence = NA,
  stringsAsFactors = FALSE
)

# Merge combinations with missing_tiles data to include coordinates and geometries
full_missing_tiles <- combinations %>%
  left_join(missing_tiles, by = "OS_Tile")

# Combine with the original bds_tiles
full_bds_df <- bind_rows(bds_tiles, full_missing_tiles)
full_bds_df <- st_as_sf(full_bds_df)

#Remove empty rows

full_bds_df <-full_bds_df[!st_is_empty(full_bds_df), ]

#Northern Ireland is buggered but the rest works fine...
#Conversion from Irish crs to BNG hasn't worked properly

# Plot data by species
ggplot(data = full_bds_df) +
  geom_sf(aes(fill = factor(Presence)), color = "black", size = 0.1) +
  scale_fill_manual(values = c("1" = "red", "NA" = "white"), 
                    na.value = "white", 
                    name = "Presence",
                    labels = c("Present", "Not Present")) +
  facet_wrap(~ Species) +
  labs(title = "Deer Species Presence",
       fill = "Presence") +
  theme_minimal()

#Create a raster for each species ####

# Define the resolution of the raster (10 km grid)
resolution <- 10000  # 10 km resolution in meters

# Create an empty raster template with the desired extent and resolution
# Adjust the extent to match the extent of your data
extent_template <- extent(full_bds_df)  # Adjust according to your actual data
template_raster <- raster(extent_template, res = resolution, crs = st_crs(full_bds_df)$proj4string)

# Define the species list
deer_species <- c("Roe", "Red", "Fallow", "Muntjac", "Sika", "CWD")

# Create an empty list to store the rasters for each species
species_rasters <- list()

# Loop through each species and apply the rasterization process
for (species in deer_species) {
  
  # Subset by species and replace NAs with 0
  species_subset <- subset(full_bds_df, Species %in% c(species)) %>%
    st_as_sf() #%>%
    #mutate(Presence = ifelse(is.na(Presence), 0, Presence))
  
  # Remove empty geometries
  species_subset <- species_subset[!st_is_empty(species_subset), ]
  
  # Rasterize the subset based on the 'Presence' field
  species_raster <- rasterize(species_subset, 
                              template_raster, 
                              field = "Presence", 
                              fun = "max")  # Use 'max' or another function based on your needs
  
  # Store the raster in the list, with the species name as the key
  species_rasters[[species]] <- species_raster
}

# Set up plotting window for 6 plots (2 rows, 3 columns)
par(mfrow = c(2, 3))

# Loop through each species raster and plot
for (species in names(species_rasters)) {
  plot(species_rasters[[species]], 
       main = paste(species, "Presence"), 
       col = terrain.colors(10))  # You can change color scheme if needed
}

# Loop through each species raster and save
for (species in names(species_rasters)) {
  # Define the filename based on species name
  filename <- here("output", paste0(species, "_presence_raster_GB_2005_to_2022_BDS.tif"))
  
  # Crop the raster to the shapefile extent
  cropped_raster <- crop(species_rasters[[species]], 
                         extent(GB))
  
  # Mask the cropped raster to the shapefile
  masked_raster <- mask(cropped_raster, GB)
  
  # Save the raster to the specified file
  writeRaster(species_rasters[[species]], 
              filename, 
              format = "GTiff", 
              overwrite = TRUE)
  
  # Optional: Print a message indicating success
  message(paste("Saved raster for species:", species, "to", filename))
}

#Crop to England and Wales for iDeer tool
#Remove Scotland
EW <- GB[!grepl("Scotland", GB$CTRY22NM),]

# Loop through each species raster, crop, and save
for (species in names(species_rasters)) {
  
  # Define the filename for the cropped raster
  cropped_filename <- here("output", paste0(species, "_presence_raster_EW_2005_to_2022_BDS.tif"))
  
  # Crop the raster to the shapefile extent
  cropped_raster <- crop(species_rasters[[species]], 
                         extent(EW))
  
  # Mask the cropped raster to the shapefile
  masked_raster <- mask(cropped_raster, EW)
  
  # Save the cropped and masked raster
  writeRaster(masked_raster, 
              cropped_filename, 
              format = "GTiff", 
              overwrite = TRUE)
  
  # Optional: Print a message indicating success
  message(paste("Saved cropped raster for species:", species, "to", cropped_filename))
}

# Set up plotting window for 6 plots (2 rows, 3 columns)
par(mfrow = c(2, 3))

#save tidied csv
#Remove the Northern Ireland data for now
full_bds_df<- full_bds_df[!grepl(".tif", full_bds_df$OS_Tile),]
full_bds_df <- full_bds_df %>% st_drop_geometry()
write.csv(full_bds_df, here("output/tidied_BDS_deer_presence_2005_to_2022.csv"))
