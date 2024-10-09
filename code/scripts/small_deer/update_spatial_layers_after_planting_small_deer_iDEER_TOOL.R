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
library(leaflet)

#British National Grid
bng <- 27700

#Import required spatial layers #--------------

#Read in woodland polygons from ArcGIS pro:
wood_polys <- st_read(here("data/derived-data/NFILCM_2022_filtered_EW_arcgis.shp"))
st_crs(wood_polys) <- bng

hab_patches_all <- wood_polys %>%
  dplyr::select(-c(SHAPE_Leng, Shape_Area, Shape_Le_1)) %>%
  mutate(patch_ID = dplyr::row_number(),
         Shape_Area = st_area(geometry))%>%
  mutate(Shape_Area = as.numeric(Shape_Area))

#Filter by ew10k tiles

#Linear feature layer
#Dataset = CEH Woody Linear Feature Framework (2016)
#sf dataset
#Modified using script make_LF_raster.R to make a raster
#linear features in England and Wales, outside woodlands

lf <- raster(here("data/derived-data/WLF_EW_RASTER.tif"))
crs(lf) <- bng

#This layer does NOT include woodland edge type
nfi_lcm_map <- raster(here("output/nfi_lcm_2022_overlaid.tif"))
crs(nfi_lcm_map) <- bng
extent(nfi_lcm_map)

#DAMS
#Interpolated from 50m resolution to 25m resolution
#To match the resolution of other raster layers
#Interpolation method = bilinear
dams <- raster(here("data/raw-data/DAMS/dams_25m_bng.tif"))
crs(dams) <- bng

#Binary woodland map

NFI_LCM_woods_only <- raster(here("output/EW_datasets_2022/NFILCM_2022_binary_woodland_all_tiles_EW.tif"))
crs(NFI_LCM_woods_only) <- bng
#plot(NFI_LCM_woods_only)

#function to extract pixel values from raster layers
source(here("code/functions/extract_raster_pixels_func.R"))

#current risk raster
current.risk <- raster("C:/Users/ik929086/Documents/iDeer-tool/data/test.risk.map.tif")

#England-Wales 10k tiles
#EW <- st_read(here("data/derived-data/10k_tiles_EW.shp"))

#Add new woodland polygons to hab_patches_all #-----------------------

#To test out updating spatial layers with new polygons

# Function to create a random polygon centered in a region with a specified half-width
create_random_polygon <- function(easting_range, northing_range, half_width) {
  # Generate random centroid within the specified range
  centroid <- c(runif(1, min(easting_range), max(easting_range)),
                runif(1, min(northing_range), max(northing_range)))
  
  # Create a square polygon around the centroid with the given half-width
  coords <- matrix(c(-half_width, -half_width, 
                     half_width, -half_width, 
                     half_width, half_width, 
                     -half_width, half_width, 
                     -half_width, -half_width), 
                   ncol = 2, byrow = TRUE)
  
  # Shift the square coordinates by the centroid values
  shifted_coords <- coords + matrix(rep(centroid, each = 5), ncol = 2)
  
  # Create polygon
  polygon <- st_polygon(list(shifted_coords))
  
  # Convert to sf object and set CRS
  sf_polygon <- st_sfc(polygon, crs = bng)
  
  return(sf_polygon)
}

# Define easting and northing ranges for Leicestershire
easting_range <- c(440000, 460000)
northing_range <- c(295000, 315000)

# Create one random polygon 50m wide and another 80m wide
polygon1 <- create_random_polygon(easting_range, northing_range, half_width = 25)  # 50m wide
polygon2 <- create_random_polygon(easting_range, northing_range, half_width = 40)  # 80m wide

# Combine the two polygons into a single sfc object
polygons_sfc <- st_sfc(polygon1[[1]], polygon2[[1]])

# Convert the sfc object to an sf object
polygons_sf <- st_sf(geometry = polygons_sfc)
st_crs(polygons_sf) <- bng

# Plot the polygons
plot(st_geometry(polygons_sf), col = c("red", "blue"), main = "Random Polygons in Leicestershire (50m & 80m wide)")

# Combine new polygons with existing data
hab_patches_all_updated <- bind_rows(hab_patches_all, polygons_sf)

plot(polygons_sf$geometry)

#Assign polygons woodland type (broadleaved or coniferous)

new_polygons_sf_types <- polygons_sf %>%
  mutate(woodland_type = c("Mainly broadleaf","Mainly conifer"))
st_crs(new_polygons_sf_types) <- bng

#plot(EW$geometry)
#plot(new_polygons_sf_types$geometry,add=TRUE)

#Update layers #####-------------------------------------------------

#1. The land cover map with woodland edges #####-------------------------------------------------
#This function produces a modified land cover raster with woodland edges
#classified by modal land cover type
#also produces a binary woodland raster to be used for subsequent spatial layers

buffered_woods_5km <- st_as_sf(st_buffer(new_polygons_sf_types, dist = 5000))
st_crs(buffered_woods_5km) <- bng

buffered_woods_7km <- st_as_sf(st_buffer(new_polygons_sf_types, dist = 7000))
st_crs(buffered_woods_7km) <- bng

plot(buffered_woods_5km$geometry)
plot(new_polygons_sf_types$geometry,add=TRUE)

#Crop original rasters to 7km buffer (e.g. landscape size selected by user)
#e.g. 5km landscape + 2km buffer

lcm_cropped <- crop(nfi_lcm_map, buffered_woods_7km)

# Reclassify the woodland_type into numeric values
new_polygons_sf_types$woodland_type_num <- ifelse(new_polygons_sf_types$woodland_type == "Mainly broadleaf", 1, 
                                                    ifelse(new_polygons_sf_types$woodland_type == "Mainly conifer", 2, NA))

#Rasterize the new woodland polygon(s) 
#value = 1 if woodland_type = broadleaved
#value = 2 if woodland_type = coniferous

# Rasterize the polygons
raster_polys <- rasterize(new_polygons_sf_types, lcm_cropped, field = "woodland_type_num")
#set zero values to NA
values(raster_polys)[values(raster_polys) <= 0] = NA

# Overlay function: replace values in nfi_lcm_map with non-NA values from woodland_raster
nfi_lcm_map_updated <- overlay(lcm_cropped, raster_polys, fun = function(nfi, wood) {
  ifelse(!is.na(wood), wood, nfi)  # If woodland_raster has a non-NA value, use it; otherwise keep nfi_lcm_map value
})

plot(nfi_lcm_map_updated)

#2. Get woodland edges #####-------------------------------------------------

crs(nfi_lcm_map_updated) <- bng

  #Get wood boundaries in tile buffer
  # make binary edge raster. 1 if edge, 0 if not ####
  #Filter for woodland only, make everything else NA
  wood <- nfi_lcm_map_updated
  wood[wood[] >= 3] = NA
  boundaries_wood = boundaries(wood, type='inner') # edge raster
  #Make woodland boundaries = 1000
  #boundaries_wood <- boundaries*1000
  # need to make NAs 0
  boundaries_wood[is.na(boundaries_wood[])] <- 0 
  plot(boundaries_wood)

#-------------------------------------

#LENGTH OF WOODLAND EDGE WITHIN 200m ####

circle.buff = raster::focalWeight(boundaries_wood, d=200, type="circle",fillNA=T)#Create buffer
circle.buff[circle.buff > 0] <- 1   # replacing weights by 1
Focal200_EDGE_AREA= raster::focal(x=boundaries_wood, w=circle.buff, fun=sum, na.rm=T, pad=TRUE, padValue=NA)

plot(Focal200_EDGE_AREA)

#Get actual area - multiply summed pixels by 25 (1 pixel = 25m2 pixel)

Focal200_EDGE_AREA <- Focal200_EDGE_AREA*25
plot(Focal200_EDGE_AREA)

Focal200_EDGE_AREA <- projectRaster(Focal200_EDGE_AREA, nfi_lcm_map_updated)

plot(Focal200_EDGE_AREA)

#-------------------------------------

#Update woodland connectivity ####

#sf object containing new woodland polygons
head(hab_patches_all_updated)

#WOODLAND CONNECTIVITY WITHIN 200m OF WOODLAND - SMALL DEER ####

#### SET CONNECTIVITY PARAMETERS ####
# connectivity parameters - % of dispersers reaching a set distance #
percentage_dispersers <- 0.05 ### 5% - 95% of individuals will go to woodlands
dispersal_distance <- 100 #400 
dispersal_contribution <-
  -((log(1 / percentage_dispersers)) / dispersal_distance)

# Buffer distance represents the cut off - this stops the script measuring every pairwise combination
dispersal_cutoff <- 0.999 ### 99.9% cut off
#buffer_cutoff <-
#round(log(1 / (1 - dispersal_cutoff)) / (log(1 / percentage_dispersers) /
#dispersal_distance), digits = 2)
buffer_cutoff = 200 #200m

# test & plot of dispersal contribution for sequence of distance up to buffer_cutoff
test <-
  data.frame(distance = (seq(0, buffer_cutoff, by = 0.5))) %>%
  mutate(contribution = (exp(dispersal_contribution * distance) * 100))

ggplot(data = test, aes(x = distance, y = contribution, group = 1)) +
  geom_line() +
  geom_point() +
  geom_vline(xintercept = buffer_cutoff, color = "red")

#See how buffer size looks
focal_patch_buffer <- st_buffer(hab_patches_all_updated[1,], buffer_cutoff)
plot(st_geometry(focal_patch_buffer))
plot(st_geometry(hab_patches_all_updated[1,]),add=TRUE)

#-------------------------------------------------------

#Get extent of landscape area to use in connectivity calculations ####

# Get the extent of the raster
raster_extent <- extent(nfi_lcm_map_updated)
raster_extent <- terra::ext(raster_extent)

# Convert the extent to a polygon (terra automatically creates SpatVector polygons)
extent_polygon <- terra::as.polygons(raster_extent)

# Convert SpatVector to sf object
extent_sf <- st_as_sf(extent_polygon)

# Assign a CRS if not already set
st_crs(extent_sf) <- bng # Use the same CRS as the raster

# Plot the extent polygon
plot(st_geometry(extent_sf), main = "Raster Extent as sf Polygon")

#------------------------------------------------------

# CALCULATE INCOMING CONNECTIVITY FOR ALL WOODLANDS ####


# Make empty list to store raster tiles
incoming_connect_df <- list()

hab_patches_all_updated <- st_as_sf(hab_patches_all_updated)

# Assuming you are looping over multiple tiles or patches, 
# this loop could represent multiple iterations (e.g., different 10km tiles)
  
  # Filter for woods inside larger buffer
  hab_patches_buffer <- st_filter(hab_patches_all_updated, extent_sf)
  
  if (nrow(hab_patches_buffer) > 0) {
    
    # Buffer all woods by 200m
    buffered_woods_200m <- st_as_sf(st_buffer(hab_patches_buffer, dist = 200))
    # Ensure CRS of buffers is BNG
    st_crs(buffered_woods_200m) <- bng
    
  } else {
    # Assign NA if nrow(hab_patches_buffer) = 0
    buffered_woods_200m <- NA
  }
    
    # Connectivity for loop ####
    
    connectivity_results <- list()
    
    # Use the 200m buffer as the source patches area
    source_woods <- st_filter(hab_patches_buffer, buffered_woods_200m, .predicate = st_intersects)
    n_source_woods <- n_distinct(source_woods$patch_ID)
    plot(source_woods$geometry)
    
    # Filter the woodlands in the original tile to get the focal patches
    focal_woods <- st_filter(hab_patches_buffer, buffered_woods_5km, .predicate = st_intersects)
    n_focal_woods <- n_distinct(focal_woods$patch_ID)
    plot(focal_woods$geometry)
    
    if (n_focal_woods > 0) {
      
      # Calculate connectivity for each focal woodland
      Connectivity_table <- NULL
      
      # Loop through each focal woodland to calculate connectivity
      for (j in 1:nrow(focal_woods)) {
        
        # Focal patch in the 10km square
        focal_patch <- focal_woods[j, ]
        
        # Buffer 200m around the focal patch
        focal_patch_buffer <- st_buffer(focal_patch, buffer_cutoff)
        
        # Find source patches within this buffer
        source_patches <- st_filter(source_woods, focal_patch_buffer, .predicate = st_intersects)
        
        # Calculate distances between the focal and source patches
        patch_dist <- as.vector(st_distance(focal_patch, source_patches))
        
        # Create a data frame for connectivity information
        Conn_table_site <- data.frame(
          focal_patch = focal_patch$patch_ID,
          focal_patch_area = focal_patch$Shape_Area,  
          source_patch = source_patches$patch_ID,
          source_patch_area = as.numeric(source_patches$Shape_Area),
          distance = patch_dist,
          incoming_connect = ifelse(
            focal_patch$patch_ID == source_patches$patch_ID,
            NA,
            source_patches$Shape_Area * exp(dispersal_contribution * patch_dist)
          )
        )
        
        # Append to 'Connectivity_table'
        if (is.null(Connectivity_table)) {
          Connectivity_table <- Conn_table_site
        } else {
          Connectivity_table <- rbind(Connectivity_table, Conn_table_site)
        }
        
        n_distinct(Connectivity_table$focal_patch)
        
        # Store results for this woodland
        connectivity_results[[paste("focal_wood", focal_woods$patch_ID[j])]] <- Connectivity_table
      }
      
    } else {
      # Assign NA if n_focal_woods = 0
      Connectiviy_table <- NA
    }
      
      # Remove rows where focal_patch = source_patch
      Connectivity_table_filt <- Connectivity_table %>%
        filter(focal_patch != source_patch)
      
      # Identify missing unique patch IDs
      missing_patch_ids <- setdiff(
        unique(Connectivity_table$focal_patch),
        unique(Connectivity_table_filt$focal_patch)
      )
      
      # Add missing patches back with 'incoming_connect' value of 0
      if (length(missing_patch_ids) > 0) {
        missing_patches <- Connectivity_table %>% filter(focal_patch %in% missing_patch_ids) %>%
          # Replace NAs in 'incoming_connect' with 0
          mutate(incoming_connect = coalesce(incoming_connect, 0))  # Replace NA with 0
        
        # Add missing patches back to the filtered table
        Connectivity_table_filt <- rbind(Connectivity_table_filt, missing_patches)
      }
      
      # Ensure no duplicate rows
      Connectivity_table_filt <- Connectivity_table_filt %>% distinct()
      
      # Sum incoming connectivity by focal patch
      incoming_connectivity_sum <- Connectivity_table_filt %>%
        dplyr::select(focal_patch, incoming_connect) %>%
        group_by(focal_patch) %>%
        dplyr::summarise(total_connect = sum(incoming_connect),
                         n = n()) %>%
        mutate(n = if_else(total_connect == 0, 0, n)) %>%  # Set 'n' to 0 when 'incoming_connect' is 0
        rename(patch_ID = focal_patch)
      

#FASTERIZE CONNECTIVITY DATA TO ASSIGN PATCH CONNECTIVITY TO PIXELS ####

#subset hab_patches_all that have a connectivity value in incoming_connectivity_sum
hab_patches_connect <- hab_patches_all_updated %>%
  #rename(patch_ID = Id) %>%
  filter(patch_ID %in% incoming_connectivity_sum$patch_ID)
#left_join the connectivity dataset to the geometry
incoming_connect_vals <- incoming_connectivity_sum  %>%
  dplyr::select(-c("n"))

hab_patches_connect <- left_join(hab_patches_connect, incoming_connect_vals, by="patch_ID")
hab_patches_connect<-st_as_sf(hab_patches_connect)

#fasterize, use land cover map as template
connect_raster <- fasterize::fasterize(hab_patches_connect, raster=boundaries_wood,field="total_connect")
crs(connect_raster) <- bng

connect_raster <- projectRaster(connect_raster, nfi_lcm_map_updated)
plot(connect_raster)

##WOODLAND + HEDGEROW AREA WITHIN 200M---------------------------------------------------------

#Get raster of woodlands within buffer
woods_binary_crop <- crop(NFI_LCM_woods_only, extent_sf)
plot(woods_binary_crop)

circle.buff = raster::focalWeight(woods_binary_crop, d=200, type="circle",fillNA=T)#Create buffer
circle.buff[circle.buff > 0] <- 1   # replacing weights by 1
Focal200_WOOD_AREA= raster::focal(x=woods_binary_crop, w=circle.buff, fun=sum, na.rm=T, pad=TRUE, padValue=NA)

plot(Focal200_WOOD_AREA)

#Get actual area - multiply summed pixels by 25 (1 pixel = 25m2 pixel)

Focal200_WOOD_AREA <- Focal200_WOOD_AREA*25
plot(Focal200_WOOD_AREA)

Focal200_WOOD_AREA <- projectRaster(Focal200_WOOD_AREA, nfi_lcm_map_updated)

#LINEAR FEATURE LENGTH WITHIN 200M--------------------------------------------------------------

LFclip <- crop(lf, extent_sf)
plot(LFclip)

circle.buff = raster::focalWeight(LFclip, d=200, type="circle",fillNA=T)#Create buffer
circle.buff[circle.buff > 0] <- 1   # replacing weights by 1
Focal200_LF_AREA= raster::focal(x=LFclip, w=circle.buff, fun=sum, na.rm=T, pad=TRUE, padValue=NA)

#Get actual area - multiply summed pixels by 25 (1 pixel = 25m2 pixel)

Focal200_LF_AREA <- Focal200_LF_AREA*25
plot(Focal200_LF_AREA)

Focal200_LF_AREA <- projectRaster(Focal200_LF_AREA, nfi_lcm_map_updated)

#-----------------------------------

#SUM THE WOODLAND AREA AND LINEAR FEATURES TOGETHER

Focal_200_WOOD_LF_AREA_BRICK <- brick(Focal200_WOOD_AREA, Focal200_LF_AREA)
plot(Focal_200_WOOD_LF_AREA_BRICK)

Focal_200_WOOD_LF_AREA_SUM <- calc(Focal_200_WOOD_LF_AREA_BRICK, sum, na.rm = TRUE)
plot(Focal_200_WOOD_LF_AREA_SUM)
rm(Focal_200_WOOD_LF_AREA_BRICK)

Focal_200_WOOD_LF_AREA_SUM <- projectRaster(Focal_200_WOOD_LF_AREA_SUM, nfi_lcm_map_updated)

#-----------------------------------

#PERENNIAL + ARABLE VEGETATION (ALTERNATIVE FORAGE)

#This will be grasslands, heathland, fenland, saltmarsh
#arable land included - small deer forager over a smaller area
#therefore the effect of attracting high densities to the area will not be very influential
#compared to the effect on far-ranging large species
#suburban and urban not included

reclass_peren_arable <- data.frame(is = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,
                                          18,19,20,21), becomes = c(0,0,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,1,0,0))

peren_arable_raster <- reclassify(nfi_lcm_map_updated, reclass_peren_arable)
crs(peren_arable_raster) <- bng

peren_arable_raster <- projectRaster(peren_arable_raster, nfi_lcm_map_updated)

#------------------------------------

#UPDATE FORAGE QUALITY MAP ####

#LAND COVER EXTRACTION AT 200m AROUND WOODLAND PIXELS

forage.q.vals <- read.csv(here("data/raw-data/Expert_Qnaire/Forage_Q_medians_large_small_deer.csv"))

land.cover.classes <- read.csv(here("data/raw-data/Expert_Qnaire/Expert_derived_quality_ranks_edge_types_with_water_no_mixed_woods.csv"))

land.cover.classes <- land.cover.classes %>%
  dplyr::select(c(Land.cover, class))

#Filter for small deer only

forage.q.vals <- forage.q.vals %>% filter(species_group %in% c("small"))

forage.q.vals <- left_join(forage.q.vals, land.cover.classes, by = "Land.cover")

#Reclassify land cover map using forage quality scores

#make is-becomes matrix
reclass_vals <- 
  forage.q.vals%>%dplyr::select(Median,class) %>%
  rename(becomes = Median,
         is = class) %>%
  relocate(is) %>%
  filter(!is.na(is))%>%
  filter(is %% 1 == 0)  # This keeps only rows where "is" is a whole number - don't need edges for this

reclass_vals

#Reclassify
map_reclass <- raster::reclassify(nfi_lcm_map_updated, reclass_vals)

#mask reclassified map to include perennial arable only

plot(peren_arable_raster)

#make polygons with which to mask
peren_arable_polys <- rasterToPolygons(peren_arable_raster,na.rm=TRUE,fun=function(x){x>0}, dissolve=TRUE) #TAKES LONG TIME
peren_arable_polys <- st_as_sf(peren_arable_polys)
plot(peren_arable_polys$geometry)

map_reclass_masked <- mask(map_reclass, peren_arable_polys)
plot(map_reclass_masked)

#create circular buffers around every pixel of 200 metres
circle.buff = raster::focalWeight (map_reclass_masked, d=200, type="circle",fillNA=T )#Create buffer
circle.buff[circle.buff > 0] <- 1   # replacing weights by 1
Focal200_FORAGE_QUAL= raster::focal(map_reclass_masked, w=circle.buff, fun=sum, na.rm=T,pad=TRUE, padValue=NA,)

plot(Focal200_FORAGE_QUAL)
crs(Focal200_FORAGE_QUAL) <- bng

Focal200_FORAGE_QUAL <- projectRaster(Focal200_FORAGE_QUAL,nfi_lcm_map_updated)

#------------------------------------

#4. MAXIMUM DAMS (metric for landscape exposure outside of woodlands)
#Need to ensure this is using the combined NFI/LCM when identifying the open habitats

# Crop dams to match the extent of nfi_lcm_map
dams_crop <- crop(dams, extent(nfi_lcm_map_updated))

#Reclass nfi/lcm land cover map so all woodland habitat set to 0
#All non-woodland habitat set to 1

reclass <- data.frame(is = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,
                             18,19,20,21), becomes = c(0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1))

open_raster <- reclassify(nfi_lcm_map_updated, reclass)
plot(open_raster)

#Multiply DAMs and binary LCM together - woodlands will be set to 0 DAMS

DAMS_open_raster <- dams_crop*open_raster
plot(DAMS_open_raster)

#focal statistics, moving window
#Calculate summed DAMS up to 200m away from each woodland pixel

circle.buff = raster::focalWeight(DAMS_open_raster, d=200, type="circle",fillNA=T )#Create buffer
circle.buff[circle.buff > 0] <- 1   # replacing weights by 1
Focal200_SUM_DAMS= raster::focal(x=DAMS_open_raster, w=circle.buff, fun=sum, na.rm=T, pad=TRUE, padValue=NA)


Focal200_SUM_DAMS <- projectRaster(Focal200_SUM_DAMS, nfi_lcm_map_updated)
plot(Focal200_SUM_DAMS)

#------------------------------------

#Get code from current_deer_impact_risk_EW_small_deer to extract raster values
#Create a dataframe containing all extracted raster values within user's landscape

#This takes too long for whole England-Wales dataset
#Try running predict() function using raster data instead

#EXTRACT LENGTH OF WOODLAND EDGE

edgelen <- extract_raster(map_reclass = Focal200_EDGE_AREA,
                          lcm = nfi_lcm_map)

ggplot(edgelen) +
  geom_tile(aes(x = x, y = y, fill = Focal200_EDGE_AREA)) +
  scale_fill_viridis_c(option = "plasma") +  # Apply a continuous viridis color scale
  theme_minimal() +
  guides(fill = guide_colorbar(barwidth = 1, barheight = 10)) +  # Customize the colorbar appearance
  labs(title = "Edge area within 200m",
       fill = "") 

#EXTRACT CONNECTIVITY

connectpix <- extract_raster(map_reclass = connect_raster,
                             lcm = nfi_lcm_map)

ggplot(connectpix) +
  geom_tile(aes(x = x, y = y, fill = connect_raster)) +
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
damspix <- extract_raster(map_reclass = Focal200_SUM_DAMS,
                          lcm = nfi_lcm_map)

ggplot(damspix) +
  geom_tile(aes(x = x, y = y, fill = Focal200_SUM_DAMS)) +
  scale_fill_viridis_c(option = "plasma") +  # Apply a continuous viridis color scale
  theme_minimal() +
  guides(fill = guide_colorbar(barwidth = 1, barheight = 10)) +  # Customize the colorbar appearance
  labs(title = "Summed DAMS within 200m",
       fill = "") 

#EXTRACT PERENNIAL/ARABLE FORAGE QUALITY

foragepix <- extract_raster(map_reclass = Focal200_FORAGE_QUAL,
                          lcm = nfi_lcm_map)

ggplot(foragepix) +
  geom_tile(aes(x = x, y = y, fill = Focal200_FORAGE_QUAL)) +
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

#make any NAs zeros apart from the connect raster
#The connect raster has the correct woodland pixels

df <- df %>%
  mutate(across(c(Focal200_EDGE_AREA, Focal200_LF_AREA, Focal_200_WOOD_LF_AREA_SUM, 
                  Focal200_SUM_DAMS, Focal200_FORAGE_QUAL), 
                ~ tidyr::replace_na(., 0)))

#Remove the NAs

df <- na.omit(df)

#plot the maps

par(mfrow = c(3, 2))

#woodland connectivity
ggplot(df) +
  geom_tile(aes(x = x, y = y, fill = connect_raster)) +
  scale_fill_viridis_c(option = "plasma") +  # Apply a continuous viridis color scale
  theme_minimal() +
  guides(fill = guide_colorbar(barwidth = 1, barheight = 10)) +  # Customize the colorbar appearance
  labs(title = "woodland + linear feature area within 200m",
       fill = "") 

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

#linear feature density
ggplot(df) +
  geom_tile(aes(x = x, y = y, fill = Focal200_LF_AREA)) +
  scale_fill_viridis_c(option = "plasma") +  # Apply a continuous viridis color scale
  theme_minimal() +
  guides(fill = guide_colorbar(barwidth = 1, barheight = 10)) +  # Customize the colorbar appearance
  labs(title = "linear feature density within 200m",
       fill = "") 

#dams
ggplot(df) +
  geom_tile(aes(x = x, y = y, fill = Focal200_SUM_DAMS)) +
  scale_fill_viridis_c(option = "plasma") +  # Apply a continuous viridis color scale
  theme_minimal() +
  guides(fill = guide_colorbar(barwidth = 1, barheight = 10)) +  # Customize the colorbar appearance
  labs(title = "summed dams within 200m",
       fill = "") 

#perennial and arable quality
ggplot(df) +
  geom_tile(aes(x = x, y = y, fill = Focal200_FORAGE_QUAL)) +
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
damage_risk_cpt["MED", "HIGH", "LOW", "MED"] <- 0.1
damage_risk_cpt["MED-HIGH", "HIGH", "LOW", "MED"] <- 0.8
damage_risk_cpt["HIGH", "HIGH", "LOW", "MED"] <- 0.1

damage_risk_cpt["LOW", "LOW", "LOW", "HIGH"] <- 0.8
damage_risk_cpt["LOW-MED", "LOW", "LOW", "HIGH"] <- 0.2
damage_risk_cpt["MED", "LOW", "LOW", "HIGH"] <- 0
damage_risk_cpt["MED-HIGH", "LOW", "LOW", "HIGH"] <- 0
damage_risk_cpt["HIGH", "LOW", "LOW", "HIGH"] <- 0

damage_risk_cpt["LOW", "MED", "LOW", "HIGH"] <- 0
damage_risk_cpt["LOW-MED", "MED", "LOW", "HIGH"] <- 0.2
damage_risk_cpt["MED", "MED", "LOW", "HIGH"] <- 0.2
damage_risk_cpt["MED-HIGH", "MED", "LOW", "HIGH"] <- 0.6
damage_risk_cpt["HIGH", "MED", "LOW", "HIGH"] <- 0

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
damage_risk_cpt["HIGH", "HIGH", "MED", "LOW"] <- 0

damage_risk_cpt["LOW", "LOW", "MED", "MED"] <- 0.1
damage_risk_cpt["LOW-MED", "LOW", "MED", "MED"] <- 0.8
damage_risk_cpt["MED", "LOW", "MED", "MED"] <- 0.1
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
  dplyr::select(-c("x","y"))
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

#PREDICTIONS TAKE A LONG TIME, ABOUT 3 MINUTES PER LINE
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
#df_sf <- st_transform(df_sf, crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
df_sf <- st_transform(df_sf, crs = bng)


# Define the value mapping for pred_damage
damage_values <- c("LOW" = 1, "LOW-MED" = 2, "MED" = 3, "MED-HIGH" = 4, "HIGH" = 5)
df_sf$value <- damage_values[df_sf$pred_damage]

# Set up the colors
val <- 1:5
pal <- c("yellow", "#FED976", "#FD8D3C", "#FC4E2A", "#E31A1C")

#Convert to raster
r <- df_sf %>% dplyr::select(geometry, value) %>% stars::st_rasterize()

r_rast <- terra::rast(r)
#replace 0 with NA
values(r_rast)[values(r_rast) <= 0] = NA

#export raster to inspect in arcgis

terra::writeRaster(r_rast, here("output/EW_datasets_2022/small_deer/updated_risk_test.tif"),overwrite=TRUE)

# Convert the raster to a data frame for ggplot
r_df <- as.data.frame(r_rast, xy = TRUE)

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


