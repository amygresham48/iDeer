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

#Import current_risk datasets #--------------

#woodland connectivity within 200m
connectivity <- raster(here("output/EW_datasets_2022/small_deer/woodland_connectivity_200m_2022_GB.tif"))

#linear feature density within 200m
lf_200m <- raster(here("output/EW_datasets_2022/small_deer/lf_density_200m_2022.tif"))

#land cover map
#This layer does NOT include woodland edge type
nfi_lcm_map <- raster(here("output/nfi_lcm_2022_overlaid.tif"))

#sum of DAMS within 200m
dams_200m <- raster(here("output/EW_datasets_2022/small_deer/sum_dams_200_non_wood_2022.tif"))

#woodland habitat patches as an sf object

#Read in woodland polygons from ArcGIS pro:
wood_polys <- st_read(here("data/derived-data/NFILCM_2022_GB_polys_arcgis.shp"))
st_crs(wood_polys) <- bng

hab_patches_all <- wood_polys %>%
  dplyr::select(-c(SHAPE_Leng, SHAPE_Area)) %>%
  mutate(patch_ID = dplyr::row_number(),
         Shape_Area = st_area(geometry))%>%
  mutate(Shape_Area = as.numeric(Shape_Area))

#Linear feature layer
#Dataset = CEH Woody Linear Feature Framework (2016)
#Need to modify to remove linear features within woodlands

lf <- st_read(here("data/raw-data/linear_features/GB_WLF_V1_0.gdb"),layer="GB_WLF_V1_0")
st_crs(lf) <- bng

#DAMS
#Interpolated from 50m resolution to 25m resolution
#To match the resolution of other raster layers
#Interpolation method = bilinear
dams <- raster(here("data/raw-data/DAMS/dams_25m_bng.tif"))
crs(dams) <- bng

#function to extract pixel values from raster layers
source(here("code/functions/extract_raster_pixels_func.R"))

#Add new woodland polygons to hab_patches_all #-----------------------

#To test out updating spatial layers with new polygons

# Define a simple bounding box or use actual coordinates for your area
bbox <- st_bbox(hab_patches_all)  # Get bounding box of existing polygons
# Generate a random point within the bounding box
set.seed(123)  # For reproducibility
random_point_1 <- st_sample(st_as_sfc(bbox), 1)  # Sample 1 point

# Create a 200m buffer around this point
buffer_200m <- st_buffer(random_point_1, dist = 200)

# Generate a second random point within the 200m buffer
random_point_2 <- st_sample(buffer_200m, 1)

# Create polygons around these points
polygons <- st_buffer(random_point_1, dist = 50) %>%
  st_union(st_buffer(random_point_2, dist = 50)) %>%
  st_cast(.,"POLYGON")

# Convert the polygons to an sf object and add dummy values
random_polygons_sf <- st_sf(geometry = polygons) %>%
  mutate(patch_ID = max(hab_patches_all$patch_ID) + 1 + row_number(),  # Continue patch_IDs
         Shape_Area = as.numeric(st_area(geometry)))  # Calculate areas

# Combine new polygons with existing data
hab_patches_all_updated <- bind_rows(hab_patches_all, random_polygons_sf)

plot(random_polygons_sf$geometry)

#Assign polygons woodland type (broadleaved or coniferous)

random_polygons_sf_types <- random_polygons_sf %>%
  mutate(woodland_type = c("Mainly broadleaf","Mainly conifer"))

#Update layers #####-------------------------------------------------

#1. The land cover map with woodland edges #####-------------------------------------------------
#This function produces a modified land cover raster with woodland edges
#classified by modal land cover type
#also produces a binary woodland raster to be used for subsequent spatial layers

buffered_woods_5km <- st_as_sf(st_buffer(random_polygons_sf, dist = 5000))

buffered_woods_7km <- st_as_sf(st_buffer(random_polygons_sf, dist = 7000))

plot(buffered_woods_5km$geometry)
plot(random_polygons_sf$geometry,add=TRUE)

eg_buffer_7km <- buffered_woods_7km[1,]
eg_buffer_5km <- buffered_woods_5km[1,]

#Crop original rasters to 7km buffer (e.g. landscape size selected by user)
#e.g. 5km landscape + 2km buffer

lcm_cropped <- crop(nfi_lcm_map, eg_buffer_7km)

# Reclassify the woodland_type into numeric values
random_polygons_sf_types$woodland_type_num <- ifelse(random_polygons_sf_types$woodland_type == "Mainly broadleaf", 1, 
                                                    ifelse(random_polygons_sf_types$woodland_type == "Mainly conifer", 2, NA))

#Rasterize the new woodland polygon(s) 
#value = 1 if woodland_type = broadleaved
#value = 2 if woodland_type = coniferous

# Rasterize the polygons
raster_polys <- rasterize(random_polygons_sf_types, lcm_cropped, field = "woodland_type_num")

# Overlay function: replace values in nfi_lcm_map with non-NA values from woodland_raster
nfi_lcm_map_updated <- overlay(lcm_cropped, raster_polys, fun = function(nfi, wood) {
  ifelse(!is.na(wood), wood, nfi)  # If woodland_raster has a non-NA value, use it; otherwise keep nfi_lcm_map value
})

#2. Reclassify woodland edges #####-------------------------------------------------

#Apply a nearest neighbour approach to classify woodland edge pixels according to the most common adjacent land cover type ####

wood.raster <- nfi_lcm_map_updated
crs(nfi_lcm_map_updated) <- bng

# Function to get the modal category of neighbors
get_mode <- function(x) {
  tbl <- table(x)
  modes <- as.numeric(names(tbl[tbl == max(tbl)]))
  return(modes)
}

  #Get nonwood habitats in tile buffer
  nonwood <- wood.raster
  #filter out woodlands
  nonwood[nonwood < 3] <- NA
  #Reclass nonwood land cover categories into groups using CEH LCM groupings:
  reclass_matrix <- matrix(c(4,5,6,5,7,5,8,5, # Grasslands = 5
                             9,6,10,6,11,6,12,6, #Mountain, heath, bog = 6
                             13,7,14,8, #Saltwater = 7, Freshwater = 8
                             15,9,16,9,17,9,18,9,19,9), ncol = 2, byrow = TRUE) #Coastal = 9
  nonwood_reclass <- reclassify(nonwood, reclass_matrix)
  
  #Get wood boundaries in tile buffer
  # make binary edge raster. 1 if edge, 0 if not ####
  #Filter for woodland only, make everything else NA
  wood <- wood.raster
  wood[wood[] >= 3] = NA
  boundaries = boundaries(wood, type='inner') # edge raster
  #Make woodland boundaries = 1000
  boundaries_wood <- boundaries*1000
  # need to make NAs 0
  boundaries_wood[is.na(boundaries_wood[])] <- 0 
  nonwood_reclass[is.na(nonwood_reclass[])] <- 0
  #Mosaic nonwood with wood boundaries, using max function. So wood boundaries will always win (value = 1000)
  nonwood_woodedges <- mosaic(boundaries_wood, nonwood_reclass, fun = max)
  
  if (sum(nonwood_woodedges[] == 1000, na.rm = TRUE) > 0) { #if there are woodland boundary pixels
    
    #Get pixel indices for whole tile.buff
    tile_buff_indices <- cellFromXY(nonwood_woodedges, xyFromCell(nonwood_woodedges, 1:ncell(nonwood_woodedges)))
    
    # Get the coordinates of cells in the buffer
    buff_coords <- xyFromCell(nonwood_woodedges, tile_buff_indices)
    
    # Identify indices of cells that fall within the tile's bounding box
    cell_indices <- which(buff_coords[, 1] >= st_bbox(eg_buffer_5km)$xmin &
                            buff_coords[, 1] <= st_bbox(eg_buffer_5km)$xmax &
                            buff_coords[, 2] >= st_bbox(eg_buffer_5km)$ymin &
                            buff_coords[, 2] <= st_bbox(eg_buffer_5km)$ymax)
    
    #Get pixel indices for pixels that == 1000 within tile.buff
    woodland_indices <- which(nonwood_woodedges[] == 1000)
    
    #subset woodland_indices for those within tile boundary
    woodland_indices_tile <- woodland_indices[woodland_indices %in% cell_indices]
    
    #LOOP TO GET NEIGHBOURING PIXELS ####
    
    # Initialize a dataframe to store results
    result_df <- data.frame(index = numeric(0), mode_neighbors = character(0), stringsAsFactors = FALSE)
    
    for (i in woodland_indices_tile) {
      neighbors <- adjacent(nonwood_woodedges, cells = i, directions = 8, pairs = FALSE)
      neighbors <- neighbors[nonwood_woodedges[neighbors] != 1000 & nonwood_woodedges[neighbors] != 0]  # Exclude cells with value 1000 and 0
      neighbor_values <- nonwood_woodedges[neighbors]
      
      #Get modal neighbour
      if (length(neighbor_values) > 0) {
        mode_result <- get_mode(neighbor_values)
      } else {
        mode_result <- NA  
      }
      # Append to the dataframe
      result_df <- rbind(result_df, data.frame(index = i, mode_neighbors = paste(mode_result, collapse = ",")))
    }
    
    #Where there is more than one mode, randomly assign one of the values to be the representative land cover type
    single_modes <- result_df %>%
      mutate(single_modes = ifelse(grepl(",", mode_neighbors), 
                                   sapply(strsplit(mode_neighbors, ","), function(x) sample(x, 1)),
                                   as.character(mode_neighbors)))
    
    single_modes$single_modes<-as.numeric(single_modes$single_modes)
    
    #Assign names to each category
    neighbor_class <- single_modes %>%
      mutate(neighbor_class = #ifelse(grepl(",", mode_neighbors), "Mixed", #if there is >1 mode, classify as a mixed edge
               ifelse(single_modes == 20, "Urban",
                      ifelse(single_modes == 21, "Suburban",
                             ifelse(single_modes == 3, "Arable",
                                    ifelse(single_modes == 5, "Grassland",
                                           ifelse(single_modes == 6, "Mountain_heath_bog",
                                                  ifelse(single_modes == 7, "Saltwater",
                                                         ifelse(single_modes == 8, "Freshwater",
                                                                ifelse(single_modes == 9, "Coastal", NA)))))))))
    
    #Assign raster values to each category to be added to lcm
    neighbor_class <- neighbor_class %>%
      mutate(neighbor_value = case_when(
        neighbor_class == "Arable" ~ 0.3,
        neighbor_class == "Grassland" ~ 0.5,
        neighbor_class == "Mountain_heath_bog" ~ 0.6,
        neighbor_class == "Saltwater" ~ 0.7,
        neighbor_class == "Freshwater" ~ 0.8,
        neighbor_class == "Coastal" ~ 0.9,
        neighbor_class == "Urban" ~ 0.202,
        neighbor_class == "Suburban" ~ 0.201,
        TRUE ~ NA_real_  # For any other cases
      ))
    
    #Convert this table into a raster
    #First make template using original raster
    neighbour_raster <- raster(ext = extent(nonwood_woodedges), res = res(nonwood_woodedges), crs = bng)
    
    #Get values to assign to pixels
    values_to_assign <- neighbor_class$neighbor_value
    
    #Get pixel indices so we know which pixels will receive each value
    pixel_indices <- neighbor_class$index
    
    #Assign values to raster based on pixel indices
    neighbour_raster[pixel_indices] <- values_to_assign
    
    #crop to 5km extent
    neighbour_raster <- crop(neighbour_raster, eg_buffer_5km)
    
    #Assign correct crs
    crs(neighbour_raster) <- bng
  }  


#Overlay the edges over the to nfi/lcm overlaid raster ####

#Crop lcm to uk10k_EW
wood.raster.crop <- crop(wood.raster, neighbour_raster)
#wood.raster.mask <- mask(wood.raster.crop, neighbour_raster)

#Get rid of the edges that do not correspond to woodlands
#make a woodland binary raster
woods.only <- wood.raster.crop
woods.only[woods.only >= 3] <- NA
woods.only[woods.only == 0] <- NA

edge_raster_woodland_edges_only <- mask(neighbour_raster, woods.only)
#This should get rid of edges that do not overlap with woodlands

#Add rasters together
# need to make NAs 0, otherwise when we add them, it wont work!
edge_raster_woodland_edges_only [is.na(edge_raster_woodland_edges_only [])] <- 0 

#Adding these two rasters together should preserve the nfi/lcm values that do not overlap with edge_raster_mosaic_woodland_edges_only
edge_core_raster <- edge_raster_woodland_edges_only + wood.raster.crop
unique(edge_core_raster)

#The result shows the type of land cover, and any decimals show the woodland edge type
#To recap the reclassified land cover categories:
#1 = BL woodland, 2 = conifer woodland, 3 = arable, 5 = grassland, 6 = mountain/bog/heath, 7 = saltwater, 8 = freshwater,
#9 = coastal, 20 = urban, 21 = suburban

#To recap the edge types:
#0.5 = Grassland, 0.6 = Mountain/heath/bog, 0.7 = saltwater, 0.8 = Freshwater, 0.9 = coastal, 0.202 = urban, 0.201 = suburban,
#0.444 = Mixed edge (multiple modes)

#Export final raster
#writeRaster(edge_core_raster, here("output/EW_datasets_2022/edge_core_raster_2022_all_tiles_EW.tif"),overwrite=TRUE)
#writeRaster(woods.only, here("output/EW_datasets_2022/NFILCM_2022_woodland_all_tiles_EW.tif"),overwrite=TRUE)

#Reclassify wood raster to make a binary raster (0/1)
reclass_matrix <- matrix(c(
  1, 1,  # Reclassify value 1 to 1 #Broadleaf
  2, 1  # Reclassify value 2 to 1 #Coniferous
), ncol=2, byrow=TRUE)
NFI_LCM_woods_only <- reclassify(woods.only, reclass_matrix)
crs(NFI_LCM_woods_only) <- bng

#writeRaster(NFI_LCM_woods_only, here("output/EW_datasets_2022/NFILCM_2022_binary_woodland_all_tiles_EW.tif"),overwrite=TRUE)

NFI_LCM_woods_only <- projectRaster(NFI_LCM_woods_only, nfi_lcm_map_updated)

#-------------------------------------

#LENGTH OF WOODLAND EDGE WITHIN 200m ####

plot(edge_raster_woodland_edges_only)

circle.buff = raster::focalWeight(edge_raster_woodland_edges_only, d=200, type="circle",fillNA=T)#Create buffer
circle.buff[circle.buff > 0] <- 1   # replacing weights by 1
Focal200_EDGE_AREA= raster::focal(x=edge_raster_woodland_edges_only, w=circle.buff, fun=sum, na.rm=T, pad=TRUE, padValue=NA)

plot(Focal200_EDGE_AREA)

#Get actual area - multiply summed pixels by 25 (1 pixel = 25m2 pixel)

Focal200_EDGE_AREA <- Focal200_EDGE_AREA*25
plot(Focal200_EDGE_AREA)

Focal200_EDGE_AREA <- projectRaster(Focal200_EDGE_AREA, nfi_lcm_map_updated)

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

#CALCULATE INCOMING CONNECTIVITY FOR ALL WOODLANDS ####

# Make empty list to store raster tiles
incoming_connect_df <- list()

  #Filter for woods inside larger buffer
  hab_patches_buffer <- st_filter(hab_patches_all_updated, eg_buffer_7km)
  
  if(nrow(hab_patches_buffer) >0) {
    
    #Buffer all woods by 200m
    buffered_woods_200m <- st_as_sf(st_buffer(hab_patches_buffer, dist = 200))
    #ensure crs of buffers is BNG
    st_crs(buffered_woods_200m) <- bng
    
    #Connectivity for loop ####
    
    connectivity_results <- list()
    
    # Use the 200m buffer as the source patches area
    source_woods <- st_filter(hab_patches_buffer, buffered_woods_200m, .predicate = st_intersects)
    n_source_woods <- n_distinct(source_woods$patch_ID)
    plot(source_woods$geometry)
    
    # Filter the woodlands in the original tile to get the focal patches
    focal_woods <- st_filter(hab_patches_buffer, eg_buffer_5km, .predicate = st_intersects)
    n_focal_woods <- n_distinct(focal_woods$patch_ID)
    plot(focal_woods$geometry)
    
    if(n_focal_woods >0) {
      
      # Calculate connectivity for each focal woodland
      Connectivity_table <- NULL
      
      # Loop through each focal woodland to calculate connectivity
      for (j in 1:nrow(focal_woods)) {
        
        # Focal patch in the 10km square square
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
      
      #Remove rows where focal_patch = source_patch
      Connectivity_table_filt <- Connectivity_table %>%
        filter(focal_patch != source_patch)
      
      # Identify missing unique patch IDs
      # These patches have no source patches so will have been filtered out
      # We need to put them back in so we can give them an incoming_connect value of 0
      missing_patch_ids <- setdiff(
        unique(Connectivity_table$focal_patch),
        unique(Connectivity_table_filt$focal_patch)
      )
      
      #print(missing_patch_ids)  # This will show which patch IDs are missing
      
      if (length(missing_patch_ids) > 0) {
        missing_patches <- Connectivity_table %>% filter(focal_patch %in% missing_patch_ids) %>%
          # Replace NAs in 'incoming_connect' with 0
          mutate(incoming_connect = coalesce(incoming_connect, 0))  # Replace NA with 0
        
        # Make rows for patches with no connectivity so we don't lose any patches
        Connectivity_table_filt <- rbind(Connectivity_table_filt, missing_patches)
      }
      
      #Ensure no duplicate rows
      Connectivity_table_filt <- Connectivity_table_filt %>% distinct()
      
      #Now no patches should be missing
      #n_distinct(Connectivity_table_filt$focal_patch)
      
      #sum incoming connectivity by focal patch
      incoming_connectivity_sum <- Connectivity_table_filt %>%
        dplyr::select(focal_patch, incoming_connect) %>%
        group_by(focal_patch) %>%
        dplyr::summarise(total_connect = sum(incoming_connect),
                         n = n()) %>%
        mutate(n = if_else(total_connect == 0, 0, n)) %>%  # Set 'n' to 0 when 'incoming_connect' is 0
        rename(patch_ID = focal_patch)
      
      incoming_connect_df[[i]] <- incoming_connectivity_sum 
      
    } else {
      # Assign NA if nrow(hab_patches_tile) = 0
      incoming_connect_df[[i]] <- NA
    }
    
  } else {
    # Assign NA if n_focal_woods = 0
    incoming_connect_df[[i]] <- NA
  }
  

# Convert incoming_connect_df list to a data frame if needed
incoming_connect_df_table <- do.call(rbind, incoming_connect_df)

#FASTERIZE CONNECTIVITY DATA TO ASSIGN PATCH CONNECTIVITY TO PIXELS ####

#subset hab_patches_all that have a connectivity value in incoming_connectivity_sum
hab_patches_connect <- hab_patches_all_updated %>%
  #rename(patch_ID = Id) %>%
  filter(patch_ID %in% incoming_connect_df_table$patch_ID)
#left_join the connectivity dataset to the geometry
incoming_connect_vals <- incoming_connect_df_table  %>%
  dplyr::select(-c("n"))

hab_patches_connect <- left_join(hab_patches_connect, incoming_connect_vals, by="patch_ID")
hab_patches_connect<-st_as_sf(hab_patches_connect)

#fasterize, use land cover map as template
connect_raster <- fasterize::fasterize(hab_patches_connect, raster=edge_core_raster,field="total_connect")
crs(connect_raster) <- bng

plot(connect_raster)

connect_raster <- projectRaster(connect_raster, nfi_lcm_map_updated)

#-----------------------------------

#LINEAR FEATURE DENSITY WITHIN 200M

#Function to erase linear features inside woodland geometry
st_erase = function(x, y)st_difference(x, st_union(y))

wood_binary_rast <- NFI_LCM_woods_only

#CALCULATE LINEAR FEATURE DENSITY WITHIN 200m OF EACH WOODLAND PIXEL ####

  chunked.wood <- crop(wood_binary_rast, eg_buffer_7km)
  chunked.wood[chunked.wood == 0] <- NA  # remove zeros
  chunked.wood.points <- rasterToPoints(chunked.wood, spatial = TRUE)
  chunked.wood.points <- st_as_sf(chunked.wood.points)
  chunked.wood.points <- sf::st_transform(chunked.wood.points, crs = bng)
  
  if (nrow(chunked.wood.points) > 0) {
    chunked.wood.points$uniqueforID <- paste("point", chunked.wood.points$geometry, sep = "_")

    lf_filtered <- st_filter(lf, eg_buffer_7km) #filter for linear features within landscape and 200m buffer
    #Get linear features outside of woodlands:
    wood.buffer <- crop(wood_binary_rast, eg_buffer_7km) #make polygons of woodlands within 200m buffer
    wood.buffer[wood.buffer == 0] <- NA  # remove zeros
    wood.buffer <- rasterToPolygons(wood.buffer)
    wood.buffer <- st_as_sf(wood.buffer) %>%
      st_transform(.,bng)
    lf_erase <- st_erase(lf_filtered,wood.buffer) #st_erase the lf within the buffer that overlap woodlands
    lf_erase <- st_as_sf(lf_erase)
    
    if (nrow(lf_erase) > 0) {
      sections_lf <- chunked.wood.points
      sections_lf_all <- list()
      
      
      buffer_size <- 200 #200m buffer size
      LFclip <- st_intersection(lf_erase, st_buffer(st_centroid(sections_lf), buffer_size))
      
      if(nrow(LFclip) > 0) {
        LFclip <- LFclip %>% mutate(lf_len = SHAPE_Length)
        
        # Convert LFclip to a data.table
        LFclip <- data.table::data.table(LFclip)
        
        # Summarize using data.table
        sections_l <- LFclip[, .(LFLEN = sum(SHAPE_Length)), by = uniqueforID]
        
        # If you need to convert back to a data frame for further dplyr operations
        sections_l <- as.data.frame(sections_l)
        
        sections_l <- merge(sections_l, sections_lf, by = "uniqueforID", all = TRUE)
        sections_l <- sections_l %>% dplyr::mutate(LFLEN = tidyr::replace_na(LFLEN, 0))
        sections_l$buffer_size <- buffer_size
        sections_lf_all[[length(sections_lf_all) + 1]] <- data.frame(sections_l)
        
        chunked.wood.points.lf <- do.call(rbind, sections_lf_all)
        chunked.wood.points.lf <- st_as_sf(chunked.wood.points.lf, crs = bng)
        chunked.wood.points.2col <- chunked.wood.points.lf %>% dplyr::select(geometry, LFLEN)
        
        template_raster <- crop(nfi_lcm_map_updated, eg_buffer_7km)
        raster_layer <- raster::rasterize(chunked.wood.points.2col, template_raster, field = "LFLEN")
        raster_layer <- crop(raster_layer, eg_buffer_5km)
        crs(raster_layer) <- bng
        
        raster_layer
        
      } else {
        # Assign NA if LFclip is empty
        raster_layer <- NA
      }
      
    } else {
      # Assign NA if nrow lf_tile is 0
      raster_layer <- NA
    }
    
  } else {
    # Assign NA if nrow chunked.wood.points is 0
    raster_layer <- NA
  }

lf_200_updated <- raster_layer
  
#-----------------------------------

#WOODLAND + HEDGEROW AREA WITHIN 200M

#Get raster of woodlands within buffer
NFI_LCM_woods_only #binary woodland raster

circle.buff = raster::focalWeight(NFI_LCM_woods_only, d=200, type="circle",fillNA=T)#Create buffer
circle.buff[circle.buff > 0] <- 1   # replacing weights by 1
Focal200_WOOD_AREA= raster::focal(x=NFI_LCM_woods_only, w=circle.buff, fun=sum, na.rm=T, pad=TRUE, padValue=NA)

plot(Focal200_WOOD_AREA)

#Get actual area - multiply summed pixels by 25 (1 pixel = 25m2 pixel)

Focal200_WOOD_AREA <- Focal200_WOOD_AREA*25
plot(Focal200_WOOD_AREA)

Focal200_WOOD_AREA <- projectRaster(Focal200_WOOD_AREA, nfi_lcm_map_updated)


#Get raster of linear features outside of woods

#lf_sf <- st_as_sf(LFclip)
#lf_raster <- raster::rasterize(lf_sf, template_raster, field = "layer") #binary linear feature raster
#circle.buff = raster::focalWeight(lf_raster, d=200, type="circle",fillNA=T)#Create buffer
#circle.buff[circle.buff > 0] <- 1   # replacing weights by 1
#Focal200_LF_AREA= raster::focal(x=lf_raster, w=circle.buff, fun=sum, na.rm=T, pad=TRUE, padValue=NA)

Focal200_LF_AREA <- lf_200_updated

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
  filter(!is.na(is))

reclass_vals

#Reclassify
map_reclass <- raster::reclassify(edge_core_raster, reclass_vals)

#mask reclassified map to include perennial arable only

plot(peren_arable_raster)

#mask raster to include

#make polygons with which to mask
peren_arable_polys <- rasterToPolygons(peren_arable_raster,na.rm=TRUE,fun=function(x){x>0}, dissolve=TRUE)
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
connect_index_cpt["LOW","LOW","LOW"] <- 1
connect_index_cpt["MED","LOW","LOW"] <- 0
connect_index_cpt["HIGH","LOW","LOW"] <- 0

#table3: lf_length_200m = MED, wood_connectivity_200m = LOW
connect_index_cpt["LOW","MED","LOW"] <- 1
connect_index_cpt["MED","MED","LOW"] <- 0
connect_index_cpt["HIGH","MED","LOW"] <- 0

#table3: lf_length_200m = HIGH, wood_connectivity_200m = LOW
connect_index_cpt["LOW","HIGH","LOW"] <- 0
connect_index_cpt["MED","HIGH","LOW"] <- 1
connect_index_cpt["HIGH","HIGH","LOW"] <- 0

#table4: lf_length_200m = LOW, wood_connectivity_200m = MED
connect_index_cpt["LOW","LOW","MED"] <- 0
connect_index_cpt["MED","LOW","MED"] <- 0
connect_index_cpt["HIGH","LOW","MED"] <- 1

#table5: lf_length_200m = LOW, wood_connectivity_200m = HIGH
connect_index_cpt["LOW","LOW","HIGH"] <- 1
connect_index_cpt["MED","LOW","HIGH"] <- 0
connect_index_cpt["HIGH","LOW","HIGH"] <- 0

#table6: lf_length_200m = MED, wood_connectivity_200m = MED
connect_index_cpt["LOW","MED","MED"] <- 0
connect_index_cpt["MED","MED","MED"] <- 0
connect_index_cpt["HIGH","MED","MED"] <- 1

#table7: lf_length_200m = HIGH, wood_connectivity_200m = HIGH
connect_index_cpt["LOW","HIGH","HIGH"] <- 0
connect_index_cpt["MED","HIGH","HIGH"] <- 0
connect_index_cpt["HIGH","HIGH","HIGH"] <- 1

#table8: lf_length_200m = HIGH, wood_connectivity_200m = MED
connect_index_cpt["LOW","HIGH","MED"] <- 0
connect_index_cpt["MED","HIGH","MED"] <- 1
connect_index_cpt["HIGH","HIGH","MED"] <- 0

#table9: lf_length_200m = MED, wood_connectivity_200m = HIGH
connect_index_cpt["LOW","MED","HIGH"] <- 0
connect_index_cpt["MED","MED","HIGH"] <- 1
connect_index_cpt["HIGH","MED","HIGH"] <- 0


connect_index_cpt

#CPT for Foraging pressure index ####

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

# Updated CPT for forage_pressure_index to ensure at most one 1 per row

forage_pressure_index_cpt["LOW","LOW","LOW","LOW"] <- 1
forage_pressure_index_cpt["MED","LOW","LOW","LOW"] <- 0
forage_pressure_index_cpt["HIGH","LOW","LOW","LOW"] <- 0

forage_pressure_index_cpt["LOW","MED","LOW","LOW"] <- 0
forage_pressure_index_cpt["MED","MED","LOW","LOW"] <- 1
forage_pressure_index_cpt["HIGH","MED","LOW","LOW"] <- 0

forage_pressure_index_cpt["LOW","HIGH","LOW","LOW"] <- 0
forage_pressure_index_cpt["MED","HIGH","LOW","LOW"] <- 0
forage_pressure_index_cpt["HIGH","HIGH","LOW","LOW"] <- 1

forage_pressure_index_cpt["LOW","LOW","MED","LOW"] <- 0
forage_pressure_index_cpt["MED","LOW","MED","LOW"] <- 1
forage_pressure_index_cpt["HIGH","LOW","MED","LOW"] <- 0

forage_pressure_index_cpt["LOW","MED","MED","LOW"] <- 1
forage_pressure_index_cpt["MED","MED","MED","LOW"] <- 0
forage_pressure_index_cpt["HIGH","MED","MED","LOW"] <- 0

forage_pressure_index_cpt["LOW","HIGH","MED","LOW"] <- 0
forage_pressure_index_cpt["MED","HIGH","MED","LOW"] <- 1
forage_pressure_index_cpt["HIGH","HIGH","MED","LOW"] <- 0

forage_pressure_index_cpt["LOW","LOW","HIGH","LOW"] <- 0
forage_pressure_index_cpt["MED","LOW","HIGH","LOW"] <- 1
forage_pressure_index_cpt["HIGH","LOW","HIGH","LOW"] <- 0

forage_pressure_index_cpt["LOW","MED","HIGH","LOW"] <- 0
forage_pressure_index_cpt["MED","MED","HIGH","LOW"] <- 0
forage_pressure_index_cpt["HIGH","MED","HIGH","LOW"] <- 1

forage_pressure_index_cpt["LOW","HIGH","HIGH","LOW"] <- 1
forage_pressure_index_cpt["MED","HIGH","HIGH","LOW"] <- 0
forage_pressure_index_cpt["HIGH","HIGH","HIGH","LOW"] <- 0

forage_pressure_index_cpt["LOW","LOW","LOW","MED"] <- 1
forage_pressure_index_cpt["MED","LOW","LOW","MED"] <- 0
forage_pressure_index_cpt["HIGH","LOW","LOW","MED"] <- 0

forage_pressure_index_cpt["LOW","MED","LOW","MED"] <- 0
forage_pressure_index_cpt["MED","MED","LOW","MED"] <- 1
forage_pressure_index_cpt["HIGH","MED","LOW","MED"] <- 0

forage_pressure_index_cpt["LOW","HIGH","LOW","MED"] <- 0
forage_pressure_index_cpt["MED","HIGH","LOW","MED"] <- 0
forage_pressure_index_cpt["HIGH","HIGH","LOW","MED"] <- 1

forage_pressure_index_cpt["LOW","LOW","MED","MED"] <- 0
forage_pressure_index_cpt["MED","LOW","MED","MED"] <- 1
forage_pressure_index_cpt["HIGH","LOW","MED","MED"] <- 0

forage_pressure_index_cpt["LOW","MED","MED","MED"] <- 0
forage_pressure_index_cpt["MED","MED","MED","MED"] <- 1
forage_pressure_index_cpt["HIGH","MED","MED","MED"] <- 0

forage_pressure_index_cpt["LOW","HIGH","MED","MED"] <- 1
forage_pressure_index_cpt["MED","HIGH","MED","MED"] <- 0
forage_pressure_index_cpt["HIGH","HIGH","MED","MED"] <- 0

forage_pressure_index_cpt["LOW","LOW","HIGH","MED"] <- 0
forage_pressure_index_cpt["MED","LOW","HIGH","MED"] <- 1
forage_pressure_index_cpt["HIGH","LOW","HIGH","MED"] <- 0

forage_pressure_index_cpt["LOW","MED","HIGH","MED"] <- 0
forage_pressure_index_cpt["MED","MED","HIGH","MED"] <- 0
forage_pressure_index_cpt["HIGH","MED","HIGH","MED"] <- 1

forage_pressure_index_cpt["LOW","HIGH","HIGH","MED"] <- 0
forage_pressure_index_cpt["MED","HIGH","HIGH","MED"] <- 1
forage_pressure_index_cpt["HIGH","HIGH","HIGH","MED"] <- 0

forage_pressure_index_cpt["LOW","LOW","LOW","HIGH"] <- 0
forage_pressure_index_cpt["MED","LOW","LOW","HIGH"] <- 1
forage_pressure_index_cpt["HIGH","LOW","LOW","HIGH"] <- 0

forage_pressure_index_cpt["LOW","MED","LOW","HIGH"] <- 1
forage_pressure_index_cpt["MED","MED","LOW","HIGH"] <- 0
forage_pressure_index_cpt["HIGH","MED","LOW","HIGH"] <- 0

forage_pressure_index_cpt["LOW","HIGH","LOW","HIGH"] <- 0
forage_pressure_index_cpt["MED","HIGH","LOW","HIGH"] <- 0
forage_pressure_index_cpt["HIGH","HIGH","LOW","HIGH"] <- 1

forage_pressure_index_cpt["LOW","LOW","MED","HIGH"] <- 0
forage_pressure_index_cpt["MED","LOW","MED","HIGH"] <- 1
forage_pressure_index_cpt["HIGH","LOW","MED","HIGH"] <- 0

forage_pressure_index_cpt["LOW","MED","MED","HIGH"] <- 0
forage_pressure_index_cpt["MED","MED","MED","HIGH"] <- 1
forage_pressure_index_cpt["HIGH","MED","MED","HIGH"] <- 0

forage_pressure_index_cpt["LOW","HIGH","MED","HIGH"] <- 1
forage_pressure_index_cpt["MED","HIGH","MED","HIGH"] <- 0
forage_pressure_index_cpt["HIGH","HIGH","MED","HIGH"] <- 0

forage_pressure_index_cpt["LOW","LOW","HIGH","HIGH"] <- 0
forage_pressure_index_cpt["MED","LOW","HIGH","HIGH"] <- 1
forage_pressure_index_cpt["HIGH","LOW","HIGH","HIGH"] <- 0

forage_pressure_index_cpt["LOW","MED","HIGH","HIGH"] <- 0
forage_pressure_index_cpt["MED","MED","HIGH","HIGH"] <- 1
forage_pressure_index_cpt["HIGH","MED","HIGH","HIGH"] <- 0

forage_pressure_index_cpt["LOW","HIGH","HIGH","HIGH"] <- 1
forage_pressure_index_cpt["MED","HIGH","HIGH","HIGH"] <- 0
forage_pressure_index_cpt["HIGH","HIGH","HIGH","HIGH"] <- 0

forage_pressure_index_cpt

#CPT for thermoregulation index ####

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
thermoreg_index_cpt["LOW","LOW"] <- 1
thermoreg_index_cpt["MED","LOW"] <- 0
thermoreg_index_cpt["HIGH","LOW"] <- 0

#table2:
thermoreg_index_cpt["LOW","MED"] <- 0
thermoreg_index_cpt["MED","MED"] <- 1
thermoreg_index_cpt["HIGH","MED"] <- 0

#table3:
thermoreg_index_cpt["LOW","HIGH"] <- 0
thermoreg_index_cpt["MED","HIGH"] <- 0
thermoreg_index_cpt["HIGH","HIGH"] <- 1

thermoreg_index_cpt

#Final CPT: deer damage risk ####

damage_risk_cpt <- array(
  0,  # Default probability for each cell
  dim = c(5, 3, 3, 3),  # Shape of the array
  dimnames = list(
    damage_risk = c("LOW","LOW-MED","MED","MED-HIGH","HIGH"),
    connectivity_index = c("LOW", "MED", "HIGH"),
    forage_pressure_index = c("LOW", "MED", "HIGH"),
    thermoreg_index = c("LOW", "MED", "HIGH")
    
  )
)
#order = damage_risk, connectivity_index,forage_pressure_index,thermoreg_index

damage_risk_cpt <- array(
  0,  # Default probability for each cell
  dim = c(5, 3, 3, 3),  # Shape of the array
  dimnames = list(
    damage_risk = c("LOW","LOW-MED","MED","MED-HIGH","HIGH"),
    connectivity_index = c("LOW", "MED", "HIGH"),
    forage_pressure_index = c("LOW", "MED", "HIGH"),
    thermoreg_index = c("LOW", "MED", "HIGH")
  )
)

# connectivity_index = LOW, thermoreg_index = LOW
damage_risk_cpt["LOW", "LOW", "LOW", "LOW"] <- 1
damage_risk_cpt["LOW-MED", "LOW", "LOW", "LOW"] <- 0
damage_risk_cpt["MED", "LOW", "LOW", "LOW"] <- 0
damage_risk_cpt["MED-HIGH", "LOW", "LOW", "LOW"] <- 0
damage_risk_cpt["HIGH", "LOW", "LOW", "LOW"] <- 0

damage_risk_cpt["LOW", "MED", "LOW", "LOW"] <- 0
damage_risk_cpt["LOW-MED", "MED", "LOW", "LOW"] <- 1
damage_risk_cpt["MED", "MED", "LOW", "LOW"] <- 0
damage_risk_cpt["MED-HIGH", "MED", "LOW", "LOW"] <- 0
damage_risk_cpt["HIGH", "MED", "LOW", "LOW"] <- 0

damage_risk_cpt["LOW", "HIGH", "LOW", "LOW"] <- 0
damage_risk_cpt["LOW-MED", "HIGH", "LOW", "LOW"] <- 0
damage_risk_cpt["MED", "HIGH", "LOW", "LOW"] <- 1
damage_risk_cpt["MED-HIGH", "HIGH", "LOW", "LOW"] <- 0
damage_risk_cpt["HIGH", "HIGH", "LOW", "LOW"] <- 0

# connectivity_index = LOW, thermoreg_index = MED
damage_risk_cpt["LOW", "LOW", "LOW", "MED"] <- 0
damage_risk_cpt["LOW-MED", "LOW", "LOW", "MED"] <- 1
damage_risk_cpt["MED", "LOW", "LOW", "MED"] <- 0
damage_risk_cpt["MED-HIGH", "LOW", "LOW", "MED"] <- 0
damage_risk_cpt["HIGH", "LOW", "LOW", "MED"] <- 0

damage_risk_cpt["LOW", "MED", "LOW", "MED"] <- 0
damage_risk_cpt["LOW-MED", "MED", "LOW", "MED"] <- 0
damage_risk_cpt["MED", "MED", "LOW", "MED"] <- 1
damage_risk_cpt["MED-HIGH", "MED", "LOW", "MED"] <- 0
damage_risk_cpt["HIGH", "MED", "LOW", "MED"] <- 0

damage_risk_cpt["LOW", "HIGH", "LOW", "MED"] <- 0
damage_risk_cpt["LOW-MED", "HIGH", "LOW", "MED"] <- 0
damage_risk_cpt["MED", "HIGH", "LOW", "MED"] <- 0
damage_risk_cpt["MED-HIGH", "HIGH", "LOW", "MED"] <- 1
damage_risk_cpt["HIGH", "HIGH", "LOW", "MED"] <- 0

# connectivity_index = LOW, thermoreg_index = HIGH
damage_risk_cpt["LOW", "LOW", "LOW", "HIGH"] <- 0
damage_risk_cpt["LOW-MED", "LOW", "LOW", "HIGH"] <- 0
damage_risk_cpt["MED", "LOW", "LOW", "HIGH"] <- 1
damage_risk_cpt["MED-HIGH", "LOW", "LOW", "HIGH"] <- 0
damage_risk_cpt["HIGH", "LOW", "LOW", "HIGH"] <- 0

damage_risk_cpt["LOW", "MED", "LOW", "HIGH"] <- 0
damage_risk_cpt["LOW-MED", "MED", "LOW", "HIGH"] <- 0
damage_risk_cpt["MED", "MED", "LOW", "HIGH"] <- 0
damage_risk_cpt["MED-HIGH", "MED", "LOW", "HIGH"] <- 0
damage_risk_cpt["HIGH", "MED", "LOW", "HIGH"] <- 1

damage_risk_cpt["LOW", "HIGH", "LOW", "HIGH"] <- 0
damage_risk_cpt["LOW-MED", "HIGH", "LOW", "HIGH"] <- 0
damage_risk_cpt["MED", "HIGH", "LOW", "HIGH"] <- 0
damage_risk_cpt["MED-HIGH", "HIGH", "LOW", "HIGH"] <- 0
damage_risk_cpt["HIGH", "HIGH", "LOW", "HIGH"] <- 1

# connectivity_index = MED, thermoreg_index = LOW
damage_risk_cpt["LOW", "LOW", "MED", "LOW"] <- 0
damage_risk_cpt["LOW-MED", "LOW", "MED", "LOW"] <- 0
damage_risk_cpt["MED", "LOW", "MED", "LOW"] <- 1
damage_risk_cpt["MED-HIGH", "LOW", "MED", "LOW"] <- 0
damage_risk_cpt["HIGH", "LOW", "MED", "LOW"] <- 0

damage_risk_cpt["LOW", "MED", "MED", "LOW"] <- 1
damage_risk_cpt["LOW-MED", "MED", "MED", "LOW"] <- 0
damage_risk_cpt["MED", "MED", "MED", "LOW"] <- 0
damage_risk_cpt["MED-HIGH", "MED", "MED", "LOW"] <- 0
damage_risk_cpt["HIGH", "MED", "MED", "LOW"] <- 0

damage_risk_cpt["LOW", "HIGH", "MED", "LOW"] <- 0
damage_risk_cpt["LOW-MED", "HIGH", "MED", "LOW"] <- 0
damage_risk_cpt["MED", "HIGH", "MED", "LOW"] <- 0
damage_risk_cpt["MED-HIGH", "HIGH", "MED", "LOW"] <- 0
damage_risk_cpt["HIGH", "HIGH", "MED", "LOW"] <- 1

# connectivity_index = MED, thermoreg_index = MED
damage_risk_cpt["LOW", "LOW", "MED", "MED"] <- 0
damage_risk_cpt["LOW-MED", "LOW", "MED", "MED"] <- 0
damage_risk_cpt["MED", "LOW", "MED", "MED"] <- 1
damage_risk_cpt["MED-HIGH", "LOW", "MED", "MED"] <- 0
damage_risk_cpt["HIGH", "LOW", "MED", "MED"] <- 0

damage_risk_cpt["LOW", "MED", "MED", "MED"] <- 0
damage_risk_cpt["LOW-MED", "MED", "MED", "MED"] <- 1
damage_risk_cpt["MED", "MED", "MED", "MED"] <- 0
damage_risk_cpt["MED-HIGH", "MED", "MED", "MED"] <- 0
damage_risk_cpt["HIGH", "MED", "MED", "MED"] <- 0

damage_risk_cpt["LOW", "HIGH", "MED", "MED"] <- 0
damage_risk_cpt["LOW-MED", "HIGH", "MED", "MED"] <- 0
damage_risk_cpt["MED", "HIGH", "MED", "MED"] <- 0
damage_risk_cpt["MED-HIGH", "HIGH", "MED", "MED"] <- 0
damage_risk_cpt["HIGH", "HIGH", "MED", "MED"] <- 1

# connectivity_index = MED, thermoreg_index = HIGH
damage_risk_cpt["LOW", "LOW", "MED", "HIGH"] <- 0
damage_risk_cpt["LOW-MED", "LOW", "MED", "HIGH"] <- 0
damage_risk_cpt["MED", "LOW", "MED", "HIGH"] <- 1
damage_risk_cpt["MED-HIGH", "LOW", "MED", "HIGH"] <- 0
damage_risk_cpt["HIGH", "LOW", "MED", "HIGH"] <- 0

damage_risk_cpt["LOW", "MED", "MED", "HIGH"] <- 0
damage_risk_cpt["LOW-MED", "MED", "MED", "HIGH"] <- 0
damage_risk_cpt["MED", "MED", "MED", "HIGH"] <- 1
damage_risk_cpt["MED-HIGH", "MED", "MED", "HIGH"] <- 0
damage_risk_cpt["HIGH", "MED", "MED", "HIGH"] <- 0

damage_risk_cpt["LOW", "HIGH", "MED", "HIGH"] <- 0
damage_risk_cpt["LOW-MED", "HIGH", "MED", "HIGH"] <- 0
damage_risk_cpt["MED", "HIGH", "MED", "HIGH"] <- 0
damage_risk_cpt["MED-HIGH", "HIGH", "MED", "HIGH"] <- 0
damage_risk_cpt["HIGH", "HIGH", "MED", "HIGH"] <- 1

# connectivity_index = HIGH, thermoreg_index = LOW
damage_risk_cpt["LOW", "LOW", "HIGH", "LOW"] <- 0
damage_risk_cpt["LOW-MED", "LOW", "HIGH", "LOW"] <- 0
damage_risk_cpt["MED", "LOW", "HIGH", "LOW"] <- 1
damage_risk_cpt["MED-HIGH", "LOW", "HIGH", "LOW"] <- 0
damage_risk_cpt["HIGH", "LOW", "HIGH", "LOW"] <- 0

damage_risk_cpt["LOW", "MED", "HIGH", "LOW"] <- 0
damage_risk_cpt["LOW-MED", "MED", "HIGH", "LOW"] <- 0
damage_risk_cpt["MED", "MED", "HIGH", "LOW"] <- 0
damage_risk_cpt["MED-HIGH", "MED", "HIGH", "LOW"] <- 1
damage_risk_cpt["HIGH", "MED", "HIGH", "LOW"] <- 0

damage_risk_cpt["LOW", "HIGH", "HIGH", "LOW"] <- 0
damage_risk_cpt["LOW-MED", "HIGH", "HIGH", "LOW"] <- 0
damage_risk_cpt["MED", "HIGH", "HIGH", "LOW"] <- 0
damage_risk_cpt["MED-HIGH", "HIGH", "HIGH", "LOW"] <- 1
damage_risk_cpt["HIGH", "HIGH", "HIGH", "LOW"] <- 0

# connectivity_index = HIGH, thermoreg_index = MED
damage_risk_cpt["LOW", "LOW", "HIGH", "MED"] <- 0
damage_risk_cpt["LOW-MED", "LOW", "HIGH", "MED"] <- 0
damage_risk_cpt["MED", "LOW", "HIGH", "MED"] <- 0
damage_risk_cpt["MED-HIGH", "LOW", "HIGH", "MED"] <- 1
damage_risk_cpt["HIGH", "LOW", "HIGH", "MED"] <- 0

damage_risk_cpt["LOW", "MED", "HIGH", "MED"] <- 0
damage_risk_cpt["LOW-MED", "MED", "HIGH", "MED"] <- 0
damage_risk_cpt["MED", "MED", "HIGH", "MED"] <- 0
damage_risk_cpt["MED-HIGH", "MED", "HIGH", "MED"] <- 0
damage_risk_cpt["HIGH", "MED", "HIGH", "MED"] <- 1

damage_risk_cpt["LOW", "HIGH", "HIGH", "MED"] <- 0
damage_risk_cpt["LOW-MED", "HIGH", "HIGH", "MED"] <- 0
damage_risk_cpt["MED", "HIGH", "HIGH", "MED"] <- 0
damage_risk_cpt["MED-HIGH", "HIGH", "HIGH", "MED"] <- 0
damage_risk_cpt["HIGH", "HIGH", "HIGH", "MED"] <- 1

# connectivity_index = HIGH, thermoreg_index = HIGH
damage_risk_cpt["LOW", "LOW", "HIGH", "HIGH"] <- 0
damage_risk_cpt["LOW-MED", "LOW", "HIGH", "HIGH"] <- 0
damage_risk_cpt["MED", "LOW", "HIGH", "HIGH"] <- 1
damage_risk_cpt["MED-HIGH", "LOW", "HIGH", "HIGH"] <- 0
damage_risk_cpt["HIGH", "LOW", "HIGH", "HIGH"] <- 0

damage_risk_cpt["LOW", "MED", "HIGH", "HIGH"] <- 0
damage_risk_cpt["LOW-MED", "MED", "HIGH", "HIGH"] <- 0
damage_risk_cpt["MED", "MED", "HIGH", "HIGH"] <- 0
damage_risk_cpt["MED-HIGH", "MED", "HIGH", "HIGH"] <- 0
damage_risk_cpt["HIGH", "MED", "HIGH", "HIGH"] <- 1

damage_risk_cpt["LOW", "HIGH", "HIGH", "HIGH"] <- 0
damage_risk_cpt["LOW-MED", "HIGH", "HIGH", "HIGH"] <- 0
damage_risk_cpt["MED", "HIGH", "HIGH", "HIGH"] <- 0
damage_risk_cpt["MED-HIGH", "HIGH", "HIGH", "HIGH"] <- 0
damage_risk_cpt["HIGH", "HIGH", "HIGH", "HIGH"] <- 1


damage_risk_cpt

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
