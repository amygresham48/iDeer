#UPDATING iDEER SPATIAL LAYERS FOR BAYESIAN BELIEF NETWORK MODEL ####
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

#Small deer-----------------------------####

#Import current_risk datasets ####

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


#-------------------------------------------------

#Add new woodland polygons to hab_patches_all

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

#-------------------------------------------------

#Update layers ####

#1. The land cover map with woodland edges
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

#-------------------------------------

#Reclassify woodland edges ####

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

#-------------------------------------

#Call function
source(here("code/functions/NFI_LCM_RASTER_FUNCTION.R"))

update_map(nfi=nfi, #latest NFI dataset 
           lcm=lcm, # latest CEH LCM dataset
           tiles=uk10k_EW) #10k tiles for England and Wales



#------------------------------------

#3. The nearest urban and suburban features

source(here("code/functions/NEAREST_URBAN_SUBURBAN_RASTER_FUNCTION.R"))

nearest_urb_suburb(wood_binary_rast = NFI_LCM_woods_only, #woodland binary raster
                   nfi_lcm_map = nfi_lcm_map, #combined nfi/lcm raster
                   tiles=uk10k_EW)

#------------------------------------

#4. MAXIMUM DAMS (metric for landscape exposure outside of woodlands)
#Need to ensure this is using the combined NFI/LCM when identifying the open habitats

#THIS FUNCTION NEEDS FIXING!!
#Extent of output for focal statistics step does not match input

source(here("code/functions/MAX_DAMS_FUNCTION.R"))

max_dams(wood_binary_rast = NFI_LCM_woods_only,
         dams = dams,
         nfi_lcm_map = nfi_lcm_map,
         tiles=uk10k_EW)

#------------------------------------

#5. Linear feature density (hedgerows and treelines)

source(here("code/functions/LINEAR_FEATURE_DENSITY_FUNCTION.R"))

linear_feature_density(wood_binary_rast = NFI_LCM_woods_only,
                       tiles=uk10k_EW,
                       lcm=nfi_lcm_map,
                       hab_patches_all=hab_patches_all
                       )

#------------------------------------

#6. Connectivity

source(here("code/functions/WOODLAND_CONNECTIVITY_RASTER_FUNCTION.R"))

incoming_connectivity(hab_patches_all = hab_patches_all,
                      nfi_lcm_map = nfi_lcm_map,
                      )


