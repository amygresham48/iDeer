update_map <- function(lcm, nfi, tiles) {

#Define British National Grid crs
bng = 27700

#Ensure nfi and lcm are both bng
st_crs(nfi) <- bng
crs(lcm) <- bng

#make template raster
template_raster <- raster(ext = extent(lcm), res = res(lcm), crs = bng)

#Get unique nfi categories
ifts <- unique(nfi$IFT_IOA)
ifts
n_cat <- length(ifts)

#Assign values to nfi, ensuring that the woodland polygon values match those of the LCM

ift_vals <- 50:(50 + n_cat - 1)
ift_vals

#Make dataframe

ift_df <- as.data.frame(ift_vals)
ift_df$IFT_IOA <- ifts

#Make values for Broadleaved = 1, Coniferous = 2, Mixed Broadleaf = 22,
#Mixed Coniferous = 23
ift_df$ift_vals[ift_df$IFT_IOA == "Broadleaved"] <- 1
ift_df$ift_vals[ift_df$IFT_IOA == "Conifer"] <- 2
ift_df$ift_vals[ift_df$IFT_IOA == "Mixed mainly broadleaved"] <- 22
ift_df$ift_vals[ift_df$IFT_IOA == "Mixed mainly conifer"] <- 23

#Add values to NFI dataset

nfi_vals <- left_join(nfi, ift_df, by = c("IFT_IOA" = "IFT_IOA"))

#rasterize nfi ####
nfi.rast <- fasterize::fasterize(nfi_vals, template_raster, field = "ift_vals")

#Create a raster mask where the Broadleaf, Coniferous, Mixed Broadleaf,
#and Mixed Coniferous woodland categories are assigned specific values,
#and the unknown categories are assigned a unique identifier (e.g., 0). 

# Define all possible values in your raster
all_possible_values <- unique(ift_df$ift_vals)

# Create a reclassification matrix
reclass_matrix <- matrix(NA, nrow = length(all_possible_values), ncol = 2)

# Fill the matrix based on conditions
#1,2,22 and 23 remain
#All others reclassified to NA
for (i in 1:length(all_possible_values)) {
  if (all_possible_values[i] %in% c(1, 2, 22, 23)) {
    reclass_matrix[i, ] <- c(all_possible_values[i], all_possible_values[i])
  } else {
    reclass_matrix[i, ] <- c(all_possible_values[i], NA)
  }
}

reclassify.nfi <- reclassify(nfi.rast, reclass_matrix)

#Combine this reclassified raster with the LCM
#The LCM will fill in any blanks.
#Give priority to reclassified NFI raster

#Mosaic the two rasters together using the overlay function ####
#This should give a raster with values from 1-23, where 1 and 2 are BL and conif woodland (same for lcm and NFI)
#Values 22 and 23 will be the mixed woodland types which are not included in the lcm but are included in the NFI dataset.
#All other remaining land cover types will be present

#Give priority to the first raster except where first raster has NAs ####
priority_function <- function(x, y) {
  
  ifelse(is.na(x), y, x)
}

nfi_lcm_mosaic <- raster::overlay(reclassify.nfi, lcm, fun = priority_function)

#export
writeRaster(nfi_lcm_mosaic, here("data/derived-data/nfi_lcm_2022_overlaid.tif"),overwrite=TRUE)

##########################################

#!!!! DEPENDING ON THE COMPUTER YOU'RE USING AND THE AREA, THIS SECTION COULD TAKE SEVERAL DAYS TO FINISH RUNNING !!!!

#Apply a nearest neighbour approach to classify woodland edge pixels according to the most common adjacent land cover type ####

#wood.raster <- raster(here("data/derived-data/nfi_lcm_2022_overlaid_.tif"))

wood.raster <- nfi_lcm_mosaic

#tiles <- uk10k_EW
#tiles <- tiles[1:2,]

# Function to get the modal category of neighbors
get_mode <- function(x) {
  tbl <- table(x)
  modes <- as.numeric(names(tbl[tbl == max(tbl)]))
  return(modes)
}

#make list to store 10k tiles

tile_list <- list()

# Set up a progress bar
pb <- progress_bar$new(total = nrow(tiles), format = "[:bar] :percent :elapsed")

for (j in 1:length(tiles$tile_name)) { 
  #for (j in 1:length(uk10k_EW$tile_name)) { 
  tile <- tiles[j,]
  #tile <- uk10k_EW[j,]
  tile.buff <- st_buffer(tile, 100) #buffer tile by 100m - gives 4 pixel buffer
  wood.raster.buff <- crop(wood.raster,tile.buff)
  
  #Get nonwood habitats in tile buffer
  nonwood <- wood.raster.buff
  #filter out woodlands
  nonwood[nonwood < 3 | nonwood > 21] <- NA
  #Reclass nonwood land cover categories into groups using CEH LCM groupings:
  reclass_matrix <- matrix(c(4,5,6,5,7,5,8,5, # Grasslands = 5
                             9,6,10,6,11,6,12,6, #Mountain, heath, bog = 6
                             13,7,14,8, #Saltwater = 7, Freshwater = 8
                             15,9,16,9,17,9,18,9,19,9), ncol = 2, byrow = TRUE) #Coastal = 9
  nonwood_reclass <- reclassify(nonwood, reclass_matrix)
  
  #Get wood boundaries in tile buffer
  # make binary edge raster. 1 if edge, 0 if not ####
  #Filter for woodland only, make everything else NA
  wood <- wood.raster.buff
  wood[wood[] > 2 & wood[] < 22] = NA
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
    tile_indices <- which(buff_coords[, 1] >= st_bbox(tile)$xmin &
                            buff_coords[, 1] <= st_bbox(tile)$xmax &
                            buff_coords[, 2] >= st_bbox(tile)$ymin &
                            buff_coords[, 2] <= st_bbox(tile)$ymax)
    
    #Get pixel indices for pixels that == 1000 within tile.buff
    woodland_indices <- which(nonwood_woodedges[] == 1000)
    
    #subset woodland_indices for those within tile boundary
    woodland_indices_tile <- woodland_indices[woodland_indices %in% tile_indices]
    
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
    
    #crop to tile extent
    neighbour_raster <- crop(neighbour_raster, tile)
    
    #Assign correct crs
    crs(neighbour_raster) <- bng
    
    #save to raster list
    
    tile_list[[j]] <- neighbour_raster
    
  } else {
    tile_list[[j]] <- NA
    
  }
  
  pb$tick()  # Increment the progress bar after each iteration
}

#Close progress bar

pb$terminate()

#save raster tile list

saveRDS(tile_list, here("output/edge_tile_list.rds"))

#tiles_to_mosaic <- readRDS(here("output/edge_tile_list.rds"))
#edges_all_tiles <- tiles_to_mosaic

edges_all_tiles <- tile_list

#Mosaic the tiles together
# Label elements that are not RasterLayers or are empty
valid_edge_rasters <- lapply(edges_all_tiles, function(raster_layer) {
  if (inherits(raster_layer, "RasterLayer") && any(!is.na(values(raster_layer)))) {
    return(raster_layer)
  } else {
    return(NULL)
  }
})

# Remove NULL elements from the list
valid_edge_rasters <- Filter(function(x) !is.null(x), valid_edge_rasters)

# Mosaic raster
edge_raster_mosaic <- do.call(mosaic, c(valid_edge_rasters, fun = mean))

#export to inspect
writeRaster(edge_raster_mosaic, here("output/edge_raster_mosaic.tif"),overwrite=TRUE)

#Add these values to nfi/lcm overlaid raster ####

#Crop lcm to uk10k_EW
wood_raster_EW <- crop(wood.raster, edge_raster_mosaic)
wood_raster_mask <- mask(wood_raster_EW, uk10k_EW)

#Get rid of the edges that do not correspond to woodlands
#make a woodland binary raster
woods.only <- wood_raster_mask
woods.only[woods.only > 2 & woods.only < 22] <- NA
woods.only[woods.only == 0] <- NA

edge_raster_mosaic_woodland_edges_only <- mask(edge_raster_mosaic, woods.only)
#This should get rid of edges that do not overlap with woodlands

#Add rasters together
# need to make NAs 0, otherwise when we add them, it wont work!
edge_raster_mosaic_woodland_edges_only[is.na(edge_raster_mosaic_woodland_edges_only[])] <- 0 
wood_raster_mask[is.na(wood_raster_mask[])] <- 0

#Adding these two rasters together should preserve the nfi/lcm values that do not overlap with edge_raster_mosaic_woodland_edges_only
edge_core_raster <- wood_raster_mask + edge_raster_mosaic_woodland_edges_only
unique(edge_core_raster)

# [1]  0.000  1.000  1.201  1.202  1.300  1.500  1.600  1.700  1.800  1.900  2.000  2.201  2.202  2.300  2.500
#[16]  2.600  2.700  2.800  2.900  3.000  4.000  5.000  6.000  7.000  8.000  9.000 10.000 11.000 12.000 13.000
#[31] 14.000 15.000 16.000 17.000 18.000 19.000 20.000 21.000 22.000 22.201 22.202 22.300 22.500 22.600 22.700
#[46] 22.800 22.900 23.000 23.201 23.202 23.300 23.500 23.600 23.700 23.800 23.900

#The result shows the type of land cover, and any decimals show the woodland edge type
#To recap the reclassified land cover categories:
#1 = BL woodland, 2 = conifer woodland, 3 = arable, 5 = grassland, 6 = mountain/bog/heath, 7 = saltwater, 8 = freshwater,
#9 = coastal, 20 = urban, 21 = suburban, 22 = mixed mainly BL, 23 = mixed mainly conif.

#To recap the edge types:
#0.5 = Grassland, 0.6 = Mountain/heath/bog, 0.7 = saltwater, 0.8 = Freshwater, 0.9 = coastal, 0.202 = urban, 0.201 = suburban,
#0.444 = Mixed edge (multiple modes)

#Export final raster
writeRaster(edge_core_raster, here("output/edge_core_raster_2022_all_tiles_EW.tif"),overwrite=TRUE)
writeRaster(woods.only, here("output/NFILCM_2022_binary_woodland_all_tiles_EW.tif"),overwrite=TRUE)

}