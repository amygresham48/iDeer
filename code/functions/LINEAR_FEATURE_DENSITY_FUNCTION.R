#LINEAR FEATURE DENSITY WITHIN 200M FOR SMALL DEER MODEL ####

linear_feature_density <- function(wood_binary_rast,tiles,lcm,lf){
  
  #Function to erase linear features inside woodland geometry
  st_erase = function(x, y)st_difference(x, st_union(y))
  
wood_binary_rast <- NFI_LCM_woods_only

#filter uk50k for tiles that are fucked up
#Found by inspecting ArcGIS

#SUNE, SUNW, SUSW, SUSE all need redoing

#tiles <- uk10k %>%
  #filter(str_detect(tile_name, c("SU")))


#-----------------------------------------------------

  #tiles <- uk10k_EW[1191:1400,]
  
  #wood_binary_rast <- NFI_LCM_woods_only_mask
  
#LOOP THROUGH EACH TILE AND CALCULATE LINEAR FEATURE DENSITY WITHIN 200m OF EACH WOODLAND PIXEL ####
  
  lf_1000_list <- list()
  
  # Create a progress bar
  pb <- progress_bar$new(
    format = "[:bar] :current/:total (:percent) elapsed: :elapsedfull",
    total = length(tiles$tile_name), clear = FALSE, width = 60
  )

  for (i in 1:length(tiles$tile_name)) {
    tile <- tiles[i,]
    #Buffer tile by 1100m
    tile.buff <- st_buffer(tile, 1100)
    chunked.wood <- crop(wood_binary_rast, tile)
    chunked.wood[chunked.wood == 0] <- NA  # remove zeros
    chunked.wood.points <- rasterToPoints(chunked.wood, spatial = TRUE)
    chunked.wood.points <- st_as_sf(chunked.wood.points)
    chunked.wood.points <- sf::st_transform(chunked.wood.points, crs = bng)
    
    if (nrow(chunked.wood.points) > 0) {
      chunked.wood.points$uniqueforID <- paste("point", chunked.wood.points$geometry, sep = "_")
      chunked.wood.points$TILENAME <- tile$tile_name
      
      lf_tile <- st_filter(lf, tile.buff) #filter for linear features within tile and 1100m buffer
      #Get linear features outside of woodlands:
      wood.buffer <- crop(wood_binary_rast, tile.buff) #make polygons of woodlands within 1100m buffer
      wood.buffer[wood.buffer == 0] <- NA  # remove zeros
      wood.buffer <- rasterToPolygons(wood.buffer)
      wood.buffer <- st_as_sf(wood.buffer) %>%
        st_transform(.,bng)
      lf_erase <- st_erase(lf_tile,wood.buffer) #st_erase the lf within the buffer that overlap woodlands
      crs(lf_erase)
      lf_erase <- st_as_sf(lf_erase)
      
      if (nrow(lf_erase) > 0) {
        sections_lf <- chunked.wood.points
        sections_lf_all <- list()
        
        
        buffer_size <- 1000 #1km buffer size
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
          
          template_raster <- crop(lcm, tile.buff)
          raster_layer <- raster::rasterize(chunked.wood.points.2col, template_raster, field = "LFLEN")
          raster_layer <- crop(raster_layer, tile)
          crs(raster_layer) <- bng
          
          # Save raster_layer to the corresponding list
          lf_1000_list[[i]] <- raster_layer
          
        } else {
          # Assign NA if LFclip is empty
          lf_1000_list[[i]] <- NA
        }
        
      } else {
        # Assign NA if nrow lf_tile is 0
        lf_1000_list[[i]] <- NA
      }
      
    } else {
      # Assign NA if nrow chunked.wood.points is 0
      lf_1000_list[[i]] <- NA
    }
    # Update the progress bar
    pb$tick()
    
  }
  
print("LF raster list complete")
  
  
#save tiles
#save raster tile list
saveRDS(lf_1000_list, here("output/intermediate-outputs/lf_1000_list_SU_10k_tiles.rds"))

#Read in all raster tiles

lf1 <- readRDS(here("output/intermediate-outputs/lf_1000_list_1_200.rds"))
lf2 <- readRDS(here("output/intermediate-outputs/lf_1000_list_201_500.rds"))
lf3 <- readRDS(here("output/intermediate-outputs/lf_1000_list_501_800.rds"))
lf4 <- readRDS(here("output/intermediate-outputs/lf_1000_list_801_1100.rds"))
lf5 <- readRDS(here("output/intermediate-outputs/lf_1000_list_1101_1191.rds"))
lf6 <- readRDS(here("output/intermediate-outputs/lf_1000_list_1191_1400.rds"))
lf7 <- readRDS(here("output/intermediate-outputs/lf_1000_list_1401_1740.rds"))
lf8 <- readRDS(here("output/intermediate-outputs/lf_1000_list_SU_10k_tiles.rds"))


# defining new list
lf_list <- c(lf1, lf2, lf3, lf4, lf5, lf6, lf7,lf8)

#lf_list <- lf_1000_list

#Mosaic the tiles together
# Label elements that are not RasterLayers or are empty
valid_lf_rasters <- lapply(lf_list, function(raster_layer) {
  if (inherits(raster_layer, "RasterLayer") && any(!is.na(values(raster_layer)))) {
    return(raster_layer)
  } else {
    return(NULL)
  }
})


# Remove NULL elements from the list
valid_lf_rasters  <- Filter(function(x) !is.null(x), valid_lf_rasters )

# Mosaic raster
lf_mosaic <- do.call(mosaic, c(valid_lf_rasters , fun = mean))

#ensure crs is bng

crs(lf_mosaic) <- bng

# Save raster
writeRaster(lf_mosaic, here("output/lf_density_1km_raster_2022_EW.tif"),overwrite=TRUE)
print("LF raster mosaic complete and saved")
  
}