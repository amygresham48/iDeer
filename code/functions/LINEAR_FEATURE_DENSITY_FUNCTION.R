linear_feature_density <- function(wood_binary_rast,tiles,lcm,hab_patches_all){
  
  lf_filtered <- st_filter(lf, tiles[1:2,])
  
  #Function to erase linear features inside woodland geometry
  st_erase = function(x, y)st_difference(x, st_union(y))
  #Get linear features outside of woodlands:
  lf_erase <- st_erase(lf_filtered,hab_patches_all)
  crs(lf_outside_woods)
  lf_erase <- st_as_sf(lf_erase)
  
#-----------------------------------------------------

  tiles <- tiles[1:2,]
  
#LOOP THROUGH EACH TILE AND CALCULATE LINEAR FEATURE DENSITY WITHIN 1KM OF EACH WOODLAND PIXEL ####
  
  lf_1000_list <- list()

  for (i in 1:length(tiles$tile_name)) {
    tile <- tiles[i,]
    tile.buff <- st_buffer(tile, 1200)
    chunked.wood <- crop(wood_binary_rast, tile)
    chunked.wood[chunked.wood == 0] <- NA  # remove zeros
    chunked.wood.points <- rasterToPoints(chunked.wood, spatial = TRUE)
    chunked.wood.points <- st_as_sf(chunked.wood.points)
    chunked.wood.points <- sf::st_transform(chunked.wood.points, crs = bng)
    
    if (nrow(chunked.wood.points) > 0) {
      chunked.wood.points$uniqueforID <- paste("point", chunked.wood.points$geometry, sep = "_")
      chunked.wood.points$TILENAME <- tile$tile_name
      
      lf_tile <- st_filter(lf_erase, tile.buff)
      
      if (nrow(lf_tile) > 0) {
        sections_lf <- chunked.wood.points
        sections_lf_all <- list()
        
        # Only use the 1000 buffer size
        buffer_size <- 1000
        LFclip <- st_intersection(lf_tile, st_buffer(st_centroid(sections_lf), buffer_size))
        
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
    
  }
  
  
}