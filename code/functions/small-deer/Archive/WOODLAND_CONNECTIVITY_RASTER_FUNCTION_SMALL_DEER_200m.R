library(ggplot2)
library(raster)
library(sf)
library(progress)

#WOODLAND CONNECTIVITY WITHIN 200m OF WOODLAND - SMALL DEER ####

bng = 27700
  
#Using GB woodland polygons
#Read in woodland polygons from ArcGIS pro:
hab_patches_all <- st_read(here("data/derived-data/LCM2023_GB_WOOD_ExportFeature.shp"))
st_crs(hab_patches_all) <- bng

hab_patches_all <- hab_patches_all %>%
  dplyr::select(-c(Shape_Leng, Shape_Area)) %>%
  mutate(patch_ID = row_number(),
         Shape_Area = st_area(geometry))%>%
  mutate(Shape_Area = as.numeric(Shape_Area))

uk10k_EW <- st_read("./data/derived-data/10k_tiles_EW.shp")

#-----------------------------------------
  
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
  focal_patch_buffer <- st_buffer(hab_patches_all[1,], buffer_cutoff)
  plot(st_geometry(focal_patch_buffer))
  plot(st_geometry(hab_patches_all[1,]),add=TRUE)
  
#---------------------------------------------- START OF LOOP ####
  
tiles <- uk10k_EW
  
  # Make empty list to store raster tiles
  incoming_connect_df <- list()
  
  # Initialize the progress bar
  pb <- progress_bar$new(
    format = "  Processing[:bar] :percent eta: :eta",
    total = nrow(tiles), clear = FALSE, width= 60)
  
  for (i in 1:length(tiles$tile_name)) { 
    tile <- tiles[i,]
    #Buffer tile by 200m
    tile.buff <- st_buffer(tile, 200)
    st_crs(tile.buff) <- bng
    #Filter for woods inside tile buffer
    hab_patches_tile <- st_filter(hab_patches_all, tile.buff)
    
    if(nrow(hab_patches_tile) >0) {
      
    #Buffer all woods by 200m
    buffered_woods_200m <- st_as_sf(st_buffer(hab_patches_tile, dist = 200))
    #ensure crs of buffers is BNG
    st_crs(buffered_woods_200m) <- bng
  
  #Connectivity for loop ####
  
  connectivity_results <- list()
  
  # Use the 200m buffer as the source patches area
  source_woods <- st_filter(hab_patches_tile, buffered_woods_200m, .predicate = st_intersects)
  
  # Filter the woodlands in the original tile to get the focal patches
  focal_woods <- st_filter(hab_patches_tile, tile, .predicate = st_intersects)
  n_focal_woods <- n_distinct(focal_woods$patch_ID)
  
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
      tile_name = tile$tile_name,
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

#----------------------------------------------------------------------

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
    
    #Update progress bar
    pb$tick()
    
  }
  
# Convert incoming_connect_df list to a data frame if needed
incoming_connect_df_table <- do.call(rbind, incoming_connect_df)

#save chunk of data

saveRDS(incoming_connect_df_table, here("output/incoming_connect_2023_small_deer_200m.rds"))

#----------------------------------------------------------------------#END OF LOOP ####
#Get pixel scale connectivity scores from patch-scale data 

#plot the data

#Read in

#incoming_connect_df_table <- readRDS(here("output/small_deer/incoming_connect_small_deer_200m.rds"))

#incoming_connect_df_geom <- left_join(incoming_connect_df_table, hab_patches_all, by = "patch_ID")

#plot(incoming_connect_df_geom$geometry, col = incoming_connect_df_geom$total_connect)
#plot(tiles$geom,add=TRUE)


#subset hab_patches_all that have a connectivity value in incoming_connectivity_sum
    hab_patches_connect <- hab_patches_all %>%
      #rename(patch_ID = Id) %>%
      filter(patch_ID %in% incoming_connect_df_table$patch_ID)
    #left_join the connectivity dataset to the geometry
    incoming_connect_vals <- incoming_connect_df_table  %>%
      dplyr::select(-c("n"))
    
    hab_patches_connect <- left_join(hab_patches_connect, incoming_connect_vals, by="patch_ID")
    hab_patches_connect<-st_as_sf(hab_patches_connect)
    
    st_write(hab_patches_connect, here("output/woodland_connectivity_2023_GB.shp"))
    
    #fasterize, use land cover map as template
    connect_raster <- fasterize::fasterize(hab_patches_connect, raster=nfi_lcm_map,field="total_connect")
    crs(connect_raster) <- bng
    
    writeRaster(connect_raster, here("output/woodland_connectivity_200m_2023_GB.tif"))
    
    connect_rast_square <- crop(connect_raster, tiles[1:10,])
    plot(connect_rast_square)
    
    
    #fasterize patch area
    
    patch_area_rast <- fasterize::fasterize(hab_patches_connect, raster=nfi_lcm_map, field="patch_area")
    crs(patch_area_rast) <- bng
    
    patch_area_square <- crop(patch_area_rast, tiles[1:10,])
    plot(patch_area_square)
    
    
    #crop raster to tile
    
    #connect_raster_tiles <- crop(connect_raster, extent(uk10k_EW))
    
    #pal <- colorRampPalette(c("red", "blue"))
    #plot(connect_raster, col = pal(100))
    
  #--------------------------
    
    #Raster Extraction
    
    # List to store extracted data
    extraction_results <- list()
    
    # Loop through each buffered buffer and crop all rasters
    #buffer_geometry <- buffered_woods_1km$geometry
    
    # Convert buffer geometry to a spatial object that raster can use
    tile_extent <- as(extent(st_bbox(uk10k_EW)), "Extent")
    
    # Get the cell numbers within the extent
    pixel_ID <- cellsFromExtent(nfi_lcm_map, tile_extent)
    
    # Get the coordinates for these cells
    cell_coords <- as.data.frame(xyFromCell(nfi_lcm_map, pixel_ID))
    
    #Extract pixel values
    pixel_values <- extract(connect_raster, cell_coords)
    
    # Create a data frame with the extracted values and cell indices
    connect.vals <- data.frame(total_connect = pixel_values, 
                               pixel_ID = pixel_ID,
                               x = cell_coords$x,
                               y = cell_coords$y)
    
    # Filter out NA values
    connect.vals <- connect.vals[!is.na(connect.vals$total_connect), ]
    

#plot
ggplot(data = hab_patches_all[1:10000,]) +
geom_sf(aes(fill = patch_area)) +
scale_fill_viridis_c() +  # Optional: for a nice color scale
theme_minimal() 

#Plot
ggplot(connect.vals[1:1000000,]) +
  geom_tile(aes(x = x, y = y, fill = total_connect)) +
  scale_fill_viridis_c(option = "plasma") +  # Apply a continuous viridis color scale
  theme_minimal() +
  guides(fill = guide_colorbar(barwidth = 1, barheight = 10)) +  # Customize the colorbar appearance
  labs(title = "Incoming connectivity",
       fill = "Total connectivity")  # Adjust the legend title to be more descriptive

#save connect.vals

saveRDS(connect.vals, here("output/woodland_connectivity_2022.rds"))

connect.vals <- readRDS(here("output/woodland_connectivity_2022.rds"))

#convert rds to raster ####

connect.vals <- connect.vals %>%
  select(!c(pixel_ID))

#make spatialpoints dataframe
coordinates(connect.vals) <- ~x + y
proj4string(connect.vals) <- "+init=epsg:27700"  # Set the CRS to British National Grid

#make a template raster

lcm_extent <- extent(lcm)
template_raster <- raster(ext= lcm_extent,resolution = 25, crs = bng)

connect.raster <- rasterize(connect.vals, template_raster, field = "total_connect", fun = mean, na.rm =TRUE)
writeRaster(connect.raster,here("output/woodland_connectivity_2022.tif"))
