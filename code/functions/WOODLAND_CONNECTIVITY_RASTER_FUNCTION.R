incoming_connectivity <- function(hab_patches_all,tiles,nfi_lcm_map) {
  
  bng <- 27700
  
  hab_patches_all <- hab_patches_all %>% dplyr::rename(patch_ID = Id,
                                                       patch_area = Shape_Area)
  
  #Buffer all woods by 5km
  buffered_woods_5km <- st_as_sf(st_buffer(hab_patches_all, dist = 5000))  # dist = 5km in meters
  #Buffer all woods by 7km (5km landscape + 2km edge buffer)
  buffered_woods_7km <- st_as_sf(st_buffer(hab_patches_all, dist = 7000))  # dist = 7km in meters
  #ensure crs of buffers is BNG
  st_crs(buffered_woods_7km) <- bng
  st_crs(buffered_woods_5km) <- bng
#-----------------------------------------
  
  #### SET CONNECTIVITY PARAMETERS ####
  # connectivity parametetrs - % of dispersers reaching a set distance #
  percentage_dispersers <- 0.05 ### 5% - 95% of individuals will go to woodlands
  dispersal_distance <- 400 
  dispersal_contribution <-
    -((log(1 / percentage_dispersers)) / dispersal_distance)
  
  # Buffer distance represents the cut off - this stops the script measuring every pairwise combination
  dispersal_cutoff <- 0.999 ### 99.9% cut off
  #buffer_cutoff <-
  #round(log(1 / (1 - dispersal_cutoff)) / (log(1 / percentage_dispersers) /
  #dispersal_distance), digits = 2)
  buffer_cutoff = 1000 #1km
  
  # test & plot of dispersal contribution for sequence of distance up to buffer_cutoff
  test <-
    data.frame(distance = (seq(0, buffer_cutoff, by = 0.5))) %>%
    mutate(contribution = (exp(dispersal_contribution * distance) * 100))
  
  ggplot(data = test, aes(x = distance, y = contribution, group = 1)) +
    geom_line() +
    geom_point() +
    geom_vline(xintercept = buffer_cutoff, color = "red")
  
  #See how buffer size looks
  focal_patch_buffer <- st_buffer(hab_patches[1,], buffer_cutoff)
  plot(st_geometry(focal_patch_buffer))
  plot(st_geometry(hab_patches[1,]),add=TRUE)
  
#----------------------------------------------
  
  #Connectivity for loop ####
  
  connectivity_results <- list()
  
  # Use the 7km buffer as the source patches area
  source_woods <- st_filter(hab_patches_all, buffered_woods_7km, .predicate = st_intersects)
  
  # Filter the woodlands in the original 5km square to get the focal patches
  focal_woods <- st_filter(hab_patches_all, buffered_woods_5km, .predicate = st_intersects)
  n_distinct(focal_woods$patch_ID)
  
  # Calculate connectivity for each focal woodland
  Connectivity_table <- NULL
  
  # Loop through each focal woodland to calculate connectivity
  for (j in 1:nrow(focal_woods)) {
    
    # Focal patch in the 10km square
    focal_patch <- focal_woods[j, ]
    
    # Buffer 1000m around the focal patch
    focal_patch_buffer <- st_buffer(focal_patch, buffer_cutoff)
    
    # Find source patches within this buffer
    source_patches <- st_filter(source_woods, focal_patch_buffer, .predicate = st_intersects)
    
    # Calculate distances between the focal and source patches
    patch_dist <- as.vector(st_distance(focal_patch, source_patches))
    
    # Create a data frame for connectivity information
    Conn_table_site <- data.frame(
      focal_patch = focal_patch$patch_ID,
      focal_patch_area = as.numeric(focal_patch$patch_area),
      source_patch = source_patches$patch_ID,
      source_patch_area = as.numeric(source_patches$patch_area),
      distance = patch_dist,
      incoming_connect = ifelse(
        focal_patch$patch_ID == source_patches$patch_ID,
        NA,
        source_patches$patch_area * exp(dispersal_contribution * patch_dist)
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
    
    print(missing_patch_ids)  # This will show which patch IDs are missing
    
    missing_patches <- Connectivity_table %>% filter(focal_patch %in% missing_patch_ids) %>%
      # Replace NAs in 'incoming_connect' with 0
      mutate(incoming_connect = coalesce(incoming_connect, 0))  # Replace NA with 0
    
    #Make rows for patches with no connectivity so we don't lose any patches
    Connectivity_table_filt <- rbind(Connectivity_table_filt, missing_patches)
    
    #Ensure no duplicate rows
    Connectivity_table_filt <- Connectivity_table_filt %>% distinct()
    
    #Now no patches should be missing
    n_distinct(Connectivity_table_filt$focal_patch)
    
    #sum incoming connectivity by focal patch
    incoming_connectivity_sum <- Connectivity_table_filt %>%
      dplyr::select(focal_patch, incoming_connect) %>%
      group_by(focal_patch) %>%
      dplyr::summarise(total_connect = sum(incoming_connect),
                       n = n()) %>%
      mutate(n = if_else(total_connect == 0, 0, n)) %>%  # Set 'n' to 0 when 'incoming_connect' is 0
      rename(patch_ID = focal_patch)

#--------------------------------
#Get pixel scale connectivity scores from patch-scale data 

    #subset hab_patches_all that have a connectivity value in incoming_connectivity_sum
    hab_patches_connect <- hab_patches_all %>%
      filter(patch_ID %in% incoming_connectivity_sum$patch_ID)
    #left_join the connectivity dataset to the geometry
    incoming_connect_vals <- incoming_connectivity_sum %>%
      dplyr::select(-c("n"))
    
    hab_patches_connect <- left_join(hab_patches_connect, incoming_connect_vals, by="patch_ID")
    hab_patches_connect<-st_as_sf(hab_patches_connect)
    
    #plot
    ggplot(data = hab_patches_connect) +
      geom_sf(aes(fill = total_connect)) +
      scale_fill_viridis_c() +  # Optional: for a nice color scale
      theme_minimal() 
    
    #fasterize, use land cover map as template
    connect_raster <- fasterize::fasterize(hab_patches_connect, raster=nfi_lcm_map,field="total_connect")
    plot(connect_raster)
    crs(connect_raster) <- bng
    
    #crop raster to buffer_5km
    
    connect_raster_5km <- crop(connect_raster, buffered_woods_5km)
    plot(connect_raster_5km)
    
    # List to store extracted data
    extraction_results <- list()
    
    # Loop through each buffered buffer and crop all rasters
    buffer_geometry <- buffered_woods_5km$geometry
    
    # Convert buffer geometry to a spatial object that raster can use
    buffer_extent <- as(extent(st_bbox(buffer_geometry)), "Extent")
    
    # Get the cell numbers within the extent
    pixel_ID <- cellsFromExtent(nfi_lcm_map, buffer_extent)
    
    # Get the coordinates for these cells
    cell_coords <- as.data.frame(xyFromCell(nfi_lcm_map, pixel_ID))
    
    #Extract pixel values
    pixel_values <- extract(connect_raster_5km, cell_coords)
    
    # Create a data frame with the extracted values and cell indices
    connect.vals <- data.frame(total_connect = pixel_values, 
                               pixel_ID = pixel_ID,
                               x = cell_coords$x,
                               y = cell_coords$y)
    
    # Filter out NA values
    connect.vals <- connect.vals[!is.na(connect.vals$total_connect), ]
    
    # Append to the results list
    extraction_results <- connect.vals
    
    # Combine all results into a single dataframe
    connect_df <- bind_rows(extraction_results)
    
    #Plot
    ggplot(connect_df) +
      geom_tile(aes(x = x, y = y, fill = total_connect)) +
      scale_fill_viridis_c(option = "plasma") +  # Apply a continuous viridis color scale
      theme_minimal() +
      guides(fill = guide_colorbar(barwidth = 1, barheight = 10)) +  # Customize the colorbar appearance
      labs(title = "Incoming connectivity",
           fill = "Total connectivity")  # Adjust the legend title to be more descriptive
    
#save connect_df

saveRDS(connect_df, here("output/woodland_connectivity_2022.rds"))

  }
  