#This function runs the complete iDeer Bayesian Belief Network workflow

#Arguments of this function:

#SPATIAL LAYERS:

#1. map = Raster layer. Land cover map. In this case, it is the combined NFI/CEHLCM
#land cover map

#2. wood_polys = Non-unified lcm polygons. Shapefile.
#This layer is made from nfi_lcm_2015_overlaid.tif
#nfi_lcm_mosaic <- raster(here("data/derived-data/nfi_lcm_2015_overlaid.tif"))
#Made in ArcGIS
#Raster to polygon of all land cover classes
#Subset of lcm_polys as follows:
#lcm_polys <- st_read(here("data/derived-data/nfi_lcm_2015_overlaid.shp"))
#Select for woodland polys only
#wood_polys <- lcm_polys %>% filter(gridcode %in% c("1","2","22","23")) %>%
  r#ename(patch_ID = Id,
         #class = gridcode)

#3. hab_patches_all = shapefile. Combined NFI 2015 and LCM 2015 woodlands
#These polygons will be used to calculate the connectivity, they are unified
#All touching polygons have become unified

#4. nearest_road = raster layer. Pixel values are distance to nearest road from
#each woodland pixel.
#This includes main roads only (A roads, B roads, motorways)

#5. nearest_urban = raster layer. Pixel values are distance to nearest urban
#area from each woodland pixel. Urban areas are defined by "map".

#6. nearest_suburban = raster layer. Pixel values are distance to nearest suburban
#area from each woodland pixel. Suburban areas are defined by "map".

#7. dams = raster layer. Pixel values are maximum DAMS score within 1km of
#each pixel.

#8. lf = raster layer. Pixel values are length of linear features (hedgerows and
#treelines) within 1km of each woodland pixel.


#INDIVIDUAL SPATIAL OBJECTS TO BE DEFINED PRIOR TO RUNNING FUNCTION

#1. fivekm_buffer = 5km landscape chosen by user. This is chosen by selecting a
#woodland polygon then drawing a 5km buffer around it

#2. sevenkm_buffer = 7km buffer around chosen point, used to prevent edge effects
#in spatial extractions

#DATA TABLES:

#1. edge.val.types = forage quality classifications for the different pixel
#classes within "map", derived from expert questionnaires.



ideer_bn_func <- function(map, wood_polys, hab_patches_all,nearest_road,
                          nearest_urban,nearest_suburban,dams,lf,
                          fivekm_buffer, sevenkm_buffer, edge.val.types) {
  
  #Load packages ####
  
  # Function to check if a package is installed, install if not, then load
  load_or_install <- function(package) {
    if (!require(package, character.only = TRUE)) {
      install.packages(package, dependencies = TRUE)
      library(package, character.only = TRUE)
    }
  }
  
  # Example usage with multiple packages
  packages <- c("ggplot2", "plyr" ,"dplyr", "sf","raster","terra","here",
                "bnlearn","RColorBrewer","rasterVis","ggmap",
                "mapview")
  
  # Loop through the packages and load or install them
  for (package in packages) {
    load_or_install(package)
  }
  
  #Confirm directory
  here()
  
  #Extract raster variables from all woodlands within fivekm_buffer ####
  
  #NEAREST URBAN EXTRACTION ####
  
  source(here("code/functions/extract_urban_raster_function.R"))
  
  nearest_urban_extraction <- extract_urban_rasters(fivekm_buffer=fivekm_buffer, 
                                                    raster_layer=nearest_urban,
                                                    land_cover_raster=map)
  #Plot
  ggplot(nearest_urban_extraction) +
    geom_tile(aes(x = x, y = y, fill = nearest_urban)) +
    scale_fill_viridis_c(option = "plasma") +  # Apply a continuous viridis color scale
    theme_minimal() +
    guides(fill = guide_colorbar(barwidth = 1, barheight = 10)) +  # Customize the colorbar appearance
    labs(title = "Distance to Nearest Urban area",
         fill = "Distance (m)")  
  
  #NEAREST SUBURBAN EXTRACTION ####
  
  source(here("code/functions/extract_suburban_raster_function.R"))
  
  nearest_suburban_extraction <- extract_suburban_rasters(fivekm_buffer=fivekm_buffer,
                                                          raster_layer=nearest_suburb,
                                                          land_cover_raster=map)
  #Plot
  ggplot(nearest_suburban_extraction) +
    geom_tile(aes(x = x, y = y, fill = nearest_suburban)) +
    scale_fill_viridis_c(option = "plasma") +  # Apply a continuous viridis color scale
    theme_minimal() +
    guides(fill = guide_colorbar(barwidth = 1, barheight = 10)) +  # Customize the colorbar appearance
    labs(title = "Distance to Nearest Suburban area",
         fill = "Distance (m)")
  
  #MAX DAMS EXTRACTION AT PIXEL LEVEL ####
  
  #Call function to extract values from pixels in dams raster
  #The extracted output contains the following columns:
  #max_dams = maximum dams value within 1km of that pixel
  #pixel_ID = obtained from full land cover map
  #x = x coordinate of pixel
  #y = y coordinate of pixel
  source(here("code/functions/extract_dams_raster_function.R"))
  
  dams_extraction <- extract_dams_raster(fivekm_buffer=fivekm_buffer, 
                                         raster_layer=dams,
                                         land_cover_raster=map)
  #Plot
  ggplot(dams_extraction) +
    geom_tile(aes(x = x, y = y, fill = max_dams)) +
    scale_fill_viridis_c(option = "plasma") +  # Apply a continuous viridis color scale
    theme_minimal() +
    guides(fill = guide_colorbar(barwidth = 1, barheight = 10)) +  # Customize the colorbar appearance
    labs(title = "Max Dams within 1km",
         fill = "Max DAMS") 
  
  #LINEAR FEATURE DENSITY WITHIN 1KM EXTRACTION AT PIXEL LEVEL ####
  
  #Call function to extract values from pixels in linear feature raster
  #The extracted output contains the following columns:
  #lf = total length of linear features within 1km of that pixel
  #pixel_ID = obtained from full land cover map
  #x = x coordinate of pixel
  #y = y coordinate of pixel
  source(here("code/functions/extract_linear_feature_density_raster_function.R"))
  
  lf_extraction <- extract_lf_raster(buffered_woods_5km=buffered_woods_5km, 
                                     raster_layer=lf,
                                     land_cover_raster=map)
  
  #Plot
  ggplot(lf_extraction) +
    geom_tile(aes(x = x, y = y, fill = lf)) +
    scale_fill_viridis_c(option = "plasma") +  # Apply a continuous viridis color scale
    theme_minimal() +
    guides(fill = guide_colorbar(barwidth = 1, barheight = 10)) +  # Customize the colorbar appearance
    labs(title = "Linear feature density",
         fill = "Linear feature length within 1km") 
  
  #NEAREST ROAD EXTRACTION AT PIXEL LEVEL ####
  
  #Call function to extract values from pixels in nearest road raster
  #This raster layer includes A roads, B roads and motorways only (not minor roads)
  #The extracted output contains the following columns:
  #nearest_road = distance to the main road nearest to that pixel.
  #pixel_ID = obtained from full land cover map
  #x = x coordinate of pixel
  #y = y coordinate of pixel
  source(here("code/functions/extract_nearest_road_raster_function.R"))
  
  nearest_road_extraction <- nearest_road_rasters(buffered_woods_5km=buffered_woods_5km, 
                                                  raster_layer=nearest_road,
                                                  land_cover_raster=map)
  #Plot
  ggplot(nearest_road_extraction) +
    geom_tile(aes(x = x, y = y, fill = nearest_road)) +
    scale_fill_viridis_c(option = "plasma") +  # Apply a continuous viridis color scale
    theme_minimal() +
    guides(fill = guide_colorbar(barwidth = 1, barheight = 10)) +  # Customize the colorbar appearance
    labs(title = "Nearest road",
         fill = "Distance (m)")  
  
  #LAND COVER EXTRACTION AT 1KM AROUND WOODLAND PIXELS ####
  
  #Reclassify land cover map using forage quality scores "edge.val.types"
  
  #make is-becomes matrix
  reclass_vals <- edge.val.types%>%dplyr::select(Forage_Q,class) %>%
    rename(becomes = Forage_Q,
           is = class) %>%
    relocate(is) %>%
    filter(!is.na(is))
  
  #Crop map to buffer_7km
  map_7km <- crop(map, buffered_woods_7km)
  
  #Reclassify
  map_reclass <- raster::reclassify(map_7km, reclass_vals)
  
  circle.buff = raster::focalWeight (map_reclass, d=1000, type="circle",fillNA=T )#Create buffer
  circle.buff[circle.buff > 0] <- 1   # replacing weights by 1
  Focal1000= raster::focal(map_reclass, w=circle.buff, fun=mean, na.rm=T)#do focal
  
  #Extract the mean forage quality values ####
  
  # Use woodland patches that intersect 5km buffer
  #Filter for woodland patches that intersect buffers
  woods_buffers <- st_filter(wood_polys, fivekm_buffer, 
                             .predicate = st_intersects)
  
  plot(woods_buffers$geometry)
  
  # Crop land cover map by the 5km buffer
  fcrop <- crop(Focal1000, fivekm_buffer)
  plot(fcrop)
  
  #Mask the land cover map with the woodland polygons
  fmask <- mask(fcrop, woods_buffers)
  plot(fmask)
  
  # Get extent of 5km buffer
  km5_extent <- as(extent(st_bbox(fivekm_buffer)), "Extent")
  
  # Get the cell numbers within the extent
  pixel_ID <- cellsFromExtent(map, km5_extent)
  
  # Get the coordinates for these cells
  cell_coords <- as.data.frame(xyFromCell(map, pixel_ID))
  
  # Extract pixel values
  pixel_values <- extract(fmask, cell_coords)
  
  # Create a data frame with the extracted values and cell indices
  lc_quality_df <- data.frame(pixel_ID = pixel_ID,
                              lc_forage_q = pixel_values,
                              patch_ID = woods_buffers$patch_ID[j],
                              x = cell_coords$x,
                              y = cell_coords$y)
  
  # Filter out NA values
  lc_quality_df <- lc_quality_df[!is.na(lc_quality_df$lc_forage_q), ]
  
  #Plot
  ggplot(lc_quality_df) +
    geom_tile(aes(x = x, y = y, fill = lc_forage_q)) +
    scale_fill_viridis_c(option = "plasma") +  # Apply a continuous viridis color scale
    theme_minimal() +
    guides(fill = guide_colorbar(barwidth = 1, barheight = 10)) +  # Customize the colorbar appearance
    labs(title = "Mean forage quality within 1km",
         fill = "Mean forage quality")
  
  #WOODLAND PIXEL EXTRACTION ####
  #Do this at the pixel level
  
  # Use woodland patches that intersect 5km buffer
  plot(woods_buffers$geometry)
  
  # Crop land cover map by the 5km buffer
  fcrop <- crop(map, buffered_woods_5km)
  plot(fcrop)
  
  #Mask the land cover map with the woodland polygons
  fmask <- mask(fcrop, woods_buffers)
  plot(fmask)
  
  # Get extent of 5km buffer
  km5_extent <- as(extent(st_bbox(buffered_woods_5km)), "Extent")
  
  # Get the cell numbers within the extent
  pixel_ID <- cellsFromExtent(map, km5_extent)
  
  # Get the coordinates for these cells
  cell_coords <- as.data.frame(xyFromCell(map, pixel_ID))
  
  # Extract pixel values
  pixel_values <- extract(fmask, cell_coords)
  
  # Create a data frame with the extracted values and cell indices
  woodpix <- data.frame(pixel_ID = pixel_ID,
                        class = pixel_values,
                        patch_ID = woods_buffers$patch_ID[j],
                        x = cell_coords$x,
                        y = cell_coords$y)
  
  # Filter out NA values
  woodpix <- woodpix[!is.na(woodpix$class), ]
  woodpix <- woodpix%>%dplyr::select(-c("X"))
  
  
  #Plot
  ggplot(woodpix) +
    geom_tile(aes(x = x, y = y, fill = class)) +
    scale_fill_viridis_c(option = "plasma") +  # Apply a continuous viridis color scale
    theme_minimal() +
    guides(fill = guide_colorbar(barwidth = 1, barheight = 10)) +  # Customize the colorbar appearance
    labs(title = "Land cover",
         fill = "Pixel class")  
  
  #ASSIGN WOODLAND PIXEL QUALITY VALUES ####
  
  #Check edge.val.types$class and woods$class match in format
  unique(edge.val.types$class)
  class(edge.val.types$class)
  edge.val.types <- edge.val.types%>%arrange(class)
  unique(edge.val.types$class)
  
  class(woodpix$class)
  woods <- woodpix%>%arrange(class)
  unique(woodpix$class)
  pcm <-woodpix%>%arrange(class)
  unique(pcm$class)
  
  # Round to 3 decimal places
  woodpix$class <- round(woodpix$class, 3)
  edge.val.types$class <- round(edge.val.types$class, 3)
  
  #Stitch the quality scores into the lcm dataset
  
  wood.edge.types <- woodpix %>% left_join(edge.val.types, by = c("class")) 

  #Replace "NA" in Forage_Q column with 0
  wood.edge.types <- wood.edge.types%>% 
    mutate(Forage_Q = ifelse(is.na(Forage_Q), 0, Forage_Q))%>%
    dplyr::select(-c("class","Land.cover"))%>%
    rename(woodland_q = Forage_Q)
  
  #Plot
  ggplot(wood.edge.types) +
    geom_tile(aes(x = x, y = y, fill = woodland_q)) +
    scale_fill_viridis_c(option = "plasma") +  # Apply a continuous viridis color scale
    theme_minimal() +
    guides(fill = guide_colorbar(barwidth = 1, barheight = 10)) +  # Customize the colorbar appearance
    labs(title = "Woodland forage quality",
         fill = "Pixel quality")  
  
  #CALCULATE CONNECTIVITY ####
  #For every woodland inside the 5km buffers
  
  #SET CONNECTIVITY PARAMETERS
  # connectivity parametetrs - % of dispersers reaching a set distance #
  percentage_dispersers <- 0.05 ### 5% - 95% of individuals will go to woodlands
  dispersal_distance <- 400 #Gets curve up to 1000m on x axis
  dispersal_contribution <-
    -((log(1 / percentage_dispersers)) / dispersal_distance)
  
  # Buffer distance represents the cut off - this stops the script measuring every pairwise combination
  dispersal_cutoff <- 0.999 ### 99.9% cut off
  #buffer_cutoff <-
  #round(log(1 / (1 - dispersal_cutoff)) / (log(1 / percentage_dispersers) /
  #dispersal_distance), digits = 2)
  buffer_cutoff = 1000 #1km
  
  #Connectivity for loop
  
  connectivity_results <- list()
  
  # Use the 7km buffer as the source patches area
  source_woods <- st_filter(hab_patches_all, sevenkm_buffer, .predicate = st_intersects)
  
  # Filter the woodlands in the original 5km square to get the focal patches
  focal_woods <- st_filter(hab_patches_all, fivekm_buffer, .predicate = st_intersects)
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
  
  #Get pixel-level connectivity from patch-scale data ####
  
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
  connect_raster <- fasterize::fasterize(hab_patches_connect, raster=Focal1000,field="total_connect")
  plot(connect_raster)
  crs(connect_raster) <- bng
  
  #crop raster to buffer_5km
  
  connect_raster_5km <- crop(connect_raster, buffered_woods_5km)
  plot(connect_raster_5km)
  
  # List to store extracted data
  extraction_results <- list()
  
  # Loop through each buffered buffer and crop all rasters
  buffer_geometry <- fivekm_buffer$geometry
  pb$tick()  # Update the progress bar
  
  # Convert buffer geometry to a spatial object that raster can use
  buffer_extent <- as(extent(st_bbox(buffer_geometry)), "Extent")
  
  # Get the cell numbers within the extent
  pixel_ID <- cellsFromExtent(map, buffer_extent)
  
  # Get the coordinates for these cells
  cell_coords <- as.data.frame(xyFromCell(map, pixel_ID))
  
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
         fill = "Total connectivity")  
  
  #left_join the datasets together ####
  
  #Forage quality of woodland pixels and landscape nutritional quality
  df <- left_join(lc_quality_df, wood.edge.types, by = c("pixel_ID","x","y"))
  
  #Connectivity
  df <- left_join(df, connect_df, by=c("pixel_ID","x","y"))
  
  #MAX DAMS
  dams_scores <- dams_extraction%>%rename(max.dams.1000=max_dams)%>%
    st_drop_geometry()
  df <- left_join(df, dams_scores, by=c("pixel_ID","x","y"))
  
  #Nearest road
  df <- left_join(df, nearest_road_data, by=c("pixel_ID","x","y"))
  
  #Nearest urban and suburban
  df <- left_join(df, nearest_suburban_extraction, by=c("pixel_ID","x","y"))
  df <- left_join(df, nearest_urban_extraction, by=c("pixel_ID","x","y"))
  
  #Make column for nearest urban and suburban using minimum value
  df$urban_proximity <- pmin(df$nearest_suburban, df$nearest_urban)
  #Drop individual columns
  df <- df%>%dplyr::select(-c("nearest_suburban","nearest_urban"))
  
  #Linear features
  df <- left_join(df, lf_extraction,  by=c("pixel_ID","x","y"))
  
  #Remove the NAs
  df <- na.omit(df)
  
  #Plot the extracted data ####
  
  #Nearest_road
  ggplot(df) +
    geom_tile(aes(x = x, y = y, fill = nearest_road)) +
    scale_fill_viridis_c(option = "plasma") +  # Apply a continuous viridis color scale
    theme_minimal() +
    guides(fill = guide_colorbar(barwidth = 1, barheight = 10)) +  # Customize the colorbar appearance
    labs(title = "Distance to Nearest Road",
         fill = "Distance (m)")  
  
  #Urban proximity
  ggplot(df) +
    geom_tile(aes(x = x, y = y, fill = urban_proximity)) +
    scale_fill_viridis_c(option = "plasma") +  # Apply a continuous viridis color scale
    theme_minimal() +
    guides(fill = guide_colorbar(barwidth = 1, barheight = 10)) +  # Customize the colorbar appearance
    labs(title = "Distance to Nearest Urban Area",
         fill = "Distance (m)")  
  
  #Linear features
  
  ggplot(df) +
    geom_tile(aes(x = x, y = y, fill = lf)) +
    scale_fill_viridis_c(option = "plasma") +  # Apply a continuous viridis color scale
    theme_minimal() +
    guides(fill = guide_colorbar(barwidth = 1, barheight = 10)) +  # Customize the colorbar appearance
    labs(title = "Linear feature density",
         fill = "LF length within 1km")  
  
  #Nutritional landscape composition
  ggplot(df) +
    geom_tile(aes(x = x, y = y, fill = lc_forage_q)) +
    scale_fill_viridis_c(option = "plasma") +  # Apply a continuous viridis color scale
    theme_minimal() +
    guides(fill = guide_colorbar(barwidth = 1, barheight = 10)) +  # Customize the colorbar appearance
    labs(title = "Nutritional landscape composition",
         fill = "Weighted mean lc within 1km")  
  
  #Connectivity
  ggplot(df) +
    geom_tile(aes(x = x, y = y, fill = total_connect)) +
    scale_fill_viridis_c(option = "plasma") +  # Apply a continuous viridis color scale
    theme_minimal() +
    guides(fill = guide_colorbar(barwidth = 1, barheight = 10)) +  # Customize the colorbar appearance
    labs(title = "Connectivity",
         fill = "Total incoming connectivity")  
  
  #Woodland Forage quality
  ggplot(df) +
    geom_tile(aes(x = x, y = y, fill = woodland_q)) +
    scale_fill_viridis_c(option = "plasma") +  # Apply a continuous viridis color scale
    theme_minimal() +
    guides(fill = guide_colorbar(barwidth = 1, barheight = 10)) +  # Customize the colorbar appearance
    labs(title = "Pixel Forage quality",
         fill = "Forage Quality score")  
  
  #Max DAMS within 1km
  ggplot(df) +
    geom_tile(aes(x = x, y = y, fill = max.dams.1000)) +
    scale_fill_viridis_c(option = "plasma") +  # Apply a continuous viridis color scale
    theme_minimal() +
    guides(fill = guide_colorbar(barwidth = 1, barheight = 10)) +  # Customize the colorbar appearance
    labs(title = "Max DAMS within 1km",
         fill = "Max DAMS")  
  
  #CONVERT NUMERIC VALUES TO CATEGORICAL (LOW, MED, HIGH) ####
  #This is based on logic from google spreadsheet "BN thresholds" 
  
  #Look at distribution of values across national datasets
  #We need to decide at what point values change between categories
  #need to do this using whole datasets rather than localised cropped datasets.
  
  #Nearest roads
  #Proximity, therefore lower distance = higher category
  df$nearest_road <- ifelse(is.na(df$nearest_road), NA,
                            ifelse(df$nearest_road <= 100, "HIGH",
                                   ifelse(df$nearest_road > 100 & df$nearest_road <= 500, "MED",
                                          ifelse(df$nearest_road > 500, "LOW", NA))))
  
  #Linear features
  #hist(lf$lf_1000_mosaic_all_tiles_corrected)
  # Replace numeric values with categorical labels
  df$lf <- ifelse(df$lf<= 2500, "LOW",
                  ifelse(df$lf> 2500 & df$lf <= 5000, "MED",
                         ifelse(df$lf> 5000, "HIGH", NA)))
  
  #nearest urban/suburban
  #Proximity, therefore "HIGH" means lower value (lower distance)
  #These thresholds are based on deer biology rather than the spead of data
  #hist(nearest_urban$nearest_urban_raster_2015_all_tiles_corrected)
  #hist(nearest_suburb$nearest_suburban_raster_2015_all_tiles_corrected)
  df$urban_proximity <- ifelse(df$urban_proximity <= 500, "HIGH",
                               ifelse(df$urban_proximity >500 & df$urban_proximity <= 1000, "MED",
                                      ifelse(df$urban_proximity >1000,"LOW",NA)))  
  #MAX DAMS
  df$max.dams.1000 <- ifelse(df$max.dams.1000 <=15,"LOW",
                             ifelse(df$max.dams.1000>15 & df$max.dams.1000 <= 20, "MED",
                                    ifelse(df$max.dams.1000 >20, "HIGH",NA)))
  
  #nutritional landscape composition score
  df$lc_forage_q <- ifelse(df$lc_forage_q <= 4, "LOW",
                           ifelse(df$lc_forage_q >4 & df$lc_forage_q <=8, "MED",
                                  ifelse(df$lc_forage_q > 8, "HIGH",NA)))
  
  #woodland pixel quality score
  df$woodland_q <- ifelse(df$woodland_q<= 4, "LOW",
                          ifelse(df$woodland_q >4 & df$woodland_q <=8, "MED",
                                 ifelse(df$woodland_q>8, "HIGH",NA)))
  
  #connectivity
  #log transform connectivity values, they are too big for easy interpretation
  df$total_connect_log <- log(df$total_connect)
  df$total_connect_log <- ifelse(df$total_connect_log <=10, "LOW",
                                 ifelse(df$total_connect_log >10 & df$total_connect_log  <=15, "MED",
                                        ifelse(df$total_connect_log  > 15, "HIGH",NA)))
  
  #remove original connectivity column
  
  df <- df%>%dplyr::select(-c("total_connect"))
  
  df_categorical <- df
  
  df_categorical <- na.omit(df_categorical)
 
  #CREATE THE BAYESIAN BELIEF NETWORK ####
  
  #1. INITIALISE CONDITIONAL PROBABILITY TABLES ####
  
  #CPTS for measured nodes ####
  urban_prox_cpt<-matrix(c(0.333333,0.333333,0.333333), ncol=3, dimnames = list(NULL, c("LOW", "MED","HIGH")))
  
  nearest_road_cpt<-matrix(c(0.333333,0.333333,0.333333), ncol=3, dimnames = list(NULL, c("LOW", "MED","HIGH")))
  
  connectivity_cpt<-matrix(c(0.333333,0.333333,0.333333),ncol = 3,dimnames = list(NULL, c("LOW", "MED","HIGH")))
  
  landscape_nutritional_score_cpt<-matrix(c(0.333333,0.333333,0.333333),ncol = 3,dimnames = list(NULL, c("LOW", "MED","HIGH")))
  
  pixel_nutritional_score_cpt <- matrix(c(0.333333,0.333333,0.333333),ncol = 3,dimnames = list(NULL, c("LOW", "MED","HIGH")))
  
  dams_cpt <- landscape_nutritional_score_cpt<-matrix(c(0.333333,0.333333,0.333333),ncol = 3,dimnames = list(NULL, c("LOW", "MED","HIGH")))
  
  lf_density_cpt <-matrix(c(0.333333,0.333333,0.333333),ncol = 3,dimnames = list(NULL, c("LOW", "MED","HIGH")))
  
  #CPT for Disturbance Index ####
  
  disturb_index_cpt <- array(
    0,  # Default probability for each cell
    dim = c(3, 3, 3, 3),  # Shape of the array (3x3x3)
    dimnames = list(
      disturb_index = c("LOW", "MED", "HIGH"),
      nearest_road = c("LOW", "MED", "HIGH"),
      urban_prox = c("LOW","MED","HIGH"),
      connectivity = c("LOW", "MED", "HIGH")
    )
  )
  #order = disturb_index, nearest_road, urban_prox,connectivity
  
  #table1: urban_prox = LOW, connectivity = LOW
  disturb_index_cpt["LOW","LOW","LOW","LOW"] <- 1
  disturb_index_cpt["MED","MED","LOW","LOW"] <- 1
  disturb_index_cpt["HIGH","HIGH","LOW","LOW"] <- 1
  #table2:  urban_prox = MED, connectivity = LOW
  disturb_index_cpt["MED","LOW","MED","LOW"] <- 1
  disturb_index_cpt["MED","MED","MED","LOW"] <- 1
  disturb_index_cpt["HIGH","HIGH","MED","LOW"] <- 1
  #table3: urban_prox = HIGH, connectivity = LOW
  disturb_index_cpt["HIGH","LOW","HIGH","LOW"] <- 1
  disturb_index_cpt["HIGH","MED","HIGH","LOW"] <- 1
  disturb_index_cpt["HIGH","HIGH","HIGH","LOW"] <- 1
  #table4: urban_prox = LOW, connectivity = MED
  disturb_index_cpt["LOW","LOW","LOW","MED"] <- 1
  disturb_index_cpt["MED","MED","LOW","MED"] <- 1
  disturb_index_cpt["MED","HIGH","LOW","MED"] <- 1
  #table5: urban_prox = MED, connectivity = MED
  disturb_index_cpt["LOW","LOW","MED","MED"] <- 1
  disturb_index_cpt["MED","MED","MED","MED"] <- 1
  disturb_index_cpt["HIGH","HIGH","MED","MED"] <- 1
  #table6: urban_prox = HIGH, connectivity = MED
  disturb_index_cpt["HIGH","LOW","HIGH","MED"] <- 1
  disturb_index_cpt["HIGH","MED","HIGH","MED"] <- 1
  disturb_index_cpt["HIGH","HIGH","HIGH","MED"] <- 1
  #table7: urban_prox = LOW, connectivity = HIGH
  disturb_index_cpt["LOW","LOW","LOW","HIGH"] <- 1
  disturb_index_cpt["LOW","MED","LOW","HIGH"] <- 1
  disturb_index_cpt["MED","HIGH","LOW","HIGH"] <- 1
  #table8: urban_prox = MED, connectivity = HIGH
  disturb_index_cpt["MED","LOW","MED","HIGH"] <- 1
  disturb_index_cpt["MED","MED","MED","HIGH"] <- 1
  disturb_index_cpt["HIGH","HIGH","MED","HIGH"] <- 1
  #table9: urban_prox = HIGH, connectivity = HIGH
  disturb_index_cpt["MED","LOW","HIGH","HIGH"] <- 1
  disturb_index_cpt["MED","MED","HIGH","HIGH"] <- 1
  disturb_index_cpt["HIGH","HIGH","HIGH","HIGH"] <- 1
  
  #CPT for Nutritional Pixel Index for woodland pixels ####
  
  NPI_cpt <- array(
    0,  # Default probability for each cell
    dim = c(3, 3),  # Shape of the array (3x3)
    dimnames = list(
      NPI = c("LOW", "MED", "HIGH"),
      pixel_nutritional_score = c("LOW", "MED", "HIGH")
    )
  )
  
  NPI_cpt["LOW","LOW"] <- 1
  NPI_cpt["MED","MED"] <- 1
  NPI_cpt["HIGH","HIGH"] <- 1
  
  #CPT for Nutritional Landscape Index ####
  
  
  NLI_cpt <- array(
    0,  # Default probability for each cell
    dim = c(3, 3, 3),  # Shape of the array (3x3)
    dimnames = list(
      NLI = c("LOW", "MED", "HIGH"),
      landscape_nutritional_score = c("LOW", "MED", "HIGH"),
      lf_density = c("LOW","MED","HIGH")
    )
  )
  #order = NLI, landscape_nutritional_score, lf_density
  NLI_cpt["LOW","LOW","LOW"] <- 1
  NLI_cpt["MED","MED","LOW"] <- 1
  NLI_cpt["HIGH","HIGH","LOW"] <- 1
  NLI_cpt["LOW","LOW","MED"] <- 1
  NLI_cpt["HIGH","MED","MED"] <- 1
  NLI_cpt["HIGH","HIGH","MED"] <- 1
  NLI_cpt["MED","LOW","HIGH"] <- 1
  NLI_cpt["HIGH","MED","HIGH"] <- 1
  NLI_cpt["HIGH","HIGH","HIGH"] <- 1
  
  #CPT for Thermoregulation Index ####
  
  thermoreg_cpt <- array(
    0,  # Default probability for each cell
    dim = c(3, 3),  # Shape of the array
    dimnames = list(
      thermoreg_index = c("LOW", "MED", "HIGH"),
      DAMS_MAX = c("LOW", "MED", "HIGH")
    )
  )
  #order = DAMS_MAX, thermoreg_index
  thermoreg_cpt["LOW","LOW"] <- 1
  thermoreg_cpt["MED","MED"] <- 1
  thermoreg_cpt["HIGH","HIGH"] <- 1
  
  #Final CPT: deer damage risk
  
  damage_risk_cpt <- array(
    0,  # Default probability for each cell
    dim = c(5, 3, 3, 3, 3),  # Shape of the array
    dimnames = list(
      damage_risk = c("LOW","LOW-MED","MED","MED-HIGH","HIGH"),
      NPI = c("LOW", "MED", "HIGH"),
      NLI = c("LOW", "MED", "HIGH"),
      thermoreg_index = c("LOW", "MED", "HIGH"),
      disturb_index = c("LOW", "MED", "HIGH")
      
    )
  )
  #order = damage_risk, patch_quality,landscape_nutrition,thermoreg_index,disturbance
  
  #table1 landscape_nutrition = LOW, thermoreg_index = LOW, disturbance_index = LOW
  damage_risk_cpt["LOW-MED","LOW","LOW","LOW","LOW"] <- 1 
  damage_risk_cpt["LOW-MED","MED","LOW","LOW","LOW"] <- 1
  damage_risk_cpt["MED","HIGH","LOW","LOW","LOW"] <- 1
  #table2 landscape_nutrition = MED, thermoreg_index = LOW, disturbance_index = LOW
  damage_risk_cpt["LOW-MED","LOW","MED","LOW","LOW"] <- 1
  damage_risk_cpt["LOW-MED","MED","MED","LOW","LOW"] <- 1
  damage_risk_cpt["MED","HIGH","MED","LOW","LOW"] <- 1
  #table3 landscape_nutrition = HIGH, thermoreg_index = LOW, disturbance_index = LOW
  damage_risk_cpt["LOW-MED","LOW","HIGH","LOW","LOW"] <- 1
  damage_risk_cpt["MED","MED","HIGH","LOW","LOW"] <- 1
  damage_risk_cpt["MED","HIGH","HIGH","LOW","LOW"] <- 1
  #table4 landscape_nutrition = LOW, thermoreg_index = MED, disturbance_index = LOW
  damage_risk_cpt["MED","LOW","LOW","MED","LOW"] <- 1
  damage_risk_cpt["MED","MED","LOW","MED","LOW"] <- 1
  damage_risk_cpt["MED-HIGH","HIGH","LOW","MED","LOW"] <- 1
  #table5 landscape_nutrition = MED, thermoreg_index = MED, disturbance_index = LOW
  damage_risk_cpt["MED","LOW","MED","MED","LOW"] <- 1
  damage_risk_cpt["MED","MED","MED","MED","LOW"] <- 1
  damage_risk_cpt["MED-HIGH","HIGH","MED","MED","LOW"] <- 1
  #table6 landscape_nutrition = HIGH, thermoreg_index = MED, disturbance_index = LOW
  damage_risk_cpt["MED","LOW","HIGH","MED","LOW"] <- 1
  damage_risk_cpt["MED-HIGH","MED","HIGH","MED","LOW"] <- 1
  damage_risk_cpt["MED-HIGH","HIGH","HIGH","MED","LOW"] <- 1
  #table7  landscape_nutrition = LOW, thermoreg_index = HIGH, disturbance_index = LOW
  damage_risk_cpt["MED-HIGH","LOW","LOW","HIGH","LOW"] <- 1
  damage_risk_cpt["MED-HIGH","MED","LOW","HIGH","LOW"] <- 1
  damage_risk_cpt["HIGH","HIGH","LOW","HIGH","LOW"] <- 1
  #table8 landscape_nutrition = MED, thermoreg_index = HIGH, disturbance_index = LOW
  damage_risk_cpt["MED-HIGH","LOW","MED","HIGH","LOW"] <- 1
  damage_risk_cpt["MED-HIGH","MED","MED","HIGH","LOW"] <- 1
  damage_risk_cpt["HIGH","HIGH","MED","HIGH","LOW"] <- 1
  #table9 landscape_nutrition = HIGH, thermoreg_index = HIGH, disturbance_index = LOW
  damage_risk_cpt["MED-HIGH","LOW","HIGH","HIGH","LOW"] <- 1
  damage_risk_cpt["HIGH","MED","HIGH","HIGH","LOW"] <- 1
  damage_risk_cpt["HIGH","HIGH","HIGH","HIGH","LOW"] <- 1
  #table10 landscape_nutrition = LOW, thermoreg_index = LOW, disturbance_index = MED
  damage_risk_cpt["LOW","LOW","LOW","LOW","MED"] <- 1
  damage_risk_cpt["LOW","MED","LOW","LOW","MED"] <- 1
  damage_risk_cpt["LOW-MED","HIGH","LOW","LOW","MED"] <- 1
  #table11 landscape_nutrition = MED, thermoreg_index = LOW, disturbance_index = MED
  damage_risk_cpt["LOW-MED","LOW","MED","LOW","MED"] <- 1
  damage_risk_cpt["LOW-MED","MED","MED","LOW","MED"] <- 1
  damage_risk_cpt["MED","HIGH","MED","LOW","MED"] <- 1
  #table12 landscape_nutrition = HIGH, thermoreg_index = LOW, disturbance_index = MED
  damage_risk_cpt["LOW-MED","LOW","HIGH","LOW","MED"] <- 1
  damage_risk_cpt["MED","MED","HIGH","LOW","MED"] <- 1
  damage_risk_cpt["MED","HIGH","HIGH","LOW","MED"] <- 1
  #table13 landscape_nutrition = LOW, thermoreg_index = MED, disturbance_index = MED
  damage_risk_cpt["LOW-MED","LOW","LOW","MED","MED"] <- 1
  damage_risk_cpt["LOW-MED","MED","LOW","MED","MED"] <- 1
  damage_risk_cpt["MED","HIGH","LOW","MED","MED"] <- 1
  #table14 landscape_nutrition = MED, thermoreg_index = MED, disturbance_index = MED
  damage_risk_cpt["MED","LOW","MED","MED","MED"] <- 1
  damage_risk_cpt["MED","MED","MED","MED","MED"] <- 1
  damage_risk_cpt["MED-HIGH","HIGH","MED","MED","MED"] <- 1
  #table15  landscape_nutrition = HIGH, thermoreg_index = MED, disturbance_index = MED
  damage_risk_cpt["MED","LOW","HIGH","MED","MED"] <- 1
  damage_risk_cpt["MED-HIGH","MED","HIGH","MED","MED"] <- 1
  damage_risk_cpt["MED-HIGH","HIGH","HIGH","MED","MED"] <- 1
  #table16 landscape_nutrition = LOW, thermoreg_index = HIGH, disturbance_index = MED
  damage_risk_cpt["MED","LOW","LOW","HIGH","MED"] <- 1
  damage_risk_cpt["MED","MED","LOW","HIGH","MED"] <- 1
  damage_risk_cpt["MED","HIGH","LOW","HIGH","MED"] <- 1
  #table17 landscape_nutrition = MED, thermoreg_index = HIGH, disturbance_index = MED
  damage_risk_cpt["MED-HIGH","LOW","MED","HIGH","MED"] <- 1
  damage_risk_cpt["MED-HIGH","MED","MED","HIGH","MED"] <- 1
  damage_risk_cpt["HIGH","HIGH","MED","HIGH","MED"] <- 1
  #table18 landscape_nutrition = HIGH, thermoreg_index = HIGH, disturbance_index = MED
  damage_risk_cpt["MED-HIGH","LOW","HIGH","HIGH","MED"] <- 1
  damage_risk_cpt["HIGH","MED","HIGH","HIGH","MED"] <- 1
  damage_risk_cpt["HIGH","HIGH","HIGH","HIGH","MED"] <- 1
  #table19 landscape_nutrition = LOW, thermoreg_index = LOW, disturbance_index = HIGH
  damage_risk_cpt["LOW","LOW","LOW","LOW","HIGH"] <- 1
  damage_risk_cpt["LOW","MED","LOW","LOW","HIGH"] <- 1
  damage_risk_cpt["LOW","HIGH","LOW","LOW","HIGH"] <- 1
  #table20 landscape_nutrition = MED, thermoreg_index = LOW, disturbance_index = HIGH
  damage_risk_cpt["LOW","LOW","MED","LOW","HIGH"] <- 1
  damage_risk_cpt["LOW","MED","MED","LOW","HIGH"] <- 1
  damage_risk_cpt["LOW-MED","HIGH","MED","LOW","HIGH"] <- 1
  #table21 landscape_nutrition = HIGH, thermoreg_index = LOW, disturbance_index = HIGH
  damage_risk_cpt["LOW","LOW","HIGH","LOW","HIGH"] <- 1
  damage_risk_cpt["LOW-MED","MED","HIGH","LOW","HIGH"] <- 1
  damage_risk_cpt["LOW-MED","HIGH","HIGH","LOW","HIGH"] <- 1
  #table22 landscape_nutrition = LOW, thermoreg_index = MED, disturbance_index = HIGH
  damage_risk_cpt["LOW","LOW","LOW","MED","HIGH"] <- 1
  damage_risk_cpt["LOW","MED","LOW","MED","HIGH"] <- 1
  damage_risk_cpt["LOW","HIGH","LOW","MED","HIGH"] <- 1
  #table23 landscape_nutrition = MED, thermoreg_index = MED, disturbance_index = HIGH
  damage_risk_cpt["LOW","LOW","MED","MED","HIGH"] <- 1
  damage_risk_cpt["LOW","MED","MED","MED","HIGH"] <- 1
  damage_risk_cpt["LOW-MED","HIGH","MED","MED","HIGH"] <- 1
  #table24 landscape_nutrition = HIGH, thermoreg_index = MED, disturbance_index = HIGH
  damage_risk_cpt["LOW","LOW","HIGH","MED","HIGH"] <- 1
  damage_risk_cpt["LOW-MED","MED","HIGH","MED","HIGH"] <- 1
  damage_risk_cpt["LOW-MED","HIGH","HIGH","MED","HIGH"] <- 1
  #table25  landscape_nutrition = LOW, thermoreg_index = HIGH, disturbance_index = HIGH
  damage_risk_cpt["LOW-MED","LOW","LOW","HIGH","HIGH"] <- 1
  damage_risk_cpt["LOW-MED","MED","LOW","HIGH","HIGH"] <- 1
  damage_risk_cpt["LOW-MED","HIGH","LOW","HIGH","HIGH"] <- 1
  #table26 landscape_nutrition = MED, thermoreg_index = HIGH, disturbance_index = HIGH
  damage_risk_cpt["LOW-MED","LOW","MED","HIGH","HIGH"] <- 1
  damage_risk_cpt["LOW-MED","MED","MED","HIGH","HIGH"] <- 1
  damage_risk_cpt["MED","HIGH","MED","HIGH","HIGH"] <- 1
  #table27 landscape_nutrition = HIGH, thermoreg_index = HIGH, disturbance_index = HIGH
  damage_risk_cpt["LOW-MED","LOW","HIGH","HIGH","HIGH"] <- 1
  damage_risk_cpt["MED","MED","HIGH","HIGH","HIGH"] <- 1
  damage_risk_cpt["MED","HIGH","HIGH","HIGH","HIGH"] <- 1
  
  #CREATE BBN STRUCTURE ####
  
  # Step 1: Explicitly define the nodes in the network
  nodes <- c(
    "disturb_index",
    "urban_prox",
    "nearest_road",
    "connectivity",
    "pixel_nutritional_score",
    "landscape_nutritional_score",
    "NLI",
    "lf_density",
    "thermoreg_index",
    "DAMS_MAX",
    "damage_risk",
    "NPI"
  )
  
  #empty graph
  e = empty.graph(nodes)
  
  arc.set = matrix(c("urban_prox", "disturb_index",
                     "nearest_road", "disturb_index", 
                     "connectivity", "disturb_index",
                     "lf_density","NLI",
                     "landscape_nutritional_score","NLI",
                     "pixel_nutritional_score","NPI",
                     "DAMS_MAX","thermoreg_index",
                     "disturb_index","damage_risk",
                     "NLI","damage_risk",
                     "NPI","damage_risk",
                     "thermoreg_index","damage_risk"),
                   ncol = 2, byrow = TRUE,
                   dimnames = list(NULL, c("from", "to")))
  
  arcs(e) <- arc.set
  
  model_string <- modelstring(e) 
  
  net<-model2network(model_string) 
  # You dont need to repeat the names of the nodes with parents (child nodes) 
  
  # Custom fitting network (matching up the nodes to their CPTs)
  dfit = custom.fit(net, dist = list(nearest_road=nearest_road_cpt, 
                                     urban_prox=urban_prox_cpt,
                                     connectivity=connectivity_cpt,
                                     lf_density=lf_density_cpt,
                                     DAMS_MAX=dams_cpt,
                                     pixel_nutritional_score=pixel_nutritional_score_cpt,
                                     landscape_nutritional_score=landscape_nutritional_score_cpt, 
                                     disturb_index=disturb_index_cpt, 
                                     NPI=NPI_cpt,
                                     NLI=NLI_cpt,
                                     thermoreg_index=thermoreg_cpt,
                                     damage_risk=damage_risk_cpt))
  
  #Plot BN structure
  graphviz.plot(net)
  
  #Check parameters
  dfit
  
  #MAKE PREDICTIONS FROM BBN ####
  
  predict_dat <- df_categorical %>%st_drop_geometry() %>% #drop geometry
    mutate(across(where(is.character), toupper)) %>% #convert characters to upper case
    dplyr::select(-c("pixel_ID","x","y"))
  #Ensure variable names match those in BBN
  predict_dat<-predict_dat%>%rename(
    nearest_road=nearest_road,
    lf_density=lf,
    urban_prox=urban_proximity,
    landscape_nutritional_score=lc_forage_q,
    pixel_nutritional_score=woodland_q,
    DAMS_MAX=max.dams.1000,
    connectivity=total_connect_log)
  
  #Make empty columns for the latent (unobserved) variables
  
  predict_dat$NPI <- NA
  predict_dat$NLI <- NA
  predict_dat$disturb_index <- NA
  predict_dat$thermoreg_index <- NA
  predict_dat$damage_risk <- NA
  
  #Ensure all columns are factors, not characters
  predict_dat <- predict_dat %>% mutate_all(as.factor)
  
  #Ensure factor levels are in correct order
  levels(predict_dat$nearest_road)<-c("LOW", "MED", "HIGH")
  levels(predict_dat$landscape_nutritional_score)<-c("LOW", "MED", "HIGH")
  levels(predict_dat$pixel_nutritional_score)<-c("LOW", "MED", "HIGH")
  levels(predict_dat$urban_prox)<-c("LOW", "MED", "HIGH")
  levels(predict_dat$lf_density)<-c("LOW", "MED", "HIGH")
  levels(predict_dat$DAMS_MAX)<-c("LOW", "MED", "HIGH")
  levels(predict_dat$connectivity)<-c("LOW", "MED", "HIGH")
  levels(predict_dat$NPI)<-c("LOW", "MED", "HIGH")
  levels(predict_dat$NLI)<-c("LOW", "MED", "HIGH")
  levels(predict_dat$disturb_index)<-c("LOW", "MED", "HIGH")
  levels(predict_dat$thermoreg_index)<-c("LOW", "MED", "HIGH")
  levels(predict_dat$damage_risk)<-c("LOW","LOW-MED","MED","MED-HIGH","HIGH")
  
  #Ensure predict_data is a data.frame
  predict_dat <- as.data.frame(predict_dat)
  
  #Predict values for latent variables
  pred_nli = predict(dfit,node="NLI",data=predict_dat, method = "bayes-lw",from=c("landscape_nutritional_score","lf_density"))
  pred_npi = predict(dfit,node="NPI",data=predict_dat, method = "bayes-lw", from = "pixel_nutritional_score")
  pred_thermoreg = predict(dfit,node="thermoreg_index",data=predict_dat, method = "bayes-lw", from="DAMS_MAX")
  pred_disturb = predict(dfit,node="disturb_index",data=predict_dat, method = "bayes-lw",from=c("nearest_road","urban_prox","connectivity"))
  
  #fill the columns
  predict_dat$NLI <- pred_nli
  predict_dat$NPI <- pred_npi
  predict_dat$thermoreg_index <- pred_thermoreg
  predict_dat$disturb_index <- pred_disturb
  
  #Predict damage
  pred_damage = predict(dfit,node = "damage_risk",data = predict_dat,method = "parents")
  #method = "bayes-lw"
  predict_dat$damage_risk <- pred_damage
  
  #Add damage risk back into spatial dataset
  
  df_categorical$pred_damage <- pred_damage
  df_categorical$pred_NLI <- pred_nli
  df_categorical$pred_npi <- pred_npi
  df_categorical$pred_thermoreg <- pred_thermoreg
  df_categorical$pred_disturb <- pred_disturb
  
  #DRAW MAPS ####
  
  # Make sf object
  df_sf <- st_as_sf(df_categorical, coords = c("x", "y"), crs = st_crs(bng))
  
  # Define the value mapping for pred_damage
  damage_values <- c("LOW" = 1, "LOW-MED" = 2, "MED" = 3, "MED-HIGH" = 4, "HIGH" = 5)
  df_sf$value <- damage_values[df_sf$pred_damage]
  
  # Rasterize
  raster_data <- stars::st_rasterize(df_sf %>% dplyr::select(value, geometry))
  
  # Get last 5 colours from YIOrRd color palette
  color_palette <- c("LOW" = "yellow", "LOW-MED" = "#FED976", "MED" = "#FD8D3C", "MED-HIGH" = "#FC4E2A", "HIGH" = "#E31A1C")
  
  
  # Extract the i-th area
  area <- buffered_woods_7km$geometry
  
  cropped_data <- st_crop(raster_data, area)
  
  # Convert the cropped data to a data frame for ggplot2
  cropped_df <- as.data.frame(cropped_data, xy = TRUE)
  
  # Create a factor for the damage categories
  cropped_df$value <- factor(cropped_df$value, levels = 1:5, labels = names(damage_values))
  
  # Replace NAs with 0
  cropped_df <- cropped_df %>%
    filter(!is.na(value))
  
  # Plot using ggplot2
  plot_title <- buffered_woods_7km$Region
  
  plot <-ggplot(cropped_df) +
    geom_raster(aes(x = x, y = y, fill = value)) +
    scale_fill_manual(
      values = color_palette, 
      name = "Deer Impact Risk",
      breaks = names(damage_values),  # Ensure all levels are shown in the legend
      labels = names(damage_values)
    ) +
    theme_minimal() +
    coord_equal() +
    labs(
      title = plot_title,
      x = "Longitude",
      y = "Latitude"
    )
  
  # Save each plot to a separate PDF file with 300 dpi resolution
  output_dir <- here("output/figures")
  dpi <- 300  # Resolution in dots per inch
  width <- 6  # Width of the PDF in inches
  height <- 6  # Height of the PDF in inches
  
  # Save the plot as a 300 dpi PDF
  ggsave(
    filename = "DI_risk", 
    plot = plot, 
    device = "pdf",  # Specify the file format
    width = width, 
    height = height, 
    dpi = dpi  # Set the resolution
  )
  
  #ADD MAP TO MAPVIEW BACKDROP ####
  
  mapview(cropped_df, xcol="x",ycol="y")
}