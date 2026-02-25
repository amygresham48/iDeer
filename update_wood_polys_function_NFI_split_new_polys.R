#Function to add new woodland polygon(s) to existing woodland polygon datasets
#The output from this function will then be used to update the small and large deer risk maps

update_wood_polys_nfi <-function(wood_polys, #new woodland polygon(s)
                             sitebuf, #user's landscape extent
                             lcm, #CEH GB land cover map 2023
                             #existing_wood_polys_merged, #merged woodland polygons
                             existing_wood_polys_unmerged, #unmerged woodland polygons
                             nfi, #raw NFI 2023 dataset
                             nfi_lcm_2023_polys, #LCM polys overlapping NFI for final raster extraction
                             template_raster #woods binary map cropped to area of interest       
){
  
  #Create buffers to use for spatial layer updates----####
  buffered_woods_3km <- st_buffer(sitebuf, dist = 3000)
  st_crs(buffered_woods_3km) <- 27700
  
  plot(buffered_woods_3km$geometry)
  plot(sitebuf$geometry,add=TRUE)
  
  #Split the new woodland polygons up into equally sized parcels----####
  #This will ensure that variation in risk can be observed for larger woodland patches
  
  #Find patch area 
  
  #split polygons into equal sized chunks
  #Minimum chunk size = 100000m2 (10 ha)
  split_poly_min_area <- function(sf_poly, min_area = 25000) {
    # Calculate the total area of the polygon
    total_area <- st_area(sf_poly)
    
    # Determine the number of desired pieces
    n_areas <- ceiling(as.numeric(total_area / min_area))
    
    if (n_areas <= 1) {
      # If the polygon is already smaller than the minimum area, return it as is
      sf_poly$Shape_Area <- total_area
      return(sf_poly)
    }
    
    # Create random points
    points_rnd <- st_sample(sf_poly, size = 10000)
    
    # k-means clustering
    points <- do.call(rbind, st_geometry(points_rnd)) %>%
      as_tibble() %>% setNames(c("lon", "lat"))
    k_means <- kmeans(points, centers = n_areas)
    
    # Create voronoi polygons
    voronoi_polys <- dismo::voronoi(k_means$centers, ext = sf_poly)
    
    # Clip to sf_poly
    crs(voronoi_polys) <- crs(sf_poly)
    voronoi_sf <- st_as_sf(voronoi_polys)
    equal_areas <- st_intersection(voronoi_sf, sf_poly)
    equal_areas$Shape_Area <- st_area(equal_areas)
    
    return(equal_areas)
  }
  
  if (nrow(wood_polys) > 0) {
    # Initialize an empty list to store the results
    all_split_results <- list()
    
    # Iterate through each row (polygon) in wood_polys
    for (i in 1:nrow(wood_polys)) {
      # Check if Shape_Area is <= 50000
      if (as.numeric(st_area(wood_polys[i, ])) <= 25000) {
        # Skip this iteration if Shape_Area is too small
        all_split_results[[i]] <- wood_polys[i, ] # keep the original polygon
        next
      }
      
      split_result <- split_poly_min_area(wood_polys[i, ]) # Process each polygon individually
      all_split_results[[i]] <- split_result
    }
    
    # Combine the results into a single sf object
    print("all_split_results")
    print(all_split_results)
    # Remove 'id' column from each element if it exists
    all_split_results <- lapply(all_split_results, function(x) {
      if ("id" %in% colnames(x)) {
        x <- x[, !names(x) %in% "id"]
      }
      return(x)
    })
    combined_results <- do.call(rbind, all_split_results)
    
    # Print the combined results
    print(combined_results)
  } else {
    print("wood_polys is empty")
  }
  
  #results_select <- combined_results %>% dplyr::select(-c("id"))
  #wood_polys_unmerged <- results_select
  
  wood_polys_unmerged <- combined_results
  wood_polys_merged <- wood_polys
  
  #Rasterize the new woodland polygons and add them to the CEH LCM ####-------------------
  #So that we can get updated arable and perennial cover
  
  #Crop lcm to site buffer
  
  lcm_cropped <- crop(lcm, buffered_woods_3km)
  plot(lcm_cropped)
  
  # Reclassify the woodland_type into numeric values
  #As we are not distinguishing between conifer and broadleaf, give them all a score of 1
  #1 = broadleaf in CEH LCM
  wood_polys_merged$raster_value <- 1
  wood_polys_unmerged$raster_value <- 1
  
  # Rasterize the polygons
  raster_polys <- rasterize(wood_polys_merged, lcm_cropped, field = "raster_value")
  ##set zero values to NA
  #values(raster_polys)[values(raster_polys) <= 0] = NA
  
  # Set the data type to 2-byte signed integer
  dataType(raster_polys) <- "INT2S"
  
  # Overlay function: replace values in lcm_cropped with non-NA values from raster_polys
  lcm_updated <- overlay(lcm_cropped, raster_polys, fun = function(lcm, wood) {
    ifelse(!is.na(wood), wood, lcm)  # If woodland_raster has a non-NA value, use it; otherwise keep lcm map value
  })
  
  plot(lcm_updated)
  
  # Set the data type to 2-byte signed integer
  dataType(lcm_updated) <- "INT2S"
  
  # OPTIONAL, but good practice for categorical data:
  lcm_updated <- ratify(lcm_updated) 
  # The ratify() function creates a Raster Attribute Table (RAT), 
  # which explicitly tells R and other software that the data is categorical.
  
  print("dataType lcm_cropped")
  print(dataType(lcm_cropped))
  print("dataType raster_polys")
  print(dataType(raster_polys))
  print("dataType lcm_updated")
  print(dataType(lcm_updated))
  
  # Compare all essential geometric properties
  # The output will be TRUE if they match perfectly.
  alignment_check <- compareRaster(
    x = lcm_cropped, 
    y = lcm_updated,
    extent = TRUE,  # Compare bounding boxes (xmin, xmax, ymin, ymax)
    rowcol = TRUE,  # Compare number of rows and columns (implies resolution match)
    crs = TRUE,     # Compare Coordinate Reference System
    orig = TRUE,    # Compare the origin (the coordinates of the center of the upper-left cell)
    stopiffalse = FALSE, # Set to FALSE to get a logical result instead of an error
    showwarning = TRUE   # Show a warning describing the differences if they exist
  )
  
  print("Alignment check")
  print(alignment_check)
  
  #make binary woodland raster

  reclass_woods <- data.frame(is = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,
                                     18,19,20,21), becomes = c(1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0))

  woods_binary <- reclassify(lcm_updated, reclass_woods)
  plot(woods_binary)
  
  #--------------------------------------------Add new woodlands to GB woodland polygon datasets ------------------------------------------------------------####
 

  #Unmerged polygons ------------------------------------------------------------------------------------------------#####
  # existing_wood_polys_unmerged  <- existing_wood_polys_unmerged %>%
  #   mutate(
  #     #patch_ID = 1:nrow(existing_wood_polys_unmerged),
  #     Shape_Area = as.numeric(st_area(geometry))
  #   )
  
  # Get the maximum OBJECTID in complete NFI dataset
  max_OBJECTID <- as.numeric(max(nfi$OBJECTID))
  
  # Modify wood_polys and assign patch_IDs that continue from max_patch_id
  wood_polys_unmerged <- wood_polys_unmerged %>%
    select(-c("raster_value")) %>%
    mutate(Shape_Area = as.numeric(st_area(geometry))) %>%
    mutate(OBJECTID = max_OBJECTID + 1:nrow(wood_polys_unmerged))
  
  #Now, crop existing_wood_polys_unmerged to extent of user's landscape
  site_woods_unmerged <- st_filter(existing_wood_polys_unmerged,buffered_woods_3km, .predicate = st_intersects)
  site_woods_unmerged <- st_as_sf(site_woods_unmerged)
  site_woods_unmerged <- site_woods_unmerged %>%
    mutate(Shape_Area = as.numeric(st_area(geometry)))
  
  # Find the intersection between existing polygons and the new polygon
  overlap <- st_intersection(site_woods_unmerged, wood_polys_unmerged)
  
  # Subtract the overlapping area from the existing polygons
  existing_unmerged_polygons_no_overlap <- st_difference(site_woods_unmerged, st_union(wood_polys_unmerged))

  #Remove group_ID column
  existing_unmerged_polygons_no_overlap <- existing_unmerged_polygons_no_overlap %>% select(-c("group_ID"))
  
  #Column names should now match for rbinding
  print("existing_unmerged_polygons_no_overlap:")
  print(existing_unmerged_polygons_no_overlap)
  print("wood_polys_unmerged:")
  print(wood_polys_unmerged)
  
  existing_unmerged_polygons_no_overlap <- existing_unmerged_polygons_no_overlap %>%
    select(-c("assigned","tile_ID","patch_ID","id"))
  
  # existing_unmerged_polygons_no_overlap <- existing_unmerged_polygons_no_overlap %>%
  #   select(-c("assigned","tile_ID","patch_ID"))
  
  print("existing_unmerged_polygons_no_overlap")
  print(existing_unmerged_polygons_no_overlap)
  
  wood_polys_unmerged<- wood_polys_unmerged %>% select(-c("MEAN"))
  
  print("wood_polys_unmerged")
  print(wood_polys_unmerged)
  
  # Bind the new polygon with the modified existing polygons
  combined_unmerged_polygons <- rbind(existing_unmerged_polygons_no_overlap, wood_polys_unmerged)
  
  #Ensure shape area is correct
  
  combined_unmerged_polygons <- combined_unmerged_polygons %>%
    mutate(
      Shape_Area = as.numeric(st_area(geometry))
    )
  
  site_woods_unmerged <- combined_unmerged_polygons
  rm(combined_unmerged_polygons)
  plot(site_woods_unmerged$geometry)
  
  #Now, crop existing_wood_polys_unmerged to extent of user's landscape
  nfi_lcm_2023_polys_site<- st_filter(nfi_lcm_2023_polys,buffered_woods_3km, .predicate = st_intersects)
  nfi_lcm_2023_polys_site <- st_as_sf(nfi_lcm_2023_polys_site)
  nfi_lcm_2023_polys_site <- nfi_lcm_2023_polys_site %>%
    mutate(Shape_Area = as.numeric(st_area(geometry)))
  
  # Find the intersection between existing polygons and the new polygon
  overlap <- st_intersection(nfi_lcm_2023_polys_site, wood_polys_unmerged)
  
  # Only continue if overlap has features
  if (nrow(overlap) > 0) {
    # Explode geometry collections into individual parts
    overlap_parts <- overlap %>%
      st_collection_extract("POLYGON", warn = FALSE)
    
    # Remove empty geometries if any
    overlap_parts_valid <- overlap_parts %>%
      filter(!st_is_empty(.))
    
    # Subset out linestrings and points and convert all to POLYGON
    overlap_parts_valid_polys_only <- overlap_parts_valid %>%
      filter(st_geometry_type(.) %in% c("POLYGON", "MULTIPOLYGON")) %>%
      st_cast("POLYGON")
    
    # Subtract the overlapping area from the existing polygons
    nfi_lcm_woods_no_overlap <- st_difference(nfi_lcm_2023_polys_site, st_union(overlap_parts_valid_polys_only))
    
  } else {
    # If no overlap, just copy the original polygons
    nfi_lcm_woods_no_overlap <- nfi_lcm_2023_polys_site
    message("No overlapping woods, going on....")
  }
  
  #Remove group_ID column
  #nfi_lcm_woods_no_overlap <- nfi_lcm_woods_no_overlap %>% select(-c("group_ID"))
  
  #Column names should now match for rbinding
  print("nfi_lcm_woods_no_overlap:")
  print(nfi_lcm_woods_no_overlap)
  print("wood_polys_unmerged:")
  print(wood_polys_unmerged)
  
  wood_polys_unmerged <- wood_polys_unmerged %>% dplyr::select(Shape_Area, geometry)
  
  #nfi_lcm_woods_no_overlap <- nfi_lcm_woods_no_overlap %>% dplyr::select(c(Shape_Area, patch_ID, geometry)) #%>%
  nfi_lcm_woods_no_overlap <- nfi_lcm_woods_no_overlap %>% dplyr::select(c(Shape_Area, geometry)) #%>%
  
    #rename(geometry = geom)
  
  #Add a column showing if polygon is a new woodland
  nfi_lcm_woods_no_overlap$new_woodland <- "No"
  wood_polys_unmerged$new_woodland <- "Yes"
  
  # Bind the new polygon(s) with the modified existing polygons
  nfi_lcm_unmerged_polys <- rbind(nfi_lcm_woods_no_overlap, wood_polys_unmerged)
  
  nfi_lcm_unmerged_polys$patch_ID <- 1:nrow(nfi_lcm_unmerged_polys)
  
  #Ensure shape area is correct
  
  nfi_lcm_unmerged_polys <- nfi_lcm_unmerged_polys %>%
    mutate(
      Shape_Area = as.numeric(st_area(geometry))
    )
  
  #return site_woods_merged and site_woods_unmerged
  
return(list(buffered_woods_3km = buffered_woods_3km,
            lcm_updated = lcm_updated, 
            woods_binary=woods_binary,
            site_woods_unmerged = site_woods_unmerged,
            nfi_lcm_unmerged_polys = nfi_lcm_unmerged_polys))
  

}