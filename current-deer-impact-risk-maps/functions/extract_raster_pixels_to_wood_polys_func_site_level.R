extract_raster_to_wood_polygons <- function(risk_map, site_woods_unmerged) {
  
  bng <- 27700
  
  risk_map_extent_sf <- st_as_sf(as(extent(risk_map), "SpatialPolygons"))
  st_crs(risk_map_extent_sf) <- bng
  site_woods_unmerged <- st_filter(site_woods_unmerged, risk_map_extent_sf)
  
  #will tell me how many of each pixel is in each polygon
  risk_map_poly_vals <- raster::extract(risk_map, site_woods_unmerged, df = TRUE, progress = "text")
  
  #get row number and patch_ID from
  wood_site_IDs <- data.frame(ID = rep(seq_len(nrow(site_woods_unmerged))),
                              patch_ID = site_woods_unmerged$patch_ID)
  # Get name of map
  raster_name <- deparse(substitute(risk_map))
  
  # Create a data frame with the extracted values and patch_IDs
  vals <- data.frame(
    ID = risk_map_poly_vals$ID,
    raster_values = risk_map_poly_vals$category  # Extracted raster values
  )
  
  #left_join the patch_IDs to this df
  
  vals <- left_join(vals, wood_site_IDs, by = c("ID"))
  vals <- vals %>% dplyr::select(-c("ID"))
  
  # Filter out NA values
  vals <- vals[!is.na(vals$raster_values), ]
  
  # Rename raster_values column to the map name
  names(vals)[names(vals) == "raster_values"] <- raster_name
  
  return(vals)
}
