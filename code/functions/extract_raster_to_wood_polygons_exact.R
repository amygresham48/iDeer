extract_raster_to_wood_polygons_exact <- function(risk_map, tiles, wood_polys_chunked) {
  
  extraction_results <- vector("list", nrow(tiles))
  problematic_tiles <- c()
  
  pb <- progress_bar$new(
    format = "  Extracting [:bar] :percent | Elapsed: :elapsed | ETA: :eta",
    total = nrow(tiles),
    clear = FALSE,
    width = 60
  )
  
  for (i in seq_len(nrow(tiles))) {
    pb$tick()
    
    tile <- tiles[i, ]
    tile_buf <- st_buffer(tile, 2000)
    
    # Convert tile buffer to terra vector (SpatVector)
    tile_buf_vect <- terra::vect(tile_buf)
    
    # Get cell indices overlapping buffered tile (terra approach)
    cells_idx <- terra::cells(risk_map, tile_buf_vect, cells=TRUE)
    
    if (length(cells_idx) == 0) {
      message(paste("Skipping tile", i, "- no raster cells overlap"))
      next
    }
    
    # Subset raster to those cells (terra::subset by cell index)
    cropped_rast_subset <- terra::subset(risk_map, cells_idx)
    
    wood_polys_tile <- wood_polys_chunked[[i]]
    if (is.null(wood_polys_tile) || nrow(wood_polys_tile) == 0) {
      next
    }
    
    # exact_extract requires raster as Raster* or terra SpatRaster - 
    # convert terra SpatRaster to RasterLayer for exactextractr compatibility
    raster_for_exact <- raster::raster(cropped_rast_subset)
    
    # exact_extract returns a list of data frames, one per polygon
    # Calculate mean values for each polygon
    tryCatch({
      values_list <- exactextractr::exact_extract(raster_for_exact, wood_polys_tile, 'mean', progress = FALSE)
    }, error = function(e) {
      message(paste("Tile", i, "error during exact_extract:", e$message))
      problematic_tiles <<- c(problematic_tiles, i)
      return(NULL)
    }) -> values_list
    
    if (is.null(values_list)) {
      next
    }
    
    # Combine values into a data frame with patch_ID and mean_risk
    risk_map_poly_vals <- data.frame(
      patch_ID = wood_polys_tile$patch_ID,
      mean_risk = unlist(values_list)
    )
    
    # Join geometry back
    merged_df <- left_join(risk_map_poly_vals, wood_polys_tile, by = "patch_ID") %>%
      filter(!st_is_empty(geometry)) %>%
      st_as_sf()
    
    extraction_results[[i]] <- merged_df
  }
  
  result_df <- dplyr::bind_rows(extraction_results)
  
  if (length(problematic_tiles) > 0) {
    message("Problematic tiles:", paste(problematic_tiles, collapse = ", "))
  }
  
  return(list(result = result_df, problematic_tiles = problematic_tiles))
}