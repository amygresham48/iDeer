extract_raster_to_wood_polygons_exact <- function(risk_map, tiles, wood_polys_chunked, output_dir) {
  
  crs_bng <- "EPSG:27700"
  
  extraction_results <- list()
  problematic_tiles <- c()
  
  pb <- progress_bar$new(
    format = "  Extracting [:bar] :percent | Elapsed: :elapsed | ETA: :eta",
    total = nrow(tiles),
    clear = FALSE,
    width = 60
  )
  
  # Create output directory if it doesn't exist
  for (i in seq_len(nrow(tiles))) {
    pb$tick()
    
    tile <- tiles[i, ]
    tile_id <- tile$tile_name
    
    output_file <- file.path(output_dir, paste0("wood_poly_extraction_", tile_id, ".rds"))
    
    if (file.exists(output_file)) {
      message(paste("Skipping tile", tile_id, "- RDS file already exists"))
      next
    }
    
    if (!tile_id %in% names(wood_polys_chunked)) {
      message(paste("Skipping tile", tile_id, "- no wood_polys_chunked entry"))
      next
    }
    
    wood_polys_tile <- wood_polys_chunked[[tile_id]]
    if (is.null(wood_polys_tile) || nrow(wood_polys_tile) == 0) {
      message(paste("Skipping tile", tile_id, "- no polygons"))
      next
    }
    
    tile_buf <- st_buffer(tile, 2000)
    tile_buf_vect <- terra::vect(tile_buf)
    
    cells_idx <- terra::cells(risk_map, tile_buf_vect)
    if (length(cells_idx) == 0) {
      message(paste("Skipping tile", tile_id, "- no raster cells overlap"))
      next
    }
    
    cropped_rast_subset <- terra::subset(risk_map, cells_idx)
    wood_polys_tile <- st_as_sf(wood_polys_tile)
    
    # Assign or transform CRS if needed
    if (is.na(st_crs(wood_polys_tile))) {
      message(paste("Tile", tile_id, "- wood_polys_tile CRS missing, assigning crs_bng"))
      st_crs(wood_polys_tile) <- crs_bng
    } else if (st_crs(wood_polys_tile)$epsg != 27700) {
      message(paste("Tile", tile_id, "- wood_polys_tile CRS differs, transforming to crs_bng"))
      wood_polys_tile <- st_transform(wood_polys_tile, crs_bng)
    }
    
    # Validate presence of patch_ID column
    if (!"patch_ID" %in% names(wood_polys_tile)) {
      stop(paste("Tile", tile_id, "- wood_polys_tile missing patch_ID column"))
    }
    if (any(duplicated(wood_polys_tile$patch_ID))) {
      stop(paste("Tile", tile_id, "- wood_polys_tile has duplicated patch_ID values"))
    }
    
    raster_for_exact <- raster::raster(cropped_rast_subset)
    raster::crs(raster_for_exact) <- crs_bng
    
    values_list <- tryCatch({
      exactextractr::exact_extract(raster_for_exact, wood_polys_tile, 'mean', progress = FALSE)
    }, error = function(e) {
      message(paste("Tile", tile_id, "error during exact_extract:", e$message))
      problematic_tiles <<- c(problematic_tiles, tile_id)
      NULL
    })
    
    if (is.null(values_list)) {
      next
    }
    
    risk_map_poly_vals <- data.frame(
      patch_ID = wood_polys_tile$patch_ID,
      mean_risk = unlist(values_list)
    )
    
    merged_df <- dplyr::left_join(risk_map_poly_vals, wood_polys_tile, by = "patch_ID") %>%
      st_as_sf() %>%
      filter(!st_is_empty(geom)) %>%
      mutate(tile_ID = tile_id)
    
    saveRDS(merged_df, file = output_file)
    
    extraction_results[[tile_id]] <- merged_df
  }
  
  result_df <- dplyr::bind_rows(extraction_results, .id = "tile_ID")  # keep track of tile IDs in result
  
  if (length(problematic_tiles) > 0) {
    message("Problematic tiles: ", paste(problematic_tiles, collapse = ", "))
  }
  
  return(list(result = result_df, problematic_tiles = problematic_tiles))
}
