extract_raster_to_wood_polygons_exact <- function(risk_map, tiles, wood_polys_chunked, output_dir) {
  
  crs_bng <- "EPSG:27700"
  
  extraction_results <- vector("list", nrow(tiles))
  problematic_tiles <- c()
  
  pb <- progress_bar$new(
    format = "  Extracting [:bar] :percent | Elapsed: :elapsed | ETA: :eta",
    total = nrow(tiles),
    clear = FALSE,
    width = 60
  )
  
  # Create output directory if it doesn't exist
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  for (i in seq_len(nrow(tiles))) {
    pb$tick()
    
    tile <- tiles[i, ]
    tile_buf <- st_buffer(tile, 2000)
    
    tile_buf_vect <- terra::vect(tile_buf)
    
    cells_idx <- terra::cells(risk_map, tile_buf_vect)
    
    if (length(cells_idx) == 0) {
      message(paste("Skipping tile", i, "- no raster cells overlap"))
      next
    }
    
    cropped_rast_subset <- terra::subset(risk_map, cells_idx)
    
    wood_polys_tile <- wood_polys_chunked[[i]]
    if (is.null(wood_polys_tile) || nrow(wood_polys_tile) == 0) {
      message(paste("Skipping tile", i, "- no polygons"))
      next
    }
    
    wood_polys_tile <- st_as_sf(wood_polys_tile)
    
    if (is.na(st_crs(wood_polys_tile))) {
      message(paste("Tile", i, "- wood_polys_tile CRS missing, assigning crs_bng"))
      st_crs(wood_polys_tile) <- crs_bng
    } else if (st_crs(wood_polys_tile)$epsg != 27700) {
      message(paste("Tile", i, "- wood_polys_tile CRS differs, transforming to crs_bng"))
      wood_polys_tile <- st_transform(wood_polys_tile, crs_bng)
    }
    
    if (!"patch_ID" %in% names(wood_polys_tile)) {
      stop(paste("Tile", i, "- wood_polys_tile missing patch_ID column"))
    }
    if (any(duplicated(wood_polys_tile$patch_ID))) {
      stop(paste("Tile", i, "- wood_polys_tile has duplicated patch_ID values"))
    }
    
    raster_for_exact <- raster::raster(cropped_rast_subset)
    raster::crs(raster_for_exact) <- crs_bng
    
    values_list <- tryCatch({
      exactextractr::exact_extract(raster_for_exact, wood_polys_tile, 'mean', progress = FALSE)
    }, error = function(e) {
      message(paste("Tile", i, "error during exact_extract:", e$message))
      problematic_tiles <<- c(problematic_tiles, i)
      NULL
    })
    
    if (is.null(values_list)) {
      next
    }
    
    risk_map_poly_vals <- data.frame(
      patch_ID = wood_polys_tile$patch_ID,
      mean_risk = unlist(values_list)
    )
    
    merged_df <- left_join(risk_map_poly_vals, wood_polys_tile, by = "patch_ID") %>%
      filter(!st_is_empty(geometry)) %>%
      st_as_sf()
    
    # Save each merged_df as RDS file
    saveRDS(merged_df, file = file.path(output_dir, paste0("wood_poly_extraction_", i, ".rds")))
    
    extraction_results[[i]] <- merged_df
  }
  
  result_df <- dplyr::bind_rows(extraction_results)
  
  if (length(problematic_tiles) > 0) {
    message("Problematic tiles:", paste(problematic_tiles, collapse = ", "))
  }
  
  return(list(result = result_df, problematic_tiles = problematic_tiles))
}