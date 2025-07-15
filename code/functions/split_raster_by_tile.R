split_raster_by_tile <- function(tiles, raster_data, output_dir, file_prefix) {
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  
  pb <- progress_bar$new(
    format = "  Splitting [:bar] :percent | Elapsed: :elapsed | ETA: :eta",
    total = nrow(tiles),
    clear = FALSE,
    width = 60
  )
  
  problematic_tiles <- c()
  
  for (i in seq_len(nrow(tiles))) {
    pb$tick()
    
    tile_id <- tiles$tile_name[i]
    outfile <- file.path(output_dir, paste0(file_prefix, tile_id, ".tif"))
    
    # Skip if file already exists
    if (file.exists(outfile)) {
      next
    }
    
    # Get tile geometry as SpatVector (terra's vector format)
    tile_geom <- vect(tiles[i, ])
    
    if (is.null(intersect(ext(tile_geom), ext(raster_data)))) {
      next  # no intersection — skip
    }
    
    # Crop and mask the raster to the tile extent
    tryCatch(
      {
        raster_crop <- crop(raster_data, tile_geom)
        raster_masked <- mask(raster_crop, tile_geom)
        
        # Skip empty rasters
        if (global(raster_masked, "notNA", na.rm = TRUE)$notNA == 0) {
          next
        }
        
        # Save the masked raster
        writeRaster(raster_masked, outfile, overwrite = TRUE)
      },
      error = function(e) {
        message(paste("Failed to process tile", tile_id, ":", e$message))
        problematic_tiles <<- c(problematic_tiles, tile_id)
      }
    )
  }
  
  if (length(problematic_tiles) > 0) {
    message("Problematic tiles during splitting: ", paste(problematic_tiles, collapse = ", "))
  }
  
  message("Splitting complete. Raster tiles saved in: ", normalizePath(output_dir))
}