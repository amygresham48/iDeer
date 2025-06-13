split_wood_polys_by_tile <- function(tiles, wood_polys) {
  
  # Add unique patch ID
  wood_polys$patch_ID <- 1:nrow(wood_polys)
  
  # List to store filtered woodlands for each tile
  wood_polys_by_tile <- list()
  
  # Track already assigned patches
  assigned_patches <- c()
  
  # Problem reporting
  problematic_tiles <- c()
  
  pb <- progress_bar$new(
    format = "  Splitting [:bar] :percent | Elapsed: :elapsed | ETA: :eta",
    total = nrow(tiles),
    clear = FALSE,
    width = 60
  )
  
  for (i in seq_len(nrow(tiles))) {
    pb$tick()
    
    tile <- tiles[i, ]
    
    # Attempt to filter woodlands intersecting this tile
    wood_polys_tile <- tryCatch(
      {
        st_filter(wood_polys, tile)
      },
      error = function(e) {
        message(paste("Skipping tile", i, "- error during st_filter:", e$message))
        problematic_tiles <<- c(problematic_tiles, i)
        return(NULL)
      }
    )
    
    # Skip if no woodlands in this tile
    if (is.null(wood_polys_tile) || nrow(wood_polys_tile) == 0) {
      next
    }
    
    # Remove any already assigned patches
    wood_polys_tile <- wood_polys_tile %>%
      filter(!patch_ID %in% assigned_patches)
    
    # Skip if none remain
    if (nrow(wood_polys_tile) == 0) {
      next
    }
    
    # Add these patches to assigned list
    assigned_patches <- c(assigned_patches, wood_polys_tile$patch_ID)
    
    # Store in list by tile index (or optionally tile ID)
    wood_polys_by_tile[[as.character(i)]] <- wood_polys_tile
  }
  
  # Report any problematic tiles
  if (length(problematic_tiles) > 0) {
    message("Problematic tiles during splitting: ", paste(problematic_tiles, collapse = ", "))
  }
  
  return(wood_polys_by_tile)
}