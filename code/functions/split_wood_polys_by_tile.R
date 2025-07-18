library(sf)
library(dplyr)
library(progress)

split_wood_polys_by_tile <- function(tiles, wood_polys, output_dir, file_prefix) {
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  
  wood_polys$patch_ID <- 1:nrow(wood_polys)
  wood_polys$assigned <- FALSE
  
  # Precompute intersections once - returns list of integer vectors (indices of wood_polys per tile)
  message("Computing intersections between woodlands and tiles...")
  tile_matches <- st_intersects(tiles, wood_polys) 
  
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
    
    outfile <- file.path(output_dir, paste0(file_prefix, tile_id, ".rds"))
    
    # Skip if file already exists
    if (file.exists(outfile)) {
      next
    }
    
    # Get indices of wood polygons intersecting this tile
    wood_idx <- tile_matches[[i]]
    
    if (length(wood_idx) == 0) {
      next
    }
    
    # Only keep polygons not assigned yet
    wood_idx_unassigned <- wood_idx[!wood_polys$assigned[wood_idx]]
    
    if (length(wood_idx_unassigned) == 0) {
      next
    }
    
    # Subset the wood polygons by these indices
    wood_polys_tile <- wood_polys[wood_idx_unassigned, ]
    
    # Add tile_ID column
    wood_polys_tile$tile_ID <- tile_id
    
    # Mark these polygons as assigned
    wood_polys$assigned[wood_idx_unassigned] <- TRUE
    
    # Save to file
    tryCatch(
      {
        saveRDS(wood_polys_tile, outfile)
      },
      error = function(e) {
        message(paste("Failed to write tile", i, ":", e$message))
        problematic_tiles <<- c(problematic_tiles, i)
      }
    )
  }
  
  if (length(problematic_tiles) > 0) {
    message("Problematic tiles during splitting: ", paste(problematic_tiles, collapse = ", "))
  }
  
  message("Splitting complete. RDS files saved in: ", normalizePath(output_dir))
}