#LAND COVER EXTRACTION FOR 1km AROUND WOODLANDS ####
#Function written by Becks
#This function takes too much time to perform at the pixel scale
#Therefore we are calculating Nutritional Landscape Quality at the patch level.
#This still takes around 45 minutes for 5 patches in 10km landscape
#2mins for 1 patch in 5km landscape

  polyFragStats <- function(POLY, veg) {
  # Create a buffer around the polygon
  buffer_poly <- buffer(POLY, width = 1000, dissolve = TRUE)
  
  # Cut out the original polygon from the buffer
  buffer_area <- erase(buffer_poly, POLY)
  
  # Crop land cover map by the buffer area
  fcrop <- crop(veg, buffer_area)
  
  # Mask the cropped land cover map
  fmask <- mask(fcrop, buffer_area)
  
  # Extract class statistics
  res <- raster::extract(fmask, buffer_area, method = "simple")
  
  # Convert the results to a data frame and calculate statistics
  res <- as.data.frame(table(res))
  res <- res %>% rename(class=res)
  res$class <- as.numeric(as.character(res$class))
  res$class <- round(res$class, 3)
  res$patch_ID <- POLY$patch_ID
  
  res <- res %>%
    mutate(total_pix = as.numeric(sum(Freq)))%>%
    dplyr::select(patch_ID, class, Freq, total_pix) %>%
    mutate(perc_cov = Freq * 100 / total_pix, buff_area = area(buffer_area))
  
  return(res)
}

#Filter for woodland patches that intersect buffers
woods_buffers <- st_filter(wood_polys, buffered_woods_5km, .predicate = st_intersects)

POLY=as(woods_buffers, "Spatial")
veg = map

# Initialize progress bar
pb <- progress_bar$new(
  format = "[:bar] :current/:total (:percent) in :elapsed, eta: :eta",
  total = length(POLY),
  clear = FALSE,
  width = 60
)

respoly <- ldply(1:length(POLY), function(i) {  # for each polygon
  pb$tick()  # Update the progress bar
  out <- polyFragStats(POLY[i,],  veg=veg) # compute fragstats metrics 
  out$buffer_size <- "b1000"
  return(out)
})  

respoly

write.csv(respoly, here("data/derived-data/Tool-extract-function-test/landscapemetrics_1000_5km.csv"))