#Load packages ####

library(sf)
library(dplyr)
library(raster)
library(pryr)
library(fasterize)
library(sp)
library(rgeos)
library(progress)
library(here)
here()

#Import polygons- made in ArcGIS from the original nfi2015/lcm2015 raster "nfi_lcm_2015_overlaid_woods_only.tif"

hab_patches_all <- st_read(here("data/derived-data/nfi_lcm_overlaid_shapefile_woods_export.shp"))
head(hab_patches_all)

hab_patches_all <- hab_patches_all %>%
  rename(patch_ID = Id) %>%
  rename(patch_area = Shape_Area)

#Crop these habitat patches to England and Wales only ####

ew_10k <- st_read(here("data/raw-data/10k_tiles_EW.shp"))

#crop hab_patches to England and Wales without clipping the edges of the woodlands

hab_patches <-st_filter(hab_patches_all, ew_10k, .predicate =st_intersects)

#plot(st_geometry(hab_patches))

#### SET CONNECTIVITY PARAMETERS ####
# connectivity parametetrs - % of dispersers reaching a set distance #
percentage_dispersers <- 0.05 ### 5%
dispersal_distance <- 435 # Number of cells
dispersal_contribution <-
  -((log(1 / percentage_dispersers)) / dispersal_distance)

# Buffer distance represents the cut off - this stops the script measuring every pairwise combination
dispersal_cutoff <- 0.999 ### 99.9% cut off
buffer_cutoff <-
  round(log(1 / (1 - dispersal_cutoff)) / (log(1 / percentage_dispersers) /
                                             dispersal_distance), digits = 0)
# test & plot of dispersal contribution for sequence of distance up to buffer_cutoff
test <-
  data.frame(distance = (seq(0, buffer_cutoff, by = 0.5))) %>%
  mutate(contribution = (exp(dispersal_contribution * distance) * 100))

ggplot(data = test, aes(x = distance, y = contribution, group = 1)) +
  geom_line() +
  geom_point() +
  geom_vline(xintercept = buffer_cutoff, color = "red")

####-------------------------------------------------------

rastin <- hab_patches
distance <- 435 ### 414 cells = 1km radius buffer, but 435 cells gets the test curve up to 1km
prop_disp <- 0.9 #percentage of population dispersing. Default is 0.05, but this isn't accurate for a deer population within
#a 1km buffer - do 90%

polys <- hab_patches %>%
  ### count here calculates the number of cells corresponding to current focal patch - equivalent of area for this example
  mutate(fid = 1:nrow(hab_patches)) #%>%
  #as("Spatial")
  
  condf <- data.frame(id = integer(), coninter = double(), conintra = double(), comp = integer(), compN = integer(), configN = double())
  
  ## Calculation input parameter values
  
  pdisp <- prop_disp
  dispdist <- distance 
  cellres <- 25
  
  idlst <- unique(polys$fid)
  
  # Create a progress bar
  pb <- progress_bar$new(
    format = "  Processing [:bar] :percent :eta",
    total = length(idlst),
    clear = FALSE,
    width = 60
  )
  
  
  ####-----------------------------------------------------------------------------####
  
  for (x in idlst[1:10]){
    
    pb$tick()  # Update progress bar
    
    #### Isolate focal patch
    
    distx <- polys[polys$fid==x,]
    
    # id patches to measure to
    # buffer from slected focal patch by bufferDistnace
    distx_buffer <- st_buffer(distx, buffer_cutoff)
    # select source patches within buffer distance of focal patch
    source_patch <- st_filter(polys, distx_buffer, .predicate = st_intersects)
    
    # calculate the distances from all the source patches to the focal patch
    patch_dist <- st_distance(distx, source_patch)
    patch_dist <- as.data.frame(patch_dist)
    
    colnames(patch_dist) <- c("dist")
    
    #Make table
    
    Conn_table_site <- data.frame(distx = distx$patch_ID, 
                                  distx_area = as.numeric(distx$patch_area),
                                  source_patch = source_patch$patch_ID, 
                                  source_patch_area = as.numeric(source_patch$patch_area),
                                  distance = patch_dist)
    
    #Remove rows where focal patch = source patch
    
    # Remove rows where `focal_patch` is the same as `source_patch`
    Conn_table_site <-  Conn_table_site %>%
      filter(distx != source_patch)
    
    df1 <- Conn_table_site %>%
      
      ## Use the cell resolution value here
      
      mutate(distfin = dispdist/cellres)
    
    ### Calculate interconnectivity value
    
    df2 <- df1 %>%
      mutate(coninter = (exp(-((log(1/pdisp))/dispdist)*distfin)*(distx_area^2)))
    
    ### Final summary table for current focal patch calculating inter/intra-connectivity as well as composition/configuration and
    ### normalised composition/configuration - also phil's adjusted interconnectivity value.
    
    dfin <- data.frame(id = distx$fid, coninter = sum(df2$coninter), conintra = ((distx$patch_area^2)*(distx$patch_area^2)*1),
                       comp = df2$distx_area^2) %>%
      mutate(compN = comp/df1$distx_area^2, configN = coninter/(df1$distx_area)^2) %>%
      mutate(philcont = coninter*distx$patch_area^2)
    
    ### Merge results with master results dataframe.
    
    condf <- rbind(condf, dfin)
    
  }
  
