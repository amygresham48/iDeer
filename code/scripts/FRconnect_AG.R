### Dependencies #########

rastin <- raster("testRast4.txt")

#Make function ####

connectfunc <- function(rastin, distance, prop_disp){
  ##
  library(sf)
  library(dplyr)
  library(raster)
  library(pryr)
  library(fasterize)
  library(sp)
  library(rgeos)
  
  #### Import raster
  
  #### Note:  In its current state the script requires individual patches to have unique id values in the input raster (no region group or clump is performed here)
  
  rast1 <- rastin
  
  ### Convert test landscape to vector format
  
  polys <- rasterToPolygons(rast1) %>%
    st_as_sf() %>%
    rename(id = 1) %>%
  
  #polys <- hab_patches %>%
    
    ### Dissolve polygons by id value (merges individual connected polygon cells into single features)
    
    group_by(id) %>%
    
    ### count here calculates the number of cells corresponding to current focal patch - equivalent of area for this example
    summarise(fid = first(id), count = n()) %>%
    #mutate(fid = seq.int(nrow(.))) %>%
    as("Spatial")
  
  condf <- data.frame(id = integer(), coninter = double(), conintra = double(), comp = integer(), compN = integer(), configN = double())
  
  ## Calculation input parameter values
  
  pdisp <- prop_disp
  dispdist <- distance ### 5 cells
  #cellres <- res(rastin)[[1]]
  cellres <- 25
  
  
  idlst <- unique(polys$fid)
  
  for (x in idlst){
    
    #### Isolate focal patch
    
    distx <- polys[polys$fid==x,]
    
    ### Isolate other surrounding patches
    
    polysx <- polys[polys$fid!=x,]
    
    ### Calculate distance from focal patch to other surrounding patches
    
    dist1 <- gDistance(distx, spgeom2 = polysx, byid = TRUE) %>%
      as.data.frame()
    
    colnames(dist1) <- c("dist")
    
    ### merge distance results back to subset dataframe of surrounding patches
    
    df1 <- cbind(as.data.frame(polysx), dist1) %>%
      
      ## Use the cell resolution value here
      
      mutate(distfin = dist/cellres)
    
    ### Calculate interconnectivity value
    
    df2 <- df1 %>%
      mutate(coninter = (exp(-((log(1/pdisp))/dispdist)*distfin)*(count^2)))
    
    ### Final summary table for current focal patch calculating inter/intra-connectivity as well as composition/configuration and
    ### normalised composition/configuration - also phil's adjusted interconnectivity value.
    
    dfin <- data.frame(id = distx$fid, coninter = sum(df2$coninter), conintra = ((distx$count^2)*(distx$count^2)*1),
                       comp = sum(df2$count^2)) %>%
      mutate(compN = comp/(sum(df1$count)^2), configN = coninter/(sum(df1$count)^2)) %>%
      mutate(philcont = coninter*distx$count^2)
    
    ### Merge results with master results dataframe.
    
    condf <- rbind(condf, dfin)
    
  }
  
  return(condf)
  
}

#Run the function ####

rastin <- rastin
distance <- 5
prop_disp <- 0.05

connectfunc(rastin, distance, prop_disp)
