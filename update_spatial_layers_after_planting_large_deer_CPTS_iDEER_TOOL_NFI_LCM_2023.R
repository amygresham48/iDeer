#UPDATING iDEER SPATIAL LAYERS FOR BAYESIAN BELIEF NETWORK MODEL - LARGE DEER ####
#Authors: Amy Gresham, Matt Guy, October 2024

#The purpose of this function is to update spatial layers for the Bayesian
#Belief Network model to produces an updated deer damage risk map presented
#in the RShiny iDeer tool after new woodlands are inserted into landscape

large_deer_risk_update <- function(wood_polys, #new woodland polygon(s)
                                   sitebuf, #user's landscape extent
                                   lcm, #CEH GB land cover map 2023
                                   dams, #GB dams map
                                   site_woods_unmerged, #unmerged woodland polygons
                                   lcm_updated, #updated lcm from previous function
                                   buffered_woods_3km, #new woods buffered by 3km
                                   nfi, #original NFI 2023 dataset
                                   nfi_lcm_unmerged_polys, #polygons to extract to from raster
                                   large_attraction_cpt_df,
                                   large_thermoreg_cpt_df,
                                   large_wood_food_value_cpt_df,
                                   large_risk_cpt_df
){

#1.PERCENTAGE COVER WOODLAND EDGE WITHIN 1000m #####-------------------------------------

  # Join the IFT_IOA column from NFI to unmerged_polys using OBJECTID
  site_woods_unmerged <- site_woods_unmerged %>%
    left_join(nfi %>% st_drop_geometry() %>% select(OBJECTID, IFT_IOA), 
              by = "OBJECTID") %>%
    #Assign "Young trees" to IFT_IOA for new polys (will contain NA in IFT_IOA)
    mutate(IFT_IOA = if_else(is.na(IFT_IOA), "Young trees", IFT_IOA)) 
  
  #Categorise woodlands types into closed canopy / mixed structure woodland types  
  site_woods_unmerged <- site_woods_unmerged %>%
    mutate(woodland_type_ID = case_when(
      IFT_IOA %in% c("Assumed woodland","Failed","Windblow","Ground prep","Felled","Low density", "Young trees","Shrub") ~ 1,
      IFT_IOA %in% c("Conifer","Broadleaved","Mixed mainly broadleaved","Mixed mainly conifer","Coppice with standards","Coppice") ~ 2,
      #IFT_IOA %in% c("Agricultural land","Grassland","Other vegetation") ~ 3, #don't need this line- boundaries function automatically obtains edges of classes (1 and 2) with NAs
      TRUE ~ NA_real_  # This handles other values (if needed, or you can leave it out)
    ))
  
  site_woods_unmerged_rast <- fasterize::fasterize(site_woods_unmerged, lcm_updated, field = "woodland_type_ID")
  
  # Get boundaries between 1 and 2
  boundaries_wood <- boundaries(site_woods_unmerged_rast, type = "inner", classes = TRUE, #make edges between classes as well as NA
                                directions = 8, #search for edges in 8 directions
                                asNA = TRUE) #non-edges returned as NA
  #plot(boundaries_wood)
  crs(boundaries_wood) <- 27700
  
  #Focal extraction of edge area within 1000m
  circle.buff = raster::focalWeight(boundaries_wood, d=1000, type="circle",fillNA=T)#Create buffer
  circle.buff[circle.buff > 0] <- 1   # replacing weights by 1
  Focal1000_EDGE_AREA= raster::focal(x=boundaries_wood, w=circle.buff, fun=sum, na.rm=T, pad=TRUE, padValue=NA)
  
  #Divide number of pixels by the total number of 25m2 pixels in a 1000m buffer*100 = percentage lf
  Focal1000_EDGE_AREA_PERC <- (Focal1000_EDGE_AREA/5027)*100
  
  #plot(Focal1000_EDGE_AREA_PERC)


#2. WOODLAND VULNERABILITY #####-------------------------------------

  #Open woodland types will have a high risk of impact from large deer
  #Closed woodland types will have a lower risk of impact
  
  #Add a column woodland_vulnerability to woods_only with following conditions
  #Where open habitats have highest
  #2 =  "Recent woodland" 
  #1 = mature conifer woodland and mature broadleaved woodland
  
  wood_vulnerability<- site_woods_unmerged %>%
    mutate(woodland_food_value = case_when(
      IFT_IOA %in% c("Assumed woodland","Failed","Windblow","Ground prep","Felled","Low density", "Young trees","Shrub") ~ 2,
      IFT_IOA %in% c("Broadleaved", "Mixed mainly broadleaved","Coppice with standards","Coppice","Conifer","Mixed mainly conifer") ~ 1,
      TRUE ~ NA_real_  # This handles other values (if needed, or you can leave it out)
    ))
  
  woodland_food_value_raster <- fasterize::fasterize(wood_vulnerability, lcm_updated,
                                                     field = "woodland_food_value")
  
  crs(woodland_food_value_raster) <- 27700
  #plot(woodland_food_value_raster)
  
#3. MAX SUMMED DAMS WITHIN 1000M #--------------------------------
  
  #crop dams to 3km around new woods
  
  #buffered_woods_5km <- st_buffer(buffered_woods_3km, dist = 2000)
  #dams_crop <- crop(dams, buffered_woods_3km)
  
  #DAMS_open_raster_test <- crop(DAMS_open_raster, tiles)
  
  # circle.buff = raster::focalWeight(dams_crop, d=1000, type="circle",fillNA=T )#Create buffer
  # circle.buff[circle.buff > 0] <- 1   # replacing weights by 1
  # Focal1000_DAMS= raster::focal(x=dams_crop, w=circle.buff, fun=sum, na.rm=T, pad=TRUE, padValue=NA)#,
  #                               #filename = here("output/GB_datasets_2023/large-deer/sum_dams_1000_non_wood_2023.tif"), 
  #                               #overwrite=T)#do focal window analysis with weights matrix
  # 
  #plot(Focal1000_DAMS)
  #plot(site_woods_unmerged$geometry,add=TRUE)
  Focal1000_DAMS <- crop(dams, buffered_woods_3km)
  #plot(Focal1000_DAMS)
  #Ensure raster instead of rast
  Focal1000_DAMS <- raster::raster(Focal1000_DAMS)
  #plot(site_woods_unmerged$geometry,add=TRUE)
  
  #Aggregate to patch, where the score for each patch is the maximum summed DAMS score for any pixel in patch
  patch_agg <- raster::extract(Focal1000_DAMS, site_woods_unmerged, fun=max, na.rm=TRUE)
  
  # Convert to data frame
  patch_agg <- data.frame(max_dams_1000_non_wood_2023 = patch_agg)
  
  # Add OBJECTID from the original polygons
  patch_agg$OBJECTID <- site_woods_unmerged$OBJECTID
  
  #print("patch_agg")
  #print(patch_agg)
  
  # patch_agg <- patch_agg %>% rename(#group_ID = poly_id,
  #   max_dams_1000_non_wood_2023 = focal_DAMS_1000m)
  
  #left_join the max summed DAMS score to GB_polys_merged
  
  polys_unmerged_max_summed_DAMS <- left_join(site_woods_unmerged, patch_agg, by=c("OBJECTID"="OBJECTID"))
  
  polys_unmerged_max_summed_DAMS <- st_as_sf(polys_unmerged_max_summed_DAMS)
  
  #Rasterize
  
  polys_unmerged_max_summed_DAMS_raster <- fasterize::fasterize(sf = polys_unmerged_max_summed_DAMS,
                                                              raster = Focal1000_DAMS,
                                                              field = "max_dams_1000_non_wood_2023")
  
  #plot(polys_unmerged_max_summed_DAMS_raster)
  
  #min and max values from original DAMS raster
  #0 is not true minimum, just value for NoData
  summed_dams_min <- 3142.74
  summed_dams_max <- 117530
  
  #Normalise summed DAMS scores from 0-1
  polys_unmerged_max_summed_DAMS_raster_normalised <- (polys_unmerged_max_summed_DAMS_raster - summed_dams_min) / 
    (summed_dams_max - summed_dams_min)
  
  #plot(polys_unmerged_max_summed_DAMS_raster_normalised)
  

#4.PERCENTAGE COVER ARABLE #####-----------------------------------

reclass_arable <- data.frame(is = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,
                                    18,19,20,21), becomes = c(0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0))

arable_raster <- reclassify(lcm_updated, reclass_arable)
crs(arable_raster) <- 27700

circle.buff = raster::focalWeight(arable_raster, d=1000, type="circle",fillNA=T)#Create buffer
circle.buff[circle.buff > 0] <- 1   # replacing weights by 1
Focal1000_ARABLE= raster::focal(x=arable_raster, w=circle.buff, fun=sum, na.rm=T, pad=TRUE, padValue=NA)
crs(Focal1000_ARABLE) <- 27700

Focal1000_ARABLE_PERC <- (Focal1000_ARABLE/5027)*100

#plot(Focal1000_ARABLE_PERC)

#5.PERCENTAGE COVER PERENNIAL ####-----------------------------------

reclass_peren <- data.frame(is = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,
                                          18,19,20,21), becomes = c(0,0,0,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,1,0,0))

peren_raster <- reclassify(lcm_updated, reclass_peren)
crs(peren_raster) <- 27700

circle.buff = raster::focalWeight(peren_raster, d=1000, type="circle",fillNA=T )#Create buffer
circle.buff[circle.buff > 0] <- 1   # replacing weights by 1
Focal1000_peren= raster::focal(x=peren_raster, w=circle.buff, fun=sum, na.rm=T, pad=TRUE, padValue=NA)

Focal1000_peren_PERC <- (Focal1000_peren/5027)*100

#plot(Focal1000_peren_PERC)

#6.WOODLAND SHELTER VALUE ####----------------------------------------

woods_shelter_quality <- site_woods_unmerged %>%
  mutate(woodland_shelter_value = case_when(
    IFT_IOA %in% c("Assumed woodland","Failed","Windblow","Ground prep","Felled","Low density", "Young trees") ~ 1,
    IFT_IOA %in% c("Coppice with standards","Coppice","Shrub") ~ 2,
    IFT_IOA %in% c("Broadleaved", "Mixed mainly broadleaved","Conifer","Mixed mainly conifer") ~ 3,
    TRUE ~ NA_real_  # This handles other values (if needed, or you can leave it out)
  ))

woodland_shelter_value_raster <- fasterize::fasterize(woods_shelter_quality , lcm_updated,
                                                      field = "woodland_shelter_value")

crs(woodland_shelter_value_raster) <- 27700
#plot(woodland_shelter_value_raster)

#Create a dataframe containing all extracted raster values within user's landscape

#EXTRACT LENGTH OF WOODLAND EDGE

# Get the cell numbers within the extent of lcm_updated
pixel_ID <- cellsFromExtent(Focal1000_EDGE_AREA_PERC, extent(st_bbox(lcm_updated)))

# Get the coordinates for these cells
cell_coords <- as.data.frame(xyFromCell(Focal1000_EDGE_AREA_PERC, pixel_ID))

# Extract pixel values from raster at these coordinates
pixel_values <- raster::extract(Focal1000_EDGE_AREA_PERC, cell_coords)

# Get raster name to rename the extracted values column
raster_name <- "Focal1000_EDGE_AREA_PERC"

# Create a dataframe with extracted values and cell info
vals <- data.frame(
  pixel_ID = pixel_ID,
  x = cell_coords$x,
  y = cell_coords$y,
  raster_values = pixel_values
)

# Rename extracted values column
names(vals)[names(vals) == "raster_values"] <- raster_name

# Replace NA with zero
vals[[raster_name]][is.na(vals[[raster_name]])] <- 0

# Remove pixel_ID if not needed for plotting
edgepix <- vals %>% select(-pixel_ID)

# # Plot with ggplot2
# ggplot(edgepix) +
#   geom_tile(aes(x = x, y = y, fill = Focal1000_EDGE_AREA_PERC)) +
#   scale_fill_viridis_c(option = "plasma") +  # Apply a continuous viridis color scale
#   theme_minimal() +
#   guides(fill = guide_colorbar(barwidth = 1, barheight = 10)) +  # Customize the colorbar appearance
#   labs(title = "Edge area within 1000m",
#        fill = "") 

#EXTRACT ARABLE AREA WITHIN 1000M

arablepix <- extract_raster(map_reclass = Focal1000_ARABLE_PERC,
                          lcm = lcm_updated)

# ggplot(arablepix) +
#   geom_tile(aes(x = x, y = y, fill = Focal1000_ARABLE_PERC)) +
#   scale_fill_viridis_c(option = "plasma") +  # Apply a continuous viridis color scale
#   theme_minimal() +
#   guides(fill = guide_colorbar(barwidth = 1, barheight = 10)) +  # Customize the colorbar appearance
#   labs(title = "Arable area within 1000m",
#        fill = "") 

arablepix <- arablepix %>% select(-c(pixel_ID))

#EXTRACT summed DAMS
damspix <- extract_raster(map_reclass = polys_unmerged_max_summed_DAMS_raster_normalised,
                          lcm = lcm_updated)

# ggplot(damspix) +
#   geom_tile(aes(x = x, y = y, fill = polys_unmerged_max_summed_DAMS_raster_normalised)) +
#   scale_fill_viridis_c(option = "plasma") +  # Apply a continuous viridis color scale
#   theme_minimal() +
#   guides(fill = guide_colorbar(barwidth = 1, barheight = 10)) +  # Customize the colorbar appearance
#   labs(title = "Max summed DAMS within 1000m",
#        fill = "") 

damspix <- damspix %>% select(-c(pixel_ID))

#EXTRACT PERENNIAL AREA

perenpix <- extract_raster(map_reclass = Focal1000_peren_PERC,
                          lcm = lcm_updated)

# ggplot(perenpix) +
#   geom_tile(aes(x = x, y = y, fill = Focal1000_peren_PERC)) +
#   scale_fill_viridis_c(option = "plasma") +  # Apply a continuous viridis color scale
#   theme_minimal() +
#   guides(fill = guide_colorbar(barwidth = 1, barheight = 10)) +  # Customize the colorbar appearance
#   labs(title = "Perennial cover within 1000m",
#        fill = "") 

perenpix <- perenpix %>% select(-c(pixel_ID))

#EXTRACT WOODLAND FOOD QUALITY

foodpix <- extract_raster(map_reclass = woodland_food_value_raster,
                          lcm = lcm_updated)

# ggplot(foodpix) +
#   geom_tile(aes(x = x, y = y, fill = woodland_food_value_raster)) +
#   scale_fill_viridis_c(option = "plasma") +  # Apply a continuous viridis color scale
#   theme_minimal() +
#   guides(fill = guide_colorbar(barwidth = 1, barheight = 10)) +  # Customize the colorbar appearance
#   labs(title = "Woodland food quality",
#        fill = "") 

foodpix <- foodpix %>% select(-c(pixel_ID))

#EXTRACT WOODLAND SHELTER QUALITY

shelterpix <- extract_raster(map_reclass = woodland_shelter_value_raster,
                           lcm = lcm_updated)

# ggplot(shelterpix) +
#   geom_tile(aes(x = x, y = y, fill = woodland_shelter_value_raster)) +
#   scale_fill_viridis_c(option = "plasma") +  # Apply a continuous viridis color scale
#   theme_minimal() +
#   guides(fill = guide_colorbar(barwidth = 1, barheight = 10)) +  # Customize the colorbar appearance
#   labs(title = "Woodland shelter quality",
#        fill = "") 

shelterpix <- shelterpix %>% select(-c(pixel_ID))


#left_join the datasets together #---------------

df <- left_join(edgepix,arablepix, by = c("x","y"))
df <- left_join(df,shelterpix, by = c("x","y"))
df <- left_join(df,damspix, by = c("x","y"))
df <- left_join(df, perenpix, by = c("x","y"))
df <- left_join(df, foodpix, by = c("x","y"))


#make any NAs zeros apart DAMS, woodland cover and urban distance
#these contain correct pixel IDs
df.no.nas <- df %>%
  mutate(across(c(Focal1000_EDGE_AREA_PERC, Focal1000_ARABLE_PERC,
                  Focal1000_peren_PERC),
                ~ tidyr::replace_na(., 0)))

#Remove the NAs

df.no.nas <- na.omit(df.no.nas)

# #plot the maps
# 
# #woodland food value raster
# ggplot(df.no.nas) +
#   geom_tile(aes(x = x, y = y, fill = woodland_food_value_raster)) +
#   scale_fill_viridis_c(option = "plasma") +  # Apply a continuous viridis color scale
#   theme_minimal() +
#   guides(fill = guide_colorbar(barwidth = 1, barheight = 10)) +  # Customize the colorbar appearance
#   labs(title = "woodland food quality",
#        fill = "") 
# 
# #woodland edge area
# ggplot(df.no.nas) +
#   geom_tile(aes(x = x, y = y, fill = Focal1000_EDGE_AREA_PERC)) +
#   scale_fill_viridis_c(option = "plasma") +  # Apply a continuous viridis color scale
#   theme_minimal() +
#   guides(fill = guide_colorbar(barwidth = 1, barheight = 10)) +  # Customize the colorbar appearance
#   labs(title = "woodland edge area within 1000m",
#        fill = "")
# 
# 
# #shelter quality
# ggplot(df.no.nas) +
#   geom_tile(aes(x = x, y = y, fill = woodland_shelter_value_raster)) +
#   scale_fill_viridis_c(option = "plasma") +  # Apply a continuous viridis color scale
#   theme_minimal() +
#   guides(fill = guide_colorbar(barwidth = 1, barheight = 10)) +  # Customize the colorbar appearance
#   labs(title = "woodland shelter quality",
#        fill = "") 
# 
# #dams
# ggplot(df.no.nas) +
#   geom_tile(aes(x = x, y = y, fill = polys_unmerged_max_summed_DAMS_raster_normalised)) +
#   scale_fill_viridis_c(option = "plasma",limits = c(0, 1)) +  # Apply a continuous viridis color scale
#   theme_minimal() +
#   guides(fill = guide_colorbar(barwidth = 1, barheight = 10)) +  # Customize the colorbar appearance
#   labs(title = "Max summed DAMS within 1000m",
#        fill = "") 
# 
# #perennial cover
# ggplot(df.no.nas) +
#   geom_tile(aes(x = x, y = y, fill = Focal1000_peren_PERC)) +
#   scale_fill_viridis_c(option = "plasma")+  # Apply a continuous viridis color scale
#   theme_minimal() +
#   guides(fill = guide_colorbar(barwidth = 1, barheight = 10)) +  # Customize the colorbar appearance
#   labs(title = "summed perennial quality within 1000m",
#        fill = "") 
# 
# #Arable cover
# ggplot(df.no.nas) +
#   geom_tile(aes(x = x, y = y, fill = Focal1000_ARABLE_PERC)) +
#   scale_fill_viridis_c(option = "plasma") +  # Apply a continuous viridis color scale
#   theme_minimal() +
#   guides(fill = guide_colorbar(barwidth = 1, barheight = 10)) +  # Customize the colorbar appearance
#   labs(title = "Arable area within 1000m",
#        fill = "") 

#Reclassify continuous data into pre-specified Low,Medium,High categories #------------------------------------

#Need to look at overall dataset to identify these boundaries

#For now, just use this example dataset

df_cat <- df.no.nas

df_cat <- df_cat %>% rename(Woodland_food_value = woodland_food_value_raster,
                            Woodland_shelter_value = woodland_shelter_value_raster,
                            Focal1000_EDGES_PERC = Focal1000_EDGE_AREA_PERC,
                            Focal1000_MAX_DAMS = polys_unmerged_max_summed_DAMS_raster_normalised,
                            Focal1000_PEREN_PERC = Focal1000_peren_PERC,
                            Woodland_food_value = woodland_food_value_raster,
                            Woodland_shelter_value = woodland_shelter_value_raster)

#Edges within 1000m
#LOW/MED/HIGH
df_cat$Focal1000_EDGES_PERC<- ifelse(df_cat$Focal1000_EDGES_PERC <= 5, "Low",
          ifelse(df_cat$Focal1000_EDGES_PERC > 5 & df_cat$Focal1000_EDGES_PERC <=20, "Medium",
          ifelse(df_cat$Focal1000_EDGES_PERC>20, "High", NA)))#)

# Woodland forage quality
df_cat$Woodland_food_value <-
         ifelse(df_cat$Woodland_food_value==1, "Medium",
            ifelse(df_cat$Woodland_food_value==2, "High", NA))

# Woodland shelter quality
df_cat$Woodland_shelter_value <-
            ifelse(df_cat$Woodland_shelter_value==1, "Low",
            ifelse(df_cat$Woodland_shelter_value==2, "Medium",
            ifelse(df_cat$Woodland_shelter_value==3 , "High", NA)))


#Arable within 1000m
#LOW/MED/HIGH
#Manually adjusted
df_cat$Focal1000_ARABLE_PERC <- ifelse(df_cat$Focal1000_ARABLE_PERC <=0, "Low",
           ifelse(df_cat$Focal1000_ARABLE_PERC > 0 & df_cat$Focal1000_ARABLE_PERC <=50, "Medium",
           ifelse(df_cat$Focal1000_ARABLE_PERC >50, "High", NA)))

#Perennial cover within 1000m
df_cat$Focal1000_PEREN_PERC <- ifelse(df_cat$Focal1000_PEREN_PERC <=25, "Low",
          ifelse(df_cat$Focal1000_PEREN_PERC > 25 & df_cat$Focal1000_PEREN_PERC <=50, "Medium",
          ifelse(df_cat$Focal1000_PEREN_PERC >50, "High", NA)))#)

#Max summed DAMS within 1000m
df_cat$Focal1000_MAX_DAMS <- ifelse(df_cat$Focal1000_MAX_DAMS <= 0.60, "Low",
                      ifelse(df_cat$Focal1000_MAX_DAMS > 0.60  & df_cat$Focal1000_MAX_DAMS <=0.80, "Medium",
                      ifelse(df_cat$Focal1000_MAX_DAMS > 0.80, "High", NA)))#)


#ensure factor levels in correct order 
df_cat$Focal1000_MAX_DAMS <- factor(df_cat$Focal1000_MAX_DAMS, levels = c("Low","Medium","High"))

df_cat$Woodland_food_value<- factor(df_cat$Woodland_food_value, levels= c("Medium","High"))

df_cat$Focal1000_ARABLE_PERC <- factor(df_cat$Focal1000_ARABLE_PERC, levels = c("Low","Medium","High"))

df_cat$Focal1000_EDGES_PERC<- factor(df_cat$Focal1000_EDGES_PERC, levels = c("Low","Medium","High"))

df_cat$Focal1000_PEREN_PERC <- factor(df_cat$Focal1000_PEREN_PERC, levels = c("Low","Medium","High"))

df_cat$Woodland_shelter_value <- factor(df_cat$Woodland_shelter_value, levels = c("Low","Medium","High"))

#unique(df_cat$NEAREST_URB_SUBURB)
unique(df_cat$Focal1000_MAX_DAMS)
unique(df_cat$Focal1000_PEREN_PERC)
unique(df_cat$Focal1000_ARABLE_PERC)
unique(df_cat$Focal1000_EDGES_PERC)
unique(df_cat$Woodland_food_value)
unique(df_cat$Woodland_shelter_value)

#Set up conditional probability tables for BBN #------------------------------------

#CPTS for measured nodes

#perennial cover within 1000m
peren_cpt <- matrix(c(0.333333,0.333333,0.333333),ncol = 3,dimnames = list(NULL, c("Low", "Medium","High")))
#arable area within 1000m
arable_cpt <- matrix(c(0.333333,0.333333,0.333333),ncol = 3,dimnames = list(NULL, c("Low", "Medium","High")))
#Max summed dams within 1000m
dams_cpt<-matrix(c(0.333333,0.333333,0.333333),ncol = 3,dimnames = list(NULL, c("Low", "Medium","High")))
#woodland value
wood_food_value_cpt <- matrix(c(0.5,0.5),ncol = 2,dimnames = list(NULL, c("Medium","High")))
wood_shelter_value_cpt <- matrix(c(0.333333,0.333333,0.333333),ncol = 3,dimnames = list(NULL, c("Low", "Medium","High")))
#woodland edge area within 1000m
edge_cpt <-matrix(c(0.333333,0.333333,0.333333),ncol = 3,dimnames = list(NULL, c("Low", "Medium","High")))


#CPT for Attraction index ------------------------------------------------####

attraction_cpt_df <- large_attraction_cpt_df %>%
  #dplyr::select(-c("X","Expertise","Confidence","Total"))
  dplyr::select(-c("X","total_score"))
head(attraction_cpt_df)

# Define the dimensions
attraction_index_levels <- c("Low", "Medium", "High")
peren_area_levels <- unique(attraction_cpt_df$Perennial_cover_1000m)
arable_area_levels <- unique(attraction_cpt_df$Arable_cover_1000m)
edge_levels <- unique(attraction_cpt_df$Woodland_edge_cover_1000m)

# Initialize the array with dimensions and names
attraction_index_cpt <- array(
  NA,  # Placeholder value
  dim = c(length(attraction_index_levels),
          length(peren_area_levels),
          length(edge_levels),
          length(arable_area_levels)),
  dimnames = list(
    attraction_index = attraction_index_levels,
    Focal1000_PEREN_PERC = peren_area_levels,
    Focal1000_EDGES_PERC = edge_levels,
    Focal1000_ARABLE_PERC = arable_area_levels
  )
)

# Populate the array
for (i in 1:nrow(attraction_cpt_df)) {
  peren_val <- attraction_cpt_df$Perennial_cover_1000m[i]
  edge_val <- attraction_cpt_df$Woodland_edge_cover_1000m[i]
  arable_val <- attraction_cpt_df$Arable_cover_1000m[i]
  
  attraction_index_cpt["Low", as.character(peren_val), as.character(edge_val), as.character(arable_val)] <- attraction_cpt_df$Low[i]
  attraction_index_cpt["Medium", as.character(peren_val), as.character(edge_val), as.character(arable_val)] <- attraction_cpt_df$Medium[i]
  attraction_index_cpt["High", as.character(peren_val), as.character(edge_val), as.character(arable_val)] <- attraction_cpt_df$High[i]
  
}

#attraction_index_cpt <- attraction_index_cpt / 100
attraction_index_cpt

#CPT for thermoregulation index --------------------------------####

thermoreg_cpt_df<- large_thermoreg_cpt_df %>%
  dplyr::select(-c("X"))

# Define the dimensions
thermoreg_index_levels <- c("Low", "Medium", "High")
max_dams_1000m_levels <- unique(thermoreg_cpt_df$MAX_DAMS_1000m)
woodland_shelter_quality_levels <- unique(thermoreg_cpt_df$woodland_shelter_value)

# Initialize the array
thermoreg_index_cpt <- array(
  NA,
  dim = c(length(thermoreg_index_levels), length(max_dams_1000m_levels), length(woodland_shelter_quality_levels)),
  dimnames = list(
    thermoreg_index = thermoreg_index_levels,
    MAX_DAMS_1000m = max_dams_1000m_levels,
    Woodland_shelter_value = woodland_shelter_quality_levels
  )
)

# Populate the array with values from the DataFrame
for (i in 1:nrow(thermoreg_cpt_df)) {
  dams_val <- thermoreg_cpt_df$MAX_DAMS_1000m[i]
  shelter_val <- thermoreg_cpt_df$woodland_shelter_value[i]
  
  thermoreg_index_cpt["Low", as.character(dams_val), as.character(shelter_val)] <- thermoreg_cpt_df$Low[i]
  thermoreg_index_cpt["Medium", as.character(dams_val),as.character(shelter_val)] <- thermoreg_cpt_df$Medium[i]
  thermoreg_index_cpt["High", as.character(dams_val),as.character(shelter_val)] <- thermoreg_cpt_df$High[i]
}

thermoreg_index_cpt

#CPT for woodland food value index --------------------------------####

wood_food_value_cpt_df <- large_wood_food_value_cpt_df %>%
  dplyr::select(-c("X","Total","Expertise","Confidence"))  

head(wood_food_value_cpt_df)

# Define the dimensions
wood_value_index_levels <- c("Medium","High")
wood_food_value_levels <- unique(wood_food_value_cpt_df$Woodland_food_value)

# Initialize the array with dimensions and names
wood_value_index_cpt <- array(
  NA,  # Placeholder value
  dim = c(length(wood_value_index_levels), 
          length(wood_food_value_levels)),
  dimnames = list(
    wood_value_index = wood_value_index_levels,
    Woodland_food_value =wood_food_value_levels
  )
)

# Populate the array with values from the DataFrame
for (i in 1:nrow(wood_food_value_cpt_df)) {
  # Get the indices for each dimension
  wood_val_idx <- which(dimnames(wood_value_index_cpt)$Woodland_food_value == wood_food_value_cpt_df$Woodland_food_value[i])
  
  # Assign values for each forage pressure index level
  wood_value_index_cpt["Medium", wood_val_idx] <- wood_food_value_cpt_df$Medium[i]
  wood_value_index_cpt["High",wood_val_idx] <- wood_food_value_cpt_df$High[i]
}
#wood_value_index_cpt <- wood_value_index_cpt / 100
wood_value_index_cpt 

#Final CPT: deer damage risk -----------------------------------####

risk_cpt_df<- large_risk_cpt_df %>%
  dplyr::select(-c("X","total_score")) %>%
  rename(attraction_index=Attraction_Index,
         wood_value_index=Woodland_Food_Value_Index,
         thermoreg_index=Thermoregulation_Index)#,
#urban_dist_index= Urban_Distance_Index)

# Replace '..' with '_' in column names
#colnames(risk_cpt_df) <- gsub("\\.\\.", "_", colnames(risk_cpt_df))

head(risk_cpt_df)

# Define the dimensions
large_risk_index_levels <- c("Low", "Low_Medium" ,"Medium", "Medium_High" , "High")
wood_value_index_levels <- unique(risk_cpt_df$wood_value_index)
attraction_index_levels <- unique(risk_cpt_df$attraction_index)
thermoreg_index_levels <- unique(risk_cpt_df$thermoreg_index)
#urban_dist_levels <- unique(risk_cpt_df$urban_dist_index)

# Initialize the array with dimensions and names
large_risk_index_cpt <- array(
  NA,  # Placeholder value
  dim = c(length(large_risk_index_levels), 
          length(wood_value_index_levels), 
          length(attraction_index_levels), 
          length(thermoreg_index_levels)),
  dimnames = list(
    large_risk_index = large_risk_index_levels,
    wood_value_index = wood_value_index_levels,
    attraction_index = attraction_index_levels,
    thermoreg_index = thermoreg_index_levels
  )
)

# Populate the array with values from the DataFrame
for (i in 1:nrow(risk_cpt_df)) {
  wood_food_val <- risk_cpt_df$wood_value_index[i]
  attract_val <- risk_cpt_df$attraction_index[i]
  thermoreg_val <- risk_cpt_df$thermoreg_index[i]
  #urban_dist_val <- risk_cpt_df$urban_dist_index[i]
  
  large_risk_index_cpt["Low", as.character(wood_food_val), as.character(attract_val), as.character(thermoreg_val)] <- risk_cpt_df$Low[i]
  large_risk_index_cpt["Low_Medium", as.character(wood_food_val), as.character(attract_val), as.character(thermoreg_val)] <- risk_cpt_df$Low_Medium[i]
  large_risk_index_cpt["Medium", as.character(wood_food_val), as.character(attract_val), as.character(thermoreg_val)] <- risk_cpt_df$Medium[i]
  large_risk_index_cpt["Medium_High", as.character(wood_food_val), as.character(attract_val), as.character(thermoreg_val)] <- risk_cpt_df$Medium_High[i]
  large_risk_index_cpt["High", as.character(wood_food_val), as.character(attract_val), as.character(thermoreg_val)] <- risk_cpt_df$High[i]
}
large_risk_index_cpt 

#Create BBN structure#------------------------------------

# Step 1: Explicitly define the nodes in the network
nodes <- c(
  "Woodland_food_value",
  "Woodland_shelter_value",
  #"dist_to_urban",
  "Focal1000_PEREN_PERC",
  "Focal1000_ARABLE_PERC",
  "MAX_DAMS_1000m",
  "Focal1000_EDGES_PERC",
  "attraction_index",
  "thermoreg_index",
  "wood_value_index",
  #"urban_dist_index",
  "large_risk_index"
)

#empty graph
library(bnlearn)
e = empty.graph(nodes)

arc.set = matrix(c("Woodland_food_value", "wood_value_index",
                   "Focal1000_PEREN_PERC", "attraction_index",
                   "Focal1000_EDGES_PERC", "attraction_index",
                   "Focal1000_ARABLE_PERC", "attraction_index",
                   "MAX_DAMS_1000m","thermoreg_index",
                   "Woodland_shelter_value","thermoreg_index",
                   "thermoreg_index","large_risk_index",
                   "wood_value_index","large_risk_index",
                   "attraction_index","large_risk_index"),
                 ncol = 2, byrow = TRUE,
                 dimnames = list(NULL, c("from", "to")))

arcs(e) <- arc.set

model_string <- modelstring(e) 

net<-model2network(model_string) 
# You dont need to repeat the names of the nodes with parents (child nodes) 

# Custom fitting network (matching up the nodes to their CPTs)
dfit = custom.fit(net, dist = list(Woodland_food_value = wood_food_value_cpt, 
                                   Focal1000_PEREN_PERC = peren_cpt,
                                   Focal1000_EDGES_PERC = edge_cpt,
                                   MAX_DAMS_1000m = dams_cpt,
                                   Focal1000_ARABLE_PERC = arable_cpt,
                                   Woodland_shelter_value = wood_shelter_value_cpt,
                                   thermoreg_index = thermoreg_index_cpt,
                                   attraction_index =attraction_index_cpt,
                                   large_risk_index=large_risk_index_cpt,
                                   wood_value_index=wood_value_index_cpt))

#Plot BN structure ####
#graphviz.plot(net)

#Check parameters ####
dfit

#Predict latent indices for BBN (Thermoreg Index, Connectivity Index, Disturbance Index and Foraging pressure index) ####--------------------------------

predict_dat <- df_cat %>%st_drop_geometry() #%>% #drop geometry
#mutate(across(where(is.character), toupper)) %>% #convert characters to upper case
#dplyr::select(-c("x","y"))

#Ensure variable names match those in BBN
predict_dat<-predict_dat%>%rename(
  Focal1000_ARABLE_PERC=Focal1000_ARABLE_PERC,
  Focal1000_PEREN_PERC=Focal1000_PEREN_PERC,
  MAX_DAMS_1000m=Focal1000_MAX_DAMS,
  Woodland_food_value=Woodland_food_value,
  Focal1000_EDGES_PERC = Focal1000_EDGES_PERC)

#Make empty columns for the latent (unobserved) variables

predict_dat$attraction_index <- NA
predict_dat$wood_value_index <- NA
predict_dat$thermoreg_index <- NA
predict_dat$large_risk <- NA

head(predict_dat)

#Ensure all columns are factors, not characters
predict_dat <- predict_dat %>% mutate_all(as.factor)

#Ensure factor levels are in correct order
levels(predict_dat$Woodland_food_value)<-c("Medium", "High")
levels(predict_dat$Focal1000_PEREN_PERC)<-c("Low", "Medium", "High")
levels(predict_dat$Focal1000_ARABLE_PERC)<-c("Low", "Medium", "High")
levels(predict_dat$MAX_DAMS_1000m)<-c("Low", "Medium", "High")
levels(predict_dat$Focal1000_EDGES_PERC)<-c("Low", "Medium", "High")
levels(predict_dat$Woodland_shelter_value) <- c("Low", "Medium", "High")

predict_dat$attraction_index <- factor(NA, levels = c("Low", "Medium", "High"))
predict_dat$wood_value_index <- factor(NA, levels = c("Medium", "High"))
predict_dat$thermoreg_index <- factor(NA, levels = c("Low", "Medium", "High"))
predict_dat$large_risk <- factor(NA, levels = c("Low", "Low_Medium", "Medium", "Medium_High", "High"))


all_nodes <- names(dfit)

#PREDICTIONS TAKE A LONG TIME, ABOUT 3 MINUTES PER LINE
#Predict values for latent variables
pred_thermoreg = predict(object=dfit,node="thermoreg_index",data=predict_dat, method = "parents")
pred_fpi = predict(dfit,node="attraction_index",data=predict_dat, method = "parents")
pred_wood_value = predict(dfit,node="wood_value_index",data=predict_dat, method = "parents")

#fill the columns
predict_dat$thermoreg_index <- pred_thermoreg
predict_dat$attraction_index <- pred_fpi
predict_dat$wood_value_index <- pred_wood_value

#Predict damage
pred_damage = predict(dfit,node = "large_risk_index",data = predict_dat,method = "parents")
predict_dat$large_risk <- pred_damage

# # scatter plot of forage pressure index and alt forage quality
# predict_dat_sample <- predict_dat[sample(nrow(predict_dat), 10000), ]
# ggplot(predict_dat_sample, aes(x = alt_forage_qual_1000m, y = attraction_index)) +
#   geom_jitter(width = 0.2, height = 0.2, size = 3, color = "blue") +
#   labs(x = "Alt forage Q", y = "Index") +
#   theme_minimal()

#Add damage risk back into spatial dataset

df_cat$pred_damage <- pred_damage
df_cat$pred_thermoreg <- pred_thermoreg
df_cat$pred_attraction_index <- pred_fpi
df_cat$pred_wood_value_index <- pred_wood_value


#Make risk raster ####---------------------------

# Convert points dataframe to sf
df_sf <- st_as_sf(df_cat, coords = c("x", "y"), crs = st_crs(27700))

# # Define palettes
# #wood_value_palette <- c("0" = NA, "1" = "orange","2" = "red")
# index_palette <- c("1" = "green", "2" = "orange", "3" = "red")
# risk_palette  <- c("1" = "green", "2" = "yellow", "3" = "orange", "4" = "red", "5" = "brown")
# 
# # Map categorical predictions to numeric values
# df_sf <- df_sf %>%
#   mutate(
#     forage_vals    = c("Low"=1, "Medium"=2, "High"=3)[pred_attraction_index],
#     thermoreg_vals   = c("Low"=1, "Medium"=2, "High"=3)[pred_thermoreg],
#     wood_value_vals= c("Medium"=2, "High"=3)[pred_wood_value_index],
#     risk_vals      = c("Low"=1, "Low_Medium"=2, "Medium"=3, "Medium_High"=4, "High"=5)[pred_damage]
#   )
# 
# #Wood value index #### ----------------------------------------------------------------------
# df_sf_selected <- df_sf%>%dplyr::select(geometry, wood_value_vals)
# #forage_r <- stars::st_rasterize(sf = df_sf_selected, template = stars_template)
# wood_value_vals_r <- stars::st_rasterize(sf = df_sf_selected)
# wood_value_vals_r[wood_value_vals_r== 0] <- NA #make zero's NAs
# 
# # Convert raster to dataframe for ggplot
# df_rast <- as.data.frame(wood_value_vals_r, xy = TRUE, na.rm = FALSE)
# 
# ggplot() +
#   geom_raster(
#     data = df_rast %>% dplyr::filter(!is.na(wood_value_vals)),
#     aes(x = x, y = y, fill = factor(wood_value_vals))
#   ) +
#   scale_fill_manual(
#     values = index_palette,
#     name = "Woodland food value Index"
#   ) +
#   coord_equal() +
#   theme_minimal()
# 
# #Thermoregulation index ####---------------------------------------------------------------------
# 
# df_sf_selected <- df_sf%>%dplyr::select(geometry, thermoreg_vals)
# #forage_r <- stars::st_rasterize(sf = df_sf_selected, template = stars_template)
# thermoreg_vals_r <- stars::st_rasterize(sf = df_sf_selected)
# thermoreg_vals_r[thermoreg_vals_r== 0] <- NA #make zero's NAs
# 
# # Convert raster to dataframe for ggplot
# df_rast <- as.data.frame(thermoreg_vals_r, xy = TRUE, na.rm = FALSE)
# 
# ggplot() +
#   geom_raster(
#     data = df_rast %>% dplyr::filter(!is.na(thermoreg_vals)),
#     aes(x = x, y = y, fill = factor(thermoreg_vals))
#   ) +
#   scale_fill_manual(
#     values = index_palette,
#     name = "Thermoreg Index",
#     drop=FALSE
#   ) +
#   coord_equal() +
#   theme_minimal()
# 
# #Attraction index #### ---------------------------------------------------------------------
# 
# df_sf_selected <- df_sf%>%dplyr::select(geometry, forage_vals)
# #forage_r <- stars::st_rasterize(sf = df_sf_selected, template = stars_template)
# forage_vals_r <- stars::st_rasterize(sf = df_sf_selected)
# forage_vals_r[forage_vals_r== 0] <- NA #make zero's NAs
# 
# # Convert raster to dataframe for ggplot
# df_rast <- as.data.frame(forage_vals_r, xy = TRUE, na.rm = FALSE)
# 
# ggplot() +
#   geom_raster(
#     data = df_rast %>% dplyr::filter(!is.na(forage_vals)),
#     aes(x = x, y = y, fill = factor(forage_vals))
#   ) +
#   scale_fill_manual(
#     values = index_palette,
#     name = "Attraction Index"
#   ) +
#   coord_equal() +
#   theme_minimal()

# #Impact risk index ####------------------------------------------------------------------------
# 
# df_sf_selected <- df_sf%>%dplyr::select(geometry, risk_vals)
# #forage_r <- stars::st_rasterize(sf = df_sf_selected, template = stars_template)
# risk_vals_r <- stars::st_rasterize(sf = df_sf_selected)
# risk_vals_r[risk_vals_r== 0] <- NA #make zero's NAs
# 
# # Convert raster to dataframe for ggplot
# df_rast <- as.data.frame(risk_vals_r, xy = TRUE, na.rm = FALSE)
# 
# ggplot() +
#   geom_raster(
#     data = df_rast %>% dplyr::filter(!is.na(risk_vals)),
#     aes(x = x, y = y, fill = factor(risk_vals, levels = names(risk_palette)))
#   ) +
#   scale_fill_manual(
#     values = risk_palette,
#     name = "Large deer impact risk index",
#     drop = FALSE
#   ) +
#   coord_equal() +
#   theme_minimal()

#Extract deer damage risk scores to woodland polygons #####----------------------

#par(mfrow = c(1, 1))

# Make sf object
df_sf <- st_transform(df_sf, crs = 27700)

# Define the value mapping for pred_damage
damage_values <- c("Low" = 1, "Low_Medium" = 2, "Medium" = 3, "Medium_High" = 4, "High" = 5)
df_sf$value <- damage_values[df_sf$pred_damage]

#Convert to raster
# Select the geometry and value column
df_sf_selected <- df_sf %>% dplyr::select(geometry, value)

risk_vals_r <- stars::st_rasterize(sf = df_sf_selected)
risk_vals_r[risk_vals_r== 0] <- NA #make zero's NAs

#r_rast <- raster::rasterize(df_sf_selected, lcm_updated, field = "value")
r_rast <- as(risk_vals_r, "Raster")
r_rast <- terra::rast(r_rast)
r_rast[r_rast== 0] <- NA

#plot(r_rast)

#EXTRACT RASTER VALUES
#mean_risk <- raster::extract(r_rast, nfi_lcm_unmerged_polys, fun=mean, na.rm=TRUE,df=TRUE)

#NEED TO USE SAME EXTRACTION METHOD AS ORIGINAL CURRENT RISK MAP
#OTHER EXTRACTED VALUES FOR EXISTING POLYGONS DO NOT MATCH
#SEE FUNCTION extract_raster_pixels_to_wood_polygons_func_10km_chunks_mean_poly_values_NFI_LCM_merged_pre_chunked_exact.R
#Ensure patch_IDs line up
nfi_lcm_unmerged_polys$patch_ID <- 1:nrow(nfi_lcm_unmerged_polys)

mean_risk <- exactextractr::exact_extract(r_rast, nfi_lcm_unmerged_polys, "mean",progress = FALSE)

mean_risk_clean <- data.frame(patch_ID = nfi_lcm_unmerged_polys$patch_ID,
                              mean_large_deer_impact_risk = round(unlist(mean_risk), 2))
                              

#Append the mean risk categories to the polygon dataset
polygon_with_mean_risk <- nfi_lcm_unmerged_polys %>%
  left_join(mean_risk_clean, by = "patch_ID")


#Get the polygons just inside the users extent (exclude buffer zone)
polygon_with_mean_risk <- st_filter(polygon_with_mean_risk, sitebuf, .predicate = st_within)

#Subset for any polygons where mean_risk <1
polygon_with_mean_risk <- polygon_with_mean_risk %>% filter(mean_large_deer_impact_risk >=1)

# ggplot(polygon_with_mean_risk) +
#   geom_sf(aes(fill = mean_large_deer_impact_risk), color = "black") +  # Fill by Weighted_Mean, black borders
#   scale_fill_gradient(low = "black", high = "yellow",limits = c(1, 5)) +  # Customize color gradient (optional)
#   labs(title = "Polygon Geometries Colored by Weighted Mean Risk",
#        fill = "Mean woodland impact risk") +  # Add title and legend label
#   theme_minimal()  # Use a minimal theme (optional)

#return(list(polygon_with_mean_risk, r_rast))
return(polygon_with_mean_risk)



}