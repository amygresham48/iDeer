#UPDATING iDEER SPATIAL LAYERS FOR BAYESIAN BELIEF NETWORK MODEL - SMALL DEER ####
#Authors: Amy Gresham, Matt Guy, October 2024

#The purpose of this function is to update spatial layers for the Bayesian
#Belief Network model to produces an updated deer damage risk map presented
#in the RShiny iDeer tool after new woodlands are inserted into landscape

#RUN AFTER update_wood_polys_function
#REQUIRES OUTPUT FROM THAT FUNCTION

small_deer_risk_update <- function(wood_polys, #new woodland polygon(s)
                                   sitebuf, #user's landscape extent
                                   lcm, #CEH GB land cover map 2023
                                   dams, #GB dams map
                                   nfi, #NFI 2023 dataset
                                   lf, #Linear feature GB raster
                                   #site_woods_merged, #merged woodland polygons
                                   site_woods_unmerged, #unmerged woodland polygons
                                   #woods_binary, #binary map of woodlands within buffer
                                   lcm_updated, #updated lcm from previous function
                                   buffered_woods_3km, #new woods buffered by 3km
                                   nfi_lcm_unmerged_polys, #extraction_polys #polygons to extract to from raster
                                   small_connectivity_cpt_df,
                                   small_attraction_cpt_df,
                                   small_wood_food_value_cpt_df,
                                   small_risk_cpt_df_full
){

#Update spatial layers #####-------------------------------------------------

#1. PERCENTAGE COVER WOODLAND EDGE WITHIN 400m #####-------------------------------------
  
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

#Focal extraction of edge area within 400m
circle.buff = raster::focalWeight(boundaries_wood, d=400, type="circle",fillNA=T)#Create buffer
circle.buff[circle.buff > 0] <- 1   # replacing weights by 1
Focal400_EDGE_AREA= raster::focal(x=boundaries_wood, w=circle.buff, fun=sum, na.rm=T, pad=TRUE, padValue=NA)

#Divide number of pixels by the total number of 25m2 pixels in a 400m buffer*100 = percentage lf
Focal400_EDGE_AREA_PERC <- (Focal400_EDGE_AREA/804)*100

#plot(Focal400_EDGE_AREA_PERC)

#2. WOODLAND VULNERABILITY ------------#############

#Open woodland types will have a high risk of impact from small deer
#Closed woodland types will have a lower risk of impact

#Add a column woodland_vulnerability to woods_only with following conditions
#Where open habitats have highest
#3 = "Recent woodland"
#2 = mature broadleaved woodland
#1 = mature conifer woodland

wood_vulnerability <- site_woods_unmerged %>%
  mutate(woodland_food_value = case_when(
    IFT_IOA %in% c("Assumed woodland","Failed","Windblow","Ground prep","Felled","Low density", "Young trees","Shrub") ~ 3,
    IFT_IOA %in% c("Broadleaved", "Mixed mainly broadleaved","Coppice with standards","Coppice") ~ 2,
    IFT_IOA %in% c("Conifer","Mixed mainly conifer") ~ 1,
    TRUE ~ NA_real_  # This handles other values (if needed, or you can leave it out)
  ))

woodland_food_value_raster <- fasterize::fasterize(wood_vulnerability, lcm_updated,
                                                   field = "woodland_food_value")
crs(woodland_food_value_raster) <- 27700
#plot(woodland_food_value_raster)

#3. CONNECTIVITY -----------------####

#merged connectivity woods
# Step 1: Get touching relationships
touching <- st_touches(site_woods_unmerged)

# Step 2: Build graph and identify groups
g <- graph_from_adj_list(touching)
groups <- components(g)$membership

# Step 3: Add group ID to the data
site_woods_unmerged$touching_group_ID <- groups

# Step 4: Dissolve (union) polygons by group
merged_connect_woods <- site_woods_unmerged%>%
  group_by(touching_group_ID) %>%
  summarise(geometry = st_union(geometry), .groups = "drop")

#plot(merged_connect_woods$geometry)

#Rasterize

connectivity_woods_raster <- fasterize::fasterize(merged_connect_woods, lcm_updated)

#crop for testing
#connectivity_woods_raster <- crop(connectivity_woods_raster, tiles)

#d = 400m radius
circle.buff = raster::focalWeight(connectivity_woods_raster, d=400, type="circle",fillNA=T)#Create buffer
circle.buff[circle.buff > 0] <- 1   # replacing weights by 1
Focal400_WOOD= raster::focal(x=connectivity_woods_raster, w=circle.buff, fun=sum, na.rm=T, pad=TRUE, padValue=NA)

#plot(Focal400_WOOD)
crs(Focal400_WOOD) <- 27700

#Divide number of pixels by the total number of 25m2 pixels in a buffer*100 = percentage woodland
#Focal2000_WOOD_PERC <- (Focal2000_WOOD/20106)*100
Focal400_WOOD_PERC <- (Focal400_WOOD/804)*100
#plot(Focal400_WOOD_PERC)

#Get average woodland cover within 400m for each merged woodland polygon
#Accounts for patch size, but also woods around each patch.
mean_wood_map_poly_vals  <- raster::extract(Focal400_WOOD_PERC, merged_connect_woods, fun=mean, na.rm=TRUE, df=TRUE)

#left_join GB_merged_polys
mean_wood_map_poly_vals <- mean_wood_map_poly_vals %>% rename(touching_group_ID = ID)
mean_wood_perc_polys <- left_join(merged_connect_woods, mean_wood_map_poly_vals, by = c("touching_group_ID"))
mean_wood_perc_polys <- st_as_sf(mean_wood_perc_polys)

woods_area_400m_tif <- fasterize::fasterize(sf = mean_wood_perc_polys,
                                            raster = connectivity_woods_raster,
                                            field = "layer",
                                            fun = "sum",
                                            background = NA)
crs(woods_area_400m_tif) <- 27700
#plot(woods_area_400m_tif)

#4.PERCENTAGE COVER LINEAR FEATURES WITHIN 400M--------####

LFclip <- crop(lf, buffered_woods_3km)
#plot(LFclip)
#LFclip <- projectRaster(LFclip, lcm_updated)

#Mask by the new woodland polygons
LFclip <- mask(LFclip, wood_polys, inverse = TRUE)

circle.buff = raster::focalWeight(LFclip, d=400, type="circle",fillNA=T)#Create buffer
circle.buff[circle.buff > 0] <- 1   # replacing weights by 1
Focal400_LF_AREA= raster::focal(x=LFclip, w=circle.buff, fun=sum, na.rm=T, pad=TRUE, padValue=NA)
crs(Focal400_LF_AREA) <- 27700

#Divide number of pixels by the total number of 25m2 pixels in a 400m buffer*100 = percentage lf
Focal400_LF_PERC <- (Focal400_LF_AREA/804)*100
#plot(Focal400_LF_PERC)

#Get average LF cover within 400m for each merged woodland polygon
#Accounts for patch size, but also woods around each patch.
mean_lf_map_poly_vals  <- raster::extract(Focal400_LF_PERC, merged_connect_woods, fun=mean, na.rm=TRUE, df=TRUE)

# Add the true group_IDs based on polygon order
mean_lf_map_poly_vals <- mean_lf_map_poly_vals %>% rename(touching_group_ID = ID)
mean_lf_map_poly_vals$group_ID <- merged_connect_woods$touching_group_ID

#left_join GB_merged_polys
mean_lf_perc_polys <- left_join(merged_connect_woods, mean_lf_map_poly_vals, by = c("touching_group_ID"))
mean_lf_perc_polys <- st_as_sf(mean_lf_perc_polys)

#Assign zeros to any rows where mean percentage cover of linear features is NaN (no features within 400m)
mean_lf_perc_polys <- mean_lf_perc_polys %>%
  mutate(percentage_LF_area_400m_small_deer_GB_2023 = ifelse(is.nan(layer), 0, layer))

lf_area_400m_tif <- fasterize::fasterize(sf = mean_lf_perc_polys,
                                         raster = lcm_updated,
                                         field = "percentage_LF_area_400m_small_deer_GB_2023",
                                         fun = "sum",
                                         background = NA)
crs(lf_area_400m_tif) <- 27700

#Create a dataframe containing all extracted raster values within user's landscape #------------------------------------

#EXTRACT LENGTH OF WOODLAND EDGE

# Get the cell numbers within the extent of lcm_updated
pixel_ID <- cellsFromExtent(Focal400_EDGE_AREA_PERC, extent(st_bbox(lcm_updated)))

# Get the coordinates for these cells
cell_coords <- as.data.frame(xyFromCell(Focal400_EDGE_AREA_PERC, pixel_ID))

# Extract pixel values from raster at these coordinates
pixel_values <- raster::extract(Focal400_EDGE_AREA_PERC, cell_coords)

# Get raster name to rename the extracted values column
raster_name <- "Focal400_EDGE_AREA_PERC"

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
#   geom_tile(aes(x = x, y = y, fill = .data[[raster_name]])) +
#   scale_fill_viridis_c(option = "plasma") +
#   theme_minimal() +
#   guides(fill = guide_colorbar(barwidth = 1, barheight = 10)) +
#   labs(title = "Percentage cover woodland edge area within 400m", fill = "")

#EXTRACT CONNECTIVITY

connectpix <- extract_raster(map_reclass = woods_area_400m_tif,
                             lcm = lcm_updated)
# ggplot(connectpix) +
#   geom_tile(aes(x = x, y = y, fill = woods_area_400m_tif)) +
#   scale_fill_viridis_c(option = "plasma") +  # Apply a continuous viridis color scale
#   theme_minimal() +
#   guides(fill = guide_colorbar(barwidth = 1, barheight = 10)) +  # Customize the colorbar appearance
#   labs(title = "Percentage cover woodland area within 400m",
#        fill = "")

connectpix  <- connectpix  %>% select(-c(pixel_ID))


#EXTRACT WOODLAND FOOD VALUE

foodpix <- extract_raster(map = woodland_food_value_raster,
                          lcm = lcm_updated)
# ggplot(foodpix) +
#   geom_tile(aes(x = x, y = y, fill = woodland_food_value_raster)) +
#   scale_fill_viridis_c(option = "plasma") +  # Apply a continuous viridis color scale
#   theme_minimal() +
#   guides(fill = guide_colorbar(barwidth = 1, barheight = 10)) +  # Customize the colorbar appearance
#   labs(title = "Percentage cover woodland area within 400m",
#        fill = "")

foodpix <- foodpix %>% select(-c(pixel_ID))


#EXTRACT LF DENSITY WITHIN 400M

LFpix <- extract_raster(map_reclass = lf_area_400m_tif,
                        lcm = lcm_updated)

# ggplot(LFpix) +
#   geom_tile(aes(x = x, y = y, fill = lf_area_400m_tif)) +
#   scale_fill_viridis_c(option = "plasma") +  # Apply a continuous viridis color scale
#   theme_minimal() +
#   guides(fill = guide_colorbar(barwidth = 1, barheight = 10)) +  # Customize the colorbar appearance
#   labs(title = "Percentage cover linear features within 400m",
#        fill = "")

LFpix <- LFpix %>% select(-c(pixel_ID))


#left_join the datasets together #---------------

df <- left_join(edgepix, connectpix, by = c("x","y"))
df <- left_join(df,LFpix, by = c("x","y"))
df <- left_join(df,foodpix, by = c("x","y"))

#make any NAs zeros apart from the dams and woodland cover raster
#These rasters have the correct woodland pixels

df.no.nas <- df %>%
  mutate(across(c(Focal400_EDGE_AREA_PERC),
                ~ tidyr::replace_na(., 0)))

#Remove the NAs

df.no.nas <- na.omit(df.no.nas)

#plot the maps

#par(mfrow = c(3, 2))

# #woodland food value
#  ggplot(df.no.nas) +
#    geom_tile(aes(x = x, y = y, fill = woodland_food_value_raster)) +
#    scale_fill_viridis_c(option = "plasma") +  # Apply a continuous viridis color scale
#    theme_minimal() +
#    guides(fill = guide_colorbar(barwidth = 1, barheight = 10)) +  # Customize the colorbar appearance
#   labs(title = "woodland food value for small deer",
#         fill = "")
# 
# #woodland edge area
# ggplot(df.no.nas) +
#   geom_tile(aes(x = x, y = y, fill = Focal400_EDGE_AREA_PERC)) +
#   scale_fill_viridis_c(option = "plasma") +  # Apply a continuous viridis color scale
#   theme_minimal() +
#   guides(fill = guide_colorbar(barwidth = 1, barheight = 10)) +  # Customize the colorbar appearance
#   labs(title = "woodland edge area within 400m",
#        fill = "")
# 
# #linear feature density
# ggplot(df.no.nas) +
#   geom_tile(aes(x = x, y = y, fill = lf_area_400m_tif)) +
#   scale_fill_viridis_c(option = "plasma") +  # Apply a continuous viridis color scale
#   theme_minimal() +
#   guides(fill = guide_colorbar(barwidth = 1, barheight = 10)) +  # Customize the colorbar appearance
#   labs(title = "linear feature density within 400m",
#        fill = "")
# 
# #woodland connectivity
# ggplot(df.no.nas) +
#   geom_tile(aes(x = x, y = y, fill = woods_area_400m_tif)) +
#   scale_fill_viridis_c(option = "plasma") +  # Apply a continuous viridis color scale
#   theme_minimal() +
#   guides(fill = guide_colorbar(barwidth = 1, barheight = 10)) +  # Customize the colorbar appearance
#   labs(title = "woodland cover within 400m",
#        fill = "")


#Reclassify continuous data into pre-specified Low,Medium,High ####

#NEED TO LOOK AT WHOLE DATASETS TO GET BIN VALUES

df_cat <- df.no.nas

df_cat <- df_cat %>% rename(Woodland_food_value = woodland_food_value_raster,
                            Focal400_WOOD_PERC = woods_area_400m_tif,
                            Focal400_LF_PERC = lf_area_400m_tif,
                            Focal400_EDGES_PERC = Focal400_EDGE_AREA_PERC)

#Linear features
# Replace numeric values with cat labels
df_cat$Focal400_LF_PERC <- ifelse(df_cat$Focal400_LF_PERC<=1, "Low",
                                  ifelse(df_cat$Focal400_LF_PERC>1 & df_cat$Focal400_LF_PERC <= 5, "Medium",
                                         ifelse(df_cat$Focal400_LF_PERC> 5, "High", NA)))

#Woodland connectivity
df_cat$Focal400_WOOD_PERC <- ifelse(
  df_cat$Focal400_WOOD_PERC <= 2, "Low",
  ifelse(df_cat$Focal400_WOOD_PERC > 2 & df_cat$Focal400_WOOD_PERC <= 10, "Medium",
         ifelse(df_cat$Focal400_WOOD_PERC > 10, "High", NA)))


#Edge density
df_cat$Focal400_EDGES_PERC <- ifelse(df_cat$Focal400_EDGES_PERC <= 5, "Low",
                                     ifelse(df_cat$Focal400_EDGES_PERC >5 & df_cat$Focal400_EDGES_PERC <=30, "Medium",
                                            ifelse(df_cat$Focal400_EDGES_PERC > 30, "High", NA)))

# Woodland forage quality
df_cat$Woodland_food_value <- ifelse(df_cat$Woodland_food_value == 1, "Low",
                                     ifelse(df_cat$Woodland_food_value==2, "Medium",
                                            ifelse(df_cat$Woodland_food_value ==3 , "High", NA)))


df_cat$Focal400_WOOD_PERC <- factor(df_cat$Focal400_WOOD_PERC, levels= c("Low","Medium","High"))
df_cat$Focal400_LF_PERC <- factor(df_cat$Focal400_LF_PERC, levels = c("Low","Medium","High"))
df_cat$Focal400_EDGES_PERC <- factor(df_cat$Focal400_EDGES_PERC, levels = c("Low","Medium","High"))
df_cat$Woodland_food_value <- factor(df_cat$Woodland_food_value, levels = c("Low","Medium","High"))

unique(df_cat$Woodland_food_value)
unique(df_cat$Focal400_EDGES_PERC)
unique(df_cat$Focal400_WOOD_PERC)
unique(df_cat$Focal400_LF_PERC)


#Set up conditional probability tables for BBN #------------------------------------

#CPTS for measured nodes

#woodland edge area within 400m
edge_cpt <-matrix(c(0.333333,0.333333,0.333333),ncol = 3,dimnames = list(NULL, c("Low", "Medium","High")))
#woodland value
wood_value_cpt <- matrix(c(0.333333,0.333333,0.333333),ncol = 3,dimnames = list(NULL, c("Low", "Medium","High")))
#linear feature area within 400m
lf_cpt <-matrix(c(0.333333,0.333333,0.333333),ncol = 3,dimnames = list(NULL, c("Low", "Medium","High")))
#woodland connectivity within 400m
wood_connect_cpt <- matrix(c(0.333333,0.333333,0.333333),ncol = 3,dimnames = list(NULL, c("Low", "Medium","High")))


#CPT for Connectivity Index ---------------#


connectivity_cpt_df <- small_connectivity_cpt_df %>%
  dplyr::select(-c("X","total_score"))

# Define the dimensions
connect_index_levels <- c("Low", "Medium", "High")
wood_connectivity_400m_levels <- unique(connectivity_cpt_df$Woodland_cover_400m)
summed_LF_400m_levels <- unique(connectivity_cpt_df$LF_cover_400m)

# Initialize the array with dimensions and names
connectivity_index_cpt <- array(
  NA,  # Placeholder value
  dim = c(length(connect_index_levels), 
          length(wood_connectivity_400m_levels), 
          length(summed_LF_400m_levels)),
  dimnames = list(
    connect_index = connect_index_levels,
    Woodland_cover_400m = wood_connectivity_400m_levels,
    LF_cover_400m = summed_LF_400m_levels)
)


# Populate the array with values from the DataFrame
for (i in 1:nrow(connectivity_cpt_df)) {
  # Get the indices for each dimension
  connect_idx <- which(dimnames(connectivity_index_cpt)$Woodland_cover_400m == connectivity_cpt_df$Woodland_cover_400m[i])
  lf_idx <- which(dimnames(connectivity_index_cpt)$LF_cover_400m == connectivity_cpt_df$LF_cover_400m[i])
  
  # Assign values for each connectivity index level
  connectivity_index_cpt["Low", connect_idx, lf_idx] <- connectivity_cpt_df$Low[i]
  connectivity_index_cpt["Medium", connect_idx, lf_idx] <- connectivity_cpt_df$Medium[i]
  connectivity_index_cpt["High", connect_idx, lf_idx] <- connectivity_cpt_df$High[i]
}
#connectivity_index_cpt <- connectivity_index_cpt / 100
connectivity_index_cpt

#CPT for Attraction index ------------------------------------------------####
attraction_cpt_df <- small_attraction_cpt_df %>%
  dplyr::select(-c("X","Total"))

head(attraction_cpt_df)

# Define the dimensions
attraction_index_levels <- c("Low", "Medium","High")
wood_edge_area_levels <- unique(attraction_cpt_df$Woodland_edge_cover_400m)

# Initialize the array with dimensions and names
attraction_index_cpt <- array(
  NA,  # Placeholder value
  dim = c(length(attraction_index_levels), 
          length(wood_edge_area_levels)),
  dimnames = list(
    attraction_index = attraction_index_levels,
    Woodland_edge_cover_400m = wood_edge_area_levels
  )
)

# Populate the array with values from the DataFrame
for (i in 1:nrow(attraction_cpt_df)) {
  # Get the indices for each dimension
  edge_idx <- which(dimnames(attraction_index_cpt)$Woodland_edge_cover_400m == attraction_cpt_df$Woodland_edge_cover_400m[i])
  
  # Assign values for each forage pressure index level
  attraction_index_cpt["Low", edge_idx] <- attraction_cpt_df$Low[i]
  attraction_index_cpt["Medium", edge_idx] <- attraction_cpt_df$Medium[i]
  attraction_index_cpt["High",edge_idx] <- attraction_cpt_df$High[i]
}
#attraction_index_cpt <- attraction_index_cpt / 100
attraction_index_cpt 

#CPT for woodland food value index --------------------------------####

wood_food_value_cpt_df <- small_wood_food_value_cpt_df %>%
  dplyr::select(-c("X","Total","Expertise","Confidence"))  

head(wood_food_value_cpt_df)

# Define the dimensions
wood_value_index_levels <- c("Low", "Medium","High")
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
  wood_value_index_cpt["Low", wood_val_idx] <- wood_food_value_cpt_df$Low[i]
  wood_value_index_cpt["Medium", wood_val_idx] <- wood_food_value_cpt_df$Medium[i]
  wood_value_index_cpt["High",wood_val_idx] <- wood_food_value_cpt_df$High[i]
}
#wood_value_index_cpt <- wood_value_index_cpt / 100
wood_value_index_cpt 


#Final CPT: deer damage risk -----------------------------------####

small_risk_cpt_df<- small_risk_cpt_df_full %>%
  dplyr::select(-c("X","total_score")) %>%
  dplyr::rename(attraction_index=Attraction_Index,
                connect_index=Connectivity_Index,
                wood_value_index=Woodland_Food_Value_Index)

# Replace '.' with '_' in column names
colnames(small_risk_cpt_df) <- gsub("\\..", "_", colnames(small_risk_cpt_df))

head(small_risk_cpt_df)

# Define the dimensions
small_risk_index_levels <- c("Low", "Low_Medium" ,"Medium", "Medium_High" , "High")
connectivity_index_levels <- unique(small_risk_cpt_df$connect_index)
attraction_index_levels <- unique(small_risk_cpt_df$attraction_index)
wood_value_index_levels <- unique(small_risk_cpt_df$wood_value_index)


# Initialize the array with dimensions and names
small_risk_index_cpt <- array(
  NA,  # Placeholder value
  dim = c(length(small_risk_index_levels), 
          length(connectivity_index_levels), 
          length(attraction_index_levels), 
          length(wood_value_index_levels )),
  dimnames = list(
    small_risk_index = small_risk_index_levels,
    connect_index = connectivity_index_levels,
    attraction_index = attraction_index_levels,
    wood_value_index = wood_value_index_levels
  )
)

# Populate the array with values from the DataFrame
for (i in 1:nrow(small_risk_cpt_df)) {
  # Get the indices for each dimension
  connect_idx <- match(small_risk_cpt_df$connect_index[i], dimnames(small_risk_index_cpt)$connect_index)
  fpi_idx <- match(small_risk_cpt_df$attraction_index[i], dimnames(small_risk_index_cpt)$attraction_index)
  wood_value_idx <- match(small_risk_cpt_df$wood_value_index[i], dimnames(small_risk_index_cpt)$wood_value_index)
  
  # Assign values
  small_risk_index_cpt["Low", connect_idx, fpi_idx, wood_value_idx] <- small_risk_cpt_df$Low[i]
  small_risk_index_cpt["Low_Medium", connect_idx, fpi_idx, wood_value_idx] <- small_risk_cpt_df$Low_Medium[i]
  small_risk_index_cpt["Medium", connect_idx, fpi_idx, wood_value_idx] <- small_risk_cpt_df$Medium[i]
  small_risk_index_cpt["Medium_High", connect_idx, fpi_idx, wood_value_idx] <- small_risk_cpt_df$Medium_High[i]
  small_risk_index_cpt["High", connect_idx, fpi_idx, wood_value_idx] <- small_risk_cpt_df$High[i]
}
#small_risk_index_cpt <- small_risk_index_cpt / 100
small_risk_index_cpt

#Create BBN structure#------------------------------------

# Step 1: Explicitly define the nodes in the network
nodes <- c(
  "Woodland_cover_400m",
  "LF_cover_400m",
  "Woodland_edge_cover_400m",
  "Woodland_food_value",
  "attraction_index",
  "wood_value_index",
  "connect_index",
  "small_risk_index"
)

#empty graph
library(bnlearn)
e = empty.graph(nodes)

arc.set = matrix(c("Woodland_cover_400m", "connect_index",
                   "LF_cover_400m", "connect_index", 
                   "Woodland_edge_cover_400m","attraction_index",
                   "Woodland_food_value","wood_value_index",
                   "wood_value_index","small_risk_index",
                   "connect_index","small_risk_index",
                   "attraction_index","small_risk_index"),
                 ncol = 2, byrow = TRUE,
                 dimnames = list(NULL, c("from", "to")))

arcs(e) <- arc.set

model_string <- modelstring(e) 

net<-model2network(model_string) 
# You dont need to repeat the names of the nodes with parents (child nodes) 

# Custom fitting network (matching up the nodes to their CPTs)
dfit = custom.fit(net, dist = list(Woodland_cover_400m = wood_connect_cpt, 
                                   LF_cover_400m = lf_cpt,
                                   Woodland_food_value= wood_value_cpt,
                                   Woodland_edge_cover_400m = edge_cpt,
                                   connect_index = connectivity_index_cpt,
                                   wood_value_index = wood_value_index_cpt,
                                   attraction_index =attraction_index_cpt,
                                   small_risk_index=small_risk_index_cpt))

#Plot BN structure ####
#graphviz.plot(net)

#Check parameters ####
dfit

#Predict latent indices for BBN (wood_value Index, Connectivity Index and Foraging pressure index) ####--------------------------------

predict_dat <- df_cat %>% st_drop_geometry() #drop geometry
#dplyr::select(-c("x","y"))

#Ensure variable names match those in BBN
predict_dat<-predict_dat%>%dplyr::rename(
  Woodland_edge_cover_400m=Focal400_EDGES_PERC,
  LF_cover_400m=Focal400_LF_PERC,
  Woodland_food_value=Woodland_food_value,
  Woodland_cover_400m=Focal400_WOOD_PERC)

#Make empty columns for the latent (unobserved) variables

predict_dat$attraction_index <- NA
predict_dat$connect_index <- NA
predict_dat$wood_value_index <- NA
predict_dat$small_risk <- NA

#Ensure all columns are factors, not characters
predict_dat <- predict_dat %>% mutate_all(as.factor)

#Ensure factor levels are in correct order
levels(predict_dat$Woodland_cover_400m)<-c("Low", "Medium", "High")
levels(predict_dat$Woodland_food_value)<-c("Low", "Medium","High")
levels(predict_dat$LF_cover_400m)<-c("Low", "Medium", "High")
levels(predict_dat$Woodland_edge_cover_400m)<-c("Low", "Medium", "High")
levels(predict_dat$attraction_index)<-c("Low", "Medium", "High")
levels(predict_dat$connect_index)<-c("Low", "Medium", "High")
levels(predict_dat$wood_value_index)<-c("Low", "Medium", "High")
levels(predict_dat$small_risk)<-c("Low","Low_Medium","Medium","Medium_High","High")

#Ensure predict_data is a data.frame
predict_dat <- as.data.frame(predict_dat)

#PREDICTIONS TAKE A LONG TIME, ABOUT 3 MINUTES PER LINE
#Predict values for latent variables
pred_wood_value = predict(object=dfit,node="wood_value_index",data=predict_dat, method = "parents")
pred_fpi = predict(dfit,node="attraction_index",data=predict_dat, method = "parents")
pred_connect = predict(dfit,node="connect_index",data=predict_dat, method = "parents")

#fill the columns
predict_dat$wood_value_index <- pred_wood_value
predict_dat$attraction_index <- pred_fpi
predict_dat$connect_index <- pred_connect

#Predict damage
pred_damage = predict(dfit,node = "small_risk_index",data = predict_dat,method = "parents")
predict_dat$small_risk <- pred_damage

#Add damage risk back into spatial dataset

df_cat$pred_damage <- pred_damage
df_cat$pred_wood_value <- pred_wood_value
df_cat$pred_attraction_index <- pred_fpi
df_cat$pred_connect <- pred_connect

# Convert points dataframe to sf
df_sf <- st_as_sf(df_cat, coords = c("x", "y"), crs = st_crs(27700))

# # Define palettes
# index_palette <- c("1" = "green", "2" = "orange", "3" = "red")
# risk_palette  <- c("1" = "green", "2" = "yellow", "3" = "orange", "4" = "red", "5" = "brown")
# 
# # Map categorical predictions to numeric values
# df_sf <- df_sf %>%
#   mutate(
#     forage_vals    = c("Low"=1, "Medium"=2, "High"=3)[pred_attraction_index],
#     connect_vals   = c("Low"=1, "Medium"=2, "High"=3)[pred_connect],
#     wood_value_vals= c("Low"=1, "Medium"=2, "High"=3)[pred_wood_value],
#     risk_vals      = c("Low"=1, "Low_Medium"=2, "Medium"=3, "Medium_High"=4, "High"=5)[pred_damage]
#   )
# 
# #Wood value index #### ----------------------------------------------------------------------
# df_sf_selected <- df_sf%>%dplyr::select(geometry, wood_value_vals)
# #forage_r <- stars::st_rasterize(sf = df_sf_selected, template = stars_template)
# wood_value_vals_r <- stars::st_rasterize(sf = df_sf_selected)
# wood_value_vals_r[wood_value_vals_r== 0] <- NA #make zero's NAs
# 
# # Convert raster values to factor with levels corresponding to all palette keys
# wood_value_vals_r[[1]] <- factor(as.character(wood_value_vals_r[[1]]),
#                                  levels = names(index_palette))
# 
# # Convert raster to dataframe for ggplot
# df_rast <- as.data.frame(wood_value_vals_r, xy = TRUE, na.rm = FALSE)
# 
# # Plot
# ggplot(na.omit(df_rast)) +
#   geom_raster(aes(x = x, y = y, fill = wood_value_vals)) +
#   scale_fill_manual(
#     values = index_palette,
#     labels = c("Low", "Medium", "High"),
#     drop = FALSE, # <-- this keeps all levels in the legend
#     na.value = "transparent",
#     name = "Wood Value"
#   ) +
#   coord_equal() +
#   theme_minimal() +
#   theme(legend.position = "right")
# 
# #Connectivity index ####---------------------------------------------------------------------
# 
# df_sf_selected <- df_sf%>%dplyr::select(geometry, connect_vals)
# #forage_r <- stars::st_rasterize(sf = df_sf_selected, template = stars_template)
# connect_vals_r <- stars::st_rasterize(sf = df_sf_selected)
# connect_vals_r[connect_vals_r== 0] <- NA #make zero's NAs
# 
# # Convert raster values to factor with levels corresponding to all palette keys
# connect_vals_r[[1]] <- factor(as.character(connect_vals_r[[1]]),
#                               levels = names(index_palette))
# 
# # Convert raster to dataframe for ggplot
# df_rast <- as.data.frame(connect_vals_r, xy = TRUE, na.rm = FALSE)
# 
# # Plot
# ggplot(na.omit(df_rast)) +
#   geom_raster(aes(x = x, y = y, fill = connect_vals)) +
#   scale_fill_manual(
#     values = index_palette,
#     labels = c("Low", "Medium", "High"),
#     drop = FALSE, # <-- this keeps all levels in the legend
#     na.value = "transparent",
#     name = "Connectivity"
#   ) +
#   coord_equal() +
#   theme_minimal() +
#   theme(legend.position = "right")
# 
# #Attraction index #### ---------------------------------------------------------------------
# 
# df_sf_selected <- df_sf%>%dplyr::select(geometry, forage_vals)
# #forage_r <- stars::st_rasterize(sf = df_sf_selected, template = stars_template)
# forage_vals_r <- stars::st_rasterize(sf = df_sf_selected)
# forage_vals_r[forage_vals_r== 0] <- NA #make zero's NAs
# 
# # Convert raster values to factor with levels corresponding to all palette keys
# forage_vals_r[[1]] <- factor(as.character(forage_vals_r[[1]]),
#                              levels = names(index_palette))
# 
# # Convert raster to dataframe for ggplot
# df_rast <- as.data.frame(forage_vals_r, xy = TRUE, na.rm = FALSE)
# 
# # Plot
# ggplot(na.omit(df_rast)) +
#   geom_raster(aes(x = x, y = y, fill = forage_vals)) +
#   scale_fill_manual(
#     values = index_palette,
#     labels = c("Low", "Medium", "High"),
#     drop = FALSE, # <-- this keeps all levels in the legend
#     na.value = "transparent",
#     name = "Attraction Index"
#   ) +
#   coord_equal() +
#   theme_minimal() +
#   theme(legend.position = "right")
# 
# #Impact risk index ####------------------------------------------------------------------------
# 
# df_sf_selected <- df_sf%>%dplyr::select(geometry, risk_vals)
# #forage_r <- stars::st_rasterize(sf = df_sf_selected, template = stars_template)
# risk_vals_r <- stars::st_rasterize(sf = df_sf_selected)
# #risk_vals_r[risk_vals_r== 0] <- NA #make zero's NAs
# 
# # Convert raster values to factor with levels corresponding to all palette keys
# risk_vals_r[[1]] <- factor(as.character(risk_vals_r[[1]]),
#                            levels = names(risk_palette))
# 
# # Convert raster to dataframe for ggplot
# df_rast <- as.data.frame(risk_vals_r, xy = TRUE, na.rm = FALSE)
# 
# # Plot
# ggplot(na.omit(df_rast)) +
#   geom_raster(aes(x = x, y = y, fill = risk_vals)) +
#   scale_fill_manual(
#     values = risk_palette,
#     labels = c("Low","Low-Medium", "Medium","Medium-High", "High"),
#     drop = FALSE, # <-- this keeps all levels in the legend
#     na.value = "transparent",
#     name = "Small deer impact risk"
#   ) +
#   coord_equal() +
#   theme_minimal() +
#   theme(legend.position = "right")


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
#OTHERWISE EXTRACTED VALUES FOR EXISTING POLYGONS DO NOT MATCH
#SEE FUNCTION extract_raster_pixels_to_wood_polygons_func_10km_chunks_mean_poly_values_NFI_LCM_merged_pre_chunked_exact.R
#Ensure patch_IDs line up
nfi_lcm_unmerged_polys$patch_ID <- 1:nrow(nfi_lcm_unmerged_polys)

mean_risk <- exactextractr::exact_extract(r_rast, nfi_lcm_unmerged_polys, "mean",progress = FALSE)

mean_risk_clean <- data.frame(patch_ID = nfi_lcm_unmerged_polys$patch_ID,
                              mean_small_deer_impact_risk = round(unlist(mean_risk), 2))


#Append the mean risk categories to the polygon dataset
polygon_with_mean_risk <- nfi_lcm_unmerged_polys %>%
  left_join(mean_risk_clean, by = "patch_ID")


#Get the polygons just inside the users extent (exclude buffer zone)
polygon_with_mean_risk <- st_filter(polygon_with_mean_risk, sitebuf, .predicate = st_within)

#Subset for any polygons where mean_risk <1
polygon_with_mean_risk <- polygon_with_mean_risk %>% filter(mean_small_deer_impact_risk >=1)

# ggplot(polygon_with_mean_risk) +
#   geom_sf(aes(fill = mean_small_deer_impact_risk), color = "black") +  # Fill by Weighted_Mean, black borders
#   scale_fill_gradient(low = "black", high = "yellow",limits = c(1, 5)) +  # Customize color gradient (optional)
#   labs(title = "Polygon Geometries Colored by Weighted Mean Risk",
#        fill = "Mean woodland impact risk") +  # Add title and legend label
#   theme_minimal()  # Use a minimal theme (optional)

#return(list(polygon_with_mean_risk, r_rast))
return(polygon_with_mean_risk)

}