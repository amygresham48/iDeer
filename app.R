#Load libraries

library(sf)
library(raster)
library(dplyr)
library(leaflet)
library(shinydashboard)
library(htmltools)
library(DT)
library(shinyWidgets)
library(htmlwidgets)
library(mapview)
library(leaflet.extras2)
library(leaflet.extras)
library(webshot)
library(ggmap)
library(plotly)
library(terra)
library(tidyterra)
library(maptiles)
library(leafpop)
library(osdatahub)
library(geojson)
library(geojsonsf)
library(geojsonio)
library(ggspatial)
library(ggthemes)
library(leafpm)
library(mapview)
library(mapedit)
library(bnlearn)
library(fasterize)
library(shiny)
library(shinyjs)
library(httr)
library(igraph)
library(stringr)
library(rsconnect)
library(gargle)
library(purrr)
library(deldir)
library(viridisLite)
library(shinyscreenshot)
library(base64enc)
library(exactextractr)


#Load required data layers ####

bng <- 27700

#England-Wales shapefile

ew_shp <- st_read(dsn = ".", layer = "England_Wales_combined")
# Transform to EPSG:4326 (WGS84, used by leaflet)
ew_shp  <- st_transform(ew_shp, crs = 4326)

# #200km grid:
# GB_200km <- st_read(dsn = ".", layer = "custom_grid_200km")
# GB_200km <- st_as_sf(GB_200km)

#England-Wales custom grid:
EW_grid <- st_read(dsn = ".", layer = "EW_subdivided_for_shinyio")
EW_grid$tile_name <- paste0("tile_",1:nrow(EW_grid))
EW_grid$id <- 1:nrow(EW_grid)
EW_grid<- EW_grid %>% dplyr::select(-c("Shape_Area","Shape_Leng"))

#BDS species distribution maps
red_bds <- raster("Red_presence_raster_GB_2005_to_2022_BDS.tif")
roe_bds <- raster("Roe_presence_raster_GB_2005_to_2022_BDS.tif")
munt_bds <- raster("Muntjac_presence_raster_GB_2005_to_2022_BDS.tif")
fallow_bds <- raster("Fallow_presence_raster_GB_2005_to_2022_BDS.tif")
sika_bds <- raster("Sika_presence_raster_GB_2005_to_2022_BDS.tif")
cwd_bds <- raster("CWD_presence_raster_GB_2005_to_2022_BDS.tif")

deer_species_rasters <- c(red_bds, roe_bds, munt_bds, fallow_bds, sika_bds, cwd_bds)

# Define the value mapping for pred_damage
damage_values <- c("Low" = 1, "Low_Medium" = 2, "Medium" = 3, "Medium_High" = 4, "High" = 5)

species_names <- c("Red deer", "Roe deer", "Muntjac deer", "Fallow deer", "Sika deer", "Chinese water deer")

#Load binary woodland raster
#Amy's path
woods <- raster("NFI_woods_only_raster_GB_2023.tif")

### England country boundary for initial map centering
longlat <- c(-1.14, 53)

### Providers of tile layers for learflet maps
### Street map and Esri world imagery
providers <- c("CartoDB.Positron", "Esri.WorldImagery")

#Layers and functions required to produce the updated risk maps #####

#CEH land cover map 2023
lcm <- raster("gblcm2023_25m.tif")

#CEH linear feature dataset 2016 (rasterized)
lf <- raster("WLF_GB_raster_LCM_matched_masked.tif")

#Conditional probability tables ####

#Connectivity
small_connectivity_cpt_df <- read.csv("Small_Deer_Connectivity_Index_CPT_post_interviews.csv")
#Attraction Index
small_attraction_cpt_df <- read.csv("Smalldeer_Attraction_Index_Woodland_edge_cover_400m_Amy.csv")
#woodland food value index
small_wood_food_value_cpt_df <- read.csv("Smalldeer_Woodland_food_value_Index.csv")
#small deer damage risk
small_risk_cpt_df_full <- read.csv("Small_Deer_Damage_Risk_CPT_post_interviews.csv")

#Attraction index
large_attraction_cpt_df <- read.csv("Large_Deer_Attraction_Index_CPT.csv")
#Thermoregulation Index
large_thermoreg_cpt_df <- read.csv("Large_Deer_Thermoreg_Index_CPT_post_interviews.csv")
#woodland food value index
large_wood_food_value_cpt_df <- read.csv("Largedeer_Woodland_food_value_Index_two_levels.csv")
#Large deer impact risk
large_risk_cpt_df <- read.csv("Large_Deer_Damage_Risk_CPT_post_interviews.csv")

#function to update woodland polygon datasets with new woodlands from user
source("update_wood_polys_function_NFI_split_new_polys.R")

#function to extract pixel values from raster layers
source("extract_raster_pixels_func.R")

#function to update small deer risk map
source("update_spatial_layers_after_planting_small_deer_CPTS_iDEER_TOOL_NFI_LCM_2023.R")

#function to update large deer risk map
source("update_spatial_layers_after_planting_large_deer_CPTS_iDEER_TOOL_NFI_LCM_2023.R")

#function to extract risk maps to polygons
#source("extract_raster_pixels_to_wood_polys_func_site_level.R")

#Get viridis palettes for map screenshots
get_viridis_base64 <- function(pal_name = "viridis") {
  tmp <- tempfile(fileext = ".png")
  png(tmp, width = 30, height = 300) # Slightly wider for better capture
  par(mar = c(0,0,0,0))
  # direction = 1 or -1 depending on your specific pal setup
  image(1, 1:100, t(1:100), col = viridisLite::viridis(100, option = pal_name, direction = 1), 
        axes = FALSE, xlab = "", ylab = "")
  dev.off()
  paste0("data:image/png;base64,", base64enc::base64encode(tmp))
}

# Helper to build the HTML structure for your legends
create_deer_legend <- function(img_str, title) {
  if (is.null(img_str) || img_str == "") return("")
  sprintf('
  <div style="background:white; padding:10px; border:2px solid rgba(0,0,0,0.2); border-radius:5px;">
    <strong style="display:block; margin-bottom:5px;">%s</strong>
    <div style="display:flex; align-items:center;">
      <div style="height:150px; width:20px; border:1px solid #999; background-image: url(%s); background-size: cover;"></div>
      <div style="height:150px; margin-left:10px; display:flex; flex-direction:column; justify-content:space-between; font-weight:bold; font-size:12px;">
        <span>5</span><span>4</span><span>3</span><span>2</span><span>1</span>
      </div>
    </div>
  </div>', title, img_str)
}

#SERVER-----------------------------###########

server <- function(input, output, session) {
  
  # Reactive values to store site and buffer data
  siter <- reactiveValues(dat = NULL) 
  sitebuf <- reactiveValues(dat = NULL)
  small_risk_buf <- reactiveValues(dat=NULL)
  large_risk_buf <- reactiveValues(dat=NULL)
  deer_species_present <- reactiveVal(NULL)
  additional_species <- reactiveVal(NULL)    # Additional species selected by user
  confirmed_species <- reactiveVal(NULL)     # Final confirmed species list
  binary_woods_buf <- reactiveVal(NULL)  #Binary map of woodlands
  #binary_woods_buf <- reactiveValues(dat=NULL)  #Binary map of woodlands
  select_mapr <- reactiveValues(dat=NULL)
  updated_small_risk_buf <- reactiveValues(dat=NULL)
  updated_large_risk_buf <- reactiveValues(dat=NULL)
  # Cache for loaded shapefiles
  tile_cache <- reactiveValues(
    small = list(),
    large = list()
  )
  
  #Define loader gif----####
  
  output$loader_container <- renderUI({
    div(id = "loader-container", 
        style = "position: fixed; top: 50%; left: 50%;
               transform: translate(-50%, -50%);
               z-index: 9999; background-color: rgba(255,255,255,0.8);
               padding: 20px; border-radius: 10px; display: none;",
        img(src = "deer-gif-please-wait.gif", class = "loader-img")
    )
  })
  
  #SELECT YOUR AREA -------------####
  
  # Initial map rendering
  output$select_map <- renderLeaflet({
    leaflet(height='200%', width='200%') %>%
      addTiles(group = "OpenStreetMap") %>%
      addProviderTiles("Esri.WorldImagery", group = "Satellite view") %>%
      addLayersControl(
        baseGroups = c("Colour street map","Satellite view"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      setView(lng = longlat[1], lat = longlat[2], zoom = 7)
  })
  
  
  observe({
    
    # user clicks
    click <- input$select_map_click
    if (is.null(click)) return()
    
    # Show loader
    session$sendCustomMessage("showLoader", TRUE)
    
    # Convert click to sf POINT
    clicked_point <- st_sfc(st_point(c(click$lng, click$lat)), crs = 4326)
    
    # Check if the point is inside the shapefile boundary
    within_boundary <- st_within(clicked_point, ew_shp, sparse = FALSE)
    if (!any(within_boundary)) {
      session$sendCustomMessage("showLoader", FALSE)  # Hide the loader before exiting
      showModal(modalDialog(
        title = "Invalid selection",
        "Please select an area within England or Wales.",
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
      return()
    }
    
    long <- as.numeric(click$lng)
    lat <- as.numeric(click$lat)
    cordtab <- data.frame(X = long, Y = lat)
    
    sitep <- st_as_sf(cordtab, coords = c("X", "Y"), crs = 4326, agr = "constant") %>%
      st_transform("epsg:27700") %>%
      mutate(id = 1)
    
    buffer_size_meters <- input$inSlider * 1000
    
    siter$dat <- sitep
    sitebuf$dat <- st_buffer(sitep, buffer_size_meters) %>%
      st_transform("EPSG:27700")
    
    # Buffer site buffer again for tile selection
    buffered_woods_3km <- st_buffer(sitebuf$dat, dist = 3000)
    
    # Find intersecting tiles
    tiles_needed <- EW_grid[st_intersects(EW_grid, buffered_woods_3km, sparse = FALSE), ]
    tile_names_needed <- tiles_needed$tile_name
    
    
    #-----------------Read SMALL DEER RISK polygons via WFS spatial query ----####
    
    # --- 1. Define buffer polygon for spatial filtering ---
    poly <- sitebuf$dat
    
    # --- 2. Identify intersecting tiles from grid ---
    tiles_needed <- EW_grid[st_intersects(EW_grid, poly, sparse = FALSE), ]
    tile_names_needed <- tiles_needed$tile_name
    
    # --- 3. Define WFS base URL ---
    wfs_url <- "https://frgeospatial.uk/geoserver/wfs"
    
    # --- 4. Construct WFS layer names based on tiles ---
    layer_names <- paste0("small_deer_risk_", tile_names_needed)
    
    # --- 5. Generate bounding box string from polygon ---
    bbox_string <- toString(st_bbox(poly))
    
    # --- 6. Loop over WFS layers, query using bbox, clean, and combine ---
    small_deer_list <- lapply(layer_names, function(layer_name) {
      message("Fetching small deer risk layer: ", layer_name)
      
      # Build WFS URL for this layer
      url_obj <- parse_url(wfs_url)
      url_obj$query <- list(
        service = "WFS",
        request = "GetFeature",
        typename = layer_name,
        srsName = "EPSG:27700",
        bbox = bbox_string,
        outputFormat = "application/json"
      )
      full_url <- build_url(url_obj)
      
      # Read and clean geometry
      sf_layer <- tryCatch({
        reg <- read_sf(full_url)
        st_crs(reg) <- "EPSG:27700"
        
        reg_clean <- reg %>%
          st_cast("GEOMETRYCOLLECTION") %>%
          st_collection_extract("POLYGON") %>%
          rename(
            mean_risk = men_rsk   # Rename risk column
          ) %>%
          mutate(mean_risk = round(mean_risk, 2)) %>%
          select(-c("tile_ID", "assignd", "Wd_Ar_2")) %>%
          st_make_valid() %>%
          select(-ptch_ID)
        
        reg_clean
      }, error = function(e) {
        warning(paste("Could not read layer", layer_name, ":", e$message))
        NULL
      })
      
      return(sf_layer)
    })
    
    # --- 7. Combine all valid layers into one ---
    small.deer.risk <- do.call(rbind, Filter(Negate(is.null), small_deer_list))
    
    # ---- HANDLE SEA / NON-MAPPED AREA CLICK ----
    if (is.null(small.deer.risk)) {
      session$sendCustomMessage("showLoader", FALSE)
      
      showModal(modalDialog(
        title = "Outside map area",
        "The maps do not cover this area. Did you click on the sea? Please select another area.",
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
      
      return()
    }
    
    # --- 9. Done ---
    print("small.deer.risk")
    print(small.deer.risk)
    
    #-----------------Read LARGE DEER RISK polygons via WFS spatial query ----####
    
    # --- 1. Define buffer polygon for spatial filtering ---
    poly <- sitebuf$dat
    
    # --- 2. Identify intersecting tiles from grid ---
    tiles_needed <- EW_grid[st_intersects(EW_grid, poly, sparse = FALSE), ]
    tile_names_needed <- tiles_needed$tile_name
    
    # --- 3. Define WFS base URL ---
    wfs_url <- "https://frgeospatial.uk/geoserver/wfs"
    
    # --- 4. Construct WFS layer names based on tiles ---
    layer_names <- paste0("large_deer_risk_", tile_names_needed)
    
    # --- 5. Generate bounding box string from polygon ---
    bbox_string <- toString(st_bbox(poly))
    
    # --- 6. Loop over WFS layers, query using bbox, clean, and combine ---
    large_deer_list <- lapply(layer_names, function(layer_name) {
      message("Fetching large deer risk layer: ", layer_name)
      
      # Build WFS URL for this layer
      url_obj <- parse_url(wfs_url)
      url_obj$query <- list(
        service = "WFS",
        request = "GetFeature",
        typename = layer_name,
        srsName = "EPSG:27700",
        bbox = bbox_string,
        outputFormat = "application/json"
      )
      full_url <- build_url(url_obj)
      
      # Read and clean geometry
      sf_layer <- tryCatch({
        reg <- read_sf(full_url)
        st_crs(reg) <- "EPSG:27700"
        
        reg_clean <- reg %>%
          st_cast("GEOMETRYCOLLECTION") %>%
          st_collection_extract("POLYGON") %>%
          rename(
            mean_risk = men_rsk   # Rename risk column
          ) %>%
          mutate(mean_risk = round(mean_risk, 2)) %>%
          select(-c("tile_ID", "assignd", "Wd_Ar_2")) %>%
          st_make_valid() %>%
          select(-ptch_ID)
        
        reg_clean
      }, error = function(e) {
        warning(paste("Could not read layer", layer_name, ":", e$message))
        NULL
      })
      
      return(sf_layer)
    })
    
    # --- 7. Combine all valid layers into one ---
    large.deer.risk <- do.call(rbind, Filter(Negate(is.null), large_deer_list))
    
    # ---- HANDLE SEA / NON-MAPPED AREA CLICK ----
    if (is.null(large.deer.risk)) {
      session$sendCustomMessage("showLoader", FALSE)
      
      showModal(modalDialog(
        title = "Outside map area",
        "The maps do not cover this area. Did you click on the sea? Please select another area.",
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
      
      return()
    }
    

    # --- 9. Done ---
    print("large.deer.risk")
    print(large.deer.risk)
    
    # --- CROP SMALL RISK TO BUFFER AND ASSIGN wood_ID AFTER FILTER ---
    tmp_small <- st_filter(small.deer.risk, sitebuf$dat, .predicate = st_within)
    
    if (nrow(tmp_small) == 0) {
      message("No existing woodland polygons within site buffer for small deer risk")
      isolate(small_risk_buf$dat <- tmp_small)
    } else {
      tmp_small <- tmp_small %>% mutate(wood_ID = dplyr::row_number())
      isolate(small_risk_buf$dat <- tmp_small)
    }
    
    # --- CROP LARGE RISK TO BUFFER AND ASSIGN wood_ID AFTER FILTER ---
    tmp_large <- st_filter(large.deer.risk, sitebuf$dat, .predicate = st_within)
    
    if (nrow(tmp_large) == 0) {
      message("No existing woodland polygons within site buffer for large deer risk")
      isolate(large_risk_buf$dat <- tmp_large)
    } else {
      tmp_large <- tmp_large %>% mutate(wood_ID = dplyr::row_number())
      isolate(large_risk_buf$dat <- tmp_large)
    }

#------ Update map with user's area of interest --------------####    
    
    woods_cropped <- raster::crop(woods, sitebuf$dat)
    woods_cropped <- raster::mask(woods_cropped, sitebuf$dat)
    crs(woods_cropped) <- "epsg:27700"
    
    binary_woods_buf(woods_cropped)
    
   
    leafletProxy("select_map") %>%
      #addTiles() %>%
      clearMarkers() %>%
      clearShapes() %>%
      addCircleMarkers(lng = long, lat = lat, radius = 1, color = "cyan") %>%
      addPolygons(data = sitebuf$dat %>% st_transform("epsg:4326"),
                  color = "#00FFFF",
                  weight = 2,
                  fillColor = "#00FFFF",
                  fillOpacity = 0.1)
    
    # Hide loader
    session$sendCustomMessage("showLoader", FALSE)
    
  })
  
  
  #DEER SPECIES ------------------------------------####
  
  # Observe which deer species are present using species distribution rasters
  species_names <- c("Red deer", "Roe deer", "Muntjac deer", "Fallow deer", "Sika deer", "Chinese water deer")
  names(deer_species_rasters) <- species_names
  
  # Reactive expression to determine species present
  deer_species_present <- reactive({
    req(sitebuf$dat)  # Ensure the buffer data is available
    
    sitebuffer <- sitebuf$dat
    
    species_present <- c()
    
    for (i in species_names) {
      extracted_vals <- raster::extract(deer_species_rasters[[i]], st_as_sf(sitebuffer))
      
      if (any(unlist(extracted_vals) == 1, na.rm = TRUE)) {
        species_present <- c(species_present, i)
      }
    }
    
    species_present
  })
  
  # Update the UI to display the species present
  output$species_list <- renderUI({
    species_present <- deer_species_present()  # Get the current species list
    
    if (length(species_present) > 0) {
      tagList(
        tags$strong(style = "font-size:16px;",
                    "According to the British Deer Society species distribution maps, these deer species are within 10km of your area of interest:"
        ),
        tags$ul(  
          lapply(species_present, function(species) {
            tags$li(style = "font-size: 16px;", paste(species))
          })
        )
      )
    } else {
      p("No deer species are present in the selected area.")
    }
  })
  
  
  # Reactive expression to store user-selected additional species
  additional_species <- reactive({
    input$additional_species  # Store selected species from dropdown
  })
  
  # Observe button click to confirm species
  observeEvent(input$confirmed_species, {
    #req(deer_species_present())  # Ensure the species present data is available
    
    detected_species <- deer_species_present()
    user_selected_species <- additional_species()  # Replace with actual user input
    
    all_species <- unique(c(detected_species, user_selected_species))
    print(all_species)
    
    confirmed_species(all_species)
  })
  
  output$debug_confirmed_species <- renderPrint({
    confirmed_species()
  })
  
  # Display confirmed species ####
  output$deer_species_list <- renderUI({
    req(confirmed_species())  # Ensure the confirmed species data is available
    
    deer_species_list <- confirmed_species()  # Get the confirmed species list
    
    if (length(deer_species_list) > 0) {
      tagList(
        strong("Final list of deer species in your area:", style = "font-size: 16px;"),
        tags$ul(
          lapply(deer_species_list, function(species) {
            tags$li(style = "font-size: 16px;", paste(species))
          })
        )
      )
    } else {
      p("No deer species have been confirmed.")
    }
  })
  
  #PLOT CURRENT RISK MAPS ------------##############
  
  output$current_risk_map <- renderLeaflet({
    req(sitebuf$dat, confirmed_species())
    detected_species <- confirmed_species()
    
    has_small_risk <- nrow(small_risk_buf$dat) > 0
    has_large_risk <- nrow(large_risk_buf$dat) > 0
    
    # Base map setup
    map <- leaflet(height='200%', 
                   width='200%',
                   options = leafletOptions(
                     zoomSnap = 0.25,  # Allows the map to stop at quarter-steps (e.g., 10, 10.25, 10.5)
                     zoomDelta = 0.10   # Clicking '+' or '-' will move the zoom by 0.5 levels instead of 1
                   )
    ) %>%
      addProviderTiles("CartoDB.Positron", group = "Grey street map") %>%
      addTiles(group = "Colour street map") %>%
      addProviderTiles("Esri.WorldImagery", group = "Satellite view") %>%
      addScaleBar(position = "bottomleft") %>%
      addPolygons(data = sitebuf$dat %>% st_transform(4326),
                  color = "#00FFFF", weight = 2, fillColor = "transparent", group = "Site buffer")
    
    # Small Deer Logic
    if (has_small_risk && any(detected_species %in% c("Roe deer", "Muntjac deer", "Chinese water deer"))) {
      small_risk_pal <- colorNumeric(palette = rev(viridisLite::plasma(5)), domain = c(1,5))
      s_img <- get_viridis_base64("plasma")
      
      map <- map %>%
        addPolygons(data = small_risk_buf$dat %>% st_transform(4326),
                    fillColor = ~small_risk_pal(6 - mean_risk),
                    fillOpacity = 1, color = "transparent", group = "Impact risk from small deer") %>%
        addControl(html = create_deer_legend(s_img, "Predicted impact risk from small deer"), 
                   position = "bottomright", layerId = "legend_small_id")
    }
    
    # Large Deer Logic
    if (has_large_risk && any(detected_species %in% c("Red deer", "Sika deer", "Fallow deer"))) {
      large_risk_pal <- colorNumeric(palette = rev(viridisLite::viridis(5)), domain = c(1,5))
      l_img <- get_viridis_base64("viridis")
      
      map <- map %>%
        addPolygons(data = large_risk_buf$dat %>% st_transform(4326),
                    fillColor = ~large_risk_pal(6 - mean_risk),
                    fillOpacity = 1, color = "transparent", group = "Impact risk from large deer") %>%
        addControl(html = create_deer_legend(l_img, "Predicted impact risk from large deer"), 
                   position = "bottomright", layerId = "legend_large_id")
    }
    
    # Decide which risk group to show/hide when both are present
    # (Added back from your previous logic to ensure initial state is correct)
    if (has_small_risk && has_large_risk) {
      map <- map %>% hideGroup("Impact risk from large deer")
    }
    
    map %>%
      addLayersControl(
        baseGroups = c("Grey street map","Colour street map", "Satellite view"),
        overlayGroups = c("Site buffer", "Impact risk from small deer", "Impact risk from large deer"),
        options = layersControlOptions(collapsed = FALSE)
      )
  })
  
  # 2. OBSERVE LAYER CHANGES WHEN USER TOGGLES BETWEEN THE RISK MAPS
  observeEvent(input$current_risk_map_groups, {
    groups <- input$current_risk_map_groups
    proxy <- leafletProxy("current_risk_map")
    # Handle Small Deer (Legend + Arrow)
    if ("Impact risk from small deer" %in% groups) {
      s_img <- get_viridis_base64("plasma")
      proxy %>% addControl(html = create_deer_legend(s_img, "Predicted impact risk from small deer"), 
                           position = "bottomright", layerId = "legend_small_id")
      shinyjs::show("arrow_small_container")
    } else {
      proxy %>% removeControl(layerId = "legend_small_id")
      shinyjs::hide("arrow_small_container")
    }
    
    # Handle Large Deer (Legend + Arrow)
    if ("Impact risk from large deer" %in% groups) {
      l_img <- get_viridis_base64("viridis")
      proxy %>% addControl(html = create_deer_legend(l_img, "Predicted impact risk from large deer"), 
                           position = "bottomright", layerId = "legend_large_id")
      shinyjs::show("arrow_large_container")
    } else {
      proxy %>% removeControl(layerId = "legend_large_id")
      shinyjs::hide("arrow_large_container")
    }
  }, ignoreNULL = FALSE)
  
  #RISK MAP MESSAGE
  output$risk_map_message <- renderUI({
    if (is.null(small_risk_buf$dat) || is.null(large_risk_buf$dat)) {
      div(style = "padding: 10px; font-size: 16px; color: #555;", 
          "Please select your area of interest then confirm deer species present.")
    } else {
      has_small_risk <- nrow(small_risk_buf$dat) > 0
      has_large_risk <- nrow(large_risk_buf$dat) > 0
      
      if (!has_small_risk && !has_large_risk) {
        div(style = "padding: 10px; font-size: 16px; color: #555;", 
            "There are currently no woodlands recorded in this area.")
      } else {
        NULL
      }
    }
  })
  
  
  #EXPORT CURRENT RISK MAP(S)---------------####
  
  risk_types <- reactive({
    detected_species <- confirmed_species()
    show_small_risk <- any(detected_species %in% c("Roe deer", "Muntjac deer", "Chinese water deer"))
    show_large_risk <- any(detected_species %in% c("Red deer", "Sika deer", "Fallow deer"))
    list(show_small_risk = show_small_risk, show_large_risk = show_large_risk)
  })
  
  output$download_shp <- downloadHandler(
    filename = function() {
      risk_info <- risk_types()
      if (risk_info$show_small_risk && risk_info$show_large_risk) {
        "deer_predicted_impact_risk_maps_shp.zip"
      } else if (risk_info$show_small_risk) {
        "small_deer_predicted_impact_risk_map_shp.zip"
      } else {
        "large_deer_predicted_impact_risk_map_shp.zip"
      }
    },
    content = function(file) {
      
      session <- shiny::getDefaultReactiveDomain()
      session$sendCustomMessage("showLoader", TRUE)
      
      on.exit({
        session$sendCustomMessage("showLoader", FALSE)
      })
      
      risk_info <- risk_types()
      
      # Temporary directory
      tmp_dir <- tempdir()
      
      # Helper to write and return all shapefile components
      write_shapefile_zip <- function(data, name_prefix) {
        out_dir <- file.path(tmp_dir, name_prefix)
        dir.create(out_dir, showWarnings = FALSE)
        out_path <- file.path(out_dir, paste0(name_prefix, ".shp"))
        st_write(data, out_path, driver = "ESRI Shapefile", delete_dsn = TRUE)
        list.files(out_dir, full.names = TRUE)
      }
      
      files_to_zip <- c()
      
      if (risk_info$show_small_risk) {
        files_to_zip <- c(files_to_zip, write_shapefile_zip(small_risk_buf$dat, "predicted_current_small_deer_impact_risk_shp"))
      }
      if (risk_info$show_large_risk) {
        files_to_zip <- c(files_to_zip, write_shapefile_zip(large_risk_buf$dat, "predicted_current_large_deer_impact_risk_shp"))
      }
      
      # Zip all selected shapefile components into one archive
      zip(zipfile = file, files = files_to_zip, flags = "-j")
    }
  )  
  
  plot_and_save_sf <- function(sf_data, filename, risk_pal, title_text) {
    req(sf_data)
    req(sitebuf$dat)
    
    # Get active session
    session <- shiny::getDefaultReactiveDomain()
    
    # Show loader
    session$sendCustomMessage("showLoader", TRUE)
    
    #Hide loader when function is finished
    on.exit({
      session$sendCustomMessage("showLoader", FALSE)
    })
    
    # Convert to Web Mercator (EPSG:3857) for compatibility with OSM tiles
    crs_mercator <- "EPSG:3857"
    sf_mercator <- st_transform(sf_data, crs_mercator)
    sitebuf_mercator <- st_transform(sitebuf$dat, crs_mercator)
    
    # #Round mean_risk to 1dp
    # sf_mercator <- sf_mercator %>%
    #   dplyr::mutate(
    #     mean_risk = round(mean_risk, 1)
    #   )
    
    # Get bounding box of the sf object and reproject to EPSG:3857
    bbox_merc <- st_bbox(sf_mercator)
    
    # Fetch OpenStreetMap tiles for the bounding box region
    map_tiles <- get_tiles(bbox_merc, provider = "OpenStreetMap", zoom = 15)
    
    bbox <- attr(map_tiles, "bbox")
  
    # Plot sf object with OpenStreetMap tiles
    p <- ggplot() +
      geom_spatraster_rgb(data = map_tiles) +  # OSM tiles as base
      geom_sf(data = sf_mercator, aes(fill = mean_risk), color = "black", linewidth = 0.1) +  # Fill based on MEAN
      #geom_sf(data = sitebuf_mercator, color = "cyan",fill = "transparent", size = 0.5)+
      geom_sf(data = sitebuf_mercator, color = "cyan",fill = "transparent", linewidth = 3)+
      #coord_sf(crs = st_crs(crs_mercator)) +
      coord_sf(
        crs = st_crs(crs_mercator),
        xlim = c(bbox["xmin"], bbox["xmax"]),
        ylim = c(bbox["ymin"], bbox["ymax"]),
        expand = FALSE
      )+
      labs(
        title = title_text,
        x = "Longitude", y = "Latitude",
        caption = "Map created using the iDeer tool V.1.0."
      ) +
      # scale_fill_gradientn(
      #   name = "Deer Risk",
      #   colours = risk_pal(100),
      #   limits = c(1,5)
      
      scale_fill_gradientn(
        name = "Impact Risk",
        colours = risk_pal(100), 
        limits = c(1, 5),
        breaks = c(5, 4, 3, 2, 1),
        guide = guide_colorbar(reverse = TRUE)
      ) +
       
      theme_map() +
      theme(
        legend.position.inside = c(1.0, 0.5),
        plot.margin = margin(2, 3, 2, 3, "cm"),
        plot.background = element_rect(colour = "black", fill = "white", linewidth = 1),
        plot.caption = element_text(hjust = 0,vjust = 0, size = 11, face = "bold"),
        legend.title = element_text(size = 14),  # increase title font size
        legend.text = element_text(size = 12),   # increase label font size
        plot.title = element_text(size = 16)
      ) +
      annotation_north_arrow(
        location = "tr", which_north = "true",
        pad_x = unit(0.1, "in"), pad_y = unit(0.2, "in"),
        style = north_arrow_orienteering
      ) +
      annotation_scale(
        location = "br",
        pad_x = unit(1.0, "in"), pad_y = unit(0.2, "in"),
        bar_cols = c("grey60", "white")
      )
    
    # Save the plot as PNG
    ggsave(filename, plot = p, width = 15, height = 12, dpi = 300, units = "in", bg = "white")
  }
  
  #Screenshot for saving png
  observeEvent(input$go_screenshot, {
    # Get active session
    session <- shiny::getDefaultReactiveDomain()
    
    # Show loader
    session$sendCustomMessage("showLoader", TRUE)
    
    # Trigger screenshot immediately
    # We target the square wrapper ID from your UI
    shinyscreenshot::screenshot(
      selector = "#map_capture_area",
      filename = paste0("ideer_screenshot_current_risk_", Sys.Date()),
      scale = 2
    )
    
    # Hide loader
    session$sendCustomMessage("showLoader", FALSE)
  })
  
  
  observeEvent(input$reset_app, {
    
    session$reload()
    
  })  
  
  #-----------------Read NFI_EW_CHUNKED polygons via WFS spatial query inside reactive element to use later ----####
  
  nfi_data <- reactive({
    
    req(sitebuf$dat, EW_grid)  # Ensure dependencies are available
    
    # --- 1. Buffer the input geometry for tile selection ---
    buffered_woods_3km <- st_buffer(sitebuf$dat, dist = 3000)
    
    # --- 2. Identify intersecting tiles from grid ---
    tiles_needed <- EW_grid[st_intersects(EW_grid, buffered_woods_3km, sparse = FALSE), ]
    tile_names_needed <- tiles_needed$tile_name
    layer_names <- paste0("nfi_EW_chunked_", tile_names_needed)
    
    # --- 3. Define WFS base URL ---
    wfs_url <- "https://frgeospatial.uk/geoserver/wfs"
    
    # --- 4. Generate bounding box string from buffered area ---
    bbox_string <- toString(st_bbox(buffered_woods_3km))
    
    # --- 5. Loop through WFS layers, query using BBOX, clean and combine ---
    nfi_list <- lapply(layer_names, function(layer_name) {
      message("Fetching NFI layer: ", layer_name)
      
      url_obj <- parse_url(wfs_url)
      url_obj$query <- list(
        service = "WFS",
        request = "GetFeature",
        typename = layer_name,
        srsName = "EPSG:27700",
        bbox = bbox_string,
        outputFormat = "application/json"
      )
      full_url <- build_url(url_obj)
      
      sf_layer <- tryCatch({
        reg <- read_sf(full_url)
        st_crs(reg) <- "EPSG:27700"
        
        reg_clean <- reg %>%
          st_cast("GEOMETRYCOLLECTION") %>%
          st_collection_extract("POLYGON") %>%
          #mutate(id = seq.int(nrow(.))) %>%
          st_make_valid()
        
        reg_clean
      }, error = function(e) {
        warning(paste("Could not read layer", layer_name, ":", e$message))
        NULL
      })
      
      return(sf_layer)
    })
    
    # --- 6. Combine all valid layers into one ---
    nfi <- do.call(rbind, Filter(Negate(is.null), nfi_list))
    
    # --- 7. Done ---
    print("NFI polygons loaded from WFS:")
    print(nfi)
    return(nfi)
  })
  
  
  #ESTABLISH NEW WOODLAND ------------------####
  
  observeEvent(input$confirmed_species, { # Trigger when species are confirmed
    if (is.null(sitebuf$dat)) {
      print("sitebuf$dat is NULL in second map:")
      return()
    }
    print("sitebuf$dat in second map:")
    print(sitebuf$dat)
    print("binary_woods_buf()")
    print(binary_woods_buf()) # Check this!
    req(binary_woods_buf())
    
    wood_palette <- c("transparent", "green")
    
    #output$new_woodland_map <- renderLeaflet({ # Render Leaflet here!
    Leaf <- leaflet(options = leafletOptions(minZoom = 4)) %>%
      addProviderTiles("CartoDB.Positron", group = "Grey street map") %>%
      addTiles(group = "Colour street map") %>%
      addProviderTiles("Esri.WorldImagery", group = "Satellite view") %>%
      addScaleBar(position = "bottomleft", options = scaleBarOptions(metric = TRUE, imperial = FALSE)) %>%
      addPolygons(
        data = sitebuf$dat %>% st_transform("epsg:4326"),
        color = "#00FFFF", # Cyan blue outline
        weight = 2, # Outline thickness
        fillColor = "transparent",
        fillOpacity = 0.1
      ) %>%
      addRasterImage(
        binary_woods_buf(),
        colors = wood_palette,
        group = "Woodland"
      ) %>%
      addDrawToolbar(
        polylineOptions = FALSE,
        polygonOptions = drawPolygonOptions(),
        circleOptions = FALSE,
        rectangleOptions = TRUE,
        markerOptions = FALSE,
        circleMarkerOptions = FALSE,
        editOptions = editToolbarOptions(edit = TRUE, remove = TRUE)
      ) %>%
      addLayersControl(
        baseGroups = c("Grey street map","Colour street map","Satellite view"),
        overlayGroups = c("Woodland"),
        options = layersControlOptions(collapsed = FALSE)
      ) 
    
    select_mapr$dat <- callModule(editMod, "new_woodland_map", leafmap = Leaf)
  })
  
  
  # Handle woodland polygon and area calculation
  woodpoly <- reactiveValues(dat = NULL)
  leafr <- reactiveValues(dat = NULL)
  
  observeEvent(input$plant, {
    
    # Show loader
    session$sendCustomMessage("showLoader", TRUE)
    
    # Add new action button to the UI
    output$plant2 <- renderUI({
      actionButton("plant2", "Run deer impact risk model",
                   style = "font-size: 14px; background-color: #28a745; color: white;"
      )
    })
    
    # Clear any existing messages
    output$planting_message <- renderUI({ NULL })
    
    # Check if any new woodlands have been drawn
    if (is.null(select_mapr$dat()) || is.null(select_mapr$dat()$finished) || nrow(select_mapr$dat()$finished) == 0) {
      output$planting_message <- renderUI({
        tags$div(
          style = "color: red; font-weight: bold; margin-top: 10px;",
          "Please draw new woodlands on the map using the tool bar on the left."
        )
      })
      
      
      
      return()
      
    }
    
    # Remove message if data is present
    output$planting_message <- renderUI({ NULL })
    
    req(select_mapr$dat()$finished)
    
    #Get max patch ID of current polygons
    max_id <- as.numeric(max(nfi_data()$OBJECTID))
    
    woodpoly$dat <- select_mapr$dat()$finished %>%
      mutate(
        woodland_ID = as.character(seq.int(nrow(.)))
      ) %>%
      st_transform("epsg:27700") %>%
      mutate(
        Shape_Area = round((as.numeric(st_area(.)) / 10000), 2),
        OBJECTID = max_id + seq_len(nrow(.)),
        MEAN = NA
      ) %>%
      dplyr::select(-c("_leaflet_id", "feature_type"))
    
    ## Check for invalid geometries here
    invalid_polygons <- !st_is_valid(woodpoly$dat)
    if (any(invalid_polygons)) {
      showModal(modalDialog(
        title = "Invalid woodland polygons",
        "Some of your woodland polygons are invalid, please draw them again. Take care to ensure the polygons have clean edges that do not overlap.",
        easyClose = TRUE,
        footer = NULL
      ))
      woodpoly$dat <- NULL  # clear invalid data
      
      #Hide loader
      session$sendCustomMessage("showLoader", FALSE)
      
      return()
    }
    
    # Continue if valid
    
    ## Check if polygons fall completely within site buffer
    outside <- !st_within(woodpoly$dat, sitebuf$dat, sparse = FALSE)[,1]
    
    if (any(outside)) {
      showModal(modalDialog(
        title = "Woodlands outside boundary",
        "Please only draw woods inside the blue circle.",
        easyClose = TRUE,
        footer = NULL
      ))
      woodpoly$dat <- NULL  # clear invalid data
      #Hide loader
      session$sendCustomMessage("showLoader", FALSE)
      return()
    }
    
    # add new polygons to back to map
    
    leafr$dat <- leaflet(woodpoly$dat) %>%
      
      addTiles() %>%
      
      addPolygons(data = st_transform(woodpoly$dat,"epsg:4326"),
                  weight = 2,
                  stroke = TRUE,
                  opacity = 1,
                  fillOpacity= 0.6,
                  smoothFactor = 0.6,
                  color="black",
                  fillColor = "green", layerId = ~woodland_ID)
    
    output$leaf1 <- renderLeaflet({
      
      print("producing leaflet map")
      
      if (is.null(woodpoly$dat)){
        return()
      }
      
      leafr$dat
      
    })
    
    woodpoly$dat <- woodpoly$dat %>% dplyr::select(-c("woodland_ID"))
    
    # Hide loader after all processing is done
    session$sendCustomMessage("showLoader", FALSE)
    
  })
  
  
  #UPDATE THE WOODLAND POLYGONS---------######
  #Add the new polygon(s) to the existing sf object for user's chosen landscape
  
  #RUN DEER RISK MAP UPDATE---------#########
  
  
  observeEvent(input$plant2, {
    
    # Show loader
    session$sendCustomMessage("showLoader", TRUE)
    
    # Update the reactive object with the confirmed data
    #woodpoly$dat <- updated_data() #TBC
    #print(woodpoly$dat)
    
    req(small_risk_buf$dat)
    print("small_risk_buf$dat:")
    print(small_risk_buf$dat)
    req(large_risk_buf$dat)
    print("large_risk_buf$dat:")
    print(large_risk_buf$dat)
    req(woodpoly$dat)
    print("woodpoly$dat:")
    print(woodpoly$dat)
    req(nfi_data())
    print("nfi_data()")
    print(nfi_data())
    
    nfi <- nfi_data()
    
    detected_species <- confirmed_species()
    show_small_risk <- any(detected_species %in% c("Roe deer", "Muntjac deer", "Chinese water deer"))
    show_large_risk <- any(detected_species %in% c("Red deer", "Sika deer", "Fallow deer"))
    
    # Determine if the detected species belong to small or large categories
    has_small_deer <- any(detected_species %in% c("Roe deer", "Muntjac deer", "Chinese water deer"))
    has_large_deer <- any(detected_species %in% c("Red deer", "Sika deer", "Fallow deer"))
    
    #Buffer site buffer again for tile selection:
    
    buffered_woods_3km <- st_buffer(sitebuf$dat, dist = 3000)
    
    # Find intersecting tiles
    tiles_needed <- EW_grid[st_intersects(EW_grid, buffered_woods_3km, sparse = FALSE), ]
    
    # Extract tile names
    tile_names_needed <- tiles_needed$tile_name
    
    #-----------------Read GB_POLYS_UNMERGED polygons via WFS spatial query ----####

    # --- 1. Define buffer polygon for spatial filtering ---
    poly <- st_buffer(sitebuf$dat, dist = 3000)

    # --- 2. Identify intersecting tiles from grid ---
    tiles_needed <- EW_grid[st_intersects(EW_grid, poly, sparse = FALSE), ]
    tile_names_needed <- tiles_needed$tile_name

    # --- 3. Define WFS base URL ---
    wfs_url <- "https://frgeospatial.uk/geoserver/wfs"

    # --- 4. Construct WFS layer names based on tiles ---
    layer_names <- paste0("GB_polys_unmerged_", tile_names_needed)

    # --- 5. Generate bounding box string from polygon ---
    bbox_string <- toString(st_bbox(poly))

    # --- 6. Loop over WFS layers, query using bbox, clean, and combine ---
    gb_polys_list <- lapply(layer_names, function(layer_name) {
      message("Fetching GB_polys layer: ", layer_name)

      # Build WFS URL for this layer
      url_obj <- parse_url(wfs_url)
      url_obj$query <- list(
        service = "WFS",
        request = "GetFeature",
        typename = layer_name,
        srsName = "EPSG:27700",
        bbox = bbox_string,
        outputFormat = "application/json"
      )
      full_url <- build_url(url_obj)

      # Read and clean geometry
      sf_layer <- tryCatch({
        reg <- read_sf(full_url)
        st_crs(reg) <- "EPSG:27700"

        reg_clean <- reg %>%
          st_cast("GEOMETRYCOLLECTION") %>%
          st_collection_extract("POLYGON") %>%
          mutate(id = seq.int(nrow(.))) %>%
          st_make_valid()

        reg_clean
      }, error = function(e) {
        warning(paste("Could not read layer", layer_name, ":", e$message))
        NULL
      })

      return(sf_layer)
    })

    # --- 7. Combine all valid layers into one ---
    GB_polys_unmerged <- do.call(rbind, Filter(Negate(is.null), gb_polys_list))

    # --- 8. Clean up unwanted columns ---
    GB_polys_unmerged <- GB_polys_unmerged %>%
      dplyr::select(-c("CATEGORY", "IFT_IOA", "COUNTRY", "Area_Ha", "Shape__Are", "Shape__Len"))

    # --- 9. Done ---
    print("GB_polys_unmerged")
    print(GB_polys_unmerged)


    # #-----------------Make template polygons for raster extraction from current risk maps ----####

    if (nrow(small_risk_buf$dat) > 0) {

      # Small risk takes priority
      nfi_lcm_2023_polys <- small_risk_buf$dat

    } else if (nrow(large_risk_buf$dat) > 0) {

      # Large risk is selected only if small risk is absent
      nfi_lcm_2023_polys <- large_risk_buf$dat

    } else {

      # Handle case where neither risk is present
      nfi_lcm_2023_polys <- NULL
      message("Warning: Neither small nor large deer risk data was available.")

    }

    print("nfi_lcm_2023_polys as template from risk maps")
    print(nfi_lcm_2023_polys)

    #-----------------Read in DAMS files from WCS using bounding extent ----####
    
    # --- 1. Buffer the polygon for spatial filtering ---
    poly <- st_buffer(sitebuf$dat, dist = 3000)
    
    # --- 2. Get needed tile names and corresponding WCS coverage names ---
    tile_names_needed <- tiles_needed$tile_name
    coverage_names <- paste0("focal1000_dams_tile_", tile_names_needed)
    
    # --- 3. Create extent object from buffered polygon ---
    query_extent <- terra::ext(poly)  # terra::ext() object
    
    # --- 4. Define base WCS URL prefix ---
    baseurl <- "WCS:https://frgeospatial.uk/geoserver/wcs?coverage="
    
    # --- 5. Loop through coverages and read using WCS URL and bounding window ---
    rasters <- list()
    
    for (i in seq_along(coverage_names)) {
      name <- coverage_names[i]
      urlx <- paste0(baseurl, name)
      
      message("Reading WCS coverage: ", name)
      
      rst <- tryCatch({
        terra::rast(urlx, win = query_extent)
      }, error = function(e) {
        warning(paste("Failed to read raster:", name, ":", e$message))
        NULL
      })
      
      if (!is.null(rst)) {
        # Crop precisely to buffered polygon
        rst_crop <- terra::crop(rst, terra::vect(poly))
        rasters[[length(rasters) + 1]] <- rst_crop
      }
    }
    
    # --- 6. Combine rasters via mosaic if needed ---
    if (length(rasters) >= 2) {
      Focal1000_DAMS <- do.call(terra::mosaic, rasters)
    } else if (length(rasters) == 1) {
      Focal1000_DAMS <- rasters[[1]]
    } else {
      stop("No WCS raster coverages found or successfully read.")
    }
    
    # --- 7. Assign CRS ---
    terra::crs(Focal1000_DAMS) <- "EPSG:27700"
    
    #Get NFI data from reactive element
    
    #Update woodland polygons
    
    existing_wood_polys_unmerged <- GB_polys_unmerged
    # existing_wood_polys_merged <- GB_polys_merged
    
    print("existing_wood_polys_unmerged:")
    print(existing_wood_polys_unmerged)
    print("sitebuf$dat:")
    print(sitebuf$dat)
    
    #Make template raster with correct pixels
    woods_cropped <- raster::crop(woods, sitebuf$dat)
    woods_cropped <- raster::mask(woods_cropped, sitebuf$dat)
    
    site_wood_output <- update_wood_polys_nfi(wood_polys = woodpoly$dat,
                                              sitebuf = sitebuf$dat,
                                              lcm = lcm,
                                              #existing_wood_polys_merged = existing_wood_polys_merged,
                                              existing_wood_polys_unmerged = existing_wood_polys_unmerged,
                                              nfi = nfi,
                                              nfi_lcm_2023_polys = nfi_lcm_2023_polys,
                                              template_raster = woods_cropped)
    
    # Extract the elements to use in update functions for risk maps:
    #site_woods_merged <- site_wood_output$site_woods_merged
    site_woods_unmerged <- site_wood_output$site_woods_unmerged
    woods_binary <- site_wood_output$woods_binary
    lcm_updated <- site_wood_output$lcm_updated
    buffered_woods_3km <- site_wood_output$buffered_woods_3km
    nfi_lcm_unmerged_polys <- site_wood_output$nfi_lcm_unmerged_polys
    
    #save these layers to inspect
    site_woods_unmerged <- st_as_sf(site_woods_unmerged)
    buffered_woods_3km <- st_as_sf(buffered_woods_3km)
    nfi_lcm_unmerged_polys <- st_as_sf(nfi_lcm_unmerged_polys)
    
    print("nfi_lcm_unmerged_polys:")
    print(nfi_lcm_unmerged_polys)
    
    # Run the models based on the detected species
    if (has_small_deer && !has_large_deer) {
      print("Running small deer species model")
      # Only small deer species detected
      
      updated_small_risk_buf$dat <- small_deer_risk_update(
        wood_polys = woodpoly$dat,      # New woodland polygon(s)
        sitebuf = sitebuf$dat,          # User's landscape extent
        lcm = lcm,                      # CEH GB land cover map 2023
        lf = lf,                        # Linear feature GB raster
        #site_woods_merged = site_woods_merged,            #merged woodland polygons
        site_woods_unmerged = site_woods_unmerged,          #unmerged woodland polygons
        lcm_updated = lcm_updated,
        buffered_woods_3km = buffered_woods_3km,
        nfi = nfi,
        small_connectivity_cpt_df = small_connectivity_cpt_df,
        small_attraction_cpt_df = small_attraction_cpt_df,
        small_wood_food_value_cpt_df = small_wood_food_value_cpt_df,
        small_risk_cpt_df_full = small_risk_cpt_df_full,
        nfi_lcm_unmerged_polys =  nfi_lcm_unmerged_polys) %>%
        select(-c(patch_ID)) %>%
        dplyr::rename(
          area_m2 = Shape_Area,
          #wood_ID = patch_ID,
          mean_risk = mean_small_deer_impact_risk)
      
      # Modify output after function completes
      updated_small_risk_buf$dat <- updated_small_risk_buf$dat %>%
        dplyr::mutate(
          Area_Ha = as.numeric(st_area(.)) / 10000) %>% 
        dplyr::filter(Area_Ha >= 0.01) %>%
        dplyr::select(-any_of("area_m2")) %>%
        mutate(wood_ID = 1:nrow(.))
      
      
    } else if (has_large_deer && !has_small_deer) {
      print("Running large deer species model")
      # Only large deer species detected
      
      updated_large_risk_buf$dat <- large_deer_risk_update(
        wood_polys = woodpoly$dat,      # New woodland polygon(s)
        sitebuf = sitebuf$dat,          # User's landscape extent
        lcm = lcm,                      # CEH GB land cover map 2023
        dams = Focal1000_DAMS,                    # GB dams map
        #site_woods_merged = site_woods_merged,            #merged woodland polygons
        site_woods_unmerged = site_woods_unmerged,          #unmerged woodland polygons
        lcm_updated = lcm_updated,
        buffered_woods_3km = buffered_woods_3km,
        nfi=nfi,
        large_attraction_cpt_df = large_attraction_cpt_df,
        large_thermoreg_cpt_df = large_thermoreg_cpt_df,
        large_wood_food_value_cpt_df = large_wood_food_value_cpt_df,
        large_risk_cpt_df = large_risk_cpt_df,
        nfi_lcm_unmerged_polys = nfi_lcm_unmerged_polys) %>%
        dplyr::select(-c(patch_ID)) %>%
          dplyr::rename(
            area_m2 = Shape_Area,
            #wood_ID = patch_ID,
            mean_risk = mean_large_deer_impact_risk)
          
      
      # Modify output after function completes
      updated_large_risk_buf$dat <- updated_large_risk_buf$dat %>%
        dplyr::mutate(
          Area_Ha = as.numeric(st_area(.)) / 10000) %>% 
        dplyr::filter(Area_Ha >= 0.01) %>%
        dplyr::select(-any_of("area_m2")) %>%
        mutate(wood_ID = 1:nrow(.))
      
    } else if (has_small_deer && has_large_deer) {
      print("Running both models")
      # Both small and large deer species detected
      
      updated_small_risk_buf$dat <- small_deer_risk_update(
        wood_polys = woodpoly$dat,      # New woodland polygon(s)
        sitebuf = sitebuf$dat,          # User's landscape extent
        lcm = lcm,                      # CEH GB land cover map 2023
        lf = lf,                        # Linear feature GB raster
        #site_woods_merged = site_woods_merged,            #merged woodland polygons
        site_woods_unmerged = site_woods_unmerged,          #unmerged woodland polygons
        lcm_updated = lcm_updated,
        buffered_woods_3km = buffered_woods_3km,
        nfi = nfi,
        small_connectivity_cpt_df = small_connectivity_cpt_df,
        small_attraction_cpt_df = small_attraction_cpt_df,
        small_wood_food_value_cpt_df = small_wood_food_value_cpt_df,
        small_risk_cpt_df_full = small_risk_cpt_df_full,
        nfi_lcm_unmerged_polys = nfi_lcm_unmerged_polys) %>%
        dplyr::select(-c(patch_ID)) %>%
        dplyr::rename(
          area_m2 = Shape_Area,
          #wood_ID = patch_ID,
          mean_risk = mean_small_deer_impact_risk)

      # Modify output after function completes
      updated_small_risk_buf$dat <- updated_small_risk_buf$dat %>%
        dplyr::mutate(
          Area_Ha = as.numeric(st_area(.)) / 10000) %>% 
        dplyr::filter(Area_Ha >= 0.01) %>%
        dplyr::select(-any_of("area_m2")) %>%
        mutate(wood_ID = 1:nrow(.))
      
      
      #LARGE DEER MAP UPDATE ------------#
      
      updated_large_risk_buf$dat <- large_deer_risk_update(
        sitebuf = sitebuf$dat,          # User's landscape extent
        lcm = lcm,                      # CEH GB land cover map 2023
        dams = Focal1000_DAMS,                    # GB dams map
        #site_woods_merged = site_woods_merged,            #merged woodland polygons
        site_woods_unmerged = site_woods_unmerged,          #unmerged woodland polygons
        lcm_updated = lcm_updated,
        buffered_woods_3km = buffered_woods_3km,
        nfi=nfi,
        large_attraction_cpt_df = large_attraction_cpt_df,
        large_thermoreg_cpt_df = large_thermoreg_cpt_df,
        large_wood_food_value_cpt_df = large_wood_food_value_cpt_df,
        large_risk_cpt_df = large_risk_cpt_df,
        nfi_lcm_unmerged_polys = nfi_lcm_unmerged_polys) %>%
        select(-c(patch_ID)) %>%
        dplyr::rename(
          area_m2 = Shape_Area,
          #wood_ID = patch_ID,
          mean_risk = mean_large_deer_impact_risk) 

      # Modify output after function completes
      updated_large_risk_buf$dat <- updated_large_risk_buf$dat %>%
        dplyr::mutate(
          Area_Ha = as.numeric(st_area(.)) / 10000) %>% 
        dplyr::filter(Area_Ha >= 0.01) %>%
        dplyr::select(-any_of("area_m2")) %>%
        mutate(wood_ID = 1:nrow(.))
       
    } else {
      # Notify user if no relevant deer species are detected
      showNotification("No relevant deer species detected. No risk maps generated.", type = "warning")
    }
    
    # Hide loader
    session$sendCustomMessage("showLoader", FALSE)
    
    showModal(modalDialog(
      title = "Your woodland planting is complete!",
      "Please click on the 'Predicted impact risk after planting' tab to view your updated impact risk maps.",
      easyClose = TRUE,
      footer = NULL
    ))
    
  })
  
  
  #PLOT FUTURE RISK MAPS-----------------####
  
  output$future_risk_map <- renderLeaflet({
    # Only require site buffer and species; risk data is handled conditionally
    req(sitebuf$dat, confirmed_species())
    
    detected_species <- confirmed_species()
    
    # Determine presence based on data existence AND species selection
    # Using is.null check is safer than req() inside renderLeaflet
    has_small_data <- !is.null(updated_small_risk_buf$dat)
    has_large_data <- !is.null(updated_large_risk_buf$dat)
    
    show_small_risk <- has_small_data && any(detected_species %in% c("Roe deer", "Muntjac deer", "Chinese water deer"))
    show_large_risk <- has_large_data && any(detected_species %in% c("Red deer", "Sika deer", "Fallow deer"))
    
    map2 <- leaflet(height = '100%', 
                    width = '100%',
                    options = leafletOptions(
                      zoomSnap = 0.25,  # Allows the map to stop at quarter-steps (e.g., 10, 10.25, 10.5)
                      zoomDelta = 0.10   # Clicking '+' or '-' will move the zoom by 0.5 levels instead of 1
                    )
                  ) %>%
      addProviderTiles("CartoDB.Positron", group = "Grey street map") %>%
      addTiles(group = "Colour street map") %>%
      addProviderTiles("Esri.WorldImagery", group = "Satellite view") %>%
      addScaleBar(position = "bottomleft") %>%
      addPolygons(data = sitebuf$dat %>% st_transform(4326),
                  color = "#00FFFF", weight = 2, fillColor = "transparent", group = "Site buffer")
    
    # Small Deer Logic
    if (show_small_risk) {
      updated_small_risk_pal <- colorNumeric(palette = rev(viridisLite::plasma(5)), domain = c(1,5))
      s_img_updated <- get_viridis_base64("plasma")
      
      map2 <- map2 %>%
        addPolygons(data = updated_small_risk_buf$dat %>% st_transform(4326),
                    fillColor = ~updated_small_risk_pal(6 - mean_risk),
                    fillOpacity = 1,
                    # Logic for the outline color
                    color = ~ifelse(new_woodland == "Yes", "black", "transparent"), 
                    # Logic for the outline thickness
                    weight = ~ifelse(new_woodland == "Yes", 1, 0),
                    opacity = 1,
                    group = "Updated impact risk from small deer"
        ) %>%
        addControl(html = create_deer_legend(s_img_updated, "Predicted impact risk from small deer after woodland planting"), 
                   position = "bottomright", layerId = "future_legend_small")
    }
    
    # Large Deer Logic
    if (show_large_risk) {
      updated_large_risk_pal <- colorNumeric(palette = rev(viridisLite::viridis(5)), domain = c(1,5))
      l_img_updated <- get_viridis_base64("viridis")
      
      map2 <- map2 %>%
        addPolygons(data = updated_large_risk_buf$dat %>% st_transform(4326),
                    fillColor = ~updated_large_risk_pal(6 - mean_risk),
                    fillOpacity = 1,
                    # Logic for the outline color
                    color = ~ifelse(new_woodland == "Yes", "black", "transparent"), 
                    # Logic for the outline thickness
                    weight = ~ifelse(new_woodland == "Yes", 1, 0),
                    opacity = 1,
                    group = "Updated impact risk from large deer"
        ) %>%
        addControl(html = create_deer_legend(l_img_updated, "Predicted impact risk from large deer after woodland planting"), 
                   position = "bottomright", layerId = "future_legend_large")
    }
    
    # Initial layer visibility
    if (show_small_risk && show_large_risk) {
      map2 <- map2 %>% hideGroup("Updated impact risk from large deer")
    }
    
    map2  <- map2 %>%
      addLayersControl(
        baseGroups = c("Grey street map", "Colour street map", "Satellite view"),
        overlayGroups = c("Site buffer", "Updated impact risk from small deer", "Updated impact risk from large deer"),
        options = layersControlOptions(collapsed = FALSE)
      )
    map2
  })
  
  #Reset button
  observeEvent(input$reset_app_future, {
    
    session$reload()
    
  })  
  
  # 2. OBSERVE LAYER CHANGES FOR FUTURE MAP
  observeEvent(input$future_risk_map_groups, {
    groups <- input$future_risk_map_groups
    proxy <- leafletProxy("future_risk_map")
    
    # Handle Small Deer (Use the _future IDs we created in the UI)
    if ("Updated impact risk from small deer" %in% groups) {
      s_img_updated <- get_viridis_base64("plasma")
      proxy %>% addControl(html = create_deer_legend(s_img_updated, "Predicted impact risk from small deer after woodland planting"), 
                           position = "bottomright", layerId = "future_legend_small")
      shinyjs::show("arrow_small_container_future")
    } else {
      proxy %>% removeControl(layerId = "future_legend_small")
      shinyjs::hide("arrow_small_container_future") 
    }
    
    # Handle Large Deer
    if ("Updated impact risk from large deer" %in% groups) {
      l_img_updated <- get_viridis_base64("viridis")
      proxy %>% addControl(html = create_deer_legend(l_img_updated, "Predicted impact risk from large deer after woodland planting"), 
                           position = "bottomright", layerId = "future_legend_large")
      shinyjs::show("arrow_large_container_future")
    } else {
      proxy %>% removeControl(layerId = "future_legend_large")
      shinyjs::hide("arrow_large_container_future")
    }
  }, ignoreNULL = FALSE)
  
  # FUTURE RISK MAP MESSAGE
  output$future_risk_map_message <- renderUI({
    # Check if the inputs exist yet
    if (is.null(woodpoly$dat)) {
      return(div(style = "padding: 10px; font-size: 16px; color: #555;", 
                 "Please digitize new woodland polygons in the 'Digitize new woodland' tab first."))
    }
    
    # Check if the models have actually produced data yet
    has_small_future <- !is.null(updated_small_risk_buf$dat)
    has_large_future <- !is.null(updated_large_risk_buf$dat)
    
    detected_species <- confirmed_species()
    needs_small <- any(detected_species %in% c("Roe deer", "Muntjac deer", "Chinese water deer"))
    needs_large <- any(detected_species %in% c("Red deer", "Sika deer", "Fallow deer"))
    
    # State: Calculating (User clicked 'Run' but data hasn't arrived)
    if ((needs_small && !has_small_future) || (needs_large && !has_large_future)) {
      return(div(style = "padding: 15px; background: #fff3cd; border: 1px solid #ffeeba; color: #856404; text-align: center;",
                 tags$i(class = "fa fa-refresh fa-spin"), 
                 " Calculating future risk based on new planting... This may take a moment."))
    }
    
    # State: No results found
    if (!has_small_future && !has_large_future) {
      return(div(style = "padding: 10px; font-size: 16px; color: #d9534f;", 
                 "No woodland found to analyze. Please ensure you have added polygons inside the buffer."))
    }
    
    # State: Success (Return NULL to hide the message box so the map is clear)
    return(NULL)
  })
  
  #EXPORT FUTURE RISK MAPS----------####
  
  risk_types <- reactive({
    detected_species <- confirmed_species()
    show_small_risk <- any(detected_species %in% c("Roe deer", "Muntjac deer", "Chinese water deer"))
    show_large_risk <- any(detected_species %in% c("Red deer", "Sika deer", "Fallow deer"))
    list(show_small_risk = show_small_risk, show_large_risk = show_large_risk)
  })
  
  output$download_shp2 <- downloadHandler(
    filename = function() {
      risk_info <- risk_types()
      if (risk_info$show_small_risk && risk_info$show_large_risk) {
        "predicted_deer_impact_risk_after_planting_shapefiles.zip"
      } else if (risk_info$show_small_risk) {
        "predicted_small_deer_impact_risk_after_planting_shapefile.zip"
      } else {
        "predicted_large_deer_impact_risk_after_planting_shapefile.zip"
      }
    },
    content = function(file) {
      
      session <- shiny::getDefaultReactiveDomain()
      session$sendCustomMessage("showLoader", TRUE)
      
      on.exit({
        session$sendCustomMessage("showLoader", FALSE)
      })
      
      risk_info <- risk_types()
      
      # Temporary directory
      tmp_dir <- tempdir()
      
      # Helper to write and return all shapefile components
      write_shapefile_zip <- function(data, name_prefix) {
        out_dir <- file.path(tmp_dir, name_prefix)
        dir.create(out_dir, showWarnings = FALSE)
        out_path <- file.path(out_dir, paste0(name_prefix, ".shp"))
        st_write(data, out_path, driver = "ESRI Shapefile", delete_dsn = TRUE)
        list.files(out_dir, full.names = TRUE)
      }
      
      files_to_zip <- c()
      
      if (risk_info$show_small_risk) {
        files_to_zip <- c(files_to_zip, write_shapefile_zip(updated_small_risk_buf$dat, "predicted_small_deer_impact_risk_after_planting_shp"))
      }
      if (risk_info$show_large_risk) {
        files_to_zip <- c(files_to_zip, write_shapefile_zip(updated_large_risk_buf$dat, "predicted_large_deer_impact_risk_after_planting_shp"))
      }
      
      # Zip all selected shapefile components into one archive
      zip(zipfile = file, files = files_to_zip, flags = "-j")
    }
  )  
  
  plot_and_save_sf_future_risk <- function(sf_data, filename, risk_pal, title_text, fill_col) {
    req(sf_data)
    req(sitebuf$dat)

    # Get active session
    session <- shiny::getDefaultReactiveDomain()

    # Show loader
    session$sendCustomMessage("showLoader", TRUE)

    #Hide loader when function is finished
    on.exit({
      session$sendCustomMessage("showLoader", FALSE)
    })

    # Convert to Web Mercator (EPSG:3857) for compatibility with OSM tiles
    crs_mercator <- "EPSG:3857"
    sf_mercator <- st_transform(sf_data, crs_mercator)
    sitebuf_mercator <- st_transform(sitebuf$dat, crs_mercator)

    # Get bounding box of the sf object and reproject to EPSG:3857
    bbox_merc <- st_bbox(sf_mercator)

    # Fetch OpenStreetMap tiles for the bounding box region
    map_tiles <- get_tiles(bbox_merc, provider = "OpenStreetMap", zoom = 15)

    bbox <- attr(map_tiles, "bbox")

    # Plot sf object with OpenStreetMap tiles
    p <- ggplot() +
      geom_spatraster_rgb(data = map_tiles) +  # OSM tiles as base
      geom_sf(data = sf_mercator, aes(fill = .data[[fill_col]]), color = "black", linewidth = 0.1) +  # Fill based on MEAN
      geom_sf(data = sitebuf_mercator, color = "cyan",fill = "transparent",linewidth = 3)+
      #coord_sf(crs = st_crs(crs_mercator)) +
      coord_sf(
        crs = st_crs(crs_mercator),
        xlim = c(bbox["xmin"], bbox["xmax"]),
        ylim = c(bbox["ymin"], bbox["ymax"]),
        expand = FALSE
      )+
      labs(
        title = title_text,
        x = "Longitude", y = "Latitude",
        caption = "Map created using the iDeer tool V.1.0."
      ) +
      # scale_fill_gradientn(
      #   name = "Impact Risk",
      #   colours = risk_pal(100),
      #   limits = c(1,5)

      scale_fill_gradientn(
        name = "Impact risk",
        colours = risk_pal(100),
        limits = c(1, 5),
        breaks = c(5, 4, 3, 2, 1),
        guide = guide_colorbar(reverse = TRUE)

      ) +
      theme_map() +
      theme(
        legend.position.inside = c(1.0, 0.5),
        plot.margin = margin(2, 3, 2, 3, "cm"),
        plot.background = element_rect(colour = "black", fill = "white", linewidth = 1),
        plot.caption = element_text(hjust = 0,vjust = 0, size = 11, face = "bold"),
        legend.title = element_text(size = 14),  # increase title font size
        legend.text = element_text(size = 12),   # increase label font size
        plot.title = element_text(size = 16)
      ) +
      annotation_north_arrow(
        location = "tr", which_north = "true",
        pad_x = unit(0.1, "in"), pad_y = unit(0.2, "in"),
        style = north_arrow_orienteering
      ) +
      annotation_scale(
        location = "br",
        pad_x = unit(1.0, "in"), pad_y = unit(0.2, "in"),
        bar_cols = c("grey60", "white")
      )

    # Save the plot as PNG
    ggsave(filename, plot = p, width = 15, height = 12, dpi = 300, units = "in", bg = "white")
  }
  
  observeEvent(input$go_screenshot2, {
    # Get active session
    session <- shiny::getDefaultReactiveDomain()
    
    # Show loader
    session$sendCustomMessage("showLoader", TRUE)
    
    # Trigger screenshot immediately
    # We target the square wrapper ID from your UI
    shinyscreenshot::screenshot(
      selector = "#map_capture_area_future",
      filename = paste0("ideer_screenshot_future_risk_", Sys.Date()),
      scale = 2
    )
    
    # Hide loader
    session$sendCustomMessage("showLoader", FALSE)
  })
  
  
  
  observeEvent(input$reset_app2, {
    
    session$reload()
    
  })  
  
}

#UI-----------##############

ui <- dashboardPage(
  
  skin = "blue",
  
  dashboardHeader(disable=TRUE),
  dashboardSidebar(
    
    #Images must be in a folder named "www" in the same directory as the RShiny scripts (server & ui)
    
    
    #iDeer logo
    div(img(src = "ideer_logo_white.png", width = "70%"), style = "text-align:center; margin-bottom: 10px;"),
    
    div(style = "text-align: center; font-size: 24px; font-weight: bold; text-decoration: underline; padding: 10px;",
        "iDeer Tool V.1.0.",
      
      # Southampton logo
      div(img(src = "UoS_logo_white.png", width = "70%"), style = "text-align:center; margin-bottom: 10px;"),
      
      # Reading logo
      div(img(src = "University-of-Reading.png", width = "70%"), style = "text-align:center;margin-bottom: 10px;"),
      
      # Bangor logo
      div(img(src = "B2_WHITE.png", width = "70%"), style = "text-align:center;margin-bottom: 10px;"),
      
      # Forest Research logo
      div(img(src = "FR logo white.png", width = "70%"), style = "text-align:center;margin-bottom: 10px;"),
      
      # NINA logo
      div(img(src = "nina.png", width = "70%"), style = "text-align:center;margin-bottom: 10px;"),
      
      # Sylva Foundation logo
      div(img(src = "sylva_logo_white.png", width = "70%"), style = "text-align:center;margin-bottom: 10px;"),
      
      # Woodland Trust logo
      div(img(src = "Woodland-Trust-Logo-500x300px.png", width = "70%"), style = "text-align:center;margin-bottom: 10px;"),
      
      # Treescapes Logo
      div(img(src = "treescapes_logo.png", width = "70%"), style = "text-align:center;"),
      div(img(src = "UKRI logo [W].png", width = "70%"), style = "text-align:center;margin-bottom: 10px;"),
      
    )),
  
  dashboardBody(
    
    #Configure loader graphic ----####
    
    useShinyjs(),
    
    tags$head(tags$script(HTML("
  Shiny.addCustomMessageHandler('showLoader', function(show) {
    var loader = document.getElementById('loader-container');
    if (loader) {
      loader.style.display = show ? 'flex' : 'none';
    }
  });
"))),
    
    tags$style(HTML("
    #loader-container {
      display: flex;
      justify-content: center;
      align-items: center;
      animation: pulse 2s infinite;
    }
    #loader-container img.loader-img {
      width: 200px;
      height: 200px;
    }
    @keyframes pulse {
      0% { transform: scale(1); }
      50% { transform: scale(1.1); }
      100% { transform: scale(1); }
    }
  ")),
    
    uiOutput("loader_container"), 
    
    #Configure tabs ------####
    
    fluidRow(
      column(width = 10, offset = 2,
             
             tabsetPanel(id = "master", selected = 1,
                         
                         tabPanel("Welcome", value = 1,
                                  
                                  fluidRow(
                                    column(12, align = "center",
                                           p("Welcome to the iDeer Tool!",
                                             style = "font-size: 20px; font-weight: bold; font-style: italic; color: #2c3e50;")
                                    )
                                  ),
                                  
                                  fluidRow(
                                    column(2),  # left spacing
                                    column(8,
                                           
                                           p("UPDATE March 2026: Pre-print for the science behind the iDeer Tool is now available:",
                                             style = "font-size: 18px;font-weight: bold"),
                                           
                                           tags$a(
                                             href = "https://ecoevorxiv.org/repository/view/12101/",
                                             target = "_blank",
                                             class = "btn btn-success", # Uses Bootstrap's button styling
                                             style = "color: white; background-color: #27ae60; border-color: #27ae60; text-decoration: none;",
                                             "Open pre-print"
                                           ),
                                           
                                           p("We designed the iDeer Tool to support decision-making about deer management and potential new woodland planting. If you are currently managing a woodland or thinking about planting new trees, this tool is for you!
                                           When managing trees, it is important to consider the risk of deer impacts.
  There is evidence that where deer feed in woodlands frequently and intensively, they can damage the bark of established trees, stunt tree growth and,
   in some cases, prevent woodland regeneration by browsing new saplings. This poses a problem if you are growing trees for timber, creating new woodlands, or wanting healthy understory vegetation to conserve woodland biodiversity. Information on the risk of deer impacts to your woodlands and trees may be helpful in your management decsions.", 
                                             style = "font-size: 16px;"),
                                           
                                           p("What does the tool do?", style = "font-size: 20px; font-weight: bold; font-style: italic; color: #2c3e50;"),
                                           
                                           p("The iDeer tool predicts relative deer impact risk to existing woodlands throughout England and Wales. Once you have viewed the current impact risk for your area of interest, you can then test different
               woodland planting scenarios by drawing new woodland(s) in the landscape. The tool will then calculate the relative deer damage risk to new woodlands, and recalculate
               the risk to surrounding woodlands. To model relative deer impact risk in the absence of national deer density data, we have made the assumption that England and Wales are entirely occupied by deer
               populations of moderate density.", style = "font-size: 16px;"),
                                           
                                           p("The iDeer tool uses two models to predict deer impact risk: one for small deer (roe, Reeve's muntjac and Chinese water deer) and one for large deer (fallow, red, sika). 
              The modelling framework we used is called Bayesian Belief Network (BBN) modelling. Below are illustrations of the BBNs used in the tool.", style = "font-size: 16px;"),
                                           
                                           div(
                                             style = "display: flex; justify-content: center; gap: 20px; margin-bottom: 10px;",
                                             img(src = "bbn_structure_figure_v3_small_deer.png", style = "width: 70%;"),
                                             img(src = "bbn_structure_figure_v3_large_deer.png", style = "width: 70%;")
                                           ),
                                           
                                           p("Disclaimer", style = "font-size: 20px; font-weight: bold; font-style: italic; color: #2c3e50;"),
                                           
                                           
                                           p("The Deer Impact Risk maps produced by the iDeer Tool are designed to support local- and
landscape-level management decisions by highlighting woodlands that are potentially at high risk of
deer impacts. However, the maps use several underlying assumptions (see technical document under 'Further information'). As such,
they complement rather than replace on-the-ground assessment of deer activity and impacts and
professional advice on deer management. The maps are not a substitute for field surveys of deer impacts or seeking advice from professional deer managers.
                                             The creators of the iDeer Tool are not responsible for outcomes of any actions or decisions informed by using the Tool. 
In England, the Forestry Commission Deer Management Officer for your region can advise you on
how to monitor and manage deer in your woodland. Additionally, NGOs such as the British Deer
Society, the Game & Wildlife Conservation Trust and the Deer Initiative Partnership also offer deer
management guidance for England and Wales.
",style = "font-size: 16px;"),
                                           
                                        p("Terms and conditions", style = "font-size: 20px; font-weight: bold; font-style: italic; color: #2c3e50;"),
                                           
                                        p("The iDeer Tool and its contents, ideas and inception was funded as a Future of UK Treescapes project
by the Natural Environment Research Council (‘iDeer: An Integrated Deer Management Platform’,
grant no. NE/X003973/1). The information it provides is freely given to support decisions around
deer management at the local and landscape scale, not for large businesses, companies or
organisations to use to gain profit. It is not to be copied for financial gain. Its information is not to be
corrupted in any way. Attribution must be made clearly to iDeer if any parts of the tool or
information herein are to be copied or published in any way by anybody",style = "font-size: 16px;"),
                                        
                                           p("Further information", style = "font-size: 20px; font-weight: bold; font-style: italic; color: #2c3e50;"),
                                           p(HTML('For full documentation of the methodology used to build this tool, please download this <a href="iDeer tool technical doc_V1.pdf" target="_blank">PDF</a>.'), 
                                             style = "font-size: 16px;"),
                                           
                                           p("Feedback form", style = "font-size: 20px; font-weight: bold; font-style:  italic; color: #2c3e50;"),
                                           p("There's always room for improvement! We would really appreciate your feedback on the iDeer tool - please click the button below to complete our short feedback form:", style = "font-size: 16px;"),
                                           
                                           actionButton(
                                             "back", "Feedback",
                                             style = "color: white; background-color: #27ae60; border-color: #27ae60;",
                                             onclick = "window.open('https://docs.google.com/forms/d/e/1FAIpQLScLBTh8ZUegzEBrxpTC5DD8SqtxhYg_udZQ2nxuW9B6DPlY7g/viewform?usp=sharing&ouid=108163412339693555853', '_blank')"
                                           )),
                                           p("Contact us", style = "font-size: 20px; font-weight: bold; font-style:  italic; color: #2c3e50;"),
                                           p("If you'd like to contact us directly with any questions, comments or ideas for future work, please email us at ideer.enquiries@gmail.com", style = "font-size: 16px;"),
                                           column(2)  # right spacing
                                  )
                         ),
                         
                         tabPanel("Instructions",
                                  
                                  fluidRow(
                                    column(12, align = "center",
                                           p("Instructions for using the iDeer Tool",
                                             style = "font-size: 20px; font-weight: bold; font-style: italic; color: #2c3e50;")
                                    )
                                  ),
                                  #value = 2,
                                  fluidRow(
                                    column(2),  # left spacing
                                    column(8,
                                           
                                    p(HTML('For a printable version of these instructions, please download this <a href="Instructions for using the iDeer tool_V1.pdf" target="_blank">PDF</a>.'), 
                                    style = "font-size: 16px; font-weight: bold;"),
                                    
                                    p("Select your area", style = "font-size: 20px; font-style: italic; color: #2c3e50;"),
                                    
                                    p("This tab presents an interactive map centred on England and Wales. You can toggle between a basic street map or ESRI World Imagery using the buttons in the top right of the tab (1). You can also zoom in and out by clicking the +/- buttons in the top left of the tab, or using your mouse (2).
You can select your area of interest by clicking anywhere on the map within England or Wales (3). This will generate a blue circle around the clicked point. If the clicked point is not in the right place, just click on the correct point and the circle will appear around the new clicked position. You can also change the radius of this circle using the sliding bar in the bottom left of the tab, from 3 km up to 8km (4). The size of this circle determines the size of the deer impact risk map(s) that will be generated by the tool.
", style = "font-size: 16px;"),
                                      
                                      div(
                                        style = "display: flex; justify-content: center; gap: 20px; margin-bottom: 10px;",
                                        img(src = "select_your_area_v2.png", style = "width: 70%;"),
                                        img(src = "blue-circle.png", style = "width: 70%;")
                                      ),
                      
                                    p("Deer species", style = "font-size: 20px; font-style: italic; color: #2c3e50;"),
                                    
                                    p("This tab uses species distribution maps from the British Deer Society to identify which deer species are present within 10km of your area of interest and therefore may be present in your area. 
                                      You may have seen deer in your area that are not included on the list. You can add additional species to the list using the drop-down box (5). 
                                      When you are happy with the deer species list, click 'Confirm deer species' (6).", style = "font-size: 16px;"),
                                    
                                    div(
                                      style = "display: flex; justify-content: center; gap: 20px; margin-bottom: 10px;",
                                      
                                      img(src = "deer_species_clicks.png", style = "width: 80%;")
                                    ),
                                    
                                    p("Current deer impact risk", style = "font-size: 20px; font-style: italic; color: #2c3e50;"),
                                    
                                    
                                    p("The maps displayed in this tab will depend on the deer species in the landscape in the previous tab. If the 
                                    deer species list contained any of the small deer species (roe deer, Chinese water deer or Reeve's muntjac), 
                                    you will see a Deer Impact Risk map for the small deer. If the deer species list contained any of the large deer 
                                    species (red deer, sika deer or fallow deer), you will see a Deer Impact Risk map for the large deer. 
                                    If both maps are displayed, you can switch between them by selecting the relevant map using the tick-box menu on 
                                    the top right corner (7).
The Deer Impact Risk score in each map can range from 1 (low Deer Impact Risk) to 5 (high Deer Impact Risk). There is a separate colour scale for the map displaying the Deer Impact Risk posed by small deer and the map displaying the Deer Impact Risk posed by large deer. 
You can download the map(s) as a Shapefile ending in .shp (which can be used in GIS mapping software) or a screenshot as a Portable Network Graphic (.png) image (which can be pasted directly into word processing software or email or be printed out) by clicking the buttons above
the maps (8). The Shapefiles may take a few seconds to save and should appear in your
computer downloads folder in a zipped folder.
If you download the map(s) as a Shapefile, the column “mean_risk” is the impact risk score for each woodland, “wood_ID” is the ID number for each woodland, and “Area_Ha” is the area of each woodland in hectares. 
If there are no woodlands present in your landscape, a message will appear informing you of this. Don't worry, you can still test different woodland planting scenarios in the next tab!
", style = "font-size: 16px;"),
                                    div(
                                      style = "display: flex; justify-content: center; gap: 20px; margin-bottom: 10px;",
                                      
                                      img(src = "current_deer_impact_risk_clicks.png", style = "width: 80%;")
                                    ),
                                    
                                    p("Plant new woodlands", style = "font-size: 20px; font-style: italic; color: #2c3e50;"),
                                    
                                    
                                    p("Now that you have viewed the impact risk to woodlands that are currently present in your landscape, you can try planting new woodlands and see how this may change the impact risk to existing woodlands, and the predicted impact risk to the new woodlands.
Using the tool panel to the left (9), draw new woodlands on your map. You can delete them and start again whenever you need to. Only draw woodlands inside the blue circle, or you will be asked to start your drawing again. When drawing a woodland, ensure the edges do not overlap each other, or the woodland will be invalid, and you will need to start again.
When you click 'Confirm woodland planting' (10) your new woodlands will appear on the second map in the tab. You can return to the first map and redraw your woodlands until you are happy with them.
When you have finished drawing and confirmed the woodland planting, click 'Run deer risk model' (11) and the iDeer tool will begin updating the woodland impact risk map(s) for your landscape. This may take up to 5 minutes.
", style = "font-size: 16px;"),
                                    div(
                                      style = "display: flex; justify-content: center; gap: 20px; margin-bottom: 10px;",
                                      #img(src = "planted_woods_tool_panel_clicks.png", style = "width: 70%;"),
                                      img(src = "new_woods_planted_clicks.png", style = "width: 70%;")
                                      
                                    ),
                                    
                                    p("Predicted impact risk after planting", style = "font-size: 20px; font-style: italic; color: #2c3e50;"),
                                    
                                       
                                    p("The maps in this tab will look similar to your original impact risk maps, but should now also include your newly planted woodlands with their impact risk scores. You can toggle between the updated risk maps using the tick-box menu on the right (12).
The new woodlands have been split into sections with a minimum area of 2.5 hectares. This will allow you to see any differences in impact risk across your new woodlands, especially if they are particularly large.
As before, you can download the maps as a Shapefile ending in .shp (which can be used in GIS mapping software) or a Portable Network Graphic (.png) image by clicking the buttons above the maps (13). The files may take a few seconds to save, and should appear in your computer downloads folder in zip files.
", style = "font-size: 16px;"),
                                    div(
                                      style = "display: flex; justify-content: center; gap: 20px; margin-bottom: 10px;",
                                      
                                      img(src = "updated_impact_risk_maps_clicks.png", style = "width: 45%;")
                                    ),
                                    
                                    column(2)  # Right margin (correctly placed outside central column)
                                    )
                                  )
                                ),  
                         
                         
                         tabPanel("Select your area",
                                  fluidRow(
                                    column(5, 
                                           br(), 
                                           br(), 
                                           leafletOutput("select_map", width = "200%", height = "600px"),
                                           #if you want to restrict options on slider:
                                           #sliderTextInput(    inputId = "myslider",    label = "Choose a value:", 
                                           #choices = c(2,3,5,7,11,13,17,19,23,29,31),    grid = TRUE)
                                           #max = 8 as resolution suffers in leaflet if current.risk raster is displayed any bigger
                                           sliderInput(inputId = "inSlider", label = "Landscape size:", min = 3, max = 8, value = 5)#,
                                           #actionButton("restrict_view", "Confirm location and landscape size")
                                    )
                                  )
                         ),
                         
                         tabPanel("Deer species",
                                  div(style = "margin-top: 20px;",  # Adds vertical spacing at the top
                                      fluidRow(
                                        column(
                                          width = 8,
                                          div(style = "margin-bottom: 20px;", uiOutput("species_list")),
                                          
                                          tags$style(HTML("
          #additional_species {
            font-size: 16px;
          }
        ")),
                                          
                                          div(style = "margin-bottom: 20px;",
                                              p("If you know of additional deer species in your area, please select species below. When you are happy with the species list, click 'Confirm deer species':", 
                                                style = "font-size: 16px;")
                                          ),
                                          
                                          div(style = "margin-bottom: 20px;",
                                              selectInput("additional_species", 
                                                          label = NULL,
                                                          choices = c("Chinese water deer", "Fallow deer", "Reeve's muntjac", "Red deer", "Roe deer", "Sika deer"),
                                                          multiple = TRUE)
                                          ),
                                          
                                          div(style = "margin-bottom: 20px;",
                                              actionButton("confirmed_species", "Confirm deer species",
                                                           style = "font-size: 14px; background-color: #28a745; color: white;")
                                          ),
                                          
                                          div(style = "margin-bottom: 20px;", uiOutput("deer_species_list"))
                                        ),
                                        column(
                                          width = 4,
                                          div(
                                            img(src = "bds_logo.png", width = "100%"),
                                            style = "text-align: right; margin-top: 10px;"
                                          )
                                        )
                                      )
                                  )
                         )
                         ,
                         
                         
                         tabPanel("Current deer impact risk",
                                  fluidRow(
                                    div(
                                      style = "flex: 1;",
                                      downloadButton("download_shp", "Export Map as .shp"),
                                      actionButton("go_screenshot", "Screenshot map", icon = icon("camera")),
                                      br(), br(),
                                      
                                      # Map container
                                      div(
                                        id = "map_capture_area", # Target this for the square screenshot
                                        style = "position: relative; width: 1200px; height: 850px; margin: auto; background: white; padding: 10px; border: 1px solid #eee;",
                                        
                                        leafletOutput("current_risk_map", width = "100%", height = "100%"),
                                        
                                        # Keep your message inside the relative container
                                        uiOutput("risk_map_message"),
                                        
                                        # Large deer arrow - Hidden by default, toggled via Server
                                        shinyjs::hidden(
                                          div(id = "arrow_large_container",
                                              absolutePanel(
                                                right = 10, top = "50%",
                                                width = 120,
                                                draggable = FALSE,
                                                style = "transform: translateY(-50%); background: transparent; border: none; z-index: 1000;",
                                                img(src = "low-high-risk-arrow-large-deer.png", width = "100%")
                                              )
                                          )
                                        ),
                                        
                                        # Small deer arrow - Hidden by default, toggled via Server
                                        shinyjs::hidden(
                                          div(id = "arrow_small_container",
                                              absolutePanel(
                                                right = 10, top = "50%",
                                                width = 120,
                                                draggable = FALSE,
                                                style = "transform: translateY(-50%); background: transparent; border: none; z-index: 1000;",
                                                img(src = "low-high-risk-arrow-small-deer.png", width = "100%")
                                              )
                                          )
                                        )
                                      ),
                                      
                                      br(),
                                      p("To start again from the beginning, press the reset button. This will restart the iDeer tool and erase all your maps!", 
                                        style = "font-size: 14px;"
                                      ),
                                      actionButton("reset_app", "Reset iDeer tool", icon = icon("redo"), class = "btn-danger")
                                    )
                                  )
                         ),
                         
                         tabPanel("Plant new woodlands",
                                  fluidRow(
                                    column(5, 
                                           br(), 
                                           br(), 
                                           editModUI("new_woodland_map", width = "200%", height = "600px")
                                    )
                                  ),
                                  br(),
                                  fluidRow(
                                    div(style = "margin-bottom: 20px;",
                                        actionButton("plant", "Confirm planting",
                                                     style = "font-size: 14px; background-color: #28a745; color: white;")
                                    )
                                  ),
                                  
                                  
                                  
                                  br(),
                                  fluidRow(
                                    column(4,
                                           verbatimTextOutput("plant_area")
                                    )
                                  ),
                                  
                                  br(),
                                  fluidRow(
                                    column(5,
                                           leafletOutput("leaf1", width = "200%", height = "600px")
                                    ),
                                  ),
                                  # br(),
                                  # fluidRow(
                                  #   column(12,
                                  #          DT::dataTableOutput("editable_table")  # Table for editing the sf attributes
                                  #   )
                                  # ),
                                  br(),
                                  fluidRow(
                                    uiOutput("plant2")
                                  ),
                                  
                                  br(),
                                  fluidRow(
                                    #  p("The future risk map will pop up in the Future Risk tab once the model has finished running, this can take up to 2 minutes")
                                    #)
                                    strong("When you are happy with your new woodland planting, click the button 'Run deer impact risk model'.
                                           The new risk map will pop up in the tab 'Predicted impact risk after planting' once the model has finished running. This can take a few minutes.
                                           Thank you for your patience!", style = "font-size: 16px;")
                                  )
                                  
                         ),
                         
                         tabPanel("Predicted impact risk after planting",
                                  fluidRow(
                                    div(
                                      style = "flex: 1;",
                                      downloadButton("download_shp2", "Export Map as .shp"),
                                      actionButton("go_screenshot2", "Screenshot map", icon = icon("camera")),
                                      br(), br(),
                                      uiOutput("loader_container_future"),
                                      
                                      # Map container
                                      div(
                                        id = "map_capture_area_future", 
                                        style = "position: relative; width: 1200px; height: 850px; margin: auto; background: white; padding: 10px; border: 1px solid #eee;",
                                        
                                        leafletOutput("future_risk_map", width = "100%", height = "100%"),
                                        
                                        # FIX 1: Unique ID for the message
                                        uiOutput("future_risk_map_message"), 
                                        
                                        # FIX 2: Unique ID for Large deer arrow
                                        shinyjs::hidden(
                                          div(id = "arrow_large_container_future",
                                              absolutePanel(
                                                right = 10, top = "50%",
                                                width = 120,
                                                draggable = FALSE,
                                                style = "transform: translateY(-50%); background: transparent; border: none; z-index: 1000;",
                                                img(src = "low-high-risk-arrow-large-deer.png", width = "100%")
                                              )
                                          )
                                        ),
                                        
                                        # FIX 3: Unique ID for Small deer arrow
                                        shinyjs::hidden(
                                          div(id = "arrow_small_container_future",
                                              absolutePanel(
                                                right = 10, top = "50%",
                                                width = 120,
                                                draggable = FALSE,
                                                style = "transform: translateY(-50%); background: transparent; border: none; z-index: 1000;",
                                                img(src = "low-high-risk-arrow-small-deer.png", width = "100%")
                                              )
                                          )
                                        )
                                      ),
                                      
                                      br(),
                                      p("To start again from the beginning, press the reset button. This will restart the iDeer tool and erase all your maps!", 
                                        style = "font-size: 14px;"
                                      ),
                                      # Note: Using the same ID 'reset_app' is okay if you want both buttons to trigger the same event, 
                                      # but usually, Shiny prefers unique IDs like 'reset_app_future'.
                                      actionButton("reset_app_future", "Reset iDeer tool", icon = icon("redo"), class = "btn-danger")
                                    )
                                  )
                         ),
                         tabPanel("About iDeer",
                                  fluidRow(
                                    column(12, align = "center",
                                           p("What is Project iDeer?",
                                             style = "font-size: 20px; font-weight: bold; font-style: italic; color: #2c3e50;")
                                    )
                                  ),
                                  p("Project iDeer ran from March 2023 - July 2025 and was funded by the UK Research and Innovation (UKRI) programme 'Future of UK Treescapes'. The project brought together an interdisciplinary team with collective expertise in woodland and deer ecology, conservation conflict, animal behaviour modelling, 
                                    social science methods and web tool development. Solutions-focussed from the start, the project worked with stakeholders
                                    involved in woodland and/or deer management, including farmers, woodland managers, public forestry bodies, and conservation practitioners, 
                                    to ensure that the iDeer tool is fit for purpose.", style = "font-size: 16px;"),
                                  
                                  p("Here is a list of the core team and project partners:", style = "font-size: 16px;"),
                                  
                                  p(HTML('
<ul style="list-style:none; padding-left:0;">
<li style="margin-bottom: 24px;">
    <div style="display:flex; align-items: center;">
      <img src="amy_gresham.png" style="width:80px; height:80px; object-fit:cover; border-radius:100%; margin-right:20px;">
      <div><strong>Dr Amy Gresham</strong><br>Post-Doctoral Researcher, University of Southampton</div>
    </div>
  </li>
  <li style="margin-bottom: 24px;">
    <div style="display:flex; align-items: center;">
      <img src="becks_spake.png" style="width:80px; height:80px; object-fit:cover; border-radius:100%; margin-right:20px;">
      <div><strong>Dr Becks Spake</strong><br>Principal Investigator, Associate Professor, University of Southampton</div>
    </div>
  </li>
  <li style="margin-bottom: 24px;">
    <div style="display: flex; align-items: center;">
      <img src="freya_st_john.png" style="width:80px; height:80px; object-fit:cover; border-radius:50%; margin-right:20px;">
      <div><strong>Dr Freya St John</strong><br>Reader in Conservation Science, Bangor University</div>
    </div>
  </li>
  <li style="margin-bottom: 24px;">
    <div style="display: flex; align-items: center;">
      <img src="graeme_shannon.png" style="width:80px; height:80px; object-fit:cover; border-radius:50%; margin-right:20px;">
      <div><strong>Dr Graeme Shannon</strong><br>Lecturer in Zoology (Behaviour), Bangor University</div>
    </div>
  </li>
  <li style="margin-bottom: 24px;">
    <div style="display: flex; align-items: center;">
      <img src="elena_cini.png" style="width:80px; height:80px; object-fit:cover; border-radius:50%; margin-right:20px;">
      <div><strong>Dr Elena Cini</strong><br>Post-Doctoral Researcher, Bangor University</div>
    </div>
  </li>
  <li style="margin-bottom: 24px;">
    <div style="display: flex; align-items: center;">
      <img src="matt_grainger.png" style="width:80px; height:90px; object-fit:cover; border-radius:50%; margin-right:20px;">
      <div><strong>Dr Matthew Grainger (Collaborator)</strong><br>Researcher, Norwegian Institute for Nature Research</div>
    </div>
  </li>
  <li style="margin-bottom: 24px;">
    <div style="display: flex; align-items: center;">
      <img src="felix_eigenbrod.png" style="width:80px; height:80px; object-fit:cover; border-radius:50%; margin-right:20px;">
      <div><strong>Prof Felix Eigenbrod (Co-Investigator)</strong><br>Professor of Applied Spatial Ecology, University of Southampton</div>
    </div>
  </li>
  <li style="margin-bottom: 24px;">
    <div style="display: flex; align-items: center;">
      <img src="chloe_bellamy.png" style="width:80px; height:80px; object-fit:cover; border-radius:50%; margin-right:20px;">
      <div><strong>Dr Chloe Bellamy (Co-Investigator)</strong><br>Spatial Scientist, Forest Research</div>
    </div>
  </li>
  <li style="margin-bottom: 24px;">
    <div style="display: flex; align-items: center;">
      <img src="matt_guy.png" style="width:80px; height:80px; object-fit:cover; border-radius:50%; margin-right:20px;">
      <div><strong>Dr Matt Guy (Collaborator)</strong><br>Spatial Scientist, Forest Research</div>
    </div>
  </li>
  <li style="margin-bottom: 24px;">
    <div style="display: flex; align-items: center;">
      <img src="andrew_rattey.png" style="width:80px; height:80px; object-fit:cover; border-radius:50%; margin-right:20px;">
      <div><strong>Dr Andrew Rattey (Collaborator)</strong><br>Spatial Scientist, Forest Research</div>
    </div>
  </li>
  <li style="margin-bottom: 24px;">
    <div style="display: flex; align-items: center;">
      <img src="robin_gill.png" style="width:80px; height:80px; object-fit:cover; border-radius:50%; margin-right:20px;">
      <div><strong>Dr Robin Gill (Co-Investigator)</strong><br>Senior Scientist, Vertebrate Ecology, Forest Research</div>
    </div>
  </li>
  <li style="margin-bottom: 24px;">
    <div style="display: flex; align-items: center;">
      <img src="paul_orsi.png" style="width:80px; height:80px; object-fit:cover; border-radius:50%; margin-right:20px;">
      <div><strong>Paul Orsi (Partner)</strong><br>Sylva Foundation</div>
    </div>
  </li>
  <li style="margin-bottom: 24px;">
    <div style="display: flex; align-items: center;">
      <img src="chris_nichols.png" style="width:80px; height:80px; object-fit:cover; border-radius:50%; margin-right:20px;">
      <div><strong>Dr Chris Nichols (Partner)</strong><br>Conservation Evidence Manager, Woodland Trust</div>
    </div>
  </li>
  <li style="margin-bottom: 24px;">
    <div style="display: flex; align-items: center;">
      <img src="lee_oliver.png" style="width:80px; height:80px; object-fit:cover; border-radius:50%; margin-right:20px;">
      <div><strong>Lee Oliver (Partner)</strong><br>Project Manager, Game and Wildlife Conservation Trust</div>
    </div>
  </li>
  <li style="margin-bottom: 24px;">
    <div style="display: flex; align-items: center;">
      <img src="alastair_ward.png" style="width:80px; height:80px; object-fit:cover; border-radius:50%; margin-right:20px;">
      <div><strong>Dr Alastair Ward (Collaborator)</strong><br>Associate Professor of Biodiversity and Ecosystem Management, University of Leeds; Honorary Scientific Advisor, British Deer Society</div>
    </div>
  </li>
</ul>
'), style = "font-size: 16px;"),
                                  
            fluidRow(
            column(12, align = "center",
        p("The expert panel",
          style = "font-size: 20px; font-weight: bold; font-style: italic; color: #2c3e50;")
            )
          ),
                                  
            p("
            We are so grateful to all the deer management practitioners and researchers on the panel
              who gave their time to help us achieve our goals with the iDeer tool:
              David Hooton, Alastair Boston, Jamie Cordery, David Jam, Arman Siddiqui, Andy Page and Maarten Ledeboer (Forestry Commission), 
              Martin Edwards (British Association for Shooting and Conservation), Nick Reed-Beale (Woodland Trust), 
              Adrian Jowitt (Natural England), Nick Jackson (Bowland Deer Management Group & Lune Valley Deer Management Group), 
              Charles Smith Jones (British Deer Society), Dr Chris Hirst (Forest Research), 
              Dr Owain Barton (University of Alberta), Dr Eilidh Smith (Durham University), Tom Logan (University of Leeds), Dr Ewan McHenry (Woodland Trust), 
              Dr Jochen Langbein (Langbein Wildlife Ecological Consultants) and Professor Rory Putman (University of Glasgow).", style = "font-size: 16px;"),
            
        div(
          style = "text-align: center; margin-top: 10px;",
          img(src = "expert_workshop.png", width = "50%"),
          p("Image: Photograph from the expert workshop in the early stages of Project iDeer", style = "margin-top: 8px; font-style: italic;")
        )
        
                                         
                         )
             )
      )
    )
  )
)

# Run the application #-----------------------------------------------------------
shinyApp(ui = ui, server = server)
