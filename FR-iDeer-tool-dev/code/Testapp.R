
library(sf)
# library(sp)

### Commented out libraries that aren't strictly required for skeleton app to run.

library(raster)
# library(rgdal)
# library(tools)
library(shiny)
# library(plyr)
library(dplyr)
# library(shinyjs)
library(RColorBrewer)
library(leaflet)
# library(lobstr)
library(shinydashboard)
library(htmltools)
# library(fasterize)

library(DT)
# library(classInt)
# library(shinyBS)
# library(foreign)

library(shinyWidgets)

### Libraries required for polygon drawing functionality ----

library(leafpm)
library(mapview)
library(mapedit)
# library(leaflet.extras)

# library(shinyjs)

# library(shinybusy)
# library(shinyscreenshot)
# library(shinyhelper)

# library(ggplot2)
# library(readr)

# library(openxlsx)

# library(stars)
# library(terra)

# library(shinybusy)
# library(shinyscreenshot)
# library(shinycssloaders)

### England country boundary

longlat <- c(-1.14, 53)


ui <- dashboardPage(
  
  skin = "blue",
  
  dashboardHeader(disable=TRUE),
  dashboardSidebar(disable = TRUE),
  
  dashboardBody(
    
    column(12,
           
           column(2,
                  
                  wellPanel(
                    
                    fluidRow(
                      
                      selectInput(
                        
                        "tab_selection", "select tab",
                        
                        choices = c("Background information" = 1,
                                    "Select area" = 2,
                                    "Baseline conditions" = 3,
                                    "New planting" = 4,
                                    "Model results" = 5)
                        
                      )
                      
                    )
                    
                    
                  )
                  
                  
                  ),
           
           column(1),
           
           column(9,
                  
                  tabsetPanel(id = "master", selected = 1,
                              
                              tabPanel("Background information",
                  
                                       value = 1,
                                       
                                       fluidRow(
                                         
                                         column(5,
                                       
                                       p("Welcome to the IDeer app"))),
                                       
                                       fluidRow(
                                         
                                         p("Insert text here")
                                         
                                       )),

                  
                  tabPanel("Select area",
                                       
                                       value = 2,
                                       
                                       fluidRow(
                                         
                                         column(5,
                                                
                                                leafletOutput("map1",width = "100%", height = "50vh")  
                                                
                                                ))
                              
                              
                  ),
                  
                  tabPanel("Baseline conditions",
                           
                           value = 3,
                           
                           fluidRow(
                             
                             column(5,
                                    
                                    editModUI("map2", width = "100%", height = "60vh") 
                                    
                             )),
                           
                           br(),
                           
                           fluidRow(
                             
                             actionButton("plant", "Confirm planting")

                           ),
                           
                           br(),
                           
                           fluidRow(
                             
                             column(4,
                             
                             verbatimTextOutput("plant_area"))
                           )
      
                  ),
                  
                  tabPanel("Simulate woodland planting",
                           
                           value = 4,
                           
                           fluidRow(
                             
                             column(5,
                                    
                                    leafletOutput("map3",width = "100%", height = "50vh")  
                                    
                             ))
                           
                           
                  ),
                  
                  tabPanel("Model results",
                           
                           value = 5,
                           
                           fluidRow(
                             
                             column(5,
                                    
                                    leafletOutput("map4",width = "100%", height = "50vh")  
                                    
                             ))

           )
           
    )
    
           )

  )
  
  )
)
  
  server <- function(input, output, session) {
    
    # observe_helpers(withMathJax = TRUE)
    
    output$map1 <- renderLeaflet({
      
      leaflet() %>% 
        
        addProviderTiles(providers$CartoDB.Positron, group="positron") %>%
        
        setView(lng = longlat[1], 
                lat = longlat[2],
                zoom = 7)
      
      
    })
    
    observe({
      
      if (is.null(siter$dat)){
        
        return()
      }
      
      sitex <- siter$dat[nrow(siter$dat),] %>% st_transform("+init=epsg:4326")
      
      ### Capturing the long/lat coordinates of the latest twinflower site subset.
      
      sitexdf <- st_coordinates(sitex) %>% 
        as.data.frame() %>% 
        mutate(tempId = seq.int(nrow(.)))

      leafletProxy("map1") %>% 
        addTiles() %>% 
        clearMarkers() %>% 
        addCircleMarkers(data = sitexdf, lng = sitexdf$X, lat = sitexdf$Y, radius = 1)
      
      
    })

    
    ### Tabpanel behaviour ----
    
    observe({
      
      tabval <- input$tab_selection

      ### if no region selected or user present on starting page - basically after you click the "return" button
      
      ### Hide all other panels if use has selected a new region id.
      
      if (tabval == 1){
        
        ### If user hasn't selected a county yet....
        
        ### Hide all tabs except the baseline regional results.
        
        hideTab(inputId = "master", target = "2")
        hideTab(inputId = "master", target = "3")
        hideTab(inputId = "master", target = "4")
        hideTab(inputId = "master", target = "5")
        
        showTab(inputId = "master", target = "1")
        
        
      } else if (tabval == 2) {
        
        hideTab(inputId = "master", target = "1")
        hideTab(inputId = "master", target = "3")
        hideTab(inputId = "master", target = "4")
        hideTab(inputId = "master", target = "5")
        
        showTab(inputId = "master", target = "2")
        
      } else if (tabval == 3) {
        
        hideTab(inputId = "master", target = "1")
        hideTab(inputId = "master", target = "2")
        hideTab(inputId = "master", target = "4")
        hideTab(inputId = "master", target = "5")
        
        showTab(inputId = "master", target = "3")

      } else if (tabval == 3) {
        
        hideTab(inputId = "master", target = "1")
        hideTab(inputId = "master", target = "2")
        hideTab(inputId = "master", target = "3")
        hideTab(inputId = "master", target = "5")
        
        showTab(inputId = "master", target = "4")
 
      } else {
        
        hideTab(inputId = "master", target = "1")
        hideTab(inputId = "master", target = "2")
        hideTab(inputId = "master", target = "3")
        hideTab(inputId = "master", target = "4")
        
        showTab(inputId = "master", target = "5")
 
      }
      
    })

    
    #### REACTIVE ELEMENT - SITE SELECTION ----
    
    siter <- reactiveValues(dat = NULL)
    
    ### REACTIVE ELEMENT - SITE BUFFER (3KM) ----
    
    sitebuf <- reactiveValues(dat = NULL)
    
    #### * User click on first map ----
    
    observe({
      
      click<-input$map1_click
      
      if(is.null(click))
        return()
      
      long <- as.numeric(click$lng)
      lat <- as.numeric(click$lat)
      
      cordtab <- data.frame(X = long,  Y = lat)
      
      ### Update reactive element to lift user coordinates - convert to point format
      ### Need to first convert to point using 4326 projection (lat/long) then convert to X/Y using st_transform
      
      sitep <- st_as_sf(cordtab, coords = c("X", "Y"), crs = 4326, agr = "constant") %>% st_transform("+init=epsg:27700") %>% 
        mutate(id = 1)
      
      siter$dat <- sitep
      
      ### Create site 3km buffer and update reactive element ----
      
      sitebuf$dat <- st_buffer(sitep, 3000)

    })
    
    #### REACTIVE ELEMENT - EDITMOD FEATURE ----
    
    map1r <- reactiveValues(dat = NULL)
    
    observe({
      
      if (is.null(siter$dat)){

        return()

      }
      
      print("Proceeding with mapping")
      
      mp1 <- mapview(siter$dat)+sitebuf$dat
      
      ### Call editMod module to allow user to draw polygons for their woodland site
      
      map1r$dat <- callModule(editMod, "map2", mp1@map)
      
 
    })

    ### REACTIVE ELEMENT - WOODLAND POLYGON ----
    
    woodpoly <- reactiveValues(dat = NULL)
    
    #### OBSERVE - HABITAT DATA ASSOCIATED WITH PROPOSED PLANTING ----
    
    observeEvent(input$plant, {
      
      if (is.null(map1r$dat)){
        
        return()
      }
      
      req(map1r$dat()$finished)

      ### be careful here to ensure that only latest drawn polygon is actually used for calculations.
      
      woodpoly$dat <- map1r$dat()$finished %>% mutate(id = seq.int(nrow(.))) %>% 
        st_transform("+init=epsg:27700") %>% mutate(areaha = as.numeric(st_area(.))/10000)
      
      print(woodpoly$dat)
 
     
  })
    
    output$plant_area <- renderText({
      
      paste0("New woodland planting area: ", round(woodpoly$dat$areaha, 3))
      
    })
    
  }
    
  
  # Run the application 
  shinyApp(ui = ui, server = server)
