#Adding the predicted damage data to an interactive satellite image ####
bng <- 27700
library(leaflet)
library(ggplot2)
library(raster)
library(sf)
library(stars)
library(here)

# Load the data
df_categorical <- readRDS(file = file.path(here("predicted_data_categorical_pixel_level_5km.rds")))

# Make sf object
df_sf <- st_as_sf(df_categorical, coords = c("x", "y"), crs = st_crs(bng))
df_sf <- st_transform(df_sf, crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

# Define the value mapping for pred_damage
damage_values <- c("LOW" = 1, "LOW-MED" = 2, "MED" = 3, "MED-HIGH" = 4, "HIGH" = 5)
df_sf$value <- damage_values[df_sf$pred_damage]

# Set up the colors
val <- 1:5
pal <- c("yellow", "#FED976", "#FD8D3C", "#FC4E2A", "#E31A1C")

#Convert to raster
r <- df_sf %>% dplyr::select(geometry, value) %>% stars::st_rasterize()

# Convert the raster to a data frame for ggplot
r_df <- as.data.frame(r, xy = TRUE)

# Create a factor for the value with all levels
r_df$value <- factor(r_df$value, levels = val, labels = names(damage_values))

# Add missing levels to the data
missing_levels <- data.frame(x = NA, y = NA, value = factor(val, labels = names(damage_values)))
r_df <- rbind(r_df, missing_levels)

# Plot raster using the color palette with custom labels
ggplot(r_df) +
  geom_raster(aes(x = x, y = y, fill = value)) +
  scale_fill_manual(values = pal, 
                    breaks = names(damage_values),
                    labels = names(damage_values),
                    na.value = "transparent") +
  theme_minimal() +
  labs(title = "Predicted Damage Raster",
       fill = "Damage Level") +
  guides(fill = guide_legend(override.aes = list(alpha = 1))) # Ensure all levels are shown in the legend

# Define the color palette function for leaflet
color_pal <- colorNumeric(palette = pal, domain = val, na.color = "transparent")

# Convert the stars object to a RasterLayer
raster_layer <- as(r, "Raster")

#Round all values to an integer
rounded_raster <- round(raster_layer)
rounded_raster <- raster::as.factor(rounded_raster)

#Project for leaflet map
rounded_raster <- projectRasterForLeaflet(rounded_raster, method="ngb")

# Create a color palette for the raster
color_fact <- colorFactor(
  palette = c("yellow", "#FED976", "#FD8D3C", "#FC4E2A", "#E31A1C"),
  domain = damage_values,
  na.color = "transparent"  # Set the color for NA values to transparent
  
)

# Create a reversed mapping for labels
damage_labels <- names(damage_values)
names(damage_labels) <- damage_values

# Create the Leaflet map
leaflet() %>%
  addProviderTiles(providers$Esri.WorldImagery) %>%
  addRasterImage(rounded_raster, colors = color_fact, opacity = 0.9, project = FALSE) %>%
  addLegend(
    pal = color_fact,
    values = damage_values,  # Use the correct domain of values
    title = "Deer impact risk",
    labFormat = labelFormat(
      transform = function(x) {
        # Use the label mapping to get the labels
        damage_labels[as.character(x)]
      }
    )
  )