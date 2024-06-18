#Plot the 4 incides as spatial layers to be included alongside the leaflet map ####

# Load the data
df_categorical <- readRDS(file = file.path(here("data/derived-data/Tool-extract-function-test/predicted_data_categorical_pixel_level_5km.rds")))

# Make sf object
df_sf <- st_as_sf(df_categorical, coords = c("x", "y"), crs = st_crs(bng))
df_sf <- st_transform(df_sf, crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

# Define the value mapping for nearest_road
damage_values <- c("LOW" = 1, "MED" = 2, "HIGH"=3)
df_sf$value <- damage_values[df_sf$INDEX]

# Set up the colors
val <- 1:3
pal <- c("lightblue", "dodgerblue", "royalblue4")

#Convert to raster
r <- df_sf %>% dplyr::select(geometry, value) %>% stars::st_rasterize()

# Convert the raster to a data frame for ggplot
r_df <- as.data.frame(r, xy = TRUE)

# Create a factor for the value with all levels
r_df$value <- factor(r_df$value, levels = val, labels = names(INDEX_VALUES))

# Add missing levels to the data
missing_levels <- data.frame(x = NA, y = NA, value = factor(val, labels = names(INDEX_VALUES)))
r_df <- rbind(r_df, missing_levels)

# Plot raster using the color palette with custom labels
ggplot(r_df) +
  geom_raster(aes(x = x, y = y, fill = value)) +
  scale_fill_manual(values = pal, 
                    breaks = names(INDEX_VALUES),
                    labels = names(INDEX_VALUES),
                    na.value = "transparent") +
  theme_minimal() +
  labs(title = "Predicted INDEX Raster",
       fill = "INDEX VALUE") +
  guides(fill = guide_legend(override.aes = list(alpha = 1))) # Ensure all levels are shown in the legend