library(sf)
library(dplyr)
library(ggplot2)

#Import example sf objects from paper

large_0 <- st_read("C:/Users/ik929086/Desktop/deer_predicted_impact_risk_maps_shp/large_risk.shp")
large_0 <- large_0 %>% rename(mean_risk = MEAN,
                              Shape_Area = Wd_Ar_2)
large_1 <- st_read("C:/Users/ik929086/Desktop/predicted_deer_impact_risk_after_planting_shapefiles/predicted_large_deer_impact_risk_shp.shp")
large_1 <- large_1 %>% rename(mean_risk = mn_l___,
                              Shape_Area = Shap_Ar) %>%
  st_cast("POLYGON")
large_2 <- st_read("C:/Users/ik929086/Desktop/predicted_deer_impact_risk_after_planting_shapefiles_2/predicted_large_deer_impact_risk_shp.shp")
large_2 <- large_2 %>% rename(mean_risk = mn_l___,
                              Shape_Area = Shap_Ar) %>%
  st_cast("POLYGON")

  
small_0 <- st_read("C:/Users/ik929086/Desktop/deer_predicted_impact_risk_maps_shp/small_risk.shp")
small_0 <- small_0 %>% rename(mean_risk = MEAN,
                              Shape_Area = Wd_Ar_2)
small_1 <- st_read("C:/Users/ik929086/Desktop/predicted_deer_impact_risk_after_planting_shapefiles/predicted_small_deer_impact_risk_shp.shp")
small_1 <- small_1 %>% rename(mean_risk = mn_s___,
                              Shape_Area = Shap_Ar) %>%
  st_cast("POLYGON")
small_2 <- st_read("C:/Users/ik929086/Desktop/predicted_deer_impact_risk_after_planting_shapefiles_2/predicted_small_deer_impact_risk_shp.shp")
small_2 <- small_2 %>% rename(mean_risk = mn_s___,
                              Shape_Area = Shap_Ar) %>%
  st_cast("POLYGON")

#Add scenario ID cols
large_0 <- large_0 %>% mutate(scenario_ID = 0)
large_1 <- large_1 %>% mutate(scenario_ID = 1)
large_2 <- large_2 %>% mutate(scenario_ID = 2)

small_0 <- small_0 %>% mutate(scenario_ID = 0)
small_1 <- small_1 %>% mutate(scenario_ID = 1)
small_2 <- small_2 %>% mutate(scenario_ID = 2)

# Now bind the two datasets together
merged_large <- bind_rows(large_0, large_1, large_2)
merged_small <- bind_rows(small_0, small_1, small_2)

#Make columns risk_diff_1 and risk_diff_2 where value = xxxx_0 - xxxx_1 and xxxx_0 - xxxx_2

# Extract scenario-specific data
large_0_vals <- merged_large %>%
  filter(scenario_ID == 0) %>%
  select(geometry, mean_risk_0 = mean_risk)

large_1_vals <- merged_large %>%
  filter(scenario_ID == 1) %>%
  select(geometry, mean_risk_1 = mean_risk)

large_2_vals <- merged_large %>%
  filter(scenario_ID == 2) %>%
  select(geometry, mean_risk_2 = mean_risk)

# Join scenario 0 to 1 and 2 by geometry (exact match)
large_diffs <- large_0_vals %>%
  st_join(large_1_vals, join = st_equals) %>%
  st_join(large_2_vals, join = st_equals) %>%
  mutate(
    risk_diff_1 = mean_risk_0 - mean_risk_1,
    risk_diff_2 = mean_risk_0 - mean_risk_2
  )

#Plot the differences using the scale colour gradient 2 (red to blue) where red = increase and blue = decrease
ggplot() +
  # Base layer: large_1 polygons in grey outlines (no fill)
  geom_sf(data = large_1, fill = "darkgray", color = "black", size = 0.3) +
  # Top layer: large_diffs polygons filled by risk_diff_1
  geom_sf(data = large_diffs, aes(fill = risk_diff_1)) +
  # Colour scale for risk differences
  scale_fill_gradient2(
    low = "blue",
    mid = "white",
    high = "red",
    midpoint = 0,
    limits = c(-1, 2),
    name = "Risk Difference"
  ) +
  # Theme and title
  theme_minimal() +
  ggtitle("Large Deer Impact Risk Difference (0 vs 1)")

#Plot the differences using the scale colour gradient 2 (red to blue) where red = increase and blue = decrease
ggplot() +
  # Base layer: large_1 polygons in grey outlines (no fill)
  geom_sf(data = large_2, fill = "darkgray", color = "black", size = 0.3) +
  # Top layer: large_diffs polygons filled by risk_diff_1
  geom_sf(data = large_diffs, aes(fill = risk_diff_2)) +
  # Colour scale for risk differences
  scale_fill_gradient2(
    low = "blue",
    mid = "white",
    high = "red",
    midpoint = 0,
    limits = c(-1, 2),
    name = "Risk Difference"
  ) +
  # Theme and title
  theme_minimal() +
  ggtitle("Large Deer Impact Risk Difference (0 vs 2)")


# #save these sf objects to folder on desktop "C:\Users\ik929086\Desktop\risk_diff_plots"
# 
# # Save matched_large_1
# st_write(matched_large_1, "C:/Users/ik929086/Desktop/risk_diff_plots/matched_large_1.shp", delete_layer = TRUE)
# 
# # Save matched_large_2
# st_write(matched_large_2, "C:/Users/ik929086/Desktop/risk_diff_plots/matched_large_2.shp", delete_layer = TRUE)
# 
# # Save matched_small_1
# st_write(matched_small_1, "C:/Users/ik929086/Desktop/risk_diff_plots/matched_small_1.shp", delete_layer = TRUE)
# 
# # Save matched_small_2
# st_write(matched_small_2, "C:/Users/ik929086/Desktop/risk_diff_plots/matched_small_2.shp", delete_layer = TRUE)
