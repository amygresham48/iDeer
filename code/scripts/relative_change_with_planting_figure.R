library(sf)
library(dplyr)
library(ggplot2)

#Import example sf objects from paper

large_0 <- st_read("C:/Users/ik929086/Desktop/deer_predicted_impact_risk_maps_shp (1)/large_risk.shp")
large_0 <- large_0 %>% rename(mean_risk_0 = MEAN)
large_1 <- st_read("C:/Users/ik929086/Desktop/predicted_deer_impact_risk_after_planting_shapefiles (2)/predicted_large_deer_impact_risk_shp.shp")
large_1 <- large_1 %>% rename(mean_risk_1 = mn_l___)
large_2 <- st_read("C:/Users/ik929086/Desktop/predicted_deer_impact_risk_after_planting_shapefiles (3)/predicted_large_deer_impact_risk_shp.shp")
large_2 <- large_2 %>% rename(mean_risk_2 = mn_l___)

  
small_0 <- st_read("C:/Users/ik929086/Desktop/deer_predicted_impact_risk_maps_shp (1)/small_risk.shp")
small_0 <- small_0 %>% rename(mean_risk_0 = MEAN)
small_1 <- st_read("C:/Users/ik929086/Desktop/predicted_deer_impact_risk_after_planting_shapefiles (2)/predicted_small_deer_impact_risk_shp.shp")
small_1 <- small_1 %>% rename(mean_risk_1 = mn_s___)
small_2 <- st_read("C:/Users/ik929086/Desktop/predicted_deer_impact_risk_after_planting_shapefiles (3)/predicted_small_deer_impact_risk_shp.shp")
small_2 <- small_2 %>% rename(mean_risk_2 = mn_s___)

#Make new sf objects with column "risk_diff" which is the difference between xxx_0$mean_risk & xxx_1$mean_risk and xxx_0$mean_risk & xxx_2$mean_risk

matched_large_1 <- st_join(large_0, large_1, join = st_equals, suffix = c("_0", "_1"))
matched_large_1 <- matched_large_1 %>% mutate(patch_id = 1:nrow(.))

matched_large_2 <- st_join(large_0, large_2, join = st_equals, suffix = c("_0", "_2"))
matched_large_2 <- matched_large_2 %>% mutate(patch_id = 1:nrow(.))

matched_small_1 <- st_join(small_0, small_1, join = st_equals, suffix = c("_0", "_1"))
matched_small_1 <- matched_small_1 %>% mutate(patch_id = 1:nrow(.))

matched_small_2 <- st_join(small_0, small_2, join = st_equals, suffix = c("_0", "_2"))
matched_small_2 <- matched_small_2 %>% mutate(patch_id = 1:nrow(.))


# Difference between large_0 and large_1
matched_large_1$risk_diff <- matched_large_1$mean_risk_0 - matched_large_1$mean_risk_1

# Difference between large_0 and large_2
matched_large_2$risk_diff <- matched_large_2$mean_risk_0 - matched_large_2$mean_risk_2

#Difference between small_0 and small_1
matched_small_1$risk_diff <- matched_small_1$mean_risk_0 - matched_small_1$mean_risk_1

# Difference between small_0 and small_2
matched_small_2$risk_diff <- matched_small_2$mean_risk_0 - matched_small_2$mean_risk_2

#Plot the differences using the scale colour gradient 2 (red to blue) where red = increase and blue = decrease
ggplot(matched_large_1) +
  geom_sf(aes(fill = risk_diff)) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0,
                       name = "Risk Difference") +
  theme_minimal() +
  ggtitle("Large Deer Impact Risk Difference (0 vs 1)")

ggplot(matched_large_2) +
  geom_sf(aes(fill = risk_diff)) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0,
                       name = "Risk Difference") +
  theme_minimal() +
  ggtitle("Large Deer Impact Risk Difference (0 vs 2)")

ggplot(matched_small_1) +
  geom_sf(aes(fill = risk_diff)) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0,
                       name = "Risk Difference") +
  theme_minimal() +
  ggtitle("Small Deer Impact Risk Difference (0 vs 1)")

ggplot(matched_small_2) +
  geom_sf(aes(fill = risk_diff)) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0,
                       name = "Risk Difference") +
  theme_minimal() +
  ggtitle("Small Deer Impact Risk Difference (0 vs 2)")
