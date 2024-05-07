#Load packages

library(sf)
library(dplyr)
library(ggplot2)
library(here)
library(progress)

here()

#CPT data exploration ####

#Import dataset

CPT <- read.csv(here("data/BBN-cpts/CPT_BBN_deer_impact_tinkering.csv"))

#Recode to numbers
recode_mapping <- c("LOW" = 1, "MED" = 2, "HIGH" = 3)
# Recode the first four columns
CPT <- CPT %>%
  mutate(across(1:4, ~ recode(., !!!recode_mapping)))

#recode impact column

recode_mapping <- c("LOW" = 1, "LOW-MED" = 2, "MED" =3,"MED-HIGH" = 4,"HIGH"=5)
# Recode the first four columns
CPT <- CPT %>%
  mutate(across(5, ~ recode(., !!!recode_mapping)))

CPT <- CPT%>%rename(NPI = Nutritional_Pixel_Index,
                    NLI = Nutritional_Landscape_Index,
                    Damage_Index = Deer_damage_Index)

ggplot(CPT, aes(x = NPI, y = Damage_Index, col = NLI, group = NLI)) +  # Set aesthetics
  geom_line() +
  geom_point() + 
  facet_grid(rows = vars(Thermoreg_Index), cols = vars(Disturbance_Index)) +  # Set facets
  theme_bw() +  # Use a clean theme
  scale_color_viridis_c() + # Use viridis color scale
  labs(x = "Patch quality", col = "Landscape quality")+
  # Create a simple secondary axis for the facets (use the appropriate scale_x function)
  scale_y_continuous(sec.axis = sec_axis(~ . , name = "Thermoreg_Index", breaks = NULL, labels = NULL)) +
  scale_x_continuous(sec.axis = sec_axis(~ . , name = "Disturbance_Index", breaks = NULL, labels = NULL))



#Have a look at disturbance CPT ####

disturb <- read.csv(here("data/BBN-cpts/CPT_Disturbance_Index.csv"))

#Recode to numbers
recode_mapping <- c("LOW" = 1, "MED" = 2, "HIGH" = 3)
# Recode the first four columns
disturb <- disturb %>%
  mutate(across(1:4, ~ recode(., !!!recode_mapping)))


ggplot(disturb, aes(x = Urban_proximity, y = Disturbance_Index, col = Connectivity, group = Connectivity)) +  # Set aesthetics
  geom_line() +
  geom_point() + 
  facet_wrap(.~Road_density) +  # Set facets
  theme_bw() +  # Use a clean theme
  scale_color_viridis_c() + # Use viridis color scale
  # Create a simple secondary axis for the facets (use the appropriate scale_x function)
  scale_x_continuous(sec.axis = sec_axis(~ . , name = "Road_density", breaks = NULL, labels = NULL))
