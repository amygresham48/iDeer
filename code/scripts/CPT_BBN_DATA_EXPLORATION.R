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

jpeg(here("output/figures/BBN_CPT_plot.jpeg"), width = 4500, height = 3500, units = "px", res = 500)
ggplot(CPT, aes(x = NPI, y = Damage_Index, col = NLI, group = NLI)) +  
  geom_line() +
  geom_point() + 
  facet_grid(rows = vars(Thermoreg_Index), cols = vars(Disturbance_Index)) +  
  theme_bw() +  
  scale_color_viridis_c(breaks = c(1, 2, 3)) + 
  labs(x = "Patch quality", col = "Landscape quality") +
  scale_y_continuous(sec.axis = sec_axis(~ . , name = "Thermoreg_Index", breaks = NULL, labels = NULL)) +
  scale_x_continuous(sec.axis = sec_axis(~ . , name = "Disturbance_Index", breaks = NULL, labels = NULL), 
                     lim = c(1, 3), 
                     breaks = seq(1, 3, 1))
dev.off()


#Disturbance CPT ####

disturb <- read.csv(here("data/BBN-cpts/CPT_Disturbance_Index.csv"))

#Recode to numbers
recode_mapping <- c("LOW" = 1, "MED" = 2, "HIGH" = 3)
# Recode the first four columns
disturb <- disturb %>%
  mutate(across(1:4, ~ recode(., !!!recode_mapping)))

ggplot(disturb, aes(x = Urban_proximity, y = Disturbance_Index, col = Connectivity, group = Connectivity)) +  # Set aesthetics
  geom_line() +
  geom_point() + 
  facet_grid(rows=vars(Connectivity), cols=vars(Road_density),labeller = label_both)+
  theme_bw() -> urbplot#+  Use a clean theme
  #scale_color_viridis_c() + # Use viridis color scale
  # Create a simple secondary axis for the facets (use the appropriate scale_x function)
 # scale_x_continuous(sec.axis = sec_axis(~ . , name = "Road_density", breaks = NULL, labels = NULL))

  ggplot(disturb, aes(x = Road_density, y = Disturbance_Index, col = Connectivity, group = Connectivity)) +  # Set aesthetics
    geom_line() +
    geom_point() + 
    facet_grid(rows=vars(Connectivity), cols=vars(Urban_proximity),labeller = label_both)+
    theme_bw() -> rdplot #+  # Use a clean theme
    # Create a simple secondary axis for the facets (use the appropriate scale_x function)
    #scale_x_continuous(sec.axis = sec_axis(~ . , name = "Road_density", breaks = NULL, labels = NULL))

  ggplot(disturb, aes(x = Connectivity, y = Disturbance_Index, col = Connectivity, group = 1)) +  # Set aesthetics
    geom_line() +
    geom_point() + 
    facet_grid(rows=vars(Road_density), cols=vars(Urban_proximity),labeller = label_both)+
    theme_bw() -> conplot #+  # Use a clean theme
  # Create a simple secondary axis for the facets (use the appropriate scale_x function)
  #scale_x_continuous(sec.axis = sec_axis(~ . , name = "Road_density", breaks = NULL, labels = NULL))
  
  
  
gridExtra::grid.arrange(urbplot, rdplot, conplot)    

#Try correcting the odd relationships, plot again:
#USING THIS LOGIC IN BBN AS OF 04/07/2024

#Disturbance CPT ####

disturb_v2 <- read.csv(here("data/BBN-cpts/CPT_Disturbance_Index_refined.csv"))

#Recode to numbers
recode_mapping <- c("LOW" = 1, "MED" = 2, "HIGH" = 3)
# Recode the first four columns
disturb_v2 <- disturb_v2 %>%
  mutate(across(1:4, ~ recode(., !!!recode_mapping)))

ggplot(disturb_v2, aes(x = Urban_proximity, y = Disturbance_Index, col = Connectivity, group = Connectivity)) +  # Set aesthetics
  geom_line() +
  geom_point() + 
  facet_grid(rows=vars(Connectivity), cols=vars(Road_density),labeller = label_both)+
  theme_bw() -> urbplot#+  Use a clean theme
#scale_color_viridis_c() + # Use viridis color scale
# Create a simple secondary axis for the facets (use the appropriate scale_x function)
# scale_x_continuous(sec.axis = sec_axis(~ . , name = "Road_density", breaks = NULL, labels = NULL))

ggplot(disturb_v2, aes(x = Road_density, y = Disturbance_Index, col = Connectivity, group = Connectivity)) +  # Set aesthetics
  geom_line() +
  geom_point() + 
  facet_grid(rows=vars(Connectivity), cols=vars(Urban_proximity),labeller = label_both)+
  theme_bw() -> rdplot #+  # Use a clean theme
# Create a simple secondary axis for the facets (use the appropriate scale_x function)
#scale_x_continuous(sec.axis = sec_axis(~ . , name = "Road_density", breaks = NULL, labels = NULL))

ggplot(disturb_v2, aes(x = Connectivity, y = Disturbance_Index, col = Connectivity, group = 1)) +  # Set aesthetics
  geom_line() +
  geom_point() + 
  facet_grid(rows=vars(Road_density), cols=vars(Urban_proximity),labeller = label_both)+
  theme_bw() -> conplot #+  # Use a clean theme
# Create a simple secondary axis for the facets (use the appropriate scale_x function)
#scale_x_continuous(sec.axis = sec_axis(~ . , name = "Road_density", breaks = NULL, labels = NULL))

gridExtra::grid.arrange(urbplot, rdplot, conplot)    

###########################

#Nutritional landscape index ####
      
nli <- read.csv(here("data/BBN-cpts/CPT_NLI.csv"))
#Recode to numbers
recode_mapping <- c("LOW" = 1, "MED" = 2, "HIGH" = 3)
# Recode the three column
nli <- nli %>%
  mutate(across(1:3, ~ recode(., !!!recode_mapping)))

ggplot(nli, aes(x = linear_density, y = NLI)) +  # Set aesthetics
  geom_line() +
  geom_point() + 
  facet_wrap(.~nutritional_landscape_composition) +  # Set facets
  theme_bw() +  # Use a clean theme
  scale_color_viridis_c() + # Use viridis color scale
  # Create a simple secondary axis for the facets (use the appropriate scale_x function)
  scale_x_continuous(sec.axis = sec_axis(~ . , name = "nutritional_landscape_composition", breaks = NULL, labels = NULL))

ggplot(nli, aes(x = nutritional_landscape_composition, y = NLI)) +  # Set aesthetics
  geom_line() +
  geom_point() + 
  facet_wrap(.~linear_density) +  # Set facets
  theme_bw() +  # Use a clean theme
  scale_color_viridis_c() + # Use viridis color scale
  # Create a simple secondary axis for the facets (use the appropriate scale_x function)
  scale_x_continuous(sec.axis = sec_axis(~ . , name = "linear feature density", breaks = NULL, labels = NULL))

