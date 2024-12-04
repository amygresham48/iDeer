#Following semi-structured interviews with our expert panel
#Adjust CPTs for small-bodied herding species vs large bodied herding species
#Small-bodied-solitary = muntjac, roe, CWD
#Large-bodied-herding = red, fallow, sika
#2 models in total
#Get rid of disturbance - not important for predicting probability of damage

#Load packages

library(sf)
library(dplyr)
library(ggplot2)
library(here)
library(progress)

here()

#1. Small bodied deer -----------####
#Greater need to thermoregulate - more sensitive to climatic extremes
#Concentrate selectors - woodland forage is of relatively greater importance
#Well-connected woodland/hedgerows positively influence rate of movement across landscapes
#Highly territorial, will defend territories within woodlands.
#heft to woodland patches
#Hedgerows important source of shelter/food/connectivity corridors.

#Read in CPTs

sd_damage <- read.csv("C:/Users/ik929086/Documents/iDeer/data/BBN-cpts/post-interviews/small-deer/CPT_damage_risk_small_deer_final_with_numbers.csv")

# #Recode to numbers
# recode_mapping <- c("LOW" = 1, "MED" = 2, "HIGH" = 3)
# # Recode the first four columns
# CPT_sd <- sd_damage %>%
#   mutate(across(1:3, ~ recode(., !!!recode_mapping)))
# 
# #recode impact column
# recode_mapping <- c("LOW" = 1, "LOW-MED" = 2, "MED" =3,"MED-HIGH" = 4,"HIGH"=5)
# # Recode the first four columns
# CPT_sd <- CPT_sd %>%
#   mutate(across(4, ~ recode(., !!!recode_mapping)))
# unique(CPT_sd$damage_risk)

sd_damage <- sd_damage %>%
  mutate(
    forage_pressure_index = factor(forage_pressure_index, levels = c("LOW", "MED", "HIGH")),
    thermoreg_index = factor(thermoreg_index, levels = c("LOW", "MED", "HIGH")),
    connect_index = factor(connect_index, levels = c("LOW", "MED", "HIGH")),
    damage_risk = factor(damage_risk, levels = c("LOW","LOW-MED","MED","MED-HIGH","HIGH"))
  )


ggplot(sd_damage, aes(x = damage_risk, y = probability, group = forage_pressure_index, color = forage_pressure_index)) +
  geom_point(size = 3) +
  geom_line() +  # Add lines to connect the points
  facet_grid(connect_index ~ thermoreg_index,
             labeller = labeller(connect_index = label_both, thermoreg_index = label_both)) +
  labs(x = "Deer Damage Level", y = "Deer Damage CP", color = "Foraging Pressure Index") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  



#2. Large bodied deer -----------####
#Less sensitive to climatic extremes, less need of woodlands for thermoregulation
#Bulk-roughage grazers - will favour grazing outside of woodlands over woodland browse
#Browsing is opportunistic
#Hedgerows less important for connectivity/food/shelter - herding behaviour facilitates movement
#across open landscapes
#Heft to landscapes and use woodlands within them.

#Read in CPTs

ld_damage <- read.csv("C:/Users/ik929086/Documents/iDeer/data/BBN-cpts/post-interviews/large-deer/CPT_damage_risk_large_deer_final.csv")

#Recode to numbers
recode_mapping <- c("LOW" = 1, "MED" = 2, "HIGH" = 3)
# Recode the first four columns
CPT_ld <- ld_damage %>%
mutate(across(1:3, ~ recode(., !!!recode_mapping)))

#recode impact column

recode_mapping <- c("LOW" = 1, "LOW-MED" = 2, "MED" =3,"MED-HIGH" = 4,"HIGH"=5)
# Recode the first four columns
CPT_ld <- CPT_ld %>%
mutate(across(4, ~ recode(., !!!recode_mapping)))


ggplot(CPT_ld, aes(x = foraging_pressure_index, y = damage_risk, group = thermoreg_index)) +  
  geom_line() +
  geom_point() + 
  facet_grid(rows = vars(thermoreg_index), cols = vars(disturb_index)) +  
  theme_bw() +  
  scale_color_viridis_c(breaks = c(1, 2, 3)) + 
  labs(x = "Patch quality", col = "Landscape quality") +
  scale_y_continuous(sec.axis = sec_axis(~ . , name = "Thermoregulation Index", breaks = NULL, labels = NULL)) +
  scale_x_continuous(sec.axis = sec_axis(~ . , name = "Disturbance Index", breaks = NULL, labels = NULL), 
                     lim = c(1, 3), 
                     breaks = seq(1, 3, 1))
