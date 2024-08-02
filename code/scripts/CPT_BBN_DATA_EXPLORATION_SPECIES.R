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

sb_thermoreg <- read.csv(here("data/BBN-cpts/post-interviews/CPT_thermoreg_small_deer.csv"))


#2. Large bodied deer -----------####
#Less sensitive to climatic extremes, less need of woodlands for thermoregulation
#Bulk-roughage grazers - will favour grazing outside of woodlands over woodland browse
#Browsing is opportunistic
#Hedgerows less important for connectivity/food/shelter - herding behaviour facilitates movement
#across open landscapes
#Heft to landscapes and use woodlands within them.