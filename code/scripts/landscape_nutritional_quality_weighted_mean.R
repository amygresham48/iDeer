#CALCULATE NUTRITIONAL LANDSCAPE QUALITY ####

respoly<-read.csv(here("data/derived-data/Tool-extract-function-test/landscapemetrics_1000_5km.csv"))
lcm <- respoly
#lcm <- lcm%>%rename(class = lc_class)
#Load landscape nutritional scores for raster categories
#Edge forage quality scores sum of woodland score + adjacent land cover score
#Water scores retained to prevent loss of woodland edge pixels
#Water edge pixels receive one point higher than regular woodland
#Still an edge so will be highly nutritious
edge.val.types <- read.csv(file.path(here("data/raw-data/Expert_Qnaire/Expert_derived_quality_ranks_edge_types_with_water.csv")))
edge.val.types <- edge.val.types%>%dplyr::select(-"X")

#Stitch the quality scores into the lcm dataset

lc.edge.types <- lcm %>% left_join(edge.val.types, by = "class") %>% mutate(Forage_Q = ifelse(is.na(Forage_Q), 0, Forage_Q))%>%
  rename(scale = buffer_size)  %>%
 dplyr::select(-"X") %>% filter(!is.na(Land.cover))

#Calculate Nutritional Landscape Index (NLI) for separate edges and lc ####
#Weight edges by their type: https://www.sciencedirect.com/science/article/pii/S0304380009003871 #fig1  

# Calculate the weighted score for each pixel type
lc.edge.types <- lc.edge.types %>%
  mutate(weighted_score = perc_cov * Forage_Q)

# Sum the weighted scores for each SQUID section
weighted_sum_by_section_buffer <- lc.edge.types %>%
  group_by(patch_ID) %>%
  summarise(total_weighted_score = sum(weighted_score),
            total_perc_cov = sum(perc_cov))

# Calculate the weighted average score for each woodland polygon
weighted_average_score_buffer <- weighted_sum_by_section_buffer %>%
  mutate(weighted_avg_score = total_weighted_score / total_perc_cov)

weighted_average_score_buffer <- weighted_average_score_buffer %>%
  group_by(patch_ID) %>%
  summarise(
    nli_edge_types_1000 = sum(weighted_avg_score, na.rm = TRUE)
  )

# Join the calculated values back to the original dataset:
#woods_buffers <- st_filter(wood_polys, buffered_woods_5km, .predicate = st_intersects)
#lc.edge.types.NLI <- left_join(woods_buffers, #weighted_average_score_buffer, by = c("patch_ID"))