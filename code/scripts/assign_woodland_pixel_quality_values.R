woodpix <- read.csv(here("data/derived-data/Tool-extract-function-test/woodland_type_pixelIDs_5km.csv"))
woods <- woodpix
woods <- woods%>%rename(class = lc_class)
unique(woods$class)

#Load landscape nutritional scores for raster categories
#Edge forage quality scores sum of woodland score + adjacent land cover score
#Water scores added in to prevent loss of woodland edge pixels
#Water edge pixels receive one point higher than regular woodland
#Still an edge so will be highly nutritious
edge.val.types <- read.csv(file.path(here("data/raw-data/Expert_Qnaire/Expert_derived_quality_ranks_edge_types_with_water.csv")))
edge.val.types <- edge.val.types%>%dplyr::select(-"X")

#Check edge.val.types$class and woods$class match in format
unique(edge.val.types$class)
class(edge.val.types$class)
edge.val.types <- edge.val.types%>%arrange(class)
unique(edge.val.types$class)

class(woods$class)
woods <- woods%>%arrange(class)
unique(woods$class)
pcm <-woods%>%arrange(class)
unique(pcm$class)

# Round to 3 decimal places
woods$class <- round(woods$class, 3)
edge.val.types$class <- round(edge.val.types$class, 3)

#Stitch the quality scores into the lcm dataset

wood.edge.types <- woods %>% left_join(edge.val.types, by = c("class")) #%>%
                                          #filter(!is.na(Land.cover))

#Replace "NA" in Forage_Q column with 0
wood.edge.types <- wood.edge.types%>% 
 mutate(Forage_Q = ifelse(is.na(Forage_Q), 0, Forage_Q))%>%
   dplyr::select(-c("X","class","Land.cover"))%>%
  rename(woodland_q = Forage_Q)