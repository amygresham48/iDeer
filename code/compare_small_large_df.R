df.no.nas.small <- readRDS("C:/Users/ik929086/Documents/iDeer/output/small_deer_full_df_no_nas.RDS")
df.no.nas.large <- readRDS("C:/Users/ik929086/Documents/iDeer/output/large_deer_full_df_no_nas.RDS")

#Compare datasets

#Edges
hist(df.no.nas.small$Focal400_EDGE_AREA_PERC)
hist(df.no.nas.large$Focal1000_EDGE_AREA_PERC)

#Linear features
hist(df.no.nas.small$Focal400_LF_PERC)

#DAMS
hist(df.no.nas.small$Focal400_MAX_SUMMED_DAMS)
hist(df.no.nas.large$Focal1000_MAX_SUMMED_DAMS)

#Connectivity
hist(df.no.nas.large$Focal1000_WOODS_PERC)
hist(df.no.nas.small$Focal400_WOODS_PERC)

#Alternative forage
hist(df.no.nas.large$Focal1000_PEREN_PERC)
hist(df.no.nas.large$Focal1000_ARABLE_PERC)
hist(df.no.nas.small$Focal400_PEREN_ARABLE_PERC)





