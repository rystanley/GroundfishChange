
#Load in libraries
library(R.matlab)
library(tidyverse)
library(data.table)
library(sf)


#Region Data ----
#common projection 
latlong <- "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"

#read bnam_PC

NAFO <- st_read("data/Divisions.shp")%>%
  st_transform(latlong)%>%
  select(ZONE,geometry)%>%
  filter(!is.na(ZONE), grepl("[3456]", ZONE)) 

NAFO$region =  ifelse(grepl("4S|4R", NAFO$ZONE), "NG",
                      ifelse(grepl("4T", NAFO$ZONE), "SG",
                             ifelse(grepl("4X|4W|4Vs|4Vn", NAFO$ZONE), "SS", 
                                    ifelse(grepl("3Pn|3Ps|3O|3N|3L|3M|3K", NAFO$ZONE), "NF",
                                           ifelse(grepl("5Y|5Ze|5Zw|6A|6B|6C|6D", NAFO$ZONE),"USA", NA)))))

######BNAM DATA#######
#Covert mat files into a data frame
#Present Climate ----
PC = readMat('data/BNAM/Tbtm_PC.mat')
F1_R45 = readMat('data/BNAM/dTbtm_dSbtm_F_R45_2046-2065.mat')
F1_R85 = readMat('data/BNAM/dTbtm_dSbtm_F_R85_2046-2065.mat')
F2_R45 = readMat('data/BNAM/dTbtm_dSbtm_F_R45_2066-2085.mat')
F2_R85 = readMat('data/BNAM/dTbtm_dSbtm_F_R85_2066-2085.mat')

# 
# bnam <- setNames(do.call(rbind.data.frame, Map('c', 
#                                                   (PC$nav.lon) %>% as.list(),
#                                                   (PC$nav.lat) %>% as.list(),
#                                                   (PC$Bathy.depth) %>% as.list(),
#                                                   (PC$land.mask) %>% as.list(),
#                                                   (PC$Tbtm.ann) %>% as.list(),
#                                                   (F1_R45$RCP45.2055.dTbtm) %>% as.list(),
#                                                   (F1_R85$RCP85.2055.dTbtm) %>% as.list(),
#                                                   (F2_R45$RCP45.2075.dTbtm) %>% as.list(),
#                                                   (F2_R85$RCP85.2075.dTbtm) %>% as.list()),
#                             ),
#                     c("longitude","latitude", "depth", "land", "btm_temp_PC", "delta_F1_R45", "delta_F1_R85", "delta_F2_R45", "delta_F2_R85")) %>% 
#   filter(land == 1, between(longitude, -71.66,-43), between(latitude, 40,53))
# 
# 
# #add NAFO regions and nafo divisions to data
# bnams <- bnam %>% st_as_sf(coords=c("longitude","latitude"),crs=latlong, remove = FALSE)%>%
#   st_join(.,NAFO,join=st_intersects)%>%
#   mutate(ZONE=as.character(ZONE))%>%  
#   st_set_geometry(NULL) %>% 
#   filter(!is.na(region)) %>% 
#   select(-land) %>% 
#   mutate(region = factor(region))
# 
# bnams <- bnams %>% mutate(btm_temp_F1_R45 = btm_temp_PC+delta_F1_R45,
#                           btm_temp_F1_R85 = btm_temp_PC+delta_F1_R85,
#                           btm_temp_F2_R45 = btm_temp_PC+delta_F2_R45,
#                           btm_temp_F2_R85 = btm_temp_PC+delta_F2_R85)
# 
# 
# 
# save(bnams, file = "data/BNAM/BNAM.RData")
# 



##########Monthly Forecast

btmp_list <- list(F1_R45, F1_R85, F2_R45, F2_R85)

month_list = rep(list(list()),12) #creating empty list for months
forecast_list <- rep(list(list()),4)

for(g in 1:12){ #number of months
  month_list[[g]] = as.list(PC$Tbtm.mon[g, 1:1001, 1:1250]) #takes the first element of each list (a matrix) and concerts it into a list within the ith element of month_list
  
}

pc_bnam = setNames(do.call(rbind.data.frame, Map('c', 
                                                           (PC$nav.lon) %>% as.list(),
                                                           (PC$nav.lat) %>% as.list(),
                                                           (PC$Bathy.depth) %>% as.list(),
                                                           (PC$land.mask) %>% as.list(), 
                                                           month_list[[1]], 
                                                           month_list[[2]], 
                                                           month_list[[3]], 
                                                           month_list[[4]], 
                                                           month_list[[5]], 
                                                           month_list[[6]], 
                                                           month_list[[7]], 
                                                           month_list[[8]], 
                                                           month_list[[9]], 
                                                           month_list[[10]], 
                                                           month_list[[11]],
                                                           month_list[[12]])), 
                             c("longitude","latitude", "depth", "land", paste("PC", 1:12, sep = ".")))  %>% 
  filter(land == 1, between(longitude, -71.66,-43), between(latitude, 38,53))

forcast_names = c("F1_R45", 'F1_R85', 'F2_R45', 'F2_R85')

for(i in 1:length(btmp_list)){
  for(g in 1:12){ #number of months
    month_list[[g]] = as.list(btmp_list[[i]][[8]][g, 1:1001, 1:1250]) #takes the first element of each list (a matrix) and concerts it into a list within the ith element of month_list
    
  }
  
  forecast_list[[i]] <- setNames(do.call(rbind.data.frame, Map('c', 
                                                               (PC$nav.lon) %>% as.list(),
                                                               (PC$nav.lat) %>% as.list(),
                                                               (PC$Bathy.depth) %>% as.list(),
                                                               (PC$land.mask) %>% as.list(), 
                                                               month_list[[1]], 
                                                               month_list[[2]], 
                                                               month_list[[3]], 
                                                               month_list[[4]], 
                                                               month_list[[5]], 
                                                               month_list[[6]], 
                                                               month_list[[7]], 
                                                               month_list[[8]], 
                                                               month_list[[9]], 
                                                               month_list[[10]], 
                                                               month_list[[11]],
                                                               month_list[[12]])), 
                                 c("longitude","latitude", "depth", "land", paste(forcast_names[i], 1:12, sep = ".")))  %>% 
    filter(land == 1, between(longitude, -71.66,-43), between(latitude, 38,53))
 
   assign(forcast_names[i], forecast_list[[i]]) 
   

}

PC <- pc_bnam %>% pivot_longer(cols = contains("."), names_to = 'month', values_to = 'btm_temp_PC') %>% mutate(month = as.integer(str_extract(month, "[:digit:]+")),
                                                                                                            season = case_when(between(month, 1, 3) ~ "Winter",
                                                                                                                               between(month, 4,6) ~ "Spring",
                                                                                                                               between(month, 7, 9) ~ "Summer",
                                                                                                                               between(month, 10, 12) ~ "Fall"))
save(PC,file = 'data/BNAM/PC.Rdata')

F1_R45 <- F1_R45 %>% pivot_longer(cols = contains("."), names_to = 'month', values_to = 'btm_temp_F1_R45') %>% mutate(month = as.integer(str_extract(month, "[:digit:]+")),
                                                                                                               season = case_when(between(month, 1, 3) ~ "Winter",
                                                                                                                                  between(month, 4,6) ~ "Spring",
                                                                                                                                  between(month, 7, 9) ~ "Summer",
                                                                                                                                  between(month, 10, 12) ~ "Fall"))
save(F1_R45, file = 'data/BNAM/F1_R45.Rdata')
F1_R85 <- F1_R85 %>% pivot_longer(cols = contains("."), names_to = 'month', values_to = 'btm_temp_F1_R85') %>% mutate(month = as.integer(str_extract(month, "[:digit:]+")),
                                                                                                               season = case_when(between(month, 1, 3) ~ "Winter",
                                                                                                                                  between(month, 4,6) ~ "Spring",
                                                                                                                                  between(month, 7, 9) ~ "Summer",
                                                                                                                                  between(month, 10, 12) ~ "Fall"))
save(F1_R85, file = 'data/BNAM/F1_R85.Rdata')
F2_R45 <- F2_R45 %>% pivot_longer(cols = contains("."), names_to = 'month', values_to = 'btm_temp_F2_R45') %>% mutate(month = as.integer(str_extract(month, "[:digit:]+")),
                                                                                                               season = case_when(between(month, 1, 3) ~ "Winter",
                                                                                                                                  between(month, 4,6) ~ "Spring",
                                                                                                                                  between(month, 7, 9) ~ "Summer",
                                                                                                                                  between(month, 10, 12) ~ "Fall"))
save(F2_R45, file = 'data/BNAM/F2_R45.Rdata')
F2_R85 <- F2_R85 %>% pivot_longer(cols = contains("."), names_to = 'month', values_to = 'btm_temp_F2_R85') %>% mutate(month = as.integer(str_extract(month, "[:digit:]+")),
                                                                                                               season = case_when(between(month, 1, 3) ~ "Winter",
                                                                                                                                  between(month, 4,6) ~ "Spring",
                                                                                                                                  between(month, 7, 9) ~ "Summer",
                                                                                                                                  between(month, 10, 12) ~ "Fall"))
save(F2_R85, file = 'data/BNAM/F2_R85.Rdata')


forecastData <- data.frame(longitude = PC$longitude, 
                 latitude = PC$latitude, 
                 depth = PC$depth, 
                 month = PC$month, 
                 season = PC$season, 
                 btm_temp_PC = PC$btm_temp_PC, 
                 btm_temp_F1_R45 = F1_R45$btm_temp_F1_R45,
                 btm_temp_F1_R85 =  F1_R85$btm_temp_F1_R85,
                 btm_temp_F2_R45 = F2_R45$btm_temp_F2_R45,
                 btm_temp_F2_R85 = F2_R85$btm_temp_F2_R85)
#complete with 

forecast <- forecastData %>% st_as_sf(coords=c("longitude","latitude"),crs=latlong, remove = FALSE)%>%
    st_join(.,NAFO,join=st_intersects)%>%
    mutate(ZONE=as.character(ZONE))%>%
    st_set_geometry(NULL) %>%
    filter(!is.na(region)) %>%
    mutate(region = factor(region))

  forecast <- forecast %>% mutate(btm_temp_F1_R45 = btm_temp_PC+btm_temp_F1_R45,
                            btm_temp_F1_R85 = btm_temp_PC+btm_temp_F1_R85,
                            btm_temp_F2_R45 = btm_temp_PC+btm_temp_F2_R45,
                            btm_temp_F2_R85 = btm_temp_PC+btm_temp_F2_R85)


save(forecast, file = "forecastBNAM.Rdata")

#save as R.data
#create model using month
#if there is an issue for lack of data in some months try season (or season to reduce work)
#then create a map and make sure that code works,
#might have to create a map for each season? An overall map? Average?
#