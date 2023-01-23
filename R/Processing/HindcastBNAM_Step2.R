library(tidyverse)
library(sf)

## General Mapping information ----
  #common projection 
    latlong <- "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"

  #read in NAFO division shape files
    NAFO <- st_read("data/Divisions.shp")%>%
      st_transform(latlong)%>%
      select(ZONE,geometry)%>%
      filter(!is.na(ZONE))


## Bottom Temperature Data Prep ----
  ##Load RData file of processed data
    load("data/BNAM_Step1.RData")

    ##Save Average Seasonal Data
    temp <- btmp %>%
      select(Longitude, Latitude, Depth, Annual_AVG, Year)%>%
      st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%
      st_join(.,NAFO,join=st_intersects)%>%
      mutate(ZONE=as.character(ZONE))%>%  
      st_set_geometry(NULL)%>%
      filter(grepl("[345]", ZONE))
 
  save(temp, file = "data/BNAM_T.RData") #save didn't work when I piped it
## Habitat Proportion Prep ----

  Pref_depth <- c(25,686)
  Pref_temp <- c(2.4,18.99) ##

  hab  <-  btmp%>%
         select(Longitude, Latitude, Depth, Annual_AVG, Year)%>%
         mutate(Habitat = ifelse(Annual_AVG >= Pref_temp[1] & Annual_AVG <= Pref_temp[2] & 
                                 Depth >= Pref_depth[1] & Depth <= Pref_depth[2], "Within",
                                                     "Outside"))


#convert to sf object with assigned projection,assign each point a "ZONE" based on the NAFO zones (takes some time)
  hab  <-  hab %>% st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%
    st_join(.,NAFO,join=st_intersects)%>%
    mutate(ZONE=as.character(ZONE))

#create new dataframe without geometry for quick computational analysis
hab  <- hab %>% st_set_geometry(NULL)%>%
  filter(grepl("[345]", ZONE))


#convert habitat to factor in order for group_by(.drop = FALSE) to work (leaves in groups containing 0)
#group by year and zone and count habitats for each year and zone
hab_group <-  hab%>%
            mutate(Habitat=factor(Habitat))%>% 
            group_by(Year, ZONE, .drop = FALSE) %>% 
            count(Habitat) %>%
            mutate(Habitat=as.character(Habitat)) #Convert back to string because otherwise loop returns NAs and idk why



prop_hab <- hab_group%>%spread(key=Habitat,value=n)%>%
             mutate(Total = Within + Outside,
                    Within_hab = Within/Total, #proportion good habitat
                    Outside_hab = Outside/Total) %>% # note it is key that these are done sequentially as each mutate steps over writes the previous column
            select(Year, ZONE, Within_hab, Outside_hab) %>%
            gather(Within_hab:Outside_hab, key = Habitat, value = Proportion) #melting data to long format

save(prop_hab, file = "data/BNAM_hab_2021.RData")

## GDD Data Prep ----

  gdd = btmp %>% 
    select(-If_land, -Winter_AVG, -Summer_AVG, -Annual_AVG) %>% 
    rename(M1 = BTmp1, M2 = BTmp2, M3 = BTmp3, M4 = BTmp4, M5 = BTmp5, M6 = BTmp6, M7 = BTmp7, M8 = BTmp8,
           M9 = BTmp9, M10 = BTmp10, M11 = BTmp11, M12 = BTmp12)

#convert to sf object with assigned projection
#assign each point a "ZONE" based on the NAFO zones (takes some time)
#convert ZONE from factor to character
  gdd = gdd %>% st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%
    st_join(.,NAFO,join=st_intersects)%>%
    mutate(ZONE=as.character(ZONE))

#create new dataframe without geometry for quick computational analysis
#filter for zones of interest (might remove 3K, 3L and 3M because they are not one fo the two stocks, ask nancy)
  gdd = gdd %>% st_set_geometry(NULL)%>%
    filter(grepl("[345]", ZONE))

## GDD Calculation Prep ---- 
  gdd = gdd %>%  gather(M1:M12, key = Month, value = Temp) %>% mutate(Depth_range = ifelse(Depth >= Pref_depth[1] & Depth <= Pref_depth[2], "Within", "Outside"), DPM = ifelse(grepl("//bM1//b|M3|M5|M7|M8|M10|M12", .$Month), 31, ifelse(grepl("M4|M6|M9|M11", .$Month), 30, 28))) %>% 
    mutate(GDD = ifelse(Temp >= Pref_temp[1], Temp*DPM, 0))
  
#Create new data frame with juvenile habitat depth range 
  gdd_dep = filter(gdd, Depth_range == "Within")
 
## GDD Estimate Scaled Mean #1 ---- #used

#Reduce select only temperatures above 3 degrees for scaled calculation
  gdd_red = gdd_dep %>% filter(Temp >= Pref_temp[1])

#Find number of observation in each zone
  nafo_dims = gdd_dep %>% filter(Year == 1990, Month == "M1") %>% group_by(ZONE) %>% count() %>% rename(Tot_obs = n)

  gdd_mean = gdd_red %>% group_by(Year, Month, ZONE) %>% summarise(mGDD = mean(GDD))
  gdd_count = gdd_red %>% group_by(Year, Month, ZONE) %>% count()
  
  gdd_f = merge(gdd_mean, gdd_count) %>% merge(nafo_dims)
  
  gdd_scaled = gdd_f %>% mutate(sGDD = mGDD*n/Tot_obs)
  gdd_scaled = gdd_scaled %>% group_by(Year, ZONE) %>% summarise(sGDD = sum(sGDD))


## GDD Estimate 2 ----

gdd_average = gdd_dep %>% mutate(mGDD = ifelse(Temp >= 3, Temp*DPM, 0)) %>% group_by(Year, ZONE) %>% summarise(mGDD = mean(mGDD))
 
## GDD Estimate 3 ----

#If Temp > 3, then 30, else 0. row means 
  gdd_E3 = gdd_dep %>%  mutate(Days_Above3 = ifelse(Temp >= 3, DPM, 0)) 

#Creating data frame with total number of growing degree days per observation for annual winter and summer
  gdd_E3 = gdd_E3 %>% group_by(Year, ZONE) %>% summarise(mDays = mean(Days_Above3))
    
#Compile GDD estimates into one data frame

  GDD = merge(gdd_scaled, gdd_average) %>% merge(gdd_E3)

  
  save(GDD, file = "GDD_2021.RData")
  


## Mapping Temp Prep ----
  
  ##Removing monthly data from dataframe
  map_temp = btmp %>% select(Longitude, Latitude, Annual_AVG, Winter_AVG, Summer_AVG, Year)
  
  save(map_temp, file = "BNAM_map.RData")
  
## Stock Calculation for later

gdd_means = gdd_means %>% mutate(Stock = ifelse(grepl("4T|4S|4R", gdd_means$ZONE), "GSL",
                                                ifelse(grepl("4X|4W|4Vs|3Pn|3Ps|3O|3N", gdd_means$ZONE), "SS_NF", "NA")))
gdd_stock = gdd_means %>% filter(Stock != "NA") %>% 
  group_by(Year, Stock, Season) %>% 
  summarise_at(.vars = c("GDD"), list(mean))




