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
  #read in EEZ shape file
    EEZ <- st_read("data/Canada_EEZ.shp")%>%
      st_transform(latlong)%>%
      select(EEZ, geometry)
  #cropping NAFO divisions by EEZ, by using st_intersects to keep only the area that is in both
    NAFO <- NAFO %>% st_intersection(EEZ) %>% select(ZONE, geometry)

## Bottom Temperature Data Prep ----

  ##Load RData file of processed data
    load("data/BNAM_Step1.RData")

     # can put it right in the pipe!
    ##Save Average Seasonal Data
    temp <- btmp %>%
      select(Longitude, Latitude, Depth, Winter_AVG, Summer_AVG, Annual_AVG, Year)%>%
      st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%
      st_join(.,NAFO,join=st_intersects)%>%
      mutate(ZONE=as.character(ZONE))%>%  
      st_set_geometry(NULL)%>%
      filter(grepl("[34]", ZONE))
 
  save(temp, file = "data/BNAM_T.RData") #save didn't work when I piped it
## Habitat Proportion Prep ----
    #Ryan edit - Put this all in pipe and pre-defined the limits. THis way you can more quickly
    #play with the limits and don't have to go through the mess of code to do it. 

  Pref_depth <- c(25,200,25,200)
  Pref_temp <- c(3,15,1,17)

  hab  <-  btmp%>%
         select(Longitude, Latitude, Depth, Winter_AVG, Summer_AVG, Annual_AVG, Year)%>%
         mutate(Habitat = ifelse(Annual_AVG >= Pref_temp[1] & Annual_AVG <= Pref_temp[2] & 
                                 Depth >= Pref_depth[1] & Depth <= Pref_depth[2], "Preferred",
                                 ifelse(Annual_AVG >= Pref_temp[3] & Annual_AVG <= Pref_temp[4] & 
                                        Depth >= Pref_depth[3] & Depth <= Pref_depth[4],
                                                     "Good", "Not_suitable")))


#convert to sf object with assigned projection,assign each point a "ZONE" based on the NAFO zones (takes some time)
  hab  <-  hab %>% st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%
    st_join(.,NAFO,join=st_intersects)%>%
    mutate(ZONE=as.character(ZONE))

#create new dataframe without geometry for quick computational analysis
#filter for zones of interest (might remove 3K, 3L and 3M because they are not one fo the two stocks, ask nancy)
hab  <- hab %>% st_set_geometry(NULL)%>%
  filter(grepl("[34]", ZONE))


#convert habitat to factor in order for group_by(.drop = FALSE) to work (leaves in groups containing 0)
#group by year and zone and count habitats for each year and zone
hab_group <-  hab%>%
            mutate(Habitat=factor(Habitat))%>% 
            group_by(Year, ZONE, .drop = FALSE) %>% 
            count(Habitat) %>%
            mutate(Habitat=as.character(Habitat)) #Convert back to string because otherwise loop returns NAs and idk why


## Ryan Note ** this is a great way to do this, and shows the nice indexing logic of R. 
#However dplyr and tidyr make this even more seamless. I took your steps and piped it into one 
#continuous pipe. This is not the right way to do this, as there is no right way to do this, however,
#it is sometimes better to have it all in one step so that when repeating the code you don't miss
#a step and then mess up the process. Hopefully this can be followed from your previous steps (commented below)

prop_hab <- hab_group%>%spread(key=Habitat,value=n)%>%
             mutate(Total = Good + Preferred + Not_suitable,
                    Good = (Good + Preferred)/Total, #proportion good habitat
                    Preferred = Preferred/Total,
                    Not_suitable = Not_suitable/Total)%>%# note it is key that these are done sequentially as each mutate steps over writes the previous column
            select(Year, ZONE, Preferred, Good)%>%
            gather(Preferred:Good, key = Habitat, value = Proportion) #melting data to long format

    # #create character vector containg all unique habitat names
    # #create dataframe containing all zones by year
    # habitats = c(unique(hab_group$Habitat))
    # total_hab = filter(hab_group , Habitat == "Good")[,1:2]
    # 
    # #assign the count of each habitat by zone and year to a new column of total_hab and name them accordingly
    # for(i in 1:3){
    #   nam = habitats[i]
    #   total_hab[,i+2] =filter(hab_group, Habitat == habitats[i])[,4]
    # }
    # names(total_hab)[-(1:2)] = habitats

    # 
    # #create new dataframe with year, zone, habitat and actualy numbers to calculate percentages
    # #create new dataframe where some vars are not exclusive i.e good should include Preferred
    # 
    # prop_hab = total_hab
    # prop_hab$Total = total_hab$Good + total_hab$Preferred + total_hab$Not_suitable 
    # prop_hab$Good = total_hab$Good + total_hab$Preferred

    # convert numbers to proportions
    # prop_hab$Preferred = prop_hab$Preferred/prop_hab$Total
    # prop_hab$Good = prop_hab$Good/prop_hab$Total


    # #clean up data frame
    # prop_hab = prop_hab %>% select(Year, ZONE, Preferred, Good)
    # 
    # #melting data to long format
    # #must be a data.table to melt 
    # prop_hab = prop_hab %>% gather(Preferred:Good, key = Habitat, value = Proportion)

save(prop_hab, file = "data/BNAM_hab.RData")



## GDD Data Prep ----

  gdd = btmp %>% 
    mutate(Year = rep(1990:2018,each= dim(btmp)[1]/29)) %>% 
    filter(If_land != 0) %>% 
    select(-If_land, -Winter_AVG, -Summer_AVG, -Annual_AVG) %>% 
    filter(Longitude >= -73, Longitude <= -40 ,Latitude >= 38, Latitude <= 53) %>% 
    rename(M1 = BTmp_M1, M2 = BTmp_M2, M3 = BTmp_M3, M4 = BTmp_M4, M5 = BTmp_M5, M6 = BTmp_M6, M7 = BTmp_M7, M8 = BTmp_M8,
           M9 = BTmp_M9, M10 = BTmp_M10, M11 = BTmp_M11, M12 = BTmp_M12)


#convert to sf object with assigned projection
#assign each point a "ZONE" based on the NAFO zones (takes some time)
#convert ZONE from factor to character
  gdd = gdd %>% st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%
    st_join(.,NAFO,join=st_intersects)%>%
    mutate(ZONE=as.character(ZONE))

#create new dataframe without geometry for quick computational analysis
#filter for zones of interest (might remove 3K, 3L and 3M because they are not one fo the two stocks, ask nancy)
  gdd = gdd %>% st_set_geometry(NULL)%>%
    filter(grepl("[34]", ZONE))




## GDD Calculation Prep ----
  gdd = gdd %>%  gather(M1:M12, key = Month, value = Temp) %>% mutate(Depth_range = ifelse(Depth >= 25 & Depth <= 400, "Within", "Outside"), DPM = ifelse(grepl("//bM1//b|M3|M5|M7|M8|M10|M12", .$Month), 31, ifelse(grepl("M4|M6|M9|M11", .$Month), 30, 28))) %>% 
    mutate(GDD = Temp*DPM)
  
  
#Create new data frame with juvenile habitat depth range 
  gdd_dep = filter(gdd, Depth_range == "Within")
  

## GDD Estimate Scaled Mean #1 ----

#Reduce select only temperatures above 3 degrees for scaled calculation
  gdd_red = gdd_dep %>% filter(Temp >= 3)

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

  
  save(GDD, file = "GDD.RData")
  


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
