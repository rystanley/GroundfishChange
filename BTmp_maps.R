#Code to generate maps of bottom temperature

#Load libraries ----
  library(data.table)
  library(sp)
  library(sf) 
  library(ggplot2)
  library(tidyverse)
  library(rnaturalearth)
  library(Hmisc)

#Source functions ----
  source("R/make_map2.R")

#Load data ----
  load("data/BNAM_map.RData")


## 1990 Bottom Temperature Map ----

 map_1990 = map_temp%>%
            filter(Year == 1990)%>%
            select(Longitude, Latitude, Annual_AVG)  #miun = -1, max = 13

  temp_1990 = make_map(xyz = map_1990, variable = "Temperature",lims=c(-1,5)) 
  
    temp_1990 + theme(legend.position = "right") + ggtitle("Average Bottom Temperature 1990") +
      scale_fill_gradientn(colours=topo.colors(7), breaks=c(0,2,4,6,10),limits=c(-1, 5)) 
    

    
  map_2018 =select(filter(map_temp, Year == 2018), Longitude, Latitude, Annual_AVG) 
    
  temp_2018 = make_map(xyz = map_2018, variable="Temperature")
    
    temp_2018 + theme(legend.position = "right") + ggtitle("Average Bottom Temperature 2018") +  scale_fill_gradient(limits=c(-1, 15))
    