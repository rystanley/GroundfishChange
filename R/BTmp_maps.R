#Code to generate maps of bottom temperature

#Load libraries ----
  library(data.table)
  library(sp)
  library(sf) 
  library(ggplot2)
  library(tidyverse)
  library(rnaturalearth)
  library(Hmisc)
  library(viridis)
  library(viridisLite)

#Source functions ----
  source("R/make_map2.R")

## General Mapping information ----
#common projection 
latlong <- "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"


#Load data ----
load("data/BNAM_map.RData")

NAFO <- st_read("data/Divisions.shp")%>%
  st_transform(latlong)%>%
  select(ZONE,geometry)%>%
  filter(!is.na(ZONE), grepl("[345]", ZONE)) 
  

## Example Bottom Temperature Maps ----

#I have adjusted make_map to impose limits on your data if you have data that is out of the range. 
#however you loose some of the story. If you were doing maps between 1990 and 2018 I would look at the range 
#of data for those years and impose a fixed scale. I have also found that the viridis colour pallet
#has a nice colour scale for temperature (to my eyes anyway)

#Find the max and min values for each seasonal average among the years of interest.

Years <- c(1990,2018)
Seasons <- c("Annual_AVG","Winter_AVG","Summer_AVG")

lims <- map_temp%>%
        filter(Year %in% c(1990,2018))%>%
        gather(Seasons,key="Season",value="Temp")%>%
        group_by(Season)%>%
        summarise(min=min(Temp,na.rm=T),max=max(Temp,na.rm=T))%>%
        ungroup()%>%
        data.frame()
  
#Create the plots of interest

if(!dir.exists("output")){dir.create("output/")} #create a output directory if it doesn't exist

for(y in Years){
  for(s in Seasons){
    
    p1 <- make_map(xyz = filter(map_temp,Year==y)%>%
                         select(Longitude, Latitude, s), 
                   lims=c(filter(lims,Season==s)%>%pull(min),
                          filter(lims,Season==s)%>%pull(max)),
                   long.lim = c(-72.99797, -46))+ #get rid of non-halibut area on the plot
          scale_fill_viridis_c(limits=c(filter(lims,Season==s)%>%pull(min),
                                        filter(lims,Season==s)%>%pull(max)),
                               option = "A",
                               name=expression(paste("Temperature ",degree,"C",sep="")))+
          ggtitle(paste0("Average ",gsub("_AVG","",s)," Temperature ",y))+
          theme(legend.position = "right")
    
    ggsave(paste0("output/Average_",gsub("_AVG","",s),"_Temperature_",y,".png"),
           p1,dpi=600,width=8,height=6,units="in")
    
    
  }#end of Seasons loop
}#end of Years loop


## Single Averaged Map over all years
single_map <- map_temp%>% 
  select(Longitude, Latitude, Annual_AVG)%>%
  group_by(Latitude, Longitude)%>%
  summarise(mTemp = mean(Annual_AVG))

## @Ryan, for some reason this code doesn't work when I try to assign it (m1 <- make_map(...) and print it (;m1) any idea why?
#
m1 <- make_map(single_map,long.lim = c(-72.99797, -46),NAFO=NAFO)+ #get rid of non-halibut area on the plot
  scale_fill_viridis_c(limits=c(filter(lims,Season==Seasons[1])%>%pull(min),
                                filter(lims,Season==Seasons[1])%>%pull(max)),
                       option = "C",
                       name=expression(paste("Temperature ",degree,"C",sep="")))+
  ggtitle(paste0("Average ",gsub("_AVG","",Seasons[1])," Temperature "))+
  theme(legend.position = "right");m1

ggsave("output/map_temp.pdf",m1,dpi=600,width=8,height=6,units="in")