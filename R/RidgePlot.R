library(data.table)
library(sp)
library(sf) 
library(ggplot2)
library(here)
library(tidyverse)
library(rnaturalearth)
library(Hmisc)
library(ggridges)

## General Mapping information ----
  #common projection 
  latlong <- "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"
  
  #read in NAFO division shape files
  NAFO <- st_read("data/Divisions.shp")%>%
    st_transform(latlong)%>%
    select(ZONE,geometry)%>%
    filter(!is.na(ZONE))

## Bottom Temperature Data Prep ----
  
  if(!dir.exists("output")){dir.create("output/")} #create a output directory if it doesn't exist
  
##Load RData file of processed data
load("data/BNAM_Step1.RData")

##read and assign NAFO zones
  NAFO <- st_read("data/Divisions.shp")%>%
    st_transform(latlong)%>%
    select(ZONE,geometry)%>%
    filter(!is.na(ZONE))
  
  NAFOzones <- btmp%>%
    filter(Year==1990)%>% #since all years are the same we can just do this for one year and it will align
    st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%
    st_join(.,NAFO,join=st_intersects)%>%
    mutate(ZONE=as.character(ZONE))%>%pull(ZONE)

##Save Average Seasonal Data 

Pref_depth <- c(50,200,25,200)
Pref_temp <- c(3,8,1.5,10)

monthnames <- c("January","Febuary","March","April","May",
                "June","July","August","September",
                "October","November","December")

names(btmp)[grep("BTmp_",names(btmp))] = monthnames

hab  <-  btmp%>%
   mutate(NAFO = rep(NAFOzones,length(unique(Year))),
          Habitat = ifelse(Annual_AVG >= Pref_temp[1] & Annual_AVG <= Pref_temp[2] & 
                            Depth >= Pref_depth[1] & Depth <= Pref_depth[2], "Preffered",
                          ifelse(Annual_AVG >= Pref_temp[3] & Annual_AVG <= Pref_temp[4] & 
                                   Depth >= Pref_depth[3] & Depth <= Pref_depth[4],
                                 "Good", "Not_suitable")))%>%
  select(-c("Annual_AVG","Winter_AVG","Summer_AVG","If_land"))%>%
  gather(monthnames,key="Month",value="Temp")%>%
  mutate(Month=factor(Month,levels=rev(monthnames)))
 

p1 <- ggplot(filter(hab,Year%in%c(1993,2018),
                        Depth<Pref_depth[2],
                        Depth>Pref_depth[1],
                        NAFO %in% c("4X","3N")),
             aes(x=Temp,y=Month,fill=stat(x)))+
  geom_vline(xintercept=Pref_temp[1:2],lty=1,col="grey60")+ #Preferred habitat (solid line)
  geom_vline(xintercept=Pref_temp[3:4],lty=2,col="grey60")+ #Marginal habitat (dashed line)
  geom_density_ridges_gradient(rel_min_height = 0.01,alpha=0.9)+
  scale_fill_viridis_c(name=expression(paste("Temperature ",degree,"C",sep="")),option="C")+
  theme_bw()+
  labs(x=expression(paste("Temperature ",degree,"C",sep="")))+
  facet_grid(NAFO~Year);p1

ggsave("output/NAFO-Year-Ridges-Example.png",p1,dpi=600)

#Just summer
p2 <- ggplot(filter(hab,Year%in%c(1993,2018),
                    Depth<Pref_depth[2],
                    Depth>Pref_depth[1],
                    Month %in% monthnames[6:9],
                    NAFO %in% c("4X","3N")),
             aes(x=Temp,y=Month,fill=stat(x)))+
  geom_vline(xintercept=Pref_temp[1:2],lty=1,col="grey60")+ #Preferred habitat (solid line)
  geom_vline(xintercept=Pref_temp[3:4],lty=2,col="grey60")+ #Marginal habitat (dashed line)
  geom_density_ridges_gradient(rel_min_height = 0.01,alpha=0.9)+
  scale_fill_viridis_c(name=expression(paste("Temperature ",degree,"C",sep="")),option="C")+
  theme_bw()+
  labs(x=expression(paste("Temperature ",degree,"C",sep="")))+
  facet_grid(NAFO~Year);p2

ggsave("output/NAFO-Year-Ridge-example.png",p2,dpi=600)

#look at summer temp among years

hab2  <-  btmp%>%
  mutate(NAFO = rep(NAFOzones,length(unique(Year))),
         Habitat = ifelse(Annual_AVG >= Pref_temp[1] & Annual_AVG <= Pref_temp[2] & 
                            Depth >= Pref_depth[1] & Depth <= Pref_depth[2], "Preffered",
                          ifelse(Annual_AVG >= Pref_temp[3] & Annual_AVG <= Pref_temp[4] & 
                                   Depth >= Pref_depth[3] & Depth <= Pref_depth[4],
                                 "Good", "Not_suitable")))%>%
  select(-c(monthnames,"If_land"))%>%
  gather("Annual_AVG","Winter_AVG","Summer_AVG",key="Season",value="Temp")%>%
  mutate(Season=gsub("_AVG","",Season),
         Season=factor(Season,levels=c("Summer","Winter","Annual")))

p3 <- ggplot(filter(hab2,
                    Season=="Summer",
                    Depth<Pref_depth[2],
                    Depth>Pref_depth[1],
                    NAFO %in% c("4X","3N")),
             aes(x=Temp,y=factor(Year),fill=stat(x)))+
  geom_vline(xintercept=Pref_temp[1:2],lty=1,col="grey60")+ #Preferred habitat (solid line)
  geom_vline(xintercept=Pref_temp[3:4],lty=2,col="grey60")+ #Marginal habitat (dashed line)
  geom_density_ridges_gradient(rel_min_height = 0.01,alpha=0.9)+
  scale_fill_viridis_c(name=expression(paste("Temperature ",degree,"C",sep="")),option="C")+
  theme_bw()+
  facet_wrap(~NAFO,ncol=2)+
  labs(x=expression(paste("Temperature ",degree,"C",sep="")),y="Year");p3

ggsave("output/NAFO_SummerAve_Example.png",p3,dpi=600)
