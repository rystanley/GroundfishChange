#This file includes the temperature and depth ranges for atlantic halibut
#among the three different geographical regions (SS,GSL,NF)

#load packages
library(tidyverse) #for dyplyr and tidyr
library(ggplot2) #plotting
library(data.table) #faster readign and writing
library(sf)



## General Mapping information ----
#common projection 
latlong <- "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"

#read in NAFO division shape files
NAFO <- st_read("data/Divisions.shp")%>%
  st_transform(latlong)%>%
  select(ZONE,geometry)%>%
  filter(!is.na(ZONE))


#loading multi regional rv data
load("data/Halibut_MultiRegionData.RData")
halidat$region <- factor(halidat$region, levels=c("maritimes", "gulf", "newfoundland", "quebec"))

## Quick plots by Group-------------------

p1 <- ggplot(halidat,aes(x=depth,col=species))+
  stat_ecdf(geom="step",lwd=2)+
  facet_grid(region~.,scales="free_y")+
  theme_bw()+
  scale_x_log10()+
  annotation_logticks(sides="b")+
  labs(x="Depth (m)",y="ecdf",col="");p1

p2 <- ggplot(filter(halidat,temperature <= max(filter(halidat,species=="Halibut")%>%pull(temperature),na.rm=T)),
             aes(x=temperature,col=species))+
  stat_ecdf(geom="step",lwd=1.5)+
  facet_grid(region~.,scales="free_y")+
  theme_bw()+
  labs(x=expression(paste("Temperature ",degree,"C",sep="")),y="ecdf",col="");p2

ggsave("output/MultiRegion_HalibutDepth.png",p1,dpi=600,width=6,height=6,units="in")
ggsave("output/MultiRegion_HalibutTemp.png",p2,dpi=600,width=6,height=6,units="in") 

## Quick Plots by region by grouping----
#Grouping into SS, NF and GSL based on existing groupings
halidat <- halidat %>% mutate(broadregion = ifelse(halidat$region == "maritimes", "SS",
                                              ifelse(halidat$region == "newfoundland", "NF", "GSL")))
                                              

p3 <- ggplot(halidat,aes(x=depth,col=species))+
  stat_ecdf(geom="step",lwd=2)+
  facet_grid(broadregion~.,scales="free_y")+
  theme_bw()+
  scale_x_log10()+
  annotation_logticks(sides="b")+
  labs(x="Depth (m)",y="ecdf",col="");p3


p4 <- ggplot(filter(halidat,temperature <= max(filter(halidat,species=="Halibut")%>%pull(temperature),na.rm=T)),
             aes(x=temperature,col=species))+
  stat_ecdf(geom="step",lwd=1.5)+
  facet_grid(broadregion~.,scales="free_y")+
  theme_bw()+
  labs(x=expression(paste("Temperature ",degree,"C",sep="")),y="ecdf",col="");p4

ggsave("output/TriRegion_HalibutDepth.png",p3,dpi=600,width=6,height=6,units="in")
ggsave("output/TriRegion_HalibutTemp.png",p4,dpi=600,width=6,height=6,units="in")
  
## Quick plots by grouping by NAFO divsions into respective regions

#convert to sf object with assigned projection,assign each point a "ZONE" based on the NAFO zones (takes some time)
load("data/GeoRegions.RData")

regidat <- halidat %>%
  st_join(.,GeoRegions,join=st_intersects)%>%
  mutate(region=as.character(ZONE)) %>% filter(!is.na(region))


regidat$region <- factor(regidat$region, levels=c("SS", "GSL", "NF"))


#create stock column
regidat <- regidat %>% mutate(stock = ifelse(region == "GSL", "Northern","Southern"))


## Create dataframes for vertical lines
tt=regidat%>%
  data.frame()%>%
  filter(!is.na(stock), !is.na(temperature))%>%
  group_by(stock, species)%>%
  summarise(q1=quantile(temperature,0.05),q2=quantile(temperature,0.9))%>%
  ungroup()%>%
  data.frame()

td=regidat%>%
  data.frame()%>%
  filter(!is.na(stock), !is.na(depth))%>%
  group_by(stock, species)%>%
  summarise(q1=quantile(depth,0.1),q2=quantile(depth,0.9))%>%
  ungroup()%>%
  data.frame()

#Plotting ecdfs ----
f1 <- ggplot(regidat,aes(x=depth,col=species, linetype = species))+
  stat_ecdf(geom="step",lwd=1.4)+
  facet_grid(stock~.,scales="free_y")+
  geom_vline(aes(xintercept= q1), td, linetype =
               ifelse(td$species == "Halibut", 1, 2))+
  geom_vline(aes(xintercept= q2), td, linetype =
               ifelse(td$species == "Halibut", 1, 2))+
  scale_y_continuous(breaks = seq(0, 1, by = .2))+
  scale_x_log10(limits = c(20,1000))+
  annotation_logticks(sides="b")+
  theme_bw()+
  labs(x="Depth (m)",y="ecdf",col="", linetype = "");f1


f2 <- ggplot(filter(regidat,temperature <= max(filter(regidat,species=="Halibut")%>%pull(temperature),na.rm=T)),
             aes(x=temperature,col=species, linetype = species))+
  stat_ecdf(geom="step",lwd=1.4)+
  facet_grid(stock~.,scales="free_y")+
  geom_vline(aes(xintercept= q1), tt, linetype =
               ifelse(tt$species == "Halibut", 1, 2))+
  geom_vline(aes(xintercept= q2), tt, linetype =
               ifelse(tt$species == "Halibut", 1, 2))+
  scale_x_continuous(limits = c(-1,12), breaks = seq(0, 12, by = 2))+
  scale_y_continuous(breaks = seq(0, 1, by = .2))+
  theme_bw()+ 
  labs(x=expression(paste("Temperature ",degree,"C",sep="")),
       y="ecdf",col="", linetype = "");f2

ggsave("output/stockecdf_HalibutDepth.png",f1,dpi=600,width=6,height=6,units="in")
ggsave("output/stockecdf_HalibutTemp.png",f2,dpi=600,width=6,height=6,units="in")





