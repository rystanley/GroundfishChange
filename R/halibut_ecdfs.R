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

#Creating data frame for vlines in the ecdf plots
vdepth <- regidat %>% filter(!is.na(region), !is.na(depth)) %>% 
  group_by(region, species) %>% 
  nest() %>% 
  mutate(
    ret = map(data, ~quantile(.$depth, probs = c(0.1, 0.9))),
    ret = invoke_map(tibble, ret)
  ) %>%
  unnest(ret) %>% rename(low = "10%", high = "90%" )

vtemp <- regidat %>% filter(!is.na(region), !is.na(temperature)) %>% 
  group_by(region, species) %>% 
  nest() %>% 
  mutate(
    ret = map(data, ~quantile(.$temperature, probs = c(0.1, 0.9))),
    ret = invoke_map(tibble, ret)
  ) %>%
  unnest(ret) %>% rename(low = "10%", high = "90%" )


#Plotting ecdfs ----
p5 <- ggplot(regidat,aes(x=depth,col=species, linetype = species))+
  stat_ecdf(geom="step",lwd=1)+
  facet_grid(region~.,scales="free_y")+
  geom_vline(aes(xintercept= low), vdepth, linetype =
             ifelse(vdepth$species == "Halibut", 1, 2))+
  geom_vline(aes(xintercept= high), vdepth, linetype =
               ifelse(vdepth$species == "Halibut", 1, 2))+
  theme_bw()+
  scale_x_log10(limits = c(20,1000))+
  annotation_logticks(sides="b")+
  labs(x="Depth (m)",y="ecdf",col="", linetype = "");p5


p6 <- ggplot(filter(regidat,temperature <= max(filter(regidat,species=="Halibut")%>%pull(temperature),na.rm=T)),
             aes(x=temperature,col=species, linetype = species))+
  stat_ecdf(geom="step",lwd=1)+
  facet_grid(region~.,scales="free_y")+
  geom_vline(aes(xintercept= low), vtemp, linetype =
               ifelse(vtemp$species == "Halibut", 1, 2))+
  geom_vline(aes(xintercept= high), vtemp, linetype =
               ifelse(vtemp$species == "Halibut", 1, 2))+
  scale_x_continuous(limits = c(-1,12))+
  theme_bw()+ 
  labs(x=expression(paste("Temperature ",degree,"C",sep="")),
       y="ecdf",col="", linetype = "");p6

ggsave("output/NAFOTriRegion_HalibutDepth.png",p5,dpi=600,width=6,height=6,units="in")
ggsave("output/NAFOTriRegion_HalibutTemp.png",p6,dpi=600,width=6,height=6,units="in")


