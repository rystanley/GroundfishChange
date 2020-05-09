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
  mutate(region=as.character(ZONE))

regidat$region <- factor(regidat$region, levels=c("SS", "GSL", "NF"))


p5 <- ggplot(regidat,aes(x=depth,col=species, linetype = species))+
  stat_ecdf(geom="step",lwd=1)+
  facet_grid(region~.,scales="free_y")+
  geom_vline(aes(group = region), xintercept=quantile(regidat %>% filter(species == "Halibut", !is.na(depth)) %>%  .$depth, c(.05,.95)),lty=1,col="black")+ 
  geom_vline(aes(group = region), xintercept=quantile(regidat %>% filter(species == "Survey", !is.na(depth)) %>%  .$depth, c(.05,.95)),lty=2,col="black")+
  theme_bw()+
  scale_x_log10(limits = c(20,1000))+
  annotation_logticks(sides="b")+
  labs(x="Depth (m)",y="ecdf",col="", linetype = "");p5

ggplot(depth_ecdf, aes(Depth, colour = Group, linetype= Group)) + stat_ecdf(geom = "step", lwd=1.7)  + 
  scale_x_continuous(breaks = seq(0, 1000, by = 50), limits = c(0,300), expand = c(0,10))+
  geom_vline(xintercept=quantile(depth_ecdf %>% filter(Group == "Halibut") %>%  .$Depth, c(.05,.95)),lty=1,col="black")+ 
  geom_vline(xintercept=quantile(depth_ecdf %>% filter(Group == "Survey") %>%  .$Depth, c(.05,.95)),lty=2,col="black")+
  theme(axis.line = element_line(colour = "black"), panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), panel.background = element_blank(),
        legend.position = c(.9, 0.5), axis.title.x = element_text(margin = margin(t = 10)),
        text = element_text(size=17), axis.text.x = element_text(color = "grey20", size = 14, vjust = .5),
        axis.text.y = element_text(color = "grey20", size = 14, vjust = .5))+
  labs(x="Depth (m)",y="Proportion\n",col="", linetype = "");e2


p6 <- ggplot(filter(regidat,temperature <= max(filter(regidat,species=="Halibut")%>%pull(temperature),na.rm=T)),
             aes(x=temperature,col=species))+
  stat_ecdf(geom="step",lwd=1.5)+
  facet_grid(region~.,scales="free_y")+
  theme_bw()+
  labs(x=expression(paste("Temperature ",degree,"C",sep="")),y="ecdf",col="");p6

ggsave("output/NAFOTriRegion_HalibutDepth.png",p3,dpi=600,width=6,height=6,units="in")
ggsave("output/NAFOTriRegion_HalibutTemp.png",p4,dpi=600,width=6,height=6,units="in")
