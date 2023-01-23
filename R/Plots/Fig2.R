#load packages
library(tidyverse) #for dyplyr and tidyr
library(sf)
library(rnaturalearth)
## General Mapping information ----
#common projection 
latlong <- "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"

#read in NAFO division shape files
EEZ <- st_read("data/Canada_EEZ.shp")%>%
  st_transform(latlong)

NAFO <- st_read("data/Divisions.shp")%>%
  st_transform(latlong)%>%
  select(ZONE,geometry)%>%
  filter(!is.na(ZONE), grepl("[345]", ZONE)) 

NAFO$region =  ifelse(grepl("4S|4R", NAFO$ZONE), "NG",
                      ifelse(grepl("4T", NAFO$ZONE), "SG",
                             ifelse(grepl("4X|4W|4Vs|4Vn", NAFO$ZONE), "SS", 
                                    ifelse(grepl("3Pn|3Ps|3O|3N|3L|3M|3K", NAFO$ZONE), "NF",
                                           ifelse(grepl("5Y|5Ze|5Zw|6A|6B|6C|6D", NAFO$ZONE),"USA", NA)))))

NAFO$stock =  ifelse(grepl("4S|4R|4T", NAFO$ZONE), "GSL",
                             ifelse(grepl("4X|4W|4Vs|4Vn|3Ps|3O|3N", NAFO$ZONE), "SS-GBs", 
                                    ifelse(grepl("5Y|5Ze|5Zw", NAFO$ZONE), "GM-GB", NA)))

#Smaller USA zones that go into the canadian EEZ 
ss_ext <- st_intersection(EEZ, NAFO %>% filter(grepl("[5]", ZONE))) %>% select(region, geometry) %>% mutate(region = "SS")


Canada <- ne_states(country = "Canada",returnclass = "sf")%>%
  # st_transform(latlong)%>%
  # filter(name %in% c("Nunavut","Ontario","Québec","Newfoundland and Labrador",
  #                    "Nova Scotia","Prince Edward Island","New Brunswick"))%>%
  select(latitude,longitude,geonunit,geometry)%>%
  st_union()%>% #group provinces + territories
  st_as_sf()

USA <- ne_states(country = "United States of America",returnclass = "sf")%>%
  st_transform(latlong)%>%
  filter(region%in%c("Northeast","South"))%>%
  select(latitude,longitude,geonunit,geometry)%>%
  st_union()%>% #group states
  st_as_sf()


#With regions
p1 <- ggplot()+
  geom_sf(data = NAFO %>% filter(!is.na(region)) %>% select(-stock), mapping = aes(fill = region, geometry = geometry))+
  geom_sf_text(data=NAFO,aes(label=ZONE),colour="black", size = 3.2)+
  geom_sf(data = ss_ext, lwd = 0, color = NA, fill = "#ffffb3")+ 
  geom_sf(data = EEZ, lty = 2, fill = alpha("black", 0))+
  annotate("text", x = -57, y = 40.7, label = "EEZ", size = 3.2)+ #x = -47, y = 49.6
  annotate("text", x = -52.5, y = 50.7, label = "3K", size = 3.2)+ #annotate("text", x = -52.5, y = 50.7, label = "3K", size = 2.5)+ for TR #annotate("text", x = -48.7, y = 44, label = "3N", size = 2.5)+ #for BR
  geom_sf(data = USA) + 
  geom_sf(data = Canada) +
  coord_sf(xlim = c(-71.66,-42),  ylim = c(39,52), expand=FALSE)+
  #scale_fill_brewer(breaks=c("GM-GB", "GSL", "SS-GBs"), palette = "Set3")+
  scale_fill_manual(values = c("#8dd3c7", "#fb8072", "#bebada", "#ffffb3", "#80b1d3"))+
  geom_segment(aes(x = -67.5, y = 42.33, xend = -65.98, yend = 42.33))+
  theme_bw()+
  theme(legend.position = "bottom",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "grey", colour = "black"),
        plot.background = element_rect(colour = "grey"),
        strip.background = element_rect(colour = "black", fill = "grey"))+
  labs(x=expression(paste("Longitude ",degree,"W",sep="")),
       y=expression(paste("Latitude ",degree,"N",sep="")),
       fill = "Survey Region")
p1


#Without Region
p2 <- ggplot()+
  geom_sf(data = NAFO %>% filter(!is.na(region)),fill = "#99CBBB", mapping = aes(geometry = geometry))+ #99BBBB
  geom_sf_text(data=NAFO,aes(label=ZONE),colour="black", size = 2.5)+
  geom_sf(data = EEZ, lty = 2, aes(fill = NA))+
  annotate("text", x = -57, y = 40.7, label = "EEZ", size = 2.5)+ #x = -47, y = 49.6
  geom_sf(data = Canada, fill = "cornsilk") + 
  geom_sf(data = USA, fill = "cornsilk") + 
  coord_sf(xlim = c(-71.66,-42),  ylim = c(39,52), expand=FALSE)+
  scale_fill_brewer(palette = "Set2")+
  ##scale_linetype_manual(name = "tt", values = "dashed")+
  theme_bw()+
  theme(legend.position = "bottom",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white", colour = "black"),
        plot.background = element_rect(colour = "white"),
        strip.background = element_rect(colour = "black", fill = "white"))+
  labs(x=expression(paste("Longitude ",degree,"W",sep="")),
       y=expression(paste("Latitude ",degree,"N",sep="")),
       fill = "Survey Region")


library(cowplot)
world <- ne_countries(scale = "medium", returnclass = "sf")

worldmap <- ggplot(data = world) +
  geom_sf(data = world, aes()) +
  xlab("")+ylab("")+
  coord_sf(xlim = c(-80,20), ylim = c(20,65))+ 
  geom_rect(xmin = -71.66, xmax = -42, ymin = 39, ymax = 52,  #xlim = c(-71.66,-42),  ylim = c(39,52)
            fill = NA, colour = "black", size = 0.5) +
  annotate(geom = "text", x = -30, y = 50, label = "North Atlantic", 
           fontface = "italic", color = "grey22", size = 3) +
  theme_bw()+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white", colour = "black"),
        plot.background = element_rect(colour = "white"),
        axis.ticks.length=unit(0, "cm"),
        plot.margin = unit(c(0,0,-5,-5), "mm"),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        strip.background = element_rect(colour = "black", fill = "white"))#+
#labs(x=expression(paste("Longitude ",degree,"W",sep="")),
#    y=expression(paste("Latitude ",degree,"N",sep="")))
ggdraw(p1) +
  draw_plot(worldmap, width = 0.27, height = 0.36,
            x = 0.72, y = 0.675) #x = 0.72, y = 0.152 BR x = 0.72, y = 0.65 TR

ggsave(file = "Figures/Fig1_regions.tiff", dpi = 600, width = 7.25, height = 5.4375, units = "in")
ggsave(file = "Figures/Fig1_regions.jpeg", dpi = 300, width = 7.25, height = 5.4375, units = "in")


ggsave("Figure_RegionsMapTR.png")
