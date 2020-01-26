
## Load Packages ----

library(tidyverse) #dplyr and tidyr
library(ggplot2) #plotting
library(data.table) #faster processing


rv = fread("data/Halibut_2018_RV.csv")

#Select columns that will be used
rv = rv %>% select(LATITUDE, LONGITUDE, YEAR, SEASON, NAME, DMAX, BOTTOM_TEMPERATURE, BOTTOM_SALINITY, TOTALNUMBERSTANDARDIZED, TOTALWEIGHTSTANDARDIZED_KG, PRESENCE) %>% rename(Zone = NAME, Depth = DMAX, Temp = BOTTOM_TEMPERATURE, Year = YEAR, Season = SEASON, Salinity = BOTTOM_SALINITY, Abundance = TOTALNUMBERSTANDARDIZED, Kg = TOTALWEIGHTSTANDARDIZED_KG, Presence = PRESENCE)

#Changing NA's to 0 in for abundance and weight
rv = rv %>% replace_na(list(Abundance = 0, Kg = 0))

#Rounding up for abundance in order to plot
rv_round = rv
rv_round$Abundance = ceiling(rv_round$Abundance)

## Temperature distribution curve ----
rv_temp = rv_round %>% filter(!is.na(Temp), !is.na(Zone)) %>% mutate(Region = ifelse(grepl("\\b4T\\b", .$Zone), "GSL", "SS"), T_range = cut(.$Temp, breaks = c(seq(from = floor(min(.$Temp)), to = ceiling(max(.$Temp)), by = .5))))
# floor(min(rv$Temp))
#Expanding by # in abundance for plotting
obs_hal = rv_temp %>% filter(Presence == "P") %>% uncount(Abundance)


  
## Plots all areas sampled by temp ----
ggplot(rv_temp, aes(x = T_range)) + geom_histogram(stat = "count") +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) + theme(axis.line = element_line(colour = "black"),
                                                                      panel.grid.major = element_blank(),
                                                                      panel.grid.minor = element_blank(),
                                                                      panel.background = element_blank()) +
  ggtitle("Temperatures Surveyed")#outer = -1-13 , middle = 1-10 , inner = 4-8
  
## Binned temperature plot
ggplot(obs_hal, aes(x = T_range)) + geom_histogram(stat = "count") +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) + theme(axis.line = element_line(colour = "black"),
                                                                      panel.grid.major = element_blank(),
                                                                      panel.grid.minor = element_blank(),
                                                                      panel.background = element_blank()) +
  ggtitle("Abundance of Halibut by Temperature") # outer = 0-12 , middle = 3-10, inner 4-9, narrow inner = 5-8
#create binned barplot for all areas sampled present or not



## Depth Distribution Curve

rv_depth = rv_round %>% filter(!is.na(Depth)) %>% mutate(D_range = cut(.$Depth, breaks = c(seq(from = floor(min(.$Depth)), to = ceiling(max(.$Depth))-660, by = 10))))

obs_depth_hal = rv_depth %>% filter(Presence == "P") %>% uncount(Abundance)


# Plots all areas sampled by temp
ggplot(rv_depth, aes(x = D_range)) + geom_histogram(stat = "count") +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) + theme(axis.line = element_line(colour = "black"),
                                                                      panel.grid.major = element_blank(),
                                                                      panel.grid.minor = element_blank(),
                                                                      panel.background = element_blank()) +
  ggtitle("Depths Surveyed") #outer = 11-150 , middle = 20-91 , inner = 31-61
 
## Binned temperature plot (does'nt tell how where the survey mostly sampled)

ggplot(obs_depth_hal, aes(x = D_range)) + geom_histogram(stat = "count") +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) + theme(axis.line = element_line(colour = "black"),
                                                                      panel.grid.major = element_blank(),
                                                                      panel.grid.minor = element_blank(),
                                                                      panel.background = element_blank()) +
  ggtitle("Abundance of Halibut by Depth") # outer = 20-170 , middle = 30-120, inner 30-80, narrow inner = 40-60
#create binned barplot for all areas sampled present or not

## ECDF plots ----
 
# Survey area
surv_ecdf = rv_round %>% filter(!is.na(Temp))
#Abundance
abun_ecdf = rv_round %>% filter(Presence == "P", !is.na(Temp)) %>% uncount(Abundance)
abun_depth <- rv_round %>% filter(Presence == "P", !is.na(Depth)) %>% uncount(Abundance)

## Abundance
p1 = ggplot(filter(abun_ecdf, Season == "SUMMER"), 
       aes(x=Temp,y= 0, fill = stat(x))) + 
  geom_density_ridges_gradient()+
  scale_x_continuous(breaks = seq(-1, 17, by = 1), limits = c(-1,15), expand = c(0, 0))+
  scale_y_discrete(expand = c(0,0))+
  geom_vline(xintercept=quantile(abun_ecdf$Temp, c(.05,.95)),lty=2,col="black")+ #90% of data between both lines
  scale_fill_viridis_c(name=expression(paste("Temperature ",degree,"C",sep="")),option="C", labels = c("0", "5", "10", "15")) +
  theme(axis.line = element_line(colour = "black"), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), panel.background = element_blank(),
        legend.position = c(.9, 0.45), legend.box.background = element_rect(colour = "black"), legend.background = element_blank())+
  labs(y = "Density of Juvenile Habitat");p1

ggsave("output/density_temp.png",p1,dpi=600,width=8,height=6,units="in")


## Depth 

p3 = ggplot(filter(abun_depth, Season == "SUMMER", !is.na(Depth)), 
            aes(x=Depth, y=0, fill = stat(x)))+ 
  geom_density_ridges_gradient(scale = 1, rel_min_height = 0.0001)+
  scale_x_reverse(breaks = seq(0, 550, by = 50))+
  scale_y_discrete(expand = c(0,0))+  
  geom_vline(xintercept=quantile(abun_depth$Depth, c(.05,.95)),lty=2,col="black")+
  coord_flip()+
  scale_fill_viridis_c(name=expression(paste("Depth ","(M)",sep="")), option="E", 
                       labels = c("500", "400", "300", "200", "100","0"))+
  theme(axis.line = element_line(colour = "black"), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), panel.background = element_blank(), 
        legend.position = c(.9, 0.45), legend.box.background = element_rect(colour = "black"), 
        legend.background = element_blank())+ 
  labs(y = "Density of Juvenile Habitat");p3

ggsave("output/density_depth.png",p3,dpi=600,width=8,height=6,units="in")



## ECDF Temp  
ggplot() + stat_ecdf(data = abun_ecdf, mapping = aes(Temp, colour = Presence)) + 
  stat_ecdf(data = surv_ecdf, mapping = aes(Temp), geom = "step", pad = FALSE) + 
  scale_x_continuous(breaks = seq(-2, 17, by = 1), limits = c(-2,15)) + theme(axis.line = element_line(colour = "black"),
                                                           panel.grid.major = element_blank(),
                                                           panel.grid.minor = element_blank(),
                                                           panel.background = element_blank()) +
  ggtitle("Temperature Surveyed vs Presence of Halibut")

## ECDF Depth 

ggplot() + stat_ecdf(data = abun_ecdf, mapping = aes(Depth, colour = Presence), geom = "step", pad = FALSE) + 
  stat_ecdf(data = surv_ecdf, mapping = aes(Depth), geom = "step", pad = FALSE) + 
  scale_x_continuous(breaks = seq(0, 1000, by = 50), limits = c(0,400)) + theme(axis.line = element_line(colour = "black"),
                                                             panel.grid.major = element_blank(),
                                                             panel.grid.minor = element_blank(),
                                                             panel.background = element_blank()) +
  ggtitle("Depth Surveyed vs Presence of Halibut")

## ECDF Salinity 
ggplot() + stat_ecdf(data = abun_ecdf, mapping = aes(Salinity, colour = Presence), geom = "step", pad = FALSE) + 
  stat_ecdf(data = surv_ecdf, mapping = aes(Salinity), geom = "step", pad = FALSE) + 
  scale_x_continuous(breaks = seq(25, 37, by = 1), limits = c(29,37)) + theme(axis.line = element_line(colour = "black"),
                                                             panel.grid.major = element_blank(),
                                                             panel.grid.minor = element_blank(),
                                                             panel.background = element_blank()) +
  ggtitle("Salinity Surveyed vs Presence of Halibut")

## ECDF Plots by Season ----

## ECDF Temp  
ggplot() + stat_ecdf(data = abun_ecdf, mapping = aes(Temp, colour = Presence), geom = "step", pad = FALSE) + 
  stat_ecdf(data = sT_ecdf, mapping = aes(Temp), geom = "step", pad = FALSE) + facet_wrap(~Season) +
  scale_x_continuous(breaks = seq(-2, 17, by = 1), limits = c(-2,15)) + theme(axis.line = element_line(colour = "black"),
                                                                              panel.grid.major = element_blank(),
                                                                              panel.grid.minor = element_blank(),
                                                                              panel.background = element_blank()) +
  ggtitle("Temperature Surveyed vs Presence of Halibut")

## ECDF Depth 

ggplot() + stat_ecdf(data = abun_ecdf, mapping = aes(Depth, colour = Presence), geom = "step", pad = FALSE) + 
  stat_ecdf(data = surv_ecdf, mapping = aes(Depth), geom = "step", pad = FALSE) + facet_wrap(~Season) +
  scale_x_continuous(breaks = seq(0, 1000, by = 50), limits = c(0,400)) + theme(axis.line = element_line(colour = "black"),
                                                                                panel.grid.major = element_blank(),
                                                                                panel.grid.minor = element_blank(),
                                                                                panel.background = element_blank()) +
  ggtitle("Depth Surveyed vs Presence of Halibut")

## ECDF Salinity 
ggplot() + stat_ecdf(data = abun_ecdf, mapping = aes(Salinity, colour = Presence), geom = "step", pad = FALSE) + 
  stat_ecdf(data = surv_ecdf, mapping = aes(Salinity), geom = "step", pad = FALSE) + facet_wrap(~Season) +
  scale_x_continuous(breaks = seq(25, 37, by = 1), limits = c(29,37)) + theme(axis.line = element_line(colour = "black"),
                                                                              panel.grid.major = element_blank(),
                                                                              panel.grid.minor = element_blank(),
                                                                              panel.background = element_blank()) +
  ggtitle("Salinity Surveyed vs Presence of Halibut")


## Abundance Plots ----

bio_abun = rv %>% group_by(Year, Season) %>% summarise(Abundance = sum(Abundance), Kg = sum(Kg))


ggplot(bio_abun, aes(x = Year, y = Abundance, colour = Season)) + geom_point() +
  geom_smooth() + theme(axis.line = element_line(colour = "black"),
                        panel.grid.major = element_blank(),
                        panel.grid.minor = element_blank(),
                        panel.background = element_blank())

ggplot(bio_abun, aes(x = Year, y = Abundance)) + geom_point() +
  geom_smooth() + theme(axis.line = element_line(colour = "black"),
                        panel.grid.major = element_blank(),
                        panel.grid.minor = element_blank(),
                        panel.background = element_blank())


## Biomass Plots ----

ggplot(bio_abun, aes(x = Year, y = Kg, colour = Season)) + geom_point() +
  geom_smooth() + theme(axis.line = element_line(colour = "black"),
                        panel.grid.major = element_blank(),
                        panel.grid.minor = element_blank(),
                        panel.background = element_blank())

ggplot(bio_abun, aes(x = Year, y = Kg)) + geom_point() +
  geom_smooth() + theme(axis.line = element_line(colour = "black"),
                        panel.grid.major = element_blank(),
                        panel.grid.minor = element_blank(),
                        panel.background = element_blank())


## Biomass vs GDD ----

#will probably have to create a data frame in GDD file for total GDD in
total_gdd = GDD %>% filter(grepl("4T|4S|4R", GDD$ZONE))%>% 
  group_by(Year)%>% 
  summarise(Tgdd = sum(sGDD))

bio_gdd = merge(bio_abun %>% filter(Season == "SUMMER") %>% select(Year, Kg),total_gdd, by = "Year")


#Try this again with the summer GDD of SS
#Make sure the total GDD in groups (SS, GSL or NF) are divided (or scaled) by the number of points in all the NAFO zones associated with them
ggplot(bio_gdd, aes(x = Tgdd, y = Kg)) + geom_point() +
  geom_smooth(method = "lm") + theme(axis.line = element_line(colour = "black"),
                                     panel.grid.major = element_blank(),
                                     panel.grid.minor = element_blank(),
                                     panel.background = element_blank())
