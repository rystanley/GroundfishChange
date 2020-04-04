
## Load Packages ----

library(tidyverse) #dplyr and tidyr
library(ggplot2) #plotting
library(data.table) #faster processing
library(ggridges)
library(patchwork)

rv <- fread("data/Halibut_2018_RV.csv")

#Select columns that will be used
rv <-  rv %>% select(LATITUDE, LONGITUDE, YEAR, SEASON, NAME, DMAX, BOTTOM_TEMPERATURE, BOTTOM_SALINITY, 
                   TOTALNUMBERSTANDARDIZED, TOTALWEIGHTSTANDARDIZED_KG, PRESENCE) %>%
  rename(Zone = NAME, Depth = DMAX, Temp = BOTTOM_TEMPERATURE, Year = YEAR, Season = SEASON, Salinity = BOTTOM_SALINITY, 
         Abundance = TOTALNUMBERSTANDARDIZED, Kg = TOTALWEIGHTSTANDARDIZED_KG, Presence = PRESENCE) %>%
  replace_na(list(Abundance = 0, Kg = 0))

#Rounding up for abundance in order to plot
rv_round <-  rv
rv_round$Abundance = ceiling(rv_round$Abundance)


## ECDF plots ----
 
# Survey area
surv_temp = rv_round %>% filter(!is.na(Temp))%>% select(-Abundance, -Presence) %>% mutate(Group = "Survey")
surv_depth = rv_round %>% filter(!is.na(Depth))%>% select(-Abundance, -Presence) %>% mutate(Group = "Survey")
#Abundance
abun_temp = rv_round %>% filter(Presence == "P", !is.na(Temp)) %>% 
  uncount(Abundance) %>% 
  select(-Presence) %>% 
  mutate(Group = "Halibut")
 
abun_depth <- rv_round %>% filter(Presence == "P", !is.na(Depth)) %>% 
  uncount(Abundance) %>%  
  select(-Presence) %>% 
  mutate(Group = "Halibut")
## ECDF Temp 
temp_ecdf <-  rbind(surv_temp, abun_temp) %>% filter(Season == "SUMMER")

e1 <-  ggplot(temp_ecdf, aes(Temp, colour = Group, linetype= Group)) + stat_ecdf(geom = "step", lwd=1.7)  + 
  scale_x_continuous(breaks = seq(-2, 17, by = 1), limits = c(-1,14))+
  geom_vline(xintercept=quantile(temp_ecdf %>% filter(Group == "Halibut") %>%  .$Temp, c(.05,.95)),lty=1,col="black")+ 
  geom_vline(xintercept=quantile(temp_ecdf %>% filter(Group == "Survey") %>%  .$Temp, c(.05,.95)),lty=2,col="black")+
  theme(axis.line = element_line(colour = "black"), panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), panel.background = element_blank(),
        legend.position = c(.9, 0.5), axis.title.x = element_text(margin = margin(t = 10)),
        text = element_text(size=17), axis.text.x = element_text(color = "grey20", size = 14, vjust = .5),
        axis.text.y = element_text(color = "grey20", size = 14, vjust = .5))+
  labs(x= expression(paste("Temperature ",degree,"C",sep="")), y="Proportion\n",col="", linetype = "");e1

ggsave("output2/temp_ecdf.png",e1,dpi=300,width=8,height=6,units="in")


## ECDF Depth
depth_ecdf <-  rbind(surv_depth, abun_depth) %>% filter(Season == "SUMMER")

e2 <-  ggplot(depth_ecdf, aes(Depth, colour = Group, linetype= Group)) + stat_ecdf(geom = "step", lwd=1.7)  + 
    scale_x_continuous(breaks = seq(0, 1000, by = 50), limits = c(0,300), expand = c(0,10))+
    geom_vline(xintercept=quantile(depth_ecdf %>% filter(Group == "Halibut") %>%  .$Depth, c(.05,.95)),lty=1,col="black")+ 
    geom_vline(xintercept=quantile(depth_ecdf %>% filter(Group == "Survey") %>%  .$Depth, c(.05,.95)),lty=2,col="black")+
    theme(axis.line = element_line(colour = "black"), panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), panel.background = element_blank(),
          legend.position = c(.9, 0.5), axis.title.x = element_text(margin = margin(t = 10)),
          text = element_text(size=17), axis.text.x = element_text(color = "grey20", size = 14, vjust = .5),
          axis.text.y = element_text(color = "grey20", size = 14, vjust = .5))+
    labs(x="Depth (m)",y="Proportion\n",col="", linetype = "");e2

ggsave("output2/depth_ecdf.png",e2,dpi=300,width=8,height=6,units="in")






##Merged ecdfs faceted
p1 = e1+labs(tag="A")
p2 = e2+labs(tag="B")

e3 = p1 + p2 

ggsave("output2/ecdf_merged.png",e3,dpi=300,width=12,height=6,units="in")
## Abundance #code above changed so this is probably wont work properly
p1 = ggplot() + 
  geom_density_ridges_gradient(filter(abun_temp, Season == "SUMMER"), 
       mapping = aes(x=Temp,y= 0.3, fill = stat(x)))+
  geom_density_ridges_gradient(filter(surv_temp, Season == "SUMMER"), 
                               mapping = aes(x=Temp,y= 0.2, fill = stat(x)))+
  scale_x_continuous(breaks = seq(-2, 17, by = 1), limits = c(-2,15), expand = c(0, 1))+
  scale_y_discrete(expand = c(0,0))+
  geom_vline(xintercept=quantile(abun_temp$Temp, c(.05,.95)),lty=1,col="black")+ 
  geom_vline(xintercept=quantile(surv_temp$Temp, c(.05,.95)),lty=2,col="black")+#90% of data between both lines
  scale_fill_viridis_c(name=expression(paste("Temperature ",degree,"C",sep="")),option="C", alpha = .8) +
  theme(axis.line = element_line(colour = "black"), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), panel.background = element_blank(),
        legend.position = c(.9, 0.45), legend.box.background = element_rect(colour = "black"), legend.background = element_blank(), axis.text.y = label = c("one", "two"))+
  labs(y = "Density of Juvenile Habitat");p1

ggsave("output2/density_temp.pdf",p1,dpi=300,width=8,height=6,units="in")


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

ggsave("output2/density_depth.pdf",p3,dpi=300,width=8,height=6,units="in")


 




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
