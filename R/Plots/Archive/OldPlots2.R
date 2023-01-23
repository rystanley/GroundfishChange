library(tidyverse)
library(sf) 
library(data.table)
library(viridis)
library(ggplot2)
library(RColorBrewer)
library(ggrepel)

library(sp) #not sure if i use this package will have to check

#Load Data
load("data/BNAM_T.RData")#the Rdata file here needs to be fixed since it doesn't contain zone anymore

## Plotting Average Bottom Temperature ----

avg_btmp = temp %>% 
           select("Year", "ZONE", "Depth", "Annual_AVG") %>% 
           group_by(Year, ZONE) %>% 
           summarise_all(list(mean))

###Total Bottom Temperature trend by NAFO zone for each season
#Melting data into long format for implementing season as group
Avg_btm_temp_2 = melt.data.table(as.data.table(avg_btmp), id.vars = c("Year","ZONE", "Depth")) %>% 
                 rename(Season = variable, Temperature = value) 

#Adding Region Column (named stock, will have to change*)
Avg_btm_temp_2$Stock =  ifelse(grepl("4T|4S|4R", Avg_btm_temp_2$ZONE), "GSL",
                         ifelse(grepl("4X|4W|4Vs|4Vn", Avg_btm_temp_2$ZONE), "SS", 
                                ifelse(grepl("3Pn|3Ps|3O|3N|3L|3M|3K", Avg_btm_temp_2$ZONE), "NF", NA)))

Avg_btm_temp_2$Stock <- factor(Avg_btm_temp_2$Stock, levels=c("SS", "GSL", "NF"))

#Plotting Annual Average Temperature 
p1 <- ggplot(filter(Avg_btm_temp_2, Season == "Annual_AVG", !is.na(Stock)), 
             aes(x = Year, y = Temperature,  colour = ZONE))+ 
  geom_line()+ 
  geom_smooth(method = "lm", aes(colour = ZONE), se = FALSE)+
  facet_wrap(~Stock)+
  scale_colour_viridis(discrete = TRUE, option = "C")+ 
  theme_bw()+
  theme(axis.line = element_line(colour = "black"), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), panel.background = element_blank(),
        legend.box.background = element_rect(colour = "black"), legend.background = element_blank(),
        strip.background =element_rect(fill="#f0f0f0"),
        text = element_text(size=17), axis.text.x = element_text(color = "grey20", size = 14, vjust = .5),
        axis.text.y = element_text(color = "grey20", size = 14, vjust = .5))+
  guides(color = guide_legend(reverse = TRUE))+
  labs(colour = "NAFO\nDivision", y = "Annual Avereage Temeprature (C)\n", x = "Year");p1

ggsave("output2/annual_temp.png",p1,dpi=300,width=8,height=6,units="in")




## Habitat Plots ----
load("data/BNAM_hab_2021.RData")

#ggplot(filter(prop_hab, Habitat == "Preferred"), aes(x = Year, y = Proportion)) + 
#  geom_line() + facet_wrap(~ZONE, scales = "free") +
#  geom_smooth(method = "lm") + theme_bw() + theme(axis.line = element_line(colour = "black"),
                                               #   panel.grid.major = element_blank(),
                                               #   panel.grid.minor = element_blank(),
                                               #   panel.background = element_blank()) +
 # ggtitle("Proportion of Preferred Habitat")


colourCount = length(unique(prop_hab$ZONE))
getPalette = colorRampPalette(brewer.pal(8, "Set1"))(colourCount)

## Plot for poster pref hab
prop_hab$Stock =  ifelse(grepl("4S|4R|4T", prop_hab$ZONE), "GSL",
                         ifelse(grepl("4X|4W|4Vs|4Vn|3Ps|3O|3N", prop_hab$ZONE), "SS-GBs", 
                                ifelse(grepl("5Y|5Ze|5Zw", prop_hab$ZONE), "GM-GB", NA)))
#Change to percentage
prop_hab$Proportion = prop_hab$Proportion*100

prop_hab$Stock <- factor(prop_hab$Stock, levels=c("GM-GB","SS-GBs", "GSL"))

#Plotting Preffered habitat over time (will be different due to new windows*)
p2 <- ggplot(filter(prop_hab, Habitat == "Within_hab"), 
       aes(x = Year, y = Proportion))+ #colour = getPalette(colourCount)
  geom_smooth(aes(colour = ZONE), se = FALSE)+
  scale_colour_manual(values = getPalette)+
  facet_wrap(~Stock, ncol = 4)+
  theme_bw()+
  theme(axis.line = element_line(colour = "black"), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), panel.background = element_blank(),
        axis.title.x = element_text(margin = margin(t = 10)), 
        axis.title.y = element_text(margin = margin(r = 10)),
        legend.box.background = element_rect(colour = "black"), legend.background = element_blank(),
        strip.background =element_rect(fill="#f0f0f0"),
        text = element_text(size=11),
        axis.text.y = element_text(color = "grey20", size = 11, vjust = .5),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, color = "grey20", size = 11))+
  guides(color = guide_legend(reverse = TRUE))+
  labs(colour = "NAFO\nDivision", y = "Suitable Habitat (%)", x = "Year");p2

ggsave(file = "Figures/SuppFig1.tiff", dpi = 600, width = 7.25, height = 5.4375, units = "in")
ggsave(file = "Figures/SuppFig1.jpeg", dpi = 300, width = 7.25, height = 5.4375, units = "in")

                                 
## GDD Plots ----

load("GDD_2021.RData")
#Plot of the average GDD for each NAFO zone by season
#Grouping by region(stock* need to change)
GDD$Stock =  ifelse(grepl("4S|4R|4T", GDD$ZONE), "GSL",
                     ifelse(grepl("4X|4W|4Vs|4Vn|3Ps|3O|3N", GDD$ZONE), "SS-GBs", 
                            ifelse(grepl("5Y|5Ze|5Zw", GDD$ZONE), "GM-GB", NA)))

GDD$Stock <- factor(GDD$Stock, levels=c("GM-GB","SS-GBs", "GSL"))


#Plotting GDD by region (may be different by region? depending on how we define it*)
p3 <- ggplot(GDD, aes(x = Year, y = sGDD,  colour = ZONE))+ 
  geom_smooth(aes(colour = ZONE), se = FALSE)+
  facet_wrap(~Stock, ncol = 4)+
  scale_colour_manual(values = getPalette)+
  theme_bw()+
  theme(axis.line = element_line(colour = "black"), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), panel.background = element_blank(),
        legend.box.background = element_rect(colour = "black"), legend.background = element_blank(),
        strip.background =element_rect("#f0f0f0"),
        text = element_text(size=11),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, color = "grey20", size = 11),
        axis.text.y = element_text(color = "grey20", size = 11, vjust = .5))+#a6bddb  #f0f0f0
  guides(color = guide_legend(reverse = TRUE))+
  labs(colour = "NAFO\nDivision", y = "GDD\n", x = "\nYear");p3

ggsave(file = "Figures/SuppFig2.tiff", dpi = 600, width = 7.25, height = 5.4375, units = "in")
ggsave(file = "Figures/SuppFig2.jpeg", dpi = 300, width = 7.25, height = 5.4375, units = "in")



