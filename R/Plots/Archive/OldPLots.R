library(tidyverse)
library(sf) 
library(data.table)
library(viridis)
library(ggplot2)

load("data/perc_hab_stocks.RData")

p1 <- ggplot(perc_hab, 
             aes(x = Year, y = Proportion,  colour = ZONE))+ 
  geom_line()+ 
  geom_smooth(method = "lm", aes(colour = ZONE), se = FALSE)+
  facet_wrap(~Stock)+
  scale_colour_viridis(discrete = TRUE, option = "D")+ 
  theme_bw()+
  theme(axis.line = element_line(colour = "black"), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), panel.background = element_blank(),
        axis.title.x = element_text(margin = margin(t = 10)), 
        axis.title.y = element_text(margin = margin(r = 10)),
        legend.box.background = element_rect(colour = "black"), legend.background = element_blank(),
        strip.background =element_rect(fill="#f0f0f0"),
        text = element_text(size=17), axis.text.x = element_text(color = "grey20", size = 14, vjust = .5),
        axis.text.y = element_text(color = "grey20", size = 14, vjust = .5))+
  guides(color = guide_legend(reverse = TRUE))+
  labs(colour = "NAFO\nDivision", y = "Preferred Habitat (%)", x = "Year");p1

ggsave("output/pref-hab_percent_stocks.png",p1,dpi=300,width=8,height=6,units="in")

## GDD Plots ----

load("data/GDD_stocks.RData")
#Plot of the average GDD for each NAFO zone by season
#Grouping by region(stock* need to change), keeping in case we want to group by stock, region AND NAFO zones
# GDD$Stock = ifelse(grepl("4T|4S|4R", GDD$ZONE), "GSL",
#                    ifelse(grepl("4X|4W|4Vs|4Vn", GDD$ZONE), "SS", 
#                           ifelse(grepl("3Pn|3Ps|3O|3N|3L|3M|3K", GDD$ZONE), "NF", NA)))

#GDD$Stock <- factor(GDD$Stock, levels=c("SS", "GSL", "NF"))

#Plotting GDD by region (may be different by region? depending on how we define it*)
p2 <- ggplot(GDD, aes(x = Year, y = sGDD,  colour = ZONE))+ 
  geom_line()+ 
  geom_smooth(method = "lm", aes(colour = ZONE), se = FALSE)+
  facet_wrap(~Stock)+
  scale_colour_viridis(discrete = TRUE, option = "D")+ 
  theme_bw()+
  theme(axis.line = element_line(colour = "black"), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), panel.background = element_blank(),
        legend.box.background = element_rect(colour = "black"), legend.background = element_blank(),
        strip.background =element_rect("#f0f0f0"),
        text = element_text(size=17), axis.text.x = element_text(color = "grey20", size = 14, vjust = .5),
        axis.text.y = element_text(color = "grey20", size = 14, vjust = .5))+#a6bddb  #f0f0f0
  guides(color = guide_legend(reverse = TRUE))+
  labs(colour = "NAFO\nDivision", y = "GDD\n", x = "\nYear");p2

ggsave("output/GDD.png",p2,dpi=300,width=8,height=6,units="in")

