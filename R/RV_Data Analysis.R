
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

#Confidence Interval for Mean temperature of abundance
#6.486 + c(-1,1)*1.959964*(4.784259/sqrt(6136))

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


## ECDF Temp  
ggplot() + stat_ecdf(data = abun_ecdf, mapping = aes(Temp, colour = Presence), geom = "step", pad = FALSE) + 
  stat_ecdf(data = sT_ecdf, mapping = aes(Temp), geom = "step", pad = FALSE) + 
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



