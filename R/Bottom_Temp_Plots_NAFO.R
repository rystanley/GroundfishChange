library(data.table)
library(sp)
library(sf) 
library(ggplot2)
library(here)
library(tidyverse)
library(rnaturalearth)
library(Hmisc)

#Load Data
load("data/BNAM_T.RData")#the Rdata file here needs to be fixed since it doesn't contain zone anymore

## Plotting Average Bottom Temperature ----

avg_btmp = temp %>% select("Year", "ZONE", "Winter_AVG", "Summer_AVG", "Annual_AVG") %>% group_by(Year, ZONE) %>% 
  summarise_all(list(mean))

###Total Bottom Temperature trend by NAFO zone for each season
#Melting data into long format for implementing season as group
Avg_btm_temp_2 = melt.data.table(as.data.table(avg_btmp), id.vars = c("Year","ZONE")) %>% 
  rename(Season = variable, Temperature = value) 

### THIS IS A FINAL PLOT
  p1 <- ggplot(Avg_btm_temp_2 %>% filter(Season == "Summer_AVG"), 
         aes(x = Year, y = Temperature,  colour = ZONE))+ 
    geom_line()+ 
    geom_smooth(method = "lm", aes(colour = ZONE))+
    theme_bw()+
    theme(axis.line = element_line(colour = "black"), panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), panel.background = element_blank())+
    ggtitle("Summer Average Temperature");p1

ggsave("output/Summer_average_temp.png",p1,dpi=600,width=8,height=6,units="in")






ggplot(Avg_btm_temp_2, aes(x = Year, y = Temperature,  colour = Season )) + 
  geom_line() + facet_wrap(~ZONE, scales = "free") + 
  geom_smooth(method = "lm", aes(colour = Season)) + theme_bw() + theme(axis.line = element_line(colour = "black"),
                                                                        panel.grid.major = element_blank(),
                                                                        panel.grid.minor = element_blank(),
                                                                        panel.background = element_blank()) +
  ggtitle("All Seasons Average Temperature")

ggplot(Avg_btm_temp_2 %>% filter(Season == 'Annual_AVG'), aes(x = Year, y = Temperature)) + 
  geom_line() + facet_wrap(~ZONE, scales = "free") + 
  geom_smooth(method = "lm") + theme_bw() + theme(axis.line = element_line(colour = "black"),
                                                  panel.grid.major = element_blank(),
                                                  panel.grid.minor = element_blank(),
                                                  panel.background = element_blank()) +
  ggtitle("Annual Avereage Temperature")

ggplot(Avg_btm_temp_2 %>% filter(Season == 'Winter_AVG'), aes(x = Year, y = Temperature)) + 
  geom_line() + facet_wrap(~ZONE, scales = "free") + 
  geom_smooth(method = "lm") + theme_bw() + theme(axis.line = element_line(colour = "black"),
                                                  panel.grid.major = element_blank(),
                                                  panel.grid.minor = element_blank(),
                                                  panel.background = element_blank())+
  ggtitle("Winter Avereage Temperature")


s1 <- ggplot(Avg_btm_temp_2 %>% filter(Season == 'Summer_AVG'), aes(x = Year, y = Temperature)) + 
  geom_line() + facet_wrap(~ZONE, scales = "free") + 
  geom_smooth(method = "lm") + theme_bw() + theme(axis.line = element_line(colour = "black"),
                                                  panel.grid.major = element_blank(),
                                                  panel.grid.minor = element_blank(),
                                                  panel.background = element_blank()) +
  ggtitle("Summer Avereage Temperature")

ggsave("output/Summer_average_temp-zone.png",s1,dpi=600,width=8,height=6,units="in")


## Habitat Plots ----
load("data/BNAM_hab.RData")

ggplot(filter(prop_hab, Habitat == "Preffered"), aes(x = Year, y = Proportion)) + 
  geom_line() + facet_wrap(~ZONE, scales = "free") +
  geom_smooth(method = "lm") + theme_bw() + theme(axis.line = element_line(colour = "black"),
                                                  panel.grid.major = element_blank(),
                                                  panel.grid.minor = element_blank(),
                                                  panel.background = element_blank()) +
  ggtitle("Proportion of Preffered Habitat")


ggplot(filter(prop_hab, Habitat == "Good"), aes(x = Year, y = Proportion)) + 
  geom_line() + facet_wrap(~ZONE, scales = "free") +
  geom_smooth(method = "lm") + theme_bw() + theme(axis.line = element_line(colour = "black"),
                                                  panel.grid.major = element_blank(),
                                                  panel.grid.minor = element_blank(),
                                                  panel.background = element_blank()) +
  ggtitle("Proportion of Good Habitat")




## GDD Plots ----

load("data/GDD.RData")
#Plot of the average GDD for each NAFO zone by season

# GDD Estimate 1 (Scaled mean GDD of temps >3   x   n(temps > 3)   /   total obs in zone)
ggplot(GDD, aes(x = Year, y = sGDD)) + 
  geom_line() + facet_wrap(~ZONE, scales = "free") + 
  geom_smooth(method = "lm") + theme_bw() + theme(axis.line = element_line(colour = "black"),
                                                  panel.grid.major = element_blank(),
                                                  panel.grid.minor = element_blank(),
                                                  panel.background = element_blank()) +
  ggtitle("Scaled Annual Average GDD per Grid Cell")


# GDD Estimate 2 Plot (Average GDD of all depth between 25-200)
ggplot(GDD, aes(x = Year, y = mGDD)) + 
  geom_line() + facet_wrap(~ZONE, scales = "free") + 
  geom_smooth(method = "lm") + theme_bw() + theme(axis.line = element_line(colour = "black"),
                                                  panel.grid.major = element_blank(),
                                                  panel.grid.minor = element_blank(),
                                                  panel.background = element_blank()) +
  ggtitle("Annual Average GDD")


# GDD Estimate 3 Plot (average number of days > 3, within the depth range)  
ggplot(GDD, aes(x = Year, y = Avg_days)) + 
  geom_line() + facet_wrap(~ZONE, scales = "free") + 
  geom_smooth(method = "lm") + theme_bw() + theme(axis.line = element_line(colour = "black"),
                                                  panel.grid.major = element_blank(),
                                                  panel.grid.minor = element_blank(),
                                                  panel.background = element_blank()) +
  ggtitle("Avgerage Number of Days > 3")




##########Stocks###########
#i do not know why this code below doesn't work here, yet it worked for dataframe percent_hab below
avg_btmp = Avg_btm_temp_2 %>% mutate(Stock = ifelse(grepl("4T|4S|4R", Avg_btm_temp_2$ZONE), "GSL",
                                                      ifelse(grepl("4X|4W|4Vs|4Vn", Avg_btm_temp_2$ZONE), "SS", 
                                                             ifelse(grepl("3Pn|3Ps|3O|3N", Avg_btm_temp_2$ZONE), "NF", NA))))

#dividing data into two stocks using NAFo zones
stock_hab_prop = percent_hab %>% mutate(Stock = ifelse(grepl("4T|4S|4R", percent_hab$ZONE), "GSL", 
                                                       ifelse(grepl("4X|4W|4Vs|3Pn|3Ps|3O|3N", percent_hab$ZONE), "SS_NF", "NA")))

stock_hab = stock_hab_prop %>% filter(Stock != "NA") %>% group_by(Year,Stock, Habitat_suitability) %>% summarise_at(.vars = c("proportion","sd"), funs(mean))

#Merging dataframes because above code didnt work
stock_temp = merge(avg_btmp, stock_hab_prop[,c(1,3,6)]) %>% unique() %>% filter(Stock != "NA") %>% 
  group_by(Year, Stock) %>% summarise(Ann_AVG = mean(Annual_AVG))

#Plot average temperature by stocks
ggplot(stock_temp, aes(x = Year, y = Ann_AVG, colour = Stock)) + 
  geom_line() + geom_smooth(method = "lm")



p2 <- ggplot(avg_btmp %>% filter(Season == "Summer_AVG", !is.na(Stock)), 
             aes(x = Year, y = Temperature,  colour = ZONE))+ 
  geom_line()+ 
  geom_smooth(method = "lm", aes(colour = ZONE))+
  facet_wrap(~Stock)+
  theme_bw()+
  theme(axis.line = element_line(colour = "black"), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), panel.background = element_blank())+
  ggtitle("Summer Average Temperature");p2

ggsave("output/Summer_average_temp-groups.png",p2,dpi=600,width=8,height=6,units="in")




#################Plots with Trend Lines##############################

###Total Bottom Temperature trend by NAFO zone for each season
#Melting data into long format for implementing season as group

###Averaege Temperature by stock
ggplot(stock_temp, aes(x = Year, y = Ann_AVG, colour = Stock)) + 
  geom_line() + geom_smooth(method = "lm") + theme_bw() + theme(axis.line = element_line(colour = "black"),
                                                                panel.grid.major = element_blank(),
                                                                panel.grid.minor = element_blank(),
                                                                panel.background = element_blank()) +
  ggtitle("Annual Bottom Temperature")

###Proportion of habitat type by zone for preffered and good temperatures with and without preffered depth

ggplot(percent_pref, aes(x = Year, y = proportion)) + 
  geom_line() + facet_wrap(~ZONE, scales = "free") +
  geom_smooth(method = "lm") + theme_bw() + theme(axis.line = element_line(colour = "black"),
                                                  panel.grid.major = element_blank(),
                                                  panel.grid.minor = element_blank(),
                                                  panel.background = element_blank()) +
  ggtitle("Proportion of Preffered Habitat")


ggplot(percent_good, aes(x = Year, y = proportion)) + 
  geom_line() + facet_wrap(~ZONE, scales = "free") + 
  geom_smooth(method = "lm") + theme_bw() + theme(axis.line = element_line(colour = "black"),
                                                  panel.grid.major = element_blank(),
                                                  panel.grid.minor = element_blank(),
                                                  panel.background = element_blank()) +
  ggtitle("Proportion of Good Habitat")




###Proportion of habitat type by stock for preffered and good temperatures with and without preffered depth
filter(stock_hab, Habitat_suitability == "Preffered") %>%  
  ggplot(aes(x = Year, y = proportion, colour = Stock)) + 
  geom_line() +
  geom_smooth(method = "lm") + theme(axis.line = element_line(colour = "black"),
                                     panel.grid.major = element_blank(),
                                     panel.grid.minor = element_blank(),
                                     panel.background = element_blank()) +
  ggtitle("Proportion of Preffered Habitat")



filter(stock_hab, Habitat_suitability == "Good") %>%  
  ggplot(aes(x = Year, y = proportion, colour = Stock)) + 
  geom_line() +
  geom_smooth(method = "lm") + theme(axis.line = element_line(colour = "black"),
                                     panel.grid.major = element_blank(),
                                     panel.grid.minor = element_blank(),
                                     panel.background = element_blank()) +
  ggtitle("Proportion of Good Habitat")



#Plots by Stock

ggplot(filter(gdd_stock, Season == "Annual"), aes(x = Year, y = GDD,  colour = Stock)) + 
  geom_line() + geom_smooth(method = "lm", aes(colour = Stock)) + theme_bw() + theme(axis.line = element_line(colour = "black"),
                                                                        panel.grid.major = element_blank(),
                                                                        panel.grid.minor = element_blank(),
                                                                        panel.background = element_blank()) +
  ggtitle("Annual Average GDD")


ggplot(filter(gdd_stock, Season != "Annual"), aes(x = Year, y = GDD,  colour = Season)) + facet_wrap(~Stock) +
  geom_line() + geom_smooth(method = "lm", aes(colour = Season)) + theme_bw() + theme(axis.line = element_line(colour = "black"),
                                                                                     panel.grid.major = element_blank(),
                                                                                     panel.grid.minor = element_blank(),
                                                                                     panel.background = element_blank()) +
  ggtitle("Stock Average GDD")

