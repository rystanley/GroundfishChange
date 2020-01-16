library(data.table)
library(sp)
library(sf) 
library(ggplot2)
library(here)
library(tidyverse)
library(rnaturalearth)
library(Hmisc)

######Read in csv files
setwd("~/School 2019-2020/Halibut/BIO/Bottom_temp_raw_files")

#reads all csv files within directory as one single data frame
#Removing all rows that are land (land_mask = 0)

btmp = list.files(pattern="*.csv") %>% 
  map_df(~fread(.))%>%
  data.frame


#Add new column for year using rep() to differentiate between each of the 29 years
#Removing all rows that are land (land_mask = 0)
#Select columns we want to keep for analysis
#Roughly crop to desired NAFO zones long and lat
btmp = btmp %>% 
  mutate(Year = rep(1990:2018,each= dim(btmp)[1]/29)) %>% 
  filter(If_land != 0) %>% 
  select(Longitude, Latitude, Depth, Winter_AVG, Summer_AVG, Annual_AVG, Year) %>% 
  filter(Longitude >= -73, Longitude <= -40 ,Latitude >= 38, Latitude <= 53)


#Adding a column for habitat suitability

btmp$Habitat = ifelse(btmp$Annual_AVG >= 3 & btmp$Annual_AVG <= 8 & btmp$Depth >= 50 & btmp$Depth >= 25 & btmp$Depth <= 200, 
                      "Preffered", ifelse(btmp$Annual_AVG >= 1.5 & btmp$Annual_AVG <= 10 & btmp$Depth >= 25 & btmp$Depth <= 200,
                                          "Good", ifelse(btmp$Annual_AVG >= 3 & btmp$Annual_AVG <= 8, "Pref_temp", 
                                                         ifelse(btmp$Annual_AVG >= 1.5 & btmp$Annual_AVG <= 10, "Good_temp", "Not_suitable"))))


#common projection
latlong <- "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"

#read in NAFO division shape files
setwd("~/School 2019-2020/Halibut/BIO/Divisions")

NAFO = st_read("Divisions.shp")%>%
  st_transform(latlong)%>%
  select(ZONE,geometry)%>%
  filter(!is.na(ZONE))




#convert to sf object with assigned projection
#assign each point a "ZONE" based on the NAFO zones (takes some time)
#convert ZONE from factor to character
btmp_sf = btmp %>% st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%
  st_join(.,NAFO,join=st_intersects)%>%
  mutate(ZONE=as.character(ZONE))

#create new dataframe without geometry for quick computational analysis
#filter for zones of interest (might remove 3K, 3L and 3M because they are not one fo the two stocks, ask nancy)
btmp_hab = btmp_sf %>% st_set_geometry(NULL)%>%
  filter(grepl("[34]", ZONE))


################
#ggplot avaerage bottom temperature
Avg_btm_temp = btmp_hab %>% select("Year", "ZONE", "Winter_AVG", "Summer_AVG", "Annual_AVG") %>% group_by(Year, ZONE, .drop = FALSE) %>% summarise_all(funs(mean))

ggplot(Avg_btm_temp, aes(x = Year, y = Winter_AVG)) + 
  geom_line() + facet_wrap(~ZONE) + geom_smooth(method = "lm")
geom_errorbar(aes(ymin= proportion-sd,ymax= proportion+sd))

ggsave("Winter_Bottom_Temp_byzone.png")

ggplot(Avg_btm_temp, aes(x = Year, y = Summer_AVG)) + 
  geom_line() + facet_wrap(~ZONE) + 
  geom_errorbar(aes(ymin= proportion-sd,ymax= proportion+sd))

ggsave("Summer_Bottom_Temp_byzone.png")

ggplot(Avg_btm_temp, aes(x = Year, y = Annual_AVG)) + 
  geom_line() + facet_wrap(~ZONE) + 
  geom_errorbar(aes(ymin= proportion-sd,ymax= proportion+sd))

ggsave("Annual_Bottom_Temp_byzone.png")
###############


#convert habitat to factor in order for group_by(.drop = FALSE) to work (leaves in groups containing 0)
#group by year and zone and count habitats for each year and zone
btmp_hab$Habitat = factor(btmp_hab$Habitat)
habitat_group = btmp_hab %>% group_by(Year, ZONE, .drop = FALSE) %>% count(Habitat)

#Convert back to string because otherwise loop returns NAs and idk why
habitat_group$Habitat = as.character(habitat_group$Habitat)



#create character vector containg all unique habitat names
#create dataframe containing all zones by year
hab = c(unique(habitat_group$Habitat))
total_hab = filter(habitat_group , Habitat == "Good")[,1:2]

#assign the count of each habitat by zone and year to a new column of total_hab and name them accordingly
for(i in 1:5){
  nam = hab[i]
  total_hab[,i+2] =filter(habitat_group, Habitat == hab[i])[,4]
}
names(total_hab)[-(1:2)] = hab



#create new dataframe with year, zone, habitat and actualy numbers to calculate percentages
#create new dataframe where some vars are not exclusive i.e good should include preffered, same with good_temp is for all temps not just specific depths

percent_hab = total_hab
percent_hab$Total = total_hab$Good + total_hab$Good_temp +total_hab$Pref_temp + total_hab$Preffered + total_hab$Not_suitable 
percent_hab$Good = total_hab$Good + total_hab$Preffered
percent_hab$Good_temp =  total_hab$Good + total_hab$Good_temp
percent_hab$Pref_temp = total_hab$Pref_temp + total_hab$Preffered


#convert numbers to percentages
percent_hab$Preffered = percent_hab$Preffered/percent_hab$Total
percent_hab$Good = percent_hab$Good/percent_hab$Total
percent_hab$Good_temp =  percent_hab$Good_temp/percent_hab$Total
percent_hab$Pref_temp = percent_hab$Pref_temp/percent_hab$Total

#clean up data frame
percent_hab = percent_hab %>% select(Year, ZONE, Preffered, Good, Pref_temp, Good_temp)

#melting data to long format
#must be a data.table to melt 
percent_hab = melt.data.table(as.data.table(percent_hab), id.vars = c("Year","ZONE")) %>% 
  rename(Habitat_suitability = variable, proportion = value) 

#grouping by year and zone to calculate standard deveiation
#merge sd and percent_hab dataframes
percent_hab = merge(percent_hab, percent_hab %>% group_by(Year, Habitat_suitability) %>% summarise(sd = sd(proportion)))

#creating dataframes for each habitat type in order to plot by zone easily (might be a better way)
percent_pref = percent_hab %>% filter(Habitat_suitability == "Preffered")
percent_good = percent_hab %>% filter(Habitat_suitability == "Good")
percent_pref_temp = percent_hab %>% filter(Habitat_suitability == "Pref_temp")
percent_good_temp = percent_hab %>% filter(Habitat_suitability == "Good_temp")


#change working directory for saved plots
setwd("~/School 2019-2020/Halibut/BIO/Plots")


ggplot(percent_pref, aes(x = Year, y = proportion)) + 
  geom_line() + facet_wrap(~ZONE) 
geom_errorbar(aes(ymin= proportion-sd,ymax= proportion+sd))

theme(panel.spacing = unit(2, "lines"))

ggsave("Percent_Preffered_Habitat_NAFO.png")

ggplot(percent_good, aes(x = Year, y = proportion)) + 
  geom_line() + facet_wrap(~ZONE) + 
  geom_errorbar(aes(ymin= proportion-sd,ymax= proportion+sd))

ggsave("Percent_Good_Habitat_NAFO.png")

ggplot(percent_pref_temp, aes(x = Year, y = proportion)) + 
  geom_line() + facet_wrap(~ZONE) + 
  geom_errorbar(aes(ymin= proportion-sd,ymax= proportion+sd))

ggsave("Percent_Pref_Temp_DepthNA_NAFO.png")

ggplot(percent_good_temp, aes(x = Year, y = proportion)) + 
  geom_line() + facet_wrap(~ZONE) + 
  geom_errorbar(aes(ymin= proportion-sd,ymax= proportion+sd))

ggsave("Percent_Good_Temp_DepthNA_NAFO.png")

#redo as one faceted table with all 4 habitats in each plot, need to melt data first


#get the average of habitat suitability for each year among all NAFO zones
#note: can use .vars = names(.)[3:6] to summarise specific columns
percent_hab_all = percent_hab %>% group_by(Year,Habitat_suitability) %>% summarise_at(.vars = c("proportion","sd"), funs(mean))


#plot of the overall average bottom temp
ggplot(percent_hab_all, aes(x = Year, y = proportion, colour = Habitat_suitability)) + 
  geom_line() +
  geom_errorbar(aes(ymin= proportion-sd,ymax= proportion+sd))

ggsave("Habitat_Prop_byyear.png")




##########Stocks###########
#i do not know why this code below doesn't work here, yet it worked for dataframe percent_hab below
Avg_btm_temp = Avg_btm_temp %>% mutate(Stock = ifelse(grepl("4T|4S|4R", Avg_btm_temp$ZONE), "GSL",
                                                      ifelse(grepl("4X|4W|4Vs|3Pn|3Ps|3O|3N", Avg_btm_temp$ZONE), "SS_NF", "NA")))

#dividing data into two stocks using NAFo zones
stock_hab_prop = percent_hab %>% mutate(Stock = ifelse(grepl("4T|4S|4R", percent_hab$ZONE), "GSL", 
                                                       ifelse(grepl("4X|4W|4Vs|3Pn|3Ps|3O|3N", percent_hab$ZONE), "SS_NF", "NA")))

stock_hab = stock_hab_prop %>% filter(Stock != "NA") %>% group_by(Year,Stock, Habitat_suitability) %>% summarise_at(.vars = c("proportion","sd"), funs(mean))

#Merging dataframes because above code didnt work
stock_temp = merge(Avg_btm_temp, stock_hab_prop[,c(1,3,6)]) %>% unique() %>% filter(Stock != "NA") %>% 
  group_by(Year, Stock) %>% summarise(Ann_AVG = mean(Annual_AVG))

#Plot average temperature by stocks
ggplot(stock_temp, aes(x = Year, y = Ann_AVG, colour = Stock)) + 
  geom_line() + geom_smooth(method = "lm")

#create ggplot by stocks with and without error bars
#you can save the figures without error bars if youd like too
filter(stock_hab, Habitat_suitability == "Preffered") %>%  
  ggplot(aes(x = Year, y = proportion, colour = Stock)) + 
  geom_line() +
  geom_errorbar(aes(ymin= proportion-sd,ymax= proportion+sd))



filter(stock_hab, Habitat_suitability == "Good") %>%  
  ggplot(aes(x = Year, y = proportion, colour = Stock)) + 
  geom_line() +
  geom_errorbar(aes(ymin= proportion-sd,ymax= proportion+sd))



filter(stock_hab, Habitat_suitability == "Good_temp") %>%  
  ggplot(aes(x = Year, y = proportion, colour = Stock)) + 
  geom_line() +
  geom_errorbar(aes(ymin= proportion-sd,ymax= proportion+sd))


filter(stock_hab, Habitat_suitability == "Pref_temp") %>%  
  ggplot(aes(x = Year, y = proportion, colour = Stock)) + 
  geom_line() +
  geom_errorbar(aes(ymin= proportion-sd,ymax= proportion+sd))






#################Plots with Trend Lines##############################

###Total Bottom Temperature trend by NAFO zone for each season
#Melting data into long format for implementing season as group
Avg_btm_temp_2 = melt.data.table(as.data.table(Avg_btm_temp), id.vars = c("Year","ZONE")) %>% 
  rename(Season = variable, Temperature = value) 



ggplot(Avg_btm_temp_2, aes(x = Year, y = Temperature,  colour = Season )) + 
  geom_line() + facet_wrap(~ZONE, scales = "free") + 
  geom_smooth(method = "lm", aes(colour = Season)) + theme_bw() + theme(axis.line = element_line(colour = "black"),
                                                  panel.grid.major = element_blank(),
                                                  panel.grid.minor = element_blank(),
                                                  panel.background = element_blank()) 

ggplot(Avg_btm_temp_2 %>% filter(Season == 'Annual_AVG'), aes(x = Year, y = Temperature)) + 
  geom_line() + facet_wrap(~ZONE, scales = "free") + 
  geom_smooth(method = "lm") + theme_bw() + theme(axis.line = element_line(colour = "black"),
                                                 panel.grid.major = element_blank(),
                                                 panel.grid.minor = element_blank(),
                                                 panel.background = element_blank())

ggplot(Avg_btm_temp_2 %>% filter(Season == 'Winter_AVG'), aes(x = Year, y = Temperature)) + 
  geom_line() + facet_wrap(~ZONE, scales = "free") + 
  geom_smooth(method = "lm") + theme_bw() + theme(axis.line = element_line(colour = "black"),
                                                  panel.grid.major = element_blank(),
                                                  panel.grid.minor = element_blank(),
                                                  panel.background = element_blank())


ggplot(Avg_btm_temp_2 %>% filter(Season == 'Summer_AVG'), aes(x = Year, y = Temperature)) + 
  geom_line() + facet_wrap(~ZONE, scales = "free") + 
  geom_smooth(method = "lm") + theme_bw() + theme(axis.line = element_line(colour = "black"),
                                                  panel.grid.major = element_blank(),
                                                  panel.grid.minor = element_blank(),
                                                  panel.background = element_blank())






###Averaege Temperature by stock
ggplot(stock_temp, aes(x = Year, y = Ann_AVG, colour = Stock)) + 
  geom_line() + geom_smooth(method = "lm") + theme_bw() + theme(axis.line = element_line(colour = "black"),
                                                                panel.grid.major = element_blank(),
                                                                panel.grid.minor = element_blank(),
                                                                panel.background = element_blank())

###Proportion of habitat type by zone for preffered and good temperatures with and without preffered depth

ggplot(percent_pref, aes(x = Year, y = proportion)) + 
  geom_line() + facet_wrap(~ZONE, scales = "free") +
  geom_smooth(method = "lm") + theme_bw() + theme(axis.line = element_line(colour = "black"),
                                                  panel.grid.major = element_blank(),
                                                  panel.grid.minor = element_blank(),
                                                  panel.background = element_blank())


ggplot(percent_good, aes(x = Year, y = proportion)) + 
  geom_line() + facet_wrap(~ZONE, scales = "free") + 
  geom_smooth(method = "lm") + theme_bw() + theme(axis.line = element_line(colour = "black"),
                                                  panel.grid.major = element_blank(),
                                                  panel.grid.minor = element_blank(),
                                                  panel.background = element_blank())


ggplot(percent_pref_temp, aes(x = Year, y = proportion)) + 
  geom_line() + facet_wrap(~ZONE, scales = "free") + 
  geom_smooth(method = "lm") + theme_bw() + theme(axis.line = element_line(colour = "black"),
                                                  panel.grid.major = element_blank(),
                                                  panel.grid.minor = element_blank(),
                                                  panel.background = element_blank())


ggplot(percent_good_temp, aes(x = Year, y = proportion)) + 
  geom_line() + facet_wrap(~ZONE, scales = "free") + 
  geom_smooth(method = "lm") + theme_bw() + theme(axis.line = element_line(colour = "black"),
                                                  panel.grid.major = element_blank(),
                                                  panel.grid.minor = element_blank(),
                                                  panel.background = element_blank())


###Proportion of habitat type by stock for preffered and good temperatures with and without preffered depth
filter(stock_hab, Habitat_suitability == "Preffered") %>%  
  ggplot(aes(x = Year, y = proportion, colour = Stock)) + 
  geom_line() +
  geom_smooth(method = "lm")


filter(stock_hab, Habitat_suitability == "Good") %>%  
  ggplot(aes(x = Year, y = proportion, colour = Stock)) + 
  geom_line() +
  geom_smooth(method = "lm")


filter(stock_hab, Habitat_suitability == "Pref_temp") %>%  
  ggplot(aes(x = Year, y = proportion, colour = Stock)) + 
  geom_line() +
  geom_smooth(method = "lm")

filter(stock_hab, Habitat_suitability == "Good_temp") %>%  
  ggplot(aes(x = Year, y = proportion, colour = Stock)) + 
  geom_line() +
  geom_smooth(method = "lm")

