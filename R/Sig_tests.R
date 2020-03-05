
library(tidyverse)
library(car)
library(broom)
library(marmap)
library(mapdata)

## Avereage Bottom Temperatures
load("data/BNAM_Step2.RData")

btmp$ZONE <- factor(btmp$ZONE)
mTemp <- btmp %>% group_by(Year, ZONE) %>%  
         filter(Depth <= 400)%>% 
         summarise(Temp = mean(Annual_AVG))


##Creating a saturated model
temp_lm <- lm(Temp ~ ZONE*Year, mTemp)
summary(temp_lm)


#Creating a vector of zones for loop
zones <- as.character(unique(mTemp$ZONE))
#ANOVA for each zones (not needed anymore)
#for(z in zones){
#lm <- lm(Temp ~ Year, data = mTemp %>% filter(ZONE == z))
#lm <- summary(lm)
#print(c(z, lm))
#}


#Organising all lm values into a dataframe
lm_vals <- mTemp %>% 
              group_by(ZONE) %>% 
              nest() %>% 
              mutate(model = map(data, ~lm(Temp ~ Year, data = .) %>% tidy)) %>% 
              unnest(model) %>% 
              filter(term == 'Year')

#Organising all lm values into a dataframe
rsquare <- mTemp %>% 
  group_by(ZONE) %>% 
  nest() %>% 
  mutate(model = map(data, ~lm(Temp ~ Year, data = .) %>% glance)) %>% 
  unnest(model) %>% 
  select(ZONE, data, r.squared, adj.r.squared)

##Merge data frames
slopes <- full_join(lm_vals %>% select(-data), rsquare %>% select(-data))


#Map for Presentation (attempt 1)
latlong <- "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"
load("data/BNAM_map.RData")
canada <- map_data("worldHires", "Canada")

NAFO <- st_read("data/Divisions.shp")%>%
  st_transform(latlong)%>%
  select(ZONE,geometry)%>%
  filter(!is.na(ZONE), grepl("[345]", ZONE)) 

NAFO_slopes <- full_join(slopes, NAFO %>% filter(grepl("[34]", ZONE))) %>% select(ZONE, geometry, estimate)


ggplot(NAFO_slopes)+
  geom_sf(aes(fill = estimate, geometry = geometry))+
  scale_fill_viridis_c(option = "D",
                       name=expression(paste("Rate of Change")))+
  geom_polygon(data = canada, aes(x=long, y = lat, group = group), 
               fill = "grey70")+
  coord_sf(xlim = c(-42, -68), ylim = c(39, 52), expand = FALSE) +
  theme_bw()
## Preferred Habitat --
load("BNAM_hab.RData")
#This is depth between 25-200m

hab <- prop_hab %>% filter(Habitat == "Preferred")

##Creating a saturated model
hab_lm <- lm(Proportion ~ ZONE*Year, hab)
summary(hab_lm)

#lm values for each zone
#Organising all lm values into a dataframe
lm_vals0 <- hab %>% 
  group_by(ZONE) %>% 
  nest() %>% 
  mutate(model = map(data, ~lm(Proportion ~ Year, data = .) %>% tidy)) %>% 
  unnest(model) %>% 
  filter(term == 'Year')

## GDD --
load("GDD.RData")
#Should depth be restriced to <400 or <200 (note to self change code to restrict depth in GDD if I haven't)
GDD <- GDD %>% filter(Habitat == "Preferred")

##Creating a saturated model
gdd_lm <- lm(sGDD ~ ZONE*Year, hab)
summary(gdd_lm)

#lm values for each zone
#Organising all lm values into a dataframe
lm_vals0 <- gdd %>% 
  group_by(ZONE) %>% 
  nest() %>% 
  mutate(model = map(data, ~lm(sGDD ~ Year, data = .) %>% tidy)) %>% 
  unnest(model) %>% 
  filter(term == 'Year')













###What we did in our meeting
#Linear model
btmp$ZONE <- factor(btmp$ZONE)
str(btmp)
mylm <- lm(Annual_AVG~ ZONE*Year, btmp)
summary(mylm)
mylm2<-lm(Annual_AVG~ ZONE+ZONE:Year, btmp)
summary(mylm2)


for(z in zones){
  nova <- aov(Annual_AVG ~ Year, data = btmp %>% filter(ZONE == z))
  nova <- summary(nova)
  print(c(z, nova))
}


lm4x <- lm(Annual_AVG ~ Year, data = btmp %>% filter(ZONE == "4X"))
summary(lm4x)
aov(lm4x)
                       #Tukeys test for each significant zone
sig <- zones[!zones %in% c("3Pn", "3M")]
for(s in sig){
  nova <- aov(Annual_AVG ~ Year, data = temp %>% filter(ZONE == s))
  tuk <- TukeyHSD(nova, "Year")
  print(c(s, tuk))
}




# ##Testing summer temperature changes ----
# #ANOVA for each zones
# for(z in zones){
#   nova <- aov(Summer_AVG ~ Year, data = temp %>% filter(ZONE == z))
#   nova <- summary(nova)
#   print(c(z, nova))
# }
# #Tukeys test for each significant zone
# for(s in sig){
#   nova <- aov(Summer_AVG ~ Year, data = temp %>% filter(ZONE == s))
#   tuk <- TukeyHSD(nova, "Year")
#   print(c(s, tuk))
# }



#next try for summer temperature (or season based on survey data)
#then test change in habitat suitability and GDD
