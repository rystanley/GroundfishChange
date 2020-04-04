
library(tidyverse)
library(data.table)
library(broom)



#for the map (but cause some funtuions to not work)
#library(marmap)
#library(mapdata)

## Avereage Bottom Temperatures ----
load("data/BNAM_Step2.RData")



#Probably get rid of depth restrictions, because we just want to show which areas are icnreaseing the most, not necessarily habitable areas, since we can't plot all areas wihin this depth range
btmp$ZONE <- factor(btmp$ZONE)
mTemp <- btmp %>% group_by(Year, ZONE) %>%  
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


#Organising all lm values into a dataframe (sometimes this doesn't work and soemtimes it does... very annoying)
lm_vals_temp <- mTemp %>% 
              group_by(ZONE) %>% 
              nest() %>% 
              mutate(model = map(data, ~lm(Temp ~ Year, data = .) %>% tidy)) %>% 
              unnest(model) %>% 
              filter(term == 'Year')

#Organising all lm values into a dataframe
rsquare_temp <- mTemp %>% 
  group_by(ZONE) %>% 
  nest() %>% 
  mutate(model = map(data, ~lm(Temp ~ Year, data = .) %>% glance)) %>% 
  unnest(model) %>% 
  select(ZONE, data, r.squared, adj.r.squared)

##Merge data frames
slopes_temp <- full_join(lm_vals_temp %>% select(-data), rsquare_temp %>% select(-data))%>% 
  mutate(Y_var = "Annual Avg Temp") %>% filter(ZONE != "3M")



## Preferred Habitat ----
load("data/BNAM_hab.RData")
#This is depth between 25-200m

hab <- prop_hab %>% filter(Habitat == "Preferred") %>% mutate(Proportion = Proportion*100)

##Creating a saturated model
hab_lm <- lm(Proportion ~ ZONE*Year, hab)
summary(hab_lm)

#lm values for each zone
#Organising all lm values into a dataframe
lm_vals_hab <- hab %>% 
  group_by(ZONE) %>% 
  nest() %>% 
  mutate(model = map(data, ~lm(Proportion ~ Year, data = .) %>% tidy)) %>% 
  unnest(model) %>% 
  filter(term == 'Year')

#Organising all lm values into a dataframe
rsquare_hab <- hab %>% 
  group_by(ZONE) %>% 
  nest() %>% 
  mutate(model = map(data, ~lm(Proportion ~ Year, data = .) %>% glance)) %>% 
  unnest(model) %>% 
  select(ZONE, data, r.squared, adj.r.squared)

##Merge data frames
slopes_hab <- full_join(lm_vals_hab %>% select(-data), rsquare_hab %>% select(-data)) %>% 
  mutate(Y_var = "Preferred Habitat")






## GDD ----
load("GDD.RData")

##Creating a saturated model
gdd_lm <- lm(sGDD ~ ZONE*Year, GDD)
summary(gdd_lm)

#lm values for each zone
#Organising all lm values into a dataframe
lm_vals_gdd <- GDD %>% 
  group_by(ZONE) %>% 
  nest() %>% 
  mutate(model = map(data, ~lm(sGDD ~ Year, data = .) %>% tidy)) %>% 
  unnest(model) %>% 
  filter(term == 'Year')


#Organising all lm values into a dataframe
rsquare_gdd <- GDD %>% 
  group_by(ZONE) %>% 
  nest() %>% 
  mutate(model = map(data, ~lm(sGDD ~ Year, data = .) %>% glance)) %>% 
  unnest(model) %>% 
  select(ZONE, data, r.squared, adj.r.squared)

##Merge data frames
slopes_gdd <- full_join(lm_vals_gdd %>% select(-data), rsquare_gdd %>% select(-data))%>% 
  mutate(Y_var = "GDD")

#Export data frames

lm_output <- rbind(slopes_temp, slopes_hab, slopes_gdd) %>% mutate(Significance = ifelse(p.value <= 0.001,"***",ifelse(
                                                                                         p.value <= 0.01,"**", ifelse(
                                                                                         p.value <= 0.05,"*", "Not Sig"))))

lm_outputr <- lm_output %>% mutate_at(vars(3:8), funs(round(., 3)))                                                                               
fwrite(lm_outputr, file = "output/Stat_lmtables_new.csv", row.names = FALSE)

## By region (not sig) ----


btmp$Region =  ifelse(grepl("4T|4S|4R", btmp$ZONE), "GSL",
                      ifelse(grepl("4X|4W|4Vs|4Vn", btmp$ZONE), "SS", 
                             ifelse(grepl("3Pn|3Ps|3O|3N|3L|3K", btmp$ZONE), "NF", NA)))

btmp$Region <- factor(btmp$Region)
reg_temp <- btmp %>% group_by(Year, Region) %>%  
  summarise(Temp = mean(Annual_AVG))

##Creating a saturated model
temp_lm <- lm(Temp ~ Region*Year, reg_temp)
summary(temp_lm)



##Map for Presentation (attempt 1) ----
#get mapping baselayers 
load("data/BNAM_map.RData")
Canada <- ne_states(country = "Canada",returnclass = "sf")%>%
  select(latitude,longitude,geonunit,geometry)%>%
  st_union()%>% #group provinces + territories
  st_as_sf()

USA <- ne_states(country = "United States of America",returnclass = "sf")%>%
  st_transform(latlong)%>%
  filter(region%in%c("Northeast","South"))%>%
  select(latitude,longitude,geonunit,geometry)%>%
  st_union()%>% #group states
  st_as_sf()


NAFO <- st_read("data/Divisions.shp")%>%
  st_transform(latlong)%>%
  select(ZONE,geometry)%>%
  filter(!is.na(ZONE), grepl("[345]", ZONE)) 

#merge data frames with NAFO regions and slopes for temperature change
NAFO_slopes <- full_join(slopes, NAFO %>% filter(grepl("[345]", ZONE))) %>% select(ZONE, geometry, estimate) 

p <- ggplot(NAFO_slopes)+
  geom_sf(aes(fill = estimate, geometry = geometry))+
  scale_fill_viridis_c(option = "D",
                       name=expression(paste("Rate of Change (", degree,"C per year)")))+
  geom_sf_text(data=NAFO,aes(label=ZONE),colour="red", size = 7 )+
  geom_sf(data = Canada)+
  geom_sf(data = USA)+
  coord_sf(xlim = c(-42, -71.7), ylim = c(39, 52), expand = FALSE) +
  theme_bw()+
  theme(legend.position = "bottom",
        legend.text = element_text(angle=-90, vjust = .6),
        text = element_text(size=18), 
        axis.text.x = element_text(color = "grey20", size = 12, vjust = .5),
        axis.text.y = element_text(color = "grey20", size = 12, vjust = .5),
        axis.title.x = element_text(margin = margin(t = 10)), 
        axis.title.y = element_text(margin = margin(b = 10)),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white", colour = "black"),
        plot.background = element_rect(colour = "white"),
        strip.background = element_rect(colour = "black", fill = "white"))+
  labs(x=expression(paste("Longitude ",degree,"W",sep="")),
       y=expression(paste("Latitude ",degree,"N",sep="")));p

ggsave("output/map_slopes.tiff",p,dpi=300,width=12,height=8,units="in")

