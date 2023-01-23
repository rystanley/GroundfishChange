#Data Exploration
library(tidyverse)

#load("data/groundfish_surveys/RV_2021Feb19.RData")
load("data/groundfish_surveys/RV_hali_PA_Mar2021.RData")
rv <- halibut
glimpse(rv)
summary(rv)

#Build table of data by region
rv %>% group_by(region) %>% summarise(min(year), max(year), n()) #total observations


rv %>% filter(presence == 1, !(is.na(btm_temp) & is.na(depth))) %>% group_by(region) %>% summarise(min(year), max(year), n())

rv %>% group_by(presence) %>% count()



hali <- rv %>% filter(presence == 1)

#Histograms of varaibles
#Year coverage
ggplot(hali)+
  geom_histogram(aes(year), col = "white") 
#bottom temperature
ggplot(hali)+
  geom_histogram(aes(btm_temp), col = "white") #Here we have a value of temp that is way too high
#depth
ggplot(hali)+
  geom_histogram(aes(depth), col = "white")



#
ggplot(rv)+ 
  geom_point(aes(x = depth, y = btm_temp)) #one depth variable that is outlier and one temp outlier

ggplot(rv %>% filter(btm_temp < 30, depth < 2000))+ 
  geom_point(aes(x = depth, y = btm_temp))

ggplot(hali %>% filter(btm_temp < 30, depth < 2000))+ 
  geom_point(aes(x = depth, y = btm_temp))

ggplot(rv %>% filter(!is.na(presence), btm_temp<30))+
  geom_violin(aes(x = as.factor(presence), y = btm_temp),
              draw_quantiles = c(.25,.5,.75)) #remove the NAs is presence


#By region
ggplot(rv)+ 
  geom_boxplot(aes(x = region, y = btm_temp))

ggplot(rv)+ 
  geom_boxplot(aes(x = region, y = depth)) #Majority of data is under 500

ggplot(rv)+ 
  geom_boxplot(aes(x = region, y = year)) # NF and NG seem to be lacking compared to other regions

ggplot(rv)+ 
  geom_boxplot(aes(x = region, y = month)) # NG and SG are pretty much entirely surveyed in august and september





#PLotting biomass, count and weight by temp and depth
#Scotian Shelf
ggplot(hali %>% filter(region == "SS"))+
  geom_smooth(aes(x = btm_temp, y = count))

ggplot(hali %>% filter(region == "SS"))+
  geom_smooth(aes(x = depth, y = count))

#Southern Gulf
ggplot(hali %>% filter(region == "SG"))+
  geom_smooth(aes(x = btm_temp, y = biomass))

ggplot(hali %>% filter(region == "SG"))+
  geom_smooth(aes(x = depth, y = biomass))

# Northern Gulf
ggplot(hali %>% filter(region == "NG"))+
  geom_smooth(aes(x = btm_temp, y = count))

ggplot(hali %>% filter(region == "NG"))+
  geom_smooth(aes(x = depth, y = count))
# Newfoundland
ggplot(hali %>% filter(region == "NF"))+
  geom_smooth(aes(x = btm_temp, y = totwgt)) #total weight not the best to use, it would be best if we had count or biomass

ggplot(hali %>% filter(region == "NF"))+
  geom_smooth(aes(x = depth, y = totwgt))
#USA 
ggplot(hali %>% filter(region == "USA"))+
  geom_smooth(aes(x = btm_temp, y = count))

ggplot(hali %>% filter(region == "USA"))+
  geom_smooth(aes(x = depth, y = count))





rv2$year_group <- cut_interval(rv$year, n = 10)


#Plots over time
ggplot(rv2)+
  geom_boxplot(aes(x = year_group, y = presence))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


ggplot(hali)+
  geom_smooth(aes(x = year, y = count))

ggplot(hali)+
  geom_smooth(aes(x = year, y = totwgt))

ggplot(hali)+
  geom_smooth(aes(x = year, y = catch))

ggplot(hali)+
  geom_smooth(aes(x = year, y = biomass))


