
library(tidyverse)
library(car)
load("BNAM_Step2.RData")

temp <- btmp %>% select(-Depth, -Habitat) %>%
  filter(Year == c(1990,2000,2010,2018))
temp$Year <- factor(temp$Year)
temp$ZONE <- factor(temp$ZONE)

temp <- temp %>% group_by(ZONE) %>% aov(Annual_AVG ~ Year) %>% 
  summary(nova_int)
#Creating a vector of zones for loops
zones <- as.character(unique(temp$ZONE))
#ANOVA for each zones
for(z in zones){
  nova <- aov(Annual_AVG ~ Year, data = temp %>% filter(ZONE == z))
  nova <- summary(nova)
  print(c(z, nova))
}
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


#Next steps
#test based on range of years (decades?)
#next try for summer temperature (or season based on survey data)
#then test change in habitat suitability and GDD