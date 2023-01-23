
library(tidyverse)
library(data.table)
library(broom)



## Preferred Habitat ----
load("data/BNAM_hab_2021.RData")
#This is depth between 25-200m

hab <- prop_hab %>% filter(Habitat == "Within_hab") %>% mutate(Percent = Proportion*100,
                                                               ZONE = factor(ZONE)) #setting 3M to reference for model

hab$ZONE <- relevel(hab$ZONE, ref = "3M")

##Creating a saturated model
hab_lm <- lm(Percent ~ Year, hab)
summary(hab_lm)


#Changing percent slops 

hab <- hab %>%
  group_by(ZONE) %>% 
  mutate(pct_change = (lag(Proportion)/Proportion-1) * 100) %>% 
  mutate(adjustedpctchange = ifelse(pct_change <100, pct_change*-1, pct_change-100)) #can be deleted

#lm values for each zone
#Organising all lm values into a dataframe
lm_vals_hab <- hab %>% 
  group_by(ZONE) %>% 
  nest() %>% 
  mutate(model = map(data, ~lm(Percent ~ Year, data = .) %>% tidy)) %>% 
  unnest(model) %>% 
  filter(term == 'Year') %>% 
  mutate(CI = qnorm(.975)*std.error)


# lm_vals_hab <- hab %>% 
#   group_by(ZONE) %>% 
#   nest() %>% 
#   mutate(model = map(data, ~lm(adjustedpctchange ~ Year, data = .) %>% tidy)) %>% 
#   unnest(model) %>% 
#   filter(term == 'Year') %>% 
#   mutate(CI = qnorm(.975)*std.error/sqrt(29))  note this can be deleted if percent change inst used


#add confint() to slopes into one data frame and save


#Organising all lm values into a dataframe
rsquare_hab <- hab %>% 
  group_by(ZONE) %>% 
  nest() %>% 
  mutate(model = map(data, ~lm(Percent ~ Year, data = .) %>% glance)) %>% 
  unnest(model) %>% 
  select(ZONE, data, r.squared, adj.r.squared)

##Merge data frames
slopes_hab <- full_join(lm_vals_hab %>% select(-data), rsquare_hab %>% select(-data)) %>% 
  mutate(Y_var = "Percent Thermal Habitat")

write.csv(slopes_hab, file = "data/slopes_habA.csv", row.names = F)




## GDD ----
load("data/GDD_2021.RData")

GDD <- GDD %>% mutate(ZONE = factor(ZONE),
                      ZONE = relevel(ZONE, ref = "3M"))

#Calculate poercent change (don't think this is worth doing because it makes interopretation confusing)
GDD <- GDD %>%
  group_by(ZONE) %>% 
  mutate(pct_change = (lag(sGDD)/sGDD-1) * 100) %>% 
  mutate(adjustedpctchange = ifelse(pct_change <100, pct_change*-1, pct_change-100)) #this works, just change to if its decreasing multiply percentage by -1 and increase subtract 100

##Creating a saturated model start filter adjustedpctchange replaces adjusted pctchange88
gdd_lm <- lm(sGDD ~ ZONE*Year, GDD)
summary(gdd_lm)

#lm values for each zone
#Organising all lm values into a dataframe
lm_vals_gdd <- GDD %>% 
  group_by(ZONE) %>% 
  nest() %>% 
  mutate(model = map(data, ~lm(sGDD ~ Year, data = .) %>% tidy)) %>% 
  unnest(model) %>% 
  filter(term == 'Year') %>% 
  mutate(CI = qnorm(.975)*std.error)


#Organising all lm values into a dataframe
rsquare_gdd <- GDD %>% 
  group_by(ZONE) %>% 
  nest() %>% 
  mutate(model = map(data, ~lm(sGDD ~ Year, data = .) %>% glance)) %>% 
  unnest(model) %>% 
  select(ZONE, data, r.squared, adj.r.squared)

##Merge data frames
slopes_gdd <- full_join(lm_vals_gdd %>% select(-data), rsquare_gdd %>% select(-data))%>% 
  mutate(Y_var = "Growing Degree Days")

#Export data frames

lm_output <- rbind(slopes_hab, slopes_gdd) %>% mutate(Significance = ifelse(p.value <= 0.001,"***",ifelse(
                                                                                         p.value <= 0.01,"**", ifelse(
                                                                                         p.value <= 0.05,"*", "Not Sig"))))

lm_outputr <- lm_output %>% mutate_at(vars(3:8), funs(round(., 3))) %>% mutate(Y_var = factor(Y_var, levels = c("Percent Thermal Habitat","Growing Degree Days")))                                                                        
write.csv(lm_outputr, file = "data/slopes_fig3A.csv", row.names = FALSE)


#Calculating the overall means with CI

confidence_interval <- function(vector, interval) {
  # Standard deviation of sample
  vec_sd <- sd(vector)
  # Sample size
  n <- length(vector)
  # Error according to t distribution
  result <- qt(interval, df = n - 1) * vec_sd / sqrt(n)
  # Confidence interval as a vector
  return(result)
}

lm_outputr %>% group_by(Y_var) %>% 
  summarise(mean = mean(estimate), ci = confidence_interval(estimate, 0.95), sd = sd(estimate))




## By region (not sig) ----

#Create plot for slopes
lm_outputr$ZONE <- fct_rev(lm_outputr$ZONE)

fig3 <- ggplot(lm_outputr)+
  geom_point(aes(x = ZONE, y = estimate))+
  facet_wrap(~Y_var, ncol = 2, scales = "free")+
  geom_hline(aes(yintercept = 0), linetype = 2)+
  geom_errorbar(aes(x = ZONE, ymin = estimate-CI, ymax = estimate+CI))+
  theme_bw()+
  theme(axis.text.x=element_text(angle = 90, vjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(fill="white"))+
  labs(x = "NAFO Division", y = paste0("Estimated \u0394Change per Year"));fig3


ggsave(file = "Figures/fig3_new.tiff",fig3, dpi = 600, width = 7.25, height = 5.4375, units = "in")
ggsave(file = "Figures/fig3_new.jpeg",fig3, dpi = 300, width = 7.25, height = 5.4375, units = "in")

fig3_flipped <- fig3+ facet_wrap(~Y_var, ncol = 1, scales = "free") +
  coord_flip()

ggsave(file = "Figures/fig3_flipped.tiff",fig3_flipped, dpi = 600, width = 7.25, height = 5.4375, units = "in")
ggsave(file = "Figures/fig3_flipped.jpeg",fig3_flipped, dpi = 300, width = 7.25, height = 5.4375, units = "in")






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

ggsave("output/map_slopesA.tiff",p,dpi=300,width=12,height=8,units="in")






#Percent Change plot
hab_bind <- hab %>% select(Year, ZONE, Percent) %>% mutate(unit = "Thermal Habitat", value = Percent)
gdd_bind <- GDD %>% select(Year, ZONE, sGDD) %>% mutate(unit = "Growing Degree Days", value = sGDD)
plotChange <- bind_rows(hab_bind, gdd_bind) %>% select(-Percent, -sGDD) %>% 
  mutate(period = case_when(Year < 2004 ~ "Before",
                            Year >= 2004 ~ "After")) %>%
  group_by(period, ZONE, unit) %>% summarise(mean = mean(value)) %>% 
  pivot_wider(names_from = period, values_from = mean)%>% 
  mutate(percentChange = (After/Before)*100 - 100,
         percentDiff = After - Before,
         unit = factor(unit, levels = c("Thermal Habitat", "Growing Degree Days")))




ggplot(plotChange, aes(x = Before, percentChange))+
  geom_text(aes(label=ZONE),hjust=0.5, vjust=0.5)+
  geom_hline(yintercept = 0, linetype = 2)+
  scale_x_continuous(limits = c(0,NA))+
  scale_y_continuous(breaks = c(0,25,50,75), labels = c("0","25", "50", "75"), limits = c(-15,75))+
  facet_wrap(~unit, scales = "free_x")+
  theme_bw()+
  theme(strip.background = element_blank(),
        strip.text.x = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.x=element_text(hjust = 0.5),
        panel.spacing = unit(1, "lines"))+
  labs(x = "Percent Thermal Habitat (1990-2003)                        Growing Degree Days (1990-2003)",
       y = "Percent Change (2004-2018)")

ggsave(file = "Figures/Fig4_new.tiff", dpi = 600, width = 7.25, height = 5.4375, units = "in")
ggsave(file = "Figures/Fig4_new.jpeg", dpi = 300, width = 7.25, height = 5.4375, units = "in")
