library(tidyverse)
library(sf) #spatial work
library(mgcv) #predicting with gams
library(scales) #rescale function

source("R/Other/make_map_ac.R")

#load BNAM

#load Model
#common projection 
latlong <- "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"

#read in NAFO division shape files
EEZ <- st_read("data/Canada_EEZ.shp")%>%
  st_transform(latlong)

NAFO <- st_read("data/Divisions.shp")%>%
  st_transform(latlong)%>%
  select(ZONE,geometry)%>%
  filter(!is.na(ZONE), grepl("[345]", ZONE)) 

NAFO$region =  ifelse(grepl("4S|4R", NAFO$ZONE), "NG",
                      ifelse(grepl("4T", NAFO$ZONE), "SG",
                             ifelse(grepl("4X|4W|4Vs|4Vn", NAFO$ZONE), "SS", 
                                    ifelse(grepl("3Pn|3Ps|3O|3N|3L|3M|3K", NAFO$ZONE), "NF",
                                           ifelse(grepl("5Y|5Ze|5Zw|6A|6B|6C|6D", NAFO$ZONE),"USA", NA)))))

## Using new BNAM files ----
load("data/groundfish_surveys/RV_species_PA_Mar2021_missingUS.RData") #PA for halibut and other species (exceept other species not avaialbe for US data)
rv <- rv_pata %>% select(-(16:25))
#Read in models
load("data/BNAM/BNAM.RData")
#load(file = "data/year_model.R")
##load(file = "data/bnam_gam.R") outdated
#load(file = "data/final_model.R")
load("forecastBNAM.Rdata")
load('data/BNAM/F1_R45.Rdata')
load('data/BNAM/F1_R85.Rdata')
load('data/BNAM/F2_R45.Rdata')
load('data/BNAM/F2_R85.Rdata')

#Make prediction with model data
rv_map <- rv %>% filter(!is.na(btm_temp), !is.na(depth)) %>% ungroup()
rv_map$fitted <- m8$fitted.values 

 make_map(rv_map %>% select(longitude, latitude, fitted), fill_name = "Probability of Occurrence", long.lim = c(-71.66,-42), lat.lim = c(39,52)) +
    scale_fill_continuous(type = "viridis", guide = guide_colourbar(label.theme = element_text(angle = 90, size = 8.5, hjust = .5, vjust = .5)),
                           limits = c(min(rv_map$fitted), max(rv_map$fitted))) + labs(title = "Fitted Values")

loop_vars <- c("btm_temp_PC", "btm_temp_F1_R45", "btm_temp_F1_R85", "btm_temp_F2_R45", "btm_temp_F2_R85")#,
              # "delta_F1_R45", "delta_F1_R85", "delta_F2_R45", "delta_F2_R85")

titles <- c("Present Climate", rep(c("2046-2065 Climate @ RCP 4.5","2046-2065 Climate @ RCP  8.5", 
                                   "2066-2085 Climate @ RCP 4.5", "2066-2085 Climate @ RCP 8.5"), 2))
#Prediction Present Climate 

mapdata <- forecast %>% select(longitude,latitude, region, season) 
#Mapping
for(i in 1:length(loop_vars)){
  prob <- predict(seasonModel, newdata = forecast %>% 
                       select(longitude, latitude, region, depth, season, loop_vars[i]) %>%
                       rename(btm_temp = loop_vars[i]), 
                     type=c("response"))
  mapdata[i+4] <- prob
}




colnames(mapdata) <- c("longitude", "latitude", "region", "season", loop_vars)

mapdata2 <-mapdata %>%  mutate(delta_F1_R45 = btm_temp_F1_R45 - btm_temp_PC,
                              delta_F1_R85 = btm_temp_F1_R85 - btm_temp_PC,
                              delta_F2_R45 = btm_temp_F2_R45 -  btm_temp_PC,
                              delta_F2_R85 = btm_temp_F2_R85 - btm_temp_PC) %>% 
  filter((region == "NF" & (season == 'Fall' | season == "Spring")) |
         (region == 'NG' & season == 'Summer') |
         (region == 'SS' & (season == 'Summer' | season == 'Winter')) |
         (region == 'SG' & season == 'Summer') |
         (region == 'USA' & (season == "Spring" | season == "Fall"))) %>%
  group_by(longitude, latitude, region) %>%
  mutate(delta_F1_R45 = mean(delta_F1_R45),
         delta_F1_R85 = mean(delta_F1_R85),
         delta_F2_R45 = mean(delta_F2_R45),
         delta_F2_R85 = mean(delta_F2_R85)) %>% 
  ungroup() %>% select(-region, -season)


loop_vars <- c("btm_temp_PC", "btm_temp_F1_R45", "btm_temp_F1_R85", "btm_temp_F2_R45", "btm_temp_F2_R85",
               "delta_F1_R45", "delta_F1_R85", "delta_F2_R45", "delta_F2_R85")

for(i in 1:length(loop_vars)){
  mapdata1 <- mapdata2
p1 <- make_map(mapdata1[c(1:2, i+2)], fill_name = "Probability of Occurrence")
  
  if(i > 5){p1 <- p1 + scale_fill_gradientn(colours = c("blue", "white", "red"), values = rescale(c(quantile(mapdata1$delta_F2_R85, probs = .999)[[1]]*-1, 0.0, quantile(mapdata1$delta_F2_R85, probs = .999)[[1]])), #rescaled to have even heat scale on ggplot
                                            guide = guide_colourbar(label.theme = element_text(angle = 90, size = 8, hjust = .5, vjust = .5)),limits = c(quantile(mapdata1$delta_F2_R85, probs = .999)[[1]]*-1, quantile(mapdata1$delta_F2_R85, probs = .999)[[1]]))+
                                   labs(fill = expression(paste(Delta,"Probability of Occurrence  " , sep="")))
  }else{p1 <- p1 + scale_fill_continuous(type = "viridis", limits = c(0, quantile(mapdata1$btm_temp_F2_R85, probs = .999)[[1]]), guide = guide_colourbar(label.theme = element_text(angle = 90, size = 8, hjust = .5, vjust =  .5)))}
  
  #assigns a name for the newly create dataframe based on year (1989+i)
  assign(paste0("p_",loop_vars[i]), p1) 
  rm(p1)
}


p_btm_temp_PC + geom_sf(data = NAFO, alpha = 0, colour = "azure4")  +
  scale_x_continuous(limits = c(-71.62,-43), expand = c(0,0))+
  scale_y_continuous(limits = c(40,52), expand = c(0,0))+
  geom_sf_text(data=NAFO,aes(label=ZONE),colour="white", size = 3.5)+
  geom_sf(data = EEZ, lty = 2, alpha = 0, colour = "azure4")+
  annotate("text", x = -57, y = 40.7, label = "EEZ", colour = "white", size = 2.5)

save(p_btm_temp_PC, file = "PC_Raster.Rdata")
ggsave(file = "Figures/Fig5_Season.tiff", dpi = 600, width = 7.25, height = 5.4375, units = "in")
ggsave(file = "Figures/Fig5_Season.jpeg", dpi = 300, width = 7.25, height = 5.4375, units = "in")


p_btm_temp_F1_R45 
p_btm_temp_F1_R85
p_btm_temp_F2_R45
p_btm_temp_F2_R85



patch1 <- p_delta_F1_R45 + geom_sf(data = NAFO, alpha = 0, colour = "azure4")  +
  scale_x_continuous(limits = c(-71.62,-43), expand = c(0,0))+
  scale_y_continuous(limits = c(40,52), expand = c(0,0))+
  geom_sf_text(data=NAFO,aes(label=ZONE),colour="black", size = 2)+
  geom_sf(data = EEZ, lty = 2, alpha = 0, colour = "azure4")+
  annotate("text", x = -57, y = 40.7, label = "EEZ", colour = "black", size = 2.5)

patch2 <- p_delta_F2_R45+ geom_sf(data = NAFO, alpha = 0, colour = "azure4")  +
  scale_x_continuous(limits = c(-71.62,-43), expand = c(0,0))+
  scale_y_continuous(limits = c(40,52), expand = c(0,0))+
  #geom_sf_text(data=NAFO,aes(label=ZONE),colour="black", size = 2)+
  geom_sf(data = EEZ, lty = 2, alpha = 0, colour = "azure4")
 # annotate("text", x = -57, y = 40.7, label = "EEZ", colour = "white", size = 2.5)

patch3 <- p_delta_F1_R85+ geom_sf(data = NAFO, alpha = 0, colour = "azure4")  +
  scale_x_continuous(limits = c(-71.62,-43), expand = c(0,0))+
  scale_y_continuous(limits = c(40,52), expand = c(0,0))+
 # geom_sf_text(data=NAFO,aes(label=ZONE),colour="black", size = 2)+
  geom_sf(data = EEZ, lty = 2, alpha = 0, colour = "azure4")
 # annotate("text", x = -57, y = 40.7, label = "EEZ", colour = "white", size = 2.5)

patch4 <- p_delta_F2_R85+ geom_sf(data = NAFO, alpha = 0, colour = "azure4")  +
  scale_x_continuous(limits = c(-71.62,-43), expand = c(0,0))+
  scale_y_continuous(limits = c(40,52), expand = c(0,0))+
 # geom_sf_text(data=NAFO,aes(label=ZONE),colour="black", size = 2)+
  geom_sf(data = EEZ, lty = 2, alpha = 0, colour = "azure4")
 # annotate("text", x = -57, y = 40.7, label = "EEZ", colour = "white", size = 2.5)




library(patchwork)
library(cowplot)

#p_btm_temp_PC/
  
myplot <- (patch1 + patch3)/(patch2+patch4) + #(p_delta_F1_R45 + p_delta_F1_R85)/(p_delta_F2_R45+p_delta_F2_R85) + 
  plot_annotation(tag_levels = 'a', tag_suffix = ")") +
  plot_layout(guides = "collect") & theme(legend.position = 'bottom', plot.tag = element_text(size = 8),
                                          legend.title= element_text(size=10, vjust = 0.9))

myplot$title <- 'test'

myplot[[1]][[1]] <- myplot[[1]][[1]] + theme(axis.text.x = element_blank(),
                                             plot.tag.position = c(.176,.875),
                                       axis.title.x = element_blank(),
                                       axis.title.y = element_text(hjust = 0.5, size = 9),
                                       plot.title = element_text(hjust = 0.5, size = 8)) +
                    labs(title = "RCP 4.5 (2046-2065)")

myplot[[1,2]] <- myplot[[1,2]] + theme(axis.text.y = element_blank(),
                                       plot.tag.position = c(0.042,.875),
                                   axis.title.y = element_blank(),
                                   axis.text.x = element_blank(),
                                   axis.title.x = element_blank(),
                                   plot.margin = margin(0, 0, -4, -4, "cm"),
                                   plot.title = element_text(hjust = 0.5, size = 8))+
  labs(title = "RCP 8.5 (2046-2065)")

myplot[[2]][[1]] <- myplot[[2]][[1]] + theme(plot.title = element_text(hjust = 0.5, size = 8),
                                             axis.title.y = element_text(hjust = 0.5, size = 9),
                                             axis.title.x = element_text(hjust = 0.5, size = 9,),
                                             plot.tag.position = c(0.176,.922))+
  labs(title = "RCP 4.5 (2066-2085)")

myplot[[2,2]] <- myplot[[2,2]] + theme(axis.text.y = element_blank(),
                                       plot.tag.position = c(0.042,.922),
                                       axis.title.y = element_blank(),
                                       axis.title.x = element_text(hjust = 0.5, size = 9),
                                       plot.margin = margin(-4, 0, 0, -4, "cm"),
                                       plot.title = element_text(hjust = 0.5, size = 8))+
  labs(title = "RCP 8.5 (2066-2085)")

myplot

save(myplot, file = "Fig6.Rdata")
ggsave(file = "Figures/Fig6_season_av.tiff", dpi = 600, width = 7.25, height = 5.4375, units = "in")
ggsave(file = "Figures/Fig6_season_av.jpeg", dpi = 300, width = 7.25, height = 5.4375, units = "in")

# NAFO <- st_read("data/Divisions.shp")%>%
#   st_transform(latlong)%>%
#   select(ZONE,geometry)%>%
#   filter(!is.na(ZONE))
# 
# #Change plot by divisions 
# div_change <- mapdata %>% 
#   select(latitude, longitude, contains("delta")) %>% 
#   pivot_longer(cols = contains("delta"), "RCP")%>% 
#   st_as_sf(coords=c("longitude","latitude"),crs=latlong, remove = FALSE)%>%
#   st_join(.,NAFO,join=st_intersects)%>%
#   mutate(ZONE=as.character(ZONE))%>%  
#   st_set_geometry(NULL) %>% 
#   group_by(ZONE, RCP) %>%
#   summarise(mean_prob = mean(value))
#   
# 
# 
# 
# ggplot(div_change, aes(x = ZONE, y = mean_prob))+
#   geom_segment(aes(y=0, yend=mean_prob, x=ZONE, xend=ZONE), color="black") +
#   geom_hline(yintercept = 0, lty = 2)+
#   geom_point( color="#99CBBB", size=3.5) +
#   coord_flip()+
#   #scale_x_discrete(limits = rev(levels(ZONE)))+
#   facet_wrap(~RCP, labeller = as_labeller(c(delta_F1_R45 = "2046-2065 @ RCP 4.5", delta_F1_R85 ="2046-2065 @ RCP 8.5", delta_F2_R45 ="2066-2085 @ RCP 4.5", delta_F2_R85 ="2066-2085 @ RCP 8.5")))+
#   theme_bw() +
#   theme(
#     panel.grid.major.x = element_blank(),
#     axis.ticks.x = element_blank()
#   ) +
#   labs(x = "NAFO Division", y = expression(paste(Delta," Probability of Occurrence",sep="")))
# 
# ggsave("output/Figure_ChangeByDivision.png")
# 
# reg_change <- mapdata %>% 
#   select(latitude, longitude, contains("delta")) %>% 
#   pivot_longer(cols = contains("delta"), "RCP")%>% 
#   st_as_sf(coords=c("longitude","latitude"),crs=latlong, remove = FALSE)%>%
#   st_join(.,NAFO,join=st_intersects)%>%
#   mutate(ZONE=as.character(ZONE))%>%  
#   st_set_geometry(NULL) %>% 
#   group_by(region, RCP) %>%
#   summarise(mean_prob = mean(value))
# 
# 
# ggplot(reg_change, aes(x = region, y = mean_prob))+
#   geom_segment(aes(y=0, yend=mean_prob, x=region, xend=region), color="black") +
#   geom_hline(yintercept = 0, lty = 2)+
#   geom_point( color="#99CBBB", size=3.5) +
#   coord_flip()+
#   facet_wrap(~RCP, labeller = as_labeller(c(delta_F1_R45 = "2046-2065 @ RCP 4.5", delta_F1_R85 ="2046-2065 @ RCP 8.5", delta_F2_R45 ="2066-2085 @ RCP 4.5", delta_F2_R85 ="2066-2085 @ RCP 8.5")))+
#   theme_bw() +
#   theme(
#     panel.grid.major.x = element_blank(),
#     axis.ticks.x = element_blank()
#   ) +
#   labs(x = "Region", y = expression(paste(Delta," Probability of Occurrence",sep="")))
# 




##### Percent Change in Probability Plots ###############
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


# 
# #Add divisions by spatial points 
# pChangeOverall <- mapdata %>% 
#   select(latitude, longitude, contains("temp")) %>% 
#   rename(F1_R45 = btm_temp_F1_R45, F1_R85 = btm_temp_F1_R85, F2_R45 = btm_temp_F2_R45, F2_R85 = btm_temp_F2_R85 ) %>% 
#   pivot_longer(cols = contains("F"), "RCP")%>% 
#   st_as_sf(coords=c("longitude","latitude"),crs=latlong, remove = FALSE)%>%
#   st_join(.,NAFO,join=st_intersects)%>%
#   mutate(ZONE=as.character(ZONE))%>%  
#   st_set_geometry(NULL) %>% 
#   group_by(RCP) %>%
#   summarise(percent_change = ((mean(value)/mean(btm_temp_PC))-1) *100, ci = ((confidence_interval(value, 0.95)/confidence_interval(btm_temp_PC, 0.95)-1) *100), PCMean = mean(btm_temp_PC))
# 
# 
# 
# 
# pChangeRegion <- mapdata %>% 
#   select(latitude, longitude, contains("temp")) %>% 
#   rename(F1_R45 = btm_temp_F1_R45, F1_R85 = btm_temp_F1_R85, F2_R45 = btm_temp_F2_R45, F2_R85 = btm_temp_F2_R85 ) %>% 
#   pivot_longer(cols = contains("F"), "RCP")%>% 
#   st_as_sf(coords=c("longitude","latitude"),crs=latlong, remove = FALSE)%>%
#   st_join(.,NAFO,join=st_intersects)%>%
#   mutate(ZONE=as.character(ZONE))%>%  
#   st_set_geometry(NULL) %>% 
#   mutate(managmentRegion = ifelse(grepl("4S|4R|4T", ZONE), "GSL",
#                                   ifelse(grepl("4X|4W|4Vs|4Vn|3Ps|3O|3N", ZONE), "SS-GBs", 
#                                          ifelse(grepl("5Y|5Ze|5Zw", ZONE), "GM-GB", NA)))) %>% 
#   group_by(managmentRegion, RCP) %>%
#   summarise(percent_change = ((mean(value)/mean(btm_temp_PC))-1) *100, ci = ((confidence_interval(value, 0.95)/confidence_interval(btm_temp_PC, 0.95)-1) *100), PCMean = mean(btm_temp_PC))
# 
# 
# 
pChangeDivision <- mapdata2 %>%
  select(latitude, longitude, contains("temp")) %>%
  rename(F1_R45 = btm_temp_F1_R45, F1_R85 = btm_temp_F1_R85, F2_R45 = btm_temp_F2_R45, F2_R85 = btm_temp_F2_R85 ) %>%
 # select(-contains("delta")) %>% 
  pivot_longer(cols = contains("F"), "RCP")%>%
  st_as_sf(coords=c("longitude","latitude"),crs=latlong, remove = FALSE)%>%
  st_join(.,NAFO,join=st_intersects)%>%
  mutate(ZONE=as.character(ZONE))%>%
  st_set_geometry(NULL) %>%
  group_by(ZONE, RCP) %>%
  summarise(percent_change = ((mean(value)/mean(btm_temp_PC))-1) *100, ci = ((confidence_interval(value, 0.95)/confidence_interval(btm_temp_PC, 0.95)-1) *100), PCMean = mean(btm_temp_PC), valMean = mean(value))


# 
# ggplot(pChangeOverall)+
#   geom_point(aes(x = RCP, y = percent_change))+
#   geom_hline(aes(yintercept = 0), linetype = 2)+
#   geom_errorbar(aes(x = RCP, ymin = percent_change-ci, ymax = percent_change+ci))+
#   theme_bw()+
#   theme(axis.text.x=element_text(angle = 90, vjust = 0.5),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank())+
#   labs(x = "Climate Scenarios", y = paste0("Estimated %Change in Probability"))
# 
# 
# 
# 
# ggplot(pChangeRegion)+
#   geom_point(aes(x = managmentRegion, y = percent_change))+
#   facet_wrap(~RCP, ncol = 2)+
#   geom_hline(aes(yintercept = 0), linetype = 2)+
#   geom_errorbar(aes(x = managmentRegion, ymin = percent_change-ci, ymax = percent_change+ci))+
#   theme_bw()+
#   theme(axis.text.x=element_text(angle = 90, vjust = 0.5),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank())+
#   labs(x = "Managment Region", y = paste0("Estimated %Change in Probability"))
# 
# labeller = c('F1_R45' = 'RCP 4.5 (2046-2065)',
#              'F1_R85' = 'RCP 8.5 (2046-2065)',
#              'F2_R45' = 'RCP 4.5 (2066-2085)',
#              'F2_R85' = 'RCP 8.5 (2066-2085)')
# 
# ggplot(pChangeDivision)+
#   geom_point(aes(x = fct_rev(ZONE), y = percent_change))+
#   facet_wrap(~RCP, ncol = 2, labeller = as_labeller(c('F1_R45' = 'RCP 4.5 (2046-2065)',
#                                           'F1_R85' = 'RCP 8.5 (2046-2065)',
#                                           'F2_R45' = 'RCP 4.5 (2066-2085)',
#                                           'F2_R85' = 'RCP 8.5 (2066-2085)')))+
#   geom_hline(aes(yintercept = 0), linetype = 2)+
#   geom_errorbar(aes(x = ZONE, ymin = percent_change-ci, ymax = percent_change+ci))+
#   theme_bw()+
#   theme(axis.text.x=element_text(angle = 90, vjust = 0.5),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank())+
#   labs(x = "NAFO Division", y = paste0("Estimated %Change in Probability"))
# 
# 

###### Geom Text Percent Change Plots #######


# ggplot(pChangeRegion, aes(x = PCMean, percent_change))+
#   geom_text(aes(label=managmentRegion),hjust=0.5, vjust=0.5)+
#   geom_hline(yintercept = 0, linetype = 2)+
#   facet_wrap(~RCP)+
#   theme_bw()+
#   theme(
#     panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#     axis.text.x=element_text(hjust = 0.5))+
#   labs(x = "Current Probability of Occurence under Present Climate",
#        y = "Estimated %Change in Probability")+
#   facet_wrap(~RCP, labeller = as_labeller(c('F1_R45' = 'RCP 4.5 (2046-2065)',
#                                             'F1_R85' = 'RCP 8.5 (2046-2065)',
#                                             'F2_R45' = 'RCP 4.5 (2066-2085)',
#                                             'F2_R85' = 'RCP 8.5 (2066-2085)')))

ggplot(pChangeDivision, aes(x = PCMean, percent_change))+
  geom_text(aes(label=ZONE),hjust=0.5, vjust=0.5)+
  geom_hline(yintercept = 0, linetype = 2)+
  facet_wrap(~RCP, labeller = as_labeller(c('F1_R45' = 'RCP 4.5 (2046-2065)',
                                            'F1_R85' = 'RCP 8.5 (2046-2065)',
                                            'F2_R45' = 'RCP 4.5 (2066-2085)',
                                            'F2_R85' = 'RCP 8.5 (2066-2085)')))+
  theme_bw()+
  theme(
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.x=element_text(hjust = 0.5),
        strip.background = element_rect(fill="white"))+
  labs(x = "Probability of Occurence (Present Climate)",
       y = "Estimated %Change in Probability")


ggsave(file = "Figures/Fig7_season_av.tiff", dpi = 600, width = 7.25, height = 5.4375, units = "in")
ggsave(file = "Figures/Fig7_season_av.jpeg", dpi = 300, width = 7.25, height = 5.4375, units = "in")




tt <- pChangeDivision %>% 
  mutate(stock = ifelse(grepl("4S|4R", ZONE), "NG",
                         ifelse(grepl("4T", ZONE), "SG",
                                ifelse(grepl("4X|4W|4Vs|4Vn", ZONE), "SS", 
                                       ifelse(grepl("3Pn|3Ps|3O|3N|3L|3M|3K", ZONE), "NF",
                                              ifelse(grepl("5Y|5Ze|5Zw|6A|6B|6C|6D", ZONE),"USA", NA))))))


supp <- pChangeDivision %>% select(ZONE, RCP, percent_change) %>% 
  mutate(percent_change = round(percent_change, digits = 1)) %>% 
  pivot_wider(names_from = RCP, values_from = percent_change)


write.excel <- function(x,row.names=FALSE,col.names=TRUE,...) {
  write.table(x,"clipboard",sep="\t",row.names=row.names,col.names=col.names,...)
}

write.excel(supp)
