####
# Analysais of Anomalies from RV groundfish data
# Andrew czich
# May 6 2021
####
  #Atlantic Halibut Landings #Can be removed once next plots are made ----
  # Andrew Czich
  # March 25 2021
  
  #Load packages
  library(tidyverse)
  
  #Read in data
  landings <- read.csv('data/FAOHlaibutLandings_toplot.csv')
  
  #removing france from dataset
  landings <- landings %>% filter(Country != "France")
  
  #create a plot of landings over time by country (Canada and US)
 p1 <-  ggplot(landings,aes(x = Year, y = Landings))+
    geom_point(shape = 18, size = 2) +
    facet_wrap(~Country, scales = "free", ncol = 1) +
    geom_hline(data = landings %>% filter(Country == 'Canada'), aes(yintercept = mean(Landings)), lty = 2)+
    geom_hline(data = landings %>% filter(Country == 'USA'), aes(yintercept = mean(Landings)), lty = 2)+
    geom_line()+
    theme_bw()+
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          strip.background = element_rect(fill="white"))
  
 ggsave(file = "Figures/Fig2_white.tiff", dpi = 600, width = 7.25, height = 5.4375, units = "in")
 ggsave(file = "Figures/Fig2_white.jpeg", dpi = 300, width = 7.25, height = 5.4375, units = "in")
 
  #just canada
  
  ggplot(landings %>% filter(Country == "Canada"),aes(x = Year, y = Landings))+
    geom_point(shape = 18, size = 2) +
    geom_line() +
    geom_hline(aes(yintercept = mean(Landings)), lty = 2)+
    theme_bw()+
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
  
  
  
# end  ----  

  