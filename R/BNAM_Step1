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

btmp_raw = list.files(pattern="*.csv") %>% 
  map_df(~fread(.))%>%
  data.frame()


#Add new column for year using rep() to differentiate between each of the 29 years
#Removing all rows that are land (land_mask = 0)
#Select columns we want to keep for analysis
#Roughly crop to desired NAFO zones long and lat
btmp = btmp_raw %>% 
  mutate(Year = rep(1990:2018,each= dim(btmp_raw)[1]/29)) %>% 
  filter(If_land != 0) %>% 
  filter(Longitude >= -73, Longitude <= -40 ,Latitude >= 38, Latitude <= 53)

save(btmp, file = "BNAM_Step1.RData")
