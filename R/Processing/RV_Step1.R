#Load libraries
library(tidyverse)
library(lubridate)
#Read in Rv data
load("data/groundfish_surveys/RVMar.RData")
load("data/groundfish_surveys/GulfRV.RData")
##load("data/groundfish_surveys/NLRV.RData")
load("data/groundfish_surveys/NF_RVSurvey.RData")
##load("data/groundfish_surveys/QCRV.RData")
load("data/GSL_RVTrawl.RData")
usa <- read.csv("data/groundfish_surveys/HalibutPAdata.csv")

#Small little test to show raw data and depths missing
tt <- Maritimes %>% filter(species == 'HIPPOGLOSSUS HIPPOGLOSSUS')
tt %>% count()
tt %>% filter(!is.na(depth)) %>% count()




#Create Presence Data Frame with all regions

#newfoundland data extract important variable and rename to common naming converntion
nfl <- nfdata2 %>% filter(Weight != 0 | Number != 0) %>% 
  rename(btm_temp = temp, depth = DepthMean, latitude = DDLat, longitude = DDLon, presence = halibut_PA, 
         totwgt = Weight, count = Number, speciesCode = Species) %>%
  select(year, month, day, latitude, longitude, btm_temp, depth, totwgt, count, presence) %>% 
  mutate(species = str_to_upper(ifelse(presence == 1, "Hippoglossus Hippoglossus", "Other")), 
         region = "NF",
         survey = case_when(between(month, 3,8)~"NF-Sp",
                            month > 8 | month < 3 ~ "NF-F"))

tt <- usa

ggplot(tt)+
  geom_histogram(aes(x = Month,), position = "stack")
# 
# #newfoundland data extract important variable and rename to common naming converntion
# nfl11 <- nl1 %>% filter(totwgt != 0) %>% 
#   rename(btm_temp = bott_temp) %>%
#   select(year, month, day, latitude, longitude, btm_temp, depth, region, species, totwgt, count) %>% 
#   mutate(presence = ifelse(species == "Hippoglossus hippoglossus" , 1, 0), 
#          species = str_to_upper(species), 
#          region = "NF")

  
#gulf data extract important variables and rename to common naming convention
gulf <- GULF %>% 
  filter(biomass > 0, catch > 0) %>% #*** change to | or
  select(-species) %>% 
  rename(species = latin_name, btm_temp = temperature) %>%
  select(year, month, day, latitude, longitude, btm_temp, depth, region, species, biomass, catch) %>% 
  mutate(presence = ifelse(species == "HIPPOGLOSSUS HIPPOGLOSSUS" , 1, 0), 
         region = "SG",
         survey = "SG")


#quebec data processing
# qc <- qc1 %>% 
#   rename(species = taxa, btm_temp = bott_temp) %>%
#   select(year, month, day, latitude, longitude, btm_temp, depth, region, species, totwgt, count) %>% 
#   mutate(species = str_to_upper(species)) %>% 
#   mutate(presence = ifelse(species == "HIPPOGLOSSUS HIPPOGLOSSUS" , 1, 0), 
#          region = "NG",
#          longitude = ifelse(longitude > 0, longitude*-1, longitude)) 

ng_data <- read.csv("data/GSL_RVTrawl.csv")

#Northern Gulf
#Convert date into 3 columns
ng_data1 <-
  ng_data %>% mutate(month = month(date), day = day(date)) %>%
  select(
    year = YEAR,
    month,
    day,
    latitude = LATITUDE,
    longitude = LONGITUDE,
    species = SPECIE_SCIENTIFIC_NAME,
    btm_temp = TEMPERATURE,
    depth = DEPTH,
    totwgt = WEIGHT,
    count = number
  ) %>%
  mutate(presence = ifelse(species == "Hippoglossus hippoglossus" & count > 0 , 1, 0),
         region = "NG")

qc <- ng_data1 %>% filter(!is.na(btm_temp)) %>% mutate(species = str_to_upper(species)) %>% 
  filter(!(count == 0 & species == "HIPPOGLOSSUS HIPPOGLOSSUS")) %>% 
  mutate(survey = "NG")


#maritime data, adding presence and region varaibles
mari <- Maritimes %>%  filter(!(totwgt == 0 & count == 0)) %>%
  rename(btm_temp = temperature) %>%
  mutate(presence = ifelse(species == "HIPPOGLOSSUS HIPPOGLOSSUS" , 1, 0),
         region = "SS",
         survey = case_when(between(month, 6, 10) ~"SS-Su",
                            month < 6 | month > 10 ~ "SS-W"))

#USA data
#Here are the data.
#Depth is in m, lat and lon are in decimal degrees, weight is in kg. 
#If there is a 101 in the Species column, you have halibut catch and weight and number will 
#be reported. Otherwise you will see NA in all three columns, meaning no halibut for that tow. 
#I removed all non-random stations, all tows that showed gear problems, and limited this to bottom 
#trawl surveys only.
usa_clean <- usa %>% 
  rename_with(.fn = tolower) %>% rename(longitude = lon, latitude = lat, btm_temp = temp, totwgt = weight) %>% 
  mutate(presence = ifelse(is.na(species), 0, 1)) %>% 
  mutate(region = "USA", species = ifelse(species == 101, "HIPPOGLOSSUS HIPPOGLOSSUS", NA),
         survey = case_when(between(month, 7,12) ~ "USA-F",
                            between(month, 1,6) ~ "USA-Sp"))
  


#combine all data frames to one
rv <- bind_rows(mari,gulf,qc,nfl, usa_clean)

#Check the data 
summary(rv)
rv <- rv %>% filter(!(is.na(btm_temp) & is.na(depth)), btm_temp < 30) #remove outlier in temp

#Removing false absence data
#True data with only one observation per lat, lon year month and day (keep all)
rv_1 <- rv %>% group_by(latitude, longitude, year, month, day) %>%  filter(n() == 1) %>% #US data doesn't contain other species
  mutate(halibut_PA = ifelse(str_detect(species, "HIPPOGLOSSUS HIPPOGLOSSUS"), 1, 0),
         cod_PA = ifelse(any(str_detect(species, "GADUS MORHUA")), 1, 0),
         redfish_PA = ifelse(any(str_detect(species, "SEBASTES")), 1, 0),
         pollock_PA = ifelse(any(str_detect(species, "POLLACHIUS VIRENS")), 1, 0),
         flounders_PA = ifelse(any(str_detect(species, "HIPPOGLOSSOIDES")), 1, 0),
         plaice_PA = ifelse(any(str_detect(species, "HIPPOGLOSSOIDES PLATESSOIDES")), 1, 0),
         hermit_PA = ifelse(any(str_detect(species, "PAGUR")), 1, 0),
         shrimp_PA = ifelse(any(str_detect(species, "PANDAL")), 1, 0),
         capelin_PA = ifelse(any(str_detect(species, "MALLOTUS VILLOSUS")), 1, 0),
         sculpin_PA = ifelse(any(str_detect(species, "MYOXOCEPHALUS")), 1, 0) ,
         sandlance_PA = ifelse(any(str_detect(species, "AMMODYT")), 1, 0)) %>% 
  mutate(halibut_PA = replace_na(halibut_PA, 0)) %>% 
  select(-presence)
  

#here I process the data so we dont have 50 absences and one presence in a single trawl. Realistically it should just be 1 presence row

#this is old and could skew the data a bit
# rv_dup <- rv %>%
#   group_by(latitude, longitude, year, month, day) %>% #group by variables to see if multiple species are caught at same date, time and location
#   filter(n() > 1) %>%   #filter if there are multiple occurrences of the same date,time and location
#   mutate(row_num = row_number(), #add a column for counting the number in each group (to use for filtering later)
#          true_P = ifelse(species == "HIPPOGLOSSUS HIPPOGLOSSUS", 1, #if halibut was in catch give it value 1
#          ifelse(!any(species == "HIPPOGLOSSUS HIPPOGLOSSUS") & row_num == 1, 0, NA))) %>% #if not, give first element in group value 0
#   ungroup() %>%
#   filter(!is.na(true_P)) %>% #keep only 1 PA observation for each unique year, month, day, long, lat group (each unqiue trawl)
#   select(-c(row_num, presence)) %>%
#   rename(presence = true_P)



rv_dup2 <- rv %>% 
  group_by(latitude, longitude, year, month, day) %>% #group by variables to see if multiple species are caught at same date, time and location
  filter(n() > 1) %>%    #filter if there are multiple occurrences of the same date,time and location
  mutate(halibut_PA = ifelse(str_detect(species, "HIPPOGLOSSUS HIPPOGLOSSUS"), 1, NA),
         cod_PA = ifelse(any(str_detect(species, "GADUS MORHUA")), 1, 0),
         redfish_PA = ifelse(any(str_detect(species, "SEBASTES")), 1, 0),
         pollock_PA = ifelse(any(str_detect(species, "POLLACHIUS VIRENS")), 1, 0),
         flounders_PA = ifelse(any(str_detect(species, "HIPPOGLOSSOIDES")), 1, 0),
         plaice_PA = ifelse(any(str_detect(species, "HIPPOGLOSSOIDES PLATESSOIDES")), 1, 0),
         hermit_PA = ifelse(any(str_detect(species, "PAGUR")), 1, 0),
         shrimp_PA = ifelse(any(str_detect(species, "PANDAL")), 1, 0),
         capelin_PA = ifelse(any(str_detect(species, "MALLOTUS VILLOSUS")), 1, 0),
         sculpin_PA = ifelse(any(str_detect(species, "MYOXOCEPHALUS")), 1, 0) ,
         sandlance_PA = ifelse(any(str_detect(species, "AMMODYT")), 1, 0),
         ) %>% 
  group_by(halibut_PA, .add = TRUE) %>% 
  mutate(id = row_number()) %>% #add a column for counting the number in each group (to use for filtering later)
  ungroup(halibut_PA) %>%
  mutate(halibut_PA = ifelse(str_detect(species, "HIPPOGLOSSUS HIPPOGLOSSUS", negate = TRUE) & id == 1, 0, 
                      ifelse(halibut_PA == 1, 1, NA))) %>%  #if not, give first element in group value 0
  ungroup() %>% 
  filter(!is.na(halibut_PA)) %>% #keep only 1 PA observation for each unique year, month, day, long, lat group (each unique trawl)
  select(-c(id, presence))


#combine data
rv_pata <- bind_rows(rv_1, rv_dup2)
summary(rv_pata)
rv_pata$region <- factor(rv_pata$region)

#save final data frame
save(rv_pata,file = "data/groundfish_surveys/RV_PAdata.RData")



rv_final <- read.csv("data/MaritimesDepthFilled_filtered.csv")

tableData <- rv_final %>% filter(!is.na(depth), !is.na(month))

RV_table <- tableData %>% group_by(region) %>% summarise(total = n(), halibut = sum(halibut_PA), 
                                           percent = (halibut/total)*100, min_depth = min(depth), max_depth = max(depth),
                                           min_month = min(month), max_month = max(month),
                                           start_year = min(year), end_year = max(year)) %>% select(-halibut)

write.csv(RV_table, file = "data/groundfish_surveys/RVsummary_table.csv", row.names = FALSE)


RV_table2 <- tableData %>% group_by(region) %>% summarise(total = n(), halibut = sum(halibut_PA), 
                                                         percent = (halibut/total)*100, min_depth = quantile(depth, 0.05), max_depth = quantile(depth, 0.95),
                                                         min_month = min(month), max_month = max(month),
                                                         start_year = min(year), end_year = max(year)) %>% select(-halibut)

