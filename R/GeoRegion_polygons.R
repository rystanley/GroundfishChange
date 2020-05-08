#load packages
library(tidyverse) #for dyplyr and tidyr
library(sf)
## General Mapping information ----
#common projection 
latlong <- "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"

#read in NAFO division shape files
NAFO <- st_read("data/Divisions.shp")%>%
  st_transform(latlong)%>%
  select(ZONE,geometry)%>%
  filter(!is.na(ZONE))%>% 
  filter(grepl("[34]", ZONE))


NAFO$ZONE =  ifelse(grepl("4T|4S|4R", NAFO$ZONE), "GSL",
                         ifelse(grepl("4X|4W|4Vs|4Vn", NAFO$ZONE), "SS", 
                                ifelse(grepl("3Pn|3Ps|3O|3N|3L|3M|3K", NAFO$ZONE), "NF", NA)))

#Merged polygons within the same group (ZONE) into one (dissovles polygons)
GeoRegions <- NAFO %>% group_by(ZONE) %>% summarise()

save(GeoRegions, file = "data/GeoRegions.RData")

