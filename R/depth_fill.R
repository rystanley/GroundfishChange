#load libraries
library(dplyr)
library(sf)
library(raster)

#map projection
latlong <- "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"

#Load in the Maritimes Data
load("data/RVData_Summarized_Feb_2021.RData")

#Load in the DEM that is a pretty good resolution
load("data/PredSpaceData.RData")
bathydat <- predSpace%>%dplyr::select("plon","plat","z")
colnames(bathydat) <- c("long","lat","depth")
coordinates(bathydat) <- ~long+lat
proj4string(bathydat) <- "+proj=utm +zone=20 +datum=NAD83 +units=km +no_defs +ellps=GRS80 +towgs84=0,0,0"
gridded(bathydat) <- TRUE
dem <- raster(bathydat)

gebco <- raster("data/gebco_2019_Canada.tif")

#fill in the coordinate 'holes'
rv <- RVdata%>%
      filter(!is.na(LATITUDE))%>% #filter null coordinates
      st_as_sf(coords=c("LONGITUDE","LATITUDE"),crs=latlong,remove=FALSE)%>%
      st_transform(proj4string(dem))%>%
      mutate(depth = extract(dem,as_Spatial(.)))%>%
      st_transform(proj4string(gebco))%>%
      mutate(depth2 = extract(gebco,as_Spatial(.)),
             depth_integrated = ifelse(is.na(DEPTH),ifelse(is.na(depth),depth2,depth),DEPTH))%>% #integrate. Take the value of depth from the RV first, then the high resolution, then the lowest resolution in GEBCO
      st_transform(latlong)
      

sum(is.na(rv$DEPTH))
sum(is.na(rv$depth))
sum(is.na(rv$depth2))
sum(is.na(rv$depth_integrated))

#save the output
write.csv(data.frame(rv)%>%dplyr::select(-geometry),"data/MaritimesDepthFilled.csv",row.names = F)

