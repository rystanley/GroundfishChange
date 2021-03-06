#load libraries
library(dplyr)
library(sf)
library(raster)

#map projection
latlong <- "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"

#Load in the Maritimes Data
#load("data/RVData_Summarized_Feb_2021.RData")
load("data/RV_PAdata.RData")

RVdata <- rv_pata

#Load in the DEM that is a pretty good resolution
# load("data/PredSpaceData.RData")
# bathydat <- predSpace%>%dplyr::select("plon","plat","z")
# colnames(bathydat) <- c("long","lat","depth")
# coordinates(bathydat) <- ~long+lat
# proj4string(bathydat) <- "+proj=utm +zone=20 +datum=NAD83 +units=km +no_defs +ellps=GRS80 +towgs84=0,0,0"
# gridded(bathydat) <- TRUE
# dem <- raster(bathydat)

gebco <- raster("data/gebco_halibut_extent.tif")

#fill in the coordinate 'holes'
rv <- RVdata%>%
      filter(!is.na(latitude))%>% #filter null coordinates
      st_as_sf(coords=c("longitude","latitude"),crs=latlong,remove=FALSE)%>%
      st_transform(proj4string(gebco))

##there was an error with mutate that I can't really understand 
rv$depth_gebco <- extract(gebco,as_Spatial(rv))

rv <- rv%>%mutate(depth_integrated = ifelse(is.na(depth),depth_gebco,depth*-1))%>%
      st_transform(latlong)

#checks on the output
sum(is.na(rv$depth)) #the number of missing points
sum(is.na(rv$depth_gebco)) #should be zero
sum(rv$depth_integrated == rv$depth_gebco) #should be the same as the number of missing points

#compare the observed points to the ones extracted from the raster. 
ggplot(data=rv%>%filter(!is.na(depth)),aes(x=depth_integrated,y=depth_gebco))+
   geom_point()

mod=lm(depth_integrated~depth_gebco,data=rv%>%filter(!is.na(depth)))
summary(mod)#pretty good fit with a slop of very close to 1 to 1 with a good R squared. But you can see there are some big deviations, which is the issue of using a model 
#with a large extent. Likely the big depth differences are likely associated with canyons and shelf breaks. It might be advisable to delete the observations that have a gebco extractions 
#which are well beyond the depth range that you do have data for!

#Lets investigate where the big mismatches are

mismatches <- rv%>%
              filter(!is.na(depth))%>%
              mutate(depth = depth *-1,
                     mismatch = depth - depth_gebco)%>%
              filter(abs(mismatch)>100)

plot(gebco)
points(as_Spatial(mismatches),pch=20,cex=0.3) #most of the mismatches are in areas of steep slope.
              
   
#Filter data where GEBCO is used to assign a depth, but that depth is outside the general range of observations for the species. 
#here i used the 5th percentile. 
lower = quantile(rv$depth*-1,0.975,na.rm=T)%>%as.numeric() #very shallow cut off 
upper = quantile(rv$depth*-1,0.025,na.rm=T)%>%as.numeric() #very deep cut off

print(paste("depths between",lower,"and",upper,"m",sep=" "))

rv_filtered <- rv%>%
               filter(!is.na(depth)|is.na(depth)&depth_gebco<lower&depth_gebco>upper)

  
#save the output
write.csv(data.frame(rv)%>%dplyr::select(-geometry),"data/MaritimesDepthFilled.csv",row.names = F)
write.csv(data.frame(rv_filtered)%>%dplyr::select(-geometry),"data/MaritimesDepthFilled_filtered.csv",row.names=F)

