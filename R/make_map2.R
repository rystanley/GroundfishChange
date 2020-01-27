make_map <- function(xyz,resolution=10,rasterFun=mean,facet=NULL,bathy=FALSE,NAFO=NULL,
                     variable="",trans=NULL,facetorder=NULL,ncol=2,buffer=NULL,
                     long.lim = NULL,lat.lim=NULL,
                     lims = NULL){
  
  #Specify variables ------------
  
  #Plot returns a ggplot object
  #xyz - is data with longitude, latitude, and the z variable of interest
  #resolution - is minimum grid cell resolution in km2 (default is 10)
  #rasterFun - function for the rasterize function -- see ?raster::rasterize for details 
  #facet - variable to facet the data by (this is a grouping variable) (default is NULL)
  #bathy - logical (default: FALSE) specifying whether you want a bathymetric contour line
  #NAFO - is a NAFO sf object (default is NULL)
  #variable - legend title for "z" variable
  #trans - variable specifying if any transfomration should be applied to the fill scale (default is no transformation (NULL) and the other option is 'log')
  #ncol - is the number of columns  for a facet - default is 2
  #buffer - this is a small buffer that can be used to trim out odd data along the borders of the plot. Units are decimal degrees.
  #long.lim - forced longitude extent
  #lat.lim - forced latitude extent
  #lims - is a transformation range applied to the data where values above and below the limts are assigned the upper and lower limit values
 
  #load required libraries -----------
  
  require(shape)
  require(ggplot2)
  require(mapdata)
  require(marmap)
  require(maptools)
  require(dplyr)
  require(sf)
  require(rnaturalearth)
  
  #Function to take xyz and create gridded raster data -------------
  rasFun <- function(x,...){
    
    out <- x%>%
      st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%
      st_transform(planar)%>%
      st_coordinates()%>%
      raster::rasterize(.,grid,field=xyz[,3],fun=rasterFun)%>%
      raster::projectRaster(.,crs=latlong)%>%
      raster::rasterToPolygons(.)%>% # this part is slow
      st_as_sf()%>%
      rename(MAP = layer)
    
    return(out)
    
  }
  
  #Projections ------------
  latlong <- "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"
  planar <- "+proj=utm +zone=20 +datum=NAD83 +units=km +no_defs +ellps=GRS80 +towgs84=0,0,0"
  
  #Transformation --------------
  
  names(xyz)[3] <- "z" # just to standardize the code. XYZ from the function variable will inherit the workspace names. 
  
  if(!is.null(lims)){
    
    if(lims[1]>min(xyz[,3],na.rm=T) | lims[2]<max(xyz[,3],na.rm=T)){
    
      rng <- round(range(xyz[,3]),2)
      
      #Message stating what is happening
      writeLines(paste0("Transforming data range from (",rng[1],",",rng[2],") to (",lims[1],",",lims[2],")"))
      
      xyz <- xyz%>%mutate(z = ifelse(z<lims[1],lims[1],
                           ifelse(z>lims[2],lims[2],z)))
    
    }
    
  }
  
  # Make raster grid ----------------
  
  grid <- xyz%>%
    st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%
    st_transform(planar)%>% #make in planar coordinates so you can set the resolution
    raster::extent(.)%>%
    raster::raster(.)
  
  raster::res(grid) <- resolution #scale of the regular grid
  proj4string(grid) <- planar
  
  #Rasterize the data  -----------
  
  #rasterize - no facet
  if(is.null(facet)){
    ras <- rasFun(xyz)
  }
  
  #rasterize - with facet grouping variable
  if(!is.null(facet)){
    ras <- xyz%>%
      rename(Longitude = 1, Latitude = 2, Z = 3)%>%
      mutate(GROUP=carfacet)%>%
      group_by(GROUP)%>%
      do(rasFun(.))%>%
      ungroup()
    
    #set order of the facets if facets are there and an order is specified
    if(!is.null(facetorder)){ras$GROUP <- factor(ras$GROUP,levels=facetorder)}
    
  }
  
  #Set plotting extent ------------
  rextent <- xyz%>%
    st_as_sf(coords=c("Longitude","Latitude"),crs=latlong)%>%
    raster::extent(.)
  
  if(!is.null(long.lim)){Long.lim=long.lim}else{Long.lim  <-  c(rextent[1], rextent[2])}
  if(!is.null(lat.lim)){Long.lim=lat.lim}else{Lat.lim <-  c(rextent[3], rextent[4])}

  #if buffer is required  
    if(!is.null(buffer)){
      
      Long.lim[1] <- Long.lim[1]+buffer
      Long.lim[2] <- Long.lim[2]-buffer
      Lat.lim[1] <- Lat.lim[1]+buffer
      Lat.lim[2] <- Lat.lim[2]-buffer

    }
  
  #Download bathymetry -------------
  
  #Extract bathymetric data and store it in a folder called 'data' in your current working director
  #This will make later extractions faster. Data converted to tabular ggplot using fortify
  
  if(bathy){
    if(!dir.exists("data")){dir.create("data/")} #create a data directory if it doesn't exist
    curdir <- getwd()
    setwd("data/")
    bathy <- getNOAA.bathy(Long.lim[1],Long.lim[2],Lat.lim[1],Lat.lim[2],res=1,keep=T)%>%fortify()
    setwd(curdir)
  }
  
  #get mapping baselayers ----------
  
   Canada <- ne_states(country = "Canada",returnclass = "sf")%>%
    # st_transform(latlong)%>%
    # filter(name %in% c("Nunavut","Ontario","Québec","Newfoundland and Labrador",
    #                    "Nova Scotia","Prince Edward Island","New Brunswick"))%>%
    select(latitude,longitude,geonunit,geometry)%>%
    st_union()%>% #group provinces + territories
    st_as_sf()
  
  USA <- ne_states(country = "United States of America",returnclass = "sf")%>%
    st_transform(latlong)%>%
    filter(region%in%c("Northeast","South"))%>%
    select(latitude,longitude,geonunit,geometry)%>%
    st_union()%>% #group states
    st_as_sf()
  
  #Make the plot ----------
  if(is.null(NAFO)){
  p1 <- ggplot() +
    geom_sf(data=ras,aes(fill=MAP),colour=NA)+
    geom_sf(data = Canada) + 
    geom_sf(data = USA) + 
    coord_sf(xlim = Long.lim,  ylim = Lat.lim, expand=FALSE)+
    theme_bw()+
    theme(legend.position = "bottom",
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = "white", colour = "black"),
          plot.background = element_rect(colour = "white"),
          strip.background = element_rect(colour = "black", fill = "white"))+
    labs(x=expression(paste("Longitude ",degree,"W",sep="")),
         y=expression(paste("Latitude ",degree,"N",sep="")))
  }
  
  if(!is.null(NAFO)){
    p1 <- ggplot() +
      geom_sf(data=ras,aes(fill=MAP),colour=NA)+
      geom_sf(data = NAFO, colour = "black", fill = NA)+
      geom_sf_text(data=NAFO_poly,aes(label=ZONE),colour="white")+
      geom_sf(data = Canada) + 
      geom_sf(data = USA) + 
      coord_sf(xlim = Long.lim,  ylim = Lat.lim, expand=FALSE)+
      theme_bw()+
      theme(legend.position = "bottom",
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "white", colour = "black"),
            plot.background = element_rect(colour = "white"),
            strip.background = element_rect(colour = "black", fill = "white"))+
      labs(x=expression(paste("Longitude ",degree,"W",sep="")),
           y=expression(paste("Latitude ",degree,"N",sep="")))
    
  }
  
  #apply bathymetric contour (hard coded at 200 m for the ~shelf break)   
  if(bathy){p1 <- p1+geom_contour(data=bathy,aes(x=x,y=y,z=z),breaks=c(-200),lwd=0.05,colour="grey20")}
  
  #Apply facet
  if(!is.null(facet)){p1=p1+facet_wrap(~GROUP,ncol=ncol)} 
  
  #return the ggplot object back --------------
  return(p1)
  
}