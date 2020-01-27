## This is code that I (Ryan) used to generate the multispecies RV data. Instead of uploading all of the base
## data, and changing the file-paths, I have opted to just leave it as code (so I can go back to it later), 
## and will upload the Halibut_MultiRegionData.RData for plotting later. 

#load libraries -------
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggridges)
library(Mar.datawrangling)
library(lubridate)
library(sf)

#common projection -----
latlong <- "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"

#load data --------

#Newfoundland
load("R:/Science/CESD/HES_MPAGroup/Projects/SPERA/Biological Classification/John_OB/bioclassification/Data/NfldData.RData")

nl1 <- Nfld%>%
       select(year,month,day,latitude,longitude,depth,bott_temp,region,comm,totwgt)%>%
       filter(grepl(toupper("Hippoglossus"),comm))%>%
       mutate(species="Halibut")%>%
       select(-comm)

nl2 <- Nfld%>%
      select(year,month,day,latitude,longitude,depth,bott_temp,region,comm,totwgt)%>%
      mutate(ID = paste(year,month,day,latitude,longitude,sep="-"))%>%
      distinct(ID,.keep_all = T)%>%
      mutate(species="Set_hold", #these are just holders
             totwgt=NA)%>%
      select(names(nl1))
          
newfoundland <- rbind(nl1,nl2)%>%
                rename(temperature=bott_temp,
                       weight=totwgt)%>%
                mutate(region="newfoundland",
                       number=NA)

rm(Nfld,nl1,nl2)

save(newfoundland,file = "data/NLClean.RData")

#Quebec
QC <- readRDS("R:/Science/CESD/HES_MPAGroup/Projects/SPERA/Biological Classification/John_OB/bioclassification/Data/QC_invertsAdded.rds")

qc1 <- QC%>%
       select(year,month,day,DD_lat,DD_lon,depth,temp,taxa,sample_wgt_corr,catch_corr)%>%
       filter(grepl(tolower("Hippoglossus"),tolower(taxa)))%>%
       mutate(species="Halibut")%>%
       select(-taxa)

qc2 <- QC%>%
  select(year,month,day,DD_lat,DD_lon,depth,temp,taxa,sample_wgt_corr,catch_corr)%>%
  mutate(ID = paste(year,month,day,DD_lat,DD_lon,sep="-"))%>%
  distinct(ID,.keep_all = T)%>%
  mutate(species="Set_hold",
         sample_wgt_corr=NA,
         catch_corr=NA)%>%
  select(names(qc1))

quebec <- rbind(qc1,qc2)%>%
          rename(temperature=temp,
                 latitude=DD_lat,
                 longitude=DD_lon,
                 weight=sample_wgt_corr,
                 number=catch_corr)%>%
          mutate(longitude = longitude * -1,
                 region="quebec")%>%
          select(names(newfoundland))# so we can bring together later.

rm(QC,qc1,qc2)

save(quebec,file = "data/QCClean.RData")

#Gulf 
#GULF <- readRDS("R:/Science/CESD/HES_MPAGroup/Projects/SPERA/Biological Classification/John_OB/bioclassification/Data/Gulf_invertsAdded.rds")
  GulfSpecCodes <- read.csv("c:/Users/stanleyr/Documents/Github/GroundfishChange/data/Gulf_Maritimes species codes.csv",stringsAsFactors = F)

  GULF <- read.csv("data/sGSL_RV_RStanley_MAY2017.csv",stringsAsFactors = F)%>%
          rename(name=X_NAME_)%>%
          filter(name %in% c("biomass","catch"))%>%
          gather(names(.)[grep("X_",names(.))],key=species,value=var)%>%
          mutate(species=as.numeric(gsub("X_","",species)))%>%
          spread(key = name,value = var)%>%
          right_join(filter(GulfSpecCodes,species %in% unique(.$species)),by="species")

    glf1 <- GULF%>%
            select(year,month,day,latitude,longitude,depth,temperature,catch,biomass,latin_name)%>%
            filter(grepl(toupper("Hippoglossus"),latin_name),
                   catch>0 | biomass>0)%>%
            rename(species=latin_name)%>%
            mutate(species="Halibut")
            
    glf2 <- GULF%>%
      select(year,month,day,latitude,longitude,depth,temperature,catch,biomass,latin_name)%>%
      mutate(ID = paste(year,month,day,latitude,longitude,sep="-"))%>%
      distinct(ID,.keep_all = T)%>%
      rename(species=latin_name)%>%
      mutate(species="Set_hold",
             catch=NA,
             biomass=NA)%>%
      select(names(glf1))

    gulf <- rbind(glf1,glf2)%>%
      rename(weight=biomass,
             number=catch)%>%
      mutate(region="gulf")%>%
      select(names(newfoundland))# so we can bring together later.

    save(gulf,file = "data/GulfClean.RData")
    
#Maritimes
#Read in Species codes
  SpecCode <- read.csv("c:/Users/stanleyr/Documents/Github/Fundian/data/MergedSpeciesList.csv",stringsAsFactors = F)

#Read in RVdata 
  load("c:/Users/stanleyr/Documents/Github/Fundian/data/RVData.RData") ## 2018 pull from mardatawrangling
    RVdata <- summarize_catches()
    rm(GSCAT,GSDET,GSMISSIONS,GSINF,FGP_TOWS_NW2,ds_all) #remove data that has been aggregated by the summarize_catches() function

  #Get the year information 
  RVdata$YEAR <- year(RVdata$SDATE)
  RVdata$MONTH <- month(RVdata$SDATE)
  RVdata$DAY <- day(RVdata$SDATE)

  #Clean up missing stations
    RVdata[!complete.cases(RVdata$LATITUDE),c("LATITUDE","LONGITUDE")]
    RVdata[!complete.cases(RVdata$LONGITUDE),c("LATITUDE","LONGITUDE")]
    rownames(RVdata[!complete.cases(RVdata$LATITUDE),c("LATITUDE","LONGITUDE")]) == rownames(RVdata[!complete.cases(RVdata$LONGITUDE),c("LATITUDE","LONGITUDE")])
    RVdata <- RVdata[complete.cases(RVdata$LATITUDE),]
    
  #Clean up data
    names(RVdata) <- tolower(names(RVdata))
    
    Maritimes <- RVdata%>%
              filter(xtype==1,season=="SUMMER",year %in% 1970:2017,dur>19)%>%
              mutate(weight = totwgt*1.75/dist,
                     number = totno*1.75/dist)%>%
              rename(temperature = bottom_temperature)%>%
              select(year,month,day,latitude,longitude,depth,
                     temperature,number,weight,spec.gsspecies)%>%
              rename(species = spec.gsspecies)
    
    mar1 <- Maritimes%>%
      select(year,month,day,latitude,longitude,depth,temperature,number,weight,species)%>%
      filter(grepl(toupper("Hippoglossus"),species))%>%
      mutate(species="Halibut")
    
    mar2 <- Maritimes%>%
      select(year,month,day,latitude,longitude,depth,temperature,number,weight,species)%>%
      mutate(ID = paste(year,month,day,latitude,longitude,sep="-"))%>%
      distinct(ID,.keep_all = T)%>%
      mutate(species="Set_hold",
             catch=NA,
             biomass=NA)%>%
      select(names(mar1))
    
    maritimes <- rbind(mar1,mar2)%>%
      mutate(region="maritimes")%>%
      select(names(newfoundland))# so we can bring together later.
    
    save(maritimes,file = "data/MarClean.RData")
    
## Bring Data all together and assign spatial object ----------------
  halidat <- rbind(maritimes,gulf,quebec,newfoundland)%>%
             st_as_sf(coords=c("longitude","latitude"),crs=latlong)%>%
             mutate(species=ifelse(species=="Set_hold","Survey",species))

    
## Save output ----------------
  save(halidat,file = "data/Halibut_MultiRegionData.RData")
  save(maritimes,file = "data/MarClean.RData")
  
## Quick plots -------------------
  
  p1 <- ggplot(halidat,aes(x=depth,col=species))+
  stat_ecdf(geom="step",lwd=2)+
    facet_grid(region~.,scales="free_y")+
    theme_bw()+
    scale_x_log10()+
    annotation_logticks(sides="b")+
    labs(x="Depth (m)",y="ecdf",col="");p1
  
  p2 <- ggplot(filter(halidat,temperature <= max(filter(halidat,species=="Halibut")%>%pull(temperature),na.rm=T)),
                aes(x=temperature,col=species))+
    stat_ecdf(geom="step",lwd=1.5)+
    facet_grid(region~.,scales="free_y")+
    theme_bw()+
    labs(x=expression(paste("Temperature ",degree,"C",sep="")),y="ecdf",col="");p2

  ggsave("output/MultiRegion_HalibutDepth.png",p1,dpi=600,width=6,height=6,units="in")
  ggsave("output/MultiRegion_HalibutTemp.png",p2,dpi=600,width=6,height=6,units="in") 