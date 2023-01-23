setwd("~/School 2019-2020/Halibut") #change working directory (i don't think i need this)*
library(R.matlab)
library(tidyverse)
library(data.table)
######BOTTOM DATA#######

data = readMat('data/BNAM/BNAM_Tbtm_1990_2018.mat')

data = readMat('data/dTbtm_dSbtm_F_R45_2066-2085.mat')
# data is a list that consists of multiple matrices (the [1:1001, 1:1250] data) and 

# arrays (the [1:12, 1:1001, 1:1250] data). It cannot simply be exported as a single dataframe.
# Instead, the data must be subsetted and the different matrices and arrays extracted
# individually.  

## The following shows an extraction of the monthly data. 
## These fields are not anomalies, but are 'difference' fields that must be added to the
## present-day climatology data


################TEMPERATURE#################

###Covert matrices into lists

longitude = (data$nav.lon[1:801,1:401]) %>% as.list() 
latitude = (data$nav.lat[1:801,1:401]) %>% as.list() 
depth = (data$Bathy.depth[1:801,1:401]) %>% as.list()
land = (data$land.mask[1:801,1:401]) %>% as.list()
btmp_years_list = data[7:35] #Here I select the years from the list in the BNAM data

#Create empty lists to use in for loop below

year_df_list = rep(list(list()),29) #create an empty array with dimensiosn for each year (29 years)
month_list = rep(list(list()),12) #creating empty list for months



for(i in 1:length(btmp_years_list)){ #loop the number of years you'd like to create dataframes for (29)
  for(g in 1:12) #number of months
  {
    month_list[[g]] = as.list(btmp_years_list[[i]][g, 1:801, 1:401]) #takes the first element of each list (a matrix) and concerts it into a list within the ith element of month_list
    
  }
  
  
  # year_df_list[[i]] = setNames(do.call(rbind.data.frame, Map('c', longitude, latitude, depth, land, month_list[[1]], month_list[[2]], month_list[[3]], month_list[[4]], month_list[[5]], month_list[[6]], month_list[[7]], month_list[[8]], month_list[[9]], month_list[[10]], month_list[[11]], month_list[[12]])),c("Longitude","Latitude", "Depth", "If_land", paste("M", 1:12, sep = ""))) %>% mutate(Annual_AVG = rowMeans(select(.,M1:M12)), Winter_AVG = rowMeans(select(.,M1:M3)), Summer_AVG = rowMeans(select(.,M7:M9)) ) #creates a dataframe using the long, lat, depth, land and months for each year
  
  year_df_list[[i]] = setNames(do.call(rbind.data.frame, Map('c', longitude, latitude, depth, land, month_list[[1]], month_list[[2]], month_list[[3]], month_list[[4]], month_list[[5]], month_list[[6]], month_list[[7]], month_list[[8]], month_list[[9]], month_list[[10]], month_list[[11]], month_list[[12]])),c("Longitude","Latitude", "Depth", "If_land", paste("BTmp", 1:12, sep = ""))) %>% mutate(Annual_AVG = rowMeans(select(.,BTmp1:BTmp12)), Winter_AVG = rowMeans(select(.,BTmp1:BTmp3)), Spring_AVG = rowMeans(select(.,BTmp4:BTmp6)), Summer_AVG = rowMeans(select(.,BTmp7:BTmp9)), Fall_AVG = rowMeans(select(.,BTmp10:BTmp12))) #creates a dataframe using the long, lat, depth, land and months for each year
  
  #assigns a name for the newly create dataframe based on year (1989+i)
  nam = paste("BTmp_",  i + (data$yr0[1] - 1), sep = "")  
  assign(nam, year_df_list[[i]]) 
  fwrite(year_df_list[[i]], file = paste("BNAM_BTmp_",  i + (data$yr0[1] - 1), ".csv", sep = ""), row.names = FALSE) #write's the new dataframe as a csv with a unique year
}
