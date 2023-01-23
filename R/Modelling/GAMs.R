library(mgcv)#GAMs
library(gratia) #for plotting GAMs in a ggplot2 fashion
library(tidyverse)
library(pROC)

#Model tests
library(lmtest)
library(pscl)
library(DHARMa)
library(scales)

set.seed(1997)

load('data/RVData.Rdata')
##### Determine Range of Covariates where halibut are present
rv %>% filter(halibut_PA == 1) %>% select(year, month, day, latitude, longitude, depth, btm_temp)%>% summary()
rv %>% filter(halibut_PA == 0) %>% select(year, month, day, latitude, longitude, depth, btm_temp)%>% summary()
rv %>% group_by(region, halibut_PA) %>% count()
rv %>% group_by(season, halibut_PA) %>% count()

############## K-fold Crossvalidation ###################
library(caret)
library(gamclass)

index <- createDataPartition(rv$halibut_PA, p=0.8, list = FALSE, times = 1) 

#########Year Partitioned Data ###############

earlyData <- rv %>% filter(year < 2012)
lateData <- rv %>% filter(year >= 2012)


earlyModel <- gam(halibut_PA ~ te(longitude, latitude) + s(btm_temp) + s(depth) + region + season, family = binomial,
                  data = earlyData, method = "REML")

earlyModel2 <- gam(halibut_PA ~ te(longitude, latitude) +s(btm_temp, by = season, k =10)+ s(depth,by = region, k =10) + region + season, family = binomial,
                   data = earlyData, method = "REML")

save(earlyModel2, file = "data/early_model2.R")

lateModel <- gam(halibut_PA ~ te(longitude, latitude) + s(btm_temp) + s(depth) + region + season, family = binomial,
                  data = lateData, method = "REML")


save(lateModel, file = "data/late_model.R")


#### Model with BNAM temps instead of RV temps ####

library(s2)
library(tidyverse)


#load('data/MaritimesDepthFilled.Rdata')
load('data/BNAM_Step1.RData') #load in bnam temperature data

set1 <- rv
set2 <- btmp %>% filter(Year == 1990) %>% mutate(id = row_number())

#Get long and latitude from each dataset and match up the nearest points
rv_s2 <- s2_lnglat(set1$longitude, set1$latitude)
bnam_s2 <- s2_lnglat(set2$Longitude, set2$Latitude)
set1$closest <- s2_closest_feature(rv_s2, bnam_s2)
set1


bnamTemp4RV <- btmp %>% select(!contains('AVG')) %>% pivot_longer(cols = matches('[[:digit:]]'), names_to = "month", values_to = "temp") %>% 
    mutate(month = str_extract(month, '[:digit:]+'),
           month = as.integer(month)) %>% left_join(set2 %>% select(Longitude, Latitude, Depth, id )) %>% 
    right_join(set1, by = c("id" = "closest", "month" = "month", "Year" = "year"))



bnamModel2 <- gam(halibut_PA ~ te(longitude, latitude) +s(temp, by = season, k =10)+ s(Depth,by = region, k =10) + region + season, family = binomial,
                  data = bnamTemp4RV, method = "REML")

save(bnamModel2, file = "data/BNAM_model2.R")

####
#Final Model used in predictions
#seasonModel <- gam(halibut_PA ~ te(longitude, latitude) + s(btm_temp) + s(depth) + region + season, family = binomial,
#                   data = rv, method = "REML")

sumTable <- summary(seasonModel)
write.table(sumTable, file = "output/sumTable.txt", sep = ",", quote = FALSE, row.names = F)
gamC <- gam.check(seasonModel)[1]


p <- plot(I, scheme = 2, trans = plogis, pages = 0,
          shift = plogis(coef(I)[1]),
          seWithMean = TRUE,
          shade = TRUE, shade.col = "lightblue")


#Determine preferred temperature range
probs <- plogis(p[[2]]$fit)[1:100] #extract probabilities from partial smooth temp
temp <- p[[2]]$x #extract temperatures in partial smooth
partialTemp <- data.frame(temp = temp,prob = probs) #combine into data frame
meanProb <- mean(partialTemp$prob) #find the mean probability
partialTemp %>% mutate(distance = abs(meanProb-prob)) %>% arrange(distance) %>% head(10) #print the the closest points to the mean
#mean prob = 0.498
#range = 2.4 to 18.98

#Determine preferred depth lower range
probs <- plogis(p[[3]]$fit)[1:100] #extract probabilities from partial smooth temp
temp <- p[[3]]$x #extract temperatures in partial smooth
partialTemp <- data.frame(temp = temp,prob = probs) #combine into data frame
meanProb <- mean(partialTemp$prob) #find the mean probability
partialTemp %>% mutate(distance = abs(meanProb-prob)) %>% arrange(distance) %>% head(10) #print the the closest points to the mean
#mean prob 0.229
#701.5 upper range




roc(rv$halibut_PA, seasonModel$fitted.values, plot = TRUE, legacy.axes = TRUE, xlab = "False Positive Rate", ylab = "True Positive Rate",
    col = "#377eb8", lwd = 2, print.auc = TRUE) #this way does the same thing in one step compared to 3

#To compare other plots, but only comparing to itself now.
plot.roc(rv$halibut_PA, seasonModel$fitted.values, add = TRUE, print.auc = TRUE, print.auc.y = .4,
         col = "#4daf4a", lwd = 2)


summary(seasonModel)


AIC(seasonModel)
#BIC(m1,m2,m3,m4,m5, m6, m7, m8)


#logLik.gam(m7)
#logLik.gam(m8)
#gam.check(m5)
#gam.check(m6)
load("data/I_model.RData")

 #par(pty = s) #this removes padding on either side of the plot
#Checking VIF and Concurvity

concurvity(I_tempRegion, full = TRUE) #not sure why long and lat are so high here
options(scipen = 999)
concTable <- concurvity(I_tempRegion, full = FALSE) #but then are not high in this one
write.csv(concTable, file = "output/concTable.csv", row.names = F)


rv <- rv %>% mutate(season = factor(season),
                    region = factor(region))

I_tempRegion <- gam(halibut_PA ~ te(longitude, latitude) + s(btm_temp, by = region, k =10) +s(btm_temp, by = season, k =10)+ s(depth,by = region, k =10) + region + season, family = binomial,
                    data = rv, method = "REML")

gam(halibut_PA ~ te(longitude, latitude) +s(btm_temp, by = season, k =10)+ s(depth,by = region, k =10) + region + season, family = binomial,
    data = rv, method = "REML")


m1 <- gam(halibut_PA ~ te(longitude, latitude) + s(btm_temp, k =20) + s(depth, k =10) + region + season, family = binomial,
                    data = rv, method = "REML")
m2 <- gam(halibut_PA ~ te(longitude, latitude) + s(btm_temp, k =20) + s(depth, k =20) + region + season, family = binomial,
          data = rv, method = "REML")
m3 <- gam(halibut_PA ~ te(longitude, latitude) + s(btm_temp, k =10) + s(depth, k =8) + region + season, family = binomial,
          data = rv, method = "REML")
m4 <- gam(halibut_PA ~ te(longitude, latitude) + s(btm_temp, k =5) + s(depth, k =5) + region + season, family = binomial,
          data = rv, method = "REML")
m5 <- gam(halibut_PA ~ te(longitude, latitude, k = 50) + s(btm_temp, k =5) + s(depth, k =5) + region + season, family = binomial,
          data = rv, method = "REML")

plot(I_tempRegion, scheme = 2, trans = plogis, pages = 0,
     shift = plogis(coef(I_tempRegion)[1]),
     seWithMean = TRUE,
     shade = TRUE, shade.col = "lightblue")



predict1 <- predict(I, newdata=rv, type=c("response"), se.fit = T)

predict2 <- predict(I, newdata=rv, type=c("link"), se.fit = T)
predict2 <- predict(I_tempRegion, newdata=rv, type=c("response"))
predict3 <- predict(m3, newdata=rv , type=c("response"))
predict4 <- predict(m4, newdata=rv , type=c("response"))

auc <- roc(rv$halibut_PA, predict1, plot = T, legacy.axes = TRUE, xlab = "False Positive Rate", ylab = "True Positive Rate",
               col = "#377eb8", lwd = 2, print.auc = T)

plot.roc(rv$halibut_PA, predict2, add = TRUE, print.auc = TRUE, print.auc.y = .4,
         col = "#4daf4a", lwd = 2)

##Finding Most Important Variable
IO_ll <- gam(halibut_PA ~ s(btm_temp, by = season, k =10)+ s(depth,by = region, k =10) + region + season, family = binomial,
                    data = rv, method = "REML")


IO_temp <- gam(halibut_PA ~ te(longitude, latitude) + s(depth,by = region, k =10) + region + season, family = binomial,
                    data = rv, method = "REML")


IO_depth <- gam(halibut_PA ~ te(longitude, latitude) +s(btm_temp, by = season, k =10) + region + season, family = binomial,
                    data = rv, method = "REML")



IO_region2 <- gam(halibut_PA ~ te(longitude, latitude) +s(btm_temp, by = season, k =10)+ s(depth, by = region, k =10) + season, family = binomial,
                    data = rv, method = "REML")


IO_season2 <- gam(halibut_PA ~ te(longitude, latitude) +s(btm_temp,by = season, k =10)+ s(depth,by = region, k =10) + region, family = binomial,
                    data = rv, method = "REML")


AIC(I, IO_ll, IO_temp, IO_depth, IO_region, IO_season, IO_season2)


plot(evaluate(presence, absence, I), 'ROC') # I .853
plot(evaluate(presence, absence, IO_ll), 'ROC') # ll .825
plot(evaluate(presence, absence, IO_temp), 'ROC') # temp .832
plot(evaluate(presence, absence, IO_depth), 'ROC') # depth .835
plot(evaluate(presence, absence, IO_region), 'ROC') #region .844
plot(evaluate(presence, absence, IO_season), 'ROC') #season 0.85

####Finding Threshold Value ########
library(dismo)
load("forecastBNAM.Rdata")

presence <- rv %>% filter(halibut_PA == 1)
absence <- rv %>% filter(halibut_PA == 0) 
load("data/I_model.RData")
eval <- evaluate(presence, absence, I)
plot(eval, 'ROC')

pedictions <- uncertaintyData[1:2] %>% mutate(pred = unPred$fit)

source("R/Other/make_map_ac.R")

make_map(pedictions)

tr <- plogis(threshold(eval, stat = "sensitivity"))
mean(pedictions$pred)
#kappa = 0.165
#spec_sens = 0.0673
# prevalence = 0.479
#equal sense spec = 0.085
make_map(pedictions %>% mutate(pred = ifelse(pred > 0.165, 1, 0)), fill_name = "Max Kappa", rasterFun = max, lat.lim = c(39,52))


pedictions %>% mutate(pred = ifelse(pred > 0.165, 1, 0)) %>% group_by(longitude, latitude) %>% mutate(prob = ifelse(any(pred == 1), 1, 0))
                      