###########################
#Cross Validation of SDMs
library(mgcv)#GAMs
#library(gratia) #for plotting GAMs in a ggplot2 fashion
library(tidyverse)
library(pROC)
library(pscl)


load('data/RVdata.Rdata')
load("data/early_model.R")
load("data/late_model.R")
load("data/BNAM_model.R")
load("data/OG_model.R")
load("data/final_model.R")
load("data/seasonModel.R")


AIC(og,final)

summary(final)
gam.check(final)



#Cross Validation by Time
earlyData <- rv %>% filter(year < 2000)
lateData <- rv %>% filter(year >= 2000)

predictionsLate <- predict(earlyModel2, newdata=lateData, type=c("response"))

aucLate <- roc(lateData$halibut_PA, predictionsLate, plot = T, legacy.axes = TRUE, xlab = "False Positive Rate", ylab = "True Positive Rate",
       col = "#377eb8", lwd = 2, print.auc = T)

#Cross Validation for BNAM_temps and depth

predictBNAM <- predict(bnamModel2, newdata=rv %>% rename(temp = btm_temp,Depth = depth), type=c("response"))

aucBNAM <- roc(rv$halibut_PA, predictBNAM, plot = T, legacy.axes = TRUE, xlab = "False Positive Rate", ylab = "True Positive Rate",
               col = "#377eb8", lwd = 2, print.auc = T)

#Concurvuty of the SDM was checked and the longitude and latitude tensor smooth was determined to have high concurvity with other 
#covariates in the model, meaning that this smooth could be well-approximated by some combination of other smooths in the model,
#Convurvity was investigated further to determine which other smooth are most strongly concurved with the latitude/longitude smooth.
#We found that a combination of depth and temperature could approximate lat/longitude smooth, however, it is no surprise, 
#and latitude and longitude are included to accoutn for spatial autocorrelation so it must remain as do the other as they are key factors in determining suitable habitat


#fit a new model for each region or survey
#Do a t-test to determine if any of the smooths are significantly different from one another

#have each model predict with BNAM current data and test if the predictions significantly differ from one another
#perhaps test models with anova

library(mgcv)#GAMs
library(gratia) #for plotting GAMs in a ggplot2 fashion
library(tidyverse)
library(pROC)

load('data/RVData.Rdata')

rv <- rv %>% mutate(season = factor(season),
                    region = factor(region))

# Compare predictive ability using five-fold CV
nfolds <- 10
case.folds <- rep(1:nfolds,length.out=nrow(rv))
# divide the cases as evenly as possible
case.folds <- sample(case.folds) # randomly permute the order
fold=1
aucList <- list()
# is in which position
for(fold in 1:nfolds) {
  # What are the training cases and what are the test cases?
  train <- rv[case.folds!=fold,]
  test <- rv[case.folds==fold,]
  # Fit to the training set
  trainModel <- gam(halibut_PA ~ te(longitude, latitude) +s(btm_temp, by = season, k =10)+ s(depth,by = region, k =10) + region + season, family = binomial,
                    data = train, method = "REML")
  
  
  # Predict on the test set
  predictions <- predict(trainModel, newdata=test, type=c("response"))
  # What's the mean-squared error?
  #fold.mses[fold,paste(bw)] <- mean((test$growth - predictions)^2)
  # Using paste() here lets us access the column with the right name...
  #ROC Accuracy Test
  tt <- roc(test$halibut_PA, predictions, plot = F, legacy.axes = TRUE, xlab = "False Positive Rate", ylab = "True Positive Rate",
            col = "#377eb8", lwd = 2, print.auc = F)
  aucList[fold] <- tt$auc
}

aucList %>% unlist() %>% mean()

save(aucList, file = "output/auc_kFoldCV_results_IModel.RData")
