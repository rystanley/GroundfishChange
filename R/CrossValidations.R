library(mgcv)#GAMs
library(gratia) #for plotting GAMs in a ggplot2 fashion
library(tidyverse)
library(pROC)

load('data/RVData.Rdata')


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
    trainModel <- gam(halibut_PA ~ te(longitude, latitude) + s(btm_temp) + s(depth) + region + season, family = binomial,
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
