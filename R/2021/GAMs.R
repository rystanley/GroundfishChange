library(mgcv)#GAMs
library(gratia) #for plotting GAMs in a ggplot2 fashion
library(tidyverse)

#Model tests
library(lmtest)
library(pscl)
library(DHARMa)

load("data/groundfish_surveys/RV_2021Feb19.RData")


rv$region <- factor(rv$region)

#Non spatial Model
t1 <- gam(presence ~ s(btm_temp), family = binomial,
          data = rv, method = "REML")

summary(t1) #r-squared is terribly low yet everything is significant
gam.check(t1)

plot(t1, pages = 1,
     trans = plogis,
     shift = plogis(coef(t1)[1]),
     seWithMean = TRUE,
     shade = TRUE, shade.col = "lightblue")


t2 <- gam(presence ~ s(btm_temp) + s(depth), family = binomial,
          data = rv, method = "REML")

summary(t2) #r-squared is terribly low yet everythijng is significant
gam.check(t2)

plot(t2, pages = 1,
     trans = plogis,
     shift = plogis(coef(t1)[1]),
     seWithMean = TRUE,
     shade = TRUE, shade.col = "lightblue")


t3 <- gam(presence ~ s(btm_temp) + s(depth) + region, family = binomial,
          data = rv, method = "REML")

summary(t3) #r-squared is terribly low yet everythijng is significant
gam.check(t3)

plot(t3, pages = 1,
     trans = plogis,
     shift = plogis(coef(t1)[1]),
     seWithMean = TRUE,
     shade = TRUE, shade.col = "lightblue")



plot(t3, scheme = 1,
     trans = plogis,
     shift = plogis(coef(t1)[1]),
     seWithMean = TRUE,
     shade = TRUE, shade.col = "lightblue")



vis.gam(t1, plot.type = "persp", type = "response", theta = 230) #this just confuses me since theres no untis or region labels

# Calculate the probability at the mean
plogis(coef(t1)[1])



# #Model 1 Simple Model just lat and long
# m1 <- gam(presence ~ s(longitude, latitude), family = binomial,
#                 data = rv, method = "REML")
# gam.check(m1)
# 
# #Model 2 add temperature
# m2 <- gam(presence ~ s(longitude, latitude) + s(btm_temp), family = binomial,
#                 data = rv, method = "REML")
# lrtest(m1,m2)
# gam.check(m2)
# #Model 3 add depth
# m3 <- gam(presence ~ s(longitude, latitude) + s(btm_temp) + s(depth), family = binomial,
#           data = rv, method = "REML")
# lrtest(m2,m3)
# gam.check(m3)
# vis.gam(x = m3,
#         veiw = c("longitude", "latitude"),
#         plot.type = "contour",
#         too.far = 0.01)





# Model 4 change number or basis
# m11 <- gam(presence ~ s(longitude, latitude) + s(btm_temp) + s(depth), family = binomial,
#           data = rv, method = "REML")
# 
# 
# m5 <- gam(presence ~ s(longitude, latitude) + s(btm_temp, k = 20) + s(depth), family = binomial,
#           data = rv, method = "REML")
# 
# m6 <- gam(presence ~ s(longitude, latitude) + s(btm_temp, k = 29) + s(depth), family = binomial,
#           data = rv, method = "REML")
# 
# m7 <- gam(presence ~ s(longitude, latitude, k = 32) + s(btm_temp) + s(depth), family = binomial,
#           data = rv, method = "REML")
# 
# ## My Current Best model (I think)
m8 <- gam(presence ~ s(longitude, latitude) + s(btm_temp) + s(depth) + region, family = binomial,
          data = rv, method = "REML")
# 
# m9 <- gam(presence ~ s(longitude, latitude) + s(btm_temp, k = 12) + s(depth) + s(year), family = binomial,
#           data = rv, method = "REML")
# 
# m10 <- gam(presence ~ s(longitude, latitude) + s(btm_temp, k = 12) + s(depth) + te(year, btm_temp), family = binomial,
#           data = rv, method = "REML")






#Checking VIF and Concurvity

concurvity(m8, full = TRUE) #not sure why long and lat are so high here
concurvity(m8, full = FALSE) #but then are not high in this one