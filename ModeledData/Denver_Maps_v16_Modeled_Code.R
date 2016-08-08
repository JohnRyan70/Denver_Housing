
#################################
#

setwd('C:/Users/John/Denver_Housing_Project/ACS_Data/Final_Data')

load("ACSData9_14.RData")

# Identify and remove missing values from analysis

table(is.na(ACSData9_14$MedHouseVal))

# Noticing ten missing observations of median housing value, and taking out 
test <- subset(ACSData9_14, is.na(MedHouseVal)) # So you can look at the 2 observations missing this variable
ACSData9_14nona <- subset(ACSData9_14, !is.na(MedHouseVal)) 
table(is.na(ACSData9_14nona$MedHouseVal)) #Now no NA's in median housing value
save(ACSData9_14nona, file = "ACSData9_14nona.RData")


# Now, to test models using regression

library(corrplot)
library(foreign)
library(glmnet)
library(broom)
library(foreign)
library(MASS)
library(car)
library(ggplot2)

ACSData9_14nona.mod <-ACSData9_14nona[c("MedHouseVal",
                                        "HHInc_Own_PercUnder35K",
                                        "HHInc_Own_Perc35Kto100K",
                                        "HHInc_Own_PercOver100K",
                                        "IndInc_Median",
                                        "PercNotHSGrad",
                                        "PercBAorMore",
                                        "HHs_Perc_MarriedCplFam",
                                        "HHs_AvgFamilySize",
                                        "MedianAge",
                                        "PercUnder150PercPov",
                                        "Housing_Perc_Vacant",
                                        "MeanHrsWkd",
                                        "SchlDistRnkLYr")]
save(ACSData9_14nona.mod, file = "ACSData9_14nona.mod.RData")


# Create a training/test split of 70% train, 30% test

set.seed(1234)
ind <-sample(2,nrow(ACSData9_14nona.mod), replace=TRUE, prob=c(0.7, 0.3))
ACSData9_14nona.mod.train <-ACSData9_14nona.mod[ind==1,]
ACSData9_14nona.mod.test <-ACSData9_14nona.mod[ind==2,]

table(is.na(ACSData9_14nona.mod.train$MedHouseVal)) # 400 observations
table(is.na(ACSData9_14nona.mod.test$MedHouseVal)) # 178 observations

save(ACSData9_14nona, file = "ACSData9_14nona.RData")
save(ACSData9_14nona.mod, file = "ACSData9_14nona.mod.RData")
save(ACSData9_14nona.mod.train, file = "ACSData9_14nona.mod.training.RData")
save(ACSData9_14nona.mod.test, file = "ACSData9_14nona.mod.test.RData")


# Modeling using OLS first

model1 <- lm(MedHouseVal ~ 
               HHInc_Own_Perc35Kto100K +
               HHInc_Own_PercOver100K +
               IndInc_Median +
               PercNotHSGrad +
               PercBAorMore +
               HHs_Perc_MarriedCplFam +
               HHs_AvgFamilySize +
               MedianAge +
               PercUnder150PercPov +
               Housing_Perc_Vacant +
               MeanHrsWkd +
               SchlDistRnkLYr,
             data = ACSData9_14nona.mod.train)

# make predictions and summarize accuracy
predictions1 <- predict(model1, ACSData9_14nona.mod.train)

model1.res = resid(model1)
summary(model1)
AIC(model1)
BIC(model1)
summary(model1.res)
#ggplot(model1.res)
sm <-summary(model1)
sum(sm$residuals^2)
sqrt(mean(sm$residuals^2))

vif(model1)


model2 <- lm(MedHouseVal ~ 
               HHInc_Own_Perc35Kto100K +
               HHInc_Own_PercOver100K +
               IndInc_Median +
               PercBAorMore +
               HHs_Perc_MarriedCplFam +
               HHs_AvgFamilySize +
               MedianAge +
               PercUnder150PercPov +
               Housing_Perc_Vacant +
               MeanHrsWkd +
               SchlDistRnkLYr,
             data = ACSData9_14nona.mod.train)

# make predictions and summarize accuracy
predictions2 <- predict(model2, ACSData9_14nona.mod.train)

model2.res = resid(model2)
summary(model2)
AIC(model2)
BIC(model2)
summary(model2.res)
vif(model2)


model3 <- lm(MedHouseVal ~ 
               HHInc_Own_Perc35Kto100K +
               HHInc_Own_PercOver100K +
               IndInc_Median +
               PercNotHSGrad +
               PercBAorMore +
               HHs_Perc_MarriedCplFam +
               HHs_AvgFamilySize +
               MedianAge +
               PercUnder150PercPov +
               Housing_Perc_Vacant +
               SchlDistRnkLYr,
             data = ACSData9_14nona.mod.train)

# make predictions and summarize accuracy
predictions3 <- predict(model3, ACSData9_14nona.mod.train)

model3.res = resid(model3)
summary(model3)
AIC(model3)
BIC(model3)
summary(model3.res)
#ggplot(model1.res)
vif(model3)


model4 <- lm(MedHouseVal ~ 
               HHInc_Own_PercOver100K +
               IndInc_Median +
               PercNotHSGrad +
               PercBAorMore +
               HHs_Perc_MarriedCplFam +
               HHs_AvgFamilySize +
               MedianAge +
               PercUnder150PercPov +
               Housing_Perc_Vacant +
               MeanHrsWkd +
               SchlDistRnkLYr,
             data = ACSData9_14nona.mod.train)


# make predictions and summarize accuracy
predictions4 <- predict(model4, ACSData9_14nona.mod.train)

model4.res = resid(model4)
summary(model4)
AIC(model4)
BIC(model4)
summary(model4.res)
#ggplot(model1.res)

vif(model4)

library(corrgram)
corrgram(ACSData9_14nona.mod.train, order=NULL, 
         lower.panel=panel.shade,
         upper.panel=NULL,
         text.panel=panel.txt,
         main="Correlogram of data")


### Multicollinearity

## Multicollinearity and LASSO Regression 


############################
#
#
#  Try LASSO regression to reduce multicollinearity

# Preparing matrices for modeling
# We include a -1 at the end of x.train and x.test, to drop a duplicate intercept term
set.seed(1111)
x.train=model.matrix(MedHouseVal ~ 
                       HHInc_Own_Perc35Kto100K +
                       HHInc_Own_PercOver100K +
                       IndInc_Median +
                       PercNotHSGrad +
                       PercBAorMore +
                       HHs_Perc_MarriedCplFam +
                       HHs_AvgFamilySize +
                       MedianAge +
                       PercUnder150PercPov +
                       Housing_Perc_Vacant +
                       MeanHrsWkd +
                       SchlDistRnkLYr
                     - 1,
                     data = ACSData9_14nona.mod.train)


y.train=ACSData9_14nona.mod.train$MedHouseVal

x.test=model.matrix(MedHouseVal ~ 
                      HHInc_Own_Perc35Kto100K +
                      HHInc_Own_PercOver100K +
                      IndInc_Median +
                      PercNotHSGrad +
                      PercBAorMore +
                      HHs_Perc_MarriedCplFam +
                      HHs_AvgFamilySize +
                      MedianAge +
                      PercUnder150PercPov +
                      Housing_Perc_Vacant +
                      MeanHrsWkd +
                      SchlDistRnkLYr
                    - 1,
                    data = ACSData9_14nona.mod.test)

# Performing lasso, with cross-validation, ten folds (default; can choose more)

set.seed(1111)
fit.lasso=cv.glmnet(x.train,y.train,alpha=1)

# Discovering the best lambda, using minimum (not one-standard-error)

### Here are the coefficients for the minimum lambda rule:

coef(fit.lasso, s = "lambda.min")

plot(fit.lasso,xvar="lambda",label=TRUE)

## Plot the change in coefficients as the tuning parameter changes.
plot(fit.lasso$glmnet.fit, "lambda", label=TRUE)


## Comparing OLS and LASSO Results

## RMSE for LASSO
bestlamb.lasso <- fit.lasso$lambda.min
bestlamb.lasso
pred.lasso <-predict(fit.lasso, s=bestlamb.lasso, newx=x.train)
lasso.err <-mean((ACSData9_14nona.mod.train$MedHouseVal-pred.lasso)^2)
sqrt(lasso.err)

# Check coefficients and RMSE with test data
reg.lasso <-cv.glmnet(x.train,y.train, alpha=1)
pred.lasso2 <-predict(reg.lasso, s=bestlamb.lasso, newx=x.test)
lasso.err2 <-mean((ACSData9_14nona.mod.test$MedHouseVal-pred.lasso2)^2)
coef.lasso2 <-predict(reg.lasso, type="coefficients", s=bestlamb.lasso)
coef.lasso2
sqrt(lasso.err2)

