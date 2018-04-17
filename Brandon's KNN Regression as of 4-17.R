# We need the class library for logistic regression and knn
library(class)
# We need the MASS library for LDA and QDA
library(MASS)
library(caret)
library(FNN)

# import csv file
data = read.csv("R:/training_all_counties_all_years.csv", header = TRUE)

combined_mort_y = data[,5]
male_mort_y = data[,10]
female_mort_y = data[,8]

x1_year = data[,11]
x2_good_days_ratio = data[,13]
x3_mod_days_ratio = data[,14]
x4_unhealth_sens_ratio = data[,15]
x5_unhealth_ratio = data[,16]
x6_very_unhealth_ratio = data[,17]
x7_hazardous_ratio = data[,18]
x8_maxAQI = data[,19]
x9_90percentileAQUI = data[,20]
x10_median_AQI = data[,21]
x11_CO_ratio = data[,22]
x12_NO2_ratio = data[,23]
x13_ozone_ratio = data[,24]
x14_SO2_ratio = data[,25]
x15_PM2.5_ratio = data[,26]
x16_PM10_ratio = data[,27]
days_with_AQI  = data[,12]

# Combined Mortality data frame
dataf1=data.frame(combined_mort_y, 
                  x8_maxAQI,
                  x9_90percentileAQUI,
                  x10_median_AQI)

# Female mortality data frame
dataf1female=data.frame(female_mort_y, 
                  x8_maxAQI,
                  x9_90percentileAQUI,
                  x10_median_AQI)

# Male mortality data frame
dataf1male=data.frame(male_mort_y, 
                        x8_maxAQI,
                        x9_90percentileAQUI,
                        x10_median_AQI)


# You can play around with these to explore different regimes.
trainSize = 10000
testSize = 10000

# Set the random seed to keep the plots looking the same as I rerun things
set.seed(1234)

# This is now a vector of random indices into the data frame
trainSamples = sample(nrow(dataf1), nrow(dataf1)*0.5, replace=FALSE) 

train = dataf1[trainSamples,]
test = dataf1[-trainSamples,]

# Female train and test
trainSamplesf = sample(nrow(dataf1female), nrow(dataf1female)*0.5, replace=FALSE) 

trainf = dataf1female[trainSamples,]
testf = dataf1female[-trainSamples,]

# Male train and test
trainSamplesm = sample(nrow(dataf1male), nrow(dataf1male)*0.5, replace=FALSE) 

trainm = dataf1male[trainSamples,]
testm = dataf1male[-trainSamples,]

#dim(train)
#dim(test)

n.train <- nrow(train)
klist <- 1 - seq(n.train) # we test all values of k
nfolds <- 5 # we make 5-fold cross-validation
y.pred.train <- knn(klist,train,train,train)
y.pred.test <- knn(klist,x.train,y.train,x.test)
y.pred.cv <- knn.cv(klist,x8_maxAQI,train$combined_mort_y,nfolds)

y=dataf1$combined_mort_y
y.test=y[test]

# KNN 
dataf1.knn = knn.reg(train = train, test = test, y = combined_mort_y, k = 100)

# KNN trying for every value of k = number of rows
dataf1.knn = knn.reg(train = train, test = test, y = combined_mort_y, k = nrow(train))

# KNN nfolds
dataf1.knnnfolds = knn.reg(train = train, test = test, y = combined_mort_y, nfolds)
plot(test$combined_mort_y, dataf1.knnnfolds$pred, cex=1.5, xlab="Predictors", ylab="Combined Mortality Rate")

# KNN female
dataf1.knnf = knn.reg(train = train, test = test, y = female_mort_y, k = 100)

# KNN male
dataf1.knnm = knn.reg(train = train, test = test, y = male_mort_y, k = 100)

y.pred.cv <- knn.cv(train = train, test = test, y = combined_mort_y,nfolds)

air.label <- data[,19]
air.data <- data[,5]
c1 <- data[,5]
knn.cv(air.data, air.label, c1, k = 2)

res.knn.cv <- knn.cv(data = dataf1, label = 2, k = 7, p = 12, method = "regression")
res.knn.cv
head(res.knn.cv$error.ind)


summary(knn.cv)

plot(test$combined_mort_y, dataf1.knn$pred, cex=1.5, xlab="Predictors", ylab="Combined Mortality Rate")

plot(test$female_mort_y, dataf1.knnf$pred, cex=1.5, xlab="Predictors", ylab="Female Mortality Rate")

plot(test$male_mort_y, dataf1.knnm$pred, cex=1.5, xlab="Predictors", ylab="Male Mortality Rate")

# Compute mean-square error (MSE) for combined mortality rate
mse.train <- apply((dataf1.knn$pred - test)^2 , 2, mean)
mse.test  = mean((dataf1.knn$pred - test$combined_mort_y)^2)
mse.test
mse.train <- apply((train - train$combined_mort_y)^2 , 2, mean)
mse.train
mse.cv <- apply((dataf1.knnn - y.train)^2 , 2, mean)

dataf1.knn$pred - test$combined_mort_y


# Plot MSE as a function of k
plot(mse.train , ylim=c(0,2) , type='l' , xlab='k' , ylab='MSE', col=1 , lwd=2)
lines(mse.test , col=2 , lwd=2)
lines(mse.cv, col=3 , lwd=2)
legend("bottomright",legend=c('Train','Test','CV'),text.col=seq(3) , lty=1 , col=seq(3))

# Compute MSE for female mortality rate
msef  = mean((dataf1.knnf$pred - test$female_mort_y)^2)
msef

# Compute MSE for male mortality rate
msem  = mean((dataf1.knnm$pred - test$male_mort_y)^2)
msem

mse.test <- apply((y.pred.test - y.test)^2 , 2, mean)
mse.cv <- apply((y.pred.cv - y.train)^2 , 2, mean)

lines(dataf1$combined_mort_y, dataf1.knn$pred, col = "darkorange", lwd = 0.25)


summary(dataf1.knn)
dataf1.knn

mse(dataf1.knn)

# KNN = 10
dataf1.knn10 = knn.reg(train = (x1_year+ 
                                x2_good_days_ratio+ 
                                x3_mod_days_ratio+
                                x4_unhealth_sens_ratio+
                                x5_unhealth_ratio+
                                x6_very_unhealth_ratio+ 
                                x7_hazardous_ratio+
                                x8_maxAQI+
                                x9_90percentileAQUI+
                                x10_median_AQI+
                                x11_CO_ratio+
                                x12_NO2_ratio+
                                x13_ozone_ratio+
                                x14_SO2_ratio+
                                x15_PM2.5_ratio+
                                x16_PM10_ratio+
                                days_with_AQI), test = NULL, y = combined_mort_y, k = 10)

plot(combined_mort_y, dataf1.knn10$pred, cex=1.5, xlab="Predictors", ylab="Combined Mortality Rate")
lines(dataf1$combined_mort_y, dataf1.knn10$pred, col = "darkorange", lwd = 0.25)

# KNN = 25
dataf1.knn25 = knn.reg(train = (x1_year+ 
                                  x2_good_days_ratio+ 
                                  x3_mod_days_ratio+
                                  x4_unhealth_sens_ratio+
                                  x5_unhealth_ratio+
                                  x6_very_unhealth_ratio+ 
                                  x7_hazardous_ratio+
                                  x8_maxAQI+
                                  x9_90percentileAQUI+
                                  x10_median_AQI+
                                  x11_CO_ratio+
                                  x12_NO2_ratio+
                                  x13_ozone_ratio+
                                  x14_SO2_ratio+
                                  x15_PM2.5_ratio+
                                  x16_PM10_ratio+
                                  days_with_AQI), test = NULL, y = combined_mort_y, k = 25)

plot(combined_mort_y, dataf1.knn25$pred, cex=1.5, xlab="Predictors", ylab="Combined Mortality Rate")
lines(dataf1$combined_mort_y, dataf1.knn25$pred, col = "darkorange", lwd = 0.25)

# KNN = 250
dataf1.knn250 = knn.reg(train = (x1_year+ 
                                  x2_good_days_ratio+ 
                                  x3_mod_days_ratio+
                                  x4_unhealth_sens_ratio+
                                  x5_unhealth_ratio+
                                  x6_very_unhealth_ratio+ 
                                  x7_hazardous_ratio+
                                  x8_maxAQI+
                                  x9_90percentileAQUI+
                                  x10_median_AQI+
                                  x11_CO_ratio+
                                  x12_NO2_ratio+
                                  x13_ozone_ratio+
                                  x14_SO2_ratio+
                                  x15_PM2.5_ratio+
                                  x16_PM10_ratio+
                                  days_with_AQI), test = NULL, y = combined_mort_y, k = 250)

plot(combined_mort_y, dataf1.knn250$pred, cex=1.5, xlab="Predictors", ylab="Combined Mortality Rate")
lines(dataf1$combined_mort_y, dataf1.knn250$pred, col = "darkorange", lwd = 0.25)

# KNN k =10 with male mortality rate as y
dataf1.knnm10 = knn.reg(train = (x1_year+ 
                                  x2_good_days_ratio+ 
                                  x3_mod_days_ratio+
                                  x4_unhealth_sens_ratio+
                                  x5_unhealth_ratio+
                                  x6_very_unhealth_ratio+ 
                                  x7_hazardous_ratio+
                                  x8_maxAQI+
                                  x9_90percentileAQUI+
                                  x10_median_AQI+
                                  x11_CO_ratio+
                                  x12_NO2_ratio+
                                  x13_ozone_ratio+
                                  x14_SO2_ratio+
                                  x15_PM2.5_ratio+
                                  x16_PM10_ratio+
                                  days_with_AQI), test = NULL, y = male_mort_y, k = 10)

plot(combined_mort_y, dataf1.knnm10$pred, cex=1.5, xlab="Predictors", ylab="Combined Mortality Rate")
lines(dataf1$combined_mort_y, dataf1.knnm10$pred, col = "darkorange", lwd = 0.25)

summary(dataf1.knnm10)
dataf1.knnm10

cor(dataf1, y = NULL)
plot(dataf1)
confint(dataf1)
coef(summary(data1.knn))
