# We need the class library for logistic regression and knn
library(class)
# We need the MASS library for LDA and QDA
library(MASS)
library(caret)
library(FNN)


#this function pretty plots the predictions against the true values
plot_res <-function(y, pred){
  lims=c(min(min(pred),min(y)), max(max(pred),max(y)))
  plot(y, pred, xlim=lims, ylim=lims)
  
}

# import csv file
data=read.csv("data_cleaned/training_all_counties_all_years.csv",header = TRUE)
#data

names(data)
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

df=data.frame(combined_mort_y, 
              male_mort_y,
              female_mort_y, 
              x1_year, 
              x11_CO_ratio,
              x12_NO2_ratio,
              x13_ozone_ratio,
              x14_SO2_ratio,
              x15_PM2.5_ratio,
              x16_PM10_ratio,
              days_with_AQI)

names(df)
#can scale but it doesn't make much difference
sdf=scale(df)
dim(sdf)
#sdf=df
# Set the random seed to keep the plots looking the same as I rerun things
set.seed(1234)
idx = sample(nrow(df), nrow(df)*0.5)

train = sdf[idx,5:11]
test = sdf[-idx,5:11]
train_y = sdf[-idx,1]
test_y = sdf[-idx,1]

dim(test)

# KNN

#?knn.reg
#don't need the algorithm arg unless running too slow
#BE SURE train/test only include predictors, and y is set correctly
fit = knn.reg(train = train, test = test, y = train_y, k = 100)
plot_res(test_y, fit$pred)

err = mean((test_y - fit$pred)^2)
err

err = mean((0 - fit$pred)^2)

