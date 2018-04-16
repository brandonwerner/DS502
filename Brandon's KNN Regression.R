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

dataf1=data.frame(combined_mort_y, 
                  x1_year, 
                  x2_good_days_ratio, 
                  x3_mod_days_ratio, 
                  x4_unhealth_sens_ratio,
                  x5_unhealth_ratio,
                  x6_very_unhealth_ratio, 
                  x7_hazardous_ratio,
                  x8_maxAQI,
                  x9_90percentileAQUI,
                  x10_median_AQI,
                  x11_CO_ratio,
                  x12_NO2_ratio,
                  x13_ozone_ratio,
                  x14_SO2_ratio,
                  x15_PM2.5_ratio,
                  x16_PM10_ratio,
                  days_with_AQI)

View(dataf1)

# You can play around with these to explore different regimes.
trainSize = 5000
testSize = 5000

# Set the random seed to keep the plots looking the same as I rerun things
set.seed(1234)
# Cut down the number of points to make things easier to see
data = Default[1:(trainSize+testSize),]
# This is now a vector of random indices into the data frame
trainSamples = sample(1:nrow(dataf1), trainSize, replace=FALSE) 
# Array of all FALSE
train = logical(nrow(dataf1))
# We test our training and testing data
train[trainSamples] = TRUE
test = !train



# KNN 
#dataf.knn<- knn.reg(dataf1$X, y=dataf1$y, k=3)

trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
dataf1.knn = knn.reg(train = (x1_year+ 
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
                                days_with_AQI), test = NULL, y = combined_mort_y, k = 100, algorithm=c("kd_tree", "cover_tree", "brute"))
plot(combined_mort_y, dataf1.knn$pred, xlab="Predictors", ylab="Combined Mortality Rate")
dataf1.knn