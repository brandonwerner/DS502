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
data=read.csv("data_cleaned/training_all_counties_all_years_ll.csv",header = TRUE)
#data

names(data)
df = data[c("combined_mort_y",
            "female_mort_y",
            "male_mort_y",
            "Year",
            "Good.Days",
            "Unhealthy.for.Sensitive.Groups.Days",
            "Unhealthy.Days",
            "Very.Unhealthy.Days",
            "Hazardous.Days",
            "Max.AQI",
            "Median.AQI",
            "Days.NO2",                            
            "Days.Ozone",
            "Days.SO2",
            "Days.PM2.5",
            "Days.PM10",
            "Latitude",
            "Longitude")]  

dfc = df[c("combined_mort_y",
             "Latitude",
             "Longitude")] 

names(df)
#can scale but it doesn't make much difference
sdf=scale(dfc)
dim(sdf)
sdf=dfc
# Set the random seed to keep the plots looking the same as I rerun things
set.seed(1234)
idx = sample(nrow(df), nrow(df)*0.5)

train = sdf[idx,2:ncol(sdf)]
test = sdf[-idx,2:ncol(sdf)]
train_y = sdf[-idx,1]
test_y = sdf[-idx,1]

dim(test)
# KNN
#?knn.reg
#don't need the algorithm arg unless running too slow
#BE SURE train/test only include predictors, and y is set correctly
fit = knn.reg(train = train, test = test, y = train_y, k = 3)
plot_res(test_y, fit$pred)

err = mean((test_y - fit$pred)^2)
err

err = mean((0 - fit$pred)^2)

