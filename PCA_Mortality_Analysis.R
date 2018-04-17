library(pls)
data=read.csv("data_cleaned/training_all_counties_all_years.csv",header = TRUE)
#data

plot_res <-function(y, pred){
  lims=c(min(min(pred),min(y)), max(max(pred),max(y)))
  plot(y, pred, xlim=lims, ylim=lims)
  
}

combined_mort_y = data[,5]
male_mort_y = data[,10]
female_mort_y = data[,8]

year = data[,11]
good_days_ratio = data[,13]
mod_days_ratio = data[,14]
unhealth_sens_ratio = data[,15]
unhealth_ratio = data[,16]
very_unhealth_ratio = data[,17]
hazardous_ratio = data[,18]
maxAQI = data[,19]
median_AQI = data[,21]
CO_ratio = data[,22]
NO2_ratio = data[,23]
ozone_ratio = data[,24]
SO2_ratio = data[,25]
PM2.5_ratio = data[,26]
PM10_ratio = data[,27]
days_with_AQI  = data[,12]

df=data.frame(combined_mort_y, 
              male_mort_y,
              female_mort_y, 
              year, 
              good_days_ratio, 
              very_unhealth_ratio, 
              hazardous_ratio,
              maxAQI,
              CO_ratio,
              NO2_ratio,
              ozone_ratio,
              SO2_ratio,
              PM2.5_ratio,
              PM10_ratio,
              days_with_AQI)


dim(df)
nrow(df)

#look at some plots of the data using a sample

set.seed(9)
#use heirarchical cross validataion to tune parameters for pcr, pls, and local regression

idx = sample(nrow(df), nrow(df)*0.5)

#could try subsets of years
#df<- subset(df, x1_year>1985)

train = data.frame(df[idx,])
test = data.frame(df[-idx,])
nrow(train)
nrow(test)

########### Combined ###################

fit = pcr(combined_mort_y~
          year+
          good_days_ratio+ 
          very_unhealth_ratio+ 
          hazardous_ratio+
          maxAQI+
          CO_ratio+
          NO2_ratio+
          ozone_ratio+
          SO2_ratio+
          PM2.5_ratio+
          PM10_ratio+
          days_with_AQI, data=train, center=TRUE, scale=TRUE, validataion="CV")
validationplot(fit, val.type="MSEP")

?pcr.predict
#seem to have elbows at 3 and 12 components 
summary(fit)

MSEP(fit)
#3 comps = 153
#11 comps = 144.2
#17 comps = 139.8
R2(fit)
#3 comps = 0.125
#11 comps = 0.179
#17 comps = 0.204

#PCA shows that even using all principle components, we can only explain 
#20% of the variation in the data.

#pred.pcr <- predict(yarn.pcr, ncomp = 1:6, newdata=subset(yarn, !train))
pred = predict(fit, ncomp = 11, newdata=test)
mean((pred-test$combined_mort_y)^2)
plot_res(test$combined_mort_y, as.matrix(pred))


############### Female ###################
fit = pcr(female_mort_y~x1_year+
            year+
            good_days_ratio+ 
            very_unhealth_ratio+ 
            hazardous_ratio+
            maxAQI+
            CO_ratio+
            NO2_ratio+
            ozone_ratio+
            SO2_ratio+
            PM2.5_ratio+
            PM10_ratio+
            days_with_AQI, data=train, center=TRUE, scale=TRUE, validataion="CV")
validationplot(fit, val.type="MSEP")

?pcr.predict
#seem to have elbows at 3 and 12 components 
summary(fit)

MSEP(fit)
#3 comps = 153
#11 comps = 144.2
#17 comps = 139.8
R2(fit)
#3 comps = 0.125
#11 comps = 0.179
#17 comps = 0.204

#PCA shows that even using all principle components, we can only explain 
#20% of the variation in the data.

#pred.pcr <- predict(yarn.pcr, ncomp = 1:6, newdata=subset(yarn, !train))
pred = predict(fit, ncomp = 11, newdata=test)
mean((pred-test$combined_mort_y)^2)
plot_res(test$combined_mort_y, as.matrix(pred))

################## MALE ##############################

fit = pcr(male_mort_y~x1_year+
            year+
            good_days_ratio+ 
            very_unhealth_ratio+ 
            hazardous_ratio+
            maxAQI+
            CO_ratio+
            NO2_ratio+
            ozone_ratio+
            SO2_ratio+
            PM2.5_ratio+
            PM10_ratio+
            days_with_AQI, data=train, center=TRUE, scale=TRUE, validataion="CV")
validationplot(fit, val.type="MSEP")

#seem to have elbows at 3 and 12 components 
summary(fit)

MSEP(fit)
#3 comps = 153
#11 comps = 144.2
#17 comps = 139.8
R2(fit)
#3 comps = 0.125
#11 comps = 0.179
#17 comps = 0.204

#PCA shows that even using all principle components, we can only explain 
#20% of the variation in the data.

#pred.pcr <- predict(yarn.pcr, ncomp = 1:6, newdata=subset(yarn, !train))
pred = predict(fit, ncomp = 12, newdata=test)
mean((pred-test$combined_mort_y)^2)
plot_res(test$combined_mort_y, as.matrix(pred))





########################################################################################
#####################    PLS      ######################################################
########################################################################################


########### Combined ###################

fit = plsr(combined_mort_y~x1_year+
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
            days_with_AQI, data=train, center=TRUE, scale=TRUE, validataion="CV")
validationplot(fit, val.type="MSEP")

?pcr.predict
#seem to have elbows at 3 and 12 components 
summary(fit)

MSEP(fit)
#3 comps = 153
#11 comps = 144.2
#17 comps = 139.8
R2(fit)
#3 comps = 0.125
#11 comps = 0.179
#17 comps = 0.204

#PCA shows that even using all principle components, we can only explain 
#20% of the variation in the data.

#pred.pcr <- predict(yarn.pcr, ncomp = 1:6, newdata=subset(yarn, !train))
pred = predict(fit, ncomp = 11, newdata=test)
mean((pred-test$combined_mort_y)^2)
plot_res(test$combined_mort_y, as.matrix(pred))


############### Female ###################
fit = plsr(female_mort_y~x1_year+
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
            days_with_AQI, data=train, center=TRUE, scale=TRUE, validataion="CV")
validationplot(fit, val.type="MSEP")

#seem to have elbows at 3 and 12 components 
summary(fit)

MSEP(fit)
#3 comps = 153
#11 comps = 144.2
#17 comps = 139.8
R2(fit)
#3 comps = 0.125
#11 comps = 0.179
#17 comps = 0.204

#PCA shows that even using all principle components, we can only explain 
#20% of the variation in the data.

#pred.pcr <- predict(yarn.pcr, ncomp = 1:6, newdata=subset(yarn, !train))
pred = predict(fit, ncomp = 11, newdata=test)
mean((pred-test$combined_mort_y)^2)
plot_res(test$combined_mort_y, as.matrix(pred))
plot(fit)
################## MALE ##############################

fit = plsr(male_mort_y~x1_year+
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
            days_with_AQI, data=train, center=TRUE, scale=TRUE, validataion="CV")
validationplot(fit, val.type="MSEP")

?pcr.predict
#seem to have elbows at 3 and 12 components 
summary(fit)

MSEP(fit)
#3 comps = 153
#11 comps = 144.2
#17 comps = 139.8
R2(fit)
#3 comps = 0.125
#11 comps = 0.179
#17 comps = 0.204

#PCA shows that even using all principle components, we can only explain 
#20% of the variation in the data.

as.matrix(pred)
#pred.pcr <- predict(yarn.pcr, ncomp = 1:6, newdata=subset(yarn, !train))
pred = predict(fit, ncomp = 5, newdata=test)
mean((pred-test$combined_mort_y)^2)
plot_res(test$combined_mort_y, as.matrix(pred))
