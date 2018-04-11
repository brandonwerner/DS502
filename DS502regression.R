data=read.csv("training_all_counties_all_years.csv",header = TRUE)
#data

head(data)

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

dataf=data.frame(combined_mort_y, 
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

plot(dataf)
cor(dataf)


full = lm(combined_mort_y~ 
         x1_year+ 
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
        days_with_AQI, data = dataf)

summary(full)

library(leaps)
leaps=regsubsets(combined_mort_y~ 
                   x1_year+ 
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
                   x16_PM10_ratio,data=dataf,  nbest=15)

#To view the ranked models according to the adjusted R-squared criteria and BIC, respectively:

plot(leaps, scale="adjr2")
plot(leaps, scale="bic")

plot(leaps, scale="r2")
plot(leaps, scale="Cp")


#To fit the null model (The model with only the intercept)
null=lm(combined_mort_y~1, data=data)
null


#We can perform forward selection using the command:
step(null, scope=list(lower=null, upper=full), direction="forward",level="0.01")

#We can perform backward elimination on the same data set using the command:
step(full, data=Housing, direction="backward",level="0.05")

  
library(stats)
datam = as.matrix(dataf)
set.seed(2)
km.out = kmeans(datam, 2, nstart = 10)
km.out


