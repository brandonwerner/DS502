library(gam)

trainSize = 10000
testSize = 10000

# Set the random seed to keep the plots looking the same as I rerun things
set.seed(1234)

# This is now a vector of random indices into the data frame
trainSamples = sample(nrow(dataf1), nrow(dataf1)*0.5, replace=FALSE) 

train = dataf1[trainSamples,]
test = dataf1[-trainSamples,]

dim(train)
dim(test)

gam.m3=gam(combined_mort_y~ns(x1_year)+ns(x2_good_days_ratio)+ns(x3_mod_days_ratio)+ns(x4_unhealth_sens_ratio)+ns(x5_unhealth_ratio)+ns(x6_very_unhealth_ratio)+ 
             ns(x7_hazardous_ratio)+
             ns(x8_maxAQI)+
             ns(x9_90percentileAQUI)+
             ns(x10_median_AQI)+
             ns(x11_CO_ratio)+
             ns(x12_NO2_ratio)+
             ns(x13_ozone_ratio)+
             ns(x14_SO2_ratio)+
             ns(x15_PM2.5_ratio)+
             ns(x16_PM10_ratio)+
             ns(days_with_AQI), data=dataf1)

gam.m3

plot(test$combined_mort_y, gam.m3$pred, cex=1.5, xlab="Predictors", ylab="Combined Mortality Rate")

par(mfrow=c(1,3))
plot(gam.m3, se=TRUE, col="blue")

mse  = mean((gam.m3$pred - test$combined_mort_y)^2)
mse

# summary
summary(gam.m3)

# Mean Squared Error
mse(gam.m3)

# Residual Sum of Squares
aov(formula = combined_mort_y ~ x1_year+ 
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
      days_with_AQI, dataf1)


anova(gam.m3, test="F")
gam.m4=gam(combined_mort_y~s(x1_year, 4)+s(x2_good_days_ratio, 5)+s(x3_mod_days_ratio, 5)+s(x4_unhealth_sens_ratio, 5)+s(x5_unhealth_ratio, 5)+s(x6_very_unhealth_ratio, 5)+ 
             s(x8_maxAQI, 5)+
             s(x9_90percentileAQUI, 5)+
             s(x10_median_AQI, 5)+
             s(x11_CO_ratio, 5)+
             s(x12_NO2_ratio, 5)+
             s(x13_ozone_ratio, 5)+
             s(x14_SO2_ratio, 5)+
             s(x15_PM2.5_ratio, 5)+
             s(x16_PM10_ratio, 5)+
             s(days_with_AQI, 5), data=dataf1)
par(mfrow=c(1,3))
plot(gam.m4, se=TRUE, col="blue")

summary(gam.m4)

anova(gam.m4, test="F")
coef(summary(gam.m4))
