library(gam)
gam.m3=gam(combined_mort_y~s(x1_year, 4)+s(x2_good_days_ratio, 5)+s(x3_mod_days_ratio, 5)+s(x4_unhealth_sens_ratio, 5)+s(x5_unhealth_ratio, 5)+s(x6_very_unhealth_ratio, 5)+ 
             s(x7_hazardous_ratio, 5)+
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
plot(gam.m3, se=TRUE, col="blue")

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
