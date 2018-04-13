data=read.csv("data_cleaned/training_all_counties_all_years.csv",header = TRUE)
#data

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


dim(df)
nrow(df)

#look at some plots of the data using a sample

set.seed(9)
smp = df[sample(nrow(df), nrow(df)*0.05),]
dim(smp)

#plot(smp)
#cor(smp)

plot(smp[,2],smp[,1], xlab="Year", ylab="Mortality Rate", main="Mortality Rate by Year")
plot(table(df$x1_year), ylim=c(0,1000), xlab="Year", ylab="Number of Samples", main = "Data Size by Year")

mean_mort = aggregate(df$combined_mort_y , list(df$x1_year), mean)

plot(mean_mort, main = "Mean Mortality Rate", xlab="Year", ylab="Mean Mortality Rate")

#Compute the overall MSE if we used the mean value for each year as our predictive model.
colnames(mean_mort)[1] <- "x1_year"
colnames(mean_mort)[2] <- "mean_mort"

mdf = merge(x = df, y = mean_mort)
head(mdf)

library(hydroGOF)
e = mse(mdf$combined_mort_y, mdf$mean_mort)
e
