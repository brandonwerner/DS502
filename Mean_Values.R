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
              male_mort_y,
                female_mort_y, 
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
smp = df[sample(nrow(df), nrow(df)*0.2),]
dim(smp)

############# plot correlation matrix ########################

#plot(smp)
library(ggplot2)
library(reshape2)

cormat <- round(cor(smp[,5:13]),2)
cormat

#melted_cormat <- melt(cormat)
#ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_raster()

# Get lower triangle of the correlation matrix
get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}
# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}
upper_tri <- get_upper_tri(cormat)

# Melt the correlation matrix
melted_cormat <- melt(upper_tri, na.rm = TRUE)

# Heatmap
ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()


######## Plot Mortality Rates ###################

plot(smp$combined_mort_y,smp$Year, xlab="Year", ylab="Mortality Rate", main="Mortality Rate by Year")
plot(table(df$x1_year), ylim=c(0,1000), xlab="Year", ylab="Number of Samples", main = "Data Size by Year")

mean_mort_c = aggregate(df$combined_mort_y , list(df$x1_year), mean)
mean_mort_f = aggregate(df$female_mort_y , list(df$x1_year), mean)
mean_mort_m = aggregate(df$male_mort_y , list(df$x1_year), mean)

plot(mean_mort_c, type='o', ylim=c(20,85), main = "Mean Mortality Rate", xlab="Year", ylab="Mean Mortality Rate", col="black", pch=16)
lines(mean_mort_f, type='o', main = "Mean Female Mortality Rate", xlab="Year", ylab="Mean Mortality Rate", col="red", pch=16)
lines(mean_mort_m, type='o',main = "Mean Male Mortality Rate", xlab="Year", ylab="Mean Mortality Rate", col="blue", pch=16)
#Compute the overall MSE if we used the mean value for each year as our predictive model.
legend('bottomright', c("Male Mortality Rate", "Combined Mortality Rate", "Female Mortality Rate") , 
       pch=16, col=c('blue', 'black', 'red'), bty='n', cex=1)



################ Use mean of year to predict ###########

colnames(mean_mort_c)[1] <- "x1_year"
colnames(mean_mort_c)[2] <- "mean_mort_c"
colnames(mean_mort_f)[1] <- "x1_year"
colnames(mean_mort_f)[2] <- "mean_mort_f"
colnames(mean_mort_m)[1] <- "x1_year"
colnames(mean_mort_m)[2] <- "mean_mort_m"

mdf = merge(x = df, y = mean_mort_c)
mdf = merge(x = mdf, y = mean_mort_f)
mdf = merge(x = mdf, y = mean_mort_m)
head(mdf)
#GetMSE
e = mean((mdf$combined_mort_y - mdf$mean_mort_c)^2)
e

f = mean((mdf$female_mort_y - mdf$mean_mort_f)^2)
f

m = mean((mdf$male_mort_y - mdfm$mean_mort_m)^2)
m
