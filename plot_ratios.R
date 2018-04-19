df=read.csv("data_cleaned/training_mortality_ratios.csv", header = TRUE)
names(df)

set.seed(9)
smp = df[sample(nrow(df), nrow(df)*0.2),]
dim(smp)

######## Plot Mortality Rates ###################

plot(smp$Year, smp$female_mort_y, xlab="Year", ylab="Mortality Rate", main="Mortality Rate by Year")
plot(table(df$Year), ylim=c(0,1000), xlab="Year", ylab="Number of Samples", main = "Data Size by Year")

mean_mort_c = aggregate(df$combined_mort_y , list(df$Year), mean)
mean_mort_f = aggregate(df$female_mort_y , list(df$Year), mean)
mean_mort_m = aggregate(df$male_mort_y , list(df$Year), mean)

plot(mean_mort_c, type='o',  main = "Mean Mortality Rate", xlab="Year", ylab="Mean Mortality Rate", col="black", pch=16)
lines(mean_mort_f, type='o', main = "Mean Female Mortality Rate", xlab="Year", ylab="Mean Mortality Rate", col="red", pch=16)
lines(mean_mort_m, type='o',main = "Mean Male Mortality Rate", xlab="Year", ylab="Mean Mortality Rate", col="blue", pch=16)
#Compute the overall MSE if we used the mean value for each year as our predictive model.
legend('bottomright', c("Male Mortality Rate", "Combined Mortality Rate", "Female Mortality Rate") , 
       pch=16, col=c('blue', 'black', 'red'), bty='n', cex=1)


