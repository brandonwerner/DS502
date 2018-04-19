df=read.csv("data_cleaned/training_all_counties_all_years_ll.csv",header = TRUE)
names(df)

mean_mort_c = aggregate(df$combined_mort_y , list(df$Year), mean)
mean_mort_f = aggregate(df$female_mort_y , list(df$Year), mean)
mean_mort_m = aggregate(df$male_mort_y , list(df$Year), mean)

colnames(mean_mort_c)[1] <- "Year"
colnames(mean_mort_c)[2] <- "mean_mort_c"
colnames(mean_mort_f)[1] <- "Year"
colnames(mean_mort_f)[2] <- "mean_mort_f"
colnames(mean_mort_m)[1] <- "Year"
colnames(mean_mort_m)[2] <- "mean_mort_m"

mdf = merge(x = df, y = mean_mort_c)
mdf = merge(x = mdf, y = mean_mort_f)
mdf = merge(x = mdf, y = mean_mort_m)

#Transform mortality rates to ratio
mdf$combined_mort_y = mdf$combined_mort_y/mdf$mean_mort_c
mdf$female_mort_y = mdf$female_mort_y/mdf$mean_mort_f
mdf$male_mort_y = mdf$male_mort_y/mdf$mean_mort_m
head(mdf)
?write.csv
# Write to file
write.csv(mdf, file = "data_cleaned/training_mortality_ratios.csv")


df=read.csv("data_cleaned/validate_all_counties_all_years_ll.csv",header = TRUE)
names(df)

mean_mort_c = aggregate(df$combined_mort_y , list(df$Year), mean)
mean_mort_f = aggregate(df$female_mort_y , list(df$Year), mean)
mean_mort_m = aggregate(df$male_mort_y , list(df$Year), mean)

colnames(mean_mort_c)[1] <- "Year"
colnames(mean_mort_c)[2] <- "mean_mort_c"
colnames(mean_mort_f)[1] <- "Year"
colnames(mean_mort_f)[2] <- "mean_mort_f"
colnames(mean_mort_m)[1] <- "Year"
colnames(mean_mort_m)[2] <- "mean_mort_m"

mdf = merge(x = df, y = mean_mort_c)
mdf = merge(x = mdf, y = mean_mort_f)
mdf = merge(x = mdf, y = mean_mort_m)

#Transform mortality rates to ratio
mdf$combined_mort_y = mdf$combined_mort_y/mdf$mean_mort_c
mdf$female_mort_y = mdf$female_mort_y/mdf$mean_mort_f
mdf$male_mort_y = mdf$male_mort_y/mdf$mean_mort_m
head(mdf)
?write.csv
# Write to file
write.csv(mdf, file = "data_cleaned/validate_mortality_ratios.csv")



df=read.csv("data_cleaned/training_all_counties_all_years_ll.csv",header = TRUE)
names(df)

mean_mort_c = aggregate(df$combined_mort_y , list(df$Year), mean)
mean_mort_f = aggregate(df$female_mort_y , list(df$Year), mean)
mean_mort_m = aggregate(df$male_mort_y , list(df$Year), mean)

colnames(mean_mort_c)[1] <- "Year"
colnames(mean_mort_c)[2] <- "mean_mort_c"
colnames(mean_mort_f)[1] <- "Year"
colnames(mean_mort_f)[2] <- "mean_mort_f"
colnames(mean_mort_m)[1] <- "Year"
colnames(mean_mort_m)[2] <- "mean_mort_m"

mdf = merge(x = df, y = mean_mort_c)
mdf = merge(x = mdf, y = mean_mort_f)
mdf = merge(x = mdf, y = mean_mort_m)

# Write to file
write.csv(mdf, file = "data_cleaned/training_mortality_rates.csv")


df=read.csv("data_cleaned/validate_all_counties_all_years_ll.csv",header = TRUE)
names(df)

mean_mort_c = aggregate(df$combined_mort_y , list(df$Year), mean)
mean_mort_f = aggregate(df$female_mort_y , list(df$Year), mean)
mean_mort_m = aggregate(df$male_mort_y , list(df$Year), mean)

colnames(mean_mort_c)[1] <- "Year"
colnames(mean_mort_c)[2] <- "mean_mort_c"
colnames(mean_mort_f)[1] <- "Year"
colnames(mean_mort_f)[2] <- "mean_mort_f"
colnames(mean_mort_m)[1] <- "Year"
colnames(mean_mort_m)[2] <- "mean_mort_m"

mdf = merge(x = df, y = mean_mort_c)
mdf = merge(x = mdf, y = mean_mort_f)
mdf = merge(x = mdf, y = mean_mort_m)

# Write to file
write.csv(mdf, file = "data_cleaned/validate_mortality_rates.csv")










dfc = df[c("combined_mort_y",
           "Latitude",
           "Longitude")] 
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
plot(table(df$Year), ylim=c(0,1000), xlab="Year", ylab="Number of Samples", main = "Data Size by Year")

mean_mort_c = aggregate(df$combined_mort_y , list(df$Year), mean)
mean_mort_f = aggregate(df$female_mort_y , list(df$Year), mean)
mean_mort_m = aggregate(df$male_mort_y , list(df$Year), mean)

plot(mean_mort_c, type='o', ylim=c(20,85), main = "Mean Mortality Rate", xlab="Year", ylab="Mean Mortality Rate", col="black", pch=16)
lines(mean_mort_f, type='o', main = "Mean Female Mortality Rate", xlab="Year", ylab="Mean Mortality Rate", col="red", pch=16)
lines(mean_mort_m, type='o',main = "Mean Male Mortality Rate", xlab="Year", ylab="Mean Mortality Rate", col="blue", pch=16)
#Compute the overall MSE if we used the mean value for each year as our predictive model.
legend('bottomright', c("Male Mortality Rate", "Combined Mortality Rate", "Female Mortality Rate") , 
       pch=16, col=c('blue', 'black', 'red'), bty='n', cex=1)



################ Use mean of year to predict ###########

colnames(mean_mort_c)[1] <- "Year"
colnames(mean_mort_c)[2] <- "mean_mort_c"
colnames(mean_mort_f)[1] <- "Year"
colnames(mean_mort_f)[2] <- "mean_mort_f"
colnames(mean_mort_m)[1] <- "Year"
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

#Transform mortality rates to ratio
mdf$combined_mort_y = mdf$combined_mort_y/mdf$mean_mort_c

