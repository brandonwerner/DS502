library ( splines )
library(FNN)
#this function pretty plots the predictions against the true values
plot_res <-function(y, pred){
  lims=c(min(min(pred),min(y)), max(max(pred),max(y)))
  plot(y, pred, xlim=lims, ylim=lims)
  
}
data=read.csv("data_cleaned/training_all_counties_all_years.csv",header = TRUE)
#data

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
#90percentileAQUI = data[,20]
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


#x7_hazardous_ratio is EXTREMELY sparse
min(df$x7_hazardous_ratio)
max(df$x7_hazardous_ratio)
mean(df$x7_hazardous_ratio)
length(subset(df$x7_hazardous_ratio, x7_hazardous_ratio != 0))

for (i in c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17)){
  
  l=length(df[,i])
  n=table(df[,i])[1]
  x=l-n
  print(i)
  print( l-n )
  print(" ")
}

#smp = df[sample(nrow(sdf), nrow(sdf)*0.05),]
#pairs(smp)

#DROP SPARSE VARIABLES AND TRY AGAIN

################### Linear Regression ###############
sdf = data.frame(scale(df))
set.seed(9)
idx = sample(nrow(sdf), nrow(sdf)*0.5)
#could try subsets of years
#df<- subset(df, x1_year>1985)
train = data.frame(sdf[idx,])
test = data.frame(sdf[-idx,])

full = lm(combined_mort_y~year+ 
          good_days_ratio+
          maxAQI+
          CO_ratio+
          NO2_ratio+
          ozone_ratio+
          SO2_ratio+
          PM2.5_ratio+
          days_with_AQI, data = sdf)
pred = predict(full, sdf)
par(mfrow=c(1,1))
plot_res(sdf$combined_mort_y, pred)
mean((sdf$combined_mort_y- pred)^2)

summary(full)

full = lm(female_mort_y~year+ 
            good_days_ratio+
            maxAQI+
            CO_ratio+
            NO2_ratio+
            ozone_ratio+
            SO2_ratio+
            PM2.5_ratio+
            days_with_AQI, data = sdf)
pred = predict(full, sdf)
par(mfrow=c(1,1))
plot_res(sdf$female_mort_y, pred)
summary(full)
mean((sdf$female_mort_y- pred)^2)

full = lm(male_mort_y~year+ 
            good_days_ratio+
            maxAQI+
            CO_ratio+
            NO2_ratio+
            ozone_ratio+
            SO2_ratio+
            PM2.5_ratio+
            days_with_AQI, data = sdf)
pred = predict(full, sdf)
par(mfrow=c(1,1))
plot_res(sdf$male_mort_y, pred)
summary(full)
mean((sdf$male_mort_y- pred)^2)



################### KNN ##############################

set.seed(1234)
idx = sample(nrow(df), nrow(df)*0.5)

train = sdf[idx,4:9]
test = sdf[-idx,4:9]
train_y = sdf[-idx,1]
test_y = sdf[-idx,1]


fit = knn.reg(train = train, test = test, y = train_y, k = 5)
plot_res(test_y, fit$pred)

err = mean((test_y - fit$pred)^2)
err

err = mean((0 - fit$pred)^2)
################## REGRESSION TREES ##################

library(tree)

train = sdf[idx,]
test = sdf[-idx,]

?tree
tr = tree(combined_mort_y~year+ 
                 good_days_ratio+
                 maxAQI+
                 CO_ratio+
                 NO2_ratio+
                 ozone_ratio+
                 SO2_ratio+
                 PM2.5_ratio+
                 days_with_AQI, data = sdf)
summary(tr)
par(mfrow=c(1,1))
plot(tr)
text(tr, pretty=0)
pred = predict(tr, test)
mean((pred-test$combined_mort_y)^2)
plot_res(test$combined_mort_y, pred)

tr = tree(female_mort_y~year+ 
            good_days_ratio+
            maxAQI+
            CO_ratio+
            NO2_ratio+
            ozone_ratio+
            SO2_ratio+
            PM2.5_ratio+
            days_with_AQI, data = sdf)
summary(tr)
par(mfrow=c(1,1))
plot(tr)
text(tr, pretty=0)



pred = predict(tr, test)
mean((pred-test$female_mort_y)^2)
plot_res(test$female_mort_y, pred)


tr = tree(male_mort_y~year+ 
            good_days_ratio+
            maxAQI+
            CO_ratio+
            NO2_ratio+
            ozone_ratio+
            SO2_ratio+
            PM2.5_ratio+
            days_with_AQI, data = sdf)
summary(tr)
par(mfrow=c(1,1))
plot(tr)
text(tr, pretty=0)
pred = predict(tr, test)
mean((pred-test$male_mort_y)^2)
plot_res(test$male_mort_y, pred)

#Use cross-validation in order to determine the optimal level of tree complexity. 
#Does pruning the tree improve the test MSE?

#cvt = cv.tree(tr)
#cvt
#plot(cvt$size, cvt$dev, type="b")
#plot(cvt$k, cvt$dev, type="b")
#pt = prune.tree(tr,best=13)
#ppred = predict(pt, test)

#plot(pt)
#text(pt, pretty=0)
#(sum(ppred-test$Sales)^2) / nrow(test)

#Use the bagging approach in order to analyze this data.
library(randomForest)
fdim(train)

?randomForest
bag = randomForest(combined_mort_y~year+ 
                     good_days_ratio+
                     maxAQI+
                     CO_ratio+
                     NO2_ratio+
                     ozone_ratio+
                     SO2_ratio+
                     PM2.5_ratio+
                     days_with_AQI, data =train)
bpred = predict(bag, test)
mean((bpred-test$combined_mort_y)^2)
importance(bag)
plot_res(test$combined_mort_y, bpred)

#female
bagf = randomForest(female_mort_y~year+ 
                      good_days_ratio+
                      maxAQI+
                      CO_ratio+
                      NO2_ratio+
                      ozone_ratio+
                      SO2_ratio+
                      PM2.5_ratio+
                      days_with_AQI, data =train)
bpredf = predict(bagf, test)
mean((bpredf-test$female_mort_y)^2)
importance(bagf)
plot_res(test$female_mort_y, bpredf)

#male
bagm = randomForest(male_mort_y~year+ 
                      good_days_ratio+
                      maxAQI+
                      CO_ratio+
                      NO2_ratio+
                      ozone_ratio+
                      SO2_ratio+
                      PM2.5_ratio+
                      days_with_AQI, data =train)
bpredm = predict(bagm, test)
mean((bpredm-test$combined_mort_y)^2)
importance(bag)
plot_res(test$male_mort_y, bpredm)

#Use random forests to analyze this data. 
rf = randomForest(combined_mort_y~year+ 
                     good_days_ratio+
                     maxAQI+
                     CO_ratio+
                     NO2_ratio+
                     ozone_ratio+
                     SO2_ratio+
                     PM2.5_ratio+
                     days_with_AQI, data =train)
rpred = predict(rf, test)
mean((rpred-test$combined_mort_y)^2)
importance(rf)
plot_res(test$combined_mort_y, rpred)
############### SPLINES  #############################

fit = lm ( combined_mort_y???bs ( age , knots = c (25 ,40 ,60) ) , data = Wage )
pred = predict ( fit , newdata = list ( age = age . grid ) , se = T )
plot ( age , wage , col =" gray ")
lines ( age . grid , pred$fit , lwd =2)
lines ( age . grid , pred$fit +2* pred$se , lty =" dashed ")
lines ( age . grid , pred$fit -2* pred$se , lty =" dashed ")