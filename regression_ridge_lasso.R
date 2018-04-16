data=read.csv("training_all_counties_all_years.csv",header = TRUE)
#data

head(data)

summary(data)

combined_mort_y = data[,5]
male_mort_y = data[,10]
female_mort_y = data[,8]

location = data[,3]
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

cor(dataf)

full_comb = lm(combined_mort_y~ 
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

male_comb = lm(male_mort_y~ 
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

female_comb = lm(female_mort_y~ 
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

summary(full_comb)
summary(male_comb)
summary(female_comb)

female_comb = lm(female_mort_y~ 
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

lm.CO = lm(x11_CO_ratio ~ 
                   x1_year, data = dataf)
summary(lm.CO)
confint(lm.CO)


lm.NO2 = lm(x12_NO2_ratio ~ 
             x1_year, data = dataf)
summary(lm.NO2)

lm.ozone = lm(x13_ozone_ratio ~ 
             x1_year, data = dataf)
summary(lm.ozone)


lm.SO2 = lm(x14_SO2_ratio ~ 
             x1_year, data = dataf)
summary(lm.SO2)

lm.PM2.5 = lm(x15_PM2.5_ratio ~ 
             x1_year, data = dataf)
summary(lm.PM2.5)

lm.PM10 = lm(x16_PM10_ratio ~ 
             x1_year, data = dataf)
summary(lm.PM10)

lm.male_mort = lm(male_mort_y ~ 
                    x1_year, data = dataf)
summary(lm.male_mort)

lm.female_mort =lm(female_mort_y ~ 
                     x1_year, data = dataf) 
summary(lm.female_mort)

lm.medAQI = lm(x10_median_AQI ~ 
                x1_year, data = dataf)
summary(lm.medAQI)

confint(lm.CO)
confint(lm.NO2)
confint(lm.ozone)
confint(lm.SO2)
confint(lm.PM2.5)
confint(lm.PM10)
confint(lm.male_mort)
confint(lm.female_mort)
confint(lm.medAQI)

library(leaps)


#To fit the null model (The model with only the intercept)
null_comb=lm(combined_mort_y~1, data=data)
null_male = lm(male_mort_y~1, data=data)
null_female = lm(female_mort_y~1, data=data)


#We can perform forward selection using the command:
step(null_comb, scope=list(lower=null_comb, upper=full_comb), direction="forward",level="0.01")
step(null_male, scope=list(lower=null_comb, upper=male_comb), direction="forward",level="0.01")
step(null_female, scope=list(lower=null_comb, upper=female_comb), direction="forward",level="0.01")

#We can perform backward selection using the command:
step(full_comb, data = dataf, direction="backward",level="0.05")
step(male_comb, data = dataf, direction="backward",level="0.05")
step(female_comb, data = dataf, direction="backward",level="0.05")



#plot(leaps, scale="adjr2")
#plot(leaps, scale="bic")

#plot(leaps, scale="r2")
#plot(leaps, scale="Cp")




smallcomb = lm(combined_mort_y~x1_year+x8_maxAQI+x10_median_AQI)
malecomb = lm(male_mort_y~x1_year+x8_maxAQI+x10_median_AQI)
femalecomb = lm(female_mort_y~x1_year+x8_maxAQI+x10_median_AQI)

summary(smallcomb)
summary(malecomb)
summary(femalecomb)


# ridge regression
dataf_c =data.frame(combined_mort_y,
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

dataf_m =data.frame(male_mort_y,
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

dataf_f =data.frame(female_mort_y,
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



library(glmnet)
grid = 10^seq(10, -2, length=100)

#combined mortality
x = model.matrix(combined_mort_y~.,dataf_c)
y =dataf$combined_mort_y

ridge.mod = glmnet(x,y,alpha=0, lambda = grid) 
set.seed(1)
train = sample(1:nrow(x), nrow(x)/2)
test  = (-train)
y.test = y[test]
ridge.mod = glmnet(x[train,], y[train], alpha=0, lambda=grid, thresh=1e-12)
ridge.pred=predict(ridge.mod, s=4, newx=x[test,])
mean((ridge.pred-y.test)^2)
ridge.pred=predict(ridge.mod, s=1000000, newx=x[test,])
mean((ridge.pred-y.test)^2)

# use cross-validation to choose lambda
cv.out = cv.glmnet(x[train,], y[train], alpha=0)
plot(cv.out)
bestlam=cv.out$lambda.min
bestlam
ridge.pred=predict(ridge.mod, s=bestlam, newx=x[test,])
mean((ridge.pred-y.test)^2)

out=glmnet(x,y,alpha=0)
predict(out, type="coefficients", s=bestlam)[1:19,]

#male mortality
x = model.matrix(male_mort_y~.,dataf_m)
y =dataf$male_mort_y

ridge.mod = glmnet(x,y,alpha=0, lambda = grid) 
set.seed(1)
train = sample(1:nrow(x), nrow(x)/2)
test  = (-train)
y.test = y[test]
ridge.mod = glmnet(x[train,], y[train], alpha=0, lambda=grid, thresh=1e-12)
ridge.pred=predict(ridge.mod, s=4, newx=x[test,])
mean((ridge.pred-y.test)^2)
ridge.pred=predict(ridge.mod, s=1000000, newx=x[test,])
mean((ridge.pred-y.test)^2)

# use cross-validation to choose lambda
cv.out = cv.glmnet(x[train,], y[train], alpha=0)
plot(cv.out)
bestlam=cv.out$lambda.min
bestlam
ridge.pred=predict(ridge.mod, s=bestlam, newx=x[test,])
mean((ridge.pred-y.test)^2)

out=glmnet(x,y,alpha=0)
predict(out, type="coefficients", s=bestlam)[1:19,]

# female mortality
x = model.matrix(female_mort_y~.,dataf_f)
y =dataf$female_mort_y

ridge.mod = glmnet(x,y,alpha=0, lambda = grid) 
set.seed(1)
train = sample(1:nrow(x), nrow(x)/2)
test  = (-train)
y.test = y[test]
ridge.mod = glmnet(x[train,], y[train], alpha=0, lambda=grid, thresh=1e-12)
ridge.pred=predict(ridge.mod, s=4, newx=x[test,])
mean((ridge.pred-y.test)^2)
ridge.pred=predict(ridge.mod, s=1000000, newx=x[test,])
mean((ridge.pred-y.test)^2)

# use cross-validation to choose lambda
cv.out = cv.glmnet(x[train,], y[train], alpha=0)
plot(cv.out)
bestlam=cv.out$lambda.min
bestlam
ridge.pred=predict(ridge.mod, s=bestlam, newx=x[test,])
mean((ridge.pred-y.test)^2)

out=glmnet(x,y,alpha=0)
predict(out, type="coefficients", s=bestlam)[1:19,]



#lasso

#combined mortality
x = model.matrix(combined_mort_y~.,dataf_c)
y =dataf$combined_mort_y

lasso.mod = glmnet(x[train,],y[train], alpha=1, lambda=grid)
plot(lasso.mod)


set.seed(1)
cv.out = cv.glmnet(x[train,],y[train],alpha=1)
plot(cv.out)
bestlam = cv.out$lambda.min
lasso.pred=predict(lasso.mod, s=bestlam, newx=x[test,])
mean((lasso.pred-y.test)^2)
out = glmnet(x,y,alph=1, lambda=grid)
lasso.coef = predict(out, type="coefficients", s=bestlam)[1:19,]
lasso.coef

#male mortality
x = model.matrix(male_mort_y~.,dataf_m)
y =dataf$male_mort_y

lasso.mod = glmnet(x[train,],y[train], alpha=1, lambda=grid)
plot(lasso.mod)


set.seed(1)
cv.out = cv.glmnet(x[train,],y[train],alpha=1)
plot(cv.out)
bestlam = cv.out$lambda.min
lasso.pred=predict(lasso.mod, s=bestlam, newx=x[test,])
mean((lasso.pred-y.test)^2)
out = glmnet(x,y,alph=1, lambda=grid)
lasso.coef = predict(out, type="coefficients", s=bestlam)[1:19,]
lasso.coef

# female mortality
x = model.matrix(female_mort_y~.,dataf_f)
y =dataf$female_mort_y

lasso.mod = glmnet(x[train,],y[train], alpha=1, lambda=grid)
plot(lasso.mod)


set.seed(1)
cv.out = cv.glmnet(x[train,],y[train],alpha=1)
plot(cv.out)
bestlam = cv.out$lambda.min
lasso.pred=predict(lasso.mod, s=bestlam, newx=x[test,])
mean((lasso.pred-y.test)^2)
out = glmnet(x,y,alph=1, lambda=grid)
lasso.coef = predict(out, type="coefficients", s=bestlam)[1:19,]
lasso.coef

