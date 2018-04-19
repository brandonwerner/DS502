library(FNN)
library(gam)
library(tree)
library(glmnet)
library(pls)
source("util.R")

# import csv file
################## RATIOS #########################
df=read.csv("data_cleaned/training_mortality_subtract.csv", header = TRUE)
outcome="sub"
################## RATES  #########################
#df<-read.csv("data_cleaned/training_mortality_rates.csv", header = TRUE)
#outcome="rate"
#data
#dim(df)
names(df)
#head(df)
#scaling?
#df=scale(df)
#df
#reorder by column index
#first col is response
#include all predictors
dfc <- df[c(2,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23)]
dff <- df[c(3,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23)]
dfm <- df[c(4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23)]

#####################################
### SET THE PREDICTOR YOU WANT #####
#####################################

#combined
#dfc<-dfc
#target="combined"
#female
dfc<-dff
target="female"
#male
#dfc<-dfm
#target="male"

names(dfc)
#can choose different sets of features
#dff <- df[c(5,2,14,17,18,19,20,21,22,23,24)]
#dfm <- df[c(6,2,7,14,17,18,19,20,21,22,23,24)]

#split into tuning and evaluating sets
set.seed(1234)
idx = sample(nrow(df), nrow(df)*0.5)
dfc_tune = dfc[idx,1:ncol(dfc)]
dfc_eval = dfc[-idx,1:ncol(dfc)]
#dff_tune = dff[idx,1:ncol(dff)]
#dff_eval = dff[-idx,1:ncol(dff)]
#dfm_tune = dfm[idx,1:ncol(dfm)]
#dfm_eval = dfm[-idx,1:ncol(dfm)]

#dfs = list(dfc, dff, dfm)
#tunes=list(dfc_tune,dff_tune,dfm_tune)
#evals=list(dfc_eval,dff_eval,dfm_eval)
#head(dfc)

############################################ ANNUAL MEAN ##########################################
#print("Get MEAN MODEL MSE")
ec = mean((df$combined_mort_y - 1)^2)
ec
ef = mean((df$female_mort_y - 1)^2)
ef
em = mean((df$male_mort_y - 1)^2)
em
################################################ KNN ##############################################

ks = c(1,3,5,10,20,30,50,100)
#ks = c(1,3,5,7,9,10,11,15,18,20)

# Tune k
kerrors=rep(0,length(ks))
for (i in 1:length(ks)){
    kerrors[i]=cv_knn(dfc, ks[i], 10)
}
jpeg(paste('figures/knn_varyk',target,outcome,".jpg", sep="_" ))
plot(ks, kerrors, type="l")
dev.off()

#print(ks)
#print(kerrors)
#print(min(kerrors))

#c best 9
#f best 9
#m best 10

#7 also good enough prob for all

bestK = ks[which(min(kerrors) == kerrors)[[1]]]
print("Best K")
print(bestK)
#Evaluate
print("KNN CROSS VAL MSE")
print(cv_knn(dfc_eval, bestK, 10))
#plot training error
fit = knn.reg(train=dfc_eval[,3:length(dfc_eval)-1], test=dfc_eval[,3:length(dfc_eval)-1], y=dfc_eval[,1], k = bestK)
#compute MSE
jpeg(paste('figures/knn_pred',target,outcome,".jpg", sep="_" ))
plot_res(dfc_eval[,1], fit$pred)
dev.off()




################################################ GAM ##############################################

# Tune degree
degs = c(1,2,3,5,7,9)
gerrors=rep(0,length(degs))
for (i in 1:length(degs)){
  gerrors[i]=cv_gam(dfc_tune, degs[i], 10)
}
jpeg(paste('figures/gam_varydeg',target,outcome,".jpg", sep="_" ))
plot(degs, gerrors,type="l")
dev.off()

best_deg = degs[which(min(gerrors) == gerrors)[[1]]]
print("Best Degree")
print(best_deg)
#Evaluate 
print(" gam dfc mse")
dfc_mse = cv_gam(dfc_eval, best_deg, 10)
print(dfc_mse)


#plot training error
fit_gam = gam(as.formula(paste(paste(colnames(dfc)[1], "~ s(", 
                                     paste(colnames(dfc)[3:length(dfc)-1], collapse = 
                                             paste(",",toString(best_deg), ") + s(", sep=""),sep = "")),
                               paste(",",toString(best_deg), ")", sep=""))), data=dfc)

summary(fit_gam)
jpeg(paste('figures/gam_features',target,outcome,".jpg", sep="_" ))
par(mfrow=c(4,5))
plot(fit_gam)
dev.off()

jpeg(paste('figures/gam_pred',target,outcome,".jpg", sep="_" ))
par(mfrow=c(1,1))
plot_res(dfc_eval[,1], predict(fit_gam, dfc_eval))
dev.off()



################################################ Least Squares ##############################################

# Nothing to tune!

#Evaluate 
print("least squares alldfc mse")
dfc_mse = cv_lm(dfc, 10)
print(dfc_mse)

#plot training error
fit_lm = lm(as.formula(paste(colnames(dfc)[1], "~", 
                              paste(colnames(dfc)[2:length(dfc)], 
                                    collapse =  "+"), sep="")), data=dfc)
summary(fit_lm)


#jpeg(paste('figures/least_squares_residuals',target,outcome,".jpg", sep="_" ))
#par(mfrow=c(2,2))
#plot(fit_lm)
#dev.off()


#jpeg(paste('figures/least_squares_pred',target,outcome,".jpg", sep="_" ))
#par(mfrow=c(1,1))
#plot_res(dfc[,1], predict(fit_lm, dfc))
#dev.off()


#Should drop Days.PM10 ? lm shows it is linear combo of other predictor 
#names(dfc)
dfc_try = dfc[c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,19,20)]
names(dfc_try)

print("least squares no PM10 dfc mse")
dfc_mse = cv_lm(dfc_try, 10)
print(dfc_mse)

fit_lm = lm(as.formula(paste(colnames(dfc_try)[1], "~", 
                              paste(colnames(dfc_try)[2:length(dfc_try)], 
                                    collapse =  "+"), sep="")), data=dfc_try)
summary(fit_lm)


jpeg(paste('least_squares_residuals',target,outcome,".jpg", sep="_" ))
par(mfrow=c(2,2))
plot(fit_lm)
dev.off()


jpeg(paste('figures/least_squares_pred',target,outcome,".jpg", sep="_" ))
par(mfrow=c(1,1))
plot_res(dfc[,1], predict(fit_lm, dfc_try))
dev.off


#dfc_try = dfc_try[c(1,2,17)]
#names(dfc_try)
#fit_lm = lm(as.formula(paste(colnames(dfc_try)[1], "~", 
#                             paste(colnames(dfc_try)[2:3], 
#                                   collapse =  "+"), sep="")), data=dfc_try)
#summary(fit_lm)

#jpeg(paste('figures/least_squares_residual_year_pm25',target,outcome,".jpg", sep="_" ))
#par(mfrow=c(2,2))
#plot(fit_lm)
#dev.off()


jpeg(paste('figures/least_squares_pred_year_pm25',target,outcome,".jpg", sep="_" ))
par(mfrow=c(1,1))
plot_res(dfc[,1], predict(fit_lm, dfc_try))
dev.off()
print("least squares year, pm25 mse")
mean((predict(fit_lm, dfc_try) - dfc[,1])^2)


################################################ Ridge ##############################################

# use cross-validation to choose lambda
ridge = cv.glmnet(as.matrix(dfc_tune[2:length(dfc_tune)]), as.matrix(dfc_tune[[1]]), alpha=0)
ridgeLambda = ridge$lambda.min
print("lambda")
print(ridgeLambda)

#Evaluate 
print("ridge dfc mse")
dfc_mse = cv_ridge(dfc_eval, ridgeLambda, 10)
print(dfc_mse)

#look at model
fit_r<-glmnet(as.matrix(dfc_eval[2:length(dfc_eval)]), as.matrix(dfc_eval[[1]]), alpha=0)
# Mean Squared Error
pred = predict(fit_r, as.matrix(dfc_eval[2:length(dfc_eval)]), type="response", s=ridgeLambda)
sum = predict(fit_r, as.matrix(dfc_eval[2:length(dfc_eval)]), type="coefficients", s=ridgeLambda)
summary(sum)

summary(fit_r)

jpeg(paste('figures/ridge_paths',target,outcome,".jpg", sep="_" ))
par(mfrow=c(1,1))
plot(fit_r)
dev.off()


jpeg(paste('figures/ridge_pred',target,outcome,".jpg", sep="_" ))
par(mfrow=c(1,1))
plot_res(dfc_eval[,1], pred)
dev.off()


################################################ Lasso ##############################################


# lasso regression
# use cross-validation to choose lambda
lasso = cv.glmnet(as.matrix(dfc_tune[2:length(dfc_tune)]), as.matrix(dfc_tune[[1]]), alpha=1)
lassoLambda = lasso$lambda.min
print("lambda")
print(lassoLambda)

#Evaluate 
print("lasso dfc mse")
dfc_mse = cv_lasso(dfc_eval, lassoLambda, 10)
print(dfc_mse)

#look at coefs
fit_l<-glmnet(as.matrix(dfc_eval[2:length(dfc_eval)]), as.matrix(dfc_eval[[1]]), alpha=1)
# Mean Squared Error
pred_l = predict(fit_l, as.matrix(dfc_eval[2:length(dfc_eval)]), type="response", s=lassoLambda)
sum = predict(fit_l, as.matrix(dfc_eval[2:length(dfc_eval)]), type="coefficients", s=lassoLambda)
summary(sum)

jpeg(paste('figures/lasso_paths',target,outcome,".jpg", sep="_" ))
par(mfrow=c(1,1))
plot(fit_l)
dev.off()

jpeg(paste('figures/lasso_pred',target,outcome,".jpg", sep="_" ))
par(mfrow=c(1,1))
plot_res(dfc_eval[,1], pred_l)
dev.off()


################################################ PCR ##############################################

# Tune num comps
tune_pcr = pcr(as.formula(paste(colnames(dfc_tune)[1], "~", 
                               paste(colnames(dfc_tune)[2:length(dfc_tune)], 
                                     collapse =  "+"), sep="")), data=dfc_tune)
summary(tune_pcr)
jpeg(paste('figures/pcr_ncomps_mse',target,outcome,".jpg", sep="_" ))
validationplot(tune_pcr, val.type="MSEP")
dev.off()
jpeg(paste('figures/pcr_ncomps_R2',target,outcome,".jpg", sep="_" ))
validationplot(tune_pcr, val.type="R2")
dev.off()
msep =MSEP(tune_pcr)
print(msep)
r2 = R2(tune_pcr)
print(r2)

m = msep$val[1,1, ]
bestNcomp = m[which(min(m) == m)[[1]]]
print("Best num comps")
print(bestNcomp)

#print("pcr 5 comps dfc mse")
#dfc_mse = cv_pcr(dfc_eval, 5, 10)
#print(dfc_mse)
print("pcr dfc mse")
dfc_mse = cv_pcr(dfc_eval, bestNcomp, 10)
print(dfc_mse)

fit_pcr = pcr(as.formula(paste(colnames(dfc)[1], "~", 
                           paste(colnames(dfc)[2:length(dfc)], 
                                 collapse =  "+"), sep="")), data=dfc)


# Training error
summary(fit_pcr)
par(mfrow=c(1,1))
pred = pred = predict(fit_pcr, ncomp = bestNcomp, newdata=dfc)
#need to plot by hand
jpeg(paste('figures/pcr_pred',target,outcome,".jpg", sep="_" ))
plot_res(dfc[,1], as.matrix(pred))
dev.off()

#pred = pred = predict(fit_pcr, ncomp = 15, newdata=dfc)
#jpeg(paste('figures/pcr_15',target,outcome,".jpg", sep="_" ))
#plot_res(dfc[,1], as.matrix(pred))
#dev.off()
########################################## TREE #########################################
#Nothing to tune -- trees use few features, so no need to prune
#Evaluate 
print("tree dfc mse")
dfc_mse = cv_tree(dfc, 10)
print(dfc_mse)
tr = tree(as.formula(paste(colnames(dfc)[1], "~", 
                             paste(colnames(dfc)[2:length(dfc)], 
                                   collapse =  "+"), sep="")), data=dfc)
summary(tr)

jpeg(paste('figures/tree',target,outcome,".jpg", sep="_" ))
par(mfrow=c(1,1))
plot(tr)
text(tr, pretty=0)
dev.off()
pred = predict(tr, dfc)
jpeg(paste('figures/tree_pred',target,outcome,".jpg", sep="_" ))
plot_res(dfc[,1], pred)
dev.off()


########################################## RF #########################################
library(randomForest)
install.packages("randomForest")
#(e) Use random forests to analyze this data.
print("random forest dfc mse")
dfc_mse = cv_rf(dfc, 10)
print(dfc_mse)
rf = randomForest(as.formula(paste(colnames(dfc)[1], "~", 
                                   paste(colnames(dfc)[2:length(dfc)], 
                                         collapse =  "+"), sep="")), data=dfc)
importance(rf)
summary(rf)

jpeg(paste('figures/rf_tree',target,outcome,".jpg", sep="_" ))
par(mfrow=c(1,1))
plot(rf)
text(rf, pretty=0)
dev.off()
pred = predict(rf, dfc)
jpeg(paste('rf_figures/tree_pred',target,outcome,".jpg", sep="_" ))
plot_res(dfc[,1], pred)
dev.off()
