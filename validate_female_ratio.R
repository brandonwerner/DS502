library(FNN)
library(gam)
library(tree)
library(glmnet)
library(pls)
source("util.R")

# import csv file
################## RATIOS #########################
df=read.csv("data_cleaned/training_mortality_ratios.csv", header = TRUE)
df_val=read.csv("data_cleaned/validate_mortality_ratios.csv", header = TRUE)
outcome="ratio"
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

dfc <- df[c(3,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23)]

#####################################
### SET THE PREDICTOR YOU WANT #####
#####################################

target="female"

names(dfc)
#can choose different sets of features
#dff <- df[c(5,2,14,17,18,19,20,21,22,23,24)]
#dfm <- df[c(6,2,7,14,17,18,19,20,21,22,23,24)]

#split into tuning and evaluating sets
set.seed(1234)

################################################ KNN ##############################################

bestK = 
print("Best K")
print(bestK)
#Evaluate
print("KNN VALIDATE MSE")
#plot training error
fit = knn.reg(train=dfc[,3:length(dfc)-1], test=df_val[,3:length(dfc)-1], y=df_val[,1], k = bestK)
#compute MSE
print(mean((fit$pred - df_val[,1])^2))
jpeg(paste('figures/validate_knn',target,outcome,".jpg", sep="_" ))
plot_res(dfc[,1], fit$pred)
dev.off()




################################################ GAM ##############################################

best_deg = 


#plot training error
fit_gam = gam(as.formula(paste(paste(colnames(dfc)[1], "~ s(", 
                                     paste(colnames(dfc)[3:length(dfc)-1], collapse = 
                                             paste(",",toString(best_deg), ") + s(", sep=""),sep = "")),
                               paste(",",toString(best_deg), ")", sep=""))), data=dfc)

summary(fit_gam)
jpeg(paste('figures/gam_validation_features',target,outcome,".jpg", sep="_" ))
par(mfrow=c(4,5))
plot(fit_gam)
dev.off()

jpeg(paste('figures/validate_gam',target,outcome,".jpg", sep="_" ))
par(mfrow=c(1,1))
plot_res(dfc[,1], predict(fit_gam, df_val))
dev.off()

print("Validate gam mse")
print(mean((predict(fit_gam, df_val) - df_val[,1])^2))

################################################ Least Squares ##############################################

# Nothing to tune!

#Evaluate 


dfc_try = dfc[c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,19,20)]
names(dfc_try)

print("least squares no PM10 dfc mse")
dfc_mse = cv_lm(dfc_try, 10)
print(dfc_mse)

fit_lm = lm(as.formula(paste(colnames(dfc_try)[1], "~", 
                              paste(colnames(dfc_try)[2:length(dfc_try)], 
                                    collapse =  "+"), sep="")), data=dfc_try)
summary(fit_lm)

jpeg(paste('figures/validate_least_squares',target,outcome,".jpg", sep="_" ))
par(mfrow=c(1,1))
plot_res(dfc[,1], predict(fit_lm, dfc_val))
dev.off

print("least squares validate mse")
print(mean((predict(fit_lm, df_val) - df_val[,1])^2))


################################################ Ridge ##############################################

ridgeLambda = 
  
#look at model
fit_r<-glmnet(as.matrix(dfc[2:length(dfc)]), as.matrix(dfc[[1]]), alpha=0)

# Mean Squared Error
pred = predict(fit_r, as.matrix(df_val[2:length(df_val)]), type="response", s=ridgeLambda)
sum = predict(fit_r, as.matrix(df_val[2:length(df_val)]), type="coefficients", s=ridgeLambda)
summary(sum)

summary(fit_r)

jpeg(paste('figures/validate_ridge',target,outcome,".jpg", sep="_" ))
par(mfrow=c(1,1))
plot_res(df_val[,1], pred)
dev.off()

print("Ridge validate mse")
print(mean((predict(fit_r, df_val) - df_val[,1])^2))


################################################ Lasso ##############################################


# lasso regression
lassoLambda = 

#look at coefs
fit_l<-glmnet(as.matrix(dfc[2:length(dfc)]), as.matrix(dfc[[1]]), alpha=1)
# Mean Squared Error
pred_l = predict(fit_l, as.matrix(dfc[2:length(df_val)]), type="response", s=lassoLambda)
sum = predict(fit_l, as.matrix(dfc[2:length(df_val)]), type="coefficients", s=lassoLambda)
summary(sum)

jpeg(paste('figures/validate_lasso',target,outcome,".jpg", sep="_" ))
par(mfrow=c(1,1))
plot_res(df_val[,1], pred_l)
dev.off()

print("Ridge validate mse")
print(mean((predict(fit_l, df_val) - df_val[,1])^2))

################################################ PCR ##############################################

# Tune num comps
bestNcomp = 

fit_pcr = pcr(as.formula(paste(colnames(dfc)[1], "~", 
                           paste(colnames(dfc)[2:length(dfc)], 
                                 collapse =  "+"), sep="")), data=dfc)


# Training error
summary(fit_pcr)
par(mfrow=c(1,1))
pred = predict(fit_pcr, ncomp = bestNcomp, newdata=df_val)
#need to plot by hand
jpeg(paste('figures/validate_pcr',target,outcome,".jpg", sep="_" ))
plot_res(df_val[,1], as.matrix(pred))
dev.off()
print("PCR validate mse")
print(mean((predict(fit_pcr, df_val) - df_val[,1])^2))

########################################## TREE #########################################
#Nothing to tune -- trees use few features, so no need to prune
#Evaluate 
tr = tree(as.formula(paste(colnames(dfc)[1], "~", 
                             paste(colnames(dfc)[2:length(dfc)], 
                                   collapse =  "+"), sep="")), data=dfc)
summary(tr)

jpeg(paste('figures/tree_final',target,outcome,".jpg", sep="_" ))
par(mfrow=c(1,1))
plot(tr)
text(tr, pretty=0)
dev.off()
pred = predict(tr, df_val)
jpeg(paste('figures/validate_tree',target,outcome,".jpg", sep="_" ))
plot_res(df_val[,1], pred)
dev.off()

print("TREE validate mse")
print(mean((predict(tr, df_val) - df_val[,1])^2))

########################################## RF #########################################
library(randomForest)
install.packages("randomForest")

rf = randomForest(as.formula(paste(colnames(dfc)[1], "~", 
                                   paste(colnames(dfc)[2:length(dfc)], 
                                         collapse =  "+"), sep="")), data=dfc)
importance(rf)
summary(rf)

jpeg(paste('figures/rf_tree_final',target,outcome,".jpg", sep="_" ))
par(mfrow=c(1,1))
plot(rf)
text(rf, pretty=0)
dev.off()
pred = predict(rf, df_val)
jpeg(paste('rf_figures/validate_rf',target,outcome,".jpg", sep="_" ))
plot_res(df_val[,1], pred)
dev.off()
