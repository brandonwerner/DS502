library(class)
library(caret)
library(FNN)
library(gam)
library(tree)
library(glmnet)
library(pls)
source("util.R")

# import csv file
df=read.csv("data_cleaned/training_mortality_ratios.csv", header = TRUE)
#data
#dim(df)
names(df)

#scaling?
#df=scale(df)
#df
#reorder by column index
#first col is response
#include all predictors
dfc <- df[c(4,2,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24)]
dff <- df[c(5,2,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24)]
dfm <- df[c(6,2,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24)]

#can choose different sets of features
#dff <- df[c(5,2,14,17,18,19,20,21,22,23,24)]
#dfm <- df[c(6,2,7,14,17,18,19,20,21,22,23,24)]

#split into tuning and evaluating sets
set.seed(1234)
idx = sample(nrow(df), nrow(df)*0.5)
dfc_tune = dfc[idx,1:ncol(dfc)]
dfc_eval = dfc[-idx,1:ncol(dfc)]
dff_tune = dff[idx,1:ncol(dff)]
dff_eval = dff[-idx,1:ncol(dff)]
dfm_tune = dfm[idx,1:ncol(dfm)]
dfm_eval = dfm[-idx,1:ncol(dfm)]

dfs = list(dfc, dff, dfm)
tunes=list(dfc_tune,dff_tune,dfm_tune)
evals=list(dfc_eval,dff_eval,dfm_eval)
#head(dfc)



################################################ KNN ##############################################

#ks = c(1,3,5,10,20,30,50,100)
ks = c(1,3,5,7,9,10,11,15,18,20)

# Tune k
for(j in 1:length(tunes)){
  df=tunes[[j]]
  kerrors=rep(0,length(ks))
  for (i in 1:length(ks)){
    kerrors[i]=cv_knn(df, ks[i], 10)
  }
  #plot(ks, kerrors, type="l")
  print(ks)
  print(kerrors)
  print(min(kerrors))
}

#c best 9
#f best 9
#m best 10

#7 also good enough prob for all

#best k=7

#Evaluate 
print("dfc mse")
print(cv_knn(dfc_eval, 7, 10))
print("dff mse")
print(cv_knn(dff_eval, 7, 10))
print("dfm mse")
print(cv_knn(dfm_eval, 9, 10))
#plot training error
fit = knn.reg(train=dfc_eval[,3:length(dfc_eval)-1], test=dfc_eval[,3:length(dfc_eval)-1], y=dfc_eval[,1], k = 7)
#compute MSE
plot_res(dfc_eval[,1], fit$pred)

ks
ks[which(min(kerrors) == kerrors)[[1]]]



################################################ GAM ##############################################

# Tune degree
#ks = c(1,3,5,10,20,30,50,100)
degs = c(1,2,3,5,7,9)
gerrors=rep(0,length(degs))
for (i in 1:length(degs)){
  gerrors[i]=cv_gam(dfc_tune, degs[i], 10)
}
plot(degs, gerrors,type="l")

best_deg = degs[which(min(gerrors) == gerrors)[[1]]]

#Evaluate 
print("dfc mse")
dfc_mse = cv_gam(dfc_eval, best_deg, 10)
print(dfc_mse)
#print("dff mse")
#print(cv_knn(dff_eval, 7, 10))
#print("dfm mse")
#print(cv_knn(dfm_eval, 9, 10))


#plot training error
fit_gam = gam(as.formula(paste(paste(colnames(df)[1], "~ s(", 
                                     paste(colnames(df)[3:length(df)-1], collapse = 
                                             paste(",",toString(best_deg), ") + s(", sep=""),sep = "")),
                               paste(",",toString(best_deg), ")", sep=""))), data=df)

summary(fit_gam)
par(mfrow=c(4,5))
plot(fit_gam)
par(mfrow=c(1,1))
plot_res(dfc_eval[,1], predict(fit_gam, dfc_eval))



################################################ Least Squares ##############################################



# Nothing to tune!

#Evaluate 
print("dfc mse")
dfc_mse = cv_lm(dfc, 10)
print(dfc_mse)
#print("dff mse")
#print(cv_knn(dff_eval, 7, 10))
#print("dfm mse")
#print(cv_knn(dfm_eval, 9, 10))


#plot training error
fit_lm = lm(as.formula(paste(colnames(dfc)[1], "~", 
                              paste(colnames(dfc)[3:length(dfc)-1], 
                                    collapse =  "+"), sep="")), data=dfc)
summary(fit_lm)
par(mfrow=c(2,2))
plot(fit_lm)

par(mfrow=c(1,1))
plot_res(dfc[,1], predict(fit_lm, dfc))

#Should drop Days.PM10 ? lm shows it is linear combo of other predictor 
names(dfc)
dfc_try = dfc[c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,19,20)]
names(dfc_try)
fit_lm = lm(as.formula(paste(colnames(dfc_try)[1], "~", 
                              paste(colnames(dfc_try)[3:length(dfc_try)-1], 
                                    collapse =  "+"), sep="")), data=dfc_try)
summary(fit_lm)
par(mfrow=c(2,2))
plot(fit_lm)

par(mfrow=c(1,1))
plot_res(dfc[,1], predict(fit_lm, dfc_try))
mean((predict(fit_lm, dfc_try) - dfc_try$combined_mort_y)^2)


dfc_try = dfc[c(1,3,11,12,13,14,15,17,19,20)]
names(dfc_try)
fit_lm = lm(as.formula(paste(colnames(dfc_try)[1], "~", 
                             paste(colnames(dfc_try)[3:length(dfc_try)-1], 
                                   collapse =  "+"), sep="")), data=dfc_try)
summary(fit_lm)
par(mfrow=c(2,2))
plot(fit_lm)

par(mfrow=c(1,1))
plot_res(dfc[,1], predict(fit_lm, dfc_try))
mean((predict(fit_lm, dfc_try) - dfc_try$combined_mort_y)^2)


################################################ Ridge ##############################################

# use cross-validation to choose lambda
ridge = cv.glmnet(as.matrix(dfc_tune[2:length(dfc_tune)]), as.matrix(dfc_tune[[1]]), alpha=0)
ridgeLambda = ridge$lambda.min
print("lambda")
print(ridgeLambda)

#Evaluate 
print("dfc mse")
dfc_mse = cv_ridge(dfc_eval, ridgeLambda, 10)
print(dfc_mse)
#print("dff mse")
#print(cv_knn(dff_eval, 7, 10))
#print("dfm mse")
#print(cv_knn(dfm_eval, 9, 10))

#plot training error
fit_r<-glmnet(as.matrix(dfc_tune[2:length(dfc_eval)]), as.matrix(dfc_eval[[1]]), alpha=0)
# Mean Squared Error
pred = predict(fit_r, as.matrix(dfc_eval[2:length(dfc_eval)]), type="response", s=ridgeLambda)
summary(fit_r)
par(mfrow=c(2,2))
plot(fit_r)

par(mfrow=c(1,1))
plot_res(dfc_eval[,1], pred)


################################################ Lasso ##############################################


# lasso regression
# use cross-validation to choose lambda
lasso = cv.glmnet(as.matrix(dfc_tune[2:length(dfc_tune)]), as.matrix(dfc_tune[[1]]), alpha=1)
lassoLambda = lasso$lambda.min
print("lambda")
print(lassoLambda)

#Evaluate 
print("dfc mse")
dfc_mse = cv_lasso(dfc_eval, lassoLambda, 10)
print(dfc_mse)
#print("dff mse")
#print(cv_knn(dff_eval, 7, 10))
#print("dfm mse")
#print(cv_knn(dfm_eval, 9, 10))

#plot training error
fit_l<-glmnet(as.matrix(dfc_tune[2:length(dfc_eval)]), as.matrix(dfc_eval[[1]]), alpha=1)
# Mean Squared Error
pred_l = predict(fit_l, as.matrix(dfc_eval[2:length(dfc_eval)]), type="response", s=lassoLambda)
summary(fit_l)
par(mfrow=c(2,2))
plot(fit_l)

par(mfrow=c(1,1))
plot_res(dfc_eval[,1], pred_l)


################################################ PCR ##############################################

# Tune num comps
tune_pcr = pcr(as.formula(paste(colnames(dfc_tune)[1], "~", 
                               paste(colnames(dfc_tune)[2:length(dfc_tune)], 
                                     collapse =  "+"), sep="")), data=dfc_tune)
summary(tune_pcr)
validationplot(tune_pcr, val.type="MSEP")
validationplot(tune_pcr, val.type="R2")
MSEP(fit)
R2(fit)
#try 7 or 15 comps

print("dfc mse")
dfc_mse = cv_pcr(dfc_eval, 7, 10)
print(dfc_mse)
dfc_mse = cv_pcr(dfc_eval, 15, 10)
print(dfc_mse)

fit_pcr = pcr(as.formula(paste(colnames(dfc)[1], "~", 
                           paste(colnames(dfc)[2:length(dfc)], 
                                 collapse =  "+"), sep="")), data=dfc)


# Training error
summary(fit_pcr)
par(mfrow=c(1,1))
pred = pred = predict(fit_pcr, ncomp = 7, newdata=dfc)
plot_res(dfc$combined_mort_y, as.matrix(pred))
pred = pred = predict(fit_pcr, ncomp = 15, newdata=dfc)
plot_res(dfc$combined_mort_y, as.matrix(pred))


########################################## TREE #########################################
#Nothing to tune -- trees use few features, so no need to prune
#Evaluate 
print("dfc mse")
dfc_mse = cv_tree(dfc, 10)
print(dfc_mse)
?tree
tr = tree(as.formula(paste(colnames(dfc)[1], "~", 
                             paste(colnames(dfc)[2:length(dfc)], 
                                   collapse =  "+"), sep="")), data=dfc)
# Mean Squared Error
err = mean((predict(tr, dfc) - dfc$combined_mort_y)^2)  
summary(tr)
par(mfrow=c(1,1))
plot(tr)
text(tr, pretty=0)
pred = predict(tr, dfc)
mean((pred-dfc$combined_mort_y)^2)
plot_res(dfc$combined_mort_y, pred)



