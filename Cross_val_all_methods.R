library(class)
library(caret)
library(FNN)
library(gam)
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
fit_lm = glm(as.formula(paste(colnames(dfc)[1], "~", 
                              paste(colnames(dfc)[3:length(dfc)-1], 
                                    collapse =  "+"), sep="")), data=dfc)
summary(fit_lm)

#Should drop Days.PM10 ? lm shows it is linear combo of other predictor 
names(dfc)
dfc_try = dfc[c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,19,10)]
names(dfc_try)
fit_lm = glm(as.formula(paste(colnames(dfc_try)[1], "~", 
                              paste(colnames(dfc_try)[3:length(dfc_try)-1], 
                                    collapse =  "+"), sep="")), data=dfc_try)
summary(fit_lm)
par(mfrow=c(2,2))
plot(fit_lm)

par(mfrow=c(1,1))
plot_res(dfc[,1], predict(fit_lm, dfc))









