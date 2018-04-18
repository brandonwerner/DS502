library(class)
library(caret)
library(FNN)
library(gam)
source("util.R")

# import csv file
df=read.csv("data_cleaned/training_mortality_ratios.csv", header = TRUE)
#data
dim(df)
names(df)

#reorder by column index
#first col is response
#include all predictors
#dfc <- df[c(4,2,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24)]
#dff <- df[c(5,2,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24)]
#dfm <- df[c(6,2,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24)]

dff <- df[c(5,2,14,17,18,19,20,21,22,23,24)]
dfm <- df[c(6,2,7,14,17,18,19,20,21,22,23,24)]
#split into tuning and evaluating sets
set.seed(1234)
idx = sample(nrow(df), nrow(df)*0.5)
dfc_tune = dfc[idx,1:ncol(dfc)]
dfc_eval = dfc[-idx,1:ncol(dfc)]
dff_tune = dff[idx,1:ncol(dff)]
dff_eval = dff[-idx,1:ncol(dff)]
dfm_tune = dfm[idx,1:ncol(dfm)]
dfm_eval = dfm[-idx,1:ncol(dfm)]

dfs = c(dfc, dff, dfm)
tunes=list(dfc_tune,dff_tune,dfm_tune)
evals=list(dfc_eval,dff_eval,dfm_eval)
head(dfc)
################################################ KNN ##############################################
cv_knn<-function(df, num_k, num_folds){
  #init error vector
  errors <-rep(0,num_folds)
  df["idx"]<-get_folds(df,num_folds)
  for (fold in 1:num_folds){
    #get training and testing for this fold
    train <- df[df$idx!=fold,]
    test <- df[df$idx==fold,]
    #for methods that need a matrix not dataframe, convert
    trainf <- train[,3:ncol(train)-1]
    testf <- test[,3:ncol(train)-1]
    train_y <- train[,1]
    test_y <- test[,1]
    #fit model on training
    #use to predict testing
    fit <- knn.reg(train = trainf, test = testf, y = train_y, k = num_k)
    #compute MSE
    err <- mean((test_y - fit$pred)^2)
    #put errors in a vector
    errors[fold]<-err
    #print(fold)
  }
  return(mean(errors))
}

for(j in 1:length(tunes)){
  # Tune k
  #ks = c(1,3,5,10,20,30,50,100)
  df=tunes[[j]]
  ks = c(1,3,5,7,9,10,11,15,18,20)
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
11
9
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

#can scale but it doesn't make much difference
sdf=scale(dfc)
sdf=dfc

################################################ GAM ##############################################
cv_gam<-function(df, deg, num_folds){
  #init error vector
  errors = rep(0,num_folds)
  df["idx"]=get_folds(df,num_folds)
  for (fold in 1:num_folds){
    #get training and testing for this fold
    train <- df[df$idx!=fold,]
    test <- df[df$idx==fold,]
    #for methods that need a matrix not dataframe, convert
    trainf = train[,3:ncol(train)-1]
    testf = test[,3:ncol(train)-1]
    train_y = train[,1]
    test_y = test[,1]
    #fit model on training
    #use to predict testing
    fit=gam(train_y~trainf[,1]+
                       s(trainf[,2], deg), data=trainf)  
    #compute MSE
    err = mean((test_y - fit$pred)^2)
    #put errors in a vector
    errors[fold]=err
    print(fold)
  }
  return(mean(errors))
}
errs = cv_gam(dfc, 3, 10)
errs# Tune k
#ks = c(1,3,5,10,20,30,50,100)
ks = c(1,3,5,7,9,10,11,15,18,20)
kerrors=rep(0,length(ks))
ks
for (i in 1:length(ks)){
  kerrors[i]=cv_knn(dfc_tune, ks[i], 10)
}
plot(ks, kerrors, type="l")

df=dfc
df["idx"]=get_folds(df,3)
fold=3
train <- df[df$idx!=fold,]
test <- df[df$idx==fold,]
#for methods that need a matrix not dataframe, convert
trainf = train[,3:ncol(train)-1]
testf = test[,3:ncol(train)-1]
train_y = train[,1]
test_y = test[,1]
fit=gam(train_y~trainf[,1]+
          s(trainf[,2], 3), data=trainf)

print(summary(gam.m3))
print("##############")
# Mean Squared Error
print(mean((predict(gam.m3, test) - test$female_mort_y)^2))
par(mfrow=c(1,1))
plot_res(test$female_mort_y,predict(gam.m3, test),as.factor(test$x1_year))

print("knn mse")
print(cv_knn(dfc_eval, 7, 10))




