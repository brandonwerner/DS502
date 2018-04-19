library(class)
library(caret)
library(FNN)
library(gam)
library(tree)
library(glmnet)

#this function pretty plots the predictions against the true values
plot_res <-function(y, pred){
  lims=c(min(min(pred),min(y)), max(max(pred),max(y)))
  plot(y, pred, xlim=lims, ylim=lims)
  
}

#this function lets you do CV
get_folds<-function(X, nfolds){
  n = nrow(X)
  if(n %% nfolds != 0){
    f = floor(n/nfolds)
    n=(f+1)*nfolds
  }
  folds = rep(1:nfolds, length.out=n)
  return(sample(folds)[1:nrow(X)]) 
}

##Cross fold validation for knn
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

##Cross fold validation for gam
#deg is the degree parameter
cv_gam<-function(df, deg, num_folds){
  #init error vector
  errors = rep(0,num_folds)
  df["idx"]=get_folds(df,num_folds)
  for (fold in 1:num_folds){
    #get training and testing for this fold
    train <- df[df$idx!=fold,]
    test <- df[df$idx==fold,]
    train_y = train[,1]
    test_y = test[,1]
    #fit model on training
    #use to predict testing
    fit_gam <-gam(as.formula(paste(paste(colnames(train)[1], "~ s(", 
                                         paste(colnames(train)[3:length(train)-1], collapse = 
                                                 paste(",",toString(deg), ") + s(", sep=""),sep = "")),
                                   paste(",",toString(deg), ")", sep=""))), data=train)
    # Mean Squared Error
    err = mean((predict(fit_gam, test) - test_y)^2)  
    #put errors in a vector
    errors[fold]=err
    print(fold)
  }
  return(mean(errors))
}


##Cross fold validation for least squares
cv_lm<-function(df, num_folds){
  #init error vector
  errors = rep(0,num_folds)
  df["idx"]=get_folds(df,num_folds)
  for (fold in 1:num_folds){
    #get training and testing for this fold
    train <- df[df$idx!=fold,]
    test <- df[df$idx==fold,]
    train_y = train[,1]
    test_y = test[,1]
    #fit model on training
    #use to predict testing
    fit_lm <-glm(as.formula(paste(colnames(train)[1], "~", 
                            paste(colnames(train)[2:length(train)], 
                            collapse =  "+"), sep="")), data=train)
    # Mean Squared Error
    err = mean((predict(fit_lm, test) - test_y)^2)  
    #put errors in a vector
    errors[fold]=err
    print(fold)
  }
  return(mean(errors))
}
            
  
##Cross fold validation for ridge
#l is the lambda parameter
cv_ridge<-function(df, l, num_folds){
  #init error vector
  errors = rep(0, num_folds)
  df["idx"]=get_folds(df,num_folds)
  for (fold in 1:num_folds){
    #get training and testing for this fold
    train <- df[df$idx!=fold,]
    test <- df[df$idx==fold,]
    trainf <- train[,3:ncol(train)-1]
    testf <- test[,3:ncol(train)-1]
    train_y = train[,1]
    test_y = test[,1]
    #fit model on training
    #use to predict testing
    fit_r<-glmnet(as.matrix(trainf), as.matrix(train_y), alpha=0)
    # Mean Squared Error
    pred = predict(fit_r, newx=as.matrix(testf), type="response", s=l)
    err = mean((pred - test_y)^2)  
    #put errors in a vector
    errors[fold]=err
    print(fold)
  }
  return(mean(errors))
}


##Cross fold validation for lasso
#l is the lambda parameter
cv_lasso<-function(df, l, num_folds){
    #init error vector
    errors = rep(0, num_folds)
    df["idx"]=get_folds(df,num_folds)
    for (fold in 1:num_folds){
      #get training and testing for this fold
      train <- df[df$idx!=fold,]
      test <- df[df$idx==fold,]
      trainf <- train[,3:ncol(train)-1]
      testf <- test[,3:ncol(train)-1]
      train_y = train[,1]
      test_y = test[,1]
      #fit model on training
      #use to predict testing
      fit_r<-glmnet(as.matrix(trainf), as.matrix(train_y), alpha=1)
      # Mean Squared Error
      pred = predict(fit_r, newx=as.matrix(testf), type="response", s=l)
      err = mean((pred - test_y)^2)  
      #put errors in a vector
      errors[fold]=err
      print(fold)
    }
    return(mean(errors))
  }

cv_pcr<-function(df, l, num_folds){
  #init error vector
  errors = rep(0, num_folds)
  df["idx"]=get_folds(df,num_folds)
  for (fold in 1:num_folds){
    #get training and testing for this fold
    train <- df[df$idx!=fold,]
    test <- df[df$idx==fold,]
    trainf <- train[,3:ncol(train)-1]
    testf <- test[,3:ncol(train)-1]
    train_y = train[,1]
    test_y = test[,1]
    #fit model on training
    #use to predict testing
    fit_r<-glmnet(as.matrix(trainf), as.matrix(train_y), alpha=1)
    # Mean Squared Error
    pred = predict(fit_r, newx=as.matrix(testf), type="response", s=l)
    err = mean((pred - test_y)^2)  
    #put errors in a vector
    errors[fold]=err
    print(fold)
  }
  return(mean(errors))
}

##Cross fold validation for trees
cv_tree<-function(df, num_folds){
  #init error vector
  errors = rep(0,num_folds)
  df["idx"]=get_folds(df,num_folds)
  for (fold in 1:num_folds){
    #get training and testing for this fold
    train <- df[df$idx!=fold,]
    test <- df[df$idx==fold,]
    train_y = train[,1]
    test_y = test[,1]
    #fit model on training
    #use to predict testing
    tr = tree(as.formula(paste(colnames(train)[1], "~", 
                                  paste(colnames(train)[2:length(train)], 
                                        collapse =  "+"), sep="")), data=train)
    # Mean Squared Error
    err = mean((predict(tr, test) - test_y)^2)  
    #put errors in a vector
    errors[fold]=err
    print(fold)
  }
  return(mean(errors))
}


##Cross fold validation for random forest
cv_rf<-function(df, deg, num_folds){
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
    fit_lm = glm(as.formula(paste(colnames(dfc)[1], "~", 
                                  paste(colnames(dfc)[3:length(dfc)-1], 
                                        collapse =  "+"), sep="")), data=df)
    # Mean Squared Error
    err = mean((predict(fit_lm, testf) - test_y)^2)  
    #put errors in a vector
    errors[fold]=err
    print(fold)
  }
  return(mean(errors))
}
            
            