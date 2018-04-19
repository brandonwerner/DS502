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
    #for methods that need a matrix not dataframe, convert
    trainf = train[,3:ncol(train)-1]
    testf = test[,3:ncol(train)-1]
    train_y = train[,1]
    test_y = test[,1]
    #fit model on training
    #use to predict testing
    fit_gam = gam(as.formula(paste(paste(colnames(df)[1], "~ s(", 
                                         paste(colnames(df)[3:length(df)-1], collapse = 
                                                 paste(",",toString(deg), ") + s(", sep=""),sep = "")),
                                   paste(",",toString(deg), ")", sep=""))), data=df)
    # Mean Squared Error
    err = mean((predict(fit_gam, testf) - test_y)^2)  
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


##Cross fold validation for lasso
#l is the lambda parameter
cv_lasso<-function(df, l, num_folds){
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
            
            

##Cross fold validation for trees
cv_tree<-function(df, deg, num_folds){
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
            
            