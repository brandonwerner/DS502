#this function pretty plots the predictions against the true values
plot_res <-function(y, pred){
  lims=c(min(min(pred),min(y)), max(max(pred),max(y)))
  plot(y, pred, xlim=lims, ylim=lims)
  
}

#this function lets you do CV
get_folds<-function(X, nfolds){
  folds = rep(1:nfolds, length.out=nrow(X))  
  return(sample(folds)) 
}