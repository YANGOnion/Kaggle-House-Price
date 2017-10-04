

trainOneModel=function(data,method){
  require(data.table)
  require(caret)
  require(doParallel)
  data[,id:=seq(1,.N,1)]
  train=data[!is.na(SalePrice)]
  train[,SalePrice:=log(SalePrice)]
  test=data[is.na(SalePrice)]
  set.seed(1)
  folds=createFolds(y=train$SalePrice,k=5,list=T,returnTrain=TRUE)
  cluster <- makeCluster(detectCores() - 1)
  registerDoParallel(cluster)
  ## train folds
  fitControl <- trainControl(method = "repeatedcv",number = 10,repeats = 10,allowParallel = TRUE)
  trainOneFold=function(fold,dt_feature){
    training=train[fold]
    testing=train[-fold]
    nz=which(nearZeroVar(training,saveMetrics = TRUE)$zeroVar==T)
    if(length(nz)!=0){
      training=training[,!nz,with=F]
      testing=testing[,!nz,with=F]
    }
    testing=replaceNewLevel(training,testing)
    model=train(SalePrice~.,method=method,data=training[,!c("id")],trControl = fitControl)
    predFold=data.table(id=testing[,id],SalePrice=predict(model,testing))
    return(predFold)
  }
  train_feature=rbindlist(lapply(folds,FUN=trainOneFold,dt_feature=train))
  train_feature=train_feature[order(id)]
  ## train all
  model_all=model=train(SalePrice~.,method=method,data=train,trControl = fitControl)
  stopCluster(cluster)
  registerDoSEQ()
  ## predict in test
  test=replaceNewLevel(train,test)
  test_feature=data.table(id=test[,id],SalePrice=predict(model_all,test))
  return(list(train_feature=train_feature,test_feature=test_feature))
}

trainOneModel(data,"lm")

