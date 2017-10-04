
#!/usr/bin/Rscript
setwd("/home/jiaoy/ywc/R/House_Price")
library(caret)
library(data.table)
data=fread("data/data_cart.csv")
data[,`:=`(MSSubClass=as.character(MSSubClass),MoSold=as.character(MoSold),YrSoldFac=as.character(YrSoldFac))]
#data=data[c(1:100,2900:2919),c(3,4,6,81)]

replaceNewLevel=function(training,testing){
  require(data.table)
  testing=testing[,lapply(1:ncol(testing),FUN=function(x){
    newCol=testing[[x]]
    if(is.character(testing[[x]])){
      newLevel=setdiff(unique(testing[[x]]),unique(training[[x]]))
      if(length(newLevel)!=0)
        newCol[testing[[x]]%in%newLevel]=names(table(training[[x]]))[order(table(training[[x]]),decreasing=T)[1]]
    }
    return(newCol)
  })]
  names(testing)=names(training)
  return(testing)
}

trainOneModel=function(data,method){
  require(data.table)
  require(caret)
  require(doMC)
  data[,id:=seq(1,.N,1)]
  train=data[!is.na(SalePrice)]
  train[,SalePrice:=log(SalePrice)]
  test=data[is.na(SalePrice)]
  set.seed(1)
  folds=createFolds(y=train$SalePrice,k=5,list=T,returnTrain=TRUE)
  ## train folds
  fitControl <- trainControl(method = "repeatedcv",number = 10,repeats = 10,allowParallel = TRUE)
  trainOneFold=function(fold,dt_feature){
    training=train[fold]
    testing=train[-fold]
    # removeZeroVar
    nz=which(nearZeroVar(training,saveMetrics = TRUE)$zeroVar==T)
    if(length(nz)!=0){
      training=training[,!nz,with=F]
      testing=testing[,!nz,with=F]
    }
    testing=replaceNewLevel(training,testing)
    registerDoMC(cores = 5)
    model=train(SalePrice~.,method=method,data=training[,!c("id")],trControl = fitControl)
    predFold=data.table(id=testing[,id],SalePrice=predict(model,testing))
    return(predFold)
  }
  train_feature=rbindlist(lapply(folds,FUN=trainOneFold,dt_feature=train))
  train_feature=train_feature[order(id)]
  ## train all
  registerDoMC(cores = 5)
  model_all=model=train(SalePrice~.,method=method,data=train,trControl = fitControl)
  ## predict in test
  test=replaceNewLevel(train,test)
  test_feature=data.table(id=test[,id],SalePrice=predict(model_all,test))
  return(list(train_feature=train_feature,test_feature=test_feature))
}

# trainOneModel=function(data,method){
#   require(data.table)
#   require(caret)
#   require(doParallel)
#   data[,id:=seq(1,.N,1)]
#   train=data[!is.na(SalePrice)]
#   train[,SalePrice:=log(SalePrice)]
#   test=data[is.na(SalePrice)]
#   set.seed(1)
#   folds=createFolds(y=train$SalePrice,k=5,list=T,returnTrain=TRUE)
#   cluster <- makeCluster(detectCores() - 1)
#   registerDoParallel(cluster)
#   ## train folds
#   fitControl <- trainControl(method = "repeatedcv",number = 10,repeats = 10,allowParallel = TRUE)
#   trainOneFold=function(fold,dt_feature){
#     training=train[fold]
#     testing=train[-fold]
#     nz=which(nearZeroVar(training,saveMetrics = TRUE)$zeroVar==T)
#     if(length(nz)!=0){
#       training=training[,!nz,with=F]
#       testing=testing[,!nz,with=F]
#     }
#     testing=replaceNewLevel(training,testing)
#     model=train(SalePrice~.,method=method,data=training[,!c("id")],trControl = fitControl)
#     predFold=data.table(id=testing[,id],SalePrice=predict(model,testing))
#     return(predFold)
#   }
#   train_feature=rbindlist(lapply(folds,FUN=trainOneFold,dt_feature=train))
#   train_feature=train_feature[order(id)]
#   ## train all
#   model_all=model=train(SalePrice~.,method=method,data=train,trControl = fitControl)
#   stopCluster(cluster)
#   registerDoSEQ()
#   ## predict in test
#   test=replaceNewLevel(train,test)
#   test_feature=data.table(id=test[,id],SalePrice=predict(model_all,test))
#   return(list(train_feature=train_feature,test_feature=test_feature))
# }

xgb_feature=trainOneModel(data,"xgbTree")
save(xgb_feature,file="model/xgb_feature")

rf_feature=trainOneModel(data,"rf")
save(rf_feature,file="model/rf_feature")

gbm_feature=trainOneModel(data,"gbm")
save(gbm_feature,file="model/gbm_feature")

cor(xgb_feature$train_feature$SalePrice,rf_feature$train_feature$SalePrice)
cor(xgb_feature$train_feature$SalePrice,gbm_feature$train_feature$SalePrice)
cor(rf_feature$train_feature$SalePrice,gbm_feature$train_feature$SalePrice)

train_feature=data.table(xgb=xgb_feature$train_feature$SalePrice,rf=rf_feature$train_feature$SalePrice,
                         gbm=gbm_feature$train_feature$SalePrice,SalePrice=train[,log(SalePrice)])
test_feature=data.table(xgb=xgb_feature$test_feature$SalePrice,rf=rf_feature$test_feature$SalePrice,
                        gbm=gbm_feature$test_feature$SalePrice)

cluster <- makeCluster(detectCores() - 1)
registerDoParallel(cluster)
fitControl <- trainControl(method = "repeatedcv",number = 10,repeats = 10,allowParallel = TRUE)
set.seed(1)
system.time({
  modelFit=train(SalePrice~.,method="brnn",data=train_feature,trControl = fitControl)
})
stopCluster(cluster)
registerDoSEQ()

test[,SalePrice:=exp(predict(modelFit,test_feature))]
fwrite(test[,.(SalePrice)],"data/submit9.csv")


