
library(data.table)
library(ggplot2)
library(caret)
library(mice)
library(Amelia)
data=rbind(fread("data/train.csv"),fread("data/test.csv"),fill=T)
str(data)

colSums(is.na(data))
missmap(data,col=c('grey', 'steelblue'))
# Change some NAs to "None" or constant: Alley,BsmtQual,BsmtCond,BsmtExposure,BsmtFinType1,BsmtFinType2,FireplaceQu,
# GarageType,GarageYrBlt(int),GarageFinish,GarageQual,GarageCond,PoolQC,Fence,MiscFeature
NA_var=c("Alley","BsmtQual","BsmtCond","BsmtExposure","BsmtFinType1","BsmtFinType2","FireplaceQu",
         "GarageType","GarageFinish","GarageQual","GarageCond","PoolQC","Fence","MiscFeature")
data_fill=data[,NA_var,with=F]
data_fill[is.na(data_fill)]="None"
data=data[,!NA_var,with=F]
data=cbind(data,data_fill)
data[is.na(GarageYrBlt),GarageYrBlt:=0]
data[,`:=`("Id"=NULL)]
# Some numeric col should be converted to character: MSSubClass, MoSold, YrSold
data[,`:=`(MSSubClass=as.character(MSSubClass),MoSold=as.character(MoSold),YrSoldFac=as.character(YrSold))]

## Imputing method 1
# Imputing categroies variables by most common value
data=data[,lapply(.SD,FUN=function(x){
  if(is.character(x)&&any(is.na(x))){
    x[is.na(x)]=names(table(x))[order(table(x),decreasing = T)][1]
  }
  return(x)
})]
# Imputing numeric variables by knn in `caret`
preObj=preProcess(data[,-c("SalePrice")],method="knnImpute")
data=predict(preObj,data)



## Imputing method 2
# all character variables convert to factor
data=data[,lapply(.SD,FUN=function(x){
  if(is.character(x)) x=as.factor(x)
  return(x)
})]
# Imputing variables using mice
impute=mice(data[,!c("SalePrice")],method="cart")
impute$imp
data=cbind(as.data.table(complete(impute)),data[,.(SalePrice)])

# near zero variance
nz=nearZeroVar(data,saveMetrics = TRUE)

train=data[!is.na(SalePrice)]
test=data[is.na(SalePrice)]
train[,SalePrice:=log(SalePrice)]
set.seed(1)
inTrain=createDataPartition(y=train[,SalePrice],p=0.75,list=FALSE)
training=train[inTrain,]
testing=train[-inTrain,]


library(doParallel)
cluster <- makeCluster(detectCores() - 1)
registerDoParallel(cluster)
paramGrid=expand.grid(eta=c(0.25,0.3,0.35),max_depth=c(2,3,4),colsample_bytree=c(0.6,0.8,1),gamma=0,min_child_weight=1,subsample=c(0.5,0.75,1),nrounds=150)
fitControl <- trainControl(method = "repeatedcv",number = 10,repeats = 10,allowParallel = TRUE)
set.seed(1)
system.time({
  modelFit=train(SalePrice~.,method="xgbTree",data=train,trControl = fitControl,tuneGrid = paramGrid)
})
stopCluster(cluster)
registerDoSEQ()

library(ModelMetrics)
testing=testing[!Condition2%in%c("Artery","PosA")]
testing=testing[!Heating=="Floor"]
testing=testing[!HeatingQC=="Po"]
rmse(log(testing$SalePrice),log(predict(modelFit,testing)))

test[MSSubClass=="150",MSSubClass:="120"]
test[,SalePrice:=predict(modelFit,test)]
fwrite(test[,.(SalePrice)],"data/submit8.csv")


### model save
# 1 
# preprocess: "Alley","FireplaceQu","PoolQC","Fence","MiscFeature" set to NULL; 
# Categories imputed by most type; 
# Numeric imputed by knn.
# variables: All
# method: xgboost
save(modelFit,file="model/model1")

# 2
# preprocess: Change some NAs to "None" or constant: Alley,BsmtQual,BsmtCond,BsmtExposure,BsmtFinType1,BsmtFinType2,FireplaceQu,
# GarageType,GarageYrBlt(int),GarageFinish,GarageQual,GarageCond,PoolQC,Fence,MiscFeature
# Some numeric col should be converted to character: MSSubClass, MoSold, YrSold(remain original also)
# Imputing by mice and cart method
# variables: All
# method: xgboost
save(modelFit,file="model/model2")

# 3
# preprocess: Change some NAs to "None" or constant: Alley,BsmtQual,BsmtCond,BsmtExposure,BsmtFinType1,BsmtFinType2,FireplaceQu,
# GarageType,GarageYrBlt(int),GarageFinish,GarageQual,GarageCond,PoolQC,Fence,MiscFeature
# Some numeric col should be converted to character: MSSubClass, MoSold, YrSold(remain original also)
# Imputing by mice and rf method
# variables: All
# method: xgboost
save(modelFit,file="model/model3")

# 4
# preprocess: Change some NAs to "None" or constant: Alley,BsmtQual,BsmtCond,BsmtExposure,BsmtFinType1,BsmtFinType2,FireplaceQu,
# GarageType,GarageYrBlt(int),GarageFinish,GarageQual,GarageCond,PoolQC,Fence,MiscFeature
# Some numeric col should be converted to character: MSSubClass, MoSold, YrSold(remain original also)
# Imputing by mice and rf method
# variables: All
# method: rf
save(modelFit,file="model/model4")

# 5
# preprocess: Change some NAs to "None" or constant: Alley,BsmtQual,BsmtCond,BsmtExposure,BsmtFinType1,BsmtFinType2,FireplaceQu,
# GarageType,GarageYrBlt(int),GarageFinish,GarageQual,GarageCond,PoolQC,Fence,MiscFeature
# Some numeric col should be converted to character: MSSubClass, MoSold, YrSold(remain original also)
# Categories imputed by most type; 
# Numeric imputed by knn.
# new MSSubClass 150->120
# variables: All
# method: xgbTree
save(modelFit,file="model/model5")

# 6
# preprocess: Change some NAs to "None" or constant: Alley,BsmtQual,BsmtCond,BsmtExposure,BsmtFinType1,BsmtFinType2,FireplaceQu,
# GarageType,GarageYrBlt(int),GarageFinish,GarageQual,GarageCond,PoolQC,Fence,MiscFeature
# Some numeric col should be converted to character: MSSubClass, MoSold, YrSold(remain original also)
# Imputing by mice and cart method
# variables: All
# method: xgbTree
# log SalePrice
save(modelFit,file="model/model6")

