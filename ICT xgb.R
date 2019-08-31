setwd("E:/2019 ICT/DATA")

library(data.table)
library(dplyr)
library(magrittr)
library(ggplot2)
library(MLmetrics)
library(caret)
library(dummies)
library(xgboost)

#Raw1
Raw <- fread("NEW.csv", encoding = "UTF-8")
data <- Raw %>% filter(서비스업종코드명 == "한식음식점")
data <- data[,c(6,7,31:85,98:100,103:121,137,149)]
colnames(data)[1] <- "Money"
colnames(data)
for (i in 1:81){
  data[,i] <- as.numeric(data[,i])
}
str(data)

#Raw2

#RFE
set.seed(1000)
d_x <- data[,-1]
d_y <- data[,1]

sum(is.na(data))

subsets <- seq(9,24, by=3)
ctrl <- rfeControl(functions = rfFuncs, 
                   method = "cv",
                   number = 5,
                   verbose = FALSE)
rfProfile <- rfe(d_x,d_y,                    
                 sizes = subsets,
                 rfeControl = ctrl)
rfProfile
predictors(rfProfile)
plot(rfProfile, type=c('g','o'))
a <- data.frame(Accuracy=rfProfile$results$Accuracy, x=c(9,12,15,18,21,23),col=1)
ggplot(a, aes(x=x, y=Accuracy,color=col))+geom_line(lwd=2)+geom_point(size=5)+theme_bw()

#start
index = createDataPartition(data$Money,p=0.8,list=F)
train = data[index,]
test = data[-index,]

xgb_train <- data[index,]
xgb_test <- data[-index,]

set.seed(1000)
Folds = createFolds(train$Money,k=5,returnTrain = TRUE)
Ftest1 = setdiff(1:nrow(train),Folds$Fold1)
Ftest2 = setdiff(1:nrow(train),Folds$Fold2)
Ftest3 = setdiff(1:nrow(train),Folds$Fold3)
Ftest4 = setdiff(1:nrow(train),Folds$Fold4)
Ftest5 = setdiff(1:nrow(train),Folds$Fold5)

FOLDS_TEST = list()
FOLDS_TEST$Fold1 = Ftest1
FOLDS_TEST$Fold2 = Ftest2
FOLDS_TEST$Fold3 = Ftest3
FOLDS_TEST$Fold4 = Ftest4
FOLDS_TEST$Fold5 = Ftest5

#xgb.DMatrix
xgb_mat_train <- xgb.DMatrix(as.matrix(xgb_train[,-1]),
                             label = xgb_train$Money)
xgb_mat_test <- xgb.DMatrix(as.matrix(xgb_test[,-1]),
                            label = xgb_test$Money)

#tune1
set.seed(1000)

max.depth.grid = seq(2,10,2) 
min.child.grid = seq(1,9,2)

xg_tune_1 = as.data.frame(matrix(nrow=length(max.depth.grid)*length(min.child.grid),ncol=3,NA))
colnames(xg_tune_1) = c('max_depth','min_child_weight','RMSE')
x=1

for (i in max.depth.grid){
  for (j in min.child.grid){
    set.seed(1000)
    params_1 = list(objective = "reg:linear",
                    eval_metric = 'rmse',
                    eta = 0.1,
                    max_depth = i,
                    min_child_weight = j,
                    subsample = 0.8, colsample_bytree = 0.8)
    
    xgb_cv_1 = xgb.cv(params = params_1,
                      data = xgb_mat_train,
                      nround = 10000,
                      folds = FOLDS_TEST, 
                      verbose = 0,
                      prediction = TRUE,
                      early_stopping_rounds = 100,
                      nthread=2)
    
    best_rmse = xgb_cv_1$evaluation_log[,min(test_rmse_mean)]
    xg_tune_1[x,1] = i
    xg_tune_1[x,2] = j
    xg_tune_1[x,3] = best_rmse
    x = x + 1
  }
  print('AH')
}

ggplot(xg_tune_1,aes(x=max_depth,y=min_child_weight)) + geom_tile(aes(fill=RMSE),colour = "white")+ 
  scale_fill_gradient(low = "white",high = "steelblue")+scale_x_continuous(breaks=max.depth.grid)+
  scale_y_continuous(breaks=min.child.grid)+ggtitle('max_depth X min_child')

xg_tune_1[xg_tune_1$RMSE==min(xg_tune_1$RMSE),] 

#tune2
set.seed(1000)

max.depth.grid = seq(4,8,1) 
min.child.grid = seq(1,5,1)

xg_tune_2 = as.data.frame(matrix(nrow=length(max.depth.grid)*length(min.child.grid),ncol=3,NA))
colnames(xg_tune_2) = c('max_depth','min_child_weight','RMSE')
x=1

for (i in max.depth.grid){
  for (j in min.child.grid){
    params_2 = list(objective = "reg:linear",
                    eval_metric = 'rmse',
                    eta = 0.1,
                    max_depth = i,
                    min_child_weight = j,
                    subsample = 0.8, colsample_bytree = 0.8)
    
    xgb_cv_2 = xgb.cv(params = params_2,
                      data = xgb_mat_train,
                      nround = 10000,
                      folds = FOLDS_TEST, 
                      verbose = 0,
                      prediction = TRUE,
                      early_stopping_rounds = 100,
                      nthread=2)
    
    best_rmse = xgb_cv_2$evaluation_log[,min(test_rmse_mean)]
    xg_tune_2[x,1] = i
    xg_tune_2[x,2] = j
    xg_tune_2[x,3] = best_rmse
    x = x + 1
  }
  print('AH')
}

ggplot(xg_tune_2,aes(x=max_depth,y=min_child_weight)) + geom_tile(aes(fill=RMSE),colour = "white")+ 
  scale_fill_gradient(low = "white",high = "steelblue")+scale_x_continuous(breaks=max.depth.grid)+
  scale_y_continuous(breaks=min.child.grid)+ggtitle('max_depth X min_child')

xg_tune_2[xg_tune_2$RMSE==min(xg_tune_2$RMSE),]

###sample parameter### 
set.seed(1000)

subsample.grid = seq(0.5,1,0.1) 
colsample.grid = seq(0.5,1,0.1)

subsample.grid = seq(0.4,0.6,0.05) 
colsample.grid = seq(0.6,0.8,0.05)

xg_tune_4 = as.data.frame(matrix(nrow=length(subsample.grid)*length(colsample.grid),ncol=3,NA))
colnames(xg_tune_4) = c('subsample','colsample','RMSE')
x=1

for (i in subsample.grid){
  for (j in colsample.grid){
    params_4 = list(objective = "reg:linear",
                    eval_metric = 'rmse',
                    eta = 0.1,
                    max_depth = 6, 
                    min_child_weight = 3, 
                    subsample = i, colsample_bytree = j)
    
    xgb_cv_4 = xgb.cv(params = params_4,
                      data = xgb_mat_train,
                      nround = 10000,
                      folds = FOLDS_TEST, 
                      verbose = 0,
                      prediction = TRUE,
                      early_stopping_rounds = 100,
                      nthread=2)
    
    best_rmse = xgb_cv_4$evaluation_log[,min(test_rmse_mean)]
    xg_tune_4[x,1] = i
    xg_tune_4[x,2] = j
    xg_tune_4[x,3] = best_rmse
    x = x + 1
  }
  print('AHHH')
}

ggplot(xg_tune_4,aes(x=subsample,y=colsample,size=RMSE)) + geom_point(colour='#56B4E9')+
  scale_x_continuous(breaks=subsample.grid)+scale_y_continuous(breaks=colsample.grid)+
  ggtitle('subsample X colsample')

xg_tune_4[xg_tune_4$RMSE==min(xg_tune_4$RMSE),]

##eta
set.seed(1000)

params_5 = list(objective = "reg:linear",
                eval_metric = 'rmse',
                eta = 0.01,
                max_depth = 6,
                min_child_weight = 3,
                subsample = 0.5, colsample_bytree = 0.7)

xgb_cv_5 = xgb.cv(params = params_5,
                  data = xgb_mat_train,
                  nround = 10000,
                  folds = FOLDS_TEST,
                  prediction = TRUE,
                  verbose = 1, 
                  early_stopping_rounds = 500, 
                  nthread=2) 

ggplot()+geom_line(aes(x=iter,y=test_rmse_mean),xgb_cv_5$evaluation_log,size=1.5,color='orange')+
  geom_line(aes(x=iter,y=train_rmse_mean),xgb_cv_5$evaluation_log,size=1.5) + ggtitle('train VS test')

xgb_cv_5 

#tuning parameter
param = list(objective = "reg:linear",
             eval_metric = 'rmse',
             eta = 0.003,
             max_depth = 9,
             min_child_weight = 9,
             subsample = 0.5, colsample_bytree = 0.7)

xgb_final = xgb.train(params = param, data = xgb_mat_train,
                      nround = 1448, nthread = 2)

#importance plot
importance_matrix <- xgb.importance(colnames(xgb_mat_train),
                                    model = xgb_final)
xgb.plot.importance(importance_matrix)

#Predict
xgb.pred = predict(xgb_final,xgb_mat_test)
data.frame(xgb.pred,xgb_test[,1])

y <- test$Money

library(prediction)
library(ROCR)
library(pROC)
result <- predict(xgb_final, xgb_mat_test, type = "response")
result <- prediction(result, y)

RMSE <- RMSE(y,result)