#xgboost

library(xgboost)
library(readr)
library(stringr)
library(caret)
library(car)

xgb <- xgboost(data = data.matrix(train[,c("high","low","Broker","VA","V3","Low.Zip","High.Zip","PA","V6")]), 
               label = train$cancel, 
               eta = 0.1,
               max_depth = 15, 
               nround=1000, 
               subsample = 0.5,
               colsample_bytree = 0.5,
               seed = 1,
               eval_metric = "merror",
               objective = "multi:softprob",
               num_class = 2,
               nthread = 3)

y_pred <- predict(xgb, data.matrix(test[,-31]))
auc(test$cancel,y_pred[1:1933])
auc(test$cancel,y_pred[1933:3866])

#imputation
oot<-read.csv("Test.csv")

summary(oot)
oot$zip.code<-as.factor(oot$zip.code)
oot$claim.ind<-as.factor(oot$claim.ind)
oot$ni.marital.status<-as.factor(oot$ni.marital.status)

library(DMwR)
knnOutput <- knnImputation(oot[,-c(16,17)],meth='median') 
oot<-cbind(knnOutput,oot[,16],zip.code=knnImputation(oot[,-16],k=1)$zip.code)
summary(oot)
write.csv(oot,"Test_imputed.csv",row.names = FALSE)
