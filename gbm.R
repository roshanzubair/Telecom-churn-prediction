
#gbm

fitControl <- trainControl(method = "repeatedcv", number = 4, repeats = 4)
train$outcome1<-NA
train$outcome1 <- ifelse(train$cancel == 1, "Yes","No")
set.seed(55)
set.seed(123)
indexes = sample(1:nrow(dataset_1), size=0.2*nrow(dataset_1))
dataset_1$year<-as.factor(dataset_1$year)
dataset_1$claim.ind<-as.factor(dataset_1$claim.ind)

test<-dataset_1[indexes,]
train<-dataset_1[-indexes,]
gbmFit1 <- train(as.factor(outcome1)  ~ ., 
                 data = train[,-c(17,18,19)], method = "gbm", trControl = fitControl,verbose = FALSE,)

train_ensemble$prob_gbm<-predict(gbmFit1, train,type= "prob")[,2]
test_ensemble$prob_gbm<-predict(gbmFit1, test,type= "prob")[,2]

auc(train$cancel,train_ensemble$prob_gbm)
auc(test$cancel,test_ensemble$prob_gbm)

boost.myData<-gbm(as.character(cancel)~.,data=train[,-c(19,17)],distribution="bernoulli",n.trees=1000,
                  interaction.depth=4,n.minobsinnode = 100,shrinkage = 0.01)

pred.boost<-predict(boost.myData,newdata=train,n.trees=1000,type="response")
auc(train$cancel,pred.boost)
pred.boost<-predict(boost.myData,newdata=test,n.trees=1000,type="response")
auc(test$cancel,pred.boost)
