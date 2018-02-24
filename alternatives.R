library("CHAID", lib.loc="~/R/win-library/3.4")
library(e1071)

chaid.control<-chaid_control(alpha2 = 0.05, alpha3 = -1, alpha4 = 0.05,
              minsplit = 20, minbucket = 7, minprob = 0.05,
              stump = FALSE, maxheight = 4)

dataset_1$cancel<-as.factor(dataset_1$cancel)
data_chaid<-dataset_1[,-c(17,19)]
chars<-sapply(data_chaid,is.factor)

set.seed(123)
indexes = sample(1:nrow(dataset_1), size=0.2*nrow(dataset_1))
test_chaid<-data_chaid[indexes,]
train_chai<-data_chaid[-indexes,]

tree<-chaid(as.factor(cancel)~.,train_chai[,chars],control=chaid.control)
plot(tree)

fitted.results<-predict(tree,newdata = test_chaid,type="prob")[,2]
auc(test_chaid$cancel,fitted.results)

model_nb<-naiveBayes(as.factor(cancel)~.,data=train)

pred<-predict(model_nb,test)
auc(test$cancel,pred)

table(pred,test$cancel)

model_svm<-svm(as.factor(cancel)~high+ low+ Broker+ Low.Risk.City+ High.Risk.City+ VA+ PA+ V3+ V6,data=train,
                 kernal=rbf,gamma=.2,cost=1000,probability=TRUE)

prob_svm<-predict(model_svm,train,probability=TRUE)
table(pred,train$cancel)
auc(train$cancel,attr(prob_svm,'prob')[,2])
confusionMatrix(train_ensemble$prob_svm,train$cancel)

prob_svm<-predict(model_svm,test,probability=TRUE)
table(pred,test$cancel)
auc(test$cancel,attr(prob_svm,'prob')[,2])
confusionMatrix(test_ensemble$prob_svm,test$cancel)
pred_input <- prediction(test$pred,test$cancel)
