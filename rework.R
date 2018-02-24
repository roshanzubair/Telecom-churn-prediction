setwd("C:/Users/rosha/OneDrive for Business/Travelers")
dataset_2<-read.csv("Train.csv")
dataset_1<-dataset_2[complete.cases(dataset_2),]
dataset_1<-dataset_1[which(!dataset_1$cancel==-1 ),]
dataset_1<-dataset_1[which(dataset_1$ni.age<76),]
library(woe)
library(car)
require("randomForest")
require("ROCR")
require(neuralnet)
require(nnet)
require(ggplot2)
library(caret)
library(Metrics)
library(gbm)

#logistic

scl <- function(x){ (x - min(x))/(max(x) - min(x)) }

scl <- function(x){x^2}


#convert all variables into categorical variables or standerdised variables
train<-dataset_1[which(dataset_1$year == 2014 | dataset_1$year==2016 | dataset_1$year==2015),]
test<-dataset_1[dataset_1$year == 2013,]
set.seed(123)
indexes = sample(1:nrow(train), size=0.2*nrow(train))
train_out<-train[indexes,]
train<-train[-indexes,]

train_out<-train[which(dataset_1$year==2015),]
train<-train[which(dataset_1$year == 2013 | dataset_1$year==2014),]

cancel<-(train$cancel)

train<-data.frame(cbind(scl(train$tenure),scl(train$n.adults),scl(train$n.children),scl(train$len.at.res),scl(train$premium),scl(train$ni.age),
                            class.ind(as.factor(train$claim.ind)),class.ind(train$ni.gender),class.ind(as.factor(train$ni.marital.status)),class.ind(train$sales.channel)
                      ,class.ind(train$coverage.type),class.ind(train$dwelling.type),class.ind(train$credit),class.ind(train$house.color),class.ind(train$year),cancel,
                      class.ind(train$State),class.ind(train$city.risk),class.ind(train$Zip.cat),class.ind(train$Zip.5mile),class.ind(train$Ind),train$Risk,train$Count,train$Historical_cancel))


cancel<-(train_out$cancel)

train_out<-data.frame(cbind(scl(train_out$tenure),scl(train_out$n.adults),scl(train_out$n.children),scl(train_out$len.at.res),scl(train_out$premium),scl(train_out$ni.age),
                       class.ind(as.factor(train_out$claim.ind)),class.ind(train_out$ni.gender),class.ind(as.factor(train_out$ni.marital.status)),class.ind(train_out$sales.channel)
                       ,class.ind(train_out$coverage.type),class.ind(train_out$dwelling.type),class.ind(train_out$credit),class.ind(train_out$house.color),class.ind(train_out$year),cancel,
                       class.ind(train_out$State),class.ind(train_out$city.risk),class.ind(train_out$Zip.cat),class.ind(train_out$Zip.5mile),train_out$Risk,train_out$Count))


cancel<-(test$cancel)

test<-data.frame(cbind(scl(test$tenure),scl(test$n.adults),scl(test$n.children),scl(test$len.at.res),scl(test$premium),scl(test$ni.age),
                        class.ind(as.factor(test$claim.ind)),class.ind(test$ni.gender),class.ind(as.factor(test$ni.marital.status)),class.ind(test$sales.channel)
                  ,class.ind(test$coverage.type),class.ind(test$dwelling.type),class.ind(test$credit),class.ind(test$house.color),class.ind(test$year),cancel,
                  class.ind(test$State),class.ind(test$city.risk),class.ind(test$Zip.cat),class.ind(test$Zip.5mile),class.ind(test$Ind),test$Risk,test$Count,test$Historical_cancel))

train[,7:55]<-data.frame(sapply(7:55,function(x) as.factor(train[,x])))
train_out[,7:49]<-data.frame(sapply(7:49,function(x) as.factor(train_out[,x])))
test[,7:54]<-data.frame(sapply(7:54,function(x) as.factor(test[,x])))
test$X2014<-'0'
test$X2016<-'0'
test$V57<-test$V55

rownames(train) <- seq(length=nrow(train))
iv.mult(train,"cancel",TRUE) 
train$V52<-train$V2+train$V3
model <- glm(cancel~high+ low+ Broker+ V3+Low.Zip+High.Zip +V57+V6,family=binomial(link='logit'),data=train)
summary(model)
trained.results <- predict(model,newdata=train,type='response')
auc(train$cancel,trained.results)
tested.results <- predict(model,newdata=train_out,type='response')
auc(train_out$cancel,tested.results)
fitted.results <- predict(model,newdata=test,type='response')
auc(test$cancel,fitted.results)

pred_input <- prediction(fitted.results,test$cancel)
AUC <- performance(pred_input,"auc")
print(AUC@y.values)

model <- glm(cancel~high+ low+ Broker+ V3+Low.Zip+High.Zip+V50+X2013,family=binomial(link='logit'),data=train)
summary(model)
trained.results <- predict(model,newdata=train,type='response')
auc(train$cancel,trained.results)
tested.results <- predict(model,newdata=train_out,type='response')
auc(train_out$cancel,tested.results)
fitted.results <- predict(model,newdata=test,type='response')
auc(test$cancel,fitted.results)


# validation

oot<-read.csv("Test_imputed.csv")

oot_1<-data.frame(cbind(scl(oot$tenure),scl(oot$n.adults),scl(oot$n.children),scl(oot$len.at.res),scl(oot$premium),scl(oot$ni.age),
                       class.ind(as.factor(oot$claim.ind)),class.ind(oot$ni.gender),class.ind(as.factor(oot$ni.marital.status)),class.ind(oot$sales.channel)
                       ,class.ind(oot$coverage.type),class.ind(oot$dwelling.type),class.ind(oot$credit),class.ind(oot$house.color),class.ind(oot$year),
                       class.ind(oot$State),class.ind(oot$city.risk),class.ind(oot$Zip.cat),oot$Risk),oot$id)
oot_1$V57<-oot_1$V43
oot_1[,7:42]<-data.frame(sapply(7:42,function(x) as.factor(oot_1[,x])))

oot_1$prob=predict(model,newdata=oot_1,type='response')

write.csv(oot_1,"test$evaluated.csv")
