setwd("C:/Users/rosha/OneDrive for Business/Travelers")
dataset_2<-read.csv("Train.csv")
dataset_1<-dataset_2[complete.cases(dataset_2),]
dataset_1<-dataset_1[which(!dataset_1$cancel==-1),]
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

#preprosessing steps


scl <- function(x){ (x - min(x))/(max(x) - min(x)) }

scl <- function(x){x^2}
cancel<-(dataset_1$cancel)
dataset_3<-data.frame(cbind(scl(dataset_1$tenure),scl(dataset_1$n.adults),scl(dataset_1$n.children),scl(dataset_1$len.at.res),scl(dataset_1$premium),scl(dataset_1$ni.age),
                           class.ind(as.factor(dataset_1$claim.ind)),class.ind(dataset_1$ni.gender),class.ind(as.factor(dataset_1$ni.marital.status))),class.ind(dataset_1$sales.channel)
                           ,class.ind(dataset_1$coverage.type),class.ind(dataset_1$dwelling.type),class.ind(dataset_1$credit),class.ind(dataset_1$house.color),class.ind(dataset_1$year),cancel,
                      class.ind(dataset_1$State),class.ind(dataset_1$city.risk))

dataset_3[,7:44]<-data.frame(sapply(7:44,function(x) as.factor(dataset_3[,x])))

#convert all variables into categorical variables or standerdised variables
set.seed(1234)
indexes = sample(1:nrow(dataset_3), size=0.2*nrow(dataset_3))
test<-dataset_3[indexes,]
train<-dataset_3[-indexes,]

str(train)
summary(train)
nums<-sapply(dataset_3,is.numeric)
chars<-sapply(dataset_3,is.factor)
rownames(dataset_3) <- seq(length=nrow(dataset_3))
iv.mult(dataset_3,"cancel",TRUE) 

correlation<-data.frame(cor(train[,nums]))

nrow(dataset_1[complete.cases(dataset_1),])

quantile(train$len.at.res,probs=seq(0,1,.1))
quantile(train$n.adults,probs=seq(0,1,.1))
quantile(train$n.children,probs=seq(0,1,.1))
quantile(train$ni.age,probs=seq(0,1,.1))
quantile(train$ni.marital.status,probs=seq(0,1,.1))
quantile(train$premium,probs=seq(0,1,.1))
quantile(train$tenure,probs=seq(0,1,.1))

summary(train[,chars])

rownames(train) <- seq(length=nrow(train))
iv.mult(train,"cancel",TRUE) 
str(train)
train$cancel<-as.factor(train$cancel)

str(train)

train$premium_cat<-cut(train$premium, c(645,700,745,800,845,900,945,1000,1045,1100,1165))
prop.table(table(train$coverage.type,train$cancel))

train$V7<-(train$V1)/(train$V4)
test$V7<-(test$V1)/(test$V4)

model <- glm(cancel~high+low+Broker+VA+PA+X2014+V3+V6+X2016+X0+V7,family=binomial(link='logit'),data=train)


model <- glm(cancel ~ ni.marital.status+ credit+ tenure+ claim.ind+ sales.channel+ ni.age
             + n.children,
             family=binomial(link='logit'),data=train)

summary(model)
vif(model)
anova(model,"Chisq")

train.results <- predict(model,newdata=train,type='response')
train.results <- ifelse(train.results > 0.50,1,0)
misClasificError <- mean(train.results != train$cancel)
print(paste('Accuracy',1-misClasificError))
model.pred<- prediction(train.results, train$cancel)
model.perf<- performance(model.pred,"tpr","fpr")
auc<- performance(model.pred,"auc")
print(auc@y.values)
plot(model.perf,main="ROC Curve for Random Forest",col=2,lwd=2)
abline(a=0,b=1,lwd=2,lty=2,col="gray")
auc(train$cancel,train.results)

fitted.results <- predict(model,newdata=test,type='response')
fitted.results <- ifelse(fitted.results > 0.25,1,0)

misClasificError <- mean(fitted.results != test$cancel)
print(paste('Accuracy',1-misClasificError))
model.pred<- prediction(fitted.results, test$cancel)
auc(test$cancel,fitted.results)


model.perf<- performance(model.pred,"tpr","fpr")
plot(model.perf,main="ROC Curve for Random Forest",col=2,lwd=2)
abline(a=0,b=1,lwd=2,lty=2,col="gray")

train$prob_glm<-train.results
test$prob_glm<-fitted.results
# random forest

for(i in 1:20)
{
model.rf <- randomForest(as.factor(cancel) ~ ., data=train, importance=TRUE,
                         proximity=TRUE,ntree=(i*100),nodesize=100)
importance(model.rf)

train$prob_rf<-predict(model.rf,type="prob",newdata = train)[,2]
test$prob_rf<-predict(model.rf,type="prob",newdata = test)[,2]

print(auc(train$cancel,train$prob_rf))
print(auc(test$cancel,test$prob_rf))
fitted.results<-ifelse(test$prob_rf>0.4,1,0)
misClasificError <- mean(fitted.results != test$cancel)
print(paste('Accuracy for',i*100,'tree is',1-misClasificError))
}


model.rf <- randomForest(as.factor(cancel) ~ ., data=train[,-c(17,19)], importance=TRUE,
                         proximity=TRUE,ntree=300,nodesize=10,mtry=3,
                         sampsize=100)
importance(model.rf)

train$prob_rf<-predict(model.rf,type="prob",newdata = train)[,2]
test$prob_rf<-predict(model.rf,type="prob",newdata = test)[,2]

print(auc(train$cancel,train$prob_rf))
print(auc(test$cancel,test$prob_rf))

# neural network

train$cancel1<-class.ind(train$cancel)
seedsANN = nnet(cancel1~.,data=train[,-18],size=25)
test$predict_nn<-predict(seedsANN,test[,-18],type = "class")
table(test$predict_nn,test$cancel)

# neural net package
train$outcome1 <- ifelse(train$cancel == 1, "Yes","No")
test$outcome1 <- ifelse(test$cancel == 1, "Yes","No")
train_nn<-cbind(train[,-33],class.ind(train$outcome1))
test_nn<-cbind(test[,-33],class.ind(test$outcome1))

# Set up formula
n <- names(train_nn)
f <- as.formula(paste("No+Yes ~", paste(n[!n %in% c("No","Yes","outcome1")], collapse = " + ")))

n<-n[-c()]

nn <- neuralnet(f,data = train_nn,hidden = c(3, 2, 2),
                act.fct = "logistic",linear.output = FALSE,lifesign = "minimal")
plot(nn)
# Compute predictions
pr.nn <- compute(nn, train_nn[1:32])

# Extract results
pr.nn_ <- pr.nn$net.result
head(pr.nn_)

# Accuracy (training set)
original_values <- max.col(train_nn[, 33:34])
pr.nn_2 <- max.col(pr.nn_)
mean(pr.nn_2 == original_values)

# Compute predictions
pr.nn <- compute(nn, test_nn[1:32])

# Extract results
pr.nn_ <- pr.nn$net.result
head(pr.nn_)

# Accuracy (training set)
original_values <- max.col(test_nn[, 33:34])
pr.nn_2 <- max.col(pr.nn_)
mean(pr.nn_2 == original_values)
auc(train$cancel,pr.nn_[,2])

train$prob_nn<-pr.nn_[,2]
test$prob_nn<-pr.nn_[,2]

auc(train$cancel,train$prob_nn)
auc(test$cancel,test$prob_nn)

#gbm

fitControl <- trainControl(method = "repeatedcv", number = 4, repeats = 4)
train$outcome1<-NA
train$outcome1 <- ifelse(train$cancel == 1, "Yes","No")
set.seed(33)
gbmFit1 <- train(as.factor(outcome1)  ~ ., 
                 data = train[,-18], method = "gbm", trControl = fitControl,verbose = FALSE)

train$prob_gbm<-predict(gbmFit1, train,type= "prob")[,2]
test$prob_gbm<-predict(gbmFit1, test,type= "prob")[,2]

auc(train$cancel,train$prob_gbm)
auc(test$cancel,test$prob_gbm)

boost.myData<-gbm(cancel~.,data=train,distribution="bernoulli",n.trees=5000,interaction.depth=4)
pred.boost<-predict(boost.myData,newdata=myData[test,],n.trees=5000,type="response")

#knn

library(class)
fitControl <- trainControl(method = "cv",number = 5,savePredictions = 'final',classProbs = T)

model_knn<-train(cancel  ~ credit+ sales.channel+ n.children+ ni.marital.status+ year+ 
                   claim.ind+ len.at.res+ n.adults,data=train, method='knn')
names(model_knn)
pred_knn<-predict(object=model_knn,data=test)
confusionMatrix(pred_knn,test$outcome1)

train_knn<-data.frame(cbind(scl(train$tenure),class.ind(train$claim.ind),scl(train$n.children),
                           class.ind(train$sales.channel),scl(train$len.at.res),class.ind(train$credit),
                           scl(train$ni.age),class.ind(train$year)),train$cancel)


test_knn<-data.frame(cbind(scl(test$tenure),class.ind(test$claim.ind),scl(test$n.children),
                            class.ind(test$sales.channel),scl(test$len.at.res),class.ind(test$credit),
                           scl(test$ni.age),class.ind(test$year),test$cancel))


for(i in 1:20)
{
print(paste("k is",i))  
pred_knn<-knn(train_knn, test_knn, train[,"cancel"], k = i, l = 0, prob = TRUE, use.all = TRUE)
print(paste("total 1s captured",sum(table(pred_knn,test$cancel)[2,])))
print(paste("Accuracy is",1-(table(pred_knn,test$cancel)[1,2]+table(pred_knn,test$cancel)[2,1])/sum(table(pred_knn,test$cancel))))
}

library(KODAMA)

x<-list(train_knn,test$knn)
knn.dist(x)

#ensembling

for(i in 1:nrow(train))
{
train$prob_all[i]<-max(train$prob_glm[i],train$prob_rf[i],train$prob_nn[i])
}

auc(train$cancel,train$prob_all)

total_prob <- ifelse(train$prob_all > 0.5,1,0)
misClasificError <- mean(total_prob != train$cancel)
print(paste('Accuracy',1-misClasificError))

summary(train$prob_all)


for(i in 1:nrow(test))
{
  test$prob_all[i]<-max(test$prob_glm[i],test$prob_rf[i],test$prob_nn[i])
}

auc(test$cancel,test$prob_all)


## model 

model_ensemble_glm<-glm(cancel~prob_glm+prob_rf+prob_nn+prob_gbm,family=binomial(link='logit'),data=train)
summary(model_ensemble_glm)
train.results <- predict(model_ensemble_glm,newdata=train,type='response')
auc(train$cancel,train.results)
test.results <- predict(model_ensemble_glm,newdata=test,type='response')
auc(test$cancel,test.results)

model_ensemble_gbm <- train(as.factor(outcome1)  ~prob_glm+prob_gbm
                            , data = train[,-18], method = "gbm", trControl = fitControl,verbose = FALSE)
model_ensemble_gbm_dev <- predict(model_ensemble_gbm, train,type= "prob")[,2] 
model_ensemble_gbm_test <- predict(model_ensemble_gbm, test,type= "prob")[,2]
auc(train$cancel,gbm_dev)
auc(test$cancel,gbm_ITV2)

