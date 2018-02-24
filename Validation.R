train<-dataset_1[which(dataset_1$year == 2013 | dataset_1$year==2016 | dataset_1$year==2015),]
test<-dataset_1[dataset_1$year == 2014,]

cancel<-(train$cancel)

train<-data.frame(cbind(scl(train$tenure),scl(train$n.adults),scl(train$n.children),scl(train$len.at.res),scl(train$premium),scl(train$ni.age),
                        class.ind(as.factor(train$claim.ind)),class.ind(train$ni.gender),class.ind(as.factor(train$ni.marital.status)),class.ind(train$sales.channel)
                        ,class.ind(train$coverage.type),class.ind(train$dwelling.type),class.ind(train$credit),class.ind(train$house.color),class.ind(train$year),cancel,
                        class.ind(train$State),class.ind(train$city.risk),class.ind(train$Zip.cat),class.ind(train$Zip.5mile),class.ind(train$Ind),train$Risk,train$Count,train$Historical_cancel))


cancel<-(test$cancel)

test<-data.frame(cbind(scl(test$tenure),scl(test$n.adults),scl(test$n.children),scl(test$len.at.res),scl(test$premium),scl(test$ni.age),
                       class.ind(as.factor(test$claim.ind)),class.ind(test$ni.gender),class.ind(as.factor(test$ni.marital.status)),class.ind(test$sales.channel)
                       ,class.ind(test$coverage.type),class.ind(test$dwelling.type),class.ind(test$credit),class.ind(test$house.color),class.ind(test$year),cancel,
                       class.ind(test$State),class.ind(test$city.risk),class.ind(test$Zip.cat),class.ind(test$Zip.5mile),class.ind(test$Ind),test$Risk,test$Count,test$Historical_cancel))

train[,7:55]<-data.frame(sapply(7:55,function(x) as.factor(train[,x])))
train_out[,7:49]<-data.frame(sapply(7:49,function(x) as.factor(train_out[,x])))
test[,7:54]<-data.frame(sapply(7:54,function(x) as.factor(test[,x])))

test$X2016<-'0'
test$V57<-test$V55

model <- glm(cancel~high+ low+ Broker+ V3+Low.Zip+High.Zip +V57+V6+X2016,family=binomial(link='logit'),data=train)
summary(model)
trained.results <- predict(model,newdata=train,type='response')
auc(train$cancel,trained.results)

fitted.results <- predict(model,newdata=test,type='response')
auc(test$cancel,fitted.results)

train<-dataset_1[which(dataset_1$year == 2014 | dataset_1$year==2016 | dataset_1$year==2015),]
test<-dataset_1[dataset_1$year == 2016,]

cancel<-(train$cancel)

train<-data.frame(cbind(scl(train$tenure),scl(train$n.adults),scl(train$n.children),scl(train$len.at.res),scl(train$premium),scl(train$ni.age),
                        class.ind(as.factor(train$claim.ind)),class.ind(train$ni.gender),class.ind(as.factor(train$ni.marital.status)),class.ind(train$sales.channel)
                        ,class.ind(train$coverage.type),class.ind(train$dwelling.type),class.ind(train$credit),class.ind(train$house.color),class.ind(train$year),cancel,
                        class.ind(train$State),class.ind(train$city.risk),class.ind(train$Zip.cat),class.ind(train$Zip.5mile),class.ind(train$Ind),train$Risk,train$Count,train$Historical_cancel))


cancel<-(test$cancel)

test<-data.frame(cbind(scl(test$tenure),scl(test$n.adults),scl(test$n.children),scl(test$len.at.res),scl(test$premium),scl(test$ni.age),
                       class.ind(as.factor(test$claim.ind)),class.ind(test$ni.gender),class.ind(as.factor(test$ni.marital.status)),class.ind(test$sales.channel)
                       ,class.ind(test$coverage.type),class.ind(test$dwelling.type),class.ind(test$credit),class.ind(test$house.color),class.ind(test$year),cancel,
                       class.ind(test$State),class.ind(test$city.risk),class.ind(test$Zip.cat),class.ind(test$Zip.5mile),class.ind(test$Ind),test$Risk,test$Count,test$Historical_cancel))

train[,7:55]<-data.frame(sapply(7:55,function(x) as.factor(train[,x])))
train_out[,7:49]<-data.frame(sapply(7:49,function(x) as.factor(train_out[,x])))
test[,7:54]<-data.frame(sapply(7:54,function(x) as.factor(test[,x])))

test$X2016<-'0'
test$V57<-test$V55

model <- glm(cancel~high+ low+ Broker+ V3+Low.Zip+High.Zip +V57+V6+X2016,family=binomial(link='logit'),data=train)
summary(model)
trained.results <- predict(model,newdata=train,type='response')
auc(train$cancel,trained.results)

fitted.results <- predict(model,newdata=test,type='response')
auc(test$cancel,fitted.results)

train<-dataset_1[which(dataset_1$year == 2014 | dataset_1$year==2016 | dataset_1$year==2015),]
test<-dataset_1[dataset_1$year == 2013,]

cancel<-(train$cancel)

train<-data.frame(cbind(scl(train$tenure),scl(train$n.adults),scl(train$n.children),scl(train$len.at.res),scl(train$premium),scl(train$ni.age),
                        class.ind(as.factor(train$claim.ind)),class.ind(train$ni.gender),class.ind(as.factor(train$ni.marital.status)),class.ind(train$sales.channel)
                        ,class.ind(train$coverage.type),class.ind(train$dwelling.type),class.ind(train$credit),class.ind(train$house.color),class.ind(train$year),cancel,
                        class.ind(train$State),class.ind(train$city.risk),class.ind(train$Zip.cat),class.ind(train$Zip.5mile),class.ind(train$Ind),train$Risk,train$Count,train$Historical_cancel))


cancel<-(test$cancel)

test<-data.frame(cbind(scl(test$tenure),scl(test$n.adults),scl(test$n.children),scl(test$len.at.res),scl(test$premium),scl(test$ni.age),
                       class.ind(as.factor(test$claim.ind)),class.ind(test$ni.gender),class.ind(as.factor(test$ni.marital.status)),class.ind(test$sales.channel)
                       ,class.ind(test$coverage.type),class.ind(test$dwelling.type),class.ind(test$credit),class.ind(test$house.color),class.ind(test$year),cancel,
                       class.ind(test$State),class.ind(test$city.risk),class.ind(test$Zip.cat),class.ind(test$Zip.5mile),class.ind(test$Ind),test$Risk,test$Count,test$Historical_cancel))

train[,7:55]<-data.frame(sapply(7:55,function(x) as.factor(train[,x])))
test[,7:54]<-data.frame(sapply(7:54,function(x) as.factor(test[,x])))

test$X2016<-'0'
test$V57<-test$V55

model <- glm(cancel~high+ low+ Broker+ V3+Low.Zip+High.Zip +V57+V6+X2016,family=binomial(link='logit'),data=train)
summary(model)
trained.results <- predict(model,newdata=train,type='response')
auc(train$cancel,trained.results)

fitted.results <- predict(model,newdata=test,type='response')
auc(test$cancel,fitted.results)

train<-dataset_1[which(dataset_1$year == 2013 | dataset_1$year==2016 | dataset_1$year==2014),]
test<-dataset_1[dataset_1$year == 2015,]

cancel<-(train$cancel)

train<-data.frame(cbind(scl(train$tenure),scl(train$n.adults),scl(train$n.children),scl(train$len.at.res),scl(train$premium),scl(train$ni.age),
                        class.ind(as.factor(train$claim.ind)),class.ind(train$ni.gender),class.ind(as.factor(train$ni.marital.status)),class.ind(train$sales.channel)
                        ,class.ind(train$coverage.type),class.ind(train$dwelling.type),class.ind(train$credit),class.ind(train$house.color),class.ind(train$year),cancel,
                        class.ind(train$State),class.ind(train$city.risk),class.ind(train$Zip.cat),class.ind(train$Zip.5mile),class.ind(train$Ind),train$Risk,train$Count,train$Historical_cancel))


cancel<-(test$cancel)

test<-data.frame(cbind(scl(test$tenure),scl(test$n.adults),scl(test$n.children),scl(test$len.at.res),scl(test$premium),scl(test$ni.age),
                       class.ind(as.factor(test$claim.ind)),class.ind(test$ni.gender),class.ind(as.factor(test$ni.marital.status)),class.ind(test$sales.channel)
                       ,class.ind(test$coverage.type),class.ind(test$dwelling.type),class.ind(test$credit),class.ind(test$house.color),class.ind(test$year),cancel,
                       class.ind(test$State),class.ind(test$city.risk),class.ind(test$Zip.cat),class.ind(test$Zip.5mile),class.ind(test$Ind),test$Risk,test$Count,test$Historical_cancel))

train[,7:55]<-data.frame(sapply(7:55,function(x) as.factor(train[,x])))
train_out[,7:49]<-data.frame(sapply(7:49,function(x) as.factor(train_out[,x])))
test[,7:54]<-data.frame(sapply(7:54,function(x) as.factor(test[,x])))

test$X2016<-'0'
test$V57<-test$V55

model <- glm(cancel~high+ low+ Broker+ V3+Low.Zip+High.Zip +V57+V6+X2016,family=binomial(link='logit'),data=train)
summary(model)
trained.results <- predict(model,newdata=train,type='response')
auc(train$cancel,trained.results)

fitted.results <- predict(model,newdata=test,type='response')
auc(test$cancel,fitted.results)
