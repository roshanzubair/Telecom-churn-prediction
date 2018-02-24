
#logistic

scl <- function(x){ (x - min(x))/(max(x) - min(x)) }

scl <- function(x){x^2}
cancel<-(dataset_1$cancel)
dataset_1$V7<-dataset_1$tenure/dataset_1$len.at.res
dataset_3<-data.frame(cbind(scl(dataset_1$tenure),scl(dataset_1$n.adults),scl(dataset_1$n.children),scl(dataset_1$len.at.res),scl(dataset_1$premium),scl(dataset_1$ni.age),
                            class.ind(as.factor(dataset_1$claim.ind)),class.ind(dataset_1$ni.gender),class.ind(as.factor(dataset_1$ni.marital.status))),class.ind(dataset_1$sales.channel)
                      ,class.ind(dataset_1$coverage.type),class.ind(dataset_1$dwelling.type),class.ind(dataset_1$credit),class.ind(dataset_1$house.color),class.ind(dataset_1$year),cancel,
                      class.ind(dataset_1$State),class.ind(dataset_1$city.risk),class.ind(dataset_1$Zip.cat),V7=scl(dataset_1$V7))

dataset_3[,7:46]<-data.frame(sapply(7:46,function(x) as.factor(dataset_3[,x])))
rownames(dataset_3) <- seq(length=nrow(dataset_3))

iv.mult(dataset_3,"cancel",TRUE) 

#convert all variables into categorical variables or standerdised variables
set.seed(123)
indexes = sample(1:nrow(dataset_3), size=0.2*nrow(dataset_3))
test<-dataset_3[indexes,]
train<-dataset_3[-indexes,]

model <- glm(cancel~high+ low+ Broker+ Low.Risk.City+ High.Risk.City+ VA+ PA+ X2014+ V3+ V6+Low.Zip,family=binomial(link='logit'),data=train)
summary(model)
train.results <- predict(model,newdata=train,type='response')
auc(train$cancel,train.results)
fitted.results <- predict(model,newdata=test,type='response')
auc(test$cancel,fitted.results)
