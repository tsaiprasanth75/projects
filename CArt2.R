attach(mydata)
mydata=mydata[,-1]

#Split the data into training and test data
set.seed(1)
sampleid=sample(2,600,replace = TRUE, prob=c(0.8,0.2))
sampleid
training =mydata[sampleid==1,]
testing = mydata[sampleid==2,]

#Attach Traininng data
attach(training)

#Import packages
library(rpart)
library(rpart.plot)

#develop the model
mymodel=rpart(pep~.,data = training,method = 'class',control = rpart.control(minsplit = 2))
plotcp(mymodel,pch=16,col='red')

#Optimum model 
mymodel=prune(mymodel,cp=0.013)
rpart.plot(mymodel)

#Featue importance
mymodel$variable.importance

#Model performance
#Preddict the values
ypredd=predict(mymodel,type = 'class')

#Actual vs predicted
mytable=table(training$pep,ypredd)
mytable
round(prop.table(mytable)*100,2)

#Validata on the test data
testpred= predict(mymodel,type = 'class',newdata = testing)

#Actual vs predicted - test data

mytesttable= table(testing$pep,testpred)
mytesttable
round(prop.table(mytesttable)*100,2)

#Export the model
saveRDS(mymodel,'C:/Users/tsaip/Desktop/ML using Phython/cartmodel.rds')


























