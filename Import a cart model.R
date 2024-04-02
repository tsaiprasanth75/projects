#Import the packages
library(rpart)
library(rpart.plot)

#Import the cart model
mycartmodel= readRDS('C:/Users/tsaip/Desktop/ML using Phython/cartmodel.rds')
rpart.plot(mycartmodel)
mydata=mydata[,-1]

#Predict for records 10 to 25

ypred=predict(mycartmodel,type='class',newdata = mydata[10:25,])
ypred
ypredprob=predict(mycartmodel,type='prob',newdata = mydata[10:25,])
mytable=table(mydata[10:25,]$pep,ypred)
mytable

