mydata=mydata[,-1]
attach(mydata)
#Import packages
library(rpart)
library(rpart.plot)
#Develop the model
mymodel=rpart(Outcome~.,data = mydata,method = 'class',control = rpart.control(minsplit = 2))
plotcp(mymodel,pch=16,col='red')
#Optimum model
mymodel=prune(mymodel,cp=0.03)
rpart.plot(mymodel)
mymodel
#preddictedd values
ypred=predict(mymodel,type = 'class')
# Combine with our data
myresult=cbind(mydata,ypred)
colnames(myresult)[6]='Predicted'
myresult
#actual vs predicted

mytable=table(Outcome,ypred)
mytable












