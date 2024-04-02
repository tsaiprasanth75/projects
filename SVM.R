mydata =  mydata[,-1]
attach(mydata)
library(e1071)
mymodel= svm(factor(Species)~.,data = mydata,cost =0.1, scale = F,kernel='linear')
summary(mymodel)

#Optimise Hyperparameter(Hyperparameter Tuning)

tune_mymodel=tune(svm,factor(Species)~.,data = mydata,kernel='linear',ranges = list(cost=c(0.001,0.01,0.1,1,5,10,25,50,100)))
summary(tune_mymodel)

# Optimum model
mymodel=tune_mymodel$best.model
summary(mymodel)
#Predicted values
ypred= predict(mymodel,mydata)

#Actual vs predicted
mytable = table(Species,ypred)
mytable
round(prop.table(mytable)*100,2)

#Clean tes
test=test[,-1]
#Predited values test data
predtest=predict(mymodel,test)
#Actual vs predicted table 
mytesttable=table(test$Species,predtest)
mytesttable
round(prop.table(mytesttable)*100,2)














