#Copy x variable 
myx= mydata[,2:6]
myy= mydata[,-1]
summary(myx)
#Scale the data

myzx=scale(myx)
myzx
summary(myzx)

#Combine scaled x with y
mynewdata=cbind(mydata$Resort_Visit,myzx)
mynewdata
#Change y variable name
colnames(mynewdata)[1]='Resort_Visit'


#Attach the data
mynewdata=as.data.frame(mynewdata)
attach(mynewdata)

#Use cd plot to see relation between x and y
cdplot(factor(Resort_Visit)~Family_Income,main = 'Conditional density plot',xlab='Family  Income',ylab = 'Resort visit',col = c(2,3))
cdplot(factor(Resort_Visit)~`Attitude Towards Travel`,main = 'Conditional density plot',xlab='Attitude Towards Travel',ylab = 'Resort visit',col = c(2,3))
cdplot(factor(Resort_Visit)~Importance_Vacation,main = 'Conditional density plot',xlab='Importance_Vacation',ylab = 'Resort visit',col = c(2,3))
cdplot(factor(Resort_Visit)~House_Size,main = 'Conditional density plot',xlab='House_Size',ylab = 'Resort visit',col = c(2,3))
cdplot(factor(Resort_Visit)~`Age _Head`,main = 'Conditional density plot',xlab='Age _Head',ylab = 'Resort visit',col = c(2,3))


#Develop linear discriminant model
library(MASS)
mymodel=lda(factor(Resort_Visit)~.,data = mynewdata)
mymodel

#Compute predicted values
ypred=predict(mymodel)
ypred

#Predicted class
predclass=ypred$class
predclass

#Predicted probabilities
predprob=ypred$posterior
predprob

#Predicted Discriminant function value

D= ypred$x
D


#Compare Actual vs predicted

myresult=cbind(Resort_Visit,predclass,predprob,D)

myresult=as.data.frame(myresult)

myresult$predclass=myresult$predclass-1
myresult

mytable=table(Resort_Visit,predclass)
mytable

prop.table(mytable)*100

#Accuracy
accuracy=40+46.67
accuracy

#Category wise accuracy
round(prop.table(mytable,1)*100,2)


















