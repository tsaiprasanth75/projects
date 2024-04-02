mydata = mydata[,-1]
#Attach the data file
attach(mydata)
#Import the package
library(e1071)

#Hyperparameter tuning
tune_mymodel = tune(svm,Conversion~.,data=mydata,kernel='linear',ranges = list(cost=c(0.001,0.01,0.1,1,5,10,50,100)))
summary(tune_mymodel)

#Choose Optimum model
mymodel= tune_mymodel$best.model
summary(mymodel)

#Predicted values
ypred=predict(mymodel,mydata)
#Residuals
res= Conversion-ypred

#MSE

mse= mean(res^2)
round(mse,3)
res_ss=sum(res^2)
total_ss =var(Conversion)*(16-1)
rsq= 1-(res_ss/total_ss)
rsq
#Optimize with polunommial kernel
tune_mymodel = tune(svm,Conversion~.,data=mydata,kernel='polynomial',ranges = list(cost=c(0.001,0.01,0.1,1,5,10,50,100),degree=c(1,2,3,4)))
summary(tune_mymodel)

#Choose Optimum model
mymodel= tune_mymodel$best.model
summary(mymodel)

#Predicted values
ypred=predict(mymodel,mydata)
#Residuals
res= Conversion-ypred

#MSE

mse= mean(res^2)
round(mse,3)
res_ss=sum(res^2)
total_ss =var(Conversion)*(16-1)
rsq= 1-(res_ss/total_ss)
rsq


#Optimize with radial kernel
tune_mymodel = tune(svm,Conversion~.,data=mydata,kernel='radial',ranges = list(cost=c(0.001,0.01,0.1,1,5,10,50,100),gamma=c(0.5,1,2,3,4,5)))
summary(tune_mymodel)

#Choose Optimum model
mymodel= tune_mymodel$best.model
summary(mymodel)

#Predicted values
ypred=predict(mymodel,mydata)
#Residuals
res= Conversion-ypred

#MSE

mse= mean(res^2)
round(mse,3)
res_ss=sum(res^2)
total_ss =var(Conversion)*(16-1)
rsq= 1-(res_ss/total_ss)
rsq





