mydata=mydata[,-1]
library(FNN)
 # seperate x aand y

x=mydata[,1:3]
y=mydata$Conversion

#Develop the model

mymodel = knn.reg(x,NULL,y,k=3)

#Predict the response

ypred= mymodel$pred

#Residuals
res=mymodel$residuals
#Model Performace measure

mse=mean(res^2)
round(mse,4)

#R square 
rsq=mymodel$R2Pred
round(rsq*100,2)

#Optimum K
rsq=c(3:10)
for (j in 1:8) {
  mymodel=knn.reg(x,NULL,y,k=j+2)
  rsq[j]=mymodel$R2Pred
  
}


rsq= round(rsq*100,2)
rsq
k= c(3:10)
#Prepare the result table
myresult= cbind(k,rsq)
myresult
# here k is the hyperparameter so this is hyperparameter tuning
























