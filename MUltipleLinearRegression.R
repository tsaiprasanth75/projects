mydata=mydata[,-1]
mydata

#preliminary analysis_ Scatter plot 
attach(mydata)
plot(Temperature,Conversion,pch=16,col='blue')
grid(col = 'grey')
plot(Time,Conversion,pch=16,col='blue')
grid(col = 'grey')
plot(`Kappa number`,Conversion,pch=16,col='blue')
grid(col = 'grey')
#Corrrelation matrix
cor(mydata)
heatmap(cor(mydata))
#Variance Inflation factor less than 5 to ignore multicollinearity
#Import the package
library(car)
#Develop the Regression model

mymodel=lm(Conversion~Temperature+Time+`Kappa number`)
#Multicollinearity-VIF

vif(mymodel)
library(MASS)
stepAIC(mymodel,direction = 'both')

finalmodel=lm(Conversion~Temperature+`Kappa number`)
vif(finalmodel)

summary(finalmodel)

ypred=predict(finalmodel)

res=residuals(finalmodel)
#MOdel Adequcy check
qqnorm(res,pch=16,col='blue')
qqline(res,col='red')

shapiro.test(res)

#REsiduals vs predicted

plot(ypred,res)

#Mean Squared error
mse=mean(res^2)
mse
rmse=sqrt(mse)
rmse
#Residual sum of square

res_ss=sum(res^2)
res_ss
#Total info in the data
total_ss=var(Conversion)*(16-1)
total_ss
res_ss/total_ss
r2=1-res_ss/total_ss
#This is basicaly R squared
#You can also calculate adj R2

adj_rsq=1-(1-r2)*(16-1)/(16-2-1)
adj_rsq


#plot(Conversion,ypred)

#REgression ANOVA

#MOdel Generalizability
#LOOCV
library(boot)
attach(mydata)
mymodel=glm(Conversion~Temperature+`Kappa number`)
summary(mymodel)
mycv=cv.glm(mydata,mymodel)
mycv=cv.glm(mydata,mymodel)
loocv_mse=mycv$delta[1]
round(loocv_mse,3)
loocv_rmse=sqrt(loocv_mse)
#K fold Cross Validation

set.seed(1)
mycv=cv.glm(mydata,mymodel,K=4)
kfold_mse=mycv$delta[1]
kfold_mse
kfold_rmse=sqrt(kfold_mse)
kfold_rmse
























