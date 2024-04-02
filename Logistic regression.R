#attach the data
attach(mydata)
# Preliminary aanaalysis
# as y is categorical and x is continous
cdplot(factor(New_Vehicle_Purchased)~Income,main='conditional density plot')
cdplot(factor(New_Vehicle_Purchased)~Age_old_Vehicle,main='conditional density plot')
#Binary logistic regression model
#glm if the residuals  not normally distributed
mymodel=glm(New_Vehicle_Purchased~Income+Age_old_Vehicle,family = binomial(logit))
summary(mymodel)
#coefficeients significance test
anova(mymodel,test = 'Chisq')
FinalModel=glm(New_Vehicle_Purchased~Age_old_Vehicle,family = binomial(logit))
summary(FinalModel)
anova(FinalModel,test = 'Chisq')
#Check Model Significance(whether using the moeddl is better than not uing model)
nullmodel=glm(New_Vehicle_Purchased~1,family = binomial(logit))
anova(nullmodel,FinalModel,test = 'Chisq')
#Predictedd probability
ypred= predict(FinalModel,type='response')
#Combine with our data
ypred=round(ypred,4)
myresult = cbind(mydata,ypred)
myresult
#Compute Preddicted class
predclass= ifelse(ypred>0.5,'1','0')
myresult=cbind(myresult,predclass)
predclass
#Model Accuracy

mytable=table(New_Vehicle_Purchased,predclass)
mytable
prop.table(mytable)*100


#Model Generalizability
library(boot)
mycv=cv.glm(mydata,FinalModel)
loocv_mse=mycv$delta[1]
loocv_mse
#Residuals
res=New_Vehicle_Purchased-ypred
mse=mean(res^2)
mse#Training mse
#Kfold
set.seed(1)
mycv=cv.glm(mydata,FinalModel,k=5)
kflod_mse=mycv$delta[1]
#LOOCV ~ Accuraccy
myfun = function(){
  #Initialize the predicted probabilities
  preddprob=c(1:20)
  for (i in 1:20){
    #Develop model excluding ith record
    mymodel=glm(New_Vehicle_Purchased~Age_old_Vehicle,family = binomial(logit),data=mydata[-i,])
    #Compute Predicted prob for ith record
    preddprob[i]=predict(mymodel,type='response',newdata = mydata[i,])
    
  }
  return(preddprob)
}

loocv_pred=myfun()
loocv_pred
#Loocv predicted class
loocv_predictedclass=ifelse(loocv_pred>0.5,'1','0')
#Loocv Actual vs predictedd
myloocvtable=table(New_Vehicle_Purchased,loocv_predictedclass)
myloocvtable





























