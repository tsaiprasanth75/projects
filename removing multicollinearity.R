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

#VIF Calculation
mymodel2=lm(Temperature~Time+`Kappa number)

# vif=1/(1-r squared)

summary(mymodel)

#Stepwise regression model

library(MASS)
stepAIC(mymodel,direction = 'both')

finalmodel=lm(Conversion~Temperature+`Kappa number`)
vif(finalmodel)






