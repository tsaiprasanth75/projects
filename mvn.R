#import package mvn
library(MVN)
#Multivariate Normality Test
#MArdia test
mytest=mvn(mydata,mvnTest = c('mardia'),multivariatePlot = 'qq')
mytest
mytest2=mvn(mydata,mvnTest = c('hz'),multivariatePlot = 'qq')
mytest2
mytest3=mvn(mydata,mvnTest = c('royston'),multivariatePlot = 'qq')
mytest3
