library(ca)
smoke
# as two variables are categorical plot mosaic plot
mosaicplot(smoke,col=c(2,3,4,5))
plot(ca(smoke))
# Row and column profile

plot(ca(smoke),mass = c(TRUE,TRUE))
#Check the independence 
chisq.test(smoke)
eig1=ca(smoke)
eig1
library(FactoMineR)
library(factoextra)
eig2=get_eig(eig1)
eig2
trace=sum(eig2$eigenvalue)
trace
corr=sqrt(trace)
corr
fviz_screeplot(eig1)
eig1
library(corrplot)
row=get_ca_row(eig1)
row
corrplot(row$contrib,is.corr = FALSE)
fviz_contrib(eig1,choice = 'row',axes=1)
fviz_contrib(eig1,choice = 'row',axes=2)
fviz_contrib(eig1,choice = 'row',axes=3)

plot(eig1,map='rowprincipal')
plot(eig1,map='colprincipal')
plot(eig1,mass='TRUE',contrib = 'absolute',map='rowgreen',arrows = c(FALSE,TRUE))


plot(eig1,mass='TRUE',contrib = 'absolute',map='colgreen',arrows = c(TRUE,FALSE))





