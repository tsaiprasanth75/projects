#Compute Mean Vecctor
mu = colMeans(mydata)
mu
#Transform into col mat
mu=matrix(mu,nrow = 3,byrow = T)
mu
#Covariance matrix
sigma=cov(mydata)
sigma
n=50
p=3
#Compute Su
Su=(n/(n-1))*sigma
Su
#Compute Su Inv
SuInv=solve(Su)
SuInv
#Read the specified vector
muo = matrix(c(60,80,5),nrow = 3,byrow = T)
muo
#Compute Mean diff
mydiff=mu-muo
mydiff
#Take Transpose of diff
mydiff_T=t(mydiff)
mydiff_T
#calculate T square
Tsquare= mydiff_T%*%SuInv
Tsquare=Tsquare%*%mydiff
Tsquare=n*Tsquare
Tsquare
# Calculate  F statistic
Fstat=((n-p)/((n-1)*p))*Tsquare
Fstat
#Compute P value
p_value=pf(Fstat,p,(n-p),lower.tail = F)
p_value
