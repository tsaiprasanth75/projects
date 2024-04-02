# Read matrix A
A= matrix(c(21,57,89,31,7,98),nrow=2,ncol=3,byrow = T)
A
# read matrix B
B=matrix(c(24,35,15,34,56,25),nrow=3,ncol=2,byrow = T)
B
#Product of A and B
C = A%*%B
C
# REad the matrix
B= matrix(c(24,35,66,15,34,17,56,25,45),nrow=3,ncol=3,byrow = T)
#det of B
det(B)
#inv of B
invB=solve(B)
round(invB,4)
C=matrix(c(1,2,2,2,2,2,2,2,1),nrow=3,ncol=3,byrow = T)
C
y=matrix(c(1,2,3),nrow=3,ncol=1,byrow = T)
y
invC=solve(C)
x=invC%*%y
x

#eigen values and eigen vectors
myresult = eigen(B)
myvalues=myresult$values
myvalues
myvectors=myresult$vectors
myvectors
round(myvectors,3)
P=myvectors
#inverse of P
PInv=solve(P)
PInv
#Matrix D
D=matrix(c(myvalues[1],0,0,0,myvalues[2],0,0,0,myvalues[3]),nrow=3,ncol=3,byrow = T)
D
#Computation of Matrix
mymatrix=P%*%D
mymatrix=mymatrix%*%PInv
mymatrix
