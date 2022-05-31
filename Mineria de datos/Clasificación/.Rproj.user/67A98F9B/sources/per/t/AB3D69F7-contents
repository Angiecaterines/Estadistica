
# Datos -------------------------------------------------------------------


library(MASS)
library(DescTools)
zinb <- read.csv("https://stats.idre.ucla.edu/stat/data/fish.csv")
X <- cbind(1,zinb$child,zinb$camper)
Z <- cbind(1,zinb$persons)

iter=100
beta=matrix(ncol=3,nrow=iter+1)
beta[1,]=c(0,0,0)

gamma =matrix(ncol=2,nrow=iter+1)
gamma[1,]=c(0,0)

for(i in 1:iter){
lambda=exp(X%*%beta[i,])
pi = exp(Z%*%gamma[i,])/(1 + exp(Z%*%gamma[i,]))
di = pi /(pi + (1- pi)*(exp(-lambda)))
di = ifelse(zinb$count==0,di,0)
WX = diag(as.vector(lambda *(1-di)))
VX = (zinb$count - lambda)/lambda
WZ = diag(as.vector(pi*(1-pi)))
VZ = (as.vector(di)-pi)/(pi*(1-pi))
gamma[i+1,] = ginv(t(Z)%*%WZ%*%Z)%*%t(Z)%*%WZ%*%(Z%*%gamma[i,]+VZ)
beta[i+1,] = ginv(t(X)%*%WX%*%X)%*%t(X)%*%WX%*%(X%*%beta[i,]+VX)
}


a<-zeroinfl(zinb$count~zinb$child + zinb$camper| zinb$persons)
b<-zeroinfl(zinb$count~1 | 1)

summary(a)
summary(b)

1-pchisq(-1127+1032,3)
## no se rechaza la hipotesis nula 