
# Datos -------------------------------------------------------------------


library(MASS)
library(pscl)
zinb <- read.csv("https://stats.idre.ucla.edu/stat/data/fish.csv")
X <- cbind(1,zinb$child,zinb$camper)
Z <- cbind(1,zinb$persons)

iter=100
beta=matrix(ncol=3,nrow=iter+1)
beta[1,]=c(0,0,0)

gamma =matrix(ncol=2,nrow=iter+1)
gamma[1,]=c(0,0)

alpha = matrix(ncol=1,nrow = iter+1)
alpha[1,]=c(0)

for(i in 1:iter){
lambda = exp(X%*%beta[i,])
pi = exp(Z%*%gamma[i,])/(1+exp(Z%*%gamma[i,]))
dm=pi/(pi+(1-pi)*exp(-lambda))
dm=ifelse(zinb$count==0,dm,0)
WX = diag(as.vector((1-dm)*lambda/(alpha + lambda)))
VX = (zinb$count -lambda) / lambda
WZ=diag(as.vector(pi*(1-pi)))
VZ = (dm-pi)/(pi*(1-pi))
}


