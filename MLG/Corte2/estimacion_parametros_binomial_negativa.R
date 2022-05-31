library(MASS)
datos = data("quine")
attach(datos)
n = dim(quine)[1]
y = quine$Days
x = quine$Sex
X=cbind(1,x)

iter=100
Beta=matrix(ncol=2,nrow=iter+1)
alpha = matrix(ncol=1, nrow = iter + 1)
Beta[1,]=c(0,2)
alpha[1,]=0.5

for(i in 1:iter){
  lambda = exp(X%*%Beta[i,])
  z = as.vector((y-as.vector(lambda))/(as.vector(lambda)))
  num = sum(-log(alpha[i,]+as.vector(lambda))-((alpha[i,]+y)/(alpha[i,]+as.vector(lambda)))+ digamma(y+alpha[i,]) + digamma(alpha[i,]) +1 + log(alpha[i,]))
  den = sum(trigamma(y+alpha[i,]) -trigamma(alpha[i,]) - (2/(alpha[i,] + lambda)) + ((alpha[i,] + y)/(alpha[i,] + lambda)^2)) + (1/alpha[i,])
  alpha[i+1,] = alpha[i,] + (num/den)
  W = diag(as.vector((alpha[i,]%*% as.vector(lambda))/(alpha[i,] + as.vector(lambda))))
  Beta[i+1,]=solve(t(X)%*%W%*%X)%*%t(X)%*%W%*%(X%*%Beta[i,]+z)
}


a=glm.nb(quine$Days~quine$Sex)
summary(a)

modelo1 = glm(quine$Days~quine$Sex,family = poisson)


# Sobredispersion ---------------------------------------------------------


library(AER)
dispersiontest(modelo1,trafo = 1)
## se rechaza la hipotesis nula evidencia estadistica de que hay sobredispersión
