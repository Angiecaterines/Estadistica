# librerias ---------------------------------------------------------------

library(MASS)
library(DescTools)

lcelulas <- c(3.36,2.88,3.63,3.41,3.78,4.02,4,4.72,5,4.23,3.73,3.85,3.97,4.51,5.54,5,5)
tiempo <- c(65,156,100,134,16,108,121,5,65,4,39,143,56,26,22,1,1)
y = tiempo
x = lcelulas
X=cbind(1,x)

iter=100
Beta=matrix(ncol=2,nrow=iter+1)
Beta[1,]=c(1,0)

for(i in 1:iter){
  lambda = exp(X%*%Beta[i,])
  W = diag(as.vector(lambda))
  z = (y - as.vector(lambda))/as.vector(lambda)
  Beta[i+1,]=solve(t(X)%*%W%*%X)%*%t(X)%*%W%*%(X%*%Beta[i,]+z)
}
Beta[i+1,]
modelo1 <-glm(y~x,family=poisson)
a<-summary(modelo1)

# Deviance ----------------------------------------------------------------



1 - pchisq(a$null.deviance-a$deviance,a$df.null-a$df.residual)

# No se rechaza ho: el modelo poisson se ajusta a las datos.


# Pseudor^2 ---------------------------------------------------------------

PseudoR2(modelo1,"Nagelkerke")



# Residuales --------------------------------------------------------------

table(residuals(modelo2,type = "pearson")>2)
table(residuals(modelo2,type = "deviance")>2)
plot(residuals(modelo2,type = "pearson"))
plot(residuals(modelo2,type = "deviance"))

# no existe la presencia de datos atípicos

##conteos

x = c(1,3)

exp(x%*%Beta[45,c(1,2)])
