## encuentro de los parametros
# se utiliza para c...lasificación financiera

n=100
y=rbinom(n,1,0.5)
x=rpois(n,4)

X=cbind(1,x)

iter=100
Beta=matrix(ncol=2,nrow=iter+1)
Beta[1,]=c(0,0)

for(i in 1:iter){
  pi=exp(X%*%Beta[i,])/(1+exp(X%*%Beta[i,]))
  W=diag(as.vector( pi*(1-pi))  )
  z=(y-pi)/(pi*(1-pi))
  Beta[i+1,]=solve(t(X)%*%W%*%X)%*%t(X)%*%W%*%(X%*%Beta[i,]+z)
}


glm(y~x,family = binomial)
glm(y~x,family = binomial(link=logit))
glm(y~x,family = binomial(link=cloglog))
glm(y~x,family = binomial(link=probit))
glm(y~x,family = binomial(link=cauchit))
glm(y~x,family = binomial(link=log))

modelo1 = glm(y~x, family = binomial)
exp(modelo1$coef)


# ejemplo1 ----------------------------------------------------------------
# Si=1 No=0
antecedentes=c(rep(1,8),rep(0,8))

# genero M=0 F=1
genero = rep(c(0,0,1,1),4)
nhijos = c(rep(0,4),rep(3,4),rep(0,4),rep(3,4))
edad = c(rep(c(65,75),8))
depresion = c(1,1,1,1,0,1,1,1,1,0,0,0,1,0,1,0)
ejm1 =glm(depresion~antecedentes+genero+nhijos+edad,family=binomial)
summary(ejm1)

exp(ejm1$coef)
round(exp(ejm1$coef),4)
# El número de hijos y el genero no influyen sobre 
# la depresión de los adultos mayores.

# El hecho de tener antecedentes de depresión aumenta el "odds"
# de tener depresión un adulto mayor en 15. 

1/round(exp(ejm1$coef),4)

# a mayor edad el riesgo de tener depresion en 1.17 puntos



# 2 da parte problema -----------------------------------------------------


# medidas de bondad de ajuste

# anova de regresión
# Prueba Omnibus-En el modelo logit
#Ho: El modelo No se ajusta a los datos.

#1-pchisq(Null deviance-Residual deviance,grados de libertad)
1-pchisq(21.15-15.10,4)

# No se rechaza Ho.Existe evidencia estadistica de que el modelo
# de regresión logistico binario no se ajusta a los datos

ejm1$coefficients

## pseudo R2 de modelos Logic
modelo0=glm(depresion~1, family=binomial)
n=length(depresion)
1-exp(-(15.10-21.17)/n)/(1-exp(-(2*logLik(modelo0))/n))

logLik(modelo0)
'log Lik.' -10.58501
(1-exp(-(15.10-21.17)/n))/(1-exp(-(-2*10.58501)/n))

# Existe una mala explicación del diagnostico de la depresión en terminos de
# los predictores.
 
## pseudo r Cuadrado
library(rms)
lrm(depresion~edad+nhijos+genero+antecedentes)


## tabla de clasificación

va=fitted.values(ejm1)
table(ifelse(va>0.6,1,0),depresion)

# porcentaje de clasificación

(5+7)/16

# El modelo clasifica de forma correcta el 75% de la información

library(MASS)
stepAIC(ejm1,direction=c("both"))

## modelo final

modelof=lrm(depresion~antecedentes)

# existe evidencia estadistica de  que los datos 
# se ajustan al modelo de regresión logistico binomial


ejm1 =glm(depresion~antecedentes,family=binomial)

va=fitted.values(ejm1)
table(ifelse(va>0.6,1,0),depresion)

exp(-0.5108+2.4567+0)/(1+exp(-0.5108+2.4567+0))

# la probabilidad de que un anciano sin antecendtes de depresión sea diagnosticado es de 0.87

residuals(ejm1,type="pearson")
plot(residuals(ejm1,type="pearson"))
plot(residuals(ejm1,type="pearson"))

