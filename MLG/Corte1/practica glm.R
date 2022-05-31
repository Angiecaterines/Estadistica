library(MASS)
install.packages("DescTools")

antecedentes<-c(rep("SI",8),rep("NO",8))
genero<-c(rep(c("M","M","F","F"),4))
hijos<-c(rep(c(0,0,0,0,3,3,3,3),2))
edad<-c(rep(c(65,75),8))
depresion<-c("SI","SI","SI","SI","NO","SI","SI","SI","SI","NO","NO","NO","SI","NO","SI","NO")
depresion<-ifelse(depresion == "SI", 1, 0)
modelo1<-glm(depresion ~ genero + hijos + edad + antecedentes)
summary(modelo1)

stepAIC(modelo1, direction = "both")

mimodelofav<-glm(depresion ~ antecedentes, family = binomial(link = "logit"))
summary(mimodelofav)

### Criterio de la deviance
1-pchisq(21.170-16.613, 1)

### Pseudo R^2
library(DescTools)
PseudoR2(mimodelofav,"Nagelkerke")

plot(residuals(mimodelofav, type = "pearson"))
?residuals

plot(residuals(mimodelofav2, type = "working"), fitted.values(mimodelofav2))

mimodelofav2<-glm(depresion ~ antecedentes, family = binomial(link = "probit"))

probabilidad2 = exp(  -0.5108 +  2.4567*(0)) / (1+ exp(-0.5108 +  2.4567*(0)))
### La probabilidad se calucula como el exp(B0 + BiXi) / 1 + exp(B0 + BiXi) 
### por los x del problema

# Problema 2 --------------------------------------------------------------

lealtad<-c(rep(1,15),rep(0,15))
marca<-as.factor(c(4,6,5,7,6,3,5,5,7,7,6,5,7,5,7,3,4,2,5,4,3,3,3,4,6,3,4,3,5,1))
producto<-as.factor(c(3,4,2,5,3,4,5,4,5,6,7,6,3,1,5,1,6,5,2,1,3,4,6,4,3,6,3,5,5,3))
compra<-as.factor(c(5,4,4,5,4,5,5,2,4,4,2,4,3,4,5,3,2,2,4,3,4,5,3,2,6,3,2,2,3,2))

modelo2<- glm(lealtad ~ marca + producto + compra)
summary(modelo2)

stepAIC(modelo2, direction = "both")

modelo3<-glm(as.factor(lealtad) ~ marca + compra, family = binomial)
aa<-summary(modelo3)

1-pchisq(aa$null.deviance-aa$deviance,aa$df.null-aa$df.residual)
PseudoR2(modelo3,"Nagelkerke")
residuals(modelo3, type = "pearson")
aa

probabilidad2 = exp(  -2.257e+01 +  4.211e+01) / (1 + exp(  -2.257e+01 +  4.211e+01))
