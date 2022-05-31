
# librerias ---------------------------------------------------------------

library(MASS)
library(DescTools)


# datos -------------------------------------------------------------------

minero<-c(0,0,rep(1,6),0,1,1,0,1,0,rep(1,4),0,1,rep(0,8),rep(1,3))
eucs<-c(2,10,16,20,19,18,12,16,3,12,32,2,16,7,10,15,30,4,4,19,11,0,0,0,3,8,8,15,21,24,15)
area <-c(22,11,51,22,4,61,16,14,5,6,3,13,24,32,22,27,18,9,21,14,4,5,19,12,15,38,24,16,6,16,15)
grazed <- as.factor(c(rep(0,9),1,0,1,1,0,1,1,0,1,1,0,1,1,1,1,0,1,0,0,0,1,1))
shrubs<- as.factor(c(rep(1,9),rep(0,9),1,rep(0,5),1,0,1,1,0,0,0))
bulokes <- c(120,67,85,45,160,75,100,321,275,227,23,277,243,117,97,99,45,123,96,261,160,192,323,190,222,148,198,93,88,138,70)
timber <-c(16,25,13,12,14,6,12,15,8,10,61,22,25,9,35,25,19,22,22,11,45,22,60,48,26,38,28,16,45,21,31)

modelo1 <- glm(minero ~ eucs + area+ grazed + shrubs+bulokes + timber,family = binomial)
summary(modelo1)

# parece ser que solo estan siendo significativas las variable número de eucaliptos en 
# cada 2 hectareas y el área pastoreada.

# seleccion del mejor modelo ----------------------------------------------

stepAIC(modelo1,direction = "both")

modelo2 <- glm(minero ~ eucs + grazed + shrubs + timber, family = binomial)
a<-summary(modelo2)


# Deviance ----------------------------------------------------------------



1 - pchisq(a$null.deviance-a$deviance,a$df.null-a$df.residual)

# No se rechaza ho: el modelo logistico binomial se ajusta a las datos.


# Pseudor^2 ---------------------------------------------------------------

PseudoR2(modelo2,"Nagelkerke")


# 2) Punto probabilidad ---------------------------------------------------

x = c(1,9,1,0,0,10)

exp(-6.7086+1.4941*0+12.7870*1+-0.5258*10)/1 + exp(-6.7086+1.4941*0+12.7870*1+-0.5258*10)

# la probabilidad de encontrar un ruidoso con 0 eucaliptos, 9 héctareas de vegetación 
# contigua que fuera pastoreado, sin arbustos presentes y sin arboles de bulokes y con
# 10 troncos de madera encontrados en el lugar es de 4.54.


# Residuales --------------------------------------------------------------

table(residuals(modelo1,type = "pearson")>2)
table(residuals(modelo1,type = "deviance")>2)
plot(residuals(modelo1,type = "pearson"))
plot(residuals(modelo1,type = "deviance"))

# existe la presencia de datos atípicos
     