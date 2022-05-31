
# Poisson -----------------------------------------------------------------


# librerias ---------------------------------------------------------------


library(readr)
library(MASS)
library(DescTools)

# Datos -------------------------------------------------------------------


medicos <- read_table2("EL BOSQUE/2022-1/MLG/Corte2/datos_taller_poisson18.txt")
medicos$sexo <- as.factor(medicos$sexo)
medicos$residente <- as.factor(medicos$residente)
modelo1 <- glm(quejas ~.,data=medicos,family=poisson)
summary(modelo1)

# Parece ser que solo es significativa la variable nnúmero de consultas

# selección del mejor modelo ----------------------------------------------

stepAIC(modelo1)
modelo2 = glm(quejas ~ consultas + residente,data = medicos, family = poisson)
a = summary(modelo2)

# Deviance ----------------------------------------------------------------

1 - pchisq(a$null.deviance-a$deviance,a$df.null-a$df.residual)

# No se rechaza ho: el modelo poisson se ajusta a las datos.


# pseudo r^2 --------------------------------------------------------------

PseudoR2(modelo2,"Nagelkerke")


# 2) Probabilidad ---------------------------------------------------------

# exp(Bo + B1x1 + B2x2)
beta = a$coefficients[,1]
exp(beta[1] + beta[2]*10000 + beta[3]*1)
# se espera que en promedio hayan 1166 quejas para un residente con 10000 consultas
# en el año.



# Binomial negativa -------------------------------------------------------


# datos -------------------------------------------------------------------


a <- read_table2("EL BOSQUE/2022-1/MLG/Corte2/datos_taller_bn_18.txt")

b<-a[,c(8:12)]
a <-a[,c(2:6)]
names(b) = names(a)
estudio <- rbind(a,b)
estudio$Eth <- as.factor(estudio$Eth)
estudio$Sex <-as.factor(estudio$Sex)
estudio$Age <- as.factor(estudio$Age)
estudio$Lrn <- as.factor(estudio$Lrn)

modelo1 <- glm.nb(Days ~ .,data = estudio)
summary(modelo1)

## Parece ser que solo la etnia no aborigen es significativa
# selección del mejor modelo ----------------------------------------------

stepAIC(modelo1)
modelo2 = glm.nb(Days ~ Eth + Age + Lrn, data = estudio)
a = summary(modelo2)

# Deviance ----------------------------------------------------------------

1 - pchisq(a$null.deviance-a$deviance,a$df.null-a$df.residual)

# No se rechaza ho: el modelo poisson se ajusta a las datos.


# pseudo r^2 --------------------------------------------------------------

PseudoR2(modelo2,"Nagelkerke")


# 2) Probabilidad ---------------------------------------------------------

# exp(Bo + B1x1 + B2x2)
beta = a$coefficients[,1]
x = c(1,1,1,0,0,1)
exp(t(x)%*%beta)
exp(beta[1] + beta[2]*1 + beta[3]*1 + beta[6]*1)

# El número de fallas esperadas de un caballero de noveno grado, no aborigen, con un
#desempeño escolar bajo es 9 aproximadamente.

# residuales --------------------------------------------------------------


plot(residuals(modelo2, type="pearson"))
plot(residuals(modelo2, type="deviance"))

table(ifelse(abs(residuals(modelo2, type="pearson"))>2,1,0))
table(ifelse(abs(residuals(modelo2, type="deviance"))>2,1,0))

## en promedio existen 9 datos atipicos


# Gamma -------------------------------------------------------------------

library(readr)
laminas <- read_table2("EL BOSQUE/2022-1/MLG/Corte2/datos_taller_gamma_17.txt")
laminas$Day <- as.factor(laminas$Day)
laminas$Mach <- as.factor(laminas$Mach)
modelo1 <- glm(Perm ~.,data=laminas, family=Gamma)
summary(modelo1)


# Mejor modelo ------------------------------------------------------------

stepAIC(modelo1)

modelo2 <- glm(Perm ~ Day + Mach, family = Gamma, data = laminas)
a <- summary(modelo2)
# Deviance ----------------------------------------------------------------

1 - pchisq(a$null.deviance-a$deviance,a$df.null-a$df.residual)

#  se rechaza ho: el modelo poisson no se ajusta a las datos.


# pseudo r^2 --------------------------------------------------------------

PseudoR2(modelo2,"Nagelkerke")


# 2) Probabilidad ---------------------------------------------------------

# exp(Bo + B1x1 + B2x2)
beta = a$coefficients[,1]
exp(beta[1] + beta[8]*1)

#Estime la permeabilidad esperada promedio de una hoja fabricada 
#por la maquina A en el día 8 dicha permeabilidad es de 1

# residuales --------------------------------------------------------------


plot(residuals(modelo2, type="pearson"))
plot(residuals(modelo2, type="deviance"))

table(ifelse(abs(residuals(modelo2, type="pearson"))>2,1,0))
table(ifelse(abs(residuals(modelo2, type="deviance"))>2,1,0))

## no existe presencia de datos atipicos.

