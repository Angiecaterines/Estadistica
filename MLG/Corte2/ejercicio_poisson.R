
# librerias ---------------------------------------------------------------

library(readr)
library(MASS)
library(DescTools)


# Datos -------------------------------------------------------------------

datos_poisson <- read_table2("EL BOSQUE/2022-1/MLG/Corte2/datos_poisson.txt")
attach(datos_poisson)
modelo1 = glm(accidentes ~ tipo_barco + meses_servicio,data = datos_poisson,family = poisson)
summary(modelo1)

# parece ser que solo estan siendo significativas las variables tipo de barco D y meses
# de servicio.


# selección del mejor modelo ----------------------------------------------

stepAIC(modelo1)
modelo2 = glm(accidentes ~ tipo_barco + meses_servicio,data = datos_poisson,family = poisson)
a = summary(modelo2)


# Deviance ----------------------------------------------------------------

1 - pchisq(a$null.deviance-a$deviance,a$df.null-a$df.residual)

# No se rechaza ho: el modelo poisson se ajusta a las datos.


# pseudo r^2 --------------------------------------------------------------

PseudoR2(modelo2,"Nagelkerke")


# 2) Probabilidad ---------------------------------------------------------

# exp(Bo + B1x1 + B2x2)
beta = a$coefficients[,1]
exp(beta[1] + beta[2]*1 + beta[6]*72)
# se espera que en promedio con 72 meses de servicio y tipo B tenga 9.46 daños 


