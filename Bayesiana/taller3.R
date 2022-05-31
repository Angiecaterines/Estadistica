

# Librerias ---------------------------------------------------------------

library(readxl)
library(tidyverse)
library(LearnBayes)
library(BayesVarSel)

# Datos -------------------------------------------------------------------

arboles <- read_excel("C:/Users/Hp/Downloads/Volumen.xlsx")

# Modelo con todas las variables explicativas ------------------------------------------


modelo1 <- lm(Volumen ~ ., data =arboles[,-c(1)] , x = T, y = T)
summary(modelo1)

modB = blinreg(modelo1$y, modelo1$x, 5000)
apply(modB$beta,2,quantile,c(.05,.5,.95))




# Modelo con la variable explicativa altura -------------------------------------------

modelo2 <- lm(Volumen ~ ., data =arboles[,-c(1,2)] , x = T, y = T)
summary(modelo2)

modB2 = blinreg(modelo2$y, modelo2$x, 5000)
apply(modB2$beta,2,quantile,c(.05,.5,.95))


# Modelo con la variable explicativa diametro -----------------------------


modelo3 <- lm(Volumen ~ ., data =arboles[,-c(1,3)] , x = T, y = T)
summary(modelo3)

modB3 = blinreg(modelo3$y, modelo3$x, 5000)
apply(modB2$beta,2,quantile,c(.05,.5,.95))


# Selección del mejor modelo ----------------------------------------------

# En este sentido, se hará uso del paquete BayesVarSel para realizar la 
# selección del mejor modelo a partir de un conjunto de modelos candidatos

mod1<-lm(Volumen ~ Altura + Diámetro,data=arboles)
mod2<-lm(Volumen ~ Altura,data=arboles)
mod3<-lm(Volumen ~ Diámetro,data=arboles)
mod4<-lm(Volumen ~. ,data=arboles[-1])

crime.BF<- Btest(models=list(modelobase=mod1,
                             modeloaltura=mod2), data=arboles)
crime.BF$BFi0
crime.BF$PostProbi  




crime.BF2<- Btest(models=list(modelobase=mod1,
                             modelodiametro=mod3), data=arboles)
crime.BF2$BFi0
crime.BF2$PostProbi


# A partir de las probabilidades posteriores fue posible observar en los dos casos que 
# el modelo completo es quien sería elegido, ya que presenta la mayor probabilidad entre todos los que se compararon.
