
# ejercicio 1 -------------------------------------------------------------


# librerias ---------------------------------------------------------------

library(MASS)
library(DescTools)
library(readr)

# datos -------------------------------------------------------------------
leucemia <- read_table2("EL BOSQUE/2022-1/MLG/Corte2/datos_parcial_leucemia.txt")
leucemia <- leucemia[,-1]
leucemia$AG <- as.factor(leucemia$AG)

modelo1 <- glm(Time ~., data=leucemia,family=poisson)
modelo2 <- glm.nb(Time ~., data=leucemia)
modelo3 <- glm(Time ~., data=leucemia,family = Gamma)
summary(modelo1)
summary(modelo2)
summary(modelo3)

stepAIC(modelo1)
stepAIC(modelo2)
stepAIC(modelo3)

modelo4 <- glm(formula = Time ~ WBC + AG, family = poisson, data = leucemia)
modelo5 <- glm.nb(formula = Time ~ AG, data = leucemia)
modelo6 <- glm(formula = Time ~ WBC + AG, family = Gamma, data = leucemia)


# Medidas de bondad de ajuste ---------------------------------------------


PseudoR2(modelo4,"Nagelkerke")
PseudoR2(modelo5,"Nagelkerke")
PseudoR2(modelo6,"Nagelkerke")

# el mejor modelo es un modelo poisson
a<-summary(modelo4)
1 - pchisq(a$null.deviance-a$deviance,a$df.null-a$df.residual)

# el mejor modelo es un modelo poisson


# b) Conteo ---------------------------------------------------------------

# exp(X'B)
Beta = a$coefficients[,1]
x = c(1,1000,1)
exp(t(x)%*%Beta)

# el tiempo de vida que le resta a un paciente con un conteo de celulas 
#blancas en la sangre de 1000 y positivo en AG es de 84 semanas.


# c) Coeficientes del modelo ----------------------------------------------

# Bo = Se espera que por cada unidad de WBC  y AG positivo incrementen 3.227333e+00 semanas de muerte
# B1 = Se espera que cuando AGpositivo sea cero,el WBC disminuya en -1.475486e-05
# B2 = Se espera que cuendo WBC sea cero, el AGpositivo aumente 1.228085e+00


# d) Datos atipicos -------------------------------------------------------

table(residuals(modelo4,"pearson")>2)
table(residuals(modelo4,"deviance")>2)

# existe la presencia de 11 datos atipicos en la muesta de datos.


# Problema2 ---------------------------------------------------------------

mejillones <- read_table2("EL BOSQUE/2022-1/MLG/Corte2/datos_parcial_mejillones.txt")
mejillones <- mejillones[,-1]
mejillones$Feeding_level <- as.factor(mejillones$Feeding_level)

modelo1 <- glm(N_dead_mussels~.,data = mejillones,family = poisson)
modelo2 <- glm.nb(N_dead_mussels~.,data = mejillones)
modelo3 <- glm(N_dead_mussels~.,data = mejillones,family = Gamma)

stepAIC(modelo1)
stepAIC(modelo2)

modelo4<- glm(formula = N_dead_mussels ~ Max_temp + Min_temp + Feeding_level, 
              family = poisson, data = mejillones)
a<-summary(modelo4)
modelo5<- glm.nb(formula = N_dead_mussels ~ Min_temp + Feeding_level, data = mejillones)
b<-summary(modelo5)

# medidas de bondad de ajuste ---------------------------------------------

PseudoR2(modelo4,"Nagelkerke")
PseudoR2(modelo5,"Nagelkerke")

1- pchisq(103.66-31.93,23-19)
1- pchisq(71.007-24.514,23-20)

# el mejor modelo es modelo poisson


# b) Conteo ---------------------------------------------------------------

#exp(x'beta)
beta = a$coefficients[,1]
x = c(1,75,60,0,0)
exp(t(x)%*%beta)

#el número de mejillones que pueden fallecer en una jaula con altos niveles de comida, que
#fueron ubicados en una área con una temperatura máxima de 75 grados fahrenheit y mínima de
#60 es de 0.5157 mejillones.


# c) coeficientes estimados --------------------------------------------------

# Bo= Se espera que por cada grado fahrenheit de temperatura minima, máxima y por cada
# nivel de comida las muertes de los mejillones disminuyan en -11.60228765.

# B1 = Se espera que cuando la temperatura minima sea cero y los niveles de comida tambien
# la temperatura máxima incremente en 0.02888088 grados fahrenheit.

# B2 = Se espera que cuando la temperatura maxima sea cero y lo niveles de comida tambien
# la temperatura minima incremente en 0.14623422 grados fahrenheit.

# B3 = Se espera que cuando la temperatura minima y maxima sea cero y el nivel de comida bajo 
# tambien lo sea los niveles de comida medios incrementen en  1.41251118 unidades.

# B4 = Se espera que cuando la temperatura minima y maxima sea cero y el nivel de comida medios 
# tambien lo sea, los niveles de comida bajos incrementen en  1.77050190 unidades.


# d) residuales -----------------------------------------------------------

table(residuals(modelo4,"pearson")>2)
table(residuals(modelo4,"deviance")>2)

# existe presencia de dos datos atipicos en el conjunto de datos.


# Problema 3 --------------------------------------------------------------

ciclones <- read_table2("EL BOSQUE/2022-1/MLG/Corte2/datos_parcial_ciclones.txt")
ciclones <- ciclones[,-1]
modelo1 <- glm(Total ~.,data=ciclones,family = poisson)
modelo2 <- glm.nb(Total ~.,data=ciclones)
modelo3 <- glm(Total ~.,data=ciclones,family = Gamma)

stepAIC(modelo1)
stepAIC(modelo2)
stepAIC(modelo3)

modelo4<-glm(formula = Total ~ OND, family = poisson, data = ciclones)
modelo5<-glm.nb(formula = Total ~ OND, data = ciclones)
modelo6 <- glm(formula = Total ~ OND, family = Gamma, data = ciclones)

# Medidas de bondad de ajuste ---------------------------------------------



PseudoR2(modelo4,"Nagelkerke")
PseudoR2(modelo5,"Nagelkerke")
PseudoR2(modelo6,"Nagelkerke")

# si bien ninguno modelo ajusta bien, el mejor modelo es un modelo Gamma

a<-summary(modelo6)
# Número promedio esperado ------------------------------------------------

#exp(X'B)
Beta=a$coefficients[,1]
x = c(1,0.06)
exp(t(x)%*%Beta)

# el número promedio esperados de cyclones en la costa de Australia si JFM = 0.06,
# AMJ = 0.05, JAS = 0.07 y OND = 0.06 es de 1 ciclon


# residuales --------------------------------------------------------------

table(residuals(modelo6,"pearson")>2)
table(residuals(modelo6,"deviance")>2)

# No existe presencia de datos atipicos en el conjunto de datos.