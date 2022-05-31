

# Problema 2 --------------------------------------------------------------


# librerias ---------------------------------------------------------------

library(MASS)
library(DescTools)
library(readr)


# datos -------------------------------------------------------------------


datos_quiz <- read_table2("EL BOSQUE/2022-1/MLG/Corte2/datos_quiz.txt")
deaths = datos_quiz$N_dead
max_temp = datos_quiz$Max_temp
min_temp = datos_quiz$Min_temp
hight = as.factor(c(rep(1,8),rep(0,16)))
med = as.factor(c(rep(0,8),rep(1,8),rep(0,8)))
low = as.factor(c(rep(0,16),rep(1,8)))
modelo1 = glm(deaths ~ max_temp+ min_temp+hight+med + low,family=poisson)
summary(modelo1)


# mejor modelo ------------------------------------------------------------

stepAIC(modelo1,direction = "both")

modelo2<-glm(formula = deaths ~ max_temp + min_temp + hight, family = poisson)
summary(modelo2)


# Medidas de bondad de ajuste ---------------------------------------------


# deviance ----------------------------------------------------------------

1- pchisq(103.656-33.634,23-20)

# se rechaza la hipotesis nula, es decir los datos se ajustan a un modelo
# de respuesta poisson


# pseudor^2 ---------------------------------------------------------------

PseudoR2(modelo2,"Nagelkerke")

# 2) Número de muertes ---------------------------------------------------------

# altos niveles de comida
# tem max 75
# tem min 60

#exp(b0+b1x1+b2x2+b3x3)
exp(-11.63269+0.03914*75+0.15841*60+-1.58209*1)

# el número de mejillones que pueden fallecer en una jaula con altos niveles de comida, que
#fueron ubicados en una ´area con una temperatura m´axima de 75 grados fahrenheit y m´inima de
#60 es de 0.46 mejillones.


# Interpretación coeficientes estimados -----------------------------------

# Bo = Se espera que cuando la temperatura maxíma, la temperatura minima y 
#      los niveles altos de comida sean cero las muertes de los mejillones decrezcan a 11 muertes

# B1 = Se espera que cuando la temperatura minima sea cero y los niveles de comida altos sean ceros
       # ocurra una temperatura maxima de 0.0391 grados fahrenheit.

# B2 = Se espera que cuando la temperatura maxima sea cero y los niveles de comida altos sean ceros
# ocurra una temperatura minima de 0.15841 grados fahrenheit.

# B3 = Se espera que cuando Se espera que cuando la temperatura maxima sea cero y la temperatura minima  sean ceros
# ocurra un decremento de la comida en -1.58209.


# Datos atipicos ----------------------------------------------------------

table(residuals(modelo2,type="pearson")>2)
table(residuals(modelo2,type="deviance")>2)

## existe la presencia de un dato atipico el dato 22



# Problema 3 --------------------------------------------------------------

library(readr)
library(AER)

# datos -------------------------------------------------------------------


datos_quiz_3 <- read_table2("EL BOSQUE/2022-1/MLG/Corte2/datos_quiz_3.txt")
cases = datos_quiz_3$Cases
pop = datos_quiz_3$Pop
age4054 <- rep(c(1,0,0,0,0,0),4)
age5559 <- rep(c(0,1,0,0,0,0),4)
age6064 <- rep(c(0,0,1,0,0,0),4)
age6569 <- rep(c(0,0,0,1,0,0),4)
age7074 <- rep(c(0,0,0,0,1,0),4)
age74 <- rep(c(0,0,0,0,0,1),4)
fredericia <- c(rep(1,6),rep(0,18))
horsens <- c(rep(0,6),rep(1,6),rep(0,12))
kolding <- c(rep(0,12),rep(1,6),rep(0,6))
vegle <- c(rep(0,18),rep(1,6))
modelo1 <- glm.nb(cases ~ pop + age4054+age5559+age6064+age6569+age7074+age74+
                    fredericia+horsens+kolding+vegle)

summary(modelo1)

modelo2 <-glm(cases ~ pop + age4054+age5559+age6064+age6569+age7074+age74+
                fredericia+horsens+kolding+vegle,family = poisson)

stepAIC(modelo1)
stepAIC(modelo2)
# parece ser que ninguna de las varaibles es capaz de estimar el número de sujetos 
# con cancer de pulmon.

modelo3 <-glm.nb(formula = cases ~ 1)
modelo4 <-glm(formula = cases ~ 1,family = poisson)
summary(modelo3)
summary(modelo4)

# Medidas de bondad de ajuste ---------------------------------------------


# Deviance ----------------------------------------------------------------

1-pchisq(27.016-27.016,0)
1-pchisq(27.704-27.704,0)


# Interpretación de parametros ------------------------------------------------
PseudoR2(modelo3,"Nagelkerke")
PseudoR2(modelo4,"Nagelkerke")

# Los datos no se ajustan al modelo con respuesta binomial negativa ni al modelo 
 # de respuesta poisson
# Número de casos ---------------------------------------------------------

exp(2.23359)
# El número de casos con cancer de pulmon son 9 dada la ciudad de V ejle en un grupo de
# 1000 hombres de 40 años.

