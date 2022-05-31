
###punto 1

#a) Diseño 2^2 con 3 replicas
#Este diseño tiene 2 factores A y B cada uno con  replicas a comparar

#b)
#tiene 2 tratamientos y 3 replicas

#c)

res<-c(82,78,71,89,80,82,70,88,84,79,66,93)
A<-as.factor(rep(c("-1","+1","-1","+1"),3))
B<-as.factor(rep(c("-1","-1","-1","+1"),3))
replica<-as.factor(c(rep("I",4),rep("II",4),rep("III",4)))
modelo1<-lm(res~A*B+replica)
anova(modelo1)

#d no existe ningun tratamiento mejor,no se existe ningun tratamiento ni interacción =!0


# supuestos

###Normalidad

# Normalidad de los residuos
#Ho:hay normalidad
#Ha:No hay normalidad

shapiro.test(residuals(modelo1))
#normalidad en los errorres


#varianza constante 
#Ho:Existe varianza constante
#Ha:No hay varianza constante
library(car)
ncvTest(modelo1)

# no existe varianza constante

#errores aleatorio
#Ho:p=0
#ha:independencia de los errores

durbinWatsonTest(modelo1)
#los errores son independientes



#punto 2
#a) Es un diseño 2^k con k=3 con 2 plicas
#b) 3 tratamientos con 2 replicas

#c) Analisis de varianza

A<-as.factor(rep(c("-1","1","-1","1","-1","1","-1","1"),2))
B<-as.factor(rep(c("-1","-1","1","1","-1","-1","1","1"),2))
C<-as.factor(rep(c("-1","-1","-1","-1","1","1","1","1"),2))
Viscosidad<-c(13.3,14.7,14.6,14.3,16.9,15.5,17.4,18.9,13.9,14.4,14.9,14.1,17.2,15.1,17.1,19.2)

modelo1<-lm(Viscosidad~A*B*C)
anova(modelo1)

# El ingrediente B y C tienen un efecto en la viscosidad de la leche
#Las interacciones B-C y A-B-C tienen un efecto en la viscosidad de la leche

modelo<-aov(Viscosidad~A*B*C)
aa<-data.frame(A,B,C,Viscosidad)
library(agricolae)
TukeyHSD(modelo)

#El tratamiento  C minimiza la viscosidad

#Supuestos del modelo

###Normalidad

# Normalidad de los residuos
#Ho:hay normalidad
#Ha:No hay normalidad

shapiro.test(residuals(modelo1))
# no hay normalidad  en los errorres


#varianza constante 
#Ho:Existe varianza constante
#Ha:No hay varianza constante
library(car)
ncvTest(modelo1)

# existe varianza constante

#errores aleatorio
#Ho:p=0
#ha:independencia de los errores

durbinWatsonTest(modelo1)
#los errores son independientes
