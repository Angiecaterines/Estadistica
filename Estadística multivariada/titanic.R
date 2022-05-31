library(tidyverse)


Titanic
X = margin.table(Titanic,c(1,3))
colnames(X)<-c("Niños","Adultos")
chisq.test(X, simulate.p.value = T, B = 1000) 

# Con un nivel de significancia menor al 1% se rechaza 
# la hipótesis de independencia entre clase  social 
# y edad de los pasajeros en la muestra.


# Porporciones marginales -------------------------------------------------

p1<-round(prop.table(X,1)*100,2);p1
#Dado que el pasajero pertenece a las clases sociales 1,2 se espera que sea adulto
#Sí el pasajero pertenece a la clase social 3 se espereraria que sea adulto
#Sí el pasajero pertenece a la tripulación es adulto


p2<-round(prop.table(X,2)*100,2);p2
# Dado que el pasajero sea niño se espera que perteneza a la 3 clase social.
# Si el pasajero es adulto es más probable que pertenezca a la clase social 3 
# o a la tripulación

plot(prop.table(X,1),col = gray(c(.2,.4,.6,.8)),main ="", las = 1)

#Sin importar la clase socail es mas probable que los pasajeros sean niños

plot(prop.table(t(X),1),col = gray(c(.3,.5,.7,.9)),main ="", las = 1)
# Es mas probable que si el pasajero es niño pertenezca a la 3 clase social
# Sí el pasajero es adulto es mas probable que pertenezca a la 3 clase social 
# y a la tripulación

require(FactoMineR)
require(factoextra)
ac = CA(X, graph = T)
ac$eig
ac$row
ac$col
fviz_screeplot(ac, addlabels = TRUE)
fviz_ca_biplot(ac, repel = TRUE)

datos<-as.data.frame(Titanic)
nomale<-datos %>% filter(Sex=="Male" & Survived=="No") %>% group_by(Class)%>%summarise(sum(Freq))
yesmale<-datos %>% filter(Sex=="Male" & Survived=="Yes") %>% group_by(Class)%>%summarise(sum(Freq))
nofemale<-datos %>% filter(Sex=="Female" & Survived=="No") %>% group_by(Class)%>%summarise(sum(Freq))
yesfemale<-datos %>% filter(Sex=="Female" & Survived=="Yes") %>% group_by(Class)%>%summarise(sum(Freq))
tabla<-cbind(nomale,yesmale[,2],nofemale[,2],yesfemale[,2])

arreglo<-array(c(118,62,4,141,154,25,13,93,422,88,106,90,670,192,3,20),dim=c(4,4),dimnames=list(c("1st","2nd","3rd","Crew"),
						      c("Nohombres","Sihombres","Nomujer","Simujer")))
tabla<-as.table(arreglo)
names(attributes(tabla)$dimnames) <- c("Clase","Sexo")

chisq.test(tabla, simulate.p.value = T, B = 1000) 

# Con un nivel de significancia menor al 1% se rechaza 
# la hipótesis de independencia entre clase  social 
# y sexo de los pasajeros que sobrevivieron o no en la muestra.


# Porporciones marginales -------------------------------------------------

# Gráficos frecuencias condicionales

t1 = round(prop.table(tabla,1)*100,2) ; t1
# es mas probable sobrevivir si se pertenece a la clase social 1 y 2 para las mujeres.
# es mas probable  no sobrevivir si se pertenece a la tribulación para los hombres. y a la 3 para las mujeres


t2 = round(prop.table(t(tabla),1)*100,2) ; t2

# dado que se es hombre es mas probable sobrevivir si se pertenece a la clase social 1 y morir si se pretence a la tripulación
# Dado que se es mujer es más probable sobrevivir si se pertenece a la clase social 1 ,2
# Dado que se es mujere es mas probable morir si se pertenece a la clase social 1

plot(t1,col = gray(c(.2,.4,.6,.8)),main ="", las = 1)
plot(t2,col = gray(c(.2,.4,.6,.8)),main ="", las = 1)

require(FactoMineR)
require(factoextra)
ac = CA(tabla, graph = T)
ac$eig

# A partir de los valores propios del AC, con los dos primeros
# ya se obtiene un 97.9% de la variabilidad (o de la información
# de la chi-cuadrado.

ac$row
# Para la primera dimensión la clase social tribulación es la   que más 
# contribuye  y en cuanto a la 
# dimensión 2 la clase social que mas contribuye es la 3

ac$col

# En la primera componenete prncipal el sexo que mas contribuyo fue los
# hombres uqe murieron seguido de las mujeres que sobrevivieron, en cuanto
# a la segunda componente las mujeres que murieron son las que más contribuyen.

fviz_screeplot(ac, addlabels = TRUE)

fviz_ca_biplot(ac, repel = TRUE)

# De acuerdo a las contribuciones de las modalidades de las variable mujere que sobrevivio
# hombres que murieron contribuyen mas a la primera dimensión,igual sucede con la primera 
# clase social y la tripulación.Para la segunda dimensión,tiene mayor contribución las
# mujeres que muerieron y la 3erd clase social tienen la mayor contribución

# Se observa asociacion entre supervivencia de las mujeres y las clases sociales 1 y 2
# entre la muerte de los hombres y la tripulación, no se muestra asociaón entre la
# tercera clase social y la mujerte de las mujeres

##lo mas cercano al origen primero

