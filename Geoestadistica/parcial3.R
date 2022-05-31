
# Librerias ---------------------------------------------------------------


library(palr)
library(gstat)
library(sp)
library(tidyverse)


# Datos -------------------------------------------------------------------

# Este conjunto de datos proporciona ubicaciones y concentraciones de metales 
# pesados en la capa superior del suelo (ppm)

data("meuse.all")
coordinates(meuse.all)= ~x+y


# Analisis Exploratorio ---------------------------------------------------


apply(meuse.all,2,function(x) sum(is.na(x)))

# métodos de imputación múltiple implementados en el paquete mice de R

multimp.mice<-mice::mice(meuse.all@data,m = 5)
meuse<-complete(multimp.mice)

par(mfrow = c(1,4))
hist(meuse$om)
plot(density(meuse$om, from = 0, to = 20),main = "om",col="blue")

hist(log(meuse$om))
plot(density(log(meuse$om), from = 0, to = 5),main = "om",col="red")
shapiro.test(log(meuse$om))
 qqnorm(log(meuse$om),col='cyan')
qqline(log(meuse$om),col='blue')
summary(meuse)
meuse$lom=log(meuse$om)


a=cbind(meuse,meuse.all@coords)
coordinates(a)= ~x+y

par(mai = c(0,0,0,0))

## la mayor concentración  de porcentaje de materia orgánica) 
# se encuentra a las orillas del rio
spplot(a,"lom")

v = variogram(om~1,a,)
v
plot(v)

# la variable es estacionaria
                                                                                                      
show.vgms()
# Analisis Estructural ----------------------------------------------------


v1 = variogram(om~1,a,cutoff = 2000,alpha= c(0,45,90,130))
v1
plot(v1)

# la variable parece ser estacionaria, pero 
# es anisotropica es decir la variabilidad oservada difiere dependiendo la
# dirección de analisis


# escogeria el variograma medido con dirección 45,
# ya que este conveme mas rapido, es decir no necesito
# realizar demasiadas mediciones a diferentes distancias para
# encontrar la variabilidad de la variable.

# Ajuste variograma téorico -----------------------------------------------



v4<-v1 %>% filter(dir.hor==45)
max(dist(coordinates(a)))

show.vgms()

vexp = fit.variogram(v4,vgm(model = "Cir"))
vgau = fit.variogram(v4,vgm(model = "Pen"))
vesf = fit.variogram(v4,vgm(model = "Sph"))

par(mfrow = c(1,3))
a1=plot(v4,vexp)
a2=plot(v4,vgau)
a3=plot(v4,vesf)
gridExtra::grid.arrange(a1,a2,a3,ncol=3)

# el punto maximo donde se estabiliza la varianza mas
# cercano al origen es el del modelo gausiano sin embargo
# los puntos no difieren mucho, sin embargo si se observa el rango
# el modelo exponencial es el mas rapido en estabilizarse 
# a una distancia de 173 es decir a partir de esta distancia ya
# no existe correlación

# mapa de predicciones ----------------------------------------------------

new = data.frame(x=8, y= 10)
coordinates(new)=~x+y
p=krige(lom~1,a,new,vexp);p

## para la coordenada 8,10 la varianza
# estimada es de 8.64 y el valor estimado de zo que se predice en es punto es 1.96

exp(p$var1.pred)

# en el punto 8, 10 se pronostica una porcentaje promedio 
# de materia organica de 6.9


data("meuse.grid")
coordinates(meuse.grid)=~x+y
plot(meuse.grid)
p2=krige(lom~1,a,meuse.grid,vexp);p2
spplot(p2,"var1.pred")
# La mayor concentración promedio de materia organica en el rio
# se presenta en la orilla norte sin embargo se observa que existe
# alta concentración promedio de materia organica a lo largo del rio
# en especial en el centro de este
