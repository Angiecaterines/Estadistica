
# Paquetes ----------------------------------------------------------------

library(ggplot2)
library(mapview)
library(tidyverse)
library(RColorBrewer)
library(sf)
library(classInt)
library(magic)
library(purr)
library(maptools)
library(readxl)
library(raster) 
library(spdep) 
library(spData)
library(ape)
library(sp)
library(rgdal)
library(rgeos)
library(tmap)
library(spgwr)
library(grid)
library(gridExtra)
library(spatialreg)

# Datos -------------------------------------------------------------------

# Casos de covid Bogotá
covidb <- read_excel("C:/Users/Hp/Downloads/covidb.xlsx")

 # shape file de Bogotá
bo3<-read_sf("C:/Users/Hp/Downloads/Loca.shp") %>% arrange(LocNombre)

# Población total de Bogotá 2020
poblacion <- read_excel("C:/Users/Hp/Downloads/pobo.xlsx")
#proyecciones de población  bogotá 2016-2020
apply(array, margin, ...)                                                                                                                                       

# Resignación de nombres,formato estandár -------------------------------

covidb<-covidb %>% rename("LocNombre"="Localidad de residencia") %>% mutate(LocNombre=toupper(LocNombre),LocNombre=chartr("ÁÉÍÓÚ", "AEIOU", toupper(LocNombre))) 
covidb<-covidb %>% filter(LocNombre!="FUERA DE BOGOTA")
bo3$LocNombre<-replace(bo3$LocNombre,bo3$LocNombre=="CANDELARIA", "LA CANDELARIA")
covidb %>% group_by(LocNombre) %>% summarise(Total=n())->k

# II CREACIÓN BASE DE DATOS POR LOCALIDAD CON LAS SIGUIENTES VARIBLES

# 1.Edad promedio por localidad 
p=covidb%>% group_by(LocNombre) %>%  summarise(promedio=mean(Edad),desviación=sd(Edad));p

# 2. Casos de coronavirus por localidad 
a=data.frame(table(covidb$LocNombre));a 

# 3. caso de coronavirus por sexo 
t=as.data.frame(table(covidb$LocNombre, covidb$Sexo));t 
m=filter(t,Var2=="M");head(m)
fe=filter(t,Var2=="F");head(fe)

# 4. casos de coronavirus por estado 
fa=data.frame(table(covidb$LocNombre,covidb$Estado))
f=filter(fa,Var2=="Fallecido"); f 
r=filter(fa,Var2=="Recuperado"); r

# 5.Creación de base de datos para unir al shapelile 
casoslocalidad=data.frame(bo3$LocNombre,p$promedio, a$Freq, m$Freq, fe$Freq, f$Freq, r$Freq); head(casoslocalidad)
colnames(casoslocalidad)=c("LocNombre", "edad", "casos", "hombres", "mujeres", "fallecidos", "Recuperados")

#IV.Unir  casoslocalidad ( casos de coronavirus) y población (población por localidad)#########
# calcular casos por km2 y millon de habitantes 
base=merge(casoslocalidad, poblacion, by="LocNombre")  

mcas=round(1000000*base$casos/base$Poblacion,0);mcas # casos por millon de habitantes
mprop=round(1000000*base$fallecidos/base$casos,0);mprop # proporción de muertos
mfal=round(1000000*base$fallecidos/base$Poblacion,0);mfal # fallecidos por millon 
mrec=round(1000000*base$Recuperados/base$Poblacion,0);mrec # recueperados por millon

# variables eternas que pueden influir


poblacion
covid19=cbind(base,mcas,mprop,mfal,mrec);covid19


bogota=merge(bo3,covid19, by="LocNombre", all.x=TRUE) # une shapefile con archivo externo 

bogota <- st_as_sf(bogota)
glimpse(bogota)
tm_shape(bogota) + 
  tm_fill("casos",
          palette = "Reds", 
          style = "quantile", 
          title = "% with a Qualification") +
  tm_borders(alpha=.4)  

# 1) mapa coropletico -----------------------------------------------------

bogota1<-bogota%>%
  mutate(casos_r=cut(casos,breaks = 
                   quantile(casos,probs = c(seq(0,1,0.1))),
                 include.lowest = T))
quantile(bogota1$casos,probs = c(seq(0,1,0.1)))
levels(bogota1$casos_r)<-c("0-0.1", "0.1-0.2" ,"0.2-0.3", "0.3-0.4", "0.4-0.5", "0.5-0.6," ,"0.6-0.7", "0.7-0.8", "0.8-0.9","0.9-1" )


mapview(bogota1,zcol = c("casos_r"),col.regions = brewer.pal(11, "Purples"))

library(tmap)
tm_shape(bogota) + tm_polygons(style="quantile", col = "casos") +
  tm_legend(outside = TRUE, text.size = .8) 

# 2) modelo de regresión --------------------------------------------------

cor(covid19[,c(2:19)])
modelo1<-lm(covid19$casos~covid19$Poblacion+covid19$Salud+covid19$T_PM+covid19$Ingresos_P)
summary(modelo1)

modelo2<-lm(covid19$casos~covid19$Salud+covid19$Ingresos_P)
summary(modelo2)
# interpretación de los parametros del modelo -----------------------------

#bo=Se espera que cuando el indice de pobreza multidimensional y el ingreso promedio por localidad
#    sean constantes se presentaran 0.003 nuevos casos de coronavirus

#b2=Por cada caso nuevo de covid-19 se puede esperar que el indice de pobreza
#    multidimensional en salud decrezca en 0.014 
#b4=Por cada caso nuevo de covid-19 se puede esperar que el ingreso promedio por 
#    localidad decrezca en 0.019 

# 3) matrices -------------------------------------------------------------

# matriz binaria ----------------------------------------------------------

# Asigna un 1 cuando un poligono encuentra un vecino,y cero cuando no lo encuentra

#Matriz binaria efecto reina 
l1=poly2nb(bogota,row.names=bogota$LocNombre, queen=TRUE);l1 
w1=nb2listw(l1, style='B'); w1 #crea la matriz de vecindad
colnames(w1)=bogota$LocNombre;w1
w1["KENNEDY",] #  barrios limitan con suba 
sum(w1[,"KENNEDY"])

#Matriz binaria sin efecto reina (torre)
l2=poly2nb(bogota,row.names=bogota$LocNombre, queen=FALSE);l2 
w2=nb2listw(l2, style='B'); w2 
colnames(w2)=bogota$LocNombre;w2
w2[,"SUBA"] #  barrios limitan con suba 
sum(w2[,"SUBA"])

# matriz estandarizada por filas ------------------------------------------
#cada suma de filas en la matriz se hace igual a uno,asi, los valores individuales Wij
# están proporcionalmente representados es decir como la suma de la fila es 1 se divide 1 en el número 
# de vecinos que se encontro por fila

# matriz estandarizada por filas efecto reina 
w3=nb2mat(l1, style='W') 
rowSums(w3)
colnames(w3)=bogota$LocNombre;w3
w3[,"KENNEDY"] #  barrios limitan con kennedy 

# matriz estandarizada por filas efecto torre

w4=nb2mat(l2, style='W') 
rowSums(w4)
colnames(w4)=bogota$LocNombre;w4
w4[,"KENNEDY"] #  barrios limitan con suba 


# estandarizada global ----------------------------------------------------

# La suma de esta matriz es 20 ya que a cada vecino tanto por fila como por columna se le da 
# una ponderación tanto por fila como por columna

# estadarización global (C) efecto reina
w5=nb2mat(l1, style='C')
rowSums(w5) 
colnames(w5)=bogota$LocNombre;w5
w5[,"KENNEDY"] #  barrios limitan con suba 
sum(w5)


# estadarización global (C) efecto torre
w6=nb2mat(l2, style='C')
rowSums(w6) 
colnames(w6)=bogota$LocNombre;w5
w6[,"KENNEDY"] #  barrios limitan con suba 
sum(w6)


# esquema de codificación estabilizadora de varianza ----------------------


# esquema de cofificación estabilizadora de varianza (S) efecto reina
w7=nb2mat(l1, style='S')
rowSums(w7) 
colnames(w7)=bogota$LocNombre;w5
w7[,"KENNEDY"] #  barrios limitan con suba 
sum(w7)

# esquema de cofificación estabilizadora de varianza (S) (C) efecto torre
w8=nb2mat(l2, style='S')
rowSums(w8) 
colnames(w8)=bogota$LocNombre;w5
w8[,"KENNEDY"] #  barrios limitan con suba 
sum(w8)


# 4) ----------------------------------------------------------------------

zij<-residuals(modelo2)
m1<-data.frame(Moran.I(zij,w1))
m2<-data.frame(Moran.I(zij,w2))
m3<-data.frame(Moran.I(zij,w3))
m4<-data.frame(Moran.I(zij,w4))
m5<-data.frame(Moran.I(zij,w5))
m6<-data.frame(Moran.I(zij,w6))
m7<-data.frame(Moran.I(zij,w7))
m8<-data.frame(Moran.I(zij,w8))
rbind(m1,m2,m3,m4,m5,m6,m7,m8)


moran<-function(zij,w1){
n<-length(zij)
ROWSUM <- rowSums(w1)
w1 <- w1/ROWSUM
s <- sum(w1)
m <- mean(zij)
y <- zij - m
cv <- sum(w1 * y %o% y)
v <- sum(y^2)
obs <- (n/s) * (cv/v)
print(obs)
}
moran(zij,w1);moran(zij,w2);moran(zij,w3);moran(zij,w4);moran(zij,w5);moran(zij,w6);moran(zij,w7);moran(zij,w8)

#parece ser que las matrices de vecindad calculadas por el método torre dan indicios una
# correlación mayor que con el metodo de reina

#El indice de morat muestra que existe una correlación espacial negativa entre los casos de covid
# y el indice de pobreza en salud y el ingreso promedio es decir que la distribución de los casos de coronavirus 
# por localidad se parece a sus vecinos mas lejanos.
# es decir los valores de las areas cercanas serán distintos contraposici´on con la Ley de Tobler.

# 5) ----------------------------------------------------------------------

geary<-function(zij,w1){
n<-length(zij)-1
s<-2*sum(w1)
m <- mean(zij)
y <- zij 
cv <- sum(w1 * (y %o% y)^2)
v<-sum((y-m)^2)
geary<-(n/s)*(cv/v)
print(geary)}

geary(zij,w8)


# un valor
# del estad´istico c que tiende a 0 (c < 1) nos indica una autocorrelaci´on positiva
# mientras que un valor que tiende a 2 (c > 1) nos estar´ia indicando una autocorrelaci´on negativa. Un valor cercano a 1 nos estar´a indicando que hay ausencia de
# autocorrelaci´on espacial en nuestros datos



# PARCIAL SEGUNDA PARTE ---------------------------------------------------


# 6) MODELOS --------------------------------------------------------------

mn<-lm(covid19$casos~covid19$Salud+covid19$Ingresos_P)
summary(mn)
AIC(mn)

# se verifica supuesto de normalidad

# Ho: los datos son normales
# Ha: los datos no se distribuyen de forma normal
r=mn$residuals
shapiro.test(r)

# se calcula la correlación a los residuales
l1=poly2nb(bogota,row.names=bogota$LocNombre, queen=TRUE);l1 
w1=nb2listw(l1, style='B')
mr1=moran(r,w1,n=length(bogota),S0 = Szero(w1))
gr1=geary(r,w1,n=length(bogota),n1 = length(bogota)-1,S0 = Szero(w1) )
i=c(mr1$I,gr1$C)

# Ho: I=0 
# Ha: I>0 Existe correlación espacial

moran.test(r,w1)
 
# no existe autocorrelación espacial

# Modelo de regresión espacial (SEM) --------------------------------------

mle =errorsarlm(mn,data=bogota,w1)
(sem<-summary(mle))

# Lambda= -0.24217 los errores en la ubicación i de los casos de coronavirus se parecen a los errores en la ubicación i de los vecinos mas lejanos

# Encuentro correlación espacial con los vecinos sale significativa al igual que salud y el ingreso promedio

shapiro.test(mle$residuals) #No se distribuye normal
moran.test(mle$residuals,w1) #No existe autocorrelación espacial en los residuales

# IM = -0.08
# H0: I=0 
# HA: I>0
# Valor p=0.5898, no se rechaza H0
# Ye= B0 + BiXi +lambda * lag.residuals + ei con i=1,...n
# Casos= 0.00026 - 0.03272*Salud - 0.00396*Ingreso -0.24217*lag.residuals + ei

# Modelo de rezago espacial (SARS) ----------------------------------------

# La dependencia espacial se encuentra e la variable dependiente

ms<-lagsarlm(mn,data=bogota,w1)
(sars<-summary(ms))

# rho=-0.16 Existe depencia espacial negativa, es decir que la distribución
# espacial de los casos de coronavirus se parece a sus vecinos mas lejanos

shapiro.test(ms$residuals) #No se distribuye normal
moran.test(ms$residuals,w1) #No existe autocorrelación espacial en los residuales

# IM = -0.2167
# H0: I=0 
# HA: I>0
# Valor p=0.89, no se rechaza H0
# Ye= B0 + BiXi +rho * lag.Yi + e con i=1,...n
# Casos= 0.00029 - 0.03476*Salud - 0.00396*Ingreso -0.16773*lag.casos + e

# Modelo espacial de Durbin -----------------------------------------------

md =errorsarlm(mn,data=bogota,w1,Durbin = T,tol.solve = 1e-30)
(med<-summary(md))

# existe autocorrelación espacial negativa es decir que la distribución de los casos de coronavirus s
# se parece a sus vecinos mas lejanos sin embargo se rechaza la hipotesis nula de depencia espacial

shapiro.test(md$residuals) #No se distribuye normal
moran.test(md$residuals,w1) #No existe autocorrelación espacial en los residuales

# IM = -0.178
# H0: I=0 
# HA: I>0
# Valor p=0.8292, no se rechaza H0
# Ye= B0 + BiXi +lambda * lag.yi + theta *lag.xi + ei con i=1,...n
# Casos=0.00029 - 0.0354*Salud - 0.00307*Ingresos - 0.2972*lag.casos - 0.0019*(0.246*lag.salud + 0.000022*lag.Ingresos) + ei


# Modelo mixto autorregresivo espacial con errores espaciales auto --------

mx=sacsarlm(mn,data=bogota,w1,type = "sac",tol.solve = 1e-30)
summary(mx)

shapiro.test(mx$residuals) # Los datos se distribuyen de forma normal
moran.test(mx$residuals,w1) #No existe autocorrelación espacial en los residuales

# IM = -0.088
# H0: I=0 
# HA: I>0
# Valor p=0.6074, no se rechaza H0
# Ye= B0 + BiXi +rho * lag.residuals*lambda + ei con i=1,...n
# Casos= 0.00030 - 0.0362*Salud - 0.0040*Ingreso -0.126*lag.residuals*-0.209 + ei

# Modelo de error de Durbin Espacial --------------------------------------

mede <- errorsarlm(mn, data = bogota, w1, etype = "emixed",tol.solve = 1e-30)
summary(mede)

shapiro.test(mede$residuals) #No se distribuye normal
moran.test(mede$residuals,w1) #No existe autocorrelación espacial en los residuales

# IM = -0.17
# H0: I=0 
# HA: I>0
# Valor p=0.82, no se rechaza H0
# Ye= B0 + BiXi +lambda * lag.residuals + theta*lag.Xi +e  con i=1,...n
# Casos= 0.00029 - 0.0354*Salud - 0.00307*Ingreso -0.2972*lag.residuals -0.0019*(0.2464*Salud + 0.000022*Ingreso) + ei

# 7) Análisis de los párametros -------------------------------------------


# 8) Dependencia espacial residuales --------------------------------------


# 9) Selección del mejor modelo -------------------------------------------
AIC<-c(AIC(mn),AIC(mle),AIC(ms),AIC(md),AIC(mx),AIC(mede))
modelos<-c("OLS","SEM","SAR","DM","SARAR","SDEM")
mm<-data.frame(AIC,modelos)

#El mejor modelo es un SAR
# Analisis Lisa -----------------------------------------------------------

local <- localmoran(x = bogota$casos, w1)
moran.map <- cbind(bogota, local)

tm_shape(moran.map) +
  tm_fill(col = "Ii",
          style = "quantile",
          title = "local moran statistic") 

quadrant <- vector(mode="numeric",length=nrow(local))

# centers the variable of interest around its mean
m.qualification <- bogota$casos - mean(bogota$casos)     

# centers the local Moran's around the mean
m.local <- local[,1] - mean(local[,1])    

# significance threshold
signif <- 0.1 

# builds a data quadrant
quadrant[m.qualification >0 & m.local>0] <- 4  
quadrant[m.qualification <0 & m.local<0] <- 1      
quadrant[m.qualification <0 & m.local>0] <- 2
quadrant[m.qualification >0 & m.local<0] <- 3
quadrant[local[,5]>signif] <- 0   

# plot in r
brks <- c(0,1,2,3,4)
colors <- c("white","blue",rgb(0,0,1,alpha=0.4),rgb(1,0,0,alpha=0.4),"red")
plot(bo3$geometry,border="lightgray",col=colors[findInterval(quadrant,brks,all.inside=FALSE)])
box()
legend("bottomleft", legend = c("insignificant","low-low","low-high","high-low","high-high"),
       fill=colors,bty="n")


# Getis-Ord approach ------------------------------------------------------

local_g <- localG(bogota$casos, w2)
local_g <- cbind(bogota, as.matrix(local_g))
names(local_g)[6] <- "gstat"
tm_sha pe(local_g) + 
  tm_fill("gstat", 
          palette = "RdBu",
          style = "pretty") +
  tm_borders(alpha=.4)
mapview(local_g,zcol = c("gstat"),col.regions = brewer.pal(11, "Reds"))

