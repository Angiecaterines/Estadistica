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
library(raster) # Permite hallar la matriz de vecidad 
library(spdep) # dependencia espacial
library(spData)
library(ape)



# Datos -------------------------------------------------------------------
# obtención de los datos,pasos importantes
# 1) Datos de coronavirus desagregados por municipios de Colombia
# 2) Shapefile de colombia desagregado por municipios de Colombia
# 3) Población real o proyectada de colombia desagregada por municipios

# La base de datos descargada de datos abiertos, ya se encuentra desagregada 
# por municipios al igual que cuenta con su respectiva poblaciónn

# https://datosabiertos.esri.co/datasets/colombia-covid19-coronavirus-municipio/data?showData=true


# casos de coronavirus por municipio y población poyectada para el 2020
casos<- read_csv("C:/Users/Hp/Downloads/Colombia_COVID19_Coronavirus_Municipio.csv") 

## Shapefile de Colombia por municipios
ctes <- st_read("C:/Users/Hp/Downloads/mpio")

# Codigos divipola, contiene el codigo correspondiente para cada munipio

Divipola<-read_excel("C:/Users/Hp/Downloads/DIVIPOLA.Municipios.xlsx")


# Limpieza y Corrección de las bases ----------------------------------------
casos<-casos%>% rename("MPIOS"=MPIO_CCNCT,"NOMBRE_MPI"=NOMBRE_MPIO,
                       "Mujeres"=ETAREO_F,"Hombres"=ETAREO_M,
                       "Poblacion_Proyectada"=TOTAL_UNIDADES_PERSONAS_PROY_2020)
casos<-casos %>% dplyr::select(MPIOS,NOMBRE_MPI,Total_Confirmados,Total_Existentes,Total_Muertos,Total_Recuperados,
                        Mujeres,Hombres,Poblacion_Proyectada)

# Construcción de variables etereas --------------------------------------------------


mcas=round(casos$Total_Confirmados/casos$Poblacion_Proyectada*1000000,0) # casos por millon de habitantes
#mprop=round(1000000*casos$Total_Muertos/casos$Total_Confirmados,0);mprop # proporción de muertos
mfal=round(1000000*casos$Total_Muertos/casos$Poblacion_Proyectada,0);mfal # fallecidos por millon 
mrec=round(1000000*casos$Total_Recuperados/casos$Poblacion_Proyectada,0);mrec # recueperados por millon

casos<-cbind(casos,mcas,mfal,mrec)

# Unión de las bases de datos ---------------------------------------------

base=merge(ctes,casos,by="MPIOS")
base=base %>% dplyr::select(DIVIPOLA=MPIOS,NOMBRE_MPI=NOMBRE_MPI.x,NOMBRE_DPT,
                            CASOS_PM=mcas)
# 1) mapa coropletico -----------------------------------------------------

bogota1<-base%>%
  mutate(CASOS_PMI=cut(CASOS_PM,breaks = 
                       quantile(CASOS_PM,probs = c(seq(0,1,0.1))),
                     include.lowest = T))
quantile(bogota1$CASOS_PM,probs = c(seq(0,1,0.1)))
levels(bogota1$CASOS_PM)<-c("0-0.1", "0.1-0.2" ,"0.2-0.3", "0.3-0.4", "0.4-0.5", "0.5-0.6," ,"0.6-0.7", "0.7-0.8", "0.8-0.9","0.9-1" )


mapview(bogota1,zcol = c("CASOS_PM"),col.regions = brewer.pal(11, "Purples"))

library(tmap)
tm_shape(bogota) + tm_polygons(style="quantile", col = "casos") +
  tm_legend(outside = TRUE, text.size = .8) 


# 3) matrices -------------------------------------------------------------

# matriz binaria ----------------------------------------------------------

# Asigna un 1 cuando un poligono encuentra un vecino,y cero cuando no lo encuentra
base=base %>% filter(base$NOMBRE_DPT=="AMAZONAS")
#Matriz binaria efecto reina 
l1=poly2nb(base,row.names=base$DIVIPOLA, queen=TRUE);l1 
w1=nb2listw(l1, style='B'); w1 #crea la matriz de vecindad
colnames(w1)=base$DIVIPOLA;w1


#Matriz binaria sin efecto reina (torre)
l2=poly2nb(base,row.names=base$DIVIPOLA, queen=FALSE);l2 
w2=nb2listw(l2, style='B'); w2 
colnames(w2)=base$DIVIPOLA; w2


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

