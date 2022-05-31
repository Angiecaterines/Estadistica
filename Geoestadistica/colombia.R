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

# Shape file de colombia por departamentos y casos de coronavirus
# https://datosabiertos.esri.co/datasets/colombia-covid19-coronavirus-municipio/data?showData=true

ctes <- st_read("C:/Users/Hp/Downloads/mpio")
ctes1 <- st_read("C:/Users/Hp/Downloads/muni")%>% rename("Codigo municipio"= MPIO_CCNCT,"Departamento"=DPTO_CNMBR,"Municipio"=NOMBRE_MPI)
casos<- read_csv("C:/Users/Hp/Downloads/Colombia_COVID19_Coronavirus_Municipio.csv") %>% rename("Mujeres"=ETAREO_F,"Hombres"=ETAREO_M,"Poblacion"=TOTAL_UNIDADES_PERSONAS_PROY_2020)
casos1<- read_csv("C:/Users/Hp/Downloads/Casos_positivos_de_COVID-19_en_Colombia (1).csv")
casos1<-casos1 %>% mutate(Sexo=toupper(casos1$Sexo))
Divipola<-read_excel("C:/Users/Hp/Downloads/DIVIPOLA.Municipios.xlsx")
# Analisis Exploratorio --------------------------------------------------


# EDAD PROMEDIO POR MUNICIPIO ---------------------------------------------

casos1 %>% group_by(`Nombre municipio`) %>% summarise(Promedio=mean(Edad),Desviación=sd(Edad))


# CASOS DE COVID POR MUNICIPIO --------------------------------------------

casos1 %>% group_by(`Nombre municipio`) %>% summarise(total_casos=n())


# CASOS DE COVID POR SEXO -------------------------------------------------

casos1 %>% group_by(`Nombre municipio`,Sexo) %>% summarise(total=n())


# CASOS DE COVID POR ESTADO -----------------------------------------------

# Mapa de casos de COVID-19 por municipios Colombianos --------------------

COLOMBIA<-cbind(ctes1[,5:7],casos[,c(9,12:15,21:23)]) %>% mutate(Densidad_km2=Poblacion/Area_km2)


# casos por millon de habitantes ------------------------------------------


Mcas <- COLOMBIA$Total_Confirmados/COLOMBIA$Poblacion*1000000 #caoss por millon de habitantes
Mprop<- (COLOMBIA$Total_Muertos/COLOMBIA$Total_Confirmados)*1000000
Mfal<- (COLOMBIA$Total_Muertos/COLOMBIA$Poblacion)*1000000
Mrec <- (COLOMBIA$Total_Recuperados/COLOMBIA$Poblacion)*1000000

COLOMBIA<-cbind(COLOMBIA,Mcas,Mprop,Mfal,Mrec)




# ANALISIS EXPLORATORIO CON LA BASE OBTENIDA ------------------------------

boxplot(as.vector(scale(COLOMBIA$Mujeres)),as.vector(scale(COLOMBIA$Hombres)),col ="green")
summary(COLOMBIA)


# LECTURA DEL SHIPEFILE ---------------------------------------------------



mapview(COLOMBIA,zcol = c("Total_Confirmados"),col.regions = brewer.pal(10, "PRGn"))



# calculo de matrix de distancias y dendograma ----------------------------

xy<-Divipola[,c(6,7)]
d<-as.matrix(round(dist(xy),0))
colnames(d)<-Divipola$Nombre...4
rownames(d)<-Divipola$Nombre...4
plot(hclust(dist(d)),cex=0.5)



# MAPAS COROPLETICOS ------------------------------------------------------

COLOMBIA1<-COLOMBIA%>%
  mutate(casos_r=cut(Total_Confirmados,breaks = 
                       quantile(Total_Confirmados,probs = c(seq(0,1,0.1))),
                     include.lowest = T))
quantile(COLOMBIA1$Total_Confirmados,probs = c(seq(0,1,0.1)))
levels(COLOMBIA1$casos_r)<-c("0-0.1", "0.1-0.2" ,"0.2-0.3", "0.3-0.4", "0.4-0.5", "0.5-0.6," ,"0.6-0.7", "0.7-0.8", "0.8-0.9","0.9-1" )


mapview(COLOMBIA1,zcol = c("casos_r"),col.regions = brewer.pal(11, "Purples"))

library(tmap)
tm_shape(COLOMBIA) + tm_polygons(style="quantile", col = "Total_Confirmados") +
  tm_legend(outside = TRUE, text.size = .8) 

# 2) modelo de regresión --------------------------------------------------

cor(COLOMBIA$Mujeres,COLOMBIA$Hombres)
A<-data.frame(COLOMBIA$Area_km2,COLOMBIA$Total_Confirmados,COLOMBIA$Mujeres,COLOMBIA$Hombres,COLOMBIA$Poblacion,COLOMBIA$Densidad_km2)
cor(A)

modelo<- lm(COLOMBIA$Total_Confirmados~COLOMBIA$Poblacion+COLOMBIA$Area_km2)
summary(modelo)


# interpretación de los parametros del modelo -----------------------------

#bo=Se espera que cuando la poblacion por municipio
#    sean constantes se presentaran --0.001523 nuevos casos de coronavirus

#b1=Por cada caso nuevo de covid-19 se puede esperar que la población
#   aumente en 8 habitantes 

#b4=Por cada caso nuevo de covid-19 se puede esperar que el el area habitable en km^2
#   decrezca en 5 habitantes

#Quiere decir esto que a mayor población mator contagio y que si el area habitable por km^2 
# es pequeña se aumentan los casos de covid-19

# 3) matrices -------------------------------------------------------------

# matriz binaria ----------------------------------------------------------

# Asigna un 1 cuando un poligono encuentra un vecino,y cero cuando no lo encuentra

#Matriz binaria efecto reina 
l1=poly2nb(COLOMBIA,row.names=COLOMBIA$Departamento, queen=TRUE);l1 
w1=nb2mat(l1, style='B',zero.policy=T); w1 #crea la matriz de vecindad
colnames(w1)=COLOMBIA$Departamento;w1
w1[,"AMAZONAS"]  
sum(w1[,"AMAZONAS"])
summary(w1)

#Matriz binaria sin efecto reina (torre)
l2=poly2nb(COLOMBIA,row.names=COLOMBIA$Departamento, queen=FALSE);l2 
w2=nb2mat(l2, style='B',zero.policy=T); w2 
colnames(w2)=COLOMBIA$Departamento;w2
w2[,"AMAZONAS"] 
sum(w2[,"AMAZONAS"])

# matriz estandarizada por filas ------------------------------------------
#cada suma de filas en la matriz se hace igual a uno,asi, los valores individuales Wij
# están proporcionalmente representados es decir como la suma de la fila es 1 se divide 1 en el número 
# de vecinos que se encontro por fila

# matriz estandarizada por filas efecto reina 
w3=nb2mat(l1, style='W',zero.policy=T) 
rowSums(w3)
colnames(w3)=COLOMBIA$Departamento;w3
sum(w3[,"AMAZONAS"]) #  barrios limitan con kennedy 

# matriz estandarizada por filas efecto torre

w4=nb2mat(l2, style='W',zero.policy=T) 
rowSums(w4)
colnames(w4)=COLOMBIA$Departamento;w4
sum(w4[,"AMAZONAS"]) #  barrios limitan con suba 
summary(w4)

# estandarizada global ----------------------------------------------------

# La suma de esta matriz es 20 ya que a cada vecino tanto por fila como por columna se le da 
# una ponderación tanto por fila como por columna

# estadarización global (C) efecto reina
w5=nb2mat(l1, style='C',zero.policy=T)
rowSums(w5) 
colnames(w5)=COLOMBIA$Departamento;w5
w5[,"AMAZONAS"] #  barrios limitan con suba 
sum(w5)


# estadarización global (C) efecto torre
w6=nb2mat(l2, style='C',zero.policy=T)
rowSums(w6) 
colnames(w6)=COLOMBIA$Departamento;w5
w6[,"AMAZONAS"] #  barrios limitan con suba 
sum(w6)


# esquema de codificación estabilizadora de varianza ----------------------


# esquema de cofificación estabilizadora de varianza (S) efecto reina
w7=nb2mat(l1, style='S',zero.policy=T)
rowSums(w7) 
colnames(w7)=COLOMBIA$Departamento;w5
w7[,"AMAZONAS"] #  barrios limitan con suba 
sum(w7)

# esquema de cofificación estabilizadora de varianza (S) (C) efecto torre
w8=nb2mat(l2, style='S',zero.policy=T)
rowSums(w8) 
colnames(w8)=COLOMBIA$Departamento;w5
w8[,"AMAZONAS"] #  barrios limitan con suba 
sum(w8)


# 4) ----------------------------------------------------------------------

zij<-residuals(modelo)
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



# pruebas de hipotesis moran ----------------------------------------------
moran.test(COLOMBIA$Poblacion, w5,randomisation=TRUE, alternative="two.sided", na.action=na.exclude)

# modelo Sars -------------------------------------------------------------

ms<-lagsarlm(modelo,data=COLOMBIA,w5,tol.solve=1e-30)

lag

# modelo durbin -----------------------------------------------------------

md<-lagsarlm(Poblacion,data=COLOMBIA,Durbin=T)



