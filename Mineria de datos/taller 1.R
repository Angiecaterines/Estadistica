
# EJERCICIO 1 slider 6 -------------------------------------------------------------
library(tidyverse) 
library(finalfit) # Ajuste y visualización de modelos
library(Amelia) # Métodos de imputación de NA's
library(DataExplorer) # Visualización de datos
library(mice) # Imputación múltiple de NA's
library(VIM) # Incluye función para imputación por kNN

datos<-ggplot2::diamonds%>%select(cut,carat,price)

# Aleatoriamente se crea un conjunto de valores fila/columna para borrar

col<-sample(2:3,15000,replace=T)
row<-sample(nrow(datos),15000,replace=T)
missing<-data.frame(row,col)

#################################################################################

# Asignar NA's a la celdas seleccionadas. El conjunto de datos con valores perdidos es dat1
dat1<-datos
for(i in 1:nrow(missing)) 
	dat1[missing[i,1],missing[i,2]]<-NA


# Se agrega la variable color a dat1
dat1$colorMAR<-diamonds$color

# NA´s en colorMAR condicionada a "Very Good" en cut

dat1$colorMAR[dat1$cut=="Very Good"]<- 
	sample(c("D", "E", "F", "G", "H", "I", "J", NA), 
	       sum(dat1$cut=="Very Good"), 
	       replace = TRUE,
	       prob = c(rep(0.1,7),0.3))

# NA´s en colorMAR condicionada a "Premium" en cut

dat1$colorMAR[dat1$cut=="Premium"]<- 
	sample(c("D", "E", "F", "G", "H", "I", "J", NA), 
	       sum(dat1$cut=="Premium"), 
	       replace = TRUE,
	       prob = c(rep(0.1,7),0.3))


# A) ----------------------------------------------------------------------
# Utilice las funciones complete.cases() y filter() para seleccionar y 
# almacenar en un nuevo data frame newdata2 sólo las observaciones completas 
# de dat1, y en un data frame newdata2 las observaciones incompletas.
# . -----------------------------------------------------------------------


#observaciones completas del data set 
newdata2<-dat1%>%filter(complete.cases(dat1)==T)

#observaciones incompletas del data set 
newdata2i<-dat1%>%filter(complete.cases(dat1)==F)


# B) ----------------------------------------------------------------------
# Realice la misma tarea del literal anterior, pero esta vez utilice la 
# función na.omit() de stats.
# . -----------------------------------------------------------------------

newdata2<-na.omit(dat1)


# C) ----------------------------------------------------------------------
# Cree una función que remueva primero del conjunto de datos dat1 la(s) 
# variable(s) que tenga(n) más del 14% de valores faltantes, y luego 
# remueva las observaciones con más de un 70% de NA’s. Verifique el tamaño
# del conjunto de datos resultante después de la remoción.

# . -----------------------------------------------------------------------


dat11<-dat1
remo<-function(x){
a<-apply(dat11,2,function(x)sum(is.na(x))/dim(dat11)[1])*100
b<-as.vector(which(a>14))
ifelse(a>14,dat11<-dat11[,-c(b)],dat11<-dat11)
cc<-apply(dat11,1,function(x)sum(is.na(x))/dim(dat11)[2])*100
dd<-as.numeric(which(cc>70))
ifelse(dd>70,dat1<-dat11[-c(dd),],dat11<-datt1)
print(dat11)
print(dim(dat11))
}
remo(dat1)


# D) ----------------------------------------------------------------------
# Cree una función que remueva primero del conjunto de datos dat1 las 
# observaciones con más de un 50% de NA’, y luego la(s) variable(s) que 
# tenga(n) más del 14% de valores faltantes. Verifique el tamaño del conjunto 
# de datos resultante después de la remoción.

# . -----------------------------------------------------------------------


remo2<-function(x){
cc<-apply(dat11,1,function(x)sum(is.na(x))/dim(dat11)[2])*100
dd<-as.numeric(which(cc>50))
ifelse(dd>50,dat11<-dat11[-c(dd),],dat11<-dat11)
a<-apply(dat11,2,function(x)sum(is.na(x))/dim(dat11)[1])*100
b<-as.vector(which(a>14))
ifelse(a>14,dat11<-dat11[,-c(b)],dat11<-dat11)
print(dat11)
print(dim(dat11))}

remo2(dat1)


# Ejercicio 1 slider 7 -------------------------------------------------------------

# Selección de variable numéricas a imputar
df.imp<-dat1%>%
	dplyr::select(carat,price)
profile_missing(df.imp)

# Imputando por la media para carat
df.imp$carat_imp<-df.imp$carat
df.imp[is.na(df.imp$carat_imp),"carat_imp"] <- mean(dat1$carat, na.rm=T)

# Imputando por la mediana para price
df.imp$price_imp<-df.imp$price
df.imp[is.na(df.imp$price_imp),"price_imp"] <- median(dat1$price, na.rm=T)


# A) ----------------------------------------------------------------------
# Construya una tabla de resumen con las variables de df.imp comparándolas 
# en términos de los estadísticos: media, mediana, varianza y cuartiles

# . -----------------------------------------------------------------------



carat<-c(summary(df.imp$carat_imp)[-c(1,6)],Varianza=var(df.imp$carat_imp))
price<-c(summary(df.imp$price_imp)[-c(1,6)],Varianza=var(df.imp$price_imp))
data.frame(carat,price)


# B) ----------------------------------------------------------------------
# ¿Qué criterio tendría presente para asegurar o no que las imputaciones 
# fueron aceptables?

# . -----------------------------------------------------------------------



# Una prueba de bondad de ajuste de la distribución para verificar si los datos
# imputados se distribuyen aproximadamente similar a la distribución teorica de la 
# ditribución.


# C) ----------------------------------------------------------------------
# Utilice la moda para imputar los valores faltantes en la variable colorMAR 
# del conjunto dat1. Compare gráficamente las variables con NA’s y las 
# variables imputadas ¿qué puede observar?

# . -----------------------------------------------------------------------


a<-dat1%>%dplyr::select(colorMAR)
profile_missing(a)

# Imputando por la media para carat
a$colorMAR_i<-a$colorMAR
b<-mlv(na.omit(dat1$colorMAR))
Moda <- function(x) {
	ux <- levels(x)
	ux[which.max(tabulate(match(x, ux)))]
}
a[is.na(a$colorMAR),"colorMAR_i"] <- Moda(dat1$colorMAR)

a%>%DataExplorer::plot_bar()

#Al imputar por la moda el sesgo de la distribución aumenta, al utilizar este  
# metodo se haria un mal procedimiento ya que los resultados obtenidos
# serian ironeos.

# Ejercicio 2 slider 7 -------------------------------------------------------------

# Utilice la imputación simple por muestreo aleatorio para imputar la variable 
# price del conjunto dat1. Compare las distribuciones, gráficamente y a través de 
# medidas descriptivas

# . -----------------------------------------------------------------------

rand.imput <-function(x){
	missing <- (is.na(x)) #vector booleano
	n.missing <- sum(missing)#Numero de NA’s
	x.obs <- x[!missing]#Datos no NA
	imputed <- x
	imputed[missing] <- sample(x.obs,n.missing,replace = T)
	#Se extrae una muestra aleatoria conocida y se remplazan estos en los NA
	return(imputed)}

rand.imput(dat1$price)
impu<-data.frame(precio_real=dat1$price,precio_imputado=rand.imput(dat1$price))
impu%>%DataExplorer::plot_histogram()

# Al realizar una imputación por muestreo aleatorio simple se puede observar que 
# la distribución empirica de los datos es igual a la distribucióin teorica.

# precio<-c(summary(na.omit(dat1$price))[-c(1,6)],Varianza=var(na.omit(dat1$price)))
# imputado<-c(summary(rand.imput(dat1$price))[-c(1,6)],Varianza=var(rand.imput(dat1$price)))
# data.frame(precio,imputado)
# 
# ff<-data.frame(datos=c(dat1$price,rand.imput(dat1$price)),tipo=c(rep("precio real",53940),rep("precio imputado",53940)))
# ggplot(ff,aes(x=datos),colour=tipo)+geom_bar()

# Ejercicio 3 slider 7 -------------------------------------------------------------

# Imputación por la media (imputación no condicional por la media) de la variable carat
# Imputación por muestreo simple de la variable price
imputasimple<-mice::mice(data =df.imp[,1:2],method = c("mean","sample"),m = 1,maxit=1)

# Variables imputadas
v.imputadas<-mice::complete(data =imputasimple)

indicadora<-as.data.frame(imputasimple$where)

union0<-dplyr::bind_cols(v.imputadas,indicadora)
names(union0)<-c("carat.mean","price.sample","carat.ind","price.ind")

# Valores imputados de carat por la media
imputados_mean<-union0[union0$carat.ind=="TRUE","carat.mean"]

# Valores imputados de price por muestreo aleatorio
imputados_sample<-union0[union0$price.ind=="TRUE","price.sample"]
 

# A) ----------------------------------------------------------------------
# Desarrolle un test de bondad de ajuste de Kolmorogov-Smirnov entre la 
# variable carat y la variable imputada imputados_mean ¿Qué significado tiene
# el resultado de este test?

# . -----------------------------------------------------------------------



## se utiliza el test kolmogorov smirnov para determinar si la distribución de los
# datos ha cambiado al realizar las imputaciones.

#ho:la distribución teorica = la distribución empirica
#ha:la distribución teorica != la distribución empirica

ks.test(x=dat1$carat,y=imputados_mean)

#Con un nivel de significancia del 0.05 rechazo ho es decir,
#la disribución de los quilates ha variado al utilizar la imputación simple de  la media


# B) ----------------------------------------------------------------------
# Desarrolle un test de bondad de ajuste de Kolmorogov-Smirnov entre la 
# variable price y la variable imputada imputados_sample ¿Qué significado 
# tiene el resultado de este test?

# . -----------------------------------------------------------------------



#ho:la distribución teorica = la distribución empirica
#ha:la distribución teorica != la distribución empirica

ks.test(x=dat1$price,y=imputados_sample)
#Con un nivel de significancia del 0.05 no rechazo  ho es decir,

# la distribución del precio de los diamantes 
#no ha cambiado al realizar la imputación de los data missing con 
#la imputación por muestreo.
