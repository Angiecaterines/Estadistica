library(readr)
library(tidyverse) 
library(finalfit) 
library(Amelia) 
library(DataExplorer) 
library(mice)
library(VIM)
## Se carga el conjunto de datos ventas_casas.
ventas_casas <- read_delim("C:/Users/Hp/Downloads/ventas_casas.csv", 
			   ";", escape_double = FALSE, trim_ws = TRUE)

# Cambio de  las etiquetas del conjunto de datos,para facilitar su manejo.

names(ventas_casas)<- c("tipo","area.total","año.constru","habitaciones","baños","precio.venta")

#Se convierte la variable tipo a factor y are.total a numerico.

ventas_casas<- ventas_casas%>%mutate(tipo = as.factor(tipo),area.total = as.numeric(area.total))

# Estructura de las variables del conjunto de datos.

glimpse(ventas_casas)

#dimensiones dek conjunto de datos
n<-dim(ventas_casas)


# tarea 1 -----------------------------------------------------------------

# Vector aa contiene las observaciones del conjunto de datos sin NA's
aa<-na.omit(ventas_casas)[-1]

# Se utiliza la función apply para realizar un resumen de cada variable numerica.
round(apply(aa ,2, function(x) c(Minimo=min(x),Q=quantile(x,probs = 0.25),Mediana=median(x),Media=mean(x),Q=quantile(x,probs = 0.75),Maxímo=max(x),Desviacion=sd(x))),2)

# Se realiza un resumen de la distribución de la variable categorica y su distribución.
ventas_casas%>%group_by(tipo)%>%summarise(Total=n(),Porcentaje=(Total/n[1])*100)


# tarea 2  ----------------------------------------------------------------
# función na.numer arroja el numero de NA's en el data set
na.number<- function(x){sum(is.na(x))}
na.number(ventas_casas)

# Porcentaje de NA's con respecto al total de valores
(na.number(ventas_casas)/(n[1]*n[2]))*100
cbind("Número de NA's"=na.number(ventas_casas),"Porcentaje de NA's"=(na.number(ventas_casas)/(n[1]*n[2]))*100)

# Grafico de la distribución de NA´s en el data set.
ventas_casas%>%missing_plot()

# tarea 3 -----------------------------------------------------------------

# El vector a contiene el numero de NA's por cada variable.
a<-apply(ventas_casas, 2, function(x)sum(is.na(x)))

# El vector b contiene el porcentaje de NA's por variable.
b<-round((apply(ventas_casas, 2, function(x)sum(is.na(x)))/dim(ventas_casas)[1])*100,2)

# data.frame con cantidad y porcentaje de NA's por variable.
cbind("Cantidad de NA'S"=a,"Porcentaje de NA's"=b)

# Grafico de resumen que muestra el porcentaje de valores NA's por variable.
DataExplorer::plot_missing(data=ventas_casas)

 # tarea 4 -----------------------------------------------------------------
# El vector cc contiene el numero de observaciones con mas de 1 NA's.
cc<-sum(apply(ventas_casas, 1, function(x)sum(is.na(x)))>1)

# El data.frame contiene el numero y porcentaje de observaiones con mas de 1 NA 
data.frame("Observaciones NA's"=cc,"Porcentaje NA's"=(cc/dim(ventas_casas)[1])*100)


# tarea 5) ----------------------------------------------------------------
#El vector dd contiene el numero de valores NA's por cada observaión.
dd<-apply(ventas_casas, 1, function(x)sum(is.na(x)))

# Eltibble gg contiene el total y porcentaje de observaciones con 1,2,3   NA's 
(gg<-ventas_casas%>%mutate(NAS=dd)%>%group_by(NAS)%>%summarise(total=n(),porcentaje=(total/2930)*100))

#El data.frame se construye para poner graficar

gg<-data.frame(Valores=round(c(gg$total,gg$porcentaje),2),NAS=rep(c(0,1,2,3),2),Tipo=c(rep("total",4),rep("porcentaje",4)))

# Grafico resumen 
ggplot(gg,aes(x=NAS,y=valores,fill=tipo))+
	geom_bar(stat='identity',
		 position = "Dodge")+ 
	coord_flip()+
	theme_light()


# tarea 6 -----------------------------------------------------------------




# Tare 7 ------------------------------------------------------------------



# a) ----------------------------------------------------------------------
rand.imput <-function(x){
	missing <- (is.na(x)) #vector booleano
	n.missing <- sum(missing)#Numero de NA’s
	x.obs <- x[!missing]#Datos no NA
	imputed <- x
	imputed[missing] <- sample(x.obs,n.missing,replace = T)
	#Se extrae una muestra aleatoria conocida y se remplazan estos en los NA
	return(imputed)}

#data frame con los valores imputados por muestro aleatorio simple

ventas_casa1<-data.frame(tipo=rand.imput(ventas_casas$tipo),
			 area.total=rand.imput(ventas_casas$area.total),
			 año.constru=rand.imput(ventas_casas$año.constru),
			 habitaciones=rand.imput(ventas_casas$habitaciones),
			 baño=rand.imput(ventas_casas$baños))

# b) ----------------------------------------------------------------------

t <- proc.time() # Inicia el cronómetro
# Imputacion de las variables tipo,are,año,habitaciones,baños usando el metodo knn con 5 vecinos
imputacionKnn<-VIM::kNN(data=ventas_casas,
			variable = c("tipo","area.total","año.constru","habitaciones",
				     "baños"),
			k=5,numFun=mean,catFun=maxCat)
ejecucion<-proc.time()-t # tiempo de ejecución

a<-imputacionKnn[7:11]
apply(a, 2, function(x)sum(x))

#Prueba chi cuadrado para verificar ajuste de los datos imputados con los verdaderos
chisq.test(x=ventas_casas$tipo,y=imputacionKnn$tipo)

# Prueba kolmogorov para verificar bondad de ajuste en cada una de las variables imputadas.

ks.test(x=ventas_casas$area.total,y=imputacionKnn$area.total)
ks.test(x=ventas_casas$año.constru,y=imputacionKnn$año.constru)
ks.test(x=ventas_casas$habitaciones,y=imputacionKnn$habitaciones)
ks.test(x=ventas_casas$baños,y=imputacionKnn$baños)


# c) ----------------------------------------------------------------------

# Imputación de las variables con NA´s por el metodo mice
multimp.mice<-mice::mice(ventas_casas,m = 5)

# Conjunto de datos imputados 

imput.mice<-complete(multimp.mice)

chisq.test(x=ventas_casas$tipo,y=imput.mice$tipo)
ks.test(x=ventas_casas$area.total,y=imput.mice$area.total)
ks.test(x=ventas_casas$año.constru,y=imput.mice$año.constru)
ks.test(x=ventas_casas$habitaciones,y=imput.mice$habitaciones)
ks.test(x=ventas_casas$baños,y=imput.mice$baños)


# tarea 8 -----------------------------------------------------------------

# ajuste del modelo 
# Train contiene el conjunto de variables a predecir

train<-imput.mice%>%mutate(precio.venta=factor(precio.venta))

# Se ajusta el modelo lineal multiple 
modelo<-lm(formula = precio.venta~as.factor(tipo)+area.total+año.constru+
	   	habitaciones+baños,data=train)

# Coeficientes del modelo ajustado.

round(coef(modelo),2)


# tarea 9 -----------------------------------------------------------------

# data frame con variables a predecir 

new<-data.frame(tipo=as.factor(c("apartamento","apartamento","casa","Duplex")),
		area.total=c(12567,45250,100225,8066),año.constru=c(1965,2010,1905,1942),
		habitaciones=c(2,2,1,4),baños=c(1,2,1,2))

# Vector con las predicciones

prediccion<-predict(object = modelo,newdata = new)
cbind(new,prediccion)
