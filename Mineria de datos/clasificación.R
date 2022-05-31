library(tidyverse)
library(FNN)

# Ejercicios --------------------------------------------------------------

# Se carga la base toyota 1436 autos explicadps en 10 varibles
toyota<-read_delim("C:/Users/Hp/Downloads/toyota.csv", ";", escape_double = FALSE, trim_ws = TRUE,)
toyota<-toyota %>% mutate(Automatic=factor(Automatic))

# verificación de datos faltantes.

any(is.na(toyota))


# Tarea 1: contruya un modelo de regresión simple para predecir el precio (Price) 
# que debería asignarsele a tres nuevos autos cuyos años de uso (Age) corresponden
# respectivamente a 10, 18 y 40.

#conjunto de entreamiento price=respuest Age=explicativa

train<-toyota %>% 
	select(Age,Price)

# Exploración visual de la distribución de los datos para determinar un modelo candidato

ggplot(train,aes(x=Age,y=Price))+
	geom_point(size=2,col="cyan3")+
	labs(x="X: Años de uso automovil",
	     y="Y: Precio Automovil",
	     title="Diagrama de dispersión: conjunto de entrenamiento")+
	theme_light()

# Modelo de regresión lineal simple ajustado

modelo<-lm(Price~Age,data = toyota)

# Carros nuevos de los cuales se quiere conocer su precio

nuevo<-data.frame(Age=c(10,18,40))

# Prediccion del precio de los autos ajustados por un modelo de regresión lineal
# simple

prediction<-predict(modelo,newdata = train)

# Al conjunto de entrenamiento se añaden los precios predichos 

train<-train %>% bind_cols(Price_est=prediction)

#Predicción del precio de los nuevos autos
prdi_nuevos<-predict(object = modelo,newdata = nuevo)

# Tabla con las predicciones del precio para  cada automovil nuevo
tab1<-data.frame(Age=nuevo$Age,Price=prdi_nuevos)

# Visualización del modelo ajustado al conjunto de entrenamiento y 
# visualización sobre el plano del precio de los nuevos autos.

ggplot()+
	geom_point(data=train,aes(x=Age,y=Price),color="gray")+
	geom_line(data=train,aes(x=Age,y=Price_est),linetype=22,color="darkturquoise")+
	geom_point(data=tab1,aes(x=Age,y=Price),color="darkmagenta",size=3)+
	labs(x="X: Años de uso automovil",
	     y="Y: Precio Automovil",
	     title="Diagrama de dispersión: valores predichos precio nuevos")+
	theme_light()

# Tarea 2: construya un modelo de regresión con base en el algoritmo kNN con 
# k=10 vecinos más próximos, para predecir los precios de venta de los autos 
# cuyos perfiles se describen en la tarea 1.

#Conjunto de entreanamiento

train<-train %>% select(Age,Price)

#Estimación de los precios de los automoiles via knn con k=10 vecino

modelokNN<-FNN::knn.reg(train = train,y =train$Price,k = 10)

pre_price<-modelokNN$pred

# data frame con las variables explicativa y de respuesta añadiendo 
# los precios estimados por knn

train<-train %>% bind_cols(Pre_price_knn=pre_price)

# Autos nuevos de los que se desea conocer su precio

nuevo<-data.frame(Age=c(10,18,40))

# Predición del precio de los 3 autos nuevos mediante KNN y k=10
prdikNN_nuevos10<-knn.reg(train=train$Age,y = train$Price,test = nuevo,k=10)$pred
prdikNN_nuevos10

# Tabla con las estimaciones de precio para cada automovil
tab2<-data.frame(Age=nuevo$Age,Price=prdikNN_nuevos10)

#Visualización de las etimaciones del modelo ajustado con respecto a los valores observados

ggplot()+
	geom_point(data=train,aes(x=Age,y=Price),color="gray")+
	geom_line(data=train,aes(x=Age,y=Pre_price_knn),linetype=21,color="magenta")+
	geom_point(data=tab2,aes(x=Age,y=Price),color="darkturquoise",size=4)+
	labs(x="X: Años de uso automovil",
	     y="Y: Precio Automovil",
	     title="Diagrama de dispersión: valores predichos de precio nuevos según kNN")+
	theme_light()

# Tarea 3: nuevamente contruya un algoritmo kNN para predecir los precios de ventas
# de los autos descritos, pero esta vez tome k=20 vecinos más próximos.

#Conjunto de entreanamiento
train<-train %>% select(Age,Price)

#Estimación de los precios de los automoiles via knn con k=20 vecinos
modelokNN1<-FNN::knn.reg(train = train,y =train$Price,k = 20)

pre_price<-modelokNN1$pred

# data frame con las variables explicativa y de respuesta añadiendo 
# los precios estimados por knn

train<-train %>% bind_cols(Pre_price_knn20=pre_price)

# Autos nuevos de los que se desea conocer su precio
nuevo<-data.frame(Age=c(10,18,40))

# Predición del precio de los 3 autos nuevos mediante KNN y k=10
prdikNN_nuevos20<-knn.reg(train=train$Age,y = train$Price,test = nuevo,k=20)$pred
prdikNN_nuevos20

tab3<-data.frame(Age=nuevo$Age,Price=prdikNN_nuevos20)
#Visualización de las etimaciones del modelo ajustado con respecto a 
# los valores observados

ggplot()+
	geom_point(data=train,aes(x=Age,y=Price),color="gray")+
	geom_line(data=train,aes(x=Age,y=Pre_price_knn20),linetype=21,color="magenta")+
	geom_point(data=tab3,aes(x=Age,y=Price),color="darkturquoise",size=4)+
	labs(x="X: Años de uso automovil",
	     y="Y: Precio Automovil",
	     title="Diagrama de dispersión: valores predichos de precio nuevos según kNN")+
	theme_light()

# Tabla para nuevos pacientes
tab3<-data.frame(Age=nuevo$Age,Price=prdikNN_nuevos20)

ggplot()+
	geom_point(data=train,aes(x=Age,y=Price),color="gray")+
	geom_line(data=train,aes(x=Age,y=Pre_price_knn20),linetype=21,color="tomato")+
	geom_point(data=tab3,aes(x=Age,y=Price),color="darkturquoise",size=4)+
	labs(x="X: Años de uso automovil",
	     y="Y: Precio Automovil",
	     title="Diagrama de dispersión: valores predichos de precio nuevos según kNN")+
	theme_light()

# Tarea 4: construya un clasificador kNN con k=10 vecinos más próximos para etiquetar 
# como automáticos (si) o no automáticos (no) los siguientes carros no observados 
# dentro del conjunto de entrenamiento cuyos perfiles se describen a continuación:


#Conjunto de entreanamiento

train1<-toyota %>% select(KM,HP,Automatic)
# Conjunto de valores explicativos para realizar la clasificación

nuevo<-data.frame(kilometraje=c(15.000,52.300,165.600),caballos=c(44,100,120))

#isualización de la calsificaión de los datos y la estimación de tipo para los nuevos autos
ggplot(train1,aes(x=HP,y=KM,colour=Automatic))+
	geom_point(size=2)+
	geom_point(nuevo,mapping =  aes(x = caballos, y = kilometraje),col="blue",size=4)+
	labs(x="X1: Caballos de fuerza",
	     y="X2: Kilometraje",
	     title="Diagrama de dispersión con resultado de tipo de auto")+
	theme_light()


#clasificación de los autos por kilometraje y caballos de fuerza

result_pred<-FNN::knn(train = train1[,1:2],test = nuevo,cl = train1$Automatic,k = 10,prob=T)


# Tarea 5: nuevamente clasifique los autos descritos en la tarea 4 en automáticos/ no automáticos 
# a través de un kNN con k=10, pero sobre las variables previamente normalizadas kilometraje (KM) 
# y caballos de fuerza (HP).


train1<-toyota %>% select(KM,HP,Automatic) %>% mutate(KM=scale(KM),HP = scale(HP))

# Se construye un data frame con el perfil del nuevo paciente
nuevo<-scale(data.frame(kilometraje=c(15.000,52.300,165.600),caballos=c(44,100,120)))
nuevo<-as.data.frame(nuevo)

ggplot(train1,aes(x=HP,y=KM,colour=Automatic))+
	geom_point(size=2)+
	geom_point(nuevo,mapping =  aes(x = caballos, y = kilometraje),col="blue",size=4)+
	labs(x="X1: Caballos de fuerza",
	     y="X2: Kilometraje",
	     title="Diagrama de dispersión estandarizado con resultado de tipo de auto ")+
	theme_light()

result_pred<-FNN::knn(train = train1[,1:2],test = nuevo,cl = train1$Automatic,k = 10,prob=T)



give.n <- function(x) { # fun. cal. el prom. y tam. de mues.
	return(c(y = mean(x), label = length(x)))}

