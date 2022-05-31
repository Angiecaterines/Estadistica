library(tidyverse)
library(FNN)
library(DataExplorer)
library(plotly)


# Paso 1 ------------------------------------------------------------------




# Se carga la base toyota 1436 autos explicadps en 10 varibles
toyota<-read_delim("C:/Users/Hp/Downloads/toyota.csv", ";", escape_double = FALSE, trim_ws = TRUE,)

# Se selecciona únicamente las variables Price (Y) y KM (X1) Age(X2) Weight(X3)
datos<-toyota %>% select(KM,Age,Weight,Price) %>% mutate_all(as.double)

rriskDistributions::fit.cont(toyota$Price)

# Paso 2 ------------------------------------------------------------------


# Seleccionar siempre la misma partición
set.seed(12345)

# Muestra aleatoria del 75% de las filas del conjunto "datos" para el conjunto de entrenamiento
train.filas<-sample(x=row.names(datos),size = dim(datos)[1]*0.75)

# CONJUNTO DE ENTRENAMIENTO (selección de columnas)
train.set<-datos[train.filas,]
train1<-train.set %>% mutate_if(is.numeric,scale)
dim(train.set)


# CONJUNTO DE PRUEBA
test.filas<-setdiff(x = row.names(datos),train.filas)
test.set<-datos[test.filas,]
test.set1<- test.set %>% mutate_if(is.numeric,scale)
dim(test.set)



# Paso 3 ------------------------------------------------------------------


modelo1<-lm(Price~KM+Age+Weight,data=train.set)

modelo2<-lm(Price~KM+(Age^2)+(Weight^3),data=train.set)

modelo3<-FNN::knn.reg(train = train.set,y =train.set$Price,k = 10)

modelo4<-FNN::knn.reg(train =train1 ,y =train1$Price,k = 10)


plot(x=1:1436,y=toyota$Price)
a<-ggplot(train.set,aes(x=KM,y=Price))+
	geom_point(size=2,col="cyan3")+
	labs(x="X: Kilometraje del automovil",
	     y="Y: Precio Automovil")+
	theme_light()

b<-ggplot(train.set,aes(x=Age,y=Price))+
	geom_point(size=2,col="cyan3")+
	labs(x="X: Kilometraje del automovil",
	     y="Y: Precio Automovil")+
	theme_light()

c<-ggplot(train.set,aes(x=Weight,y=Price))+
	geom_point(size=2,col="cyan3")+
	labs(x="X: Kilometraje del automovil",
	     y="Y: Precio Automovil")+
	theme_light()
gridExtra::grid.arrange(a,b,c,ncol=3)

# Precios predichos según modelo 1
yhat_mod1<-predict(object = modelo1)

# Precios predichos según modelo 2
yhat_mod2<-predict(object = modelo2)

# Precios predichos según modelo 3
yhat_mod3<-modelo3$pred

# Precios predichos según modelo 4
yhat_mod4<-modelo4$pred


# Se almacenan junto al conjunto de entrenamiento
tabla1<-train.set %>% 
	mutate(Price_pred1=yhat_mod1,
	       Price_pred2=yhat_mod2,
	       Price_pred3=yhat_mod3,
	       Price_pred4=yhat_mod4)


# Paso 4 -----------------------------------------------------------------

# Se accede a las columnas de tabla1
attach(tabla1)

# MSE del modelo 1
MSE_m1<-mean((Price-Price_pred1)^2)

# MSE del modelo 2
MSE_m2<-mean((Price-Price_pred2)^2)

# MSE del modelo 3
MSE_m3<-mean((Price-Price_pred3)^2)

# MSE del modelo 4
MSE_m4<-mean((Price-Price_pred4)^2)

# MSE del modelo 5
#MSE_m5<-mean((Price-Price_pred5)^2)



# Tabla comparativa
tablaMSEE<-data.frame(MSE_m1,MSE_m2,MSE_m3,MSE_m4)
tablaMSEE%>%sort(decreasing = F)

##El modelo que mejor ajusto es el modelo de regresión por knn con k=10 ya 
# que presenta el menor cuadratico medio

# Paso 5) -----------------------------------------------------------------

# Evaluación y Precios predichos por el modelo 1 
yhato_mod1<-predict(modelo1,newdata = test.set)

# Evaluación y Precios predichos por el modelo 2
yhato_mod2<-predict(modelo2,newdata = test.set)

################################################################################

# Evaluación del modelo 3 kNN con k=10 vecino más próximo
eval3<-knn.reg(train = train.set, # Sólo predictoras de train.set
	       test = test.set,   # Sólo predictoras de test.set
	       y = train.set$Price,   # Sólo respuesta de train.set
	       k = 10)

# Precios predichos por el modelo 3 para autos del conjunto de prueba
yhato_mod3<-eval3$pred

################################################################################

# Evaluación del modelo 4 kNN con k=10 vecinos más próximos
eval4<-knn.reg(train = train1,test = test.set1,y = train1$Price ,k= 10)

# Precios predichos por el modelo 3 para autos del conjunto de prueba
yhato_mod4<-eval4$pred

# ################################################################################
# 
# # Evaluación del modelo 5 kNN con k=10 vecinos más próximos
# eval4<-knn.reg(train = train.set,test = test.set[,1],y = train.set$Price,k = 10)
# 
# # Precios predichos por el modelo 5 para autos del conjunto de prueba
# yhato_mod4<-eval4$pred


# Se almacenan junto al conjunto de entrenamiento
tabla2<-test.set %>% 
	mutate(Price_pred1=yhato_mod1,
	       Price_pred2=yhato_mod2,
	       Price_pred3=yhato_mod3,
	       Price_pred4=yhato_mod4)


# Paso 6) -----------------------------------------------------------------

# MSE de prueba del modelo 1
MSEP_m1<-mean((tabla2$Price-tabla2$Price_pred1)^2)

# MSE de prueba modelo 2
MSEP_m2<-mean((tabla2$Price-tabla2$Price_pred2)^2)

# MSE de prueba modelo 3
MSEP_m3<-mean((tabla2$Price-tabla2$Price_pred3)^2)

# MSE de prueba modelo 4
MSEP_m4<-mean((tabla2$Price-tabla2$Price_pred4)^2)

# MSE de prueba modelo 5
#MSEP_m5<-mean((tabla2$Price-tabla2$Price_pred5)^2)



# Tabla comparativa
tablaMSEP<-data.frame(MSEP_m1,MSEP_m2,MSEP_m3,MSEP_m4)
tablaMSEP%>%sort(decreasing = F)


# Paso 7) -----------------------------------------------------------------

tabla<-data.frame(dato=as.numeric(c(tablaMSEE,tablaMSEP)),
	Modelo=rep(c("Modelo1","Modelo2","Modelo3", "Modelo4"),2),
	Tipo=c(rep("MSEE",4),rep("MSEP",4)))
datos1<-c(tabla$dato[1:4],0,0,0,0)
ggplot(tabla,aes(x=dato,y=Modelo,fill=Tipo))+ 
	geom_bar(stat="identity", position="dodge")
	
#El mejor modelo es el modelo 3 modelo de regresión knnn con k=10

# Paso 8) -----------------------------------------------------------------

nuevo<- tibble(KM=c(60.000,22.000,3.000),Age=c(30,25,4),Weight=c(1.300,1.500,1.070))

knn.reg(train = train.set[-4], # Sólo predictoras de train.set
	test = nuevo,   # Sólo predictoras de test.set
	y = train.set$Price,   # Sólo respuesta de train.set
	k = 10)


qqnorm(toyota$Price,col="cyan3")
qqline(toyota$Price,col="Blue")
