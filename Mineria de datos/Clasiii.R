
# Paquetes ----------------------------------------------------------------

library(factoextra)
library(tidyverse)
library(FNN)
library(NbClust)
library(randomForest)
library(rpart)

# 1)  ---------------------------------------------------------------------

# seleccione en un mismo data frame las variables de interés

# Los datos están en línea. Se seleccionan directamente del repositorio
toyota<-read.table("https://raw.githubusercontent.com/gchoi/Dataset/master/ToyotaCorolla.csv",sep=",",header = T)
attach(toyota)
toyota<-as_tibble(toyota)%>%dplyr::select(KM,Age,Price,MetColor) %>% mutate(MetColor=factor(MetColor))

dat<-data.frame(datos=c(scale(KM),scale(Age),scale(Price)),etiqueta=c(rep("KM",1436),rep("Age",1436),rep("Price",1436)),respuesta=rep(toyota$MetColor,3))
ggplot(dat,aes(x=etiqueta,y=datos,colour=respuesta))+geom_boxplot()+xlab("Variables")+ylab("Valores normalizados")

##se observa presencia de valores atipicos

# Función para asignar NA's, con  k=3
funz_score<-function(x){
	k=3
	inf=mean(x)-k*sd(x)
	sup=mean(x)+k*sd(x)
	x[x<inf]<-NA
	x[x>sup]<-NA
	x
}

# Asignación de NA's a las variables numéricas utilizando la función "funz_score"
datos_NAs<-toyota%>%mutate_if(is.numeric,funs(funz_score))

# Tabla con proporción de valores faltantes por variable 
datos_NAs%>%DataExplorer::profile_missing()



datos <- data.frame(scale(toyota[-4]))
numero_clusters <- NbClust(data = datos, min.nc = 2,
			   max.nc = 7, method = "centroid")
fviz_nbclust(numero_clusters)

# Imputación de los NA's por kNN por la mediana con k=2 vecinos
imputacionKnn<-VIM::kNN(data=datos_NAs,
			variable =colnames(datos_NAs),
			k=2,numFun = median)

toyota<-imputacionKnn[1:4]
dat<-data.frame(datos=c(scale(toyota$KM),scale(toyota$Age),scale(toyota$Price)),etiqueta=c(rep("KM",1436),rep("Age",1436),rep("Price",1436)),respuesta=rep(toyota$MetColor,3))
ggplot(dat,aes(x=etiqueta,y=datos,colour=respuesta))+geom_boxplot()+xlab("Variables")+ylab("Valores normalizados")



# 2) ----------------------------------------------------------------------

# construya una conjunto de entrenamiento (70%) y otro de prueba (30%). 
# Tome la semilla 48671. Explore dichos conjuntos en términos de la variable 
# de respuesta

#set.seed(48671)

# Muestra aleatoria del 70% de las filas del conjunto "datos" para el conjunto de entrenamiento
train.filas<-sample(x=row.names(toyota),size = dim(toyota)[1]*0.70)

# CONJUNTO DE ENTRENAMIENTO 
train<-toyota[train.filas,]
# CONJUNTO DE PRUEBA
test.filas<-setdiff(x = row.names(toyota),train.filas)
test<-toyota[test.filas,]


# 3) ----------------------------------------------------------------------

# Entrene los cinco modelos con base en el conjunto de entrenamiento y 
# almacene las correspondientes clases predichas para los automóviles 
# de dicho conjunto

# modelo 1 un modelo de regresión logístico:

modelo1<-glm(MetColor~.,family="binomial",data = train)

prob_m1<-modelo1$fitted.values
pred1<-factor(if_else(prob_m1>0.5,"1","0"))
# modelo 2 un modelo de clasificación ajustado 
# por el algoritmo kNN con k=5 vecinos más próximos

modelo2<-FNN::knn(train = train[,1:3],test=train[,1:3],
		  cl =train$MetColor,k = 5,prob = T )


# Propensidades según modelo 2
prob_m2<-attr(modelo2,"prob")
# Clases predichas según modelo 2
pred2<-modelo2[1:length(train$KM)]	

# modelo 3 un modelo de clasificación ajustado 
# por el algoritmo kNN con k=10 vecinos más próximos

modelo3<-FNN::knn(train = train[,1:3],test=train[,1:3],
		  cl =train$MetColor,k = 10,prob = T )

# Propensidades según modelo 3
prob_m3<-attr(modelo3,"prob")
# Clases predichas según modelo 3
pred3<-modelo3[1:length(train$KM)]

# modelo 4
modelo4 <- rpart(formula = as.factor(MetColor) ~ Age+KM+Price, data = train)
rpart.plot(modelo4)
pred4<-predict(modelo4,train,type = "class")

# modelo 5

modelo5 <- randomForest(MetColor~., data = train, ntree = 500)
pred5 <- predict(modelo5, newdata = train)

# Se almacenan propensidades y clases predichas al conjunto de entrenamiento

# 4) ----------------------------------------------------------------------

# estime (y almacene) las tasas de mala clasificación de entrenamiento TMCE 
# de los cinco modelos. Reporte las correspondientes matrices de confusión 
# ¿Cuál modelo ajustó mejor al conjunto de entrenamiento?


# matrices de confusión ---------------------------------------------------
matriz_m1<-table(observado=train$MetColor,predicho=pred1)
matriz_m2<-table(observado=train$MetColor,predicho=pred2)
matriz_m3<-table(observado=train$MetColor,predicho=pred3)
matriz_m4<-table(observado=train$MetColor,predicho=pred4)
matriz_m5<-table(observado=train$MetColor,predicho=pred5)


# tasa de mala clasificación ----------------------------------------------
TMCEm1<-round(((matriz_m1[1,2]+matriz_m1[2,1])/sum(matriz_m1))*100,2)
TMCEm2<-round(((matriz_m2[1,2]+matriz_m2[2,1])/sum(matriz_m2))*100,2)
TMCEm3<-round(((matriz_m3[1,2]+matriz_m3[2,1])/sum(matriz_m3))*100,2)
TMCEm4<-round(((matriz_m4[1,2]+matriz_m4[2,1])/sum(matriz_m4))*100,2)
TMCEm5<-round(((matriz_m5[1,2]+matriz_m5[2,1])/sum(matriz_m5))*100,2)
tmc<-cbind(NA,TMCEm2,TMCEm3,TMCEm4,TMCEm5)

#el modelo 5 fue el de mejor ajuste

# 5) ----------------------------------------------------------------------

# evalúe los modelos entrenados en el paso 4 utilizando el conjunto de prueba 
# y almacene las correspondientes clases predichas para los automóviles de 
# dicho conjunto.


prob_m1p<-predict(modelo1,newdata = test[,1:3])
eval1<-factor(if_else(prob_m1p>0.5,"1","0"))

#Evaluación del modelo 2: kNN con k=1 vecino más próximo
eval2<-FNN::knn(train = train[,1:3],
		test=test[,1:3],
		cl =train$MetColor,k = 5,
		prob = T )

# Evaluación del modelo 3: kNN con k=5 vecinos más próximos
eval3<-FNN::knn(train = train[,1:3],
		test=test[,1:3],
		cl =train$MetColor,k = 10,
		prob = T )
eval4<-predict(modelo4,test,type = "class")
eval5<-predict(modelo5,test)

# 6) ----------------------------------------------------------------------

# estime, almacene y reporte (en una sola tabla) las distintas métricas de 
# calidad de clasificación de los cinco modelos (TMCP, precisión, 
# sensibilidad, especificidad y puntaje F1) con base en el conjunto de prueba.
# Reporte también las correspondientes matrices de confusión


# matrices de confusion ---------------------------------------------------


matriz_m1p<-table(observado=test$MetColor,predicho=eval1)
matriz_m2p<-table(observado=test$MetColor,predicho=eval2)
matriz_m3p<-table(observado=test$MetColor,predicho=eval3)
matriz_m4p<-table(observado=test$MetColor,predicho=eval4)
matriz_m5p<-table(observado=test$MetColor,predicho=eval5)

# tasa mala calificación --------------------------------------------------


TMCPm1<-round(((matriz_m1p[1,2]+matriz_m1p[2,1])/sum(matriz_m1p))*100,2)
TMCPm2<-round(((matriz_m2p[1,2]+matriz_m2p[2,1])/sum(matriz_m2p))*100,2)
TMCPm3<-round(((matriz_m3p[1,2]+matriz_m3p[2,1])/sum(matriz_m3p))*100,2)
TMCPm4<-round(((matriz_m4p[1,2]+matriz_m4p[2,1])/sum(matriz_m4p))*100,2)
TMCPm5<-round(((matriz_m5p[1,2]+matriz_m5p[2,1])/sum(matriz_m5p))*100,2)

# sensibilidad ------------------------------------------------------------


sen1<-round(matriz_m1p[1,1]/(matriz_m1p[1,2]+matriz_m1p[1,1]),3)
sen2<-round(matriz_m2p[1,1]/(matriz_m2p[1,2]+matriz_m2p[1,1]),3)
sen3<-round(matriz_m3p[1,1]/(matriz_m3p[1,2]+matriz_m3p[1,1]),3)
sen4<-round(matriz_m4p[1,1]/(matriz_m4p[1,2]+matriz_m4p[1,1]),3)
sen5<-round(matriz_m5p[1,1]/(matriz_m5p[1,2]+matriz_m5p[1,1]),3)

# especificación ----------------------------------------------------------


esp1<-round(matriz_m1p[2,2]/(matriz_m1p[2,2]+matriz_m1p[2,1]),3)
esp2<-round(matriz_m2p[2,2]/(matriz_m2p[2,2]+matriz_m2p[2,1]),3)
esp3<-round(matriz_m3p[2,2]/(matriz_m3p[2,2]+matriz_m3p[2,1]),3)
esp4<-round(matriz_m4p[2,2]/(matriz_m4p[2,2]+matriz_m4p[2,1]),3)
esp5<-round(matriz_m5p[2,2]/(matriz_m5p[2,2]+matriz_m5p[2,1]),3)


# tasa de buena calidad ---------------------------------------------------


TbCPm1<-round(((matriz_m1p[1,1]+matriz_m1p[2,2])/sum(matriz_m1p))*100,2)
TbCPm2<-round(((matriz_m2p[1,1]+matriz_m2p[2,2])/sum(matriz_m2p))*100,2)
TbCPm3<-round(((matriz_m3p[1,1]+matriz_m3p[2,2])/sum(matriz_m3p))*100,2)
TbCPm4<-round(((matriz_m4p[1,1]+matriz_m4p[2,2])/sum(matriz_m4p))*100,2)
TbCPm5<-round(((matriz_m5p[1,1]+matriz_m5p[2,2])/sum(matriz_m5p))*100,2)


# f1 ----------------------------------------------------------------------


f11<-round(2*((1-TMCPm1)*sen1)/((1-TMCPm1)+sen1),3)
f12<-round(2*((1-TMCPm2)*sen2)/((1-TMCPm2)+sen2),3)
f13<-round(2*((1-TMCPm3)*sen3)/((1-TMCPm3)+sen3),3)
f14<-round(2*((1-TMCPm4)*sen4)/((1-TMCPm4)+sen4),3)
f15<-round(2*((1-TMCPm5)*sen5)/((1-TMCPm5)+sen5),3)

# 7) ----------------------------------------------------------------------

# Compare las anteriores métricas. A su criterio ¿Cuál modelo escogería para
# predecir si un auto necesita o no necesita una nueva capa de pintura? 
# Justifique
f1<-cbind(f11,f12,f13,f14,f15);colnames(f1)<-c("M1","M2","M3","M4","M5")
TbC<-cbind(TbCPm1,TbCPm2,TbCPm3,TbCPm4,TbCPm5);colnames(TbC)<-c("M1","M2","M3","M4","M5")
TMC<-cbind(TMCPm1,TMCPm2,TMCPm3,TMCPm4,TMCPm5);colnames(TMC)<-c("M1","M2","M3","M4","M5")
sen<-cbind(sen1,sen2,sen3,sen4,sen5);colnames(sen)<-c("M1","M2","M3","M4","M5")
esp<-cbind(esp1,esp2,esp3,esp4,esp5);colnames(esp)<-c("M1","M2","M3","M4","M5")
tabla<-rbind(TMC,TbC,sen,esp,f1);row.names(tabla)<-c("TMC","TBC","Sensibilidad","Especificidad","F1")
(tmc-TMC)

# 8) ----------------------------------------------------------------------


# Con base en el modelo que seleccionó en el punto 7, determine, con base 
# en los perfiles descritos, cuáles de los siguientes automóviles 
# necesitan/no necesitan pintura:

nuevos<-data.frame(KM=c(71235,83000,2100),Age=c(22,6,60),Price=c(11245,12300,33600))

precios.new<-predict(modelo5,newdata =nuevos )
