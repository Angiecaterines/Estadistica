library(tidyverse)
library(gridExtra)
library(caret)
library(AmesHousing)
library(foreign)
library(effects)
library(relaimpo)
library(leaps)
library(ggplot2)
set.seed(123)

# Conjunto ames preprocesada
ames<-AmesHousing::make_ames()
attach(ames)
# Selección aleatoria de filas de ames
id.propiedad<-caret::createDataPartition(y = ames$Sale_Price,p = 0.7,list = F)

# Conjunto de entrenamiento: training set
train<-ames[id.propiedad, ]
train<- train %>% dplyr:: select(Sale_Price,Lot_Frontage,Lot_Area,Street,Year_Built,Year_Remod_Add,Gr_Liv_Area)

# Conjunto de prueba: test set 
test<-ames[-id.propiedad, ]
test<- test %>% dplyr::select(Sale_Price,Lot_Frontage,Lot_Area,Street,Year_Built,Year_Remod_Add,Gr_Liv_Area)



# Punto 1 -----------------------------------------------------------------


# modelo 1 ----------------------------------------------------------------
modelo1<-lm(Sale_Price ~ .,data = train)


# modelo 2 ----------------------------------------------------------------
modelo2<-lm(Sale_Price ~ Lot_Area + Year_Built + Gr_Liv_Area,data = train)


# modelo 3 ----------------------------------------------------------------
train1<- train[,-2] %>% mutate(Lot_Frontage=(train$Lot_Frontage-mean(train$Lot_Frontage))/sd(train$Lot_Frontage))
test1<- test[,-2] %>% mutate(Lot_Frontage=(test$Lot_Frontage-mean(test$Lot_Frontage))/sd(test$Lot_Frontage))
modelo3<-lm(Sale_Price ~ Lot_Frontage + Street +  Year_Remod_Add ,data = train1)

# modelo 4 ----------------------------------------------------------------

#verifica si hay presencia de datos atipicos

# Se crea una función para normalizar las columnas y hacerlas comparables
normalizar<-function(x) (x - mean(x)) / sd(x)


# Tabla con variables normalizadas
normalizadas<-train%>%dplyr::mutate_if(is.numeric,funs(normalizar)) %>% dplyr::select(-Street)


boxplot(normalizadas,col = rainbow(6))

#Existe presencia de datos atipicos 

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
datos_NAs<-train%>%mutate_if(is.numeric,funs(funz_score))

# Tabla con proporción de valores faltantes por variable 
datos_NAs%>%DataExplorer::profile_missing()

# Imputación de los NA's por kNN por la mediana con k=2 vecinos
imputacionKnn<-VIM::kNN(data=datos_NAs,
			variable =colnames(datos_NAs),
			k=2,numFun=median)

imputacion<-imputacionKnn[1:7]
train<-imputacion
{par(mfrow=c(1,2),mai=rep(0.8,4))
	boxplot(scale(imputacion[-4]),col=rainbow(8),main="Ouliers imputados por Knn ")
	boxplot(scale(train[-4]),col=rainbow(9),main="Conjunto de datos original")}



# se utiliza la funión cal.relimp para mirar las variables que aportan al modelo

calc.relimp(modelo1,type=c("lmg","last","first","pratt"),
	    rela=TRUE)

Efectos <- allEffects(modelo1)
plot(Efectos)

#Graficos de la distribución de las variables

a<-ggplot(train,aes(x=Lot_Frontage,y= Sale_Price))+geom_point(col="tomato") + stat_smooth(method = "lm")
b<-ggplot(train,aes(x=Lot_Area,y= Sale_Price))+geom_point(col="tomato")+  stat_smooth(method = "lm")
f<-ggplot(train,aes(x=Street,y= Sale_Price,colour=Street))+geom_boxplot()
c<-ggplot(train,aes(x=Year_Built,y= Sale_Price))+geom_point(col="tomato") + stat_smooth(method = "lm")
d<-ggplot(train,aes(x=Year_Remod_Add,y= Sale_Price))+geom_point(col="tomato") +  stat_smooth(method = "lm")
e<-ggplot(train,aes(x=Gr_Liv_Area,y= Sale_Price))+geom_point(col="tomato")+  stat_smooth(method = "lm")

gridExtra::grid.arrange(a,b,f,c,d,e,ncol=3)

modelo4<-lm(Sale_Price ~ Year_Built + Year_Remod_Add + Gr_Liv_Area,data = train )
#modelo41<-lm(Sale_Price ~ Year_Built + Year_Remod_Add + Gr_Liv_Area,data = train )

#modelo41<- lm(log(Sale_Price) ~ Year_Built + Year_Remod_Add + Gr_Liv_Area,data = train )

# Punto 2 -----------------------------------------------------------------


# coeficientes de regresión -----------------------------------------------


round(summary(modelo1)$coefficients,4)
round(summary(modelo2)$coefficients,4)
round(summary(modelo3)$coefficients,4)
round(summary(modelo4)$coefficients,4)

# MSEE --------------------------------------------------------------------
n<-length(train$Sale_Price)
e<-residuals(modelo1)
RSS<-t(e)%*%e
MSEE1<-RSS/(n-2)

e<-residuals(modelo2)
RSS<-t(e)%*%e
MSEE2<-RSS/(n-2)

e<-residuals(modelo3)
RSS<-t(e)%*%e
MSEE3<-RSS/(n-2)

e<-residuals(modelo4)
RSS<-t(e)%*%e
MSEE4<-RSS/(n-2)

# R2 ----------------------------------------------------------------------

R2<-summary(modelo1)
R2.1<-R2$adj.r.squared

R2<-summary(modelo2)
R2.2<-R2$adj.r.squared

R2<-summary(modelo3)
R2.3<-R2$adj.r.squared

R2<-summary(modelo4)
R2.4<-R2$adj.r.squared

resumen<-data.frame(MSE_E=c(MSEE1,MSEE2,MSEE3,MSEE4),R2=c(R2.1,R2.2,R2.3,R2.4))


# Punto 3 -----------------------------------------------------------------

# Precios predichos para el conjunto de prueba
y.pred1<-predict(modelo1,newdata =test)

# Precios predichos para el conjunto de prueba
y.pred2<-predict(modelo2,newdata =test)

# Precios predichos para el conjunto de prueba
y.pred3<-predict(modelo3,newdata =test1)

# Precios predichos para el conjunto de prueba
y.pred4<-predict(modelo4,newdata =test)

# Precios observados para el conjunto de prueba
y.obs<-test$Sale_Price

# Medidas de potencia de predicción 
m1<-forecast::accuracy(y.pred1,y.obs)
m2<-forecast::accuracy(y.pred2,y.obs)
m3<-forecast::accuracy(y.pred3,y.obs)
m4<-forecast::accuracy(y.pred4,y.obs)

resumen<-resumen%>%mutate(RMSEP=c(m1[2],m2[2],m3[2],m4[2]))


ep1<-y.obs-y.pred1
qqnorm(ep1,col="cyan")
qqline(ep1,col="blue")

ep2<-y.obs-y.pred2
qqnorm(ep2,col="cyan")
qqline(ep2,col="blue")

ep3<-y.obs-y.pred3
qqnorm(ep3,col="cyan")
qqline(ep3,col="blue")

ep4<-y.obs-y.pred4
qqnorm(ep4,col="cyan")
qqline(ep4,col="blue")

ggplot(data.frame(ep1), aes(x=ep1))+geom_histogram(col='black', fill='red', alpha=0.4)+xlab("Residuales de predicción")+ylab("Frecuencias")
ggplot(data.frame(ep2), aes(x=ep1))+geom_histogram(col='black', fill='red', alpha=0.4)+xlab("Residuales de predicción")+ylab("Frecuencias")
ggplot(data.frame(ep3), aes(x=ep1))+geom_histogram(col='black', fill='red', alpha=0.4)+xlab("Residuales de predicción")+ylab("Frecuencias")
ggplot(data.frame(ep4), aes(x=ep1))+geom_histogram(col='black', fill='red', alpha=0.4)+xlab("Residuales de predicción")+ylab("Frecuencias")


# Punto 4 -----------------------------------------------------------------

new.data<-data.frame(Lot_Frontage=c(200,310,155),Lot_Area=c(8050,15900,48700),Street=factor(c("Pave","Grvl","Pave")),Year_Built=c(1984,1976,2000),Year_Remod_Add=c(2005,1976,2010),Gr_Liv_Area=c(2167,8145,13051))

# Predicción de precios con base en modelo1
precios.new<-predict(modelo4,newdata =new.data )

