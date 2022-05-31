
# Librerias ---------------------------------------------------------------

library(tidyverse)
library(mice)
library(factoextra)
library(FactoMineR)
library(caret)
library(leaps)
library(ISLR)
library(GGally)

# Conjunto de datos -------------------------------------------------------

winequality <- read_delim("winequality.csv",";", escape_double = FALSE, trim_ws = TRUE)
glimpse(winequality )

# Es necesario convertir cada variable a su categoria original se convierten 
# las categorias free sulfur, total sulfur y alcohol a variables númericas y la
# categoria color wine a factor


# Categorización correcta de variables ------------------------------------

winequality<-winequality %>% mutate(`free sulfur dioxide`=as.numeric(`free sulfur dioxide`),
				    `total sulfur dioxide`=as.numeric(`total sulfur dioxide`),
				    alcohol=as.numeric(alcohol))
names(winequality)<-c("color","acidezf","acidezv","acido","razucar","calorias",
		      "libreds","totalds","densidad","ph","sulfatos","alcohol",
		      "calidad")
summary(winequality)

# Se encuentran NA'S en el data set se requiere realizar limpieza tambien se 
# detecta que la variable residuo de azucar tiene valores negativos lo cual 
# es una inconsistencia.

winequality$razucar[which(winequality$razucar<=0)] <- NA


# Limpieza ----------------------------------------------------------------

(sum(is.na(winequality))/nrow(winequality))*100

# El data set presenta 11% de valores faltantes
winequality%>%DataExplorer::profile_missing()
# Grafico de resumen que muestra el porcentaje de valores NA's por variable.
DataExplorer::plot_missing(data=winequality)

# Imputación de las variables con NA´s por el metodo mice
multimp.mice<-mice(winequality,m = 5)

# Conjunto de datos imputados 

wine<-complete(multimp.mice)
boxplot(scale(wine[-1]),col=rainbow(12))

# Se detecta presencia de datos atipicos se utiliza el método de los score
# para convertir los datos que se encuentren a más de 3 desviaciones 
# estandar y luego se imputación dichos valores con el método mice

#Función para asignar NA's, con  k=4.4
funz_score<-function(x){
	k=4.4
	inf=mean(x)-k*sd(x)
	sup=mean(x)+k*sd(x)
	x[x<inf]<-NA
	x[x>sup]<-NA
	x
}

# Asignación de NA's a las variables numéricas utilizando la función "funz_score"
datos_NAs<-wine%>%mutate_if(is.numeric,funs(funz_score))

# Grafico de resumen que muestra el porcentaje de valores NA's por variable.
DataExplorer::plot_missing(data=datos_NAs)
datos_NAs%>%DataExplorer::profile_missing()
# Imputación de las variables con NA´s por el metodo mice
multimp.mice<-mice(datos_NAs,m = 5)

# Conjunto de datos imputados 

wine<-complete(multimp.mice)
boxplot(scale(wine[-1]),col=rainbow(12))


# Exploración -------------------------------------------------------------

rojo<-wine %>% filter(color=="red");blanco<-wine %>% filter(color=="white")
rojo<-as.matrix(round(apply(rojo[-1] ,2, function(x) c(Minimo=min(x),Q=quantile(x,probs = 0.25),Mediana=median(x),Media=mean(x),Q=quantile(x,probs = 0.75),Maxímo=max(x),Desviacion=sd(x))),2))
t(rojo)
blanco<-as.matrix(round(apply(blanco[-1] ,2, function(x) c(Minimo=min(x),Q=quantile(x,probs = 0.25),Mediana=median(x),Media=mean(x),Q=quantile(x,probs = 0.75),Maxímo=max(x),Desviacion=sd(x))),2))
t(blanco)
resultados<-matrix(c(rojo[,1],blanco[,1],rojo[,2],blanco[,2],rojo[,3],blanco[,3],rojo[,4],blanco[,4],
rojo[,5],blanco[,5],rojo[,6],blanco[,6],rojo[,7],blanco[,7],rojo[,8],blanco[,8],
rojo[,9],blanco[,9],rojo[,10],blanco[,10],rojo[,11],blanco[,11],rojo[,12],blanco[,12]),ncol=24)
colnames(resultados)<-c("acidezfr","acidezfb","acidezvr","acidezvb",
			"acidor" ,"acidob" ,"razucarr","razucarb","caloriasr","caloriasb",
			"libredsr","libredsb","totaldsr","totaldsb","densidadr","densidadb",
			"phr","phb","sulfatosr","sulfatosb","alcoholr","alcoholb","calidadr","calidadb")
rownames(resultados)<-rownames(rojo);resultados


anova(lm(calidad~.,wine))

#Para detectar la calidad de los vinos se decide categorizar la variable calidad en
# alta, baja y media para ello se utlizan los percentiles 0,33,66,1

# wine<-wine%>%
# 	mutate(calidad1=cut(calidad,breaks = 
# 			    	quantile(calidad,probs = c(0,1/3,2/3,1)),
# 			    include.lowest = T))
# levels(wine$calidad1)<-c("Baja","Media","Alta")
# 
# a<-ggplot(wine,aes(y=calidad,x=color,colour=color))+geom_boxplot()+theme_light()+labs(colour = "Color de vino")
# l<-ggplot(wine,aes(x=wine$calidad1,acidezf,colour=color))+geom_boxplot()+xlab("Calidad")+theme_light()
# b<-ggplot(wine,aes(x=wine$calidad1,acidezv,colour=color))+geom_boxplot()+xlab("Calidad")+theme_light() 
# cc<-ggplot(wine,aes(x=wine$calidad1,acido,colour=color))+geom_boxplot()+xlab("Calidad")+theme_light() 
# d<-ggplot(wine,aes(x=wine$calidad1,razucar,colour=color))+geom_boxplot()+xlab("Calidad")+theme_light() 
# e<-ggplot(wine,aes(x=wine$calidad1,calorias,colour=color))+geom_boxplot()+xlab("Calidad")+theme_light() 
# f<-ggplot(wine,aes(x=wine$calidad1,libreds,colour=color))+geom_boxplot()+xlab("Calidad")+theme_light() 
# g<-ggplot(wine,aes(x=wine$calidad1,totalds,colour=color))+geom_boxplot()+xlab("Calidad")+theme_light() 
# h<-ggplot(wine,aes(x=wine$calidad1,densidad,colour=color))+geom_boxplot()+xlab("Calidad")+theme_light()
# i<-ggplot(wine,aes(x=wine$calidad1,ph,colour=color))+geom_boxplot()+xlab("Calidad")+theme_light()
# j<-ggplot(wine,aes(x=wine$calidad1,sulfatos,colour=color))+geom_boxplot()+xlab("Calidad")+theme_light() 
# k<-ggplot(wine,aes(x=wine$calidad1,alcohol,colour=color))+geom_boxplot()+xlab("Calidad")+theme_light() 
# 
# gridExtra::grid.arrange(a,l,b,cc,d,e,f,g,h,i,j,k,ncol=3)

calidad<-wine %>% filter(calidad1=="Alta")
qq<-table(wine$color,wine$calidad1)
chisq.test(qq)
summary(calidad)

# El realizar el agrupamiento parece ser que los vinos con calidad alta son de color
# blanco con niveles altos de acidez fija, con bajos niveles de acidez volatil, y con alta concentración de alcohol

#en el texto se recomienda realizar un acp sin embargo la variabilidad acumulada no es suficiente para
# hacer un acp solo alcanza el 41 %
acp = PCA(wine[,-c(1,14)], scale.unit = T, quanti.sup = 3, graph = T)
fviz_pca_biplot(acp, col.ind = wine[,1])
acp$var$contrib
(1/11)*100

#Si se hiciera un ACP las variables que mas contribuirian serian total de dioxido en
# la primera compoenente y alcohol en la segunda componente

# comparamos las diferencias entre los dos grupos para ver que grupo es mejor
# ahora se hizo uso de un corrplot para identificar que variables tienen mas correlacion
# con la variable calidad para saber cuales aumentan a esta

numericas<-wine%>%select_if(is.numeric);matrizcor<-cor(numericas,method = "spearman")
library(ggcorrplot);ggcorrplot(matrizcor,type = "lower")

#se encontro que la variable alcohol esta correlacionada de manera postiva, mientras que
#la variables citric acid y el total de oxido de sulfuro estan debilmente ligadas,
#ahora las variables que estan relacionadas de manera inversa, es decir la calidad
#disminuye si estas variables estan presentes son acido volatil, calorias y densidad



# Modelo 1 ----------------------------------------------------------------
set.seed(12345)
attach(wine)
vinos<-caret::createDataPartition(y = wine$calidad,p = 0.75,list = F)

# Conjunto de entrenamiento: training set
train<-wine[vinos, ]

# Conjunto de prueba: test set 

test<-wine[-vinos, ]
modelo1<-lm(calidad~.,data = train)

#se calcula el mse y el r2

y.pred<-predict(modelo1,newdata =test );y.obs<-test$calidad
mse1<-forecast::accuracy(y.pred,y.obs)
r1<-summary(lm(calidad~.,data = test))$r.squared
#coeficientes estimados de este modelo
coeff_modelo1<-modelo1$coefficients



# modelo 2 ----------------------------------------------------------------


#se escalan las variables predictoras y se prepocesan con el paquete caret
prepro<-train%>%select(-calidad)%>%caret::preProcess(method=c("center","scale"))
#se empieza a buscar la importancia de cada variable
datos1<-prepro%>%predict(prepro, newdata = train[,-13])%>%add_column(calidad=train[,13])
mi_control<-trainControl(method="cv", number=12)
modelo2<- train(calidad~., data=datos1, method="lm", trControl=mi_control)

importancia<-caret::varImp(modelo2,useModel = T,scale=T)
#se selecionan las variables con una importancia mayor o igual al 40%
nombres_vars<-importancia$importance%>%filter(Overall>=40)%>%row.names()
datos2<-wine%>%select(nombres_vars,calidad)
modelo2<-lm(calidad~.,data=datos2)
#se calcula mse y el rmse
y.pred<-predict(modelo2,newdata =test );y.obs<-test$calidad
mse2<-forecast::accuracy(y.pred,y.obs)
#los coeficientes de las variables predictoras
coeff_modelo2<-modelo2$coefficients


# modelo 3 ----------------------------------------------------------------

 
#se usa el metodo backward para la selecion de un modelo
mod.candidatos<- regsubsets(calidad~.,data = train, nvmax = 12,method = "backward")
#se calcula el bic de los modelos para la seleccion del mejor y la estimacion de mse
bics<-summary(mod.candidatos)$bic;which.min(bics)
names(summary(mod.candidatos))
mse3<-summary(mod.candidatos)$rsq[which.min(bics)]
summary(mod.candidatos)$which[which.min(bics),]
# Coeficientes de MCO estimados
coef(object = mod.candidatos, id = which.min(bics))
#se selciona el modelo con el menor bic
modelo3<-lm(calidad~acidezf +acidezv+ razucar+ libreds+
	    	 ph + sulfatos + alcohol    ,data=wine)
#los coeficientes del modelo
coeff_modelo3<-coef(object = mod.candidatos, id = which.min(bics))
####modelo4
set.seed(87654)
#se calcula el conjunto de entrenamiento con 9 plieges
micontrol<- trainControl(method = "cv", number=9,
			 savePredictions = "all",returnResamp ="all")
#se estima el mejor modelo con el conjunto de entrenamiento y se slecionara
#el que posea menor mse y r**2
ajuste.cv<-caret::train(calidad~., data=train, method="leapForward",
			trControl=micontrol,metric="RMSE",tuneGrid =data.frame(.nvmax=1:12 ))
# se calcula MSE
RMSE_cv9<-ajuste.cv$results$RMSE;which.min(RMSE_cv9)
best.model.forward<- regsubsets(calidad~., data = wine, nvmax = 12,method = "forward")
#los coeficientes del mejor modeolo
ajuste.cv$results$Rsquared[10]
coef(object = best.model.forward, id = which.min(RMSE_cv9))
#modelo 5
#para ajustar el modelo se uso un metodo de regularizacion
library(glmnet)
mis_lambdas<-10^seq(-2, 2, length = 100)
mi_control<-trainControl(method="cv", number = 10,savePredictions = T)
modelos_ridgecv<-train(calidad~., data=wine, method = "glmnet",
		       trControl =mi_control,tuneGrid = expand.grid(alpha=0,lambda=mis_lambdas))
lambda_opt<-modelos_ridgecv$bestTune$lambda; lambda_opt
#MSE y r**2
mse5<-modelos_ridgecv$results%>%filter(lambda==lambda_opt)
#coeficientes del modelo
betasR_final<-coef(modelos_ridgecv$finalModel, modelos_ridgecv$bestTune$lambda)
#####
#selecion del modelo
mse=as.numeric(c(mse1[2],mse2[2],mse3,RMSE_cv9[9],mse5[3]));which.min(mse)
#prediccion
names(wine)
nuevos=data.frame(acidezf=6.1,acidezv= 0.28,
		  razucar = 1.5,libreds = 25,
		  ph = 3.03,sulfatos = 0.41,alcohol = 12.1)
predict(modelo3,nuevos)
