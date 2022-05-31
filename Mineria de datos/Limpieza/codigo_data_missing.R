################################################################################
################################################################################
## Detección y tratamiento de datos faltantes: **missing data** ################
################################################################################
################################################################################
#
#
#
################################################################################
## Paquetes necesarios #########################################################
################################################################################
library(tidyverse) 
library(finalfit) # Ajuste y visualización de modelos
library(Amelia) # Métodos de imputación de NA's
library(DataExplorer) # Visualización de datos
library(mice) # Imputación múltiple de NA's
library(VIM) # Incluye función para imputación por kNN
#
#
#
################################################################################
## Conjunto de datos ###########################################################
################################################################################
# Selección de datos de trabajo
datos<-ggplot2::diamonds%>%select(cut,carat,price)

# Estructura de datos
dplyr::glimpse(datos)

# ¿Hay valores faltantes en "datos"?
any(is.na(datos))

# Aleatoriamente se crea un conjunto de valores fila/columna para borrar
set.seed(979)
col<-sample(2:3,15000,replace=T)
row<-sample(nrow(datos),15000,replace=T)
missing<-data.frame(row,col)

# Asignar NA's a la celdas seleccionadas. El conjunto de datos con valores perdidos es dat1
dat1<-datos
for(i in 1:nrow(missing)) 
  dat1[missing[i,1],missing[i,2]]<-NA

# Se agrega la variable color a dat1
dat1$colorMAR<-diamonds$color

# NA´s en colorMAR condicionada a "Very Good" en cut
set.seed(1234)
dat1$colorMAR[dat1$cut=="Very Good"]<- 
  sample(c("D", "E", "F", "G", "H", "I", "J", NA), 
         sum(dat1$cut=="Very Good"), 
         replace = TRUE,
         prob = c(rep(0.1,7),0.3))

# NA´s en colorMAR condicionada a "Premium" en cut
set.seed(1234)
dat1$colorMAR[dat1$cut=="Premium"]<- 
  sample(c("D", "E", "F", "G", "H", "I", "J", NA), 
         sum(dat1$cut=="Premium"), 
         replace = TRUE,
         prob = c(rep(0.1,7),0.3))
#
#
#
################################################################################
## Detección y conteo global de NA's ###########################################
################################################################################
# '¿Hay NA's dentro del conjunto de datos?
any(is.na(dat1))

# Resumen de datos para detectar NA´s
summary(dat1)

# Función para calcular número de celdas con NA's
na.number<- function(x){sum(is.na(x))}
na.number(dat1)

# Porcentaje de NA's con respecto al total de valores
na.number(dat1)/(nrow(dat1)*ncol(dat1))*100

# Identificar fila/columna donde está el valor pérdido
ubic.NA<-which(is.na(dat1)==TRUE,arr.ind = TRUE)

# Retorna las filas con al menos, un valor perdido
ubic.NA2<-dat1[!complete.cases(dat1),]

# Porcentaje de observaciones (filas) con al menos un valor faltante dentro de los datos
incompletos<-nrow(dat1[!complete.cases(dat1),])
porcentaje<-incompletos/nrow(dat1)*100

# Distribución de los NA's en el conjunto de datos (Missing plot)
Amelia::missmap(obj=dat1,col=c("red","yellow"),rank.order = F,legend = T)

#Lo mismo, pero con finalfit (ordenando cut)
dat1%>%arrange(cut)%>%missing_plot()

# Tabla con proporción de valores faltantes por variable 
dat1%>%DataExplorer::profile_missing()

# Tabla con proporción de valores faltantes por variable con finalfit
dat1%>%finalfit::missing_glimpse()

# Gráfica con proporción de valores faltantes por variable 
DataExplorer::plot_missing(data=dat1,title = "Porcentaje de valores perdidos por variables")

# Distribución de NA's por categorias de la variable "cut"
explicativas<-names(dat1)[2:4]

dat1%>% 
  finalfit::missing_pairs(dependent = "cut",explanatory =explicativas)
#
#
#
################################################################################
## Suprimiendo missing: Listwise ###############################################
################################################################################
#Se extraen las observaciones incompletas generando un conjunto con 
# sólo observaciones completas
newdata1<-na.omit(dat1)

# verificación
any(is.na(newdata1))

# Dimensión data frame sin NA's
dim(newdata1)

# Dimensión data frame original
dim(dat1)

# Mapa con posibles NA's
newdata1%>%finalfit::missing_plot()
#
#
#
################################################################################
## Imputación simple de NA's ###################################################
################################################################################
# Histogramas para las variables numéricas de dat1
dat1%>%plot_histogram(ncol = 2)

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

# Verificación
df.imp%>%profile_missing()

# Comparación de distribuciones
df.imp%>%DataExplorer::plot_histogram(ncol = 2)

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
#
#
#
################################################################################
## Imputación de NA's mediante distribuciones no condicionadas #################
################################################################################
# Aplicación del kNN para imputación con K=5 vecinos utilizando la mediana muestral
# para las variables numéricas y la categoría más frecuentes para las categóricas

# Aplicación de kNN, junto a tiempo de ejecución real
# Se seleccionan aleatoriamente 10000 observaciones de dat1 (por tiempo ejecución)

df.recortado<-dat1%>%sample_n(size = 10000,replace = F)

t <- proc.time() # Inicia el cronómetro
imputacionKnn<-VIM::kNN(data=df.recortado,
                        variable = c("carat","price","colorMAR"),
                        k=5,numFun=median,catFun=maxCat)
ejecucion<-proc.time()-t # tiempo de ejecución

# Valores imputados para la variable carat por kNN
imputados_knn<-imputacionKnn[imputacionKnn$carat_imp=="TRUE","carat"]

# Data frame con variables observadas y variables imputadas por kNN
df.compara<-data.frame(valores=c(imputados_knn,dat1$carat),
                       tipo=c(rep("imputados.Knn",length(imputados_knn)),
                              rep("observados",length(dat1$carat))))

# Histogramas con variables numéricas con NA's e imputadas por kNN
ggplot(data=df.compara,aes(x=valores))+
  geom_histogram(data=subset(df.compara,tipo=="observados"),color = "blue", alpha = 0.2)+
  geom_histogram(data=subset(df.compara,tipo=="imputados.Knn"),fill = "red", alpha = 0.6)+
  theme_light()
#
#
#
################################################################################
## Imputación de NA's por regresión ############################################
################################################################################
# Ajuste de un modelo lineal múltiple, sólo casos completos
ajuste<-lm(carat~.,data = dat1,na.action = na.omit)
summary(ajuste)

# Data frame con observaciones donde carat tiene NA's y las demás variables no
caratNA<-dat1[is.na(dat1$carat) & !is.na(dat1$price) & !is.na(dat1$colorMAR),]

# Imputación de NA's por valores predichos del modelo
imputados.reg<-predict(object = ajuste,newdata =caratNA)

# Comparación de ditribuciones valores observados y valores imputados
df.compara1<-data.frame(valores=c(dat1$carat,imputados.reg),
                        tipo=c(rep("observados",length(dat1$carat)),
                               rep("imputados.reg",length(imputados.reg))))

ggplot(data=df.compara1,aes(x=valores))+
  geom_histogram(data=subset(df.compara1,tipo=="observados"),color = "blue", alpha = 0.2)+
  geom_histogram(data=subset(df.compara1,tipo=="imputados.reg"),fill = "red", alpha = 0.6)+
  theme_light()

# Imputación por regresión con paquete mice
s <- proc.time() # Inicia el cronómetro
reg.mice<- mice(dat1, method =c("cart","norm.predict","norm.predict","cart"), seed = 1,
                m = 1, print = FALSE)
ejecucions<-proc.time()-s # tiempo de ejecución

# Métodos de regresión empleados
metodos<-reg.mice$method

# Variables imputadas (valores observados más valores imputados)
varimput.reg<-mice::complete(reg.mice)

# Variables indicadores de imputación
indicadoras<-reg.mice$where
#
#
#
################################################################################
## Imputación múltiple de NA's #################################################
################################################################################
# Aplicación del algoritmo mice con 5 réplicas y tiempo de ejecución
t1 <- proc.time() # Inicia el cronómetro
multimp.mice<-mice::mice(dat1,m = 5)
ejecucion1<-proc.time()-t1

# Valores observados e imputados después de mice
after.mice<-complete(multimp.mice)

# ¿Qué valores fueron imputados?
donde<-as.data.frame(multimp.mice$where)

# ¿Qué método de imputación se utilizó para cada variable?
metodosimp<-multimp.mice$method

# Data frame con variables observadas y variables imputadas por mice
df.union<-dplyr::bind_cols(select(after.mice,2:4),select(donde,2:4))
names(df.union)<-c("carat","price","colorMAR","carat.mice","price.mice","colorMAR.mice")

# Valores imputados para la variable carat por mice
imputados_mice<-df.union[df.union$carat.mice=="TRUE","carat"]

# Data frame con variables observadas y variables imputadas por kNN
df.compara2<-data.frame(valores=c(imputados_mice,dat1$carat),
                        tipo=c(rep("imputados.mice",length(imputados_mice)),
                               rep("observados",length(dat1$carat))))

# Histogramas con variables numéricas con NA's e imputadas por kNN
ggplot(data=df.compara2,aes(x=valores))+
  geom_histogram(data=subset(df.compara2,tipo=="observados"),color = "blue", alpha = 0.2)+
  geom_histogram(data=subset(df.compara2,tipo=="imputados.mice"),fill = "red", alpha = 0.6)+
  theme_light()

# Tabla comparación con carat observado y carat imputado
resumen2<-df.compara2%>%group_by(tipo)%>%
  summarize("min"=min(valores,na.rm = T),
            "media"=mean(valores,na.rm = T),
            "sd"=sd(valores,na.rm=T),
            "max"=max(valores,na.rm = T),
            "Total"=n())%>%arrange(min)%>% 
  knitr::kable(row.names=FALSE, align = c("l", "r", "r", "r", "r","r"),digits = 3)

resumen2

##################################################################################
##################################################################################







