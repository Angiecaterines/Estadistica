library(tidyverse) 
library(OutlierDetection) # Métodos para detección univariada de outliers
library(Amelia) # Métodos de imputación de NA's
library(DataExplorer) # Visualización de datos
library(mice) # Imputación múltiple de NA's
library(VIM) # Incluye función para imputación por kNN
library(gridExtra) # Paquete para mosaicos de plots
library(FactoMineR) # Análisis factoriales (multivariado descriptivo)
library(DMwR) # Minería de datos con R



# Selección de datos de trabajo
set.seed(2345)
datos<-ggplot2::diamonds%>%select(cut,carat,depth,table,price)%>%sample_n(5000,replace = F)
datos<-datos[-1]
attach(datos)
# Estructura de datos
dplyr::glimpse(datos)

# ¿Hay valores faltantes en "datos"?
any(is.na(datos))


# Ejercicios
# 1) Explore visualmente las distribuciones de frecuencias de las variables analizadas. 
# Intente detectar valores atípicos en cada una de ellas.

# Se crea una función para normalizar las columnas y hacerlas comparables
normalizar<-function(x) (x - mean(x)) / sd(x)


# Tabla con variables normalizadas
normalizadas<-datos%>%dplyr::mutate_if(is.numeric,funs(normalizar))

# box plot para identificar posibles outliers por variables
boxplot(normalizadas[,2:5],col=rainbow(7),las=2,ylab="valores normalizados")

# 
# 2) Construya una tabla de resumen con los principales estadísticos 
# (centralidad, variabilidad y posición) de las variables en cuestión.

round(apply(normalizadas[-1] ,2, function(x) c(Minimo=min(x),Q=quantile(x,probs = 0.25),Mediana=median(x),Media=mean(x),Q=quantile(x,probs = 0.75),Maxímo=max(x),Desviacion=sd(x))),2)

# Ejercicios
# A. Extraiga y almacene en una lista o arreglo los valores considerados outliers 
# para cada una de las cuatro variables.

aa<-round(apply(datos,2, function(x) c(Minimo=min(x),Q=quantile(x,probs = 0.25),Mediana=median(x),Media=mean(x),Q=quantile(x,probs = 0.75),Maxímo=max(x),Desviacion=sd(x))),2)
IQR<-aa[5,]-aa[2,]
LS<-aa[5,]+(1.5*IQR)
LI<-aa[2,]-(1.5*IQR)

carat1<-datos%>%filter(carat>LS[1] | carat<LI[1])%>%select(carat)
depth1<-datos%>%filter(depth>LS[2] | depth<LI[2])%>%select(depth)
table1<-datos%>%filter(table>LS[3] | table<LI[3])%>%select(table)
price1<-datos%>%filter(price>LS[4] | price<LI[4])%>%select(price)

bb<-list(carat1,depth1,table1,price1)


# B. Determine el número de outliers y su proporción con relación al total de valores
# en cada variable.

ll<-as.data.frame(matrix(rep(NA,4),ncol=4))
aa<-as.data.frame(matrix(rep(NA,4),ncol=4))
for( i in 1:4){
	ll[i]<-bb[[i]]%>%summarise(total=n(),porcentaje=(total/dim(datos)[1])*100)
	aa[i]<-bb[[i]]%>%summarise(total=n(),porcentaje=(total/dim(datos)[1])*100)%>%select(porcentaje)
}
por<-rbind(ll,aa)
colnames(por)<-names(datos);rownames(por)<-c("Total","Porcentaje");por


# A. Cree una función que permita identificar, extraer y almacenar los valores 
# candidatos a outliers tomando el criterio de los cuartiles

detection<-function(datos){
aa<-round(apply(datos,2, function(x) c(Minimo=min(x),Q=quantile(x,probs = 0.25),Mediana=median(x),Media=mean(x),Q=quantile(x,probs = 0.75),Maxímo=max(x),Desviacion=sd(x))),2)
IQR<-aa[5,]-aa[2,]
LS<-aa[5,]+(1.5*IQR)
LI<-aa[2,]-(1.5*IQR)

bb<-list()
for (i  in 1:ncol(datos)) {
	bb[i]<-(datos%>%filter(datos[i]>LS[i] | datos[i]<LI[i]))[,i]
	
}
print(bb)
total<-c()
porcentaje<-c()
for ( i in 1:ncol(datos)){
total[[i]]<-	length(bb[[i]])
porcentaje[[i]]<-(total[[i]]/nrow(datos))*100
}
rbind(total,porcentaje)

}
detection(datos)


# B. Identifique la posición (fila y columna) de dichos valores dentro del conjunto datos.
carat2<-which(datos[,1]>LS[1] | datos[,1]<LI[1])
depth2<-which(datos[,2]>LS[2] | datos[,2]<LI[2])
table2<-which(datos[,3]>LS[3] | datos[,3]<LI[3])
price2<-which(datos[,4]>LS[4] | datos[,4]<LI[4])

# C. Identificados dichos valores ¿Qué tratamiento recomienda aplicar a dichos datos?

##imputaión



# Detección de outliers condicionada a una variable: vía boxplots
library(reshape2)

# data frame normalizadas de formato wide a formato long
normalizadas_long<-reshape2::melt(normalizadas, id = "cut")

ggplot(normalizadas_long,aes(x=cut, y=value, group=cut)) + 
  geom_boxplot(aes(fill=cut))+
  facet_grid(variable~ .)+
  theme_light()

# Detecte y ubique dentro de datos posibles valores outliers 
# en la variable table condicionada a la categoría Premium de cut

na<-normalizadas%>%filter(cut=="Premium")
na<-reshape2::melt(na,id = "cut")%>%filter(variable=="table")
ggplot(na,aes(x=cut, y=value, group=cut)) + 
  geom_boxplot(aes(fill=cut))+
  facet_grid(variable~ .)+
  theme_light()

# Detección univariada de outliers vía z-scores

# Histogramas de frecuencias de las variables numéricas
datos%>%plot_density(ncol=2,geom_density_args = list("fill"="cyan"),ggtheme = theme_light())


# Valor de k
k<-4.4

# Para depth, con k=4.4 se identifican posibles outliers:
outs.depth<-datos$depth[which(abs(normalizadas$depth)>k)]

# Para carat, con k=4.4 se identifican posibles outliers:
outs.carat1<-datos$carat[which(abs(normalizadas$carat)>k)]


# Proporción outliers con respecto total valores en carat
length(outs.carat1)/dim(datos)[1]*100



detection_score<-function(datos){
aa<-round(apply(datos,2, function(x) c(Minimo=min(x),Q=quantile(x,probs = 0.25),Mediana=median(x),Media=mean(x),Q=quantile(x,probs = 0.75),Maxímo=max(x),Desviacion=sd(x))),2)

LS=aa[4,]+k*aa[7,]
LI=aa[4,]-k*aa[7,]

bb<-list()
for (i  in 1:ncol(datos)) {
	bb[i]<-(datos%>%filter(datos[i]>LS[i] | datos[i]<LI[i]))[,i]
	
}
print(bb)


total<-c()
porcentaje<-c()
for ( i in 1:ncol(datos)){
total[[i]]<-	length(bb[[i]])
porcentaje[[i]]<-(total[[i]]/nrow(datos))*100
}
rbind(total,porcentaje)
}

detection_score(datos)

# A. Detecte, vía z-scores y con k=4.4, posibles valores outliers en las variables 
# table y price. Almacene estos valores en correspondientes vectores.

k=4.4
# Para table, con k=4.4 se identifican posibles outliers:
outs.table<-datos$table[which(abs(normalizadas$table)>k)]
outs.table1<-datos$table[-which(abs(normalizadas$table)>k)]

tableo <- data.frame(table = c(outs.table, outs.table1),
        tipo = c(rep("outlier", length(outs.table)), rep("no outlier", length(outs.table1))))
ggplot(tableo,aes(x=table,fill=tipo))+geom_histogram(bins = 20)
ggplot(tableo,aes(x=table))+
geom_histogram(aes(fill=tipo),binwidth = .2,col="blue",size=.2) +
labs(title="Histograma de depth con outliers",x="Depth")+
theme_light()

# Para price, con k=4.4 se identifican posibles outliers:
outs.price<-datos$price[which(abs(normalizadas$price)>k)]
outs.price1<-datos$price[-which(abs(normalizadas$price)>k)]

priceo<-data.frame(table=c(outs.price,outs.price1),tipo=c(rep("outlier",length(outs.price)),rep("no outlier",length(outs.price1))))
ggplot(priceo,aes(x=table,fill=tipo))+geom_histogram(bins = 20)
ggplot(priceo,aes(x=table))+
geom_histogram(aes(fill=tipo),binwidth = .2,col="blue",size=.2) +
labs(title="Histograma de depth con outliers",x="Depth")+
theme_light()



# ¿A partir de qué valores, de ambas variables, se considera que un valor es outlier?

aa<-round(apply(datos,2, function(x) c(Minimo=min(x),Q=quantile(x,probs = 0.25),Mediana=median(x),Media=mean(x),Q=quantile(x,probs = 0.75),Maxímo=max(x),Desviacion=sd(x))),2)

rbind(LI=aa[4,]-k*aa[7,],LS=aa[4,]+k*aa[7,])


# Tratamiento univariado de outliers --------------------------------------


# Ejercicio
# Tome la variable carat y calcule los percentiles 5% y 95%.
# 
hh<-apply(datos, 2, function(x) c(Cuantile =quantile(x,0.05),Cuantile =quantile(x,0.95)))

# Identifique los valores de carat considerados outliers según el criterio de los cuartiles 1 y 3 y el IQR.

detection_cap<-function(datos){
bb<-list()
for (i  in 1:ncol(datos)) {
	bb[i]<-(datos%>%filter(datos[i]<hh[1,i] | datos[i]>hh[2,i]))[,i]
	
}
print(p.detection<-bb)
total<-c()
porcentaje<-c()
for ( i in 1:4){
total[[i]]<-	length(bb[[i]])
porcentaje[[i]]<-(total[[i]]/nrow(datos))*100
}
rbind(total,porcentaje)
}
detection_cap(datos)

# Utilice capping sobre la variable carat para imputar los valores atípicos presentes en ella

# Extracción de los outliers de la variable table
outsV1<-boxplot.stats(datos$table)$out

#Se crea la función  "capping" para hacer las imputaciones
capping<-function(x){
	cuantiles<-quantile(x,c(0.05,0.95))
	x[x<cuantiles[1]]<-cuantiles[1]
	x[x>cuantiles[2]]<-cuantiles[2]
	x
}


# Comparar la variable carat antes y despúes de capping a través de un box plot. Comente

# Se aplica el capping sobre la variable table (con casos completos)
# Se tiene que los cuantiles del 5% es 54 y del 95% es 61
table.cleanout<-capping(datos$carat) #variable table sin outliers

# Box plots de variable table con y sin outliers
{par(mfrow=c(1,2),mai=rep(0.8,4))
	boxplot(datos$carat,main="Table con outliers",col="red",ylab="table")
	boxplot(table.cleanout,main="Table sin outliers",col="magenta",y="table")}


## imputación simple

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
datos_NAs<-datos%>%mutate_if(is.numeric,funs(funz_score))

# Tabla con proporción de valores faltantes por variable 
datos_NAs%>%DataExplorer::profile_missing()

# Imputación de los NA's por kNN por la mediana con k=5 vecinos
imputacionKnn<-VIM::kNN(data=datos_NAs,
                        variable =colnames(datos_NAs),
                        k=5,numFun=median)

# Comparación de distribuciones de price con y sin outliers
data.frame(con_out=datos$price,sin_out=imputacionKnn$price)%>%plot_histogram(ncol = 2)


# Selección de las variables de interés
bivariado<-datos%>%dplyr::select(depth,price)

# Diagrama de dispersión
ggplot(bivariado,aes(x=depth,price))+
  geom_point(color="black")+
  labs(title="depth versus price")+
  theme_bw()

ggplot(datos,aes(x=depth,y=price,color=(depth<=58.75 | depth>=64.75) & price>=11763.25))+
  geom_point(size=2.5)+
  theme_light()+
  labs(title="Scatter plot de depth vs. price con posibles outliers")+
  theme(legend.position = "none")

# Detección de diamantes atípicos según regla empírica
atips<-datos%>%filter((depth<=58 | depth>=64) & price>=11763.25)


# Seleccione las variables carat y price. Detecte y extraiga aquellos diamantes 
# donde carat es mayor a 2.5 y price supera los 12000 dólares. Visualice dichos 
# diamantes en un scatter plot.

ggplot(datos,aes(x=carat,y=price,color=(carat>2.5) & price>10000))+
  geom_point(size=2.5)+
  theme_light()+
  labs(title="Scatter plot de depth vs. price con posibles outliers")+
  theme(legend.position = "none")

# Detección de diamantes atípicos según regla empírica
atips<-datos%>%filter(carat>2.5 & price>10000)


# Se considera que un diamante es extremo si carat y price toma valores más 
# allá de sus correspondientes percentiles cinco y noventa y cinco. Seleccione, 
# extraiga y visualice dichos diamantes.

detection_cap(datos)
datos%>%filter((carat<quantile(carat,0.05) |carat>quantile(carat,0.95) ) & (price<quantile(price,0.05) |price>quantile(price,0.95) ))

ggplot(datos,aes(x=carat,y=price,color=(carat<quantile(carat,0.05) |carat>quantile(carat,0.95) ) & (price<quantile(price,0.05) |price>quantile(price,0.95) )))+
  geom_point(size=2.5)+
  theme_light()+
  labs(title="Scatter plot de depth vs. price con posibles outliers")+
  theme(legend.position = "none")

# Detección de iulier basada al centro de la distribución
#bivariado<-datos%>%select(carat,price)
# Centroide de la distribución (vector de medias)
centro<-colMeans(bivariado)

# Se agrega el vector de medias a "bivariado"
bivariado<-bind_rows(centro,bivariado)%>%mutate_all(scale)

# Matriz de distancias euclidianas
matriz.d<-as.matrix(dist(x = bivariado,method = "euclidian",diag = T))

# Vector con distancias de individuos al centro (se excluye centroide)
distancias_c<-matriz.d[1,1:5001]

# Se agrega el vector de distancias a bivariado y se ordena
bivariado<-bind_cols(bivariado,"dist_euc"=distancias_c)%>%arrange(dist_euc)

# Detección de los cien diamantes más "alejados" del centro
cien.outs<-slice(bivariado,4901:5001)%>%select(1:2)
ttab<-bivariado%>%mutate("tipo"=c("centroide",rep("normales",5000-100),rep("Outliers",100)))%>%mutate("tipo"=factor(tipo))
ggplot(ttab,aes(depth,price))+
geom_point(size=2.5,aes(color=tipo))+
geom_point(aes(0,0),color="tomato",size=3)+
theme_light()


# Detecte, y visualice, el 5% de los diamantes más alejados del centro de la 
# distribución conjunta de las variables normalizadas carat y price.
bivariado<-datos%>%select(carat,price)

# Centroide de la distribución (vector de medias)
centro<-colMeans(bivariado)

# Se agrega el vector de medias a "bivariado"
bivariado<-bind_rows(centro,bivariado)%>%mutate_all(scale)

# Matriz de distancias euclidianas
matriz.d<-as.matrix(dist(x = bivariado,method = "euclidian",diag = T))

# Vector con distancias de individuos al centro (se excluye centroide)
distancias_c<-matriz.d[1,1:5001]

# Se agrega el vector de distancias a bivariado y se ordena
bivariado<-bind_cols(bivariado,"dist_euc"=distancias_c)%>%arrange(dist_euc)

# Detección del 10% de los diamantes mas alejados
cien.outs<-bivariado[4501:5001,-3]
ttab<-bivariado%>%mutate("tipo"=c("centroide",rep("normales",5000-500),rep("Outliers",dim(cien.outs)[1]-1)))%>%mutate("tipo"=factor(tipo))
ggplot(ttab,aes(carat,price))+
geom_point(size=2.5,aes(color=tipo))+
geom_point(aes(0,0),color="tomato",size=3)+
theme_light()


# ¿A partir de cuál distancia un diamante se considera outlier?
min(bivariado[4501:5001,3])

#Distancia de Mahalanobis

# Calcular y ordenar las distancias de Mahalanobis al centroide de menor a mayor
mah.dist<-mahalanobis(as.matrix(bivariado[,1:2]),center=colMeans(bivariado[,1:2]),
                    cov=cov(bivariado[,1:2]))

# Se agrega el vector de distancias de mahalanobis a bivariado y se ordena
bivariado1<-bind_cols(bivariado,"dist_mah"=mah.dist)

# Umbral de detección
L<-as.numeric(quantile(mah.dist,probs = 0.95))

# Detección de los diamantes más "alejados" del centro
mah.outs<-filter(bivariado1,dist_mah>L)%>%select(1:2)

# Visualización de diamantes outliers según distancia Mahalanobis
ggplot(bivariado1,aes(depth,price,color=dist_mah>L))+
  geom_point(size=2.5)+
  geom_point(aes(0,0),color="green",size=3)+
  labs(title="Scatter plot de depth vs. price normalizados")+
  theme_light()+
  theme(legend.position = "none") 



# proyección ACP ----------------------------------------------------------

# Selección de variables numéricas y normalización de las mismas
dat_num<-datos%>%select(2:5)%>%mutate_all(scale)
colnames(dat_num)<-names(datos[,2:5])

# Realización del ACP normalizado (Análisis en Componentes Principales)
res.pca<-PCA(dat_num,graph = FALSE)

# Coordenadas factoriales de las observaciones
PC1 <- res.pca$ind$coord[,1] # Primer eje o dimensión
PC2 <- res.pca$ind$coord[,2] # Segundo eje o dimensión
nombres <- rownames(res.pca$ind$coord) # Asignación etiquetas observaciones

# Data frame con coordenadas factoriales de las dos primeras dimensiones
coordins<- data.frame(cbind(PC1,PC2))
rownames(coordins)<-nombres

# Proyección de las observaciones sobre primer plano factorial
ggplot(coordins,aes(PC1, PC2)) + 
	geom_point(aes(PC1, PC2),color="tomato",size=1.5,alpha=0.7)+
	labs(title="Diamantes proyectados sobre primer plano factorial",
	     x="Dimensión 1",y="Dimensión 2")+
	theme_light()

# Detecte el 3% de los diamantes más atípicos utilizando la distancia de Mahalanobis
# al centroide del primer plano proyectado del ACP
mah.dist<-mahalanobis(as.matrix(coordins),center=colMeans(coordins),
		      cov=cov(coordins))

# Se agrega el vector de distancias de mahalanobis a bivariado y se ordena
prueba1<-bind_cols(coordins,"dist_mah"=mah.dist)

# Umbral de detección
L<-as.numeric(quantile(mah.dist,probs = 0.03))

# Detección de los diamantes más "alejados" del centro
mah.outs<-filter(prueba1,dist_mah>L)%>%select(1:2)

# Visualización de diamantes outliers según distancia Mahalanobis
ggplot(prueba1,aes(PC1,PC2,color=dist_mah>L))+
	geom_point(size=2.5)+
	geom_point(aes(0,0),color="green",size=3)+
	labs(title="Scatter plot de depth vs. price normalizados")+
	theme_light()+
	theme(legend.position = "none")


# Proyecte los individuos identificados en el numeral anterior sobre el primer plano del ACP.


# Detección multivariada k vecinos ----------------------------------------

# Número de vecinos
k<-5

# Se calcula la matriz de distancias euclidianas para las coordenadas factoriales
# en las dos primeras dimensiones
matriz.de<-as.matrix(dist(coordins))

# Se almacenan las n k-distancias máximas (d_kmax) para cada observación
kdist<-1:length(coordins$PC1)

for(i in 1:length(coordins$PC1)){
	kdist[i]=(sort(matriz.de[i,]))[k-1]
}

# Detección visual de posibles outliers
p3<-ggplot(data=coordins,aes(x=PC1,y=PC2,color=kdist))+
	geom_point(size=kdist+1.5)+
	labs(title="Diamantes proyectados sobre primer plano factorial",
	     x="Dimensión 1",y="Dimensión 2")+
	theme_light()+
	scale_colour_gradientn(colours=c("yellow", "red"))


p4<-ggplot(data=coordins,aes(x=PC1,y=PC2,color=kdist))+
	geom_point(size=kdist+1.5)+
	geom_text(label=nombres,color="black",size=1.7)+
	labs(title="Diamantes proyectados sobre primer plano factorial",
	     x="Dimensión 1",y="Dimensión 2")+
	scale_colour_gradientn(colours=c("yellow", "red"))+
	theme_light()

grid.arrange(p3,p4,ncol=2)

# Detección de outliers con base en umbral L
# Umbral de detección
umbral<-as.numeric(quantile(kdist,probs = 0.99))

# Data frame con coordenadas factoriales y k-distancias máximas
df_outsknn<-bind_cols(coordins,"dist_knn"=kdist)

# Proyección de los outliers sobre primer plano factorial
ggplot(df_outsknn,aes(PC1,PC2,color=kdist>umbral))+
	geom_point(size=kdist+3)+
	geom_text(label=ifelse(kdist>umbral,nombres,""),color="black")+
	labs(title="Diamantes proyectados sobre primer plano factorial",
	     x="Dimensión 1",y="Dimensión 2")+
	theme_light()

# Data frame con los outliers en las variables originales
outs_multknn<-bind_cols(datos,"dist_knn"=kdist)%>%filter(dist_knn>umbral)

tipo<-c(rep("normales",4950),rep("outliers",50))
ff<-bind_cols(datos,"dist_knn"=kdist)%>%
	arrange(kdist)%>%bind_cols("tipo"=tipo)%>%select(-dist_knn)
ff1<-ff%>%select(tipo,cut)%>%table()%>%prop.table(margin = 1)%>%round(2)
ff1
ff2<-data.frame(ff1)
ggplot(ff2, aes(fill=tipo, y=Freq, x=cut)) +
	geom_bar(position="fill", stat="identity")+
	labs(title="Perfil comparativo por cut entre normales  y outliers",y="Porcentaje")+
	theme_light()
ff3<-ff%>%group_by(tipo)%>%
	summarise(med_carat=median(carat),med_depth=median(depth),med_table=median(table),
		  med_price=median(price))
ff3
# data frame normalizadas de formato wide a formato long
tlong<-ff%>%select(-cut)%>%mutate_if(is.numeric,scale)%>%reshape2::melt(id = "tipo")
ggplot(tlong,aes(x=variable,y=value,fill=tipo))+
	geom_boxplot()+labs(title="Boxplot comparativo por variables normalizadas",y="valores normalizados")+theme_light()
ff4<-ff%>%group_by(tipo,cut)%>%summarise(med_carat=median(carat),med_depth=median(depth),med_table=median(table),med_price=median(price))%>%arrange(med_price,cut,med_carat)
ff4
ff5<-ff%>%mutate_if(is.numeric,scale)%>%melt(id.vars = c(1,6),measure.vars = 2:5)
ggplot(ff5,aes(x=tipo, y=value, group=tipo)) +
	geom_boxplot(aes(fill=tipo))+
	facet_grid(cut~variable)+
	theme_light()
OutlierDetection::nnk(datos[,2:5],k = 5,cutoff = 0.95,rnames = T)

a<-OutlierDetection::nnk(normalizadas,k = 10,cutoff = 0.98)

boxplot(a$`Outlier Observations`,col=rainbow(6))



# Detección multivariada de outliers basada en densidades locales ---------

# Selección de las variables númericas de datos
datos1<-datos%>%select(2:5)
# Se aplica la detección de outliers vía LOF con k=300 vecinos
t <- proc.time() # Inicia el cronómetro
LOF<-lofactor(data=datos1, k=300)
ejecucion<-proc.time()-t # tiempo de ejecución
# Tabla con LOFs para todas las observaciones
datos1<-bind_cols(datos1,"LOFs"=LOF)
# Percentil 95 de puntajes LOF
umb1<-as.numeric(quantile(datos1$LOFs,0.95))
# Histograma con puntajes LOF y 5% de LOF más extremos
ggplot(data = datos1, aes(x=LOFs)) +
	geom_histogram(fill="steelblue",color="black",bins = 50)+
	geom_vline(aes(xintercept =umb1), color="red",size = 1.5, alpha = 0.8)+
	geom_vline(aes(xintercept =1.09), color="yellow",size = 1.5, alpha = 0.8)+
	labs(title="Puntajes LOF: elección umbral L",x="LOF's",y="Frecuencias")+
	theme_light()
# Umbral aceptable
umb2<-1.09
# Observaciones outliers sobre variables originales
outs.LOF<-datos1%>%filter(LOFs>umb2)
# Se agregan puntajes LOF a las coordenadas Factoriales
coordins1<-coordins%>%bind_cols("LOFs"=LOF)%>%
	mutate(tipo=if_else(LOFs>umb2,"outlier","normal"))
ggplot(coordins1,aes(PC1, PC2,color=tipo)) +
	geom_point()+
	geom_density2d()+
	labs(title="observaciones proyectadas sobre un plano factorial en 2d",color="LOFs")+
	theme_light()
# Tabla con variables numéricas normalizadas
normalizadas<-datos%>%
	mutate_if(is.numeric,funs(normalizar))%>%
	select(2:5)

# Se desarrolla el k-means para k=5 centroides sobre las variables normalizadas
grupos<-kmeans(normalizadas,centers = 5,iter.max = 10000)
# Centroides de los cinco clústers (vectores 2-dimensionales)
Cks<-data.frame(grupos$centers,"clusters"=factor(1:5))
# observaciones para los tres clústers
clusters<-grupos$cluster
# Distribución de los clústers
distri<-table(clusters)
# Se agregan las etiquetas de los clúster a los que pertenecen los individuos
# al conjunto datos
df.clusters<-bind_cols(datos,"cluster"=clusters)%>%mutate(clusters=factor(cluster))
# Proyección de los clústers sobre distintos espacios
a1<-ggplot(df.clusters,aes(x=depth,y=price,color=clusters))+
	geom_point()+
	labs(title="Clústers: depth versus price")+
	theme_light()
a2<-ggplot(df.clusters,aes(x=carat,y=price,color=clusters))+
	geom_point()+
	labs(title="Clústers: carat versus price")+
	theme_light()
a3<-ggplot(df.clusters,aes(x=table,y=price,color=clusters))+
	geom_point()+
	labs(title="Clústers: table versus price")+
	theme_light()
a4<-ggplot(df.clusters,aes(x=carat,y=depth,color=clusters))+
	geom_point()+
	labs(title="Clústers: carat versus depth")+
	theme_light()
grid.arrange(a1,a2,a3,a4,ncol=2)
grupos<-kmeans(coordins,centers = 5,iter.max = 10000,nstart = 50)
# Centroides de los cinco clústers (vectores 2-dimensionales)
Cks1<-data.frame(grupos$centers,"clusters"=factor(1:5))
# observaciones para los tres clústers
clusters1<-grupos$cluster
# Visualización de los cincos clusteres en el plano factorial
PCs<-coordins%>%
	mutate(clusters=factor(clusters1))
ggplot(PCs,aes(x=PC1,y= PC2,color=clusters))+
	geom_point(alpha=0.6)+
	geom_point(data=Cks1, aes(x=PC1,y=PC2,color=clusters),size = 4,pch=19,colour="black")+
	labs(title="Plano factorial con clusters y centroides del k-means")+theme_light()
# Data frame con centroides del k-means y variables numéricas normalizadas
tabla1<-rbind(normalizadas,Cks[,1:4])
# Matriz de distancias euclidianas
D<-as.matrix(dist(x = tabla1,method = "manhattan"))
# Únicamente distancias de las observaciones los k=5 centroides
distancias<-D[5001:5005,-c(5001:5005)]
# 250 observaciones mas alejadas de los centroides
distmax<-sort(apply(distancias,2,FUN = max),decreasing = T)[1:250]
# Selección de observaciones consideradas outliers en las variables originales
outs.kmeans<-datos[names(distmax),]
grupos<-kmeans(coordins,centers = 5,iter.max = 10000,nstart = 50)
# Centroides de los cinco clústers (vectores 2-dimensionales)
Cks1<-data.frame(grupos$centers)
tabla2<-rbind(coordins,Cks1)
# Matriz de distancias euclidianas
D1<-as.matrix(dist(x = tabla2,method = "manhattan"))
# Únicamente distancias de las observaciones los k=5 centroides
distancias1<-D1[5001:5005,-c(5001:5005)]
# observaciones mas alejadas de los centroides
distmax1<-apply(distancias1,2,FUN = max)
tabla2<-mutate(coordins,distmax=distmax1)%>%arrange(distmax)%>%
	mutate(tipo=factor(c(rep("normal",5000-250),rep("outlier",250))))
ggplot(tabla2,aes(x=PC1,y= PC2,color=tipo))+
	geom_point(alpha=0.6)+
	geom_point(data=Cks1, aes(x=PC1,y=PC2,color=clusters),size = 4,pch=19,colour="black")+
	labs(title="Plano factorial con centroides del k-means")+theme_light()

