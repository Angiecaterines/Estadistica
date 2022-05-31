library(readr)
library(dplyr)
library(ggplot2)
library(Amelia)
library(FactoMineR)
library(cluster)
library(kmodR)
library(tidyverse) 
library(OutlierDetection) # Métodos para detección univariada de outliers
library(Amelia) # Métodos de imputación de NA's
library(DataExplorer) # Visualización de datos
library(mice) # Imputación múltiple de NA's
library(VIM) # Incluye función para imputación por kNN
library(gridExtra) # Paquete para mosaicos de plots
library(FactoMineR) # Análisis factoriales (multivariado descriptivo)
library(DMwR) # Minería de datos con R
library(cluster)
library(factoextra)


# 1) ----------------------------------------------------------------------
# Importe el conjunto de datos, realice un resumen de sus variables y 
# verifique si hay presencia de NA’s. Si es así, impute por el método 
# más conveniente.

clientes <- read_delim("C:/Users/Hp/Downloads/clientes.csv", 
		       ";", escape_double = FALSE, trim_ws = TRUE)
glimpse(clientes)
summary(clientes[-1])
any(is.na(clientes))
apply(clientes,2,function(x)sum(is.na(x)))

clientes1<-(clientes)[-1] %>% mutate_all(scale)

boxplot(clientes1,col = rainbow(6))
detection(clientes1)

# 2) ----------------------------------------------------------------------

# Detecte posibles valores outliers para cada una de las variables bajo estudio.
# De acuerdo a su criterio, aplique el tratamiento más adecuado para manejar 
# estos valores. Explique

# En la detección de outliers via boxplot se evidencio un promedio de 6% de valores
# ouliers por variable, sin embargo esta tecnica es muy robusta y nos interesa detectar 
# patrones de compra por esta razón se realiza la detección via score con k=3 para detectar 
# aproximadamente el 11% de los valores que estan a mas de 1.5 sd del E(x) y la imputación 
# se realiza por la tecnica KNN con k=2 que sugun silohuette es el número de vecinos 
# mas optimo.

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
datos_NAs<-clientes%>%mutate_if(is.numeric,funs(funz_score))

# Tabla con proporción de valores faltantes por variable 
datos_NAs%>%DataExplorer::profile_missing()

# Imputación de los NA's por kNN por la mediana con k=2 vecinos
imputacionKnn<-VIM::kNN(data=datos_NAs,
			variable =colnames(datos_NAs),
			k=2,numFun=median)

imputacion<-imputacionKnn[2:7]
{par(mfrow=c(1,2),mai=rep(0.8,4))
boxplot(scale(imputacion),col=rainbow(8),main="Ouliers imputados por Knn ")
boxplot(scale(clientes[-1]),col=rainbow(9),main="Conjunto de datos original")}

k<-3
# Para table, con k=3 se identifican posibles outliers:
outs.table<-clientes$aseo[which(abs(clientes1$aseo)>k)]
outs.table1<-clientes$aseo[-which(abs(clientes1$aseo)>k)]

tableo <- data.frame(aseo = c(outs.table, outs.table1),
		     tipo = c(rep("outlier", length(outs.table)), rep("no outlier", length(outs.table1))))

ggplot(tableo,aes(x=aseo,fill=tipo))+geom_histogram(col="blue")+
	labs(title="Histograma de Gseosa con outliers",x="aseo")+
	theme_light()

# 3) ----------------------------------------------------------------------


# Selección de variables numéricas y normalización de las mismas
clientes1

# Realización del ACP normalizado (Análisis en Componentes Principales)
res.pca<-PCA(clientes1,graph = FALSE)


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
fviz_pca_biplot(res.pca,col.ind = "tomato")

##los posibles valores atipicos al realizar al ACP son los individuos 
# 184 326 182 24 252 48 93 57 212 66 87 82 66 334 86

# 4) ----------------------------------------------------------------------

# Utilice, con base en las variables originales, la detección multivariada de 
# outliers vía kNN (usted escoge k) para identificar los 20 clientes más atípicos.

clientes1
# Número de vecinos
k<-3

# Se calcula la matriz de distancias euclidianas para las coordenadas factoriales
# en las dos primeras dimensiones
matriz.de<-as.matrix(dist(coordins))

# Se almacenan las n k-distancias máximas (d_kmax) para cada observación
kdist<-1:length(coordins$PC1)

for(i in 1:length(coordins$PC1)){
	kdist[i]=(sort(matriz.de[i,]))[k-1]
}
# Detección de outliers con base en umbral L
# Umbral de detección
umbral<-as.numeric(quantile(kdist,probs = 0.95))

# Data frame con coordenadas factoriales y k-distancias máximas
df_outsknn<-bind_cols(coordins,"dist_knn"=kdist)

# Proyección de los outliers sobre primer plano factorial
ggplot(df_outsknn,aes(PC1,PC2,color=kdist>umbral))+
	geom_point(size=kdist+3)+
	geom_text(label=ifelse(kdist>umbral,nombres,""),color="black")+
	labs(title="Clientes proyectados sobre primer plano factorial",
	     x="Dimensión 1",y="Dimensión 2")+
	theme_light()
outliers_knn<-which(kdist>umbral)
# Data frame con los outliers en las variables originales
outs_multknn<-bind_cols(clientes,"dist_knn"=kdist)%>%filter(dist_knn>umbral)

OutlierDetection::nnk(clientes,k=3)
# 4.1) --------------------------------------------------------------------


# 4.2) --------------------------------------------------------------------


# 5) ----------------------------------------------------------------------
# Selección de las variables númericas de datos

clientes<-clientes[-1]
# Se aplica la detección de outliers vía LOF con k=300 vecinos
t <- proc.time() # Inicia el cronómetro
LOF<-lofactor(data=clientes, k=0.05*nrow(clientes)) 
ejecucion<-proc.time()-t # tiempo de ejecución

# Tabla con LOFs para todas las observaciones
datos1<-bind_cols(clientes,"LOFs"=LOF)

# Percentil 95 de puntajes LOF
umb1<-as.numeric(quantile(datos1$LOFs,0.95))
# 5.1) --------------------------------------------------------------------


# 5.2) --------------------------------------------------------------------


# 7) ----------------------------------------------------------------------
normalizadas<-clientes%>%
	mutate_if(is.numeric,funs(normalizar))%>%
	dplyr::select(-1)

fviz_nbclust(x = normalizadas,FUNcluster = kmeans, method="silhouette" , k.max = 15,
	     diss = dist(normalizadas,method = "euclidean"))

# Se desarrolla el k-means para k=5 centroides sobre las variables normalizadas
grupos<-kmeans(normalizadas,centers = 2,iter.max = 10000)


# Centroides de los cinco clústers (vectores 2-dimensionales)
Cks<-data.frame(grupos$centers,"clusters"=factor(1:2))
# Centroides de los cinco clústers (vectores 2-dimensionales)


# observaciones para los tres clústers
clusters<-grupos$cluster

# Distribución de los clústers
distri<-table(clusters)


# Se agregan las etiquetas de los clúster a los que pertenecen los individuos
# al conjunto datos
df.clusters<-bind_cols(clientes,"cluster"=clusters)%>%mutate(clusters=factor(cluster))




# Realización del ACP normalizado (Análisis en Componentes Principales)
res.pca<-PCA(normalizadas,graph = TRUE)
res.pca$var$coord
# Coordenadas factoriales de las observaciones
PC1 <- res.pca$ind$coord[,1] # Primer eje o dimensión
PC2 <- res.pca$ind$coord[,2] # Segundo eje o dimensión
nombres <- rownames(res.pca$ind$coord) # Asignación etiquetas observaciones
# Data frame con coordenadas factoriales de las dos primeras dimensiones
coordins<- data.frame(cbind(PC1,PC2))
grupos<-kmeans(coordins,centers = 2,iter.max = 10000,nstart = 10)
# Centroides de los cinco clústers (vectores 2-dimensionales)
Cks1<-data.frame(grupos$centers,"clusters"=factor(1:2))
# observaciones para los tres clústers
clusters1<-grupos$cluster


PCs<-coordins%>%
	mutate(clusters=factor(clusters1))
ggplot(PCs,aes(x=PC1,y= PC2,color=clusters))+
	geom_point(alpha=0.6)+
	geom_point(data=Cks1, aes(x=PC1,y=PC2,color=clusters),size = 4,pch=19,colour="black")+
	labs(title="Plano factorial con clusters y centroides del k-medioides")+theme_light()

pam_cluster <- kmeans(clientes1,centers = 2,iter.max = 10000)
pam_cluster

fviz_cluster(object = pam_cluster, data = clientes,geom = "point",
	     ellipse.type = "t",repel = TRUE)+
	theme_bw() +
	labs(title = "RESULTADOS ALGORITMO PAM")

# Data frame con centroides del k-means y variables numéricas normalizadas
tabla1<-rbind(normalizadas,Cks[,1:6])
# Matriz de distancias euclidianas
D<-as.matrix(dist(x = tabla1,method = "manhattan"))
# Únicamente distancias de las observaciones los k=5 centroides
distancias<-D[440:442,-c(440:441)]
# 20 observaciones mas alejadas de los centroides
distmax<-sort(apply(distancias,2,FUN = max),decreasing = T)[1:20]
# Selección de observaciones consideradas outliers en las variables originales
outs.kmeans<-clientes[names(distmax),]

distmax1<-apply(distancias,2,FUN = max)
tabla2<-cbind(coordins,distmax1)%>%arrange(distmax1)%>%
	mutate(tipo=factor(c(rep("normal",440-20),rep("outlier",20))))
ggplot(tabla2,aes(x=PC1,y= PC2,color=tipo))+
	geom_point(alpha=0.6)+
	geom_point(data=Cks1, aes(x=PC1,y=PC2,color=clusters),size = 4,pch=19,colour="black")+
	labs(title="Plano factorial con centroides del k-means")+theme_light()
ggplot(df_outsknn,aes(PC1,PC2,color=kdist>umbral))+
	geom_point(size=kdist+3)+
	geom_text(label=ifelse(kdist>umbral,nombres,""),color="black")+
	labs(title="Clientes proyectados sobre primer plano factorial",
	     x="Dimensión 1",y="Dimensión 2")+
	theme_light()


