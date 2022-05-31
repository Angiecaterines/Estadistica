require(FactoMineR)
require(factoextra)

# Ejemplo 1
head(iris)
plot(iris,col=iris$Species)
summary(iris)
boxplot(iris[,-5])
boxplot(iris$Sepal.Length ~ iris$Species)
boxplot(iris$Sepal.Width ~ iris$Species)
boxplot(iris$Petal.Length ~ iris$Species)
boxplot(iris$Petal.Width ~ iris$Species)

cor(iris[,-5])
##cualitativa suplementaria par alas variables categoricas
##cuando se tiene mas variables cualitativas se elimnan y se analiza una por una

# componenetes principales con ilustrativas cualitativas
# Analisis de correspondencia
# Analisis mixto de componentes principales
acp = PCA(iris, scale.unit = T, quali.sup = 5, graph = F)
acp$eig
##se escribe cuanto porcentje explican cada componenete 
##
#Se hara analisis en componentes principales
# El primero explica tanto
# Luego se examinan las coordenadas

acp$var$coord
##cuanto contribuye cada variable

acp$var$contrib
acp$var$cos2
acp$ind$coord
acp$ind$contrib

acp$quali.sup$coord
acp$quali.sup$cos2

fviz_pca_var(acp)
fviz_pca_ind(acp)

fviz_pca_biplot(acp, habillage = 5)

x = subset(iris, subset = (Species == "setosa" | Species == "versicolor"))
x = subset(iris, subset = (Species == "setosa" | Species == "virginica"))
x = subset(iris, subset = (Species == "virginica" | Species == "versicolor"))

t.test(x$Petal.Width~x$Species)
t.test(x$Sepal.Width~x$Species)

# Ejemplo2

USArrests
plot(USArrests)
cor(USArrests)

summary(USArrests)
boxplot(USArrests[,4])

acp = PCA(USArrests, scale.unit = T, quanti.sup = 3, graph = F)
acp$eig
acp$var$coord
acp$var$contrib
acp$ind$coord
cor(acp$ind$coord[,1:2],USArrests$UrbanPop)

fviz_pca_var(acp)
fviz_pca_ind(acp)

fviz_pca_biplot(acp, col.ind = USArrests[,3])

# Ejemplo3

summary(Theoph[,-1])
boxplot(Theoph[,5])
plot(Theoph[,-1])

cor(Theoph[,-1])

acp = PCA(Theoph, scale.unit = T, quali.sup = 1, graph = F)
acp$eig
acp$var$coord
acp$var$contrib

fviz_pca_var(acp)
fviz_pca_ind(acp)

fviz_pca_biplot(acp, habillage = 1)


