# Ejemplo escalaf?n competitividad 2017

library(readxl)
Competitividad <- data.frame(read_excel("~/EL BOSQUE/2020-2/Estadística multivariada/Competitividad.xlsx"))
head(Competitividad)

datos = Competitividad[,c(3:7)]
rownames(datos) = unclass(Competitividad$Dpto)

# Descriptivas
summary(datos)
boxplot(datos)

#  Normalidad multivariada
require(MVN)
nm = mvn(datos, mvnTest = "mardia",
         multivariatePlot = "qq", 
         multivariateOutlierMethod = "quan",
         showOutliers = T)$multivariateNormality
nm

require(corrplot)
corrplot(cor(datos), type="upper")#, order="hclust", tl.col="black", tl.srt=45)

require(PerformanceAnalytics)
chart.Correlation(datos, histogram=TRUE, pch=19)


##Wsiempre hay que hacer un analisis descriptivo

# A mano
m = apply(datos, 2, mean) ; m
datos_est = scale(datos) ; datos_est

##coeficiente de variación para mirar variabilidad promedio

c(18.08323, 19.86728, 21.61868, 20.51381, 12.11221 )/c(48.62500 ,51.29062, 56.70312, 25.53125 ,60.86250)*100

# cv=sd(datos)/mean(datos)
#S = var(datos) ; S

S = var(datos) ; S ; S=cor(datos)
vp = eigen(S)
lam = vp$values ; lam
e = vp$vectors ; e


# componentes
 #Y = t(t(e)%*%t(datos)) ; Y
Y = t(t(e)%*%t(datos_est)) ; Y

# proporci?n de varianza explicada
lam/sum(lam)

# correlaciones
cor = t(apply(e,1,function(x) x*sqrt(lam)))*diag(1/sqrt(S))
cor

# con librer?as
require(FactoMineR)
require(factoextra)
res.pca = PCA(datos, scale.unit=TRUE, graph=T)
res.pca$eig
res.pca$var$coord
res.pca$var$cos
res.pca$var$contrib
res.pca$ind$coord
res.pca$ind$contrib
fviz_screeplot(res.pca, ncp=10)
plot.PCA(res.pca, axes = c(1,2), choix="var")
plot.PCA(res.pca, axes = c(1,2), choix="ind")
fviz_pca_biplot(res.pca, axes = c(1, 2), repel = TRUE)

