library(readxl)
Sparrows <- data.frame(read_excel("Sparrows.xlsx"))
head(Sparrows)

# Aves que sobrevivieron
X1 = subset(Sparrows, subset = Sobrevivieron == "SI", select = 2:6) ; X1

# Aves que NO sobrevivieron
X2 = subset(Sparrows, subset = Sobrevivieron == "NO", select = 2:6) ; X2

require(MVN)
mvn(X1, mvnTest = "mardia", multivariatePlot = "qq" , multivariateOutlierMethod = "quan")
mvn(X1, mvnTest = "energy", multivariatePlot = "qq" , multivariateOutlierMethod = "quan")

##no hay evidencia estadistica para rechazar la normalidas
#

mvn(X2, mvnTest = "mardia", multivariatePlot = "qq" , multivariateOutlierMethod = "quan")

# Prueba de igualdad de matrices de covarianza
##primero se ve homogeneidad de varianzas
require(covTestR)


##la ventaja de la prueba de scot funciona cuando hay pocos datos y muchas variables
##estima de forma robusta la matriz de covarianzas

# cuando hay muchos individuos funcina mejor covTest="Ahmad2017"

homogeneityCovariances(Sparrows[,-1], group = Sobrevivieron, covTest = "Schott2007")

## exixte suficiente evidencia para rechazar la normalidad
# es decir la matriz de covarianzas son diferentes

S1<- cov(X1)
S2<- cov(X2)
S1;S2

n1 <- nrow(X1);n2 <- nrow(X2)
hc <- 0
for (i in  1:1000){
	
	m1 <- sample(1:n1,n1,replace = T)
	m2 <- sample(1:n2,n2,replace = T)
	X1s <- X1[m1,]
	X2s <- X2[m2,]
	datos <- cbind(rbind(X1s,X2s),sob=Sparrows$Sobrevivieron)
	hc[i]<-homogeneityCovariances(datos, group =sob, covTest = "Schott2007")$statistic
	
	
}
plot(d<-density(hc,from=0))
abline(v=1.7202e+20)
d$x
d$y

den <- approxfun(d$x,d$y)

den(0.0002)

integrate(den, 1.7202e+20,max(d$x))
#en terminos de varianza y covarianza 
# la varianza se queda corta

cor(X1);cor(X2)


##prueba de hipotesis para comparar los vectores de medias
# Asumiendo igualdad de matrices de covarianzas

require(Hotelling)
T2 = hotelling.test(X1, X2); T2

# el argumento shrinkage alternativa cuando se tienen muchas variables

T2 = hotelling.test(X1, X2, shrinkage = T); T2 # muchas variables

## si los datos no cumplen los supuestos la distribución del estadistico no es la que nos dicen
#si los datos no son normales entonces se utilizan p valores no parametricos


T2 = hotelling.test(X1, X2, shrinkage = T, perm = T); T2 # muchas variables y p-valor no param?trico

# no es necesario verificar los supuestos
# la matriz de covarianzas es para comprobar si estructura difiere

#la media de la estructura de las aves no estadisticamente diferente perso si su estructura de variabilidad

