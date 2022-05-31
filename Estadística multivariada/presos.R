require(tidyverse)
require(FactoMineR)
require(factoextra)

arreglo<-array(c(2.7,1.9,4.3,23.1,24.4,26.1,45.8,
		 0.2,7.7,3.8,18.7,24.2,18.5,58.9,
		 1.5,3.4,16.8,34.3,47.7,45.3,67.1,
		 1.4,2.8,7.7,44.2,44.3,43.2,69.6,
		 1.0,2.5,9.1,31.9,63.5,48.4,72.4,
		 1.3,2.4,6.7,32.5,42.5,38.8,70.1),
		 dim=c(7,7),dimnames=list(c("Homicidio",
		 "Violación/AS","Robo",
		 "Asalto","Propiedad","Drogas","Orden público"),
		 c("Homicidio1",
		   "Violación/AS1","Robo1",
		   "Asalto1","Propiedad1","Drogas1","Orden público1")))
												
tabla<-as.table(arreglo)
names(attributes(tabla)$dimnames) <- c("Delitos antes 2005","Delitos posteriores 2005")	 							  
colSums(tabla)	 							  
rowSums(tabla)

chisq.test(tabla, simulate.p.value = T, B = 1000) 

#Con un nivel de significancia mayor a alpha no se rechaza ho, es decir  existe
# asociación entre los delitos antes de 2005 y los delitos posteriores a 2005


ac = CA(tabla, graph = T)
round(ac$eig,3)

# A partir de los valores propios del AC, con los dos primeros
# ya se obtiene un 78.04% de la variabilidad (o de la información
# de la chi-cuadrado.


ac$row
# Para la primera dimensión los delitos antes de 2005 que mas contribuyen
# son Violaciones/Asalto sexual y delitos de orden público.
# En cuanto a la segunda dimensión los delitos posteriores de 2005 que 
# más contribuyen son Homicidio,robo y asalto

ac$col

# En la primera dimensión las violaciones/asalto sexual 
# contribuyen en un 82% siento el delito de mayor contribución en cuento a la 
# segunda dimensión el homicio y los delitos de orden publico son las mayores contribuciones



fviz_screeplot(ac, addlabels = TRUE)

fviz_ca_biplot(ac, repel = TRUE)
fila<-ac$row$contrib
columna<-ac$col$contrib
cbind(fila[,c(1,2)],columna[,c(1,2)])

# De acuerdo a las contribuciones de cada categoria se observa que los delitos antes de
# 2005 que mas contribuyen a la primera dimension son Violaciones/Asaltos sexuales y delitos 
# de orden público del mismo modo el delito que más contribuyen posterior al 2005 es
# Violaciones/Asalto sexual.

# Para la segunda dimensión se presenta mayor contribución el los delitos de Homicidio, Robo, 
# antes de 2005 y para los delitos Homicidio, Robo, Propiedad y Orden púbico
# posterior al 2005

# Se espera que si un recluso cometio Homicidio 9 años despues de su liberación, haya cometido homicidio antes del 2005
# Se observa asociación  entre Violaciones/Asalto sexual posterior al 2005 y Violaciones/Asalto sexual antes del 2005
# Se espera que exista reincidencia de robos si antes de 2005 se cometio robo
# Se espera que un recluso que ingrese a prisión por propiedad tambien haya cometido robo y sea reincidente en  delitos de robo y propiedad antes del 2005

# Si un recluso fue detenido antes de 2005 por drogas posiblemente tambien haya cometido asalto
