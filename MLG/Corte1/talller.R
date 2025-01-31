
# Librerias ---------------------------------------------------------------

library(MASS)
library(DescTools)


# Datos -------------------------------------------------------------------
datos_taller <- read.table("~/EL BOSQUE/2022-1/MLG/Corte1/datos_taller.txt", quote="\"", comment.char="")

datos_taller <- datos_taller[,-1]

names(datos_taller)<-c("visitas","ingreso","actitud","importancia","tama�o","edad")

datos_taller$visitas<- ifelse(datos_taller$visitas==1,0,1)

datos_taller$actitud <- as.factor(datos_taller$actitud)
datos_taller$importancia <- as.factor(datos_taller$importancia)

# Modelo 1 ----------------------------------------------------------------



modelo1 <-glm(visitas~ ingreso + actitud + importancia + tama�o + edad,
              data=datos_taller,family=binomial)
summary(modelo1)


# Selecci�n del mejor modelo ----------------------------------------------

stepAIC(modelo1, direction = "both")

modelo2<-glm(formula = visitas ~ ingreso + tama�o + edad, 
             data = datos_taller,family = binomial)
a<-summary(modelo2)


# Deviance ----------------------------------------------------------------

1-pchisq(a$null.deviance - a$deviance,a$df.null- a$df.residual)

# se rechaza Ho.Existe evidencia estadistica de que el modelo
# de regresi�n logistico binario se ajusta a los datos

# Pseudo R^2 --------------------------------------------------------------

PseudoR2(modelo2,"Nagelkerke")

# Residuales --------------------------------------------------------------

table(residuals(modelo2,type="pearson")>2)
plot(residuals(modelo2,type="pearson"))
plot(residuals(modelo2,type="pearson"))

# existe presencia de 1 dato atipico en el modelo