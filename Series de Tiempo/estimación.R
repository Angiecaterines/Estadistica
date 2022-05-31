##ejemplo de ajuste de un modelo Arima(p,d,q)

#serie de datos

BJsales

##Analisis exploratorio de datos
#(EDA)
plot(BJsales)
boxplot(BJsales)
summary(BJsales)
library(car)
#función para mirar si tiene problemas de variabilidad

lam<-powerTransform(BJsales)$lambda

#transforma la serie
x<-bcPower(BJsales,lam)
x<-log(BJsales)

##orden di difenciación

var(x)
var(diff(x))
var(diff(x,2))

plot(diff(x))
#Nos quedamos con orden de diff=1 porque tiene menor varianza

#Identificar los posibles modelos
requiere(TSA)
z<-diff(x)
acf(z)
pacf(z)
#1er posible modelo es AR(2) o AR(1)
eacf(z)

#2do posible modelo ARMA(1,1)
library(ggplot2)
#Estimación del modelo
#ARIMA(2,1,0);ARIMA(1,1,0) ARIMA(1,1,1)
modelo1<-arima(x,order = c(2,1,0))
modelo2<-arima(x,order = c(1,1,0))
modelo3<-arima(x,order = c(1,1,1))
e1<-residuals(modelo1)
e2<-residuals(modelo2)
e3<-residuals(modelo3)

qqnorm(e1-(max(e1)));qqline(e1-(max(e1)))
qqnorm(e2)
qqnorm(e3)


#comprarar el AIC para mirar el mejor modelo

#Intervalos de confianza del 95% para los parametros
li<-modelo1$coef[1]-1.96*sqrt(modelo1$var.coef[1,1])
ls<-modelo1$coef[1]+1.96*sqrt(modelo1$var.coef[1,1])
cbind(li,ls)

#el intervalo no contienenal cero por lo tanto es estadisticamente diferente

li<-modelo1$coef[2]-1.96*sqrt(modelo1$var.coef[1,1])
ls<-modelo1$coef[2]+1.96*sqrt(modelo1$var.coef[1,1])
cbind(li,ls)

modelo2<-arima(x,order = c(1,1,0))
modelo3<-arima(x,order = c(1,1,1))
