AirPassengers
plot(AirPassengers)
x<-1:144
z<-as.vector(AirPassengers)
plot(x,z,type="l")
mod<-lm(z~x)
abline(mod,col=4)

##variabilidad
w<-log(z)
plot(x,w,type="l")

y<-diff(w,lag=1,difference=1)
plot(x[-1],y,type="l")


##si se quisiera quitar la estacionalidad mensual
y<-diff(w,lag=12,difference=1)
plot(x[-(1:12)],y,type="l")

(1-B^12)zt

v<-diff(y,lag=12,differences = 1)
plot(ts(v))

library(car)
###para hacer la transformación de la varianza
lam=powerTransform(z)$lambda
##se transforma la serie con el lambda
y<-bcPower(z,lambda = lam)
plot(ts(y))

d<-bcnPowerInverse(y,lam,gamma=1)
plot(ts(d))

