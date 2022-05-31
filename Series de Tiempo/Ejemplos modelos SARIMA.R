require(TSA) # acf pacf
require(forecast) 
require(tseries) # Acf Pacf

x = rnorm(100,10,1)
z = ts(x, start = c(10, 3), frequency = 7) ; z
plot(z)

# Ejemplo 1
a<-AirPassengers
plot(AirPassengers)
lam = BoxCox.lambda(AirPassengers)
#z = car::bcPower(AirPassengers, lambda = lam)
z = log(AirPassengers)
plot(z)

plot(decompose(AirPassengers))
plot(decompose(z))

adf.test(z)

y = diff(z, differences = 1)


plot(y)
w = diff(z, lag = 12, differences = 1)
plot(w)

var(y) ; var(w) ; var(diff(y, lag = 12))

acf(z)

acf(diff(y, lag = 12))
pacf(diff(y, lag = 12))
eacf(diff(y, lag = 12))

mod = auto.arima(z) ; mod

pro = forecast(mod, h = 12) ; pro
plot(pro)

mod = auto.arima(AirPassengers, lambda = lam) ; mod
pro = forecast(mod, h = 12) ; pro
plot(pro)

a = mod$residuals ; a
plot(a)
acf(a)
qqnorm(a) ; qqline(a)
shapiro.test(a)

# Ejemplo 2
JohnsonJohnson
Jhon<-ts(JohnsonJohnson[1:76],
         start = c(1960,1), frequency = 4 )

plot(JohnsonJohnson)
lam = BoxCox.lambda(JohnsonJohnson)
#z = car::bcPower(AirPassengers, lambda = lam)
z = log(JohnsonJohnson)
ggplot(z)
plot(z)

plot(decompose(JohnsonJohnson))
plot(decompose(z))

adf.test(z)

y = diff(z, differences = 1)
plot(y)
w = diff(z, lag = 4, differences = 1)
plot(w)

var(y) ; var(w) ; var(diff(y, lag = 12))

acf(z)

acf(diff(y, lag = 12))
pacf(diff(y, lag = 12))
eacf(diff(y, lag = 12))

mod = auto.arima(z) ; mod

pro = forecast(mod, h = 12) ; pro
plot(pro)

mod = auto.arima(AirPassengers, lambda = lam) ; mod
pro = forecast(mod, h = 12) ; pro
plot(pro)

a = mod$residuals ; a
plot(a)
acf(a)
qqnorm(a) ; qqline(a)
shapiro.test(a)


Jhon<-ts(JohnsonJohnson[1:76],
         start = c(1960,1), frequency = 4 )

plot(Jhon)
lam = BoxCox.lambda(Jhon)
#z = car::bcPower(AirPassengers, lambda = lam)
z = log(Jhon)
plot(z)

plot(decompose(Jhon))
plot(decompose(z))

adf.test(z)

y = diff(z, differences = 1)
plot(y)
w = diff(z, lag = 4, differences = 1)
plot(w)

var(y) ; var(w) ; var(diff(y, lag = 4))



acf(w)
pacf(w)
eacf(w)

mod1 = auto.arima(Jhon,lambda = 0,allowdrift = F) ; mod
##en h es el periodo de predicción
pro1 = forecast(mod1, h = 8) ; pro
plot(pro1)
lines(JohnsonJohnson,col=1,lty=2)

mod2 = auto.arima(Jhon,max.Q = 0,max.P = 0,lambda = 0,allowdrift = F) ; mod
##en h es el periodo de predicción
pro2 = forecast(mod2, h = 8) ; pro
plot(pro2)
lines(JohnsonJohnson,col=1,lty=2)
##Si las predicciones son muy parecidas utilizamos el intervalo mas corto

#comparación longitudes de predicción
pro1$upper[,1]-pro1$lower[,1]
pro2$upper[,1]-pro2$lower[,1]

#la primra predicción tiene el intervalo mas pequeño

##comparación de pronosticos vs reales
e<-pro1$mean - JohnsonJohnson[length(JohnsonJohnson)-7:]
var(e)
sum(abs(e))
a = mod1$residuals ; a
plot(a)
acf(a)
qqnorm(a) ; qqline(a)
shapiro.test(a)

##resumiendo
#SARIMA4(0,1,1)(1,1,0)4:cumple supuesto,I.P menos amplio
#SARIMA4(1,0,1)(0,1,0)4:Predicción menos sesgada,no cumple supuestos,I.P mas amplio

#modelo seleccionado


# Ejemplo 3


?USAccDeaths
plot(USAccDeaths)
ggplot(USAccDeaths,aes(x=c(1973:1978,y=USAccDeaths)))+geom_line()
lam<-BoxCox.lambda(USAccDeaths)
z<-log(USAccDeaths)
plot(z)
plot(decompose(USAccDeaths))
plot(decompose(z))

adf.test(z)
#la menor varianza esta para la serie diferenciada estacionalmente 1 vez

y<-diff(z,lag = 12,differences = 1)
w<-diff(z,differences = 1)
ya<-diff(z,differences = 2)
ya1<-diff(z,lag=2,differences = 2)
var(y);var(w);var(ya);var(ya1)

acf(y)
pacf(y)

eacf(y)

mod1 = auto.arima(z,lambda =lam,allowdrift = F) ; mod
pro1<-forecast(mod1, h = 6) ; pro
plot(pro1)

lines(USAccDeaths,col=3,lty=7)
##Si las predicciones son muy parecidas utilizamos el intervalo mas corto

a = mod1$residuals ; a
plot(a)
acf(a)
qqnorm(a) ; qqline(a)
shapiro.test(a)

mod2 = auto.arima(USAccDeaths , lambda = lam,allowdrift = F) ; mod2

###obligar al modelo a no diferenciarse 1 vez,si quiere que utilize logaritmo pongo lambda cero

mod2 = auto.arima(USAccDeaths , lambda = 0,allowdrift = F) ; mod2

pro2 = forecast(mod2, h = 6) ; pro2
lines(USAccDeaths,col=1,lty=2)
plot(pro2,title="Muertes por accidente SARIMA(0,1,1)(0,1,1)12")

#comparación longitudes de predicción
pro1$upper[,1]-pro1$lower[,1]
pro2$upper[,1]-pro2$lower[,1]
a = mod2$residuals ; a
plot(a)
acf(a)
qqnorm(a) ; qqline(a)
shapiro.test(a)
