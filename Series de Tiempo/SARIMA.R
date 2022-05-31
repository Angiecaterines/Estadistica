library(tseries)
library(forecast)
library(tseries)
library(TSA)
##Estacional

AirPassengers
BoxCox.lambda(AirPassengers)

z<-log(AirPassengers)
plot(z)

plot(decompose(z))
adf.test(z)
y<-diff(z)
plot(y)

w<-diff(z,lag=12)
plot(w)
#la serie debe ser diferencada 
var(y);var(w);var(diff(y,lag = 12))
acf(diff(y,lag = 12))
pacf(diff(y,lag = 12))
eacf(diff(y,lag = 12))
auto.arima(z)

z<-car::bcPower(AirPassengers)
