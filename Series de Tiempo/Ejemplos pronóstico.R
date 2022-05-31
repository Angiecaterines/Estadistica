# Ejemplo: n?mero de usuarios conectados a internet en un
# servidor por minuto

require(TSA)
require(forecast)
require(tseries)
require(ggplot2)
require(car)

##ususarios de internet en un servidor prarticular

WWWusage
plot(WWWusage)

powerTransform(WWWusage)

# Se debe transformar la serie, lambda = 0

x = log(WWWusage)
plot(x)
##preube p tseries 
# ho:no estacionario
# ha:estacionaria
# test de Dickey-Fuller de estacionariedad
adf.test(x)
# la series no es estacionaria
var(x)

plot(diff(x))
var(diff(x))

plot(diff(x,differences = 2))
var(diff(x, differences = 2))

plot(diff(x,differences = 3))
var(diff(x, differences = 3))

# es necesario diferenciar 2 veces la serie

acf(diff(x,differences = 1),lag.max = 100)
pacf(diff(x,differences = 1),lag.max = 100)

#se miran siempre los circulos en eacf
eacf(diff(x,differences = 1))

# Posibles modelos ARIMA(3,1,0) ARIMA(1,1,2)

mod = arima(x,c(3,1,0)); mod
p = forecast(mod, h=10) ; p
plot(p)

##creerlo a autoarima para el orden de diferenciación
#se verifican modelos,siempre buscar el modelo parsimonioso
#la idea es quedarnos con el intervalo mas pequeño

# De acuerdo al criterio AIC
mod = auto.arima(x)
mod
##dice el orden ar y ma y cuantas veces debemos diferenciar
p = forecast(mod, h=5) ; p
plot(p)

# Ejemplo
# global annual temperature anomalies (t.global) provided by 
# the Berkeley Earth Surface Temperature Study. 
# Temperatures are given in Celsius and are reported as 
# anomalies relative to the period January 1951 to December
# 1980 average.

load(url("https://userpage.fu-berlin.de/soga/300/30100_data_sets/Earth_Surface_Temperature.RData"))
library(xts)
t.global  <- apply.yearly(t.global, mean)
t.global
temp.global  <- ts(t.global["1850/2000", 'Monthly.Anomaly.Global'])
plot(temp.global)

powerTransform(temp.global) # error, serie negativa
BoxCox.lambda(temp.global) # pack. forecast

# No es necesario transformar la serie, lambda ~ 1

# test de Dickey-Fuller de estacionariedad
adf.test(temp.global)

var(temp.global)

plot(diff(temp.global))
var(diff(temp.global))

plot(diff(temp.global,differences = 2))
var(diff(temp.global, differences = 2))

# es necesario diferenciar 1 vez la serie

acf(diff(temp.global,differences = 1))
pacf(diff(temp.global,differences = 1))
eacf(diff(temp.global,differences = 1))

# Posibles modelos ARIMA(0,1,2) ARIMA(1,1,1) 
# ARIMA(3,1,0) ARIMA(0,1,2)

# De acuerdo al criterio AIC
mod = auto.arima(temp.global)
mod

# debemos quitar el drift
mod = auto.arima(temp.global, allowdrift = F)
mod

p = forecast(mod, h=10) ; p
plot(p)

# residuales del modelo
e = resid(mod)
plot(e)
acf(e)
qqnorm(e) ; qqline(e)

