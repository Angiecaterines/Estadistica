library(tseries)
library(forecast)
library(TSA)
library(readxl)
library(MASS)
require(car)

Casos <- read_excel("Casos1 (1).xlsx")
head(Casos)

#Casos$`Fecha de diagn?stico` = as.POSIXct(Casos$`Fecha de diagn?stico`,tryFormats ="%d/%m/%Y")
# n?mero de casos diarios
x = as.numeric(as.ts(table(Casos$`Fecha de diagnóstico`)))
# rellen? con casi 0 los d?as sin casos, 
# la serie debe ser > 0
y = c(x[1],.1,.1,x[2],.1,x[-(1:2)], 72, 92)
# la convierto en serie de tiempo
y = ts(y,start = c(2020,3,6), frequency = 366)
plot(y)

w = 1:length(y)
mod = lm(y~w) ; mod
summary(mod)

e = mod$residuals
qqnorm(e) ; qqline(e)
acf(e)
pacf(e)

# modelos de regresi?n con errores autocorrela...

# tiene problemas de variabilidad y busco una 
# transformaci?n boxcox
lambda = powerTransform(y)$lambda ; lambda
# ((y^lam) - 1) /lam ; log(y) si lam = 0
# los transformo
z = bcPower(y, lambda) #sqrt(y)
plot(z)
# los datos tienen tendencia entonces diferencio
# una vez la serie
# Wt = Zt - Zt.1
plot(diff(z))

# acf, pacf y eacf para identificar el modelo
acf(diff(z))
pacf(diff(z))
eacf(diff(z),ar.max = 3, ma.max = 3)

# posibles modelos MA(1), ARMA(1,1)
# pero tambi?n se prueba un AR(1) pero no funciona

ma1 = arima(z, order = c(0,1,1)) ; ma1
acf(ma1$residuals)
qqnorm(ma1$residuals)
qqline(ma1$residuals)

arma = arima(z, order = c(1,1,1)) ; arma
acf(arma$residuals)
qqnorm(arma$residuals)
qqline(arma$residuals)

# similares pero nos quedamos con el MA(1) por
# tener menos par?metros

# predicci?n
pr = predict(ma1, 3) ; pr

# l?mites de confianza
li = pr$pred-1.96*pr$se ; li
ls = pr$pred+1.96*pr$se ; ls

# transformada inversa a la predicci?n y l?mites
yp = (lambda*pr$pred+1)^(1/lambda) #bcnPowerInverse(pr$pred, lambda, 0)
yi = (lambda*li+1)^(1/lambda) #bcnPowerInverse(li, lambda, 0)
ys = (lambda*ls+1)^(1/lambda) #bcnPowerInverse(ls, lambda, 0)
pred = data.frame(yi,yp,ys) ; pred

# gr?fico
plot(y, xlim = c(2020,2020.07), ylim = c(0,120))
lines(c(2020.055,2020.060,2020.065), pred[,2], type = "b", col = 4, lty = 1, pch=20)
lines(c(2020.055,2020.060,2020.065), pred[,1], type = "b", col = 2, lty = 1, pch=20)
lines(c(2020.055,2020.060,2020.065), pred[,3], type = "b", col = 2, lty = 1, pch=20)

