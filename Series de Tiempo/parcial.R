library(readxl)
library(tseries)
library(forecast)
library(TSA)
library(MASS)
require(car)
library(lubridate)
library(dplyr)

In <- read_excel("C:/Users/Hp/Downloads/Inflación.xlsx")
attach(In)
In<-In%>%rename("Año"="Año(aaaa)-Mes(mm)","It"="Inflación total1","Ls"="Límite superior","Mi"="Meta de inflación","Li"="Límite inferior")
In<-arrange(In,Año)
attach(In)

readRDS("datos_limpios_1.RDS")

#a) 
t<-1:327
t2<-t^2
yt<-In$It
plot(t,yt,type="l")

#b) 

modelo<-lm(yt~t+t2)
#yt=25.77-0.1931t+0.0004t^2+e

#c)
abline(modelo, col = 4)

#d)
e<-residuals(modelo)
qqnorm(e) ; qqline(e)


#e)

acf(e)
pacf(e)

#la serie es una AR(p)

#f) 
#se escoge el modelo 3
#yt=25.7762-0.1931t+0.0004t^2+1.4935e_(t-1)-0.5285e_(t-2)

ar1<-arima(yt, order = c(1,0,0)) ; ar1
pr<-predict(ar1, 3) ; pr

# limites de confianza
li<-pr$pred-1.96*pr$se ; li
ls<-pr$pred+1.96*pr$se ; ls

data.frame(li,pr$pred,ls)
