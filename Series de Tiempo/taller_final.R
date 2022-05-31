library(readxl)
library(dplyr)
require(TSA)
require(forecast)
require(tseries)
require(ggplot2)
require(car)
IPVN <- read_excel("C:/Users/Hp/Downloads/IPVN.xlsx")
IPVN<-IPVN[-c(1:8,205:214),4]
names(IPVN)<-c("Indice de precio vivienda nueva")

# names(IPVN)<-c("Fechas","Bogotá")
# a<-seq(as.Date('2004-01-28'), as.Date('2020-04-28'), by = "month")
# IPVN$Fechas<-a
IPVN<-ts(IPVN,start = c(2004,01,28),end = c(2020,04,28),frequency = 12)
plot(IPVN)
lam = BoxCox.lambda(IPVN)
z<-((IPVN^lam)-1)/lam
z<-car::bcPower(IPVN, lambda = lam)
plot(z)
plot(decompose(IPVN))
plot(decompose(z))

adf.test(z)

##diferenciada 1 vez
y<-diff(z, differences = 1)

#diferenciada estacionalmente 1 vez

w<-diff(z, lag = 12, differences = 1)
#diferenciada 1 vez y estacionalmente

x<-diff(y,lag=12)


var(y) ; var(w) ; var(x);var(diff(z,differences = 2));var(diff(z, lag = 12, differences = 2))

acf(y)
pacf(y)
eacf(y)

mod1 = auto.arima(IPVN, lambda = lam) ; mod1
pro1 = forecast(mod1, h = 8) ; pro1
plot(pro1)

a = mod1$residuals ; a
plot(a)
acf(a)
qqnorm(a) ; qqline(a)
shapiro.test(a)


##series con derivada
# mod2 = auto.arima(IPVN, lambda = lam,max.d = 1) ; mod2
# pro2 = forecast(mod2, h = 8) ; pro
# plot(pro2)
# 
# a = mod$residuals ; a
# plot(a)
# acf(a)
# qqnorm(a) ; qqline(a)
# shapiro.test(a)
# 
# pro1$upper[,1]-pro1$lower[,1]
# pro2$upper[,1]-pro2$lower[,1]

# a -----------------------------------------------------------------------




library(readxl)
talleres <- read_excel("~/EL BOSQUE/2020-1/Diseño/talleres.xlsx")
a<-talleres[2:7,2:10]
as.numeric(as.vector(as.matrix(a)))
b<-talleres[12:17,2:10]
as.numeric(as.vector(as.matrix(b)))
c<-talleres[2:13,14:15]
as.numeric(as.vector(as.matrix(c)))
d<-talleres[2:10,19:22]
as.numeric(as.vector(as.matrix(d)))
