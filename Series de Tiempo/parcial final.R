library(readxl)
cafe <- read_excel("C:/Users/Hp/Downloads/Cafe.xlsx")
cafe<-cafe[673:910,]
cafe<-ts(cafe,start = c(2000,01,01),end = c(2019,10,01),frequency = 12)


##Precio interno

pi<-cafe[,2]
plot(pi)
lam<-BoxCox.lambda(pi)
z<-car::bcPower(pi, lambda = lam)
adf.test(z)
mod1 = auto.arima(pi,lambda = 0) ; mod1
pro1 = forecast(mod1, h = 6) ; pro1
plot(pro1)

a = mod1$residuals ; a
plot(a)
acf(a)
qqnorm(a) ; qqline(a)
shapiro.test(a)


##Precio externo

pe<-cafe[,3]
plot(pe)
lam<-BoxCox.lambda(pe)
z<-car::bcPower(pe, lambda = lam)
adf.test(z)

mod1 = auto.arima(pe, lambda = lam,max.P = 1,max.Q = 1,max.p =1 ) ; mod1
pro1 = forecast(mod1, h = 6) ; pro1
plot(pro1)

a = mod1$residuals ; a
plot(a)
acf(a)
qqnorm(a) ; qqline(a)
shapiro.test(a)



##Producción

p<-cafe[,4]
plot(p)
lam<-BoxCox.lambda(p)
z<-car::bcPower(p, lambda = lam)
adf.test(z)

mod1 = auto.arima(p, lambda = lam,allowdrift = F,max.P = 1,max.Q = 1,max.p = 1) ; mod1
pro1 = forecast(mod1, h = 6) ; pro1
plot(pro1)

a = mod1$residuals ; a
plot(a)
acf(a)
qqnorm(a) ; qqline(a)
shapiro.test(a)

