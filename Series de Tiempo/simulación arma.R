
# Simulaci?n de series arma

requiere(TSA)
##Zt=mu+phi1+zt-1+at,mu=0, sa2=1

ar1 = arima.sim(list(ar = 0.4), n = 100, sd = 1)
ar1<-ts(ar1,start = c(2000,3),frequency = 12)
plot(ar1)
acf(ar1)
pacf(ar1)

##Zt=mu+phi1*zt.1+phi2*zt.2+at,mu=0, sa2=1

ar2 = arima.sim(list(ar = c(0.4,0.3)), n = 100, sd = 1)
plot(ar2)
acf(ar2)
pacf(ar2)

##Zt=mu+ at+the1*.1 ,mu=0 ,sa2=1

ma1 = arima.sim(list(ma = -0.4), n = 1000, sd = 1)
plot(ma1)
acf(ma1)
pacf(ma1)

##Zt=mu+ at+the1*.1 +the2*a2.2,mu=0 ,sa2=1
ma2 = arima.sim(list(ma = c(-0.4,0.4)), n = 100, sd = 1)
plot(ma2)
acf(ma2)
pacf(ma2)

#zt=mu +phi*zt.1+at+th1*at.1 mu=0 ,sa2=1
arma11 = arima.sim(list(ar = 0.8, ma = c(-0.2)),
              n = 100000, sd = 1)
plot(arma11)
acf(arma11)
pacf(arma11)

eacf(yt,ar.max = 3, ma.max = 3)
