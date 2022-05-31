data("JohnsonJohnson")
View(EuStockMarkets)
## que tipo de objeto es
is(JohnsonJohnson)
x<-JohnsonJohnson
##muestra start(año,)desede que mes quiero comenzar la frecuencia es mensual
ts(x,start=c(1960,2),frequency = 12)
plot(x)
##Autocorrelacion decrece lentamente
acf(x)
##diff es para eliminar la tendencia de la serie
a<-diff(x,1)
plot(a)
##al eliminar la tendencia decae mas rapido
acf(a)
