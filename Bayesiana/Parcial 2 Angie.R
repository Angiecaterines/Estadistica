
# Parcial 2 Estadística  Bayesiana ----------------------------------------


library(bayestestR)#intervalo y pruebas de hipotesis
library(dplyr)
library(ggplot2)
library(readxl)
library(mcmc)
library(MASS)
library(gibbs.met)


# datos -------------------------------------------------------------------


Pro <- read_excel("C:/Users/Hp/Downloads/Pronóstico.xlsx")
View(Pro)



# 1) Punto ----------------------------------------------------------------

# Encuentre la siguiente distribución de probabilidad a priori conjugasa de b
# y encuentre la distribución de probabiliad conjugada de b.

# Distribución a posteriori ----------------------------------


pos1 = function(the, y){
  mu = the[1] ; b = the[2]
  n = length(y)
  mu<-mean(y)
  b <-(1/n) *sum(abs(y-mu))
  a<-2.963
  B<-0.456
  log(1/(2*b))- (abs(y-mu)/b) + a*log(1/b)-B/b
  ifelse(b>0,log(1/(2*b))-(abs(y-mu)/b) + a*log(1/b)-B/b,-Inf)
}


# 2) Punto ----------------------------------------------------------------

# Asuma a=2.963 y beta=0.4556 junto con algún muestreador para obtener la
# media posterior, la mediana posterior y la moda posterior de E(X)

# Metodo monte carlo ----------------------------------------------------


met = metrop(pos1,c(2.963 ,0.456) , 10000, blen = 100,y=Pro$Viento)

plot.ts(met$batch)
plot(density(met$batch[,1]))
plot(density(met$batch[,2]))


# Metodo gibbs ------------------------------------------------------------



require(gibbs.met)
gib = gibbs_met(log_f = pos1 , no_var = 2, 
                ini_value = c(2.963 ,0.456), 
                stepsizes_met=c(0.01,0.01),iters=10000, iters_met=2, y =Pro$Viento )

plot.ts(gib)
plot(density(gib[-(1:5000),1]))
plot(density(gib[-(1:5000),2]))


posterior = met$batch[-(1:5000),2]

plot(d<-density(posterior))
fd = approxfun(d$x,d$y)

# Mediana posterior
integrate(fd, min(d$x), 146)$value

# Esperanza posterior
fd2 = approxfun(d$x,d$x*d$y)
integrate(fd2, min(d$x), max(d$x))$value

# Moda posterior
d$x[d$y==max(d$y)]


# 3) Punto ----------------------------------------------------------------

# Obtenga y grafique un intervalo de credibilidad del 89% para E(X)

# intervalo  89% de credibilidad

ci_hdi <- ci(posterior, method = "HDI",ci = 0.89) # más alta densidad
ci_eti <- ci(posterior, method = "ETI",ci=0.89) # igual probabilidad "antes" y "despues" del intervalo

segments(ci_hdi$CI_low, 0, ci_hdi$CI_low, fd(ci_hdi$CI_low), col = 4)
segments(ci_hdi$CI_high, 0, ci_hdi$CI_high, fd(ci_hdi$CI_high), col = 4)

segments(ci_eti$CI_low, 0, ci_eti$CI_low, fd(ci_eti$CI_low), col = 3)
segments(ci_eti$CI_high, 0, ci_eti$CI_high, fd(ci_eti$CI_high), col = 3)
