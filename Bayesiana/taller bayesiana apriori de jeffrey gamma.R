

# taller bayesiana --------------------------------------------------------


# librerias ---------------------------------------------------------------


library(bayestestR)#intervalo y pruebas de hipotesis
library(dplyr)
library(ggplot2)
library(readxl)
library(mcmc)
library(MASS)
library(gibbs.met)


# datos -------------------------------------------------------------------


Precip <- read_excel("Precipitación Bogotá.xlsx")
View(Precip)


# precipitación máxima por medición --------------------------------------------


pmax = apply(Precip[,8:19],1,max)
pmax
hist(pmax, freq = F, ylim = c(0,0.007))
lines(density(pmax, from = 0))


# Ajuste de distribución gamma --------------------------------------------


param = fitdistr(pmax, densfun = "gamma")

curve(dgamma(x, param$estimate[1], param$estimate[2]), from = 0, to = 1000, add = T, col = 4)


# Distribución posterior de alpha y beta ----------------------------------

pos1 = function(the, y){
  a = the[1] ; B = the[2]
  n = length(y)
  B^(n*a-1)/(gamma(a))^n*(prod(y))^a*exp(-n*B*mean(y))*(a*trigamma(a)-1)^.5
  ifelse(a>0 & B>0,log(B^(n*a-1)/(gamma(a))^n*(prod(y))^a*exp(-n*B*mean(y))*(a*trigamma(a)-1)^.5),-Inf)
  }


# Metodo monte carlo ----------------------------------------------------



met = metrop(log(pos1), c(param$estimate[1], param$estimate[2]), 10000, blen = 100, y = pmax[1:10])

plot.ts(met$batch)
plot(density(met$batch[,1]))
plot(density(met$batch[,2]))

# Posterior de a y m

Pos2 = function(the, y){
  a = the[1] ; m = max(the[2],50)
  n = length(y)
  ifelse(a>0,log(a/m)-0.5*(log(-a*trigamma(a)-1))+(n*a-1)*log(a/m)-n*log(gamma(a))+log(prod(y))-n*(a/m)*mean(y),-Inf)
}

m0 = exp(param$estimate[1]+param$estimate[2]^2/2) ; m0
met = metrop(Pos2, c(param$estimate[1],m0), nbatch =  10000, blen = 100, y = pmax)

plot.ts(met$batch)
plot(density(met$batch[-(1:5000),1]))
plot(density(met$batch[-(1:5000),2]))

require(gibbs.met)
gib = gibbs_met(log_f = Pos2 , no_var = 2, 
                ini_value = c(param$estimate[1],m0), 
                stepsizes_met=c(0.01,0.01),iters=10000, iters_met=2, y = pmax)

plot.ts(gib)
plot(density(gib[-(1:5000),1]))
plot(density(gib[-(1:5000),2]))

posterior = met$batch[-(1:5000),2]

plot(d<-density(posterior))
fd = approxfun(d$x,d$y)

# Mediana posterior
integrate(fd, min(d$x), 158.394)$value

# Esperanza posterior
fd2 = approxfun(d$x,d$x*d$y)
integrate(fd2, min(d$x), max(d$x))$value

# Moda posterior
d$x[d$y==max(d$y)]

# Intervalo de credibilidad

ci_hdi <- ci(posterior, method = "HDI", ci = 0.95) # más alta densidad
ci_eti <- ci(posterior, method = "ETI", ci = 0.95) # igual probabilidad "antes" y "despues" del intervalo

segments(ci_hdi$CI_low, 0, ci_hdi$CI_low, fd(ci_hdi$CI_low), col = 4)
segments(ci_hdi$CI_high, 0, ci_hdi$CI_high, fd(ci_hdi$CI_high), col = 4)

segments(ci_eti$CI_low, 0, ci_eti$CI_low, fd(ci_eti$CI_low), col = 3)
segments(ci_eti$CI_high, 0, ci_eti$CI_high, fd(ci_eti$CI_high), col = 3)

# Se recomienda hacerlo al 89% de credibilidad

ci_hdi <- ci(posterior, method = "HDI") # más alta densidad
ci_eti <- ci(posterior, method = "ETI") # igual probabilidad "antes" y "despues" del intervalo

segments(ci_hdi$CI_low, 0, ci_hdi$CI_low, fd(ci_hdi$CI_low), col = 4)
segments(ci_hdi$CI_high, 0, ci_hdi$CI_high, fd(ci_hdi$CI_high), col = 4)

segments(ci_eti$CI_low, 0, ci_eti$CI_low, fd(ci_eti$CI_low), col = 3)
segments(ci_eti$CI_high, 0, ci_eti$CI_high, fd(ci_eti$CI_high), col = 3)