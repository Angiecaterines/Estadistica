
library(bayestestR)
library(dplyr)
library(ggplot2)
library(readxl)
Precip <- read_excel("Precipitación Bogotá.xlsx")
View(Precip)

Precip

pmax = apply(Precip[,8:19],1,max)
pmax
hist(pmax, freq = F, ylim = c(0,0.007))
lines(density(pmax, from = 0))

require(MASS)

param = fitdistr(pmax, densfun = "log-normal")

curve(dlnorm(x, param$estimate[1], param$estimate[2]), from = 0, to = 1000, add = T, col = 4)

# Posterior de mu y sigma2

Pos1 = function(the, y){
  mu = the[1] ; s2 = the[2]
  n = length(y) ; ly = log(y)
  ifelse(s2>0,log(s2^(-.5*(n+3))*exp(-sum((ly-mu)^2)/(2*s2))),-Inf)
}

require(mcmc)

met = metrop(Pos1, c(param$estimate[1], param$estimate[2]), 10000, blen = 100, y = pmax[1:10])

plot.ts(met$batch)
plot(density(met$batch[,1]))
plot(density(met$batch[,2]))

# Posterior de mu y m

Pos2 = function(the, y){
  mu = the[1] ; m = max(the[2],50)
  n = length(y)
  ifelse(log(m)-mu>0,-(n+3)/2*(log(m)+log(log(m)-mu)) - sum((log(y)-mu)^2)/(4*(log(m)-mu)),-Inf)
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


# prueba de hipotesis -----------------------------------------------------

summary(pmax)

# Ho: m > 170
# H1: m = 170

# cuando mis distribuciones apriori son no informativas

ro_a = 1
Po = integrate(fd,170, max(d$x))$value;Po
P1 = integrate(fd,min(d$x), 170)$value;P1
ro_p = Po/P1

B = ro_a/ro_p;B

# Dado que B>1 se tiene evidencia estadistica de 
# que la hipotesis nula se sostiene


# Ho: m > 170 & m < 180
# H1: m = 170 ! m >=180


ro_a = 1
Po = integrate(fd,170, 180)$value;Po
P1 = integrate(fd,min(d$x), 170)$value + 
    integrate(fd,180, max(d$x))$value
ro_p = Po/P1

B = ro_a/ro_p;B

