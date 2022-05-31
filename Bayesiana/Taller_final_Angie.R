library(readxl)
Calidad <- data.frame(read_excel("C:/Users/Hp/Downloads/Calidad.xlsx"))
calidad <- data.frame(Articulos=Calidad$Artículos, Calidad[,3:5]*(1/(Calidad[,2])))

apply(calidad[-1],1,sum)

# usando el ultimo articulo como la muestra y
# las 35 articulos previos como información previa

x = unlist(calidad[36,2:4]*calidad[36,1]) ; x

# MLE del parámetro a priori de la Dirichlet

alpha = dirichlet.mle(calidad[1:35,-1])$alpha ; alpha

#vector de parametros alpha,estimación via maxima verosimilitud se estiman sus parametros

# Muestra posterior
p.muestra = rdirichlet(n = 1000, alpha = x+alpha)
p.muestra

par(mfrow = c(1,3))
plot(density(p.muestra[,1], from = 0.64, to = 0.96),main = "A.Aceptados",col="blue")
plot(density(p.muestra[,2], from = 0.02, to = 0.28),main = "A.Reparacion",col="purple")
plot(density(p.muestra[,3], from = 0, to = 0.16),main = "A.Desechados",col ="red")

# Las distribuciones para ningun tipo de artiulo son simetricas

# se procede  encontrar la mediana 

# densidades

d1<-density(p.muestra[,1])
d2<-density(p.muestra[,2])
d3<-density(p.muestra[,3])

# aproximaciones
fd1 = approxfun(d1$x,d1$y)
fd2 = approxfun(d2$x,d2$y)
fd3 = approxfun(d3$x,d3$y)


# Mediana posterior
integrate(fd1, min(d1$x), 0.86)$value
integrate(fd2, min(d2$x), 0.09)$value
integrate(fd3, min(d3$x), 0.02)$value

# Esperanza posterior
fd11 = approxfun(d1$x,d1$x*d1$y)
integrate(fd11, min(d1$x), max(d1$x))$value

fd12 = approxfun(d2$x,d2$x*d2$y)
integrate(fd12, min(d2$x), max(d2$x))$value

fd13 = approxfun(d3$x,d3$x*d3$y)
integrate(fd13, min(d3$x), max(d3$x))$value


# Moda posterior
d1$x[d1$y==max(d1$y)]
d2$x[d2$y==max(d2$y)]
d3$x[d3$y==max(d3$y)]


# intervalo  89% de credibilidad


# Intervalo de credibilidad para los articulos aceptados ------------------

par(mfrow = c(1,3))
plot(density(p.muestra[,1], from = 0.64, to = 0.96),main = "A.Aceptados",col="blue")
ci_hdi <- ci(p.muestra[,1], method = "HDI",ci = 0.89) # más alta densidad

segments(ci_hdi$CI_low, 0, ci_hdi$CI_low, fd1(ci_hdi$CI_low), col = 4)
segments(ci_hdi$CI_high, 0, ci_hdi$CI_high, fd1(ci_hdi$CI_high), col = 4)


#  El intervalo de credibilidad para los articulos acpetados esta entre
#  el 0.76 y el 0.88.

# Intervalo de credibilidad para los articulos en reparación --------------



plot(density(p.muestra[,2], from = 0.02, to = 0.28),main = "A.Reparacion",col="purple")
ci_hdi <- ci(p.muestra[,2], method = "HDI",ci = 0.89) # más alta densidad

segments(ci_hdi$CI_low, 0, ci_hdi$CI_low, fd2(ci_hdi$CI_low), col = 4)
segments(ci_hdi$CI_high, 0, ci_hdi$CI_high, fd2(ci_hdi$CI_high), col = 4)

# El intervalo de credibilidad para los articulos en reparación esta entre
# el 0.07 y 0.17 quiere decir entonces que como la probabilidad de reparar 
# un articulo no supera el 0.2 no se detiene el proceso por articulo
# en reparación.

# Intervalo de credibilidad para los articulos desechados -----------------


plot(density(p.muestra[,3], from = 0, to = 0.16),main = "A.Desechados",col ="red")

ci_hdi <- ci(p.muestra[,3], method = "HDI",ci = 0.89) # más alta densidad

segments(ci_hdi$CI_low, 0, ci_hdi$CI_low, fd3(ci_hdi$CI_low), col = 4)
segments(ci_hdi$CI_high, 0, ci_hdi$CI_high, fd3(ci_hdi$CI_high), col = 4)


# El intervalo de credibilidad para los articulos desechados esta entre 
# el 0.02 y el 0.08 la probabilidad supera el 0.05 quiere decir entonces 
# que el proceso de producción debe detenerse, ya que la probabilidad
# de articulos desechados esta entre 0.02 y 0.08.
