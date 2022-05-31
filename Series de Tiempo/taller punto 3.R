##############################################################################
####################               Punto 3                #################### 
##############################################################################

#a) Grafique la serie

y<-WWWusage
plot(z)
x<-1:length(y)
modelo<-lm(y~x)
abline(modelo,col="cyan")

# se mira la normalidad de los errores
e <- modelo$residuals
qqnorm(e) ; qqline(e)
##parece ser que la serie es una ma(q)
acf(e)
pacf(e)

#b) es necesario transformar la serie? si es así, transforme adecuadamente la serie. 

# La serie wwwusage tiene problemas de variabilidad
# se busca una transformación de boxcox

lambda<-powerTransform(y)$lambda ; lambda
# ((y^lam) - 1) /lam si  lam!=0
#  log(y) si lam = 0
#c) grafique la serie resultante de (b

# los transformo
z<-bcPower(y, lambda) #sqrt(y)
plot(z)


#d) ¿es necesario diferenciar la serie resultante de (b)? Sí ¿cuántas veces? 1
w<-diff(z,differences = 1)
plot(w)

#cuanto mas se diferencia la serie se pierden datos 

#e) Grafique ACF y la PACF de la serie resultante en (d) 
# ¿Qué modelo se puede identicar

acf(w)
pacf(w)

#es una serie ma(1)

eacf(w)
