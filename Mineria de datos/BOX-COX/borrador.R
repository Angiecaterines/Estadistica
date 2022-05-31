# powerTransform utiliza el enfoque de máxima verosimilitud de Box y Cox (1964) 
# para seleccionar una transformación de una respuesta univariada o multivariada 
# para normalidad, linealidad y / o varianza constante. Las familias de transformaciones 
# disponibles son la familia de potencia predeterminada de Box-Cox y dos familias 
# adicionales que son modificaciones de la familia de Box-Cox que permiten (algunas)
# respuestas negativas. El método de resumen calcula automáticamente dos o tres pruebas
# de tipo de razón de verosimilitud con respecto a las potencias de transformación.


forecast::BoxCox.lambda(cars$speed)
car::powerTransform(cars)

lam<-car::powerTransform(cars)
summary(car::powerTransform(cars))

bcPower(cars$speed,lambda = 0.87)
BoxCox(cars$speed,0.60)
dev.off()
x11()
Conf3x3 = matrix(c(1:3), nrow=1, byrow=TRUE)
layout(Conf3x3)
layout.show(3)
boxplot(cars$speed,col=3,title("Datos originales"))
boxplot(bcPower(cars$speed,lambda = 0.87),col=3,title("Transformación con bcpower"))
boxplot(BoxCox(cars$speed,0.60),col=3,title("Transformación con Boxcox"))


forecast::BoxCox()
forecast::InvBoxCox()

AirPassengers
plot(AirPassengers)
length(AirPassengers)
x = 1:144
z = as.vector(AirPassengers)
plot(x,z,type="l")
mod = lm(z~x)
abline(mod, col = 4)

w = log(z)

plot(x,w,type="l")

y = diff(w, lag = 1, differences = 1) ; y
plot(x[-1],y,type="l")


v = diff(y, lag = 12, differences = 1) ; y
plot(ts(v))

require(car)
require(forecast)
lam = powerTransform(z)$lambda ; lam
lam1<-BoxCox.lambda(z);lam1

y = bcPower(z, lambda = lam)
y1<-bcPower(z, lambda = lam1)
plot(ts(y))
plot(ts(y1)) 
d = bcnPowerInverse(y, lam, gamma = 1)
plot(ts(d))



# LIBRERIA Y DATOS
# -----------------------------------------------------------------------
library(MASS)
library(forecast)
library(e1071)  
df <- Boston 
v  <- names(df[,sapply(df, class)=="numeric"])

# TRANSFORMACION BOX-COX
# ------------------------------------------------------------------------------
for (ii in 1:length(v)){
	asimetria       <- skewness(df[,v[ii]])
	coef.variacion  <- sd(df[,v[ii]]) / mean(df[,v[ii]]) 
	
	if ( (asimetria < -1 | asimetria > 1) & coef.variacion > 1){
		print(paste("transformando variable: ",v[ii]))
		df$bc_tmp     <- BoxCox(df[,v[ii]],BoxCox.lambda(df[,v[ii]]))
		names(df)[names(df)=="bc_tmp"] <- paste("BoxCox_", v[ii], sep = "")
	}
}

# Box Cox Method, univariate
summary(p1 <- powerTransform(cycles ~ len + amp + load, Wool))

# fit linear model with transformed response:
coef(p1, round=TRUE)
summary(m1 <- lm(bcPower(cycles, p1$roundlam) ~ len + amp + load, Wool))

# Multivariate Box Cox uses Highway1 data
summary(powerTransform(cbind(len, adt, trks, sigs1) ~ 1, Highway1))

# Multivariate transformation to normality within levels of 'htype'
summary(a3 <- powerTransform(cbind(len, adt, trks, sigs1) ~ htype, Highway1))

# test lambda = (0 0 0 -1)
testTransform(a3, c(0, 0, 0, -1))

# save the rounded transformed values, plot them with a separate
# color for each highway type
transformedY <- bcPower(with(Highway1, cbind(len, adt, trks, sigs1)),
			coef(a3, round=TRUE))
scatterplotMatrix( ~ transformedY|htype, Highway1) 


sse<-c(78.0,77,76,75,73,71,70.4,57.8,48.4,41.4,36.4,34.5,33.1,31.2,30.7,30.6,30.7,31.1,32,32.7,33.9)
lambda<-seq(-1,1,0.1)
conjunto<-data.frame(sse,lambda)
plot(x=lambda,y=sse,type = "l")



