library(faraway)
library(leaps)##3conjunto de variables requesorias
library(qpcR)
prostata<-data("prostate")
attach(prostate)
mod<-regsubsets(lpsa~lcavol+lweight+age+lbph+lcp+gleason,data=prostate)
modelo<-lm(lpsa~lcavol+lweight+age+lbph+lcp+gleason)
resume<-summary(mod)

###r^2
resume$rsq

##r^2 ajustado

resume$adjr2
res<-resume$cp

plot(2:7,res)
## el mejor ,modelo de regresion es el que da el menor valor posible de cp 
# 

###eliminación hacia atras

summary(step(modelo,direction = "backward"))

# eliminación paso a paso

summary(step(modelo,direction = "both"))

### eliminación hacia adelante

modO<-lm(lpsa~1)
f<-lpsa~lcavol+lweight+age+lbph+lcp+gleason
summary(step(modO,scope=f,direction = "forward"))

##es mejor ulizar el r2 austado y el metodo stepwise

## se dejan las variables que tengan el menor press

PRESS(mode1<-lm(lpsa~lcavol))
PRESS(lm(lpsa~lcavol+lweight))
PRESS(lm(lpsa~lcavol+lweight+age))
PRESS(lm(lpsa~lcavol+lweight+age+lbph))
PRESS(lm(lpsa~lcavol+lweight+age+lbph+lcp))
PRESS(lm(lpsa~lcavol+lweight+age+lbph+lcp+gleason))


## pasos para modelar
# revisar  variables
# revisar datos atipicos o influyentes
# revisar supuestos

###Hay multicolinealidad hay si el vif >10 
vif(lm(lpsa~lcavol+lweight+age+lbph+lcp+gleason))>10

