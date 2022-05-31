
library(readr)
library(MASS)
library(faraway)
library(leaps)
library(qpcR)
library(car)
library(dplyr)

y<-c(33.20,40.30,38.70,46.80,41.40,37.50,39,40.70,30.10,52.90,38.20,31.80,43.30,44.10,42.80,33.60,34.20,48.00,38,35.90,40.40,36.80,45.20,35.10)
x1<-c(3.5,5.3,5.1,5.8,4.2,6.0,6.8,5.5,3.1,7.2,4.5,4.9,8.0,6.5,6.6,3.7,6.2,7.0,4.0,4.5,5.9,5.6,4.8,3.9)
x2<-c(9,20,18,33,31,13,25,30,5,47,25,11,23,35,39,21,7,40,35,23,33,27,34,15)
x3<-c(6.10,6.4,7.4,6.7,7.5,5.9,6,4,5.8,8.3,5,6.4,7.6,7,5,4.4,5.5,7,6,3.5,4.9,4.3,8,5)
x<-as.matrix(cbind(rep(1,24),x1,x2,x3))
b<-ginv(t(x)%*%x)%*%t(x)%*%y

# 1) a)cov(ê,^y)=0
ye<-x%*%b
es<-y-ye
residuals(lm(y~x1+x2+x3))
(cov(es,ye))

#b) ê'^y=0
t(es)%*%ye

# 2 punto

## a) residuos studentizados
influencePlot(lm(y~x1+x2+x3))
ri2<-(rstudent(lm(y~x1+x2+x3)))^2

#pueden ser datos atipicos las observaciones 12,19 

# b)test de bonferroni

outlierTest(lm(y~x1+x2+x3))

# c) la obsefvacion 19 es un dato atipico

#d) Hii

resi<-lm(y~x1+x2+x3)$residuals
vhat<-hatvalues(lm(y~x1+x2+x3))
plot(vhat,xlim=c(0,100))
s2<-anova(lm(y~x1+x2+x3))$"Mean Sq"[4]
ri<-resi/sqrt(s2*(1-vhat))
plot(ri)
abline(h=3,col=2)
abline(h=-3,col=5)
v1<-which(ri>3) 
par(mfrow=c(2,2))
plot(lm(y~x1+x2+x3))

# e)
# Distancia de cook

k<-anova(lm(y~x1+x2+x3))$"Df"[4]
dc<-((resi)/((k+1)*s2*(1-vhat)))*(vhat/(1-vhat))
dz<-cooks.distance(lm(y~x1+x2+x3))
dc-dz
which(dc>1)
which(dz>1)
ri2<-(rstudent(lm(y~x1+x2+x3)))^2
n<-length(y)

cov2<-((ri2+n-k-1-1)/(n-k-1))^(k+1)
cov1<-cov2*(1-vhat)^-1
which(abs(cov1-1)>((3*(k+1))/n))

##los posibles datos influyentes utilizando la distancia de cook  son 2,4,8,12,19

#Dfbeta
df1<-dfbeta(lm(y~x1+x2+x3))
da<-which(abs(df1)>(2/(sqrt(n))))
length(da)
modelo1<-lm(y~x1+x2+x3)
influence.measures(modelo1)
which(apply(influence.measures(modelo1)$is.inf, 1, any))
#los posibles datos influyentes utilizando Dfbeta  son 9,13,17

#Covratio
covratio(modelo1)

# Se deben eliminar las obsercaiones 12 y 19 ya que ambos datos son datos atipicos e influyentes

y<-y[-c(12,19)]
x1<-x1[-c(12,19)]
x2<-x2[-c(12,19)]
x3<-x3[-c(12,19)]

lm(y~x1+x2+x3)

##Supuestos del modelo

#varianza constante 
#Ho:Existe varianza constante
#Ha:No hay varianza constante

ncvTest(modelo1)

#No se rechaza ho,es decir existe varianza constante en los residuos

# Normalidad de los residuos
#Ho:hay normalidad
#Ha:No hay normalidad

shapiro.test(residuals(modelo1))
# No se rechaza ho, hay normalidad en los errores

#factor de iflación de la varianza
vif(modelo1)>10

# no hay multicolinealidad en el modelo

#errores aleatorio
#Ho:p=0
#ha:independencia de los errores

durbinWatsonTest(modelo1)

#No se rechaza ho

## 3 punto

#ho:b2=b3=0
#ho:b2-b3=0

y<-c(33.20,40.30,38.70,46.80,41.40,37.50,39,40.70,30.10,52.90,38.20,31.80,43.30,44.10,42.80,33.60,34.20,48.00,38,35.90,40.40,36.80,45.20,35.10)
x1<-c(3.5,5.3,5.1,5.8,4.2,6.0,6.8,5.5,3.1,7.2,4.5,4.9,8.0,6.5,6.6,3.7,6.2,7.0,4.0,4.5,5.9,5.6,4.8,3.9)
x2<-c(9,20,18,33,31,13,25,30,5,47,25,11,23,35,39,21,7,40,35,23,33,27,34,15)
x3<-c(6.10,6.4,7.4,6.7,7.5,5.9,6,4,5.8,8.3,5,6.4,7.6,7,5,4.4,5.5,7,6,3.5,4.9,4.3,8,5)

n<-length(y)
x<-as.matrix(cbind(rep(1,n),x1,x2,x3))
c<-matrix(c(0,0,1,-1),ncol=4)
b<-ginv(t(x)%*%x)%*%t(x)%*%y
h<- x%*%ginv(t(x)%*%x)%*%t(x)
SSR<- t(c%*%b)%*%ginv(c%*%ginv(t(x)%*%x)%*%t(c))%*%(c%*%b)
SSE<- t(y)%*%(diag(n)-h)%*%y
glr<- 1
gle<- n-(ncol(x)-1)-1
CMR<- SSR/glr
CME<-SSE/gle
Fcal<- CMR/CME
Fcri<- qf(0.05, glr,gle,lower.tail = F)
TANOVA<- data.frame(SC=c(SSR,SSE),GL=c(glr,gle), CM=c(CMR,CME), Fcal, Fcri)
TANOVA
(pvalor1<-pf(Fcal,glr,gle,lower.tail = F ))

# rechazo Ho es decir b2-b3!=0



