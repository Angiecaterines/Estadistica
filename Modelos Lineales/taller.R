
library(readr)
library(MASS)
library(faraway)
library(leaps)
library(qpcR)
library(car)
library(dplyr)

#PUNTO 2
#Ejercicio 8.37 

X2 <- read_table2("~/EL BOSQUE/2019-2/Modelos Lineales/2.txt")
X2<-as.matrix(X2)[,-6]
y<-X2[,1]
n<-dim(X2)[1]

# a 
# b1=0

xc<-matrix(c(X2[,2]-mean(X2[,2]),X2[,3]-mean(X2[,3]),X2[,4]-mean(X2[,4]),X2[,5]-mean(X2[,5])),ncol=4)
hc<-xc%*%solve(t(xc)%*%xc)%*%t(xc)       
scr1<-t(y)%*%hc%*%y
j<-matrix(c(rep(1,n*n)),ncol=n)
sce1<-t(y)%*%(diag(n)-((1/n)*j)-hc)%*%y
gl<-c(dim(xc)[2],(dim(hc)-dim(xc))[2])
sc<-c(scr1,sce1)
cm<-c(scr1/gl[1],sce1/gl[2])
Fuente<-c("Regresion","Eror")
data.frame(Fuente,gl,sc,cm)
fcal<-cm[1]/cm[2]
fcri<-qf(0.05,gl[1],gl[2],lower.tail=F)
data.frame(Fuente,gl,sc,cm,fcal,fcri)           
(pvalor1<-pf(fcal,gl[1],gl[2],lower.tail = F ))

###Se rechaza ho 

#b
#Ho:b1=b3=0

x<-as.matrix(cbind(rep(1,n),X2[,-c(1,6)]))
c<-matrix(c(0,1,0,-1,0),ncol=5)
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

# c ho:Bj=0 

bj<-as.matrix(b)[-1,]
s2<-SSE/gle
g<-solve(t(x)%*%x)
sgjj<-NA
for (i in 2:5) {
sgjj[i]<-sqrt(s2*g[i,i])
}
sgjj<-sgjj[-1]
t<-bj/sgjj
p<-round(pt(t,gle,lower.tail = F),3)
cbind(bj,sgjj,t,p)

#Se rechaza ho para b2 y b4 

##d 

# ho:b1=b2=12b3=12b4
# ho:b1-b2=b2-12b3=b3-b4
# Cb=0
C<-matrix(c(0,1,-1,0,0,0,0,1,-12,0,0,0,0,1,-1),byrow = T,ncol=5)
SSR<- t(C%*%b)%*%ginv(C%*%ginv(t(x)%*%x)%*%t(C))%*%(C%*%b)
SSE<- t(y)%*%(diag(n)-h)%*%y
glr<- 3
gle<- n-(ncol(x)-1)-1
CMR<- SSR/glr
CME<-SSE/gle

(Fcal<- CMR/CME)
(pvalor<-pf(Fcal,glr,gle,lower.tail = F))


# ho:b1=b2=0

c<-matrix(c(0,1,-1,0,0),ncol=5)
b<-ginv(t(x)%*%x)%*%t(x)%*%y
h<- x%*%ginv(t(x)%*%x)%*%t(x)
SSR<- t(c%*%b)%*%ginv(c%*%ginv(t(x)%*%x)%*%t(c))%*%(c%*%b)
SSE<- t(y)%*%(diag(n)-h)%*%y
glr<- 1
gle<- n-(ncol(x)-1)-1
CMR<- SSR/glr
CME<-SSE/gle
(Fcal<- CMR/CME)
(pvalor1<-pf(Fcal,glr,gle,lower.tail = F ))

# ho:b2=12b3

c<-matrix(c(0,0,1,12,0),ncol=5)
b<-ginv(t(x)%*%x)%*%t(x)%*%y
h<- x%*%ginv(t(x)%*%x)%*%t(x)
SSR<- t(c%*%b)%*%ginv(c%*%ginv(t(x)%*%x)%*%t(c))%*%(c%*%b)
SSE<- t(y)%*%(diag(n)-h)%*%y
glr<- 1
gle<- n-(ncol(x)-1)-1
CMR<- SSR/glr
CME<-SSE/gle
(Fcal<- CMR/CME)
(pvalor1<-pf(Fcal,glr,gle,lower.tail = F ))

# ho:b3=b4
c<-matrix(c(0,0,0,1,-1),ncol=5)
b<-ginv(t(x)%*%x)%*%t(x)%*%y
h<- x%*%ginv(t(x)%*%x)%*%t(x)
SSR<- t(c%*%b)%*%ginv(c%*%ginv(t(x)%*%x)%*%t(c))%*%(c%*%b)
SSE<- t(y)%*%(diag(n)-h)%*%y
glr<- 1
gle<- n-(ncol(x)-1)-1
CMR<- SSR/glr
CME<-SSE/gle
(Fcal<- CMR/CME)
(pvalor1<-pf(Fcal,glr,gle,lower.tail = F ))

# ho:b1=b2=b3=b4
# ho:b1-b2=b2-b3=b3-b4
# CB=0
C<-matrix(c(0,1,-1,0,0,0,0,0,1,-1),byrow = T,ncol=5)
SSR<- t(C%*%b)%*%ginv(C%*%ginv(t(x)%*%x)%*%t(C))%*%(C%*%b)
SSE<- t(y)%*%(diag(n)-h)%*%y
glr<- 2
gle<- n-(ncol(x)-1)-1
CMR<- SSR/glr
CME<-SSE/gle
(Fcal<- CMR/CME)
(pvalor<-pf(Fcal,glr,gle,lower.tail = F))


##e intervalo de confianza


b<-solve(t(x)%*%x)%*%t(x)%*%y
h<-x%*%solve(t(x)%*%x)%*%t(x)
sce<-(t(y)%*%(diag(n)-h)%*%y)
s2<-sce/(n-k-1)
g<-solve(t(x)%*%x)

In<-data.frame()
f<-NA
t<-NA
ls<-NA
li<-NA
lss<-NA
lii<-NA
for (i in 2:5) {
f[i]<-((b[i])^2)/(s2*g[i,i])
t[i]<-b[i]/(sqrt(s2*g[i,i]))
pt(t,n-k-1,lower.tail = F)*2
qu<-qt(0.025,n-k-1,lower.tail = F)
quu<-qt(0.00625,n-k-1,lower.tail = F)
ls[i]<-b[i]+(qu*sqrt(s2*g[2,2]))
li[i]<-b[i]-(qu*sqrt(s2*g[2,2]))
lss[i]<-b[i]+(quu*sqrt(s2*g[2,2]))
lii[i]<-b[i]-(quu*sqrt(s2*g[2,2]))
In<-cbind(li,ls,lii,lss)  
}
In

confint(lm(y~x),level = 0.95)
round(confint(lm(y~x),level = 0.987),4)



##4 punto

#usando press

x1<-X2[,2]
x2<-X2[,3]
x3<-X2[,4]
x4<-X2[,5]
a<-PRESS(lm(y~x1))$P.square
b<-PRESS(lm(y~x1+x2))$P.square
c<-PRESS(lm(y~x1+x2+x3))$P.square
d<-PRESS(lm(y~x1+x2+x3+x4))$P.square
press<-cbind(a,b,c,d)
colnames(press)<-c("Temperatura del tanque","Temperatura gasolina","Prsion de vapor en tanque","Presión de vapor en gasolina");press

#el criterio de desicion es aquella variable que tenga menor Press,las variables x2,x3,x3
# son las mejores variables predictorias

#usando cp

X2 <- read_table2("~/EL BOSQUE/2019-2/Modelos Lineales/2.txt")

mod<-regsubsets(y~x1+x2+x3+x4,data=X2)
modelo<-lm(y~x1+x2+x3+x4)
resume<-summary(mod)
###r^2
r2<-resume$rsq

##r^2 ajustado

resume$adjr2
res<-resume$cp
plot(1:4,res)

# el mejor modelo de la regresion es el que da el menor valor posible de cp,para el cual 
# cp es aproximadamente p y esa varible es temperatura del tanque

##b backward
summary(step(modelo,direction = "backward"))

#el mejor modelo es y~x2+x3+x4 eliminando la variable x1 temperatura del tanque

##c step by step
summary(step(modelo,direction = "both"))

#el mejor modelo es y~x2+x3+x4 eliminando la variable x1 temperatura del tanque

# d varianza constante 
#Ho:Existe varianza constante
#Ha:No hay varianza constante

ncvTest(modelo)

#Se rechaza ho no tiene varianza constante los errores

# Normalidad de los residuos
#Ho:hay normalidad
#Ha:No hay normalidad

shapiro.test(residuals(modelo))
#se rechaza ho no hay normalidad en los errores

#factor de iflación de la varianza
vif(modelo)>10

#hay multicolinealidad en el modelo

#errores aleatorio
#Ho:p=0
#ha:independencia de los errores

durbinWatsonTest(modelo)

# se rechaza ho,es decir exixte independencia entre los errores 

##El mejor modelo es 
lm(y~x2+x3+x4)


##5 punto

X5 <- read_table2("~/EL BOSQUE/2019-2/Modelos Lineales/5.txt")
X5<-as.matrix(X5)

#usando press

y<-X5[,1]
x1<-X5[,2]
x2<-X5[,3]
x3<-X5[,4]
a<-PRESS(lm(y~x1))$P.square
b<-PRESS(lm(y~x1+x2))$P.square
c<-PRESS(lm(y~x1+x2+x3))$P.square
press<-cbind(a,b,c)
colnames(press)<-c("x1","x2","x3");press

#la varibale x1 presenta la menos suma de cuadrados de la predicción es x1,se puede decir que no aporta al modelo

#usando cp

X5 <- read_table2("~/EL BOSQUE/2019-2/Modelos Lineales/5.txt")
mod<-regsubsets(y~x1+x2+x3,data=X5)
modelo<-lm(y~x1+x2+x3)
resume<-summary(mod)
###r^2
r2<-resume$rsq

##r^2 ajustado

resume$adjr2
cp<-resume$cp
plot(2:4,cp)

# El mejor modelo de regresión es el que da el menor valor posible 
# de cp para el cual cp es aproximadamente igual a p y este es x1


##b backward

summary(step(modelo,direction = "backward")) 

# parece ser que el mejor modelo es y~x1+x2 eliminando la variable x3


##c step by step
summary(step(modelo,direction = "both"))
#el mejor modelo es y~x1+x2 eliminando la variable x3 que no aporta al modelo


# d varianza constante 
#Ho:Existe varianza constante
#Ha:No hay varianza constante

ncvTest(modelo)

#No se rechaza ho,es decir existe varianza constante en los residuos

# Normalidad de los residuos
#Ho:hay normalidad
#Ha:No hay normalidad

shapiro.test(residuals(modelo))
#se rechaza ho no hay normalidad en los errores

#factor de iflación de la varianza
vif(modelo)>10

# no hay multicolinealidad en el modelo

#errores aleatorio
#Ho:p=0
#ha:independencia de los errores

durbinWatsonTest(modelo)

# se rechaza ho,es decir exixte independencia entre los errores 

##El mejor modelo es y~x1+x2

