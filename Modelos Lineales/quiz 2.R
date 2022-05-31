library(readr)
library(faraway)
library(leaps)##3conjunto de variables requesorias
library(qpcR)
library(car)
Toyo<- read_csv("C:/Users/Hp/Downloads/ToyotaCorolla (1).csv")
attach(Toyo)
modelo<-lm(Price~Age+KM+HP+MetColor+Automatic+CC+Doors+Weight)

### presss

m1<-PRESS(lm(Price~Age))
m2<-PRESS(lm(Price~Age+KM))
m3<-PRESS(lm(Price~Age+KM+HP))
m4<-PRESS(lm(Price~Age+KM+HP+MetColor))
m5<-PRESS(lm(Price~Age+KM+HP+MetColor+Automatic))
m6<-PRESS(lm(Price~Age+KM+HP+MetColor+Automatic+CC))
m7<-PRESS(lm(Price~Age+KM+HP+MetColor+Automatic+CC+Doors))
m8<-PRESS(lm(Price~Age+KM+HP+MetColor+Automatic+CC+Doors+Weight))

a<-cbind(m1$P.square,m2$P.square,m3$P.square,m4$P.square,m5$P.square,m6$P.square,m7$P.square,m8$P.square)
colnames(a)<-c("Age","KM","HP","MetColor","Automatic","CC","Doors","Weight")

##parece ser que las variables que explican el modelo son Age y km

mod<-regsubsets(Price~Age+KM+HP+MetColor+Automatic+CC+Doors+Weight,data=Toyo)
resume<-summary(mod)
###r^2
resume$rsq

##r^2 ajustado

resume$adjr2
res<-resume$cp
plot(1:8,res)

##el mejor modelo es aquel que da el menos valor posible de cp,lo que quiere decir
# que las variables que explican el modelo es age

#b

###regresion backward

summary(step(modelo,direction = "backward"))

##parece ser que el mejor conjunto de variables independientes 
# son age,km,hp,cc y weight

#c
##regresion step by step

summary(step(modelo,direction = "both"))

##parece ser que el mejor conjunto de variables independientes 
# son age,km,hp,cc y weight

##
modO<-lm(Price~1)
f<-Price~Age+KM+HP+MetColor+Automatic+CC+Doors+Weight
summary(step(modO,scope=f,direction = "forward"))

##parece ser que el mejor conjunto de variables independientes 
# son age,km,hp,cc y weight

#d
modelo1<-lm(Price~Age)

#varianza constante

#ho:varianza constante
#ha:varianza no constante 

ncvTest(modelo1)

#se rechaza ho NO TIENE VARIANZA COSNTANTE LOS ERRORES

#Normalidad

#ho:hay normalidad en los residuos
#ha:no hay normalidad en los residuos

shapiro.test(residuals(modelo1))

##se rechaza ho

###test de independencia de los errores
#ho: rho=0
#ha: rho!=0
durbinWatsonTest(modelo1)
#no hay independencia en los errores, es decir se rechaza Ho


###2 punto

Uni<- read_csv("C:/Users/Hp/Downloads/UniversalBank.csv")
attach(Uni)

#a residuos estudentizados

influencePlot(lm(CCAvg~Education+Mortgage+SecuritiesAccount+CDAccount+Online+CreditCard))

### los posibles datos atipicos son las observaciones
# 783,788,1938,2102,2770,2813

#b test de bonferrori

outlierTest(lm(CCAvg~Education+Mortgage+SecuritiesAccount+CDAccount+Online+CreditCard))

#c) Se pueden considerar datos atipicos las observaciones 788 y 2102

#d) matriz hii

resi<-lm(CCAvg~Education+Mortgage+SecuritiesAccount+CDAccount+Online+CreditCard)$residuals
vhat<-hatvalues(lm(CCAvg~Education+Mortgage+SecuritiesAccount+CDAccount+Online+CreditCard))
atipico<-ifelse(vhat>((2*5)/1436),"Si","NO")
table(atipico)
s2<-anova(lm(CCAvg~Education+Mortgage+SecuritiesAccount+CDAccount+Online+CreditCard))$"Mean Sq"[5]
ri<-resi/sqrt(s2*(1-vhat))

###regal de desición

v1<-which(ri>3) 
v2<-which(vhat>(2*5)/1436)
par(mfrow=c(2,2))
plot(lm(CCAvg~Education+Mortgage+SecuritiesAccount+CDAccount+Online+CreditCard))


#c) distancia de cook

k<-anova(lm(CCAvg~Education+Mortgage+SecuritiesAccount+CDAccount+Online+CreditCard))$"Df"[7]
dc<-((resi)/((k+1)*s2*(1-vhat)))*(vhat/(1-vhat))
dz<-cooks.distance(lm(CCAvg~Education+Mortgage+SecuritiesAccount+CDAccount+Online+CreditCard))
dc-dz
which(dc>1)
which(dz>1)
ri2<-(rstudent(lm(CCAvg~Education+Mortgage+SecuritiesAccount+CDAccount+Online+CreditCard)))^2
n<-nrow((Uni))

cov2<-((ri2+n-k-1-1)/(n-k-1))^(k+1)
cov1<-cov2*(1-vhat)^-1


which(abs(cov1-1)>((3*(k+1))/n))


#dfbetas
df1<-dfbeta(lm(CCAvg~Education+Mortgage+SecuritiesAccount+CDAccount+Online+CreditCard))
da<-which(abs(df1)>(2/(sqrt(n))))
length(da)
modelo1<-lm(CCAvg~Education+Mortgage+SecuritiesAccount+CDAccount+Online+CreditCard)
influence.measures(modelo1)

a<-as.data.frame(influence.measures(modelo1)$is.inf[,c(7,8)])%>%filter(dffit==TRUE)


#e) si se eliminan observaciones se eliminan las observaciones que son tanto datos atipicos como influyentes
 #las cuales son la 788 y la 2102

# f) 

Uni1<-Uni[-c(788,2102),]
attach(Uni1)

modelo2<-lm(CCAvg~Education+Mortgage+SecuritiesAccount+CDAccount+Online+CreditCard)



