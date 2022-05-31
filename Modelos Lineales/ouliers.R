library(readr)
ToyotaCorolla <- read_csv("C:/Users/Hp/Downloads/ToyotaCorolla.csv")
attach(ToyotaCorolla)
resi<-lm(Price~KM+Age+HP+CC)$residuals
vhat<-hatvalues(lm(Price~KM+Age+HP+CC))
plot(vhat,xlim=c(0,1))
abline(h=0.5)
atipico<-ifelse(vhat>((2*5)/1436),"Si","NO")
table(atipico)
s2<-anova(lm(Price~KM+Age+HP+CC))$"Mean Sq"[5]
ri<-resi/sqrt(s2*(1-vhat))
plot(ri)
abline(h=3)
abline(h=-3)
v1<-which(ri>3) 
v2<-which(vhat>(2*5)/1436)
plot(lm(Price~KM+Age+HP+CC))
par(mfrow=c(2,2))
library(car)
##test de bonferrori es un test para detectar datos atipicos
outlierTest(lm(Price~KM+Age+HP+CC))
###muestra los datos atipicos
#hat values residuos estandarizados
influencePlot(lm(Price~KM+Age+HP+CC))


####Detección de datos influyentes
#distancia cook
k<-anova(lm(Price~KM+Age+HP+CC))$"Df"[5]
dc<-((resi)/((k+1)*s2*(1-vhat)))*(vhat/(1-vhat))
dz<-cooks.distance(lm(Price~KM+Age+HP+CC))
dc-dz
which(dc>1)
which(dz>1)
ri2<-(rstudent(lm(Price~KM+Age+HP+CC)))^2
n<-nrow((ToyotaCorolla))

cov2<-((ri2+n-k-1-1)/(n-k-1))^(k+1)
cov1<-cov2*(1-vhat)^-1


which(abs(cov1-1)>((3*(k+1))/n))#formula no correcta

#dfbetas
df1<-dfbeta(lm(Price~KM+Age+HP+CC))
da<-which(abs(df1)>(2/(sqrt(n))))
length(da)
dfbeta()
modelo1<-lm(Price~KM+Age+HP+CC)
influence.measures(modelo1)
library(readr)
ToyotaCorolla <- read_csv("C:/Users/Hp/Downloads/ToyotaCorolla.csv")
attach(ToyotaCorolla)
resi<-lm(Price~KM+Age+HP+CC)$residuals
vhat<-hatvalues(lm(Price~KM+Age+HP+CC))
plot(vhat,xlim=c(0,1))
abline(h=0.5)
atipico<-ifelse(vhat>((2*5)/1436),"Si","NO")
table(atipico)
s2<-anova(lm(Price~KM+Age+HP+CC))$"Mean Sq"[5]
ri<-resi/sqrt(s2*(1-vhat))
plot(ri)
abline(h=3)
abline(h=-3)
v1<-which(ri>3) 
v2<-which(vhat>(2*5)/1436)
plot(lm(Price~KM+Age+HP+CC))
par(mfrow=c(2,2))
library(car)
##test de bonferrori es un test para detectar datos atipicos
outlierTest(lm(Price~KM+Age+HP+CC))
###muestra los datos atipicos
#hat values residuos estandarizados
influencePlot(lm(Price~KM+Age+HP+CC))


####Detección de datos influyentes
#distancia cook
k<-anova(lm(Price~KM+Age+HP+CC))$"Df"[5]
dc<-((resi)/((k+1)*s2*(1-vhat)))*(vhat/(1-vhat))
dz<-cooks.distance(lm(Price~KM+Age+HP+CC))
dc-dz
which(dc>1)
which(dz>1)
ri2<-(rstudent(lm(Price~KM+Age+HP+CC)))^2
n<-nrow((ToyotaCorolla))

cov2<-((ri2+n-k-1-1)/(n-k-1))^(k+1)
cov1<-cov2*(1-vhat)^-1


which(abs(cov1-1)>((3*(k+1))/n))#formula no correcta
#dfbetas
df1<-dfbeta(lm(Price~KM+Age+HP+CC))
da<-which(abs(df1)>(2/(sqrt(n))))
length(da)
dfbeta()
modelo1<-lm(Price~KM+Age+HP+CC)
influence.measures(modelo1)
library(readr)
ToyotaCorolla <- read_csv("C:/Users/Hp/Downloads/ToyotaCorolla.csv")
attach(ToyotaCorolla)
resi<-lm(Price~KM+Age+HP+CC)$residuals
vhat<-hatvalues(lm(Price~KM+Age+HP+CC))
plot(vhat,xlim=c(0,1))
abline(h=0.5)
atipico<-ifelse(vhat>((2*5)/1436),"Si","NO")
table(atipico)
s2<-anova(lm(Price~KM+Age+HP+CC))$"Mean Sq"[5]
ri<-resi/sqrt(s2*(1-vhat))
plot(ri)
abline(h=3)
abline(h=-3)
v1<-which(ri>3) 
v2<-which(vhat>(2*5)/1436)
plot(lm(Price~KM+Age+HP+CC))
par(mfrow=c(2,2))
library(car)
##test de bonferrori es un test para detectar datos atipicos
outlierTest(lm(Price~KM+Age+HP+CC))
###muestra los datos atipicos
#hat values residuos estandarizados
influencePlot(lm(Price~KM+Age+HP+CC))


####Detección de datos influyentes
#distancia cook
k<-anova(lm(Price~KM+Age+HP+CC))$"Df"[5]
dc<-((resi)/((k+1)*s2*(1-vhat)))*(vhat/(1-vhat))
dz<-cooks.distance(lm(Price~KM+Age+HP+CC))
dc-dz
which(dc>1)
which(dz>1)
ri2<-(rstudent(lm(Price~KM+Age+HP+CC)))^2
n<-nrow((ToyotaCorolla))

cov2<-((ri2+n-k-1-1)/(n-k-1))^(k+1)
cov1<-cov2*(1-vhat)^-1


which(abs(cov1-1)>((3*(k+1))/n))#formula no correcta
#dfbetas
df1<-dfbeta(lm(Price~KM+Age+HP+CC))
da<-which(abs(df1)>(2/(sqrt(n))))
length(da)
dfbeta()
modelo1<-lm(Price~KM+Age+HP+CC)
influence.measures(modelo1)
library(readr)
ToyotaCorolla <- read_csv("C:/Users/Hp/Downloads/ToyotaCorolla.csv")
attach(ToyotaCorolla)
resi<-lm(Price~KM+Age+HP+CC)$residuals
vhat<-hatvalues(lm(Price~KM+Age+HP+CC))
plot(vhat,xlim=c(0,1))
abline(h=0.5)
atipico<-ifelse(vhat>((2*5)/1436),"Si","NO")
table(atipico)
s2<-anova(lm(Price~KM+Age+HP+CC))$"Mean Sq"[5]
ri<-resi/sqrt(s2*(1-vhat))
plot(ri)
abline(h=3)
abline(h=-3)
v1<-which(ri>3) 
v2<-which(vhat>(2*5)/1436)
plot(lm(Price~KM+Age+HP+CC))
par(mfrow=c(2,2))
library(car)
##test de bonferrori es un test para detectar datos atipicos
outlierTest(lm(Price~KM+Age+HP+CC))
###muestra los datos atipicos
#hat values residuos estandarizados
influencePlot(lm(Price~KM+Age+HP+CC))


####Detección de datos influyentes
#distancia cook
k<-anova(lm(Price~KM+Age+HP+CC))$"Df"[5]
dc<-((resi)/((k+1)*s2*(1-vhat)))*(vhat/(1-vhat))
dz<-cooks.distance(lm(Price~KM+Age+HP+CC))
dc-dz
which(dc>1)
which(dz>1)
ri2<-(rstudent(lm(Price~KM+Age+HP+CC)))^2
n<-nrow((ToyotaCorolla))

cov2<-((ri2+n-k-1-1)/(n-k-1))^(k+1)
cov1<-cov2*(1-vhat)^-1



which(abs(cov1-1)>((3*(k+1))/n))#formula no correcta
#dfbetas
df1<-dfbeta(lm(Price~KM+Age+HP+CC))
da<-which(abs(df1)>(2/(sqrt(n))))
length(da)
dfbeta()
modelo1<-lm(Price~KM+Age+HP+CC)

influence.measures(modelo1)
###datos influyentes

length(which(apply(influence.measures(modelo1)$is.inf, 1, any)))
##datos atipicos
outlierTest(lm(Price~KM+Age+HP+CC))

##se eliminan los datos influyen=atipicos

###test de independencia de los errores
#ho: rho=0
durbinWatsonTest(modelo1)
#no hay independencia en los errores, es decir se rechaza Ho

###test normalidad residuos
#ho_ hay normalidad en los residuos
shapiro.test(residuals(modelo1))
#kolmogorov
ks.test(residuals(modelo1),"pnorm")

###test de varianza constante
ncvTest(modelo1)
#se rechaza ho NO TIENE VARIANZA COSNTANTE LOS ERRORES