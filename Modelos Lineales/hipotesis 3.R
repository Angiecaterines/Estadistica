
###bj=0

library(MASS)
set.seed(12)
n<-2000
x1<-rpois(n,3)
y<-rnorm(n,10,100)
summary(lm(y~x1))
x<-matrix(c(rep(1,length(x1)),x1),ncol=2)
k<-n-(ncol(x)-1)-1
b<-solve(t(x)%*%x)%*%t(x)%*%y
h<-x%*%solve(t(x)%*%x)%*%t(x)
sce<-(t(y)%*%(diag(n)-h)%*%y)
s2<-sce/(n-k-1)
g<-solve(t(x)%*%x)
f<-((b[4])^2)/(s2*g[4,4])
t<-b[4]/(sqrt(s2*g[4,4]))
pt(t,n-k-1,lower.tail = F)*2

##intervalo de confianza
qu<-qt(0.025,n-k-1,lower.tail = F)

ls<-b[4]+(qt(0.025,n-k-1,lower.tail = F)*sqrt(s2*g[2,2]))
li<-b[4]-(qt(0.025,n-k-1,lower.tail = F)*sqrt(s2*g[2,2]))
In<-c(li,ls);In

confint(lm(y~x),level = 0.025)


xo<-c(1,3.88)
a<-sqrt(t(xo)%*%ginv(t(x)%*%x)%*%xo)
li<-xo%*%b-(qu*sqrt(s2)*a)
ls<-xo%*%b+(qu*sqrt(s2)*a)
In<-c(li,ls);In
datos<-data.frame(y,x1)
modelo1<-lm(y~x1,data = datos)
new<-data.frame(3.88)
names(new)<-c("x1")
predict(modelo1,new,interval = "confidence")

##3 intervalo de predicción

a1<-sqrt(1+t(xo)%*%ginv(t(x)%*%x)%*%xo)
li<-xo%*%b-(qu*sqrt(s2)*a1)
ls<-xo%*%b+(qu*sqrt(s2)*a1)
In<-c(li,ls);In
predict(modelo1,new,interval = "prediction")
