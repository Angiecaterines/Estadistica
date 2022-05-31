####
#ho:B=0
#ha:B=!0
library(MASS)
set.seed(123)
n<-1000
y<-rnorm(n,70,1)
x1<-rpois(n,3)
x2<-rnorm(n,0,1)
x<-matrix(c(rep(1,length(x1)),x1,x2),ncol=3)
h<-x%*%ginv(t(x)%*%x)%*%t(x)       
scr<-t(y)%*%h%*%y
sce<-t(y)%*%(diag(n)-h)%%y
gl<-c(dim(x)[2],(dim(h)-dim(x))[2])
cm<-c(scr/gl[1],sce/gl[2])
fcal<-cm[1]/cm[2]
fcri<-qf(0.05,3,997,lower.tail=F)
pvalor<-pf(fcal,3,997,lower.tail = F)

#ho:^b1=0
#ha:^b1=!0

n<-1000
y<-rnorm(n,70,1)
x1<-rpois(n,3)
x2<-rnorm(n,0,1)
xc<-matrix(c(x1-mean(x1),x2-mean(x2)),ncol=2)
hc<-xc%*%solve(t(xc)%*%xc)%*%t(xc)       
scr1<-t(y)%*%hc%*%y
j<-matrix(c(rep(1,n*n)),ncol=n)
sce1<-t(y)%*%(diag(n)-((1/n)*j)-hc)%*%y
gl1<-c(dim(xc)[2],(dim(hc)-dim(xc))[2])
cm1<-c(scr1/gl1[1],sce1/gl1[2])
fcal1<-cm1[1]/cm1[2]
fcri1<-qf(0.05,2,997,lower.tail=F)
pvalor1<-pf(fcal1,2,997,lower.tail = F)

aov(lm(y~x1+x2))
summary(aov(y~x1+x2))


###test sobre un subconjunto de b

n<-1000
y<-rnorm(n,70,1)
x1<-rpois(n,3)
x2<-rnorm(n,0,1)
X1<-matrix(c(rep(1,length(x1)),x1),ncol=2)
h<-x%%solve(t(x)%%x)%*%t(x) 
h1<-X1%%solve(t(X1)%%X1)%*%t(X1)
scr<-t(y)%%(h-h1)%%y
sce<-t(y)%%(diag(n)-h)%%y
gl<-c(1,997)
cm<-c(scr/gl[1],sce/gl[2])
fcal<-cm[1]/cm[2]
fcri<-qf(0.05,1,997,lower.tail=F)
pvalor<-pf(fcal,1,997,lower.tail = F)