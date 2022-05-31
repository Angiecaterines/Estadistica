###quiz

#punto 1

library(readr)
library(MASS)
tabla1 <- read_table2("~/EL BOSQUE/2019-2/Modelos Lineales/tabla1.txt")
tabla1<-as.matrix(tabla1)
## Ho:B=0
## Ha:B!=0

y<-tabla1[,1]

x1<-tabla1[,2]
x2<-tabla1[,3]
x3<-tabla1[,4]
x<-matrix(c(rep(1,length(x1)),x1,x2,x3),ncol=4)
h<-x%*%solve(t(x)%*%x)%*%t(x)       
scr<-t(y)%*%h%*%y
sce<-t(y)%*%(diag(33)-h)%%y
gl<-c(dim(x)[2],(dim(h)-dim(x))[2])
cm<-c(scr/gl[1],sce/gl[2])
fcal<-cm[1]/cm[2]
fcri<-qf(0.05,gl[1],gl[2],lower.tail=F)
pvalor<-pf(fcal,gl[1],gl[2],lower.tail = F)

### Con un nivel de significancia de 0.05 se rechaza Ho  


##Ho:B3=0

c<- matrix(c(0,0,0,1), byrow=T, nrow = 1)
library(MASS)
b<-ginv(t(x)%*%x)%*%t(x)%*%y
h<- x%*%ginv(t(x)%*%x)%*%t(x)
SSR<- t(c%*%b)%*%ginv(c%*%ginv(t(x)%*%x)%*%t(c))%*%(c%*%b)
SSE<- t(y)%*%(diag(33)-h)%*%y
glr<- 1
gle<- 33-(ncol(x)-1)-1
CMR<- SSR/glr
CME<-SSE/gle
Fcal<- CMR/CME
Fcri<- qf(0.05, glr,gle,lower.tail = F)
TANOVA<- data.frame(SC=c(SSR,SSE),GL=c(glr,gle), CM=c(CMR,CME), Fcal, Fcri)
TANOVA


##Se rechaza ho

## Ho:3b2-2b1=3

C<- matrix(c(0,-2,3,0), byrow = T, nrow=1)
T<- matrix(c(3),ncol=1)
SSR1<- t((C%*%b)-T)%*%ginv(C%*%ginv(t(x)%*%x)%*%t(C))%*%((C%*%b)-T)
SSE1<- t(y)%*%(diag(33)-h)%*%y
glr1<-1
gle1<-33-(ncol(x)-1)-1
CMR1<- SSR1/glr1
CME1<- SSE1/gle1
Fcal1<- CMR1/CME1
Fcri1<- qf(0.05, glr1,gle1, lower.tail = F)
TANOVA<- data.frame(SC=c(SSR1,SSE1),GL=c(glr1,gle1), CM=c(CMR1,CME1), Fcal1, Fcri1)
TANOVA

##Se rechaza ho


tabla2 <- read_table2("~/EL BOSQUE/2019-2/Modelos Lineales/tabla2.txt")
tabla2<-as.matrix(tabla2)

## Ho:B=0
## Ha:B!=0

y2<-tabla2[,1]
x12<-tabla2[,2]
x22<-tabla2[,3]
x32<-tabla2[,4]
x42<-tabla2[,5]
x2<-matrix(c(rep(1,length(x12)),x12,x22,x32,x42),ncol=5)
h2<-x2%*%solve(t(x2)%*%x2)%*%t(x2)       
scr2<-t(y2)%*%h2%*%y2
sce2<-t(y2)%*%(diag(26)-h2)%%y2
gl2<-c(dim(x2)[2],(dim(h2)-dim(x2))[2])
cm2<-c(scr2/gl2[1],sce2/gl2[2])
fcal2<-cm2[1]/cm2[2]
fcri2<-qf(0.05,gl2[1],gl2[2],lower.tail=F)
pvalor2<-pf(fcal2,gl2[1],gl2[2],lower.tail = F)

#se rechaza ho es decir 


#B1=0
#B1!=0

n<-nrow(x2)
xc<-matrix(c(x12-mean(x12),x22-mean(x22),x32-mean(x32),x42-mean(x42)),ncol=4)
hc<-xc%*%solve(t(xc)%*%xc)%*%t(xc)       
scr1<-t(y2)%*%hc%*%y2
j<-matrix(c(rep(1,n*n)),ncol=n)
sce1<-t(y2)%*%(diag(n)-((1/n)*j)-hc)%*%y2
gl1<-c(dim(xc)[2],(dim(hc)-dim(xc))[2])
cm1<-c(scr1/gl1[1],sce1/gl1[2])
fcal1<-cm1[1]/cm1[2]
fcri1<-qf(0.05,gl1[1],gl1[2],lower.tail=F)
pvalor1<-pf(fcal1,gl1[1],gl1[2],lower.tail = F)

##Se rechaza ho


##Ho: 4B1-3B3=5

C<- matrix(c(0,4,0,-3,0), byrow = T, nrow=1)
Ta<- matrix(c(5),ncol=1)
b<-ginv(t(x2)%*%x2)%*%t(x2)%*%y2
h<- x2%*%ginv(t(x2)%*%x2)%*%t(x2)
SSR1<- t((C%*%b)-Ta)%*%ginv(C%*%ginv(t(x2)%*%x2)%*%t(C))%*%((C%*%b)-Ta)
SSE1<- t(y2)%*%(diag(n)-h)%*%y2
glr1<-1
gle1<-n-(ncol(x)-1)-1
CMR1<- SSR1/glr1
CME1<- SSE1/gle1
Fcal1<- CMR1/CME1
Fcri1<- qf(0.05, glr1,gle1, lower.tail = F)
TANOVA<- data.frame(SC=c(SSR1,SSE1),GL=c(glr1,gle1), CM=c(CMR1,CME1), Fcal1, Fcri1)
TANOVA

##Se rechaza ho


