### CB=0
set.seed(212)
n<-100
y<-rnorm(n,30,2)
xpar1<-c(y[1:50]*2)
xpar2<-c(y[51:100]*1.5)
x1<-c(xpar1,xpar2)
x2<-rpois(n,30)
x3<-3*x2
x<-matrix(c(rep(1,length(x1)),x1,x2,x3),ncol=4)
c<- matrix(c(1,0,0,1,0,1,-1,0,0,0,-1,3), byrow=T, nrow = 3)
library(MASS)
b<-ginv(t(x)%*%x)%*%t(x)%*%y
h<- x%*%ginv(t(x)%*%x)%*%t(x)
SSR<- t(c%*%b)%*%ginv(c%*%ginv(t(x)%*%x)%*%t(c))%*%(c%*%b)
SSE<- t(y)%*%(diag(n)-h)%*%y
glr<- 3
gle<- n-(ncol(x)-1)-1
CMR<- SSR/glr
CME<-SSE/gle
Fcal<- CMR/CME
Fcri<- qf(0.05, glr,gle,lower.tail = F)
TANOVA<- data.frame(SC=c(SSR,SSE),GL=c(glr,gle), CM=c(CMR,CME), Fcal, Fcri)
TANOVA




##CB=t


C<- matrix(c(1,0,0,1,0,1,0,0,0,1,-1,0,0,1,-3,-1), byrow = T, nrow=4)
T<- matrix(c(0,0.07,0.07,0.07),ncol=1)
SSR1<- t((C%*%b)-T)%*%ginv(C%*%ginv(t(x)%*%x)%*%t(C))%*%((C%*%b)-T)
SSE1<- t(y)%*%(diag(n)-h)%*%y
glr1<-4
gle1<-n-(ncol(x)-1)-1
CMR1<- SCR1/glr1
CME1<- SCE1/gle1
Fcal1<- CMR1/CME1
Fcri1<- qf(0.05, glr1,gle1, lower.tail = F)
TANOVA<- data.frame(SC=c(SSR1,SSE1),GL=c(glr1,gle1), CM=c(CMR1,CME1), Fcal1, Fcri1)
TANOVA
