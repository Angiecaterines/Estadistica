mu<-c(3,1,2)
cov<-matrix(c(4,0,2,0,1,-1,2,-1,3),ncol=3)
a<-c(1,2,1)


t(a)%*%mu
t(a)%*%cov%*%a

A<-matrix(c(1,-1,1,3,1,-2),ncol=3,byrow = T)
A%*%mu
A%*%cov%*%t(A)	  	
