library(readr)
peso<-c(69,74,68,70,72,67,66,70,76,68,72,79,74,67,66,71,74,75,75,76)
altura<-c(153,175,155,135,172,150,115,137,200,130,140,265,185,112,140,150,165,185,210,220)

sigma<-matrix(c(20,100,100,1000),ncol=2)
mu<-c(70,170)
X<-c(mean(peso),mean(altura))
n<-length(peso)
z2<-n*t((X-mu))%*%solve(sigma)%*%(X-mu)
alpha<-0.05
qu<-qchisq(0.05,2,lower.tail = F)
z2>qu



## con sigma desconocida

y1<-c(35,35,40,10,6,20,35,35,35,30)
y2<-c(3.5,4.9,30.0,2.8,2.7,2.8,4.6,10.9,8.0,1.6)
y3<-c(2.80,2.70,4.38,3.21,2.73,2.81,2.88,2.90,3.28,3.20)
Xi<-matrix(c(y1,y2,y3),ncol=3)
n<-10
p<-3
mu<-c(15.0,6.0,2.85)
X<-c(mean(y1),mean(y2),mean(y3))
j<-matrix(c(rep(1,n*n)),ncol=n)
S<-(1/(n-1))*t(Xi)%*%(diag(n)-(1/n)*j)%*%Xi
T2<-n%*%t(X-mu)%*%solve(S)%*%(X-mu)
v<-n-1
qt2<-16.766


##comparación de dos vectores de medias.

datos <- read_table2("C:/Users/Hp/Downloads/hola.txt")
m<-datos[,1:4]
h<-datos[,5:8]
y1<-round(apply(m, 2, mean),2)
y2<-round(apply(h, 2, mean),2)
s1<-cov(m)
s2<-cov(h)
n1<-nrow(m)
n2<-nrow(h)
p<-ncol(m)
v<-n1+n2-2
w1<-(n1-1)*s1
w2<-(n2-1)*s2
spi<-(1/((n1+n2)-2))*(w1+w2)
T2<-((n1*n2)/(n1+n2))%*%t((y1-y2))%*%solve(spi)%*%(y1-y2)
qT2<-15.373


## con sigma desconocidas pero diferentes

T2<-t(y1-y2)%*%solve((s1/n1)+(s2/n2))%*%(y1-y2)

## Homogeneidad de matrices de covarianza


##test de medias con sigma conocida y desconocida

medias<-function(x,mu,sigma,alpha,covarianza=c("conocida","desconocida")){
	X<-round(apply(x, 2, mean),2)
	n1<-nrow(x)
	if(covarianza=="conocida"){
		z2<-n1*t((X-mu))%*%solve(sigma)%*%(X-mu)
		qu<-qchisq(alpha,2,lower.tail = F)
		print(data.frame(z2,qu)) 
		ifelse(z2>qu ,cat("Rechazo Ho"),cat("No rechazo Ho"))
	}
	if(covarianza=="desconocida"){
		Xi<-x
		n<-nrow(x)
		p<-ncol(Xi)
		j<-matrix(c(rep(1,n*n)),ncol=n)
		S<-(1/(n-1))*t(Xi)%*%(diag(n)-(1/n)*j)%*%Xi
		sigma<-S
		T2<-n%*%t(X-mu)%*%solve(S)%*%(X-mu)
		v<-n-1
		print(data.frame(T2)) 
		print(cat("Busque en la tabla A.7 el cuantil con:",v,p, "grados de libretad"))
	}
	
}
medias(Xi,mu,sigma = 0,covarianza = "desconocida")


##test de diferencia de medias iguales pero desconocidas

dm<-function(m,h,sigma=c("igual","desconocidas")){
	y1<-round(apply(m, 2, mean),2)
	y2<-round(apply(h, 2, mean),2)
	s1<-cov(m)
	s2<-cov(h)
	n1<-nrow(m)
	n2<-nrow(h)
	p<-ncol(m)
	v<-n1+n2-2
	if(sigma=="igual"){
	w1<-(n1-1)*s1
	w2<-(n2-1)*s2
	spi<-(1/((n1+n2)-2))*(w1+w2)
	T2<-((n1*n2)/(n1+n2))%*%t((y1-y2))%*%solve(spi)%*%(y1-y2)
	print(T2)
	print(cat("Busque en la tabla A.7 el cuantil con:",v,p, "grados de libretad"))
	}
if(sigma=="diferente"){
	T2<-t(y1-y2)%*%solve((s1/n1)+(s2/n2))%*%(y1-y2)
	print(T2)
	print(cat("Busque en la tabla A.7 el cuantil con:",v,p, "grados de libretad"))
}
		
}

## test de igualdad de matrices de covarianza
dcv<-function(m,h,alpha){
k<-2
p<-ncol(m)
vi<-c(nrow(m)-1,nrow(m)-1)
si<-list(cov(m),cov(h))
spl<-det((vi[1]*si[[1]]+vi[2]*si[[2]])/sum(vi))
s1<-det(si[[1]])
s2<-det(si[[2]])
M<-((1/2)*(vi[1]*log(s1) + vi[2]*log(s2)))-((1/2)*(vi[1]+vi[2])*log(spl))
c1<-((k+1)*((2*p^2) + (3*p-1)))/(6*k*vi[1]*(p+1))
U<--2*(1-c1)*M
qcu<-qchisq(alpha,(1/2)*(k-1)*(p*(p+1)),lower.tail = F)
print(data.frame("Estadistico"=U,"Cuantil"=qcu)) 
ifelse("Estadistico"<"Cuantil" ,cat("Rechazo Ho"),cat("No rechazo Ho"))
}

dcv(X1,X2,0.05)
