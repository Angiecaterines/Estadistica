mu<-c(2,4, 3.52, 3.53, 3.49 ,3.48, 3.48, 3.54, 3.48 ,3.51,3.52, 3.47, 3.48, 3.53, 3.56, 3.49, 3.51, 3.51, 3.56, 3.53)
r<-c(0.28, 0.21 ,0.19, 0.14, 0.26, 0.17, 0.13, 0.16, 0.15, 0.09,0.18, 0.26, 0.18, 0.12, 0.10, 0.28, 0.18, 0.24, 0.21, 0.20)



carta<-function(mu,r,A2,n){
	xbb<-mean(mu)
	rb<-mean(r)
	limites<-data.frame(LCS=xbb+(A2*rb),LC=xbb,LCI=xbb-(A2*rb))
	medias<-data.frame(valores=mu,muestra=seq(1:n),controla=ifelse(mu>limites$LCS,"no control",ifelse(mu<limites$LCI,"no control","control")))
	a<-medias%>%filter(medias$controla =="no control")
	
	b<-ggplot()+geom_point(data=medias%>%filter(controla=="no control"),aes(x=muestra,y=valores),col="cyan")+
		geom_point(data=medias%>%filter(controla=="control"),aes(x=muestra,y=valores),col="green")+
		geom_line(data=medias,aes(x=muestra,y=valores))+
		geom_hline(yintercept = c(limites$LCS,limites$LC,limites$LCI),col="red")
	print(limites)
	print(b)
}
carta(mu,r,0.58,20)

#### carta con sbarra
xbb<-mean(mu)
sb<-0.0758
A3<-1.43
m<-20
limites<-data.frame(LCS=xbb+(A3*sb),LC=xbb,LCI=xbb-(A3*sb))
medias<-data.frame(valores=medias,muestra=seq(1:m),controla=ifelse(medias>limites$LCS,"no control",ifelse(medias<limites$LCI,"no control","control")))
a<-medias%>%filter(controla=="no control")
ggplot(medias,aes(x=muestra,y=valores))+geom_point(col="blue", size = 5,alpha=0.8)+
	geom_hline(yintercept=c(limites$LCS,limites$LC,limites$LCI),col="cyan") + geom_line()+geom_point(mapping = aes(y=a$valores,x=a$muestra),col="red")+xlab("Muestras")+ylab("Medias")




## rangos moviles

observaciones<-c(102.0,94.8,98.3,98.4,102.0,98.5,99.0,97.7,100.0,98.1,101.3,98.7,101.1,98.4,97.0,96.7,100.3,101.4,97.2,101.0)
abs(diff(observaciones))
rango_m<-c(7.2,3.5,0.1,3.6,3.5,0.5,1.3,2.3,1.9,3.2,2.6,2.4,2.7,1.4,0.3,3.6,1.1,4.2,3.8)
xb<-mean(observaciones)
rmb<-mean(rango_m)
d2<-1.128
limites<-data.frame(LCS=xb+((3*rmb)/d2),LC=xb,LCI=xb-((3*rmb)/d2))
ran<-data.frame(valores=observaciones,muestra=seq(1:length(observaciones)),controla=ifelse(observaciones>limites$LCS,"no control",ifelse(observaciones<limites$LCI,"no control","control")))
ggplot(ran,aes(x=muestra,y=valores))+geom_point(col="blue", size = 5,alpha=0.8)+
	geom_hline(yintercept=c(limites$LCS,limites$LC,limites$LCI),col="cyan") + geom_line()+geom_point()+xlab("observaiones")+ylab("Valor individual")



##función caracteristica

b<-function(l,k,n,phi){
	beta<-phi*(l-(k*sqrt(n)))-phi*(-l-(k*sqrt(n)))
}

##carta r
rb<-mean(rangos)
D4<-0.9
limites<-data.frame(LCS=rb*D4,LC=rb,LCI=rb*D4)
r<-data.frame(valores=datos,muestra=seq(1:length(datos)),controla=ifelse(datos>limites$LCS,"no control",ifelse(datos<limites$LCI,"no control","control")))
ggplot(r,aes(x=muestra,y=valores))+geom_point(col="blue", size = 5,alpha=0.8)+
	geom_hline(yintercept=c(limites$LCS,limites$LC,limites$LCI),col="cyan") + geom_line()+geom_point()+xlab("observaiones")+ylab("Valor individual")


#carta s
#Constantes para construir la carta S

n<-1:15
C4<-c(0,0.7979,0.8862,0.9213,0.94,0.9515,0.9594,0.9659,0.9693,0.9727,0.9754,0.9776,0.9794,0.9810,0.9823)
B3<-c(rep(0,5),0.030,0.118,0.239,0.284,0.321,0.321,0.354,0.382,0.406,0.428)
B4<-c(0,3.267,2.568,2.266,2.089,1.970,1.882,1.815,1.761,1.716,1.679,1.646,1.618,1.594,1.572)
con<-data.frame(n,C4,B3,B4)
sb<-sb(datos)
B4<-0.87
limites<-data.frame(LCS=sb*B4,LC=sb,LCI=sb*B4)
s<-data.frame(valores=datos,muestra=seq(1:length(datos)),controla=ifelse(datos>limites$LCS,"no control",ifelse(datos<limites$LCI,"no control","control")))
ggplot(s,aes(x=muestra,y=valores))+geom_point(col="blue", size = 5,alpha=0.8)+
	geom_hline(yintercept=c(limites$LCS,limites$LC,limites$LCI),col="cyan") + geom_line()+geom_point()+xlab("observaiones")+ylab("Valor individual")


##carta s con limites probabilisticos
n<-5
alpha<-0.05
qs<-qchisq(1-(alpha/2),n-1)
qi<-qchisq((alpha/2),n-1)
limites<-data.frame(LCS=(sb/con[n,2])*sqrt(qs/(n-1)),LC=sb,LCI=(sb/con[n,2])*sqrt(qi/(n-1)))
s<-data.frame(valores=datos,muestra=seq(1:length(datos)),controla=ifelse(datos>limites$LCS,"no control",ifelse(datos<limites$LCI,"no control","control")))
ggplot(s,aes(x=muestra,y=valores))+geom_point(col="blue", size = 5,alpha=0.8)+
	geom_hline(yintercept=c(limites$LCS,limites$LC,limites$LCI),col="cyan") + geom_line()+geom_point()+xlab("observaiones")+ylab("Valor individual")


##carta rm


brm<-mean(rangos)
D4<-0.8
limites<-data.frame(LCS=brm*D4,LC=D4,LCI=brm*D4)

a<-c(-21,-5,21,3,-12)
max(a)-min(a)

