
# 1) ----------------------------------------------------------------------


S <- matrix(c(5,2,2,2),ncol = 2)
vp = eigen(S)
lam = vp$values ; lam
e = vp$vectors ; e

##con rho
rho<-matrix(c(1,S[2]/(sqrt(S[1])*sqrt(S[4])), S[3]/(sqrt(S[1])*sqrt(S[4])),1),ncol=2,byrow = T)
vp = eigen(rho)
lam = vp$values ; lam
e = vp$vectors ; e

# proporci?n de varianza explicada
lam/sum(lam)



# 2) ----------------------------------------------------------------------


corr <- read_excel("C:/Users/Hp/Google Drive/EL BOSQUE/2020-2/Estadística multivariada/corr.xlsx")
corr<-corr[-1]
vp = eigen(corr)
lam = vp$values ; lam[1:3]
e = vp$vectors ; round(e[,c(1:3)],2)

##coordenadas
s1<-NA
for( i in 1:10){
s1[i]<-round(e[1,i]%*%sqrt(lam[1]),2)}

s2<-NA
for( i in 1:10){
	s2[i]<-round(e[2,i]%*%sqrt(lam[2]),2)}

s3<-NA
for( i in 1:10){s
	s3[i]<-round(e[3,i]%*%sqrt(lam[3]),2)}

plot(-1:1, -1:1, type='n', asp=1, xlab='CP1', ylab='CP2')

abline(h=0, v=0, lty=2, col=8)

## Dibuja un cÃ­rculo de centro (0,0) y radio 1
symbols(0, 0, 1, inches=F, add=T)
symbols(0, 0, sqrt(.5), inches=F, add=T)

## Dibuja los vectores y coloca los nombres
arrows(0, 0, s1, s2, length=.1)
text(s1, s2, colnames(corr), pos=3, offset=.4, col="cyan", font=1)

##contribuciones
data.frame(s1,s2)
round(data.frame(Dim1=abs(s1)/sum(abs(s1)),Dim2=abs(s2)/sum(abs(s2)))*100,2)
