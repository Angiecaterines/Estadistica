# generacion de un modelo ar(1)
# parametros phi,sza,mu
# generar una serie de tama?o N=10000
g<-function(N,phi=0.5,sza=1,mu=0){
  at<-rnorm(N,0,sqrt(sza))
  zt<-numeric(0)
  for (i in 1:N) {
   zt[i]<- ifelse(i==1,mu+at[i],mu+phi*(zt[i-1]-mu)+at[i])
  }
  zt
}
a<-g(1000)
b<-g(1000,-0.5)
ca<-g(1000,mu=5)
d<-g(1000,1)
e<-g(20)
plot(e,type = "l")

acf(e)
pacf(e)


# generacion de un modelo ma(1)
# parametros phi,sza,mu
# generar una serie de tama?o N=10000
g<-function(N,the=0.8,sza=1,mu=0){
  at<-rnorm(N,0,sqrt(sza))
  zt<-numeric(0)
  for (i in 1:N) {
    zt[i]<- ifelse(i==1,mu+at[i],mu-the*(zt[i-1]-mu))
  }
  zt
}
a<-g(1000)
b<-g(1000,-0.5)
ca<-g(1000,mu=5)
d<-g(1000,1)
e<-g(50)
plot(e,type = "l")

acf(e)
pacf(e)

