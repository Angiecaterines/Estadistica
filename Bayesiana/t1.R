library(ggplot2)
mu = 0
alpha = 2
s2 = 1.5
sigma = runif(10000,0.1)
a<-sigma^(-2*(alpha+1))*exp(-s2/(2*(sigma^2)))
hist(a, freq = F,col="Cyan")
lines(density(a),col="Blue")
plot(density(a))
d<-data.frame(sigma,a)

ggplot(d, aes(x = a)) + 
  geom_histogram(aes(y = ..density..), bins = 20, color = "white", fill = rgb(0.1,1,0.5,0.7)) +
  geom_density(color = "blue")+xlab(expression(xi(mu,sigma^2,alpha,s[0]^2)))

ggplot(d,aes(x=a))+geom_histogram(bins = 30,color = "white", fill = rgb(0.1,1,0.5,0.7))+
  xlab(expression(xi(mu,sigma^2,alpha, s[0]^2)))+ylab("Frecuencia")

sigma<-runif(10000,min =0.0001,1 )
a<-dbeta(sigma,28,24)
curve(dbeta(x,28,24),0.0001,1,n=10000,col="Blue")
curve(dbeta(x,1.4,58.2),0.0001,0.5,n=10000,col="Red")
library(invgamma)
a = 29
b <- sum(10,15,8,12,9,)
