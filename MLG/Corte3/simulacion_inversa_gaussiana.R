
# datos -------------------------------------------------------------------

x1 = rep(c(0,1),8)
x2 = rep(c(0,0,1,1),4)
x3 = rep(c(0,0,0,0,1,1,1,1),2)
x4 = c(rep(0,8),rep(1,8))

y = c(193.4,247.6,168.2,205,303.4,339.9,226.3,208.3,220,256.4,165.7,203.5,285,268,169.1,208.5)
x = cbind(1,x1,x2,x3,x4)

iter=100
beta=matrix(ncol=5,nrow=iter+1)
beta[1,]=c(0.05,0.05,0.05,0.05,0.05)

phi =matrix(ncol=1,nrow=iter+1)
phi[1,]=c(0.05)


for (i in 1:iter) {
  mu = x%*%beta[i,]
  v = ((2*y)- mu)/2
  num = sum((1/(2*phi[i,]^2)) - (y/((phi[i,]^3)*as.vector(mu^2))) + (2/(as.vector(mu)*phi[i,]^3)) - (1/(y*phi[i,]^3)))
  den = sum(-(1/(2*phi[i,])) + (y/(2*(phi[i,]^2)*as.vector(mu^2))) - (1/(as.vector(mu)*phi[i,]^2)) + (1/(2*y*phi[i,]^2)))
  phi[i+1,] = phi[i,] + (num/den)
  w = diag(1/(phi[i,]*as.vector(mu^3)))
  beta[i+1,]=solve(t(x)%*%w%*%x)%*%t(x)%*%w%*%(x%*%beta[i,]+v)
}
