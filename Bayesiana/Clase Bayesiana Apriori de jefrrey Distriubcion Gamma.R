set.seed(1)
y = rgamma(10, 2,5) ; y

curve(dgamma(x,2,5), 0, 2)
lines(density(y, from = 0), col = 2)
      
pos1 = function(the, y){
  a = the[1] ; B = the[2]
  n = length(y)
  B^(n*a-1)/(gamma(a))^n*(prod(y))^a*exp(-n*B*mean(y))*(a*trigamma(a)-1)^.5
}
require(cubature)
k = 1/cubintegrate(pos1, c(0,0), c(20,20), y=y)$integral ; k


pos = function(the, y){
  a = the[1] ; B = the[2]
  n = length(y)
  k*(a*trigamma(a)-1)^.5*B^(n*a-1)/(gamma(a))^n*(prod(y))^a*exp(-n*B*mean(y))
}

cubintegrate(pos, c(0,0), c(20,20), y=y)$integral

pos2 = function(the2, y){
  m = the2[1] ; v = the2[2]
  pos(c(m^2/v,m/v), y)*m^2/v^3
}
