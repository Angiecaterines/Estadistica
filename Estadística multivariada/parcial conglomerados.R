x1<-c(2,1.2,0.5,3,3,3.5)
x2<-c(2,2,1.5,2,2.5,2.3)
X<-matrix(c(x1,x2),ncol=2)
plot(x=x1,y=x2)
filas<-matrix(rep(NA,6*6),ncol=6,byrow = T)

for(i in 1:6){
	for(j in 1:6){
		filas[i,j]<-round(sqrt(sum((X[i,]-X[j,])^2)),2)
	}}
#3) individuos mas cerca
#los grupos mas cercanos son 3 y 6

distancia<-dist(X,method = "euclidean")
d4 = hclust(distancia, method = "complete")
plot(sort(d4$height, decreasing = T)[1:20], type = "h")
plot(d4, hang = -1)

d5 = hclust(distancia, method = "single")
plot(sort(d5$height, decreasing = T)[1:20], type = "h")
plot(d5, hang = -1)



p<-data.frame(matrix(c(0,1,1,1,0,1,0,1,1,1,0,0,1,1,1,0,0,1),ncol=6))
rownames(p)<-c("A","B","C")
distanciap<- dist(p, method = "binary")
distanciap<- dist(p, method = "")
round(head(as.matrix(distanciap)),2)
dp = hclust(distanciap, method = "ward.D2") 
plot(dp,hang = -1)



