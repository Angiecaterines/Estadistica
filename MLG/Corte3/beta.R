
# librerias ---------------------------------------------------------------

library(MASS)
library(betareg)
library(DescTools)


# datos -------------------------------------------------------------------

percent_hosp <- c(17,39,38,48,30,25,5,4,48,4,26,15,28,34,31,4,6,39,41,45,13,42,28,31,48,41,9,13,44,16)
percent_hosp <- percent_hosp/100
hospi_loc <- c(1,1,0,1,1,0,0,1,1,0,1,0,0,0,0,0,0,0,0,1,0,1,0,0,1,1,0,0,0,1)
hosp_type <-c(1,0,0,0,1,1,0,1,1,0,1,0,1,1,0,0,0,1,1,0,0,0,1,0,0,0,0,1,0,0)
n_beds <-c(56,144,61,186,132,589,53,73,154,38,318,35,184,173,63,91,77,237,56,43,64,193,363,600,468,311,65,44,479,72)


a <- betareg(percent_hosp ~hospi_loc + hosp_type+n_beds)
b<-betareg(percent_hosp ~ hospi_loc)
cc<-betareg(percent_hosp ~ hospi_loc+hosp_type)
d <- betareg(percent_hosp ~ hospi_loc+hosp_type+n_beds)



c(AIC(a),AIC(b),AIC(cc),AIC(d),AIC(e))
e<-betareg(percent_hosp ~ hospi_loc+n_beds)
betas <- summary(e)
1- pchisq(20.78-15.28,2)
lrtest(e)

######## estimación de 
x <-c(1,1,50)
beta <-betas$coefficients$mean[,1]

exp(x%*%beta)/(1 + exp(x%*%beta))


# Residuales --------------------------------------------------------------



table(residuals(e)>2)
