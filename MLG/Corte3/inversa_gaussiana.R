
# librerias ---------------------------------------------------------------

library(MASS)
library(DescTools)


# Datos -------------------------------------------------------------------

x1 = rep(c(0,1),8)
x2 = rep(c(0,0,1,1),4)
x3 = rep(c(0,0,0,0,1,1,1,1),2)
x4 = c(rep(0,8),rep(1,8))
y = c(193.4,247.6,168.2,205,303.4,339.9,226.3,208.3,220,256.4,165.7,203.5,285,268,169.1,208.5)

Kurt(y)
sd(y)/mean(y)

modelo1 =glm(y~x1+x2+x3+x4,family = inverse.gaussian(link=identity))

MASS::stepAIC(modelo1)

final = glm(formula = y ~ x1 + x2 + x3, family = inverse.gaussian(link = identity))
summary(final)

1-pchisq(0.00304792-0.00066117,15-12)
PseudoR2(final,"Nagelkerke")

table(residuals(final,type="pearson")>2)
table(residuals(final,type="deviance")>2)



