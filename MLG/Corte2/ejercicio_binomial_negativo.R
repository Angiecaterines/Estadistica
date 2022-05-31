library(AER)
library(MASS)
datos_binomial_negativa <- read_table2("EL BOSQUE/2022-1/MLG/Corte2/datos_binomial_negativa.txt")
deaths = datos_binomial_negativa$deaths
pob = datos_binomial_negativa$pob
age2534 = rep(c(1,0,0,0),4)
age3544 = rep(c(0,1,0,0),4)
age4554 = rep(c(0,0,1,0),4)
age5564 = rep(c(0,0,0,1),4)
engwales = c(rep(1,4),rep(0,12))
belgium = c(rep(0,4),rep(1,4),rep(0,8))
france = c(rep(0,8),rep(1,4),rep(0,4))
italy = c(rep(0,12),rep(1,4))
modelo1 <- glm.nb(deaths ~pob+age2534+age3544+age4554+age5564+engwales+belgium+france+italy)
summary(modelo1)


# Mejor modelo ------------------------------------------------------------

stepAIC(modelo1,direction = "both")
modelo2 <- glm.nb(deaths ~ age2534 + age3544 + age4554 + engwales + 
                    belgium + france)
summary(modelo2)


# número de muertes -------------------------------------------------------

exp(6.65250 + -2.75207*1 )
