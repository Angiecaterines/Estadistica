
# Análisis de intervención de la tasa de mortalidad por
# infarto agudo de miocardio (IAM) en Brasil, 1996-2015

# El infarto de miocardio (IM) es la principal causa de 
# muerte en Brasil. En 2004, el gobierno brasileño 
# intervino en el sistema de salud, la medida comenzó a regir
# en febrero de 2005. 

# Se realiza un pronóstico basado en un modelo de intervención. 
# El modelo propuesto se compara con un modelo SARIMA convencional.

## El conjunto de datos de mortalidad por IAM contiene 
## la mortalidad general mensual y la mortalidad por IAM en Brasil
## entre 1996 y 2015, obtenida de http://www2.datasus.gov.br/. 
# Este conjunto de datos contiene las columnas year, month, 
# iam (mortalidad IAM) y gm (mortalidad general).

library(readxl)
ami <- data.frame(read_excel("ami.xlsx"))
head(ami)

## Tasa de mortalidad por IAM:
## La serie de tiempo de interés es la razón entre
## la mortalidad IAM y la mortalidad general para cadda mes
## multiplicada por 100%

ami$ami.rate = ami$iam/ami$gm*100
boxplot(ami.rate ~ year, data = ami, xlab = "Year", ylab = "AMI rate (%)")

rate = ts(ami$ami.rate, start = c(1996,1), freq = 12)
plot(rate, lwd = 1.5, ylab = "AMI rate (%)" , xlab = "Time")
lines(filter(rate, sides = 2, rep(1,12)/12),  lwd  = 1.5, col = 4)

rate0 = window(rate, start = c(1996,1), end = c(2004,12))
rate1 = window(rate, start=c(2005,1), end=c(2015,12))

require(TSA)
par(mfrow = c(1,2))
acf(rate0) ; pacf(rate0)
par(mfrow = c(1,1))

require(forecast)
pt = BoxCox.lambda(rate0) ; pt
fit0 = auto.arima(rate0, lambda = pt, allowdrift = F); fit0
for0 = forecast(fit0, h = 24, level = 95)
plot(for0, xlim = c(1996,2015), ylim = c(5.5,7.5), lwd = 1.5, main = "", xlab = "Time", ylab = "AMI rate (%)")
lines(rate, lwd = 1.5)
abline(v = 2005, lty = 2)

pt1 = BoxCox.lambda(rate) ; pt1
fitA = auto.arima(rate, lambda = pt1, allowdrift = F)
fitA

forA = forecast(fitA, h = 12, level = 95)
plot(forA)

res = resid(fitA)
par(mfrow=c(2,2))
acf(res) ; pacf(res)
qqnorm(res) ; qqline(res)
plot(res,type="p")
par(mfrow=c(1,1))

tem = time(rate) ; tem
x = (tem<2005.083)*1 ; x

fit = arimax(BoxCox(rate, pt1), order = c(2,0,0), seasonal = list(order=c(1,1,0), period=12),
             xtransf = x, include.mean = F, transfer = list(c(1,0)))
fit

np = length(fit$coef)
(fit$coef[np]/(1-fit$coef[np-1]))

set.seed(123)
A = matrix(rnorm(1000),ncol=1)
pre = apply(A,1,function(x) simulate(fit,nsim=12,seed=x))
pron = ts(apply(pre,1,mean),start=c(2016,1),freq=12)
psd = apply(pre,1,sd)
pron.025 = ts(pron-1.96*mean(psd),start=c(2016,1),freq=12) 
pron.975 = ts(pron+1.96*mean(psd),start=c(2016,1),freq=12)

forI = forA
forI$mean = InvBoxCox(pron, pt1)
forI$lower = InvBoxCox(pron.025, pt1)
forI$upper = InvBoxCox(pron.975, pt1)
plot(forI)


# con ggplot2

library(ggplot2)
library(dplyr)
library(readxl)

ami = data.frame(read_excel("ami.xlsx"))

rate1 <- ami %>%
  mutate(ami.rate = ts(iam/gm*100, start = c(1996,1), freq = 12),
         fecha = seq(as.Date("1996-02-01"), by="month", length=240)) %>%
  filter(time(ami.rate)>2005)

p = ggplot(rate1, aes(x=fecha, y=ami.rate)) +
  geom_line() + scale_x_date(date_labels = "%Y %b") +
  xlab("")
p

pt1 = BoxCox.lambda(rate1$ami.rate) ; pt1
fit1 = auto.arima(rate1$ami.rate, lambda = pt1, allowdrift = F)
fit1

for1 = forecast(fit1, h = 12, level = 95)
plot(for1)

pred = as.data.frame(for1)
colnames(pred) = c("ami.rate", "Li", "Ls")

rate1 = rate1 %>%
  mutate(Li = NA, Ls = NA) %>%
  select(ami.rate, Li, Ls)
  
rate1.p = bind_rows(rate1, pred) ; rate1.p

rate1.p$fecha = seq(as.Date("1996-02-01"), by="month", length=nrow(rate1.p))

p = ggplot(rate1.p, aes(x=fecha, y=ami.rate)) +
  geom_line(aes(y = Li), color = "blue") +
  geom_line(aes(y = Ls), color = "blue") +
  geom_line() + scale_x_date(date_labels = "%Y %b") +
  xlab("") + geom_ribbon(aes(ymin=Li, ymax=Ls), fill = "grey70", alpha=0.5)
p

