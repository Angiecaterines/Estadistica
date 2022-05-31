library(sf)
library(ggplot2)
library(tmap) 
library(mapview)
library(tidyverse)
library(gdtools)
library(RColorBrewer)

ctes <- st_read("C:/Users/Hp/Downloads/localidades")
ctes$CODIGO_LOC <- c(03,16,19,12,11,15,17,10,09,04,13,01,02,05,20,18,06,14,08,07)
P <- c(501999,139701,110048,404697,457302,199430,673077,1088443,394648,
             887080,1218513,243465,153025,99119,109176,258287,24088,374246,733859,6531)
casos <- matrix(c(23284,19981,43265,8031,7609,15640,5374,5419,10793,16003,14127,30130,12247,
                10336,22583,7946,7356,15302,25526,21606,47132,43590,37851,81441,16734,14727,
                31461,37244,32522,69766,49512,42022,91534,7350,5782,13132,7262,6765,14027,
                4051,4210,8261,4640,4110,8750,13009,12665,25674,1399,1553,2952,15125,14784,29909,21049,17972,39021,
                2,7,19),ncol=3,byrow = T)
ctes <- ctes %>% arrange(CODIGO_LOC) %>% mutate("POBLACION" = P,"FEMENINO" = casos[,1],"MASCULINO" = casos[,2],"TOTAL CASOS"=casos[,3])
BOGOTA <- ctes[c(2,3,11:15)]
mapview(BOGOTA, zcol = c("TOTAL CASOS"),col.regions = brewer.pal(9, "Purples"))


# mapa de casos por upz ---------------------------------------------------

ctes <- st_read("C:/Users/Hp/Downloads/upla")
mapview(ctes)


