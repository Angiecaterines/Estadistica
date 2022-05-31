
# Librerias ---------------------------------------------------------------
library(ggplot2)
library(mapview)
library(tidyverse)
library(RColorBrewer)
library(sf)
library(classInt)
library(magic)
library(purr)
library(maptools)
library(readxl)
library(raster) 
library(spdep) 
library(spData)
library(ape)
library(sp)
library(rgdal)
library(rgeos)
library(tmap)
library(spgwr)
library(grid)
library(gridExtra)
library(spatialreg)

# Datos -------------------------------------------------------------------

## casos de coronavirus
casos<- read_csv("C:/Users/Hp/Downloads/Casos_positivos_de_COVID-19_en_Colombia.csv")
casos<- casos%>% rename("NOMBRE_MPI"=`Nombre municipio`,"NOMBRE_DPT"=`Nombre departamento`)
casos_por<-casos %>% group
# shapefile

ctes <- st_read("C:/Users/Hp/Downloads/mpio")
