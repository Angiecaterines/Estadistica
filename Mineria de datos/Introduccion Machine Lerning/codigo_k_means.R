################################################################################
# TAREA: GENERAR GRUPOS DE MUNICIPIOS CON CARACTERÍSTICAS COMUNES ##############
# MÉTODO: K-MEANS PARA CALSIFICACIÓN NO SUPERVISADA ############################
################################################################################

# Librerías necesarias #########################################################
library(tidyverse)

# Importando conjunto de datos #################################################
# df<-read.csv("C:/Users/user/Dropbox/ElBosque2020-II/Asignaturas_2020_I/
#                  MineriaDeDatos/Compartido_DM/datos/cundinamarca.csv", sep=";")

# Estructura de datos ##########################################################
dplyr::glimpse(df)

# Pre-procesamiento de datos ###################################################
train<-df%>%select(-Conf.pres)

# Ajuste de un K-means con k=4 clústers al conjunto de entrenamiento ###########
ajustekm<-kmeans(x =train[,-1],centers = 4,iter.max = 10000,nstart = 1)

# Distribución de los municipios en cada clúster
clusters<-ajustekm$cluster; table(clusters)

# Inclusión de los clusters al conjunto de entrenamiento
train<-mutate(train,clusters)



