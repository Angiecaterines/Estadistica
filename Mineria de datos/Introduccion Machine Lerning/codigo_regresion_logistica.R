################################################################################
# TAREA: PREDECIR ESTATUS DE CONFLICTO: MUNICIPIO DE FÓMEQUE ###################
# MÉTODO: REGRESIÓN LOGÍSTICA PARA CLASIFICACIÓN ###############################
################################################################################

# Librerías necesarias #########################################################
library(tidyverse)

# Importando conjunto de datos #################################################
# df<-read.csv("C:/Users/user/Dropbox/ElBosque2020-II/Asignaturas_2020_I/
#                  MineriaDeDatos/Compartido_DM/datos/cundinamarca.csv", sep=";")


# Extracción de municipio de Fómeque
fomeque<-df[df$Municipio=="Fomeque",]

# Estructura de datos ##########################################################
dplyr::glimpse(df)

# Pre-procesamiento de datos ###################################################
train<-df%>%mutate(Conf.pres=factor(Conf.pres))%>%
            filter(Municipio!="Fomeque")%>%
            select(-Municipio)

# # Estructura de datos ########################################################
dplyr::glimpse(train)

# Ajuste del modelo de regresión logística #####################################
modelo<-glm(formula = Conf.pres~.,family = "binomial",data=train)
summary(modelo)

# Predicción estatus del conflicto para Fómeque (Valor de referecencia "1" conflicto)
prediccion<-predict.glm(object = modelo,newdata = fomeque,type="response")
prediccion
