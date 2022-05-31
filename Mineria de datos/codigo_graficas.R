
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##################################CÓDIGO PARA GRÁFICAS #########################
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
#
#
################################## Carga de los datos   #########################
data<-read.csv2("C:/.../toyota.csv",header = T,sep = ";",dec=".")
data<-read.csv2("C:/Users/Hp/Downloads/toyota.csv",header = T,sep = ";",dec=".")

cuanti<-data.frame(data[,c(1:3,5,8,10)]) #Se extraen las variables cuantitativas

cuanti1<-na.omit(cuanti) #se extran las observaciones incompletas.

#Se extraen las variables cualitativas
cuali<-data.frame(data$FuelType,data$Automatic,
                  data$Color,Doors=factor(data$Doors))

#
#
#
######################################## Histogramas   #########################

library(DataExplorer)

cuanti<-data[,c(1:3,5,8,10)]
plot_histogram(cuanti)
#
#
#
########################################  Dot charts    #########################

par( mfrow=c(3,2),mar=c(2.1,3.1,3.1,2.1) )

stripchart(cuanti$Price,method="jitter",jitter=10,col="orange",pch=19,
           main="Dotchart Price (jitter)",xlab="Precio")

stripchart(cuanti$Age,method="jitter",jitter=10,col="green",pch=19,
           main="Dotchart Age (jitter)",xlab="Edad")

stripchart(cuanti$KM,method="jitter",jitter=10,col="purple",pch=19,
           main="Dotchart KM (jitter)",xlab="Kilometraje")

stripchart(cuanti$HP,method="jitter",jitter=10,col="orange",pch=19,
           main="Dotchart HP (jitter)",xlab="Caballo Fuerza")

stripchart(cuanti$CC,method="jitter",jitter=10,col="green",pch=19,
           main="Dotchart CC (jitter)",xlab="Centímetros cúbicos")

stripchart(cuanti$Weight,method="jitter",jitter=10,col="purple",pch=19,
           main="Dotchart Weight (jitter)",xlab="Kilogramos")
#
#
#
########################################  Box plots    ##########################

par( mfrow=c(3,2),mar=c(2.1,3.1,3.1,2.1) )

boxplot(cuanti$Price,horizontal = T,col="orange",pch=19,
        main="boxplot Price ",xlab="Precio")

boxplot(cuanti$Age,horizontal = T,col="green",pch=19,
        main="boxplot Age ",xlab="Edad")

boxplot(cuanti$KM,horizontal = T,col="purple",pch=19,
        main="boxplot KM ",xlab="Kilometraje")

boxplot(cuanti$HP,horizontal = T,col="red",pch=19,
        main="boxplot HP ",xlab="Caballo Fuerza")

boxplot(cuanti$CC,horizontal = T,col="lightblue",pch=19,
        main="boxplot CC ",xlab="Centímetros cúbicos")

boxplot(cuanti$Weight,horizontal = T,col="tomato1",pch=19,
        main="boxplot Weight ",xlab="Kilogramos")
#
#
#
#################################  Diagramas de barras   ########################

attach(cuali)
plot_bar(cuali)
#
#
#
#####################################  Correlograma   ###########################

library(GGally) ##Paquete de viasualización de datos donde está "ggcor"

ggcorr(cuanti, method = c("everything", "pearson"),
       main="correlograma de variables", )
attach(cuali)
plot_bar(cuali)
#
#
#
###############################  Matriz scatterplots   ##########################

plot_scatterplot(cuanti,by = "Price")
#
#
#
############################# Scatterplots: Dispersion ##########################

library(ggplot2)

ggplot(cuanti,aes(x=Age, y=Price))+ 
  geom_point(col="steelblue")+
  labs(y="Price", 
       x="Age", 
       title="Diagrama de dispersión")

ggplot(cuanti,aes(x=Age, y=Price))+ 
  geom_point(col="steelblue")+
  geom_jitter(width = 0.45,height = 0.05,colour="steelblue")+
  geom_smooth(method=loess , color="red", se=F)+
  labs(y="Price", 
       x="Age", 
       title="Diagrama de dispersión")

ggplot(cuanti,aes(x=Age, y=Price))+
  geom_point(colour="steelblue")+
  geom_count(col="red", show.legend=F) +
  geom_smooth(method=loess , color="red", se=F)+
  labs(y="Price", 
       x="Age", 
       title="Diagrama de dispersión")
#
#
#
############################# Density plot 2D ###################################

library(hexbin)
ggplot(cuanti, aes(x=Age, y=Price) ) +
  geom_hex(bins = 30) +
  labs(y="Price", 
       x="Age", 
       title="Hexabin: densidades")+ theme_bw()
#
#
#
############################### Contour plot ####################################

ggplot(cuanti, aes(x=Age, y=Price) ) +
  stat_density_2d(aes(fill = ..level..), geom = "polygon", colour="white")+
  labs(y="Price", 
       x="Age", 
       title="Plot de contornos")+ theme_bw()
#
#
#
############################ box plot x categorías ##############################

ggplot(data, aes(x=Color, y=Age, fill=Color)) + 
  geom_boxplot(alpha=0.5) +
  theme(legend.position="none")+
  scale_fill_manual(values=c("blue", "grey", "black","red","green"))+
  labs(y="Price", 
       x="Age", 
       title="Box plots por colores de los autos")+ theme_bw()
#
#
#
############################ violin plots x categorías ##########################

ggplot(data, aes(x=Color, y=Age, fill=Color)) + 
  geom_violin(alpha=0.5) +
  theme(legend.position="none")+
  scale_fill_manual(values=c("blue", "grey", "black","red","green"))+
  labs(y="Price", 
       x="Age", 
       title="Violin plot por colores de los autos")+ theme_bw()
#
#
#
############################ Dot plots x categorías #############################

ggplot(data, aes(x=FuelType, y=Price)) + 
  geom_point(col="tomato1", size=3) +   # Dibujar puntos
  geom_segment(aes(x=FuelType, 
                   xend=FuelType, 
                   y=min(Price), 
                   yend=max(Price)), 
               linetype="dashed", 
               size=0.1) +   # Dibujar lineas punteadas
  labs(title="Dot Plot de Precio por tipo de combustible", 
       y="Price",
       x="Tipo de combustible",
       caption="") +  
  coord_flip()
#
#
#
############################ Densidades x categorías ############################

ggplot(data,aes(x=Price, group=FuelType, fill=FuelType)) + 
  geom_density(adjust=1.5 , alpha=0.6)+
  labs(y="Densidad estimada", 
       x="Precio", 
       title="Densidades estimadas por tipo de combustible")+ theme_bw()
#
#
#
############################ Histogramas x categorías ###########################

ggplot(data, aes(Price)) + scale_fill_brewer(palette = "Spectral")+
  geom_histogram(aes(fill=FuelType), 
                 binwidth = 800,    
                 col="black", 
                 size=.1) +  
  labs(y="Frecuencia", 
       x="Precio", 
       title="Tipo de combustible por rangos de precios ($500)")+ theme_bw()
#
#
#
############################  Gráficos de perfiles   ############################

library(FactoClass)
plotct(table(cuali$data.FuelType,cuali$data.Automatic),
       profiles = "row",tables =F,legend.text=T,
       col=c("lightgreen","tomato1"),
       main="Gráfico de perfiles tipo combustible x automático")
#
#
#
#########################  Mapas de calor: Heatmpas   ###########################

tablas.c2<-table(cuali$data.FuelType,cuali$Doors)/length(cuali$data.FuelType)

library(reshape)
t.long<-melt(tablas.c2)

names(t.long)<-c("Combustible","Numero.puertas","Proporcion")

ggplot(data =t.long, aes(x =Combustible, y =Numero.puertas, fill= Proporcion))+
  geom_tile()+
  scale_fill_gradient(low = "lightblue", high = "blue")
#
#
#
####################### Diagrama de barras apiladas   ###########################

ggplot(cuali, aes(data.FuelType))+
  geom_bar(aes(fill=Doors), width = 0.8) + 
  theme(axis.text.x = element_text(angle=90, vjust=0.6)) + 
  labs(title="Diagrama de barras apilado", 
       x="Tipo de combustible",
       y="Frecuencias") 
#
#
#
#######################  Scatter plot categorizado   ############################

library(plotly)

p <- plot_ly(data, x = ~KM, y = ~Weight) 
add_markers(p, color = ~FuelType)
#
#
#
#######################        Scatter plot 3D       ############################

plot_ly(data,x=~KM,y=~HP,z=~Price,type="scatter3d",mode="markers",color=data$Price)
#
#
#
#######################   Bubble Scatter plot 3D    #############################

p <- plot_ly(data, x = ~KM, y = ~Weight) 
add_markers(p, color = ~Price, size = ~Price)
#
#
#
##################### Scatter plot matriz categorizado ##########################

library(ggplot2)
library(GGally)
ggpairs(data, columns=c(1,2,3,5,8,10), aes(color=FuelType)) + 
  ggtitle("Matriz de correlaciones x tipo de combustible")
#
#
#
######## Plano factorial Análisis Componentes principales #######################

library(ggfortify)
autoplot(prcomp(cuanti),main="Primer plano factorial de un ACP",col="steelblue")
#
#
#
############### Plano factorial ACP por categorías ##############################

autoplot(prcomp(data[,c(1,2,3,5,8,10)]), 
         main="Primer plano factorial de un ACP por tipos de combustible", 
         data = data, colour = 'FuelType')
#
#
#
######## Plano factorial Análisis Correspondencias multiple #####################

library(FactoMineR)
#Aplicación del ACM
mca1<-MCA(cuali,graph = F)

cats<-apply(cuali, 2, function(x) nlevels(as.factor(x)))

# coordenadas de variables
vars<-data.frame(mca1$var$coord,Variable = rep(names(cats), cats))

# coordenadas de observaciones
obs<-data.frame(mca1$ind$coord)

# Plano factorial del ACM
ggplot(data = obs, aes(x = Dim.1, y = Dim.2)) +
  geom_hline(yintercept = 0, colour = "gray70") +
  geom_vline(xintercept = 0, colour = "gray70") +
  geom_point(colour = "blue", alpha = 0.9) +
  geom_density2d(colour = "gray80") +
  geom_text(data = vars, 
            aes(x = Dim.1, y = Dim.2, 
                label = rownames(vars), colour = Variable)) +
  ggtitle("Primer plano factorial del ACM: categorías y observaciones") +
  scale_colour_discrete(name = "Variable")

#################################################################################
#################################################################################




