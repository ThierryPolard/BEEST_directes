## premier traitement des données pour BEEST

######################################
######################################

#vider la mémoire des travaux précédents
rm(list=ls())

## définition de l'opération "ni" (l'inverse de "in")
`%ni%` <- Negate(`%in%`)

#install.packages("openxlsx")

### chargement packages
library("ggplot2")   ## pour faire des jolis graphs
library("cowplot")   ## pour  afficher plusieurs ggplot
library("fortunes")
library("lattice")
require("grid")
require("openxlsx")


theme_set(theme_gray())

fortune(15)

######################################
######################################

## Chargement des données

## import des mesure
setwd("C:/Users/XZ4062/Desktop/LyRE/BEEST/Tâches LyRE/P_1 - Synthèse analyse et études/données/Données Seramm")
getwd()

## lecture du fichier excel( packages openxlsx)
data_ET<- read.xlsx("ET.xlsx", sheet = 1, startRow = 1, colNames = TRUE,
          rowNames = FALSE, detectDates = TRUE, skipEmptyRows = FALSE,
          rows = NULL, cols = NULL, check.names = FALSE, namedRegion = NULL)

## modifications des noms de colonnes
colnames(data_ET)<-gsub("/l","l-1",colnames(data_ET))
colnames(data_ET)<-gsub("/j","j-1",colnames(data_ET))


#attribution de la bonne classe à chaque donnée
 #verification des classes initiales
explo_class <- c( )
for (i in 1:ncol(data_ET))
explo_class[i] <- class(data_ET[,i])

table_explo_class <- cbind(colnames(data_ET), explo_class)
table_explo_class

 # modification ponctuelle des classe dates et character
data_ET$Date <- as.Date(data_ET$Date)
data_ET$Mois.Année <- as.Date(data_ET$Mois.Année)
data_ET$Meteo <- as.character(data_ET$Meteo)

 # pour les colonnes classées a tort en character (a cause des virgule) 

# remplacement des virgules par des points
  # x <- "R Tutorial"
  # gsub("ut","ot",x)

n<- ncol(data_ET)
for ( i in 1:n) { 
  data_ET[,i]<-gsub(",",".",data_ET[,i])
  }

# changement des classe en numeric

for ( i in 4:n) { 
  data_ET[,i] <- as.numeric(data_ET[,i])
}

# contrôle des classes à l'issue du traitement-----------

explo_class <- c( )
for (i in 1:ncol(data_ET))
  explo_class[i] <- class(data_ET[,i])

table_explo_class <- cbind(colnames(data_ET), explo_class)
table_explo_class
table_explo_class <- cbind(colnames(data_ET), explo_class)
table_explo_class

#----------------------

## première représentation graphique -----------------


# Définition d'une palette de couleur
#The palette with grey:
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

#pH
G_pH<- ggplot(data=data_ET, aes(x=Date, y=pH)) +
  geom_line() +
  geom_point()
G_pH

#MES
G_MES_mg_l<- ggplot(data=data_ET, aes(x=Date, y= data_ET[,grep ("MEST.mgl-1",names(data_ET))])) +
  geom_line() +
  geom_point()

G_MES_kg_j<- ggplot(data=data_ET, aes(x=Date, y= data_ET[,grep ("MEST.kgj-1",names(data_ET))])) +
  geom_line() +
  geom_point()

plot_grid(G_MES_mg_l, G_MES_kg_j, labels = c("mg/l", "kg/j"), nrow=2, align = "v")

#DCO
G_DCO.nd.mg_l<- ggplot(data=data_ET, aes(x=Date, y= data_ET[,grep ("DCO.nd.mgl-1",names(data_ET))])) +
  geom_line() +
  geom_point()

G_DCO.nd.kg_j<- ggplot(data=data_ET, aes(x=Date, y= data_ET[,grep ("DCO.nd.kgj-1",names(data_ET))])) +
  geom_line() +
  geom_point()

plot_grid(G_DCO.nd.mg_l, G_DCO.nd.kg_j, labels = c("mg/l", "kg/j"), nrow=2, align = "v")

#DBO5
G_DBO5.nd.mg_l<- ggplot(data=data_ET, aes(x=Date, y= data_ET[,grep ("DBO5.nd.mgl-1",names(data_ET))])) +
  geom_line() +
  geom_point()

G_DBO5.nd.kg_j<- ggplot(data=data_ET, aes(x=Date, y= data_ET[,grep ("DBO5.nd.kgj-1",names(data_ET))])) +
  geom_line() +
  geom_point()

plot_grid(G_DBO5.nd.mg_l, G_DBO5.nd.kg_j, labels = c("mg/l", "kg/j"), nrow=2, align = "v")

#NTK
G_NTK.mg_l<- ggplot(data=data_ET, aes(x=Date, y= data_ET[,grep ("NTK.mgl-1",names(data_ET))])) +
  geom_line() +
  geom_point()

G_NTK.kg_j<- ggplot(data=data_ET, aes(x=Date, y= data_ET[,grep ("NTK.kgj-1",names(data_ET))])) +
  geom_line() +
  geom_point()

plot_grid(G_NTK.mg_l, G_NTK.kg_j, labels = c("mg/l", "kg/j"), nrow=2, align = "v")

#NH4
G_NH4.mg_l<- ggplot(data=data_ET, aes(x=Date, y= data_ET[,grep ("N-NH4.mgl-1",names(data_ET))])) +
  geom_line() +
  geom_point()

G_NH4.kg_j<- ggplot(data=data_ET, aes(x=Date, y= data_ET[,grep ("N-NH4.kgj-1",names(data_ET))])) +
  geom_line() +
  geom_point()

plot_grid(G_NH4.mg_l, G_NH4.kg_j, labels = c("mg/l", "kg/j"), nrow=2, align = "v")

#NO2
G_NO2.mg_l<- ggplot(data=data_ET, aes(x=Date, y= data_ET[,grep ("N-NO2.mgl-1",names(data_ET))])) +
  geom_line() +
  geom_point()

G_NO2.kg_j<- ggplot(data=data_ET, aes(x=Date, y= data_ET[,grep ("N-NO2.kgj-1",names(data_ET))])) +
  geom_line() +
  geom_point()

plot_grid(G_NO2.mg_l, G_NO2.kg_j, labels = c("mg/l", "kg/j"), nrow=2, align = "v")

#NO3
G_NO3.mg_l<- ggplot(data=data_ET, aes(x=Date, y= data_ET[,grep ("N-NO3.mgl-1",names(data_ET))])) +
  geom_line() +
  geom_point()

G_NO3.kg_j<- ggplot(data=data_ET, aes(x=Date, y= data_ET[,grep ("N-NO3.kgj-1",names(data_ET))])) +
  geom_line() +
  geom_point()

plot_grid(G_NO3.mg_l, G_NO3.kg_j, labels = c("mg/l", "kg/j"), nrow=2, align = "v")

#NGL
G_NGL.mg_l<- ggplot(data=data_ET, aes(x=Date, y= data_ET[,grep ("N-NGL.mgl-1",names(data_ET))])) +
  geom_line() +
  geom_point()

G_NGL.kg_j<- ggplot(data=data_ET, aes(x=Date, y= data_ET[,grep ("N-NGL.kgj-1",names(data_ET))])) +
  geom_line() +
  geom_point()

plot_grid(G_NGL.mg_l, G_NGL.kg_j, labels = c("mg/l", "kg/j"), nrow=2, align = "v")
#PT

G_PT.mg_l<- ggplot(data=data_ET, aes(x=Date, y= data_ET[,grep ("PT.mgl-1",names(data_ET))])) +
  geom_line() +
  geom_point()

G_PT.kg_j<- ggplot(data=data_ET, aes(x=Date, y= data_ET[,grep ("PT.kgj-1",names(data_ET))])) +
  geom_line() +
  geom_point()

plot_grid(G_PT.mg_l, G_PT.kg_j, labels = c("mg/l", "kg/j"), nrow=2, align = "v")

#chlorure
G_Chlorure<- ggplot(data=data_ET, aes(x=Date, y=Chlorure)) +
  geom_line() +
  geom_point()
G_Chlorure
# --------------------------


#aggrégation des paramètres redondants

# conversion du débit donné en m3/j vers des litres/j
convert.debit <- function (x){
  x*10^3
}
# conversion [c] en mg/l en kg/l
convert.conc <- function (x){
  x/10^6
}


#MES

MEST.kg_j3 <-convert.conc (data_ET[,grep ("MEST.mgl-1",names(data_ET))]) * convert.debit (data_ET[,grep ("DEBIT.m3j-1",names(data_ET))])#conversion  et kg/l et en l/j
test <-cbind (data_ET[,8],MEST.kg_j3)
x11()
plot(data_ET$Date, data_ET[,8], col= "gray")
points (data_ET$Date, MEST.kg_j3, col= "green")




MEST.kg_j2 <-(data_ET[,grep ("MEST.mgl-1",names(data_ET))])/10^6 * data_ET[,grep ("DEBIT.m3j-1",names(data_ET))] *10^3 #conversion  et kg/l et en l/j
test <-cbind (data_ET[,8],MEST.kg_j2)
 x11()
plot(data_ET$Date, data_ET[,8], col= "gray")
points (data_ET$Date, MEST.kg_j2, col= "red")

# DCO









