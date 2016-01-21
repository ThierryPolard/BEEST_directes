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

#attribution de la bonne classe à chaque donnée

explo_class <- c( )
for (i in 1:ncol(data_ET))
explo_class[i] <- class(data_ET[,i])

table_explo_class <- cbind(colnames(data_ET), explo_class)



data_ET$Date <- as.Date(data_ET$Date)
data_ET$Mois.Année <- as.Date(data_ET$Mois.Année)
data_ET$Meteo <- as.string(data_ET$Meteo)

data_ET[,4:ncol(data_ET)] <- as.numeric(data_ET[,4:ncol(data_ET)])




