## premier traitement des données pour BEEST

######################################
######################################

#vider la mémoire des travaux précédents
rm(list=ls())

## définition de l'opération "ni" (l'inverse de "in")
`%ni%` <- Negate(`%in%`)

#install.packages("openxlsx")
#install.packages("ggthemes")


### chargement packages
require("ggplot2")   ## pour faire des jolis graphs
library("cowplot")   ## pour  afficher plusieurs ggplot
library("fortunes")
library("lattice")
library("ggthemes")
library("grid")
library("openxlsx")

#theme_set(theme_gray())

my.theme = theme_grey() + theme(text=element_text(family='Roboto', size=30))

fortune(15)

######################################
######################################

## Chargement des données

## import des mesure
setwd("C:/Users/XZ4062/Desktop/LyRE/BEEST/Tâches LyRE/P_1 - Synthèse analyse et études/données/Données Seramm")
getwd()

## lecture du fichier excel(packages openxlsx)
## Eau traitée
data_ET<- read.xlsx("ET.xlsx", sheet = 1, startRow = 1, colNames = TRUE, 
            rowNames = FALSE, detectDates = TRUE, skipEmptyRows = FALSE,
            rows = NULL, cols = NULL, check.names = FALSE, namedRegion = NULL)

## Réseau sanitaire  (= eaux usées)
data_RS<- read.xlsx("RS.xlsx", sheet = 1, startRow = 1, colNames = TRUE, 
            rowNames = FALSE, detectDates = TRUE, skipEmptyRows = FALSE,
            rows = NULL, cols = NULL, check.names = FALSE, namedRegion = NULL)
## Réseau unitaire
data_RU<- read.xlsx("RU.xlsx", sheet = 1, startRow = 1, colNames = TRUE,
            rowNames = FALSE, detectDates = TRUE, skipEmptyRows = FALSE,
            rows = NULL, cols = NULL, check.names = FALSE, namedRegion = NULL)

## Liste des sources 
sources <- c("ET", "RS", "RU")

## modifications des noms de colonnes (de /j et /l en j-1 et l-1 pour moins de confusion)
n <- length (sources) 

for (i in 1:n){
    eval(parse(text=paste0("colnames(data_",sources[i],")<-gsub(\".mg/l\",\"_conc\",colnames(data_",sources[i],"))"))) 
    eval(parse(text=paste0("colnames(data_",sources[i],")<-gsub(\".kg/j\",\"_flux\",colnames(data_",sources[i],"))")))  
}

#attribution de la bonne classe à chaque donnée

## exemple pour ET

#verification des classes initiales
explo_class <- c( )

n <-ncol(data_ET)

for (i in 1:n)
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
for ( i in 4:n) { 
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

######## solution automatisée pour les autres------

# modification ponctuelle des classe dates et character
n <- length (sources) 

for (i in 1:n){
  eval(parse(text=paste0("data_",sources[i],"$Date <- as.Date(data_",sources[i],"$Date)")))
  eval(parse(text=paste0("data_",sources[i],"$Mois.Année <- as.Date(data_",sources[i],"$Mois.Année)")))
  eval(parse(text=paste0("data_",sources[i],"$Meteo <- as.character(data_",sources[i],"$Meteo)")))
}

# pour les colonnes classées a tort en character (a cause des virgule)  
  # remplacement des virgules par des points et modification du format
m <- length (sources) 
n <- ncol(data_ET)

for (i in 1:m)
for ( j in 4:n) { 
  eval(parse(text=paste0("data_",sources[i],"[,",j,"]<-gsub(\",\",\".\",data_",sources[i],"[,",j,"])")))
  eval(parse(text=paste0("data_",sources[i],"[,",j,"] <- as.numeric(data_",sources[i],"[,",j,"])")))
}



## première représentation graphique -----------------


# Définition d'une palette de couleur
#The palette with grey:
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# fonction de création des graph

# fonction 'graph'  - un plot de base
graph <- function (A, B){
  plot (A,B)
}

## EXEMPLE : 
# graph( data_ET$Date,data_ET[,15])

# fonction graph0 - avec ggplot              
graph0 <- function (A,B,C){
  df<-A
  df$xVar <- df[,grep(B,names(df))]
  df$yVar <- df[,grep(C,names(df))]
  G_x<-ggplot(data =df, aes(x=xVar, y=yVar)  ) +
    geom_point()  +
    theme_bw()
  return(G_x)
}


# fonction GRAPH 6 construction d'autant de graphs que de sources 
GRAPH <- function(param, sources){    
  n <- length(sources)
  for (i in 1:n) {
    eval(parse(text=paste0("data <- graph0(data_",sources[i],", \"Date\", \"",param,"\")+
                           scale_x_date (name=\"\") +
                           scale_y_continuous (name=\"",param,"\")+
                           ggtitle(\"",sources[i],"\")")))
    print("ok") 
    assign(paste("G",param,sources[i],sep="_"),data,.GlobalEnv)
  } 
}

# EXEMPLE
# GRAPH ("MEST_flux", sources)

## affichage automatisé des graphs

mergePlot <- function (param,sources){
  GRAPH (param, sources)
  n<-length(sources)
  liste_temp <- paste0("G_",param,"_",paste0(sources))
  liste_graph <-paste(liste_temp,collapse=",")
  X11()
  eval(parse(text=paste0("plot_grid(",liste_graph,", nrow=",n,", align = \"v\")")))
}

##EXEMPLE
mergePlot("MEST_conc", sources)

mergePlot("DCO.nd_flux", sources)



#######################################################################################
###########################################################################################
#############

# construction des graphs

#pH
param <- "pH"
n <-length(sources)
for (i in 1:n) {
  eval(parse(text=paste0("G_",param,"_",sources[i],"<- graph0(data_",sources[i],", \"Date\", \"",param,"\")+ 
                                scale_x_date (name=\"\") +
                                scale_y_continuous (name=\"",param,"\")+
                                ggtitle(\"",sources[i],"\")")))           
}


## affichage des graphs
## version manuelle                                                                                          ---------------
##x11()
##plot_grid(G_pH_ET, G_pH_RS,G_pH_RU, labels = c("ET", "RS", "RU"), nrow=3, align = "v")
## version automatisée                                                                                       ----------------
n <-length(sources)
liste_temp <- paste0("G_",param,"_",paste0(sources))
liste_graph <-paste(liste_temp,collapse=",")

X11()
eval(parse(text=paste0("plot_grid(",liste_graph,", nrow=",n,", align = \"v\")")))
#--------------------
#MES


###
graph0 <- function (A,B,C){
  df<-A
  df$xVar <- df[,grep(B,names(df))]
  df$yVar <- df[,grep(C,names(df))]
  G_x<-ggplot(data =df, aes(x=xVar, y=yVar)  ) +
    geom_point()  +
    theme_bw()
  return(G_x)
}
###

GRAPH <- function(param, sources){    
  n <- length(sources)
  for (i in 1:n) {
    eval(parse(text=paste0("data <- graph0(data_",sources[i],", \"Date\", \"",param,"\")+
                         scale_x_date (name=\"\") +
                         scale_y_continuous (name=\"",param,"\")+
                         ggtitle(\"",sources[i],"\")")))
   print("ok") 
   assign(paste("G",param,sources[i],sep="_"),data,.GlobalEnv)
  } 
}
                                                                                         
GRAPH ("MEST_conc", sources)


## affichage automatisé des graphs

mergePlot <- function (param,sources){
  GRAPH (param, sources)
  liste_temp <- paste0("G_",param,"_",paste0(sources))
  liste_graph <-paste(liste_temp,collapse=",")
  X11()
  eval(parse(text=paste0("plot_grid(",liste_graph,", nrow=",n,", align = \"v\")")))
}

mergePlot("MEST_flux", sources)

########


G_MES_mg_l <- graph0(data_ET, "Date", "MEST.mgl-1")
G_MES_kg_j <- graph0(data_ET, "Date", "MEST.kgj-1")

G_DCO.nd.mg_l <- graph0(data_ET, "Date", "DCO.nd.mgl-1")
G_DCO.nd.kg_j <- graph0(data_ET, "Date", "DCO.nd.kgj-1")


G_DBO5.nd.mg_l<- graph0(data_ET, "Date", "DBO5.nd.mgl-1")
G_DBO5.nd.kg_j<- graph0(data_ET, "Date", "DBO5.nd.kgj-1")


G_NTK.mg_l<- graph0(data_ET, "Date", "NTK.mgl-1")
G_NTK.kg_j<- graph0(data_ET, "Date", "NTK.kgj-1")

G_NH4.mg_l<- graph0(data_ET, "Date", "N-NH4.mgl-1")
G_NH4.kg_j<- graph0(data_ET, "Date", "N-NH4.kgj-1")

G_NO2.mg_l<- graph0(data_ET, "Date", "N-NO2.mgl-1")
G_NO2.kg_j<- graph0(data_ET, "Date", "N-NO2.kgj-1")

G_NO3.mg_l<- graph0(data_ET, "Date", "N-NO3.mgl-1")
G_NO3.kg_j<- graph0(data_ET, "Date", "N-NO3.kgj-1")

G_NGL.mg_l<- graph0(data_ET, "Date", "N-NGL.mgl-1")
G_NGL.kg_j<- graph0(data_ET, "Date", "N-NGL.kgj-1")


G_PT.mg_l<- graph0(data_ET, "Date", "PT.mgl-1")
G_PT.kg_j<- graph0(data_ET, "Date", "PT.kgj-1")

G_Chlorure<- graph0(data_ET, "Date", "Chlorure")


## affichage des graphs


G_pH <- graph0(data_ET, "Date", "pH")
G_pH

plot_grid(G_MES_mg_l, G_MES_kg_j, labels = c("mg/l", "kg/j"), nrow=2, align = "v")

plot_grid(G_DCO.nd.mg_l, G_DCO.nd.kg_j, labels = c("mg/l", "kg/j"), nrow=2, align = "v")

plot_grid(G_DBO5.nd.mg_l, G_DBO5.nd.kg_j, labels = c("mg/l", "kg/j"), nrow=2, align = "v")

plot_grid(G_NTK.mg_l, G_NTK.kg_j, labels = c("mg/l", "kg/j"), nrow=2, align = "v")

plot_grid(G_NH4.mg_l, G_NH4.kg_j, labels = c("mg/l", "kg/j"), nrow=2, align = "v")

plot_grid(G_NO2.mg_l, G_NO2.kg_j, labels = c("mg/l", "kg/j"), nrow=2, align = "v")

plot_grid(G_NO3.mg_l, G_NO3.kg_j, labels = c("mg/l", "kg/j"), nrow=2, align = "v")

plot_grid(G_NGL.mg_l, G_NGL.kg_j, labels = c("mg/l", "kg/j"), nrow=2, align = "v")

plot_grid(G_PT.mg_l, G_PT.kg_j, labels = c("mg/l", "kg/j"), nrow=2, align = "v")

G_Chlorure
# --------------------------


#aggrégation des paramètres redondants

# conversion du débit donné en m3/j vers des litres/j
## création de la fonction
convert.debit <- function (x){
  x*10^3
}

## application de la fonction à la colonne débit
debit<-convert.debit (data_ET[,grep ("DEBIT.m3j-1",names(data_ET))])
## modification du nom de la colonne
#### a faire

# conversion de la concentration des colonnes en mg/l vers le  kg/l
## création de la fonction
convert.conc <- function (x){
  x/10^6
}


## application de la fonction à toutes les colones en mgll-1
df<-data_ET[,grep ("mgl-1",names(data_ET))] ## recupère ces colones dans un data.frame
df <-convert.conc(df)  ## appliquela fonction

## modifications des noms de colonnes
#colnames(df)<-gsub("mgl-1","kg_l",colnames(conv_unit))


## multiplication de ces colonnes converties (dans le df) avec celle contenantle débit pour obtenir une autre valeur en kg/j
kg_j_2 <-df
kg_j_2 <-df * debit

colnames(kg_j_2)<-gsub("mgl-1","kg_j",colnames(kg_j_2))

head(kg_j_2)



# Fusionner les deux tables
data_ET_2 <- cbind (data_ET,kg_j_2)

# Représenter graphique les 2 jeux de données pour chaque paramètre

# MES



graph2<-ggplot(data_ET_2, aes(Date)) + 
  geom_point(aes(y = data_ET_2[,grep ("MEST.kgj-1",names(data_ET_2))]), colour = "red", alpha = 0.5) + 
  geom_point(aes(y = data_ET_2[,grep ("MEST.kg_j",names(data_ET_2))]),  colour = "gray" , alpha = 0.5)+
  theme_bw()
x11()
graph2



# Définition de la fonction de construction des graphs
graph2 <- function (A,B,C,D){
  df<-A
  df$xVar <- df[,grep(B,names(df))]
  df$y1Var <- df[,grep(C,names(df))]
  df$y2Var <- df[,grep(D,names(df))]
  G_x<-ggplot(data =df, aes(x=xVar)) +
    geom_point(aes(y =y1Var), colour = "red", alpha = 0.5) + 
    geom_point(aes(y =y2Var), colour = "gray", alpha = 0.5) + 
    theme_bw()
  return(G_x)
}

G_MES <- graph2 (data_ET_2,"Date",
        "MEST.kgj-1",
        "MEST.kg_j"
        )

G_DCO <- graph2 (data_ET_2,"Date",
                 "DCO.nd.kgj-1",
                 "DCO.nd.kg_j"
                 )


G_DBO5 <- graph2 (data_ET_2,"Date",
                 "DBO5.nd.kgj-1",
                 "DBO5.nd.kg_j"
                 )

G_NTK <- graph2 (data_ET_2,"Date",
                  "NTK.kgj-1",
                  "NTK.kg_j"
                  )

G_NH4 <- graph2 (data_ET_2,"Date",
                 "N-NH4.kgj-1",
                 "N-NH4.kg_j"
                 )

G_NO2 <- graph2 (data_ET_2,"Date",
                 "N-NO2.kgj-1",
                 "N-NO2.kg_j"
                 )

G_NO3 <- graph2 (data_ET_2,"Date",
                 "N-NO3.kgj-1",
                 "N-NO3.kg_j"
                 )
G_NGL <- graph2 (data_ET_2,"Date",
                 "NGL.kgj-1",
                 "NGL.kg_j"
                 )

G_PT <- graph2 (data_ET_2,"Date",
                 "PT.kgj-1",
                 "PT.kg_j"
)

X11()
plot_grid(G_NTK, G_NH4,G_NO2,G_NO3 ,G_NGL,G_PT,
          nrow=3, align = "v")



X11()
plot_grid(G_MES, G_DBO5,G_DCO,G_NO3 ,G_NGL,
          nrow=3, align = "v")

