getwd()
setwd("/media/pablo/data1/emily/from_oldclust/scripts/gis")
list.files()
rm(list=ls()) #clear memory
gc() #clear cache

#load/install pkgs-------------------------
library(elevatr)
library(raster)
library(rgdal)
library(rgbif)
library(sp)
library(dplyr)

#load datasets-----------------------------
setwd("/media/pablo/data1/emily/from_oldclust/scripts/phylo")
amphibia <- read.table("data_amphibia.txt", h=T, sep="\t")
str(amphibia)
data.frame(amphibia)

reptiles <- read.table("data_reptilia.txt", h=T, sep="\t")
str(reptiles)
setwd("/media/pablo/data1/emily/from_oldclust/scripts/gis")
#create new dataframe with only lat/long---
locations.amph <- amphibia #create new dataframe that is a copy of original datframe
locations.amph <- locations.amph[,-c(1:13,15,17:24)] #remove columns based on their integer ID
str(locations.amph)
summary(locations.amph)

locations.rep <- reptiles #create new dataframe that is a copy of original datframe
locations.rep <- locations.rep[,-c(1:13,15,17:24)] #remove columns based on their integer ID
str(locations.rep)

# getting elevation point data using rgbif--------------------

user <-"egiles"

elevation.amph<-elevation(latitude=amphibia$Latitude, longitude=amphibia$Longitude, username=user)
head(amphibia)
head(elevation.amph)
amphibia2<-cbind(amphibia, elev_mtr=elevation.amph$elevation_geonames)
head(amphibia2)
write.table(amphibia2,"amphibia2.txt",sep="\t",row.names=FALSE)
amphibia2$elev_mtr

elevation.rep<-elevation(latitude=reptiles$Latitude, longitude=reptiles$Longitude, username=user)
reptiles2<-cbind(reptiles,elev_mtr=elevation.rep$elevation_geonames)
head(reptiles2)
write.table(reptiles2,"reptiles2.txt",sep="\t",row.names=FALSE)
reptiles2$elev_mtr
?rgbif

#---------------------------------------------------------------
# ELEVATR ONLY FOR US Locations #------------------------------
#---------------------------------------------------------------
  
  #setting the projection of the points
  points.amph<-SpatialPoints(locations.amph,proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
prj_all <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

crs(points.amph)

#extract elevation for all points ---------
amphibia_elevation<- get_elev_point(locations=points.amph, prj=prj_all, units="meters")
data.frame(amphibia_elevation)
str(amphibia_elevation)
----------------------------------------------------------------
  

----------------------------------------------------------------
  #### MQC: para extraer datos de capas raster y calcular distancias geograficas entre localidades (lat, lon)
  
  library("sp")
library("raster")

### Directorio de Trabajo
### Defino la carpeta de trabajo y la carpeta con los datos

work.dir <- "D:/extrac_values" #carpeta de trabajo
data.dir <- "D:/extrac_values" # carpeta con los datos

# Cambio a la carpeta de trabajo

setwd(work.dir)

# Localidades de A. reigi

locsreigi <- read.csv("LOCS_Reigi.csv", header=TRUE, sep=',', stringsAsFactors=F) # A. reigi
locsreigi
plot (locsreigi)
locs_reigi <- list.files(pattern = "*.csv")

# Cargarmos primero las capas con la data

#las capas deben estar en el directorio de trabajo

env_layers <- stack(list.files(path=data.dir,pattern='asc', full.names=TRUE)) # el stack permite extraer la informacion de multiples capas
# pero igual funciona para una sola

projection(env_layers) <- CRS('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0')
plot(env_layers) #graficamos de la capa bio 11 a la bio 19

# Se le da expresion espacial/geografica a los puntos de ocurrencia de A. reigi

spreigi<-SpatialPoints(locsreigi, proj4string=CRS(as.character(NA)), bbox = NULL)
spreigi

# Extraemos la data de la/las capas raster que se incluyeron 

data.clim.reigi <- extract(env_layers,spreigi) #data.clim es la matriz de datos climaticos extraidos de cada localidad
data.clim.reigi                                  # Visualizamos el archivo: cada valor de cada variable en cada localidad

write.csv(data.clim.reigi)                       # Por si lo queremos en un CSV para ver en EXCEL
write.table(as.data.frame(as.matrix(data.clim.reigi)),file = "data_raster_Reigi.csv",row.names=TRUE,col.names=TRUE)

#Para hacer una figura de los puntos proyectados sobre una de las capas. Por ejemplo, la capa bio 1.

plot(env_layers$venezuela_andes)
points(locsreigi,pch=23,col='black',cex=1)






