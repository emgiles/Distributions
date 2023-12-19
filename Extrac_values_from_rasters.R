#### MQC: para extraer datos de capas raster y calcular distancias geograficas entre localidades (lat, lon)

library("permute")
library("lattice")
library("sp")
library("raster")
library("lattice")
library("survival")
library("Formula") 
library("ggplot2")
library("Hmisc")

getwd()

### Directorio de Trabajo
### Defino la carpeta de trabajo y la carpeta con los datos
work.dir <- "/Users/egiles/Dropbox/School/Ecology/Project/Extrac_values_from_rasters" #carpeta de trabajo
data.dir <- "/Users/egiles/Dropbox/School/Ecology/Project/Extrac_values_from_rasters" # carpeta con los datos
### Cambio a la carpeta de trabajo
setwd(work.dir)

# Localidades de A. reigi
locsreigi <- read.csv("LOCS_Reigi.csv", header=TRUE, sep=',', stringsAsFactors=F) # A. reigi
locsreigi
plot (locsreigi)
locs_reigi <- list.files(pattern = "*.csv")

### Para construir la matriz de distancia geografica entre las localidades la cual 
#   llamaremos "dist.geof"; nuavemente para ambas especies

# Para A. reigi
dist.geof.reigi <- dist(coordinates(locsreigi)[,1:2]) #creamos la matriz
dist.geof.reigi                                       #visualizamos la matriz triangular
geofdist<-(as.data.frame(as.matrix(dist.geof.reigi))) #transforma los datos en una matriz rectangular
write.table ((geofdist),file = "geofist_Reigi.csv") #escribe un matriz en formato .csv de estos datos en el directorio de trabajo

### Para construir la matriz triangular de las variables climaticas, la cual llamaremos "dist.clima"; Nuavemente para la especie

### Cargarmos primero las capas con la data climatica "env_layers"
#las capas deben estar en el directorio de trabajo
env_layers <- stack(list.files(path=data.dir,pattern='asc', full.names=TRUE))
projection(env_layers) <- CRS('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0')
plot(env_layers,5) #graficamos de la capa bio 5

# Para A. reigi

spreigi<-SpatialPoints(locsreigi, proj4string=CRS(as.character(NA)), bbox = NULL)
spreigi

data.clim.reigi <- extract(env_layers,spreigi) #data.clim es la matriz de datos climaticos extraidos de cada localidad
data.clim.reigi                                  # Visualizamos el archivo: cada valor de cada variable en cada localidad

write.csv(data.clim.reigi)                       # Por si lo queremos en un CSV para ver en EXCEL
write.table(as.data.frame(as.matrix(data.clim.reigi)),file = "dataclim_Reigi.csv",row.names=TRUE,col.names=TRUE)

#Para hacer una figura de los puntos proyectados sobre una de las capas. Por ejemplo, la capa bio 1.

plot(env_layers$Bio_05)
points(locsreigi,pch=24,col='black',cex=0.9)






