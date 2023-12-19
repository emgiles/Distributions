getwd()
setwd("/media/pablo/data1/emily/from_oldclust/scripts/gis")
list.files()
rm(list=ls()) #clear memory
gc() #clear cache

#load/install pkgs-------------------------
library(ggplot2)
library(elevatr)
library(raster)
library(rgdal)
library(rgbif)
library(sp)
library(dplyr)
library(spocc)
library(geosphere)
library(mapview)
library(sf)
library(lwgeom)
library(concaveman)
library(maps)
library(rnaturalearth)
library(rnaturalearthdata)

###extract occurence points to determine distribution
svir<-occ(query="Scurria viridula", has_coords=TRUE)
obs1<-occ2df(svir)
head(obs1)
points2<-data.frame(obs1$latitude,obs1$longitude)
head(points2)

szeb<-occ(query="Scurria zebrina", has_coords=TRUE)
obs2<-occ2df(szeb)
points2.2<-data.frame(obs2$latitude,obs2$longitude)

###make polygon
points3 <- points2 %>%
  st_as_sf(coords = c("obs1.longitude", "obs1.latitude"), crs = 4326) ### WGS 84 projection http://www.juntadeandalucia.es/medioambiente/site/rediam/menuitem.04dc44281e5d53cf8ca78ca731525ea0/?vgnextoid=2a412abcb86a2210VgnVCM1000001325e50aRCRD&lr=lang_es
polygon3 <- concaveman(points3)

points3.3 <- points2.2 %>%
  st_as_sf(coords = c("obs2.longitude", "obs2.latitude"), crs = 4326) ### WGS 84 projection http://www.juntadeandalucia.es/medioambiente/site/rediam/menuitem.04dc44281e5d53cf8ca78ca731525ea0/?vgnextoid=2a412abcb86a2210VgnVCM1000001325e50aRCRD&lr=lang_es
polygon3.3 <- concaveman(points3.3)

###make map
theme_set(theme_bw())
world<- ne_countries(scale="medium", returnclass="sf") #retrieved as shapefile
class(world)

ggplot(data=world) +
  geom_sf() +
  geom_point(data = points2, aes(x = obs1.longitude, y = obs1.latitude), size = 2, 
             shape = 21, fill = "red", alpha=0.4) +
  geom_point(data = points2.2, aes(x = obs2.longitude, y = obs2.latitude), size = 2, 
             shape = 21, fill = "blue", alpha=0.4) +
  coord_sf(xlim=c(-79,-67), ylim=c(-45,-10), crs=4326)


