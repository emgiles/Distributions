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
library(spocc)
library(geosphere)
library(mapview)
library(sf)
install.packages("lwgeom")
library(lwgeom)
library(concaveman)

#load datasets-----------------------------
amphibia2 <- read.table("amphibia2.txt", h=T, sep="\t")
str(amphibia2)
data.frame(amphibia2)
head(amphibia2)

#seach for occurrences in rgbif database---
sp50<-as.character(amphibia2[50,10])
sp50_obs<-occ(query=sp50, has_coords=TRUE)
obs1<-occ2df(sp50_obs)
head(obs1)
points<-data.frame(obs1$longitude,obs1$latitude)
head(points)
points2<-data.frame(obs1$latitude,obs1$longitude)
head(points2)
points2m<-data.matrix(points2,rownames.force = NA)
head(points2m)

points3 <- points2 %>%
  st_as_sf(coords = c("obs1.longitude", "obs1.latitude"), crs = 4326) ### WGS 84 projection http://www.juntadeandalucia.es/medioambiente/site/rediam/menuitem.04dc44281e5d53cf8ca78ca731525ea0/?vgnextoid=2a412abcb86a2210VgnVCM1000001325e50aRCRD&lr=lang_es
polygon3 <- concaveman(points3)

##### read "geodeticDatum" in https://www.gbif.org/data-quality-requirements-occurrences
### Try with another crs, like 3395 (WGS 84 Mercator)
map(database="world", regions="Australia")
plot(polygon3, add = TRUE, col="red", lwd=2)
plot(points3, add = TRUE, cex=0.5)

#geographic distance
x<-distm(points,points, fun=distGeo)/1000
geo.dist <- as.dist(x)
head(geo.dist)
max(geo.dist)

area<-st_area(polygon3) / 10000000
area

### LOOP ##########
points.list <- vector("list", length = length(levels(amphibia2[,10])))
names(points.list) <- levels(amphibia2[,10])
names(points.list) <- gsub("Pseudophryne_bibroni", "Pseudophryne_bibronii", names(points.list))
names(points.list) <- gsub("Rana_palustris", "Lithobates_palustris", names(points.list))
polygon.list <- points.list
sp_obs.list<-points.list
obs.list <- points.list
area.list <- points.list

k <- 1 #dummny variable, placed in front of spp name 

for(i in names(points.list)){
  cat(paste("...\n","Calculating area to ", k, "_",i, sep = "")) ## Una nota
  sp_obs.list[[i]]<-occ(query=i, has_coords=TRUE, limit = 68000)  
  obs.list[[i]]<-occ2df(sp_obs.list[[i]])
  points.list[[i]]<-data.frame(lat=obs.list[[i]]$latitude, long=obs.list[[i]]$longitude)
  points.list[[i]] <- points.list[[i]] %>%
    st_as_sf(coords = c("long", "lat"), crs = 4326)   
  polygon.list[[i]] <- concaveman(points.list[[i]])
  area.list[[i]]<-st_area(polygon.list[[i]]) / 10000000
  cat(paste("\n The area of ", i, " is ", round(area.list[[i]],2), "\n...", sep = ""))
  k <- k+1
}

#create new column called sp2 that has the relabeled species listed below
amphibia2$sp2 <- amphibia2[,10]
amphibia2$sp2 <- gsub("Pseudophryne_bibroni", "Pseudophryne_bibronii", amphibia2$sp2)
amphibia2$sp2 <- gsub("Rana_palustris", "Lithobates_palustris", amphibia2$sp2)

amphibia2$area <-NA #create a new column filled with NA

#loop to add area to the dataframe amphibia2, area is placed in the column sp2
for(i in names(area.list)) {
  amphibia2$area[amphibia2[,"sp2"]==i] <-area.list[[i]]
  } 

str(amphibia2)
write.table(amphibia2,file="amphibia3.txt", sep="\t", row.names=FALSE, col.names=TRUE)
