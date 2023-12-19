getwd()
setwd("/Users/egiles/Dropbox/School/Ecology/Project/Data")

#-----------
# LIBRERIAS
#-----------
library(dplyr)
library(ggplot2) 
library(gridExtra)
library(ggfortify)
library(corrplot)
library(psych)
library(lme4)
library(lmerTest)

data<-read.table("compiled_data_0706.txt",head=T)
head(data)

m1<-lm(WT_1970_2000~Latitude*Phylum/Class, data)
m1
m2<-lmer(WT_1970_2000~Latitude*(1|Phylum/Class), REML=FALSE, data)

#------------
# GRAPHICAL EXPLORATION
#------------

ggplot(data, 
	aes(x=abs(Latitude), y=WT_1970_2000,colour = taxonomy2),) + 
	geom_point()+
	xlab("Absolute Latitude") +
	ylab("Warming Tolerance (Climate 1970-2000)") +
	geom_smooth(method="lm", se=FALSE) 

ggplot(data, 
	aes(x=abs(Latitude), y=WT_2050,colour = taxonomy2),) + 
	geom_point()+ 
	xlab("Absolute Latitude") +
	ylab("Warming Tolerance (Climate 2050)") +
	geom_smooth(method="lm", se=FALSE) 
		
ggplot(data, aes(x = Class, y = WT_1970_2000)) +  
		geom_boxplot() +
		xlab("Phylum:Class") +
		ylab("Warming Tolerance (Climate 1970-2000)") 
		
ggplot(data, aes(x = Class, y = WT_2050)) +  
		geom_boxplot() +
		xlab("Class") +
		ylab("Warming Tolerance (Climate 2050)") 
		theme_colour()
				
	
#-------------
# MIXED MODELING
#-------------
taxonomy<-factor(data$Phylum:data$Class:data$Order)
taxonomy2<-factor(data$Phylum:data$Class)

#Test importance of random factor
m1<-lmer(WT_1970_2000~(Phylum/Class/OrderGenus+Longitude+abs(Latitude)+ (1|t_max_metric), REML=FALSE, data)
m2<-lm(WT_1970_2000~Phylum+Class+Order+Genus+Longitude+abs(Latitude), data)

x2<- -2*logLik(m2,REML=T)+2*logLik(m1,REML=T)#Watch out with the order
pchisq(x2, df=1, lower.tail=FALSE) 

rand(m1)# NS

#Check importance of fixed factors
m1<-lmer(WT_1970_2000~Phylum+Class+Order+Genus+abs(Longitude)+abs(Latitude)+ (1|t_max_metric), REML=FALSE, data)
m2<-lmer(WT_1970_2000~Phylum+abs(Longitude)+ (1|t_max_metric), REML=FALSE, data)
anova(m1,m2) #Latitude is very significant
m3<-lmer(WT_1970_2000~Phylum+abs(Latitude)+ (1|t_max_metric), REML=FALSE, data)
anova(m1,m3) #Longitude is very significant
m4<-m1<-lmer(WT_1970_2000~abs(Longitude)+abs(Latitude)+ (1|t_max_metric), REML=FALSE, data)
anova(m1,m4) #Phylum not significant