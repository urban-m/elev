#packages needed
#tidyverse
#brms
#OpenStreetMap
#rgdal
#sp

require(tidyverse)
require(brms)
require(OpenStreetMap)
require(rgdal)
require(sp)

#check ob RTools drin ist
Sys.getenv(“PATH”)

#brauche ich weil er sonst RTools nicht findet. 
Sys.setenv(PATH = paste(Sys.getenv("PATH"), "C:\\RTools40","C:\\RTools40\\usr\\bin", sep=";"))
options(buildtools.check = function(action)TRUE)

#Daten einlesen
elevdata<-read.csv(file.choose(), header=T)
elevdataupsid<-read.csv(file.choose(), header=T)

#Mutationen f?rs Modelieren
elevdata <- mutate(elevdata, NonMarginal01 = as.logical(Nonmarginal_Uvular), NonMarginal01 = as.numeric(NonMarginal01))
elevdata<- mutate(elevdata, NonMarginal02 = as.logical(Nonmarginal_Ejective), NonMarginal02 = as.numeric(NonMarginal02))
elevdataupsid <- mutate(elevdataupsid, NonMarginal01 = as.logical(Nonmarginal_Uvular), NonMarginal01 = as.numeric(NonMarginal01))
elevdataupsid<- mutate(elevdataupsid, NonMarginal02 = as.logical(Nonmarginal_Ejective), NonMarginal02 = as.numeric(NonMarginal02))

#Modell für Uvulare, PHOIBLE data(10000 Iterationen nötig, sonst Warnmeldung)
elevmodeluvulars<-brm(NonMarginal01 ~ elevation + (1+ elevation| macroarea2) +(1+elevation|family_id), family= 'bernoulli', data=elevdata, iter=10000)
conditional_effects(elevmodeluvulars)
summary(elevmodeluvulars)
plot(elevmodeluvulars)
fituvulars<-(fitted(elevmodeluvulars, re_formula=NA, summary=TRUE))
summary(fituvulars)

#Modell für Uvulare, UPDID data (10000 Iterationen nötig, sonst Warnmeldung)
elevmodelupsiduvulars<-brm(NonMarginal01 ~ elevation + (1+ elevation| macroarea2) +(1+elevation|family_id), family= 'bernoulli', data=elevdataupsid, iter=10000)
conditional_effects(elevmodelupsiduvulars)
summary(elevmodelupsiduvulars)
plot(elevmodelupsiduvulars)
fituvularsupsid<-(fitted(elevmodelupsiduvulars, re_formula=NA, summary=TRUE))
summary(fituvularupsid)

#Modell für Ejektive, PHOIBLE data (10000 Iterationen nötig, sonst Warnmeldung)
elevmodelejectives<-brm(NonMarginal02  ~ elevation + (1+ elevation| macroarea2) +(1+elevation|family_id), family= 'bernoulli', data=elevdata, iter=10000)
conditional_effects(elevmodelejectives)
summary(elevmodelejectives)
plot(elevmodelejectives)
fitejectives<-(fitted(elevmodelejectives, re_formula=NA, summary=TRUE))
summary (fitejectives)

#Modell für Ejektive, UPSID data (20000 Iterationen nötig, sonst Warnmeldung, au?erdem hier adapt_delta = 0.99 wg. divergent transitions after warmup bei adapt_delta = 0.8)
elevmodelupsidejectives<-brm(NonMarginal02 ~ elevation + (1+ elevation| macroarea2) +(1+elevation|family_id), family= 'bernoulli', data=elevdataupsid, iter=20000, control = list(adapt_delta = 0.99))
conditional_effects(elevmodelupsidejectives)
summary(elevmodelupsidejectives)
plot(elevmodelupsidejectives)
fitejectivesupsid<-(fitted(elevmodelupsidejectives, re_formula=NA, summary=TRUE))
summary (fitejectivesupsid)

#plot maps
map <- openmap(c(80,-180), c(-70,180),type='nps', minNumTiles=100)
plot(map)
uvulars<-filter(elevdata, NonMarginal01==T)
nonuvulars<-filter(elevdata, NonMarginal01==F)
plotdatauvulars<-data.frame(lat=as.numeric(gsub(',','.',uvulars$latitude)), lon=as.numeric(gsub(',','.',uvulars$longitude)))
plotdatauvulars<-plotdatauvulars[complete.cases(plotdatauvulars),]
coordinates(plotdatauvulars)<-~lon+lat
proj4string(plotdatauvulars)<-CRS("+init=epsg:4326")
points(spTransform(plotdatauvulars,osm()), pch=21, col="white", bg="black", cex=1.2)

plotdatanonuvulars<-data.frame(lat=as.numeric(gsub(',','.',nonuvulars$latitude)), lon=as.numeric(gsub(',','.',nonuvulars$longitude)))
plotdatanonuvulars<-plotdatanonuvulars[complete.cases(plotdatanonuvulars),]
coordinates(plotdatanonuvulars)<-~lon+lat
proj4string(plotdatanonuvulars)<-CRS("+init=epsg:4326")
points(spTransform(plotdatanonuvulars,osm()), pch=21, col="black", bg="white")

ejectives<-filter(elevdata, NonMarginal02==T)
nonejectives<-filter(elevdata, NonMarginal02==F)
plotdataejectives<-data.frame(lat=as.numeric(gsub(',','.',ejectives$latitude)), lon=as.numeric(gsub(',','.',ejectives$longitude)))
plotdataejectives<-plotdataejectives[complete.cases(plotdataejectives),]
coordinates(plotdataejectives)<-~lon+lat
proj4string(plotdataejectives)<-CRS("+init=epsg:4326")
points(spTransform(plotdataejectives,osm()), pch=21, col="white", bg="black", cex=1.2)

ejectives<-filter(elevdata, NonMarginal02==T)
nonejectives<-filter(elevdata, NonMarginal02==F)
plotdatanonejectives<-data.frame(lat=as.numeric(gsub(',','.',nonejectives$latitude)), lon=as.numeric(gsub(',','.',nonejectives$longitude)))
plotdatanonejectives<-plotdatanonejectives[complete.cases(plotdatanonejectives),]
coordinates(plotdatanonejectives)<-~lon+lat
proj4string(plotdatanonejectives)<-CRS("+init=epsg:4326")
points(spTransform(plotdatanonejectives,osm()), pch=21, col="black", bg="white")