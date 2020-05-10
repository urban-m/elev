#packages needed
#tidyverse
#brms
#raster
#rasterVis
#rgdal

require(tidyverse)
require(brms)
require(raster)
require(rasterVis)
require(rgdal)

#check ob RTools drin ist
Sys.getenv(“PATH”)

#brauche ich weil er sonst RTools nicht findet. 
Sys.setenv(PATH = paste(Sys.getenv("PATH"), "C:\\RTools40","C:\\RTools40\\usr\\bin", sep=";"))
options(buildtools.check = function(action)TRUE)

#Daten einlesen
elevdata<-read.csv(file.choose(), header=T)
elevdataupsid<-read.csv(file.choose(), header=T)

#Mutationen f�rs Modelieren
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

#Modell für Ejektive, UPSID data (20000 Iterationen nötig, sonst Warnmeldung, au�erdem hier adapt_delta = 0.99 wg. divergent transitions after warmup bei adapt_delta = 0.8)
elevmodelupsidejectives<-brm(NonMarginal02 ~ elevation + (1+ elevation| macroarea2) +(1+elevation|family_id), family= 'bernoulli', data=elevdataupsid, iter=20000, control = list(adapt_delta = 0.99))
conditional_effects(elevmodelupsidejectives)
summary(elevmodelupsidejectives)
plot(elevmodelupsidejectives)
fitejectivesupsid<-(fitted(elevmodelupsidejectives, re_formula=NA, summary=TRUE))
summary (fitejectivesupsid)

#plot maps
relief_world <- raster(file.choose())
plot(relief_world, box=F, axes=F, legend=F, col = grey(1:100/100))
uvulars<-filter(elevdata, NonMarginal01==T)
nonuvulars<-filter(elevdata, NonMarginal01==F)
points(as.numeric(gsub(',','.',uvulars$longitude)), as.numeric(gsub(',','.',uvulars$latitude)), cex=0.8, pch=16, col="black", bg="black")
points(as.numeric(gsub(',','.',nonuvulars$longitude)), as.numeric(gsub(',','.',nonuvulars$latitude)), cex=0.8, pch=16, col="white", bg="white")
ejectives<-filter(elevdata, NonMarginal02==T)
nonejectives<-filter(elevdata, NonMarginal02==F)
points(as.numeric(gsub(',','.',ejectives$longitude)), as.numeric(gsub(',','.',ejectives$latitude)), cex=0.8, col="white", bg="black")
points(as.numeric(gsub(',','.',nonejectives$longitude)), as.numeric(gsub(',','.',nonejectives$latitude)), cex=0.8, col="white", bg="black")
