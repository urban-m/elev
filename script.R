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

#Mutationen f?rs Modelieren
elevdata <- mutate(elevdata, NonMarginal01 = as.logical(Nonmarginal_Uvular), NonMarginal01 = as.numeric(NonMarginal01))
elevdata<- mutate(elevdata, NonMarginal02 = as.logical(Nonmarginal_Ejective), NonMarginal02 = as.numeric(NonMarginal02))
elevdata <- mutate(elevdata, elevationlog10 = log10(elevation))

#Modell für Uvulare, PHOIBLE data
priors <-set_prior("normal(0, 2)", class="b", coef="elevationlog10")
elevmodeluvulars3<-brm(NonMarginal01 ~ elevationlog10 + (1+ elevationlog10| macroarea2) +(1|family_id), family= 'bernoulli', data=elevdata, warmup=2000, iter=3000, prior=priors, control = list(adapt_delta = 0.99))
elevmodeluvulars3samples<-posterior_samples(elevmodeluvulars3)
sum(elevmodeluvulars3samples$b_elevationlog10 < 0) /nrow(elevmodeluvulars3samples)
conditional_effects(elevmodeluvulars3)
plot(elevmodeluvulars3)
fituvulars<-(fitted(elevmodeluvulars3, re_formula=NA, summary=TRUE))
summary(fituvulars3)

#Modell für Ejektive, PHOIBLE data 
elevmodelejectives<-brm(NonMarginal02  ~ elevation + (1+ elevation| macroarea2) +(1+elevation|family_id), family= 'bernoulli', data=elevdata, iter=10000)
conditional_effects(elevmodelejectives)
summary(elevmodelejectives)
plot(elevmodelejectives)
fitejectives<-(fitted(elevmodelejectives, re_formula=NA, summary=TRUE))
summary (fitejectives)
elevmodelejectives3<-brm(NonMarginal02 ~ elevationlog10 + (1+ elevationlog10| macroarea2) +(1|family_id), family= 'bernoulli', data=elevdata, warmup=2000, iter=3000, prior=priors, control = list(adapt_delta = 0.99))
elevmodelejectives3samples<-posterior_samples(elevmodelejectives3)
sum(elevmodelejectives3samples$b_elevationlog10 < 0) /nrow(elevmodelejectives3samples)
conditional_effects(elevmodelejectives3)
plot(elevmodelejectives3)
fitejectives3<-(fitted(elevmodelejectives3, re_formula=NA, summary=TRUE))
summary(fitejectives3)


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

#extract basic descriptive stats
#by area
elevdataeurope<-elevdata[elevdata$macroarea2=='Europe',]
mean(elevdataeurope$elevation, na.rm=1)
median(elevdataeurope$elevation, na.rm=1)
mean(elevdataeurope$NonMarginal01, na.rm=1)
mean(elevdataeurope$NonMarginal02, na.rm=1)


elevdataafrica<-elevdata[elevdata$macroarea2=='Africa',]
mean(elevdataafrica$elevation, na.rm=1)
median(elevdataafrica$elevation, na.rm=1)
mean(elevdataafrica$NonMarginal01, na.rm=1)
mean(elevdataafrica$NonMarginal02, na.rm=1)

elevdatanewguinea<-elevdata[elevdata$macroarea2=='New Guinea',]
mean(elevdatanewguinea$elevation, na.rm=1)
median(elevdatanewguinea$elevation, na.rm=1)
mean(elevdatanewguinea$NonMarginal01, na.rm=1)
mean(elevdatanewguinea$NonMarginal02, na.rm=1)

elevdatasouthsoutheastasia<-elevdata[elevdata$macroarea2=='South & Southeast Asia',]
mean(elevdatasouthsoutheastasia$elevation, na.rm=1)
median(elevdatasouthsoutheastasia$elevation, na.rm=1)
mean(elevdatasouthsoutheastasia$NonMarginal01, na.rm=1)
mean(elevdatasouthsoutheastasia$NonMarginal02, na.rm=1)

elevdatasouthamerica<-elevdata[elevdata$macroarea2=='South America',]
mean(elevdatasouthamerica$elevation, na.rm=1)
median(elevdatasouthamerica$elevation, na.rm=1)
mean(elevdatasouthamerica$NonMarginal01, na.rm=1)
mean(elevdatasouthamerica$NonMarginal02, na.rm=1)

elevdatanortherneurasia<-elevdata[elevdata$macroarea2=='Northern Eurasia',]
mean(elevdatanortherneurasia$elevation, na.rm=1)
median(elevdatanortherneurasia$elevation, na.rm=1)
mean(elevdatanortherneurasia$NonMarginal01, na.rm=1)
mean(elevdatanortherneurasia$NonMarginal02, na.rm=1)

elevdatanorthamerica<-elevdata[elevdata$macroarea2=='North America',]
mean(elevdatanorthamerica$elevation, na.rm=1)
median(elevdatanorthamerica$elevation, na.rm=1)
mean(elevdatanorthamerica$NonMarginal01, na.rm=1)
mean(elevdatanorthamerica$NonMarginal02, na.rm=1)

elevdataaustralia<-elevdata[elevdata$macroarea2=='Australia',]
mean(elevdataaustralia$elevation, na.rm=1)
median(elevdataaustralia$elevation, na.rm=1)
mean(elevdataaustralia$NonMarginal01, na.rm=1)
mean(elevdataaustralia$NonMarginal02, na.rm=1)

elevdatamiddleamerica<-elevdata[elevdata$macroarea2=='Middle America',]
mean(elevdatamiddleamerica$elevation, na.rm=1)
median(elevdatamiddleamerica$elevation, na.rm=1)
mean(elevdatamiddleamerica$NonMarginal01, na.rm=1)
mean(elevdatamiddleamerica$NonMarginal02, na.rm=1)

elevdataoceania<-elevdata[elevdata$macroarea2=='Oceania',]
mean(elevdataoceania$elevation, na.rm=1)
median(elevdataoceania$elevation, na.rm=1)
mean(elevdataoceania$NonMarginal01, na.rm=1)
mean(elevdataoceania$NonMarginal02, na.rm=1)

elevdatawesternasia<-elevdata[elevdata$macroarea2=='Western Asia',]
mean(elevdatawesternasia$elevation, na.rm=1)
median(elevdatawesternasia$elevation, na.rm=1)
mean(elevdatawesternasia$NonMarginal01, na.rm=1)
mean(elevdatawesternasia$NonMarginal02, na.rm=1)

elevdataeuropeuvulars<-elevdataeurope[elevdataeurope$NonMarginal01==1,]
mean(elevdataeuropeuvulars$elevation, na.rm=1)
sum(complete.cases(elevdataeuropeuvulars$NonMarginal01))

elevdataeuropenouvulars<-elevdataeurope[elevdataeurope$NonMarginal01==0,]
mean(elevdataeuropenouvulars$elevation, na.rm=1)
sum(complete.cases(elevdataeuropenouvulars$NonMarginal01))

elevdataeuropeejectives<-elevdataeurope[elevdataeurope$NonMarginal02==1,]
mean(elevdataeuropeejectives$elevation, na.rm=1)
sum(complete.cases(elevdataeuropeejectives$NonMarginal02))

elevdataeuropenoejectives<-elevdataeurope[elevdataeurope$NonMarginal02==0,]
mean(elevdataeuropenoejectives $elevation, na.rm=1)
sum(complete.cases(elevdataeuropenoejectives$NonMarginal02))

elevdataafricauvulars<-elevdataafrica[elevdataafrica$NonMarginal01==1,]
mean(elevdataafricauvulars$elevation, na.rm=1)
sum(complete.cases(elevdataafricauvulars$NonMarginal01))

elevdataafricanouvulars<-elevdataafrica[elevdataafrica$NonMarginal01==0,]
mean(elevdataafricanouvulars$elevation, na.rm=1)
sum(complete.cases(elevdataafricanouvulars$NonMarginal01))

elevdataafricaejectives<-elevdataafrica[elevdataafrica$NonMarginal02==1,]
mean(elevdataafricaejectives$elevation, na.rm=1)
sum(complete.cases(elevdataafricaejectives$NonMarginal02))

elevdataafricanoejectives<-elevdataafrica[elevdataafrica$NonMarginal02==0,]
mean(elevdataafricanoejectives $elevation, na.rm=1)
sum(complete.cases(elevdataafricanoejectives$NonMarginal02))


elevdatanewguineauvulars<-elevdatanewguinea[elevdatanewguinea$NonMarginal01==1,]
mean(elevdatanewguineauvulars$elevation, na.rm=1)
sum(complete.cases(elevdatanewguineauvulars$NonMarginal01))


elevdatanewguineanouvulars<-elevdatanewguinea[elevdatanewguinea$NonMarginal01==0,]
mean(elevdatanewguineanouvulars$elevation, na.rm=1)
sum(complete.cases(elevdatanewguineanouvulars$NonMarginal01))

elevdatanewguineaejectives<-elevdatanewguinea[elevdatanewguinea$NonMarginal02==1,]
mean(elevdatanewguineaejectives$elevation, na.rm=1)
sum(complete.cases(elevdatanewguineaejectives$NonMarginal02))

elevdatanewguineanoejectives<-elevdatanewguinea[elevdatanewguinea$NonMarginal02==0,]
mean(elevdatanewguineanoejectives $elevation, na.rm=1)
sum(complete.cases(elevdatanewguineanoejectives$NonMarginal02))


elevdatasouthsoutheastasiauvulars<-elevdatasouthsoutheastasia[elevdatasouthsoutheastasia$NonMarginal01==1,]
mean(elevdatasouthsoutheastasiauvulars$elevation, na.rm=1)
sum(complete.cases(elevdatasouthsoutheastasiauvulars $NonMarginal01))

elevdatasouthsoutheastasianouvulars<-elevdatasouthsoutheastasia[elevdatasouthsoutheastasia$NonMarginal01==0,]
mean(elevdatasouthsoutheastasianouvulars$elevation, na.rm=1)
sum(complete.cases(elevdatasouthsoutheastasianouvulars $NonMarginal01))

elevdatasouthsoutheastasiaejectives<-elevdatasouthsoutheastasia[elevdatasouthsoutheastasia$NonMarginal02==1,]
mean(elevdatasouthsoutheastasiaejectives$elevation, na.rm=1)
sum(complete.cases(elevdatasouthsoutheastasiaejectives$NonMarginal02))

elevdatasouthsoutheastasianoejectives<-elevdatasouthsoutheastasia[elevdatasouthsoutheastasia$NonMarginal02==0,]
mean(elevdatasouthsoutheastasianoejectives $elevation, na.rm=1)
sum(complete.cases(elevdatasouthsoutheastasianoejectives$NonMarginal02))


elevdatasouthamericauvulars<-elevdatasouthamerica[elevdatasouthamerica$NonMarginal01==1,]
mean(elevdatasouthamericauvulars$elevation, na.rm=1)
sum(complete.cases(elevdatasouthamericauvulars $NonMarginal01))

elevdatasouthamericanouvulars<-elevdatasouthamerica[elevdatasouthamerica$NonMarginal01==0,]
mean(elevdatasouthamericanouvulars$elevation, na.rm=1)
sum(complete.cases(elevdatasouthamericanouvulars $NonMarginal01))

elevdatasouthamericaejectives<-elevdatasouthamerica[elevdatasouthamerica$NonMarginal02==1,]
mean(elevdatasouthamericaejectives$elevation, na.rm=1)
sum(complete.cases(elevdatasouthamericaejectives $NonMarginal02))

elevdatasouthamericanoejectives<-elevdatasouthamerica[elevdatasouthamerica$NonMarginal02==0,]
mean(elevdatasouthamericanoejectives $elevation, na.rm=1)
sum(complete.cases(elevdatasouthamericanoejectives $NonMarginal02))


elevdatanortherneurasiauvulars<-elevdatanortherneurasia[elevdatanortherneurasia$NonMarginal01==1,]
mean(elevdatanortherneurasiauvulars$elevation, na.rm=1)
sum(complete.cases(elevdatanortherneurasiauvulars $NonMarginal01))

elevdatanortherneurasianouvulars<-elevdatanortherneurasia[elevdatanortherneurasia$NonMarginal01==0,]
sum(complete.cases(elevdatanortherneurasianouvulars $NonMarginal01))
sum(complete.cases(elevdatanortherneurasiaejectives $NonMarginal02))

elevdatanortherneurasiaejectives<-elevdatanortherneurasia[elevdatanortherneurasia$NonMarginal02==1,]
mean(elevdatanortherneurasiaejectives$elevation, na.rm=1)
sum(complete.cases(elevdatanortherneurasiaejectives $NonMarginal02))

elevdatanortherneurasianoejectives<-elevdatanortherneurasia[elevdatanortherneurasia$NonMarginal02==0,]
sum(complete.cases(elevdatanortherneurasianoejectives $NonMarginal02))

nrow(elevdatanortherneurasianoejectives)
elevdatanorthamericauvulars<-elevdatanorthamerica[elevdatanorthamerica$NonMarginal01==1,]
mean(elevdatanorthamericauvulars$elevation, na.rm=1)
sum(complete.cases(elevdatanorthamericauvulars $NonMarginal01))

elevdatanorthamericanouvulars<-elevdatanorthamerica[elevdatanorthamerica$NonMarginal01==0,]
mean(elevdatanorthamericanouvulars$elevation, na.rm=1)
sum(complete.cases(elevdatanorthamericanouvulars $NonMarginal01))

elevdatanorthamericaejectives<-elevdatanorthamerica[elevdatanorthamerica$NonMarginal02==1,]
mean(elevdatanorthamericaejectives$elevation, na.rm=1)
sum(complete.cases(elevdatanorthamericaejectives $NonMarginal02))

elevdatanorthamericanoejectives<-elevdatanorthamerica[elevdatanorthamerica$NonMarginal02==0,]
mean(elevdatanorthamericanoejectives $elevation, na.rm=1)
sum(complete.cases(elevdatanorthamericanoejectives $NonMarginal02))

elevdataaustraliauvulars<-elevdataaustralia[elevdataaustralia$NonMarginal01==1,]
mean(elevdataaustraliauvulars$elevation, na.rm=1)
sum(complete.cases(elevdataaustraliauvulars $NonMarginal01))

elevdataaustralianouvulars<-elevdataaustralia[elevdataaustralia$NonMarginal01==0,]
mean(elevdataaustralianouvulars$elevation, na.rm=1)
sum(complete.cases(elevdataaustralianouvulars $NonMarginal01))

elevdataaustraliaejectives<-elevdataaustralia[elevdataaustralia$NonMarginal02==1,]
mean(elevdataaustraliaejectives$elevation, na.rm=1)
sum(complete.cases(elevdataaustraliaejectives $NonMarginal02))

elevdataaustralianoejectives<-elevdataaustralia[elevdataaustralia$NonMarginal02==0,]
mean(elevdataaustralianoejectives $elevation, na.rm=1)
sum(complete.cases(elevdataaustralianoejectives $NonMarginal02))

elevdataoceaniauvulars<-elevdataoceania[elevdataoceania$NonMarginal01==1,]
mean(elevdataoceaniauvulars$elevation, na.rm=1)
sum(complete.cases(elevdataoceaniauvulars $NonMarginal01))

elevdataoceanianouvulars<-elevdataoceania[elevdataoceania$NonMarginal01==0,]
mean(elevdataoceanianouvulars$elevation, na.rm=1)
sum(complete.cases(elevdataoceanianouvulars $NonMarginal01))

elevdataoceaniaejectives<-elevdataoceania[elevdataoceania$NonMarginal02==1,]
mean(elevdataoceaniaejectives$elevation, na.rm=1)
sum(complete.cases(elevdataoceaniaejectives $NonMarginal02))

elevdataoceanianoejectives<-elevdataoceania[elevdataoceania$NonMarginal02==0,]
sum(complete.cases(elevdataoceanianoejectives $NonMarginal02))
nrow(elevdataoceanianoejectives)

elevdatamiddleamericauvulars<-elevdatamiddleamerica[elevdatamiddleamerica$NonMarginal01==1,]
mean(elevdatamiddleamericauvulars$elevation, na.rm=1)
sum(complete.cases(elevdatamiddleamericauvulars $NonMarginal01))

elevdatamiddleamericanouvulars<-elevdatamiddleamerica[elevdatamiddleamerica$NonMarginal01==0,]
mean(elevdatamiddleamericanouvulars$elevation, na.rm=1)
sum(complete.cases(elevdatamiddleamericanouvulars $NonMarginal01))

elevdatamiddleamericaejectives<-elevdatamiddleamerica[elevdatamiddleamerica$NonMarginal02==1,]
mean(elevdatamiddleamericaejectives$elevation, na.rm=1)
sum(complete.cases(elevdatamiddleamericaejectives $NonMarginal02))


elevdatamiddleamericanoejectives<-elevdatamiddleamerica[elevdatamiddleamerica$NonMarginal02==0,]
mean(elevdatamiddleamericanoejectives $elevation, na.rm=1)
sum(complete.cases(elevdatamiddleamericanoejectives $NonMarginal02))

elevdatawesternasiauvulars<-elevdatawesternasia[elevdatawesternasia$NonMarginal01==1,]
mean(elevdatawesternasiauvulars$elevation, na.rm=1)
sum(complete.cases(elevdatawesternasiauvulars $NonMarginal01))

elevdatawesternasianouvulars<-elevdatawesternasia[elevdatawesternasia$NonMarginal01==0,]
mean(elevdatawesternasianouvulars$elevation, na.rm=1)
sum(complete.cases(elevdatawesternasianouvulars $NonMarginal01))

elevdatawesternasiaejectives<-elevdatawesternasia[elevdatawesternasia$NonMarginal02==1,]
mean(elevdatawesternasiaejectives$elevation, na.rm=1)
sum(complete.cases(elevdatawesternasiaejectives $NonMarginal02))


elevdatawesternasianoejectives<-elevdatawesternasia[elevdatawesternasia$NonMarginal02==0,]
mean(elevdatawesternasianoejectives $elevation, na.rm=1)
sum(complete.cases(elevdatawesternasianoejectives $NonMarginal02))

#by major family
elevdataafro1255<-elevdata[elevdata$family_id=='afro1255',]
mean(elevdataafro1255$elevation, na.rm=1)
median(elevdataafro1255$elevation, na.rm=1)
mean(elevdataafro1255$NonMarginal01, na.rm=1)
mean(elevdataafro1255$NonMarginal02, na.rm=1)

elevdataaraw1281<-elevdata[elevdata$family_id=='araw1281',]
mean(elevdataaraw1281$elevation, na.rm=1)
median(elevdataaraw1281$elevation, na.rm=1)
mean(elevdataaraw1281$NonMarginal01, na.rm=1)
mean(elevdataaraw1281$NonMarginal02, na.rm=1)


elevdataatha1245<-elevdata[elevdata$family_id=='atha1245',]
mean(elevdataatha1245$elevation, na.rm=1)
median(elevdataatha1245$elevation, na.rm=1)
mean(elevdataatha1245$NonMarginal01, na.rm=1)
mean(elevdataatha1245$NonMarginal02, na.rm=1)

elevdataatla1278<-elevdata[elevdata$family_id=='atla1278',]
mean(elevdataatla1278$elevation, na.rm=1)
median(elevdataatla1278$elevation, na.rm=1)
mean(elevdataatla1278$NonMarginal01, na.rm=1)
mean(elevdataatla1278$NonMarginal02, na.rm=1)

elevdataaust1307<-elevdata[elevdata$family_id=='aust1307',]
mean(elevdataaust1307$elevation, na.rm=1)
median(elevdataaust1307$elevation, na.rm=1)
mean(elevdataaust1307$NonMarginal01, na.rm=1)
mean(elevdataaust1307$NonMarginal02, na.rm=1)

elevdatacari1283<-elevdata[elevdata$family_id=='cari1283',]
mean(elevdatacari1283$elevation, na.rm=1)
median(elevdatacari1283$elevation, na.rm=1)
mean(elevdatacari1283$NonMarginal01, na.rm=1)
mean(elevdatacari1283$NonMarginal02, na.rm=1)

elevdatamand1469<-elevdata[elevdata$family_id=='mand1469',]
mean(elevdatamand1469$elevation, na.rm=1)
median(elevdatamand1469$elevation, na.rm=1)
mean(elevdatamand1469$NonMarginal01, na.rm=1)
mean(elevdatamand1469$NonMarginal02, na.rm=1)

elevdatamaya1287<-elevdata[elevdata$family_id=='maya1287',]
mean(elevdatamaya1287$elevation, na.rm=1)
median(elevdatamaya1287$elevation, na.rm=1)
mean(elevdatamaya1287$NonMarginal01, na.rm=1)
mean(elevdatamaya1287$NonMarginal02, na.rm=1)

elevdatamong1329<-elevdata[elevdata$family_id=='mong1329',]
mean(elevdatamong1329$elevation, na.rm=1)
median(elevdatamong1329$elevation, na.rm=1)
mean(elevdatamong1329$NonMarginal01, na.rm=1)
mean(elevdatamong1329$NonMarginal02, na.rm=1)

elevdatanakh1245<-elevdata[elevdata$family_id=='nakh1245',]
mean(elevdatanakh1245$elevation, na.rm=1)
median(elevdatanakh1245$elevation, na.rm=1)
mean(elevdatanakh1245$NonMarginal01, na.rm=1)
mean(elevdatanakh1245$NonMarginal02, na.rm=1)

elevdataotom1299<-elevdata[elevdata$family_id=='otom1299',]
mean(elevdataotom1299$elevation, na.rm=1)
median(elevdataotom1299$elevation, na.rm=1)
mean(elevdataotom1299$NonMarginal01, na.rm=1)
mean(elevdataotom1299$NonMarginal02, na.rm=1)

elevdatasali1255<-elevdata[elevdata$family_id=='sali1255',]
mean(elevdatasali1255$elevation, na.rm=1)
median(elevdatasali1255$elevation, na.rm=1)
mean(elevdatasali1255$NonMarginal01, na.rm=1)
mean(elevdatasali1255$NonMarginal02, na.rm=1)

elevdatasino1245<-elevdata[elevdata$family_id=='sino1245',]
mean(elevdatasino1245$elevation, na.rm=1)
median(elevdatasino1245$elevation, na.rm=1)
mean(elevdatasino1245$NonMarginal01, na.rm=1)
mean(elevdatasino1245$NonMarginal02, na.rm=1)

elevdatataik1256<-elevdata[elevdata$family_id=='taik1256',]
mean(elevdatataik1256$elevation, na.rm=1)
median(elevdatataik1256$elevation, na.rm=1)
mean(elevdatataik1256$NonMarginal01, na.rm=1)
mean(elevdatataik1256$NonMarginal02, na.rm=1)

elevdatatupi1275<-elevdata[elevdata$family_id=='tupi1275',]
mean(elevdatatupi1275$elevation, na.rm=1)
median(elevdatatupi1275$elevation, na.rm=1)
mean(elevdatatupi1275$NonMarginal01, na.rm=1)
mean(elevdatatupi1275$NonMarginal02, na.rm=1)

elevdataturk1311<-elevdata[elevdata$family_id=='turk1311',]
mean(elevdataturk1311$elevation, na.rm=1)
median(elevdataturk1311$elevation, na.rm=1)
mean(elevdataturk1311$NonMarginal01, na.rm=1)
mean(elevdataturk1311$NonMarginal02, na.rm=1)

elevdataural1272<-elevdata[elevdata$family_id=='ural1272',]
mean(elevdataural1272$elevation, na.rm=1)
median(elevdataural1272$elevation, na.rm=1)
mean(elevdataural1272$NonMarginal01, na.rm=1)
mean(elevdataural1272$NonMarginal02, na.rm=1)

elevdatagong1255<-elevdata[elevdata$family_id=='gong1255',]
mean(elevdatagong1255$elevation, na.rm=1)
median(elevdatagong1255$elevation, na.rm=1)
mean(elevdatagong1255$NonMarginal01, na.rm=1)
mean(elevdatagong1255$NonMarginal02, na.rm=1)

elevdataafro1255uvulars<-elevdataafro1255[elevdataafro1255$NonMarginal01==1,]
mean(elevdataafro1255uvulars$elevation, na.rm=1)
sum(complete.cases(elevdataafro1255uvulars$NonMarginal01))

elevdataafro1255nouvulars<-elevdataafro1255[elevdataafro1255$NonMarginal01==0,]
mean(elevdataafro1255nouvulars$elevation, na.rm=1)
sum(complete.cases(elevdataafro1255nouvulars$NonMarginal01))

elevdataafro1255ejectives<-elevdataafro1255[elevdataafro1255$NonMarginal02==1,]
mean(elevdataafro1255ejectives$elevation, na.rm=1)
sum(complete.cases(elevdataafro1255ejectives$NonMarginal02))

elevdataafro1255noejectives<-elevdataafro1255[elevdataafro1255$NonMarginal02==0,]
mean(elevdataafro1255noejectives $elevation, na.rm=1)
sum(complete.cases(elevdataafro1255noejectives$NonMarginal02))

elevdataaraw1281uvulars<-elevdataaraw1281[elevdataaraw1281$NonMarginal01==1,]
mean(elevdataaraw1281uvulars$elevation, na.rm=1)
sum(complete.cases(elevdataaraw1281uvulars$NonMarginal01))

elevdataaraw1281nouvulars<-elevdataaraw1281[elevdataaraw1281$NonMarginal01==0,]
mean(elevdataaraw1281nouvulars$elevation, na.rm=1)
sum(complete.cases(elevdataaraw1281nouvulars$NonMarginal01))

elevdataaraw1281ejectives<-elevdataaraw1281[elevdataaraw1281$NonMarginal02==1,]
mean(elevdataaraw1281ejectives$elevation, na.rm=1)
sum(complete.cases(elevdataaraw1281ejectives$NonMarginal02))

elevdataaraw1281noejectives<-elevdataaraw1281[elevdataaraw1281$NonMarginal02==0,]
mean(elevdataaraw1281noejectives $elevation, na.rm=1)
sum(complete.cases(elevdataaraw1281noejectives$NonMarginal02))

elevdataatha1245uvulars<-elevdataatha1245[elevdataatha1245$NonMarginal01==1,]
mean(elevdataatha1245uvulars$elevation, na.rm=1)
sum(complete.cases(elevdataatha1245uvulars$NonMarginal01))

elevdataatha1245nouvulars<-elevdataatha1245[elevdataatha1245$NonMarginal01==0,]
mean(elevdataatha1245nouvulars$elevation, na.rm=1)
sum(complete.cases(elevdataatha1245nouvulars$NonMarginal01))

elevdataatha1245ejectives<-elevdataatha1245[elevdataatha1245$NonMarginal02==1,]
mean(elevdataatha1245ejectives$elevation, na.rm=1)
sum(complete.cases(elevdataatha1245ejectives$NonMarginal02))

elevdataatha1245noejectives<-elevdataatha1245[elevdataatha1245$NonMarginal02==0,]
mean(elevdataatha1245noejectives $elevation, na.rm=1)
sum(complete.cases(elevdataatha1245noejectives$NonMarginal02))

elevdataatla1278uvulars<-elevdataatla1278[elevdataatla1278$NonMarginal01==1,]
mean(elevdataatla1278uvulars$elevation, na.rm=1)
sum(complete.cases(elevdataatla1278uvulars$NonMarginal01))

elevdataatla1278nouvulars<-elevdataatla1278[elevdataatla1278$NonMarginal01==0,]
mean(elevdataatla1278nouvulars$elevation, na.rm=1)
sum(complete.cases(elevdataatla1278nouvulars$NonMarginal01))

elevdataatla1278ejectives<-elevdataatla1278[elevdataatla1278$NonMarginal02==1,]
mean(elevdataatla1278ejectives$elevation, na.rm=1)
sum(complete.cases(elevdataatla1278ejectives$NonMarginal02))

elevdataatla1278noejectives<-elevdataatla1278[elevdataatla1278$NonMarginal02==0,]
mean(elevdataatla1278noejectives $elevation, na.rm=1)
sum(complete.cases(elevdataatla1278noejectives$NonMarginal02))

elevdataaust1307uvulars<-elevdataaust1307[elevdataaust1307$NonMarginal01==1,]
mean(elevdataaust1307uvulars$elevation, na.rm=1)
sum(complete.cases(elevdataaust1307uvulars$NonMarginal01))

elevdataaust1307nouvulars<-elevdataaust1307[elevdataaust1307$NonMarginal01==0,]
mean(elevdataaust1307nouvulars$elevation, na.rm=1)
sum(complete.cases(elevdataaust1307nouvulars$NonMarginal01))

elevdataaust1307ejectives<-elevdataaust1307[elevdataaust1307$NonMarginal02==1,]
mean(elevdataaust1307ejectives$elevation, na.rm=1)
sum(complete.cases(elevdataaust1307ejectives$NonMarginal02))

elevdataaust1307noejectives<-elevdataaust1307[elevdataaust1307$NonMarginal02==0,]
mean(elevdataaust1307noejectives $elevation, na.rm=1)
sum(complete.cases(elevdataaust1307noejectives$NonMarginal02))

elevdatacari1283uvulars<-elevdatacari1283[elevdatacari1283$NonMarginal01==1,]
mean(elevdatacari1283uvulars$elevation, na.rm=1)
sum(complete.cases(elevdatacari1283uvulars$NonMarginal01))

elevdatacari1283nouvulars<-elevdatacari1283[elevdatacari1283$NonMarginal01==0,]
mean(elevdatacari1283nouvulars$elevation, na.rm=1)
sum(complete.cases(elevdatacari1283nouvulars$NonMarginal01))

elevdatacari1283ejectives<-elevdatacari1283[elevdatacari1283$NonMarginal02==1,]
mean(elevdatacari1283ejectives$elevation, na.rm=1)
sum(complete.cases(elevdatacari1283ejectives$NonMarginal02))

elevdatacari1283noejectives<-elevdatacari1283[elevdatacari1283$NonMarginal02==0,]
mean(elevdatacari1283noejectives $elevation, na.rm=1)
sum(complete.cases(elevdatacari1283noejectives$NonMarginal02))

elevdatadrav1251uvulars<-elevdatadrav1251[elevdatadrav1251$NonMarginal01==1,]
mean(elevdatadrav1251uvulars$elevation, na.rm=1)
sum(complete.cases(elevdatadrav1251uvulars$NonMarginal01))

elevdatadrav1251nouvulars<-elevdatadrav1251[elevdatadrav1251$NonMarginal01==0,]
mean(elevdatadrav1251nouvulars$elevation, na.rm=1)
sum(complete.cases(elevdatadrav1251nouvulars$NonMarginal01))

elevdatadrav1251ejectives<-elevdatadrav1251[elevdatadrav1251$NonMarginal02==1,]
mean(elevdatadrav1251ejectives$elevation, na.rm=1)
sum(complete.cases(elevdatadrav1251ejectives$NonMarginal02))

elevdatadrav1251noejectives<-elevdatadrav1251[elevdatadrav1251$NonMarginal02==0,]
mean(elevdatadrav1251noejectives $elevation, na.rm=1)
sum(complete.cases(elevdatadrav1251noejectives$NonMarginal02))

elevdataindo1319uvulars<-elevdataindo1319[elevdataindo1319$NonMarginal01==1,]
mean(elevdataindo1319uvulars$elevation, na.rm=1)
sum(complete.cases(elevdataindo1319uvulars$NonMarginal01))

elevdataindo1319nouvulars<-elevdataindo1319[elevdataindo1319$NonMarginal01==0,]
mean(elevdataindo1319nouvulars$elevation, na.rm=1)
sum(complete.cases(elevdataindo1319nouvulars$NonMarginal01))

elevdataindo1319ejectives<-elevdataindo1319[elevdataindo1319$NonMarginal02==1,]
mean(elevdataindo1319ejectives$elevation, na.rm=1)
sum(complete.cases(elevdataindo1319ejectives$NonMarginal02))

elevdataindo1319noejectives<-elevdataindo1319[elevdataindo1319$NonMarginal02==0,]
mean(elevdataindo1319noejectives $elevation, na.rm=1)
sum(complete.cases(elevdataindo1319noejectives$NonMarginal02))

elevdatamand1469uvulars<-elevdatamand1469[elevdatamand1469$NonMarginal01==1,]
mean(elevdatamand1469uvulars$elevation, na.rm=1)
sum(complete.cases(elevdatamand1469uvulars$NonMarginal01))

elevdatamand1469nouvulars<-elevdatamand1469[elevdatamand1469$NonMarginal01==0,]
mean(elevdatamand1469nouvulars$elevation, na.rm=1)
sum(complete.cases(elevdatamand1469nouvulars$NonMarginal01))

elevdatamand1469ejectives<-elevdatamand1469[elevdatamand1469$NonMarginal02==1,]
mean(elevdatamand1469ejectives$elevation, na.rm=1)
sum(complete.cases(elevdatamand1469ejectives$NonMarginal02))

elevdatamand1469noejectives<-elevdatamand1469[elevdatamand1469$NonMarginal02==0,]
mean(elevdatamand1469noejectives $elevation, na.rm=1)
sum(complete.cases(elevdatamand1469noejectives$NonMarginal02))

elevdatamaya1287uvulars<-elevdatamaya1287[elevdatamaya1287$NonMarginal01==1,]
mean(elevdatamaya1287uvulars$elevation, na.rm=1)
sum(complete.cases(elevdatamaya1287uvulars$NonMarginal01))

elevdatamaya1287nouvulars<-elevdatamaya1287[elevdatamaya1287$NonMarginal01==0,]
mean(elevdatamaya1287nouvulars$elevation, na.rm=1)
sum(complete.cases(elevdatamaya1287nouvulars$NonMarginal01))

elevdatamaya1287ejectives<-elevdatamaya1287[elevdatamaya1287$NonMarginal02==1,]
mean(elevdatamaya1287ejectives$elevation, na.rm=1)
sum(complete.cases(elevdatamaya1287ejectives$NonMarginal02))

elevdatamaya1287noejectives<-elevdatamaya1287[elevdatamaya1287$NonMarginal02==0,]
mean(elevdatamaya1287noejectives $elevation, na.rm=1)
sum(complete.cases(elevdatamaya1287noejectives$NonMarginal02))

elevdatamong1329uvulars<-elevdatamong1329[elevdatamong1329$NonMarginal01==1,]
mean(elevdatamong1329uvulars$elevation, na.rm=1)
sum(complete.cases(elevdatamong1329uvulars$NonMarginal01))

elevdatamong1329nouvulars<-elevdatamong1329[elevdatamong1329$NonMarginal01==0,]
mean(elevdatamong1329nouvulars$elevation, na.rm=1)
sum(complete.cases(elevdatamong1329nouvulars$NonMarginal01))

elevdatamong1329ejectives<-elevdatamong1329[elevdatamong1329$NonMarginal02==1,]
mean(elevdatamong1329ejectives$elevation, na.rm=1)
sum(complete.cases(elevdatamong1329ejectives$NonMarginal02))

elevdatamong1329noejectives<-elevdatamong1329[elevdatamong1329$NonMarginal02==0,]
mean(elevdatamong1329noejectives $elevation, na.rm=1)
sum(complete.cases(elevdatamong1329noejectives$NonMarginal02))

elevdatanakh1245uvulars<-elevdatanakh1245[elevdatanakh1245$NonMarginal01==1,]
mean(elevdatanakh1245uvulars$elevation, na.rm=1)
sum(complete.cases(elevdatanakh1245uvulars$NonMarginal01))

elevdatanakh1245nouvulars<-elevdatanakh1245[elevdatanakh1245$NonMarginal01==0,]
mean(elevdatanakh1245nouvulars$elevation, na.rm=1)
sum(complete.cases(elevdatanakh1245nouvulars$NonMarginal01))

elevdatanakh1245ejectives<-elevdatanakh1245[elevdatanakh1245$NonMarginal02==1,]
mean(elevdatanakh1245ejectives$elevation, na.rm=1)
sum(complete.cases(elevdatanakh1245ejectives$NonMarginal02))

elevdatanakh1245noejectives<-elevdatanakh1245[elevdatanakh1245$NonMarginal02==0,]
mean(elevdatanakh1245noejectives $elevation, na.rm=1)
sum(complete.cases(elevdatanakh1245noejectives$NonMarginal02))

elevdataotom1299uvulars<-elevdataotom1299[elevdataotom1299$NonMarginal01==1,]
mean(elevdataotom1299uvulars$elevation, na.rm=1)
sum(complete.cases(elevdataotom1299uvulars$NonMarginal01))

elevdataotom1299nouvulars<-elevdataotom1299[elevdataotom1299$NonMarginal01==0,]
mean(elevdataotom1299nouvulars$elevation, na.rm=1)
sum(complete.cases(elevdataotom1299nouvulars$NonMarginal01))

elevdataotom1299ejectives<-elevdataotom1299[elevdataotom1299$NonMarginal02==1,]
mean(elevdataotom1299ejectives$elevation, na.rm=1)
sum(complete.cases(elevdataotom1299ejectives$NonMarginal02))

elevdataotom1299noejectives<-elevdataotom1299[elevdataotom1299$NonMarginal02==0,]
mean(elevdataotom1299noejectives $elevation, na.rm=1)
sum(complete.cases(elevdataotom1299noejectives$NonMarginal02))

elevdataquec1387uvulars<-elevdataquec1387[elevdataquec1387$NonMarginal01==1,]
mean(elevdataquec1387uvulars$elevation, na.rm=1)
sum(complete.cases(elevdataquec1387uvulars$NonMarginal01))

elevdataquec1387nouvulars<-elevdataquec1387[elevdataquec1387$NonMarginal01==0,]
mean(elevdataquec1387nouvulars$elevation, na.rm=1)
sum(complete.cases(elevdataquec1387nouvulars$NonMarginal01))

elevdataquec1387ejectives<-elevdataquec1387[elevdataquec1387$NonMarginal02==1,]
mean(elevdataquec1387ejectives$elevation, na.rm=1)
sum(complete.cases(elevdataquec1387ejectives$NonMarginal02))

elevdataquec1387noejectives<-elevdataquec1387[elevdataquec1387$NonMarginal02==0,]
mean(elevdataquec1387noejectives $elevation, na.rm=1)
sum(complete.cases(elevdataquec1387noejectives$NonMarginal02))

elevdatasali1255uvulars<-elevdatasali1255[elevdatasali1255$NonMarginal01==1,]
mean(elevdatasali1255uvulars$elevation, na.rm=1)
sum(complete.cases(elevdatasali1255uvulars$NonMarginal01))

elevdatasali1255nouvulars<-elevdatasali1255[elevdatasali1255$NonMarginal01==0,]
mean(elevdatasali1255nouvulars$elevation, na.rm=1)
sum(complete.cases(elevdatasali1255nouvulars$NonMarginal01))

elevdatasali1255ejectives<-elevdatasali1255[elevdatasali1255$NonMarginal02==1,]
mean(elevdatasali1255ejectives$elevation, na.rm=1)
sum(complete.cases(elevdatasali1255ejectives$NonMarginal02))

elevdatasali1255noejectives<-elevdatasali1255[elevdatasali1255$NonMarginal02==0,]
mean(elevdatasali1255noejectives $elevation, na.rm=1)
sum(complete.cases(elevdatasali1255noejectives$NonMarginal02))

elevdatasino1245uvulars<-elevdatasino1245[elevdatasino1245$NonMarginal01==1,]
mean(elevdatasino1245uvulars$elevation, na.rm=1)
sum(complete.cases(elevdatasino1245uvulars$NonMarginal01))

elevdatasino1245nouvulars<-elevdatasino1245[elevdatasino1245$NonMarginal01==0,]
mean(elevdatasino1245nouvulars$elevation, na.rm=1)
sum(complete.cases(elevdatasino1245nouvulars$NonMarginal01))

elevdatasino1245ejectives<-elevdatasino1245[elevdatasino1245$NonMarginal02==1,]
mean(elevdatasino1245ejectives$elevation, na.rm=1)
sum(complete.cases(elevdatasino1245ejectives$NonMarginal02))

elevdatasino1245noejectives<-elevdatasino1245[elevdatasino1245$NonMarginal02==0,]
mean(elevdatasino1245noejectives $elevation, na.rm=1)
sum(complete.cases(elevdatasino1245noejectives$NonMarginal02))

elevdatataik1256uvulars<-elevdatataik1256[elevdatataik1256$NonMarginal01==1,]
mean(elevdatataik1256uvulars$elevation, na.rm=1)
sum(complete.cases(elevdatataik1256uvulars$NonMarginal01))

elevdatataik1256nouvulars<-elevdatataik1256[elevdatataik1256$NonMarginal01==0,]
mean(elevdatataik1256nouvulars$elevation, na.rm=1)
sum(complete.cases(elevdatataik1256nouvulars$NonMarginal01))

elevdatataik1256ejectives<-elevdatataik1256[elevdatataik1256$NonMarginal02==1,]
mean(elevdatataik1256ejectives$elevation, na.rm=1)
sum(complete.cases(elevdatataik1256ejectives$NonMarginal02))

elevdatataik1256noejectives<-elevdatataik1256[elevdatataik1256$NonMarginal02==0,]
mean(elevdatataik1256noejectives $elevation, na.rm=1)
sum(complete.cases(elevdatataik1256noejectives$NonMarginal02))

elevdatatupi1275uvulars<-elevdatatupi1275[elevdatatupi1275$NonMarginal01==1,]
mean(elevdatatupi1275uvulars$elevation, na.rm=1)
sum(complete.cases(elevdatatupi1275uvulars$NonMarginal01))

elevdatatupi1275nouvulars<-elevdatatupi1275[elevdatatupi1275$NonMarginal01==0,]
mean(elevdatatupi1275nouvulars$elevation, na.rm=1)
sum(complete.cases(elevdatatupi1275nouvulars$NonMarginal01))

elevdatatupi1275ejectives<-elevdatatupi1275[elevdatatupi1275$NonMarginal02==1,]
mean(elevdatatupi1275ejectives$elevation, na.rm=1)
sum(complete.cases(elevdatatupi1275ejectives$NonMarginal02))

elevdatatupi1275noejectives<-elevdatatupi1275[elevdatatupi1275$NonMarginal02==0,]
mean(elevdatatupi1275noejectives $elevation, na.rm=1)
sum(complete.cases(elevdatatupi1275noejectives$NonMarginal02))

elevdataturk1311uvulars<-elevdataturk1311[elevdataturk1311$NonMarginal01==1,]
mean(elevdataturk1311uvulars$elevation, na.rm=1)
sum(complete.cases(elevdataturk1311uvulars$NonMarginal01))

elevdataturk1311nouvulars<-elevdataturk1311[elevdataturk1311$NonMarginal01==0,]
mean(elevdataturk1311nouvulars$elevation, na.rm=1)
sum(complete.cases(elevdataturk1311nouvulars$NonMarginal01))

elevdataturk1311ejectives<-elevdataturk1311[elevdataturk1311$NonMarginal02==1,]
mean(elevdataturk1311ejectives$elevation, na.rm=1)
sum(complete.cases(elevdataturk1311ejectives$NonMarginal02))

elevdataturk1311noejectives<-elevdataturk1311[elevdataturk1311$NonMarginal02==0,]
mean(elevdataturk1311noejectives $elevation, na.rm=1)
sum(complete.cases(elevdataturk1311noejectives$NonMarginal02))

elevdataural1272uvulars<-elevdataural1272[elevdataural1272$NonMarginal01==1,]
mean(elevdataural1272uvulars$elevation, na.rm=1)
sum(complete.cases(elevdataural1272uvulars$NonMarginal01))

elevdataural1272nouvulars<-elevdataural1272[elevdataural1272$NonMarginal01==0,]
mean(elevdataural1272nouvulars$elevation, na.rm=1)
sum(complete.cases(elevdataural1272nouvulars$NonMarginal01))

elevdataural1272ejectives<-elevdataural1272[elevdataural1272$NonMarginal02==1,]
mean(elevdataural1272ejectives$elevation, na.rm=1)
sum(complete.cases(elevdataural1272ejectives$NonMarginal02))

elevdataural1272noejectives<-elevdataural1272[elevdataural1272$NonMarginal02==0,]
mean(elevdataural1272noejectives $elevation, na.rm=1)
sum(complete.cases(elevdataural1272noejectives$NonMarginal02))

elevdatautoa1244uvulars<-elevdatautoa1244[elevdatautoa1244$NonMarginal01==1,]
mean(elevdatautoa1244uvulars$elevation, na.rm=1)
sum(complete.cases(elevdatautoa1244uvulars$NonMarginal01))

elevdatautoa1244nouvulars<-elevdatautoa1244[elevdatautoa1244$NonMarginal01==0,]
mean(elevdatautoa1244nouvulars$elevation, na.rm=1)
sum(complete.cases(elevdatautoa1244nouvulars$NonMarginal01))

elevdatautoa1244ejectives<-elevdatautoa1244[elevdatautoa1244$NonMarginal02==1,]
mean(elevdatautoa1244ejectives$elevation, na.rm=1)
sum(complete.cases(elevdatautoa1244ejectives$NonMarginal02))

elevdatautoa1244noejectives<-elevdatautoa1244[elevdatautoa1244$NonMarginal02==0,]
mean(elevdatautoa1244noejectives $elevation, na.rm=1)
sum(complete.cases(elevdatautoa1244noejectives$NonMarginal02))

elevdatagong1255uvulars<-elevdatagong1255[elevdatagong1255$NonMarginal01==1,]
mean(elevdatagong1255uvulars$elevation, na.rm=1)
sum(complete.cases(elevdatagong1255uvulars$NonMarginal01))

elevdatagong1255nouvulars<-elevdatagong1255[elevdatagong1255$NonMarginal01==0,]
mean(elevdatagong1255nouvulars$elevation, na.rm=1)
sum(complete.cases(elevdatagong1255nouvulars$NonMarginal01))

elevdatagong1255ejectives<-elevdatagong1255[elevdatagong1255$NonMarginal02==1,]
mean(elevdatagong1255ejectives$elevation, na.rm=1)
sum(complete.cases(elevdatagong1255ejectives$NonMarginal02))

elevdatagong1255noejectives<-elevdatagong1255[elevdatagong1255$NonMarginal02==0,]
mean(elevdatagong1255noejectives $elevation, na.rm=1)
sum(complete.cases(elevdatagong1255noejectives$NonMarginal02))


#plots
elevdatawithoutnas<-subset(elevdata, macroarea2!="NA")
p <- ggplot(data = elevdatawithoutnas, aes(x= NonMarginal01, y=elevation)) +
  geom_boxplot(aes(fill=as.logical(NonMarginal01))) +
  scale_fill_grey()

p <- p + facet_wrap( ~ macroarea2, scales="fixed")
p <- p + xlab("Presence vs. Absence of Uvulars") + ylab("Elevation") + ggtitle("Title")
p <- p + guides(fill=guide_legend(title="Legend"))


p <- ggplot(data = elevdatawithoutnas, aes(x= NonMarginal02, y=elevation)) +
  geom_boxplot(aes(fill=as.logical(NonMarginal02))) +
  scale_fill_grey()

p <- p + facet_wrap( ~ macroarea2, scales="fixed")
p <- p + xlab("Presence vs. Absence of Ejectives") + ylab("Elevation") + ggtitle("Title")
p <- p + guides(fill=guide_legend(title="Legend"))

elevdatalargefamilies<- subset(elevdata, family_id=="afro1255" | family_id=="araw1281" | family_id=="atha1245" | family_id=="atla1278" | family_id==" aust1307" | family_id=="cari1283" | family_id=="mand1469" | family_id=="maya1287" | family_id=="mong1329" | family_id=="nakh1245" | family_id=="otom1299" | family_id=="sali1255" | family_id=="sino1245" | family_id=="taik1256"| family_id=="tupi1275" | family_id=="turk1311" | family_id=="ural1272" | family_id=="gong1255", select=InventoryID:elevationlog10)

p <- ggplot(data = elevdatalargefamilies, aes(x= NonMarginal01, y=elevation)) +
  geom_boxplot(aes(fill=as.logical(NonMarginal01))) +
  scale_fill_grey()

p <- p + facet_wrap( ~ family_id, scales="fixed")
p <- p + xlab("Presence vs. Absence of Uvulars") + ylab("Elevation") + ggtitle("Title")
p <- p + guides(fill=guide_legend(title="Legend"))


p <- ggplot(data = elevdatalargefamilies, aes(x=NonMarginal02, y=elevation)) +
  geom_boxplot(aes(fill=as.logical(NonMarginal02))) +
  scale_fill_grey()

p <- p + facet_wrap( ~ family_id, scales="fixed")
p <- p + xlab("Presence vs. Absence of Uvulars") + ylab("Elevation") + ggtitle("Title")
p <- p + guides(fill=guide_legend(title="Legend"))


meanelev<-c(879.3301,420.7209, 934.5,603.0733,349.7009,579.1667,578.4516,668.0979,329.3023,1120.083,1762.818,1513.769,1472.412,2672,379.2222,2131.161,576.8421,
            229.1633,941.0952,173.3448,1084.333,1689.333)

uvularproportion<-c(0.2692308, 0.02325581, 0.4,0.03800475,0.05357143,0.04166667,
                    0.09090909,0.2384106,0.1162791,0.5,0.6666667,1,0.05882353,0.6,1,0.1632653,
                    0.1578947,0.02,0.6666667,0.09677419,0.3333333,0)


ejectiveproportion<-c(0.3076923,0, 1,0.01187648,0,0,0,0.01324503,0,0.9166667,0,1,0.1764706,0.12,0.6,0.01020408,0.05263158,0,0,0,0,0.9)

plot(meanelev, uvularproportion, bg="black", pch=21, xlab="Mean elevation per family", ylab="Proportion Uvulars")
lines(lowess(meanelev, uvularproportion, f=10, iter=10))

plot(meanelev, ejectiveproportion, bg="black", pch=21, xlab="Mean elevation per family", ylab="Proportion Ejectives")
lines(lowess(meanelev, ejectiveproportion, f=10, iter=10))

