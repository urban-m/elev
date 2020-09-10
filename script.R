#packages needed
#tidyverse
#brms
#OpenStreetMap
#rgdal
#sp
#bootfix
#jaccard

require(tidyverse)
require(brms)
require(OpenStreetMap)
require(rgdal)
require(sp)
require(boot)
require(jaccard)
require(ggplot2)

##Tell R where to find R tools
Sys.setenv(PATH = paste(Sys.getenv("PATH"), "C:\\RTools40","C:\\RTools40\\usr\\bin", sep=";"))
options(buildtools.check = function(action)TRUE)

##Read in data
elevdata<-read.csv('../uvulars_ejectives_pruned2', header=T)

##Change separator from comma to dot for coordinates and treat as numeric 
elevdata$latitude<-as.numeric(gsub(',','.',elevdata$latitude))
elevdata$longitude<-as.numeric(gsub(',','.',elevdata$longitude))

##treat macroareas as factors
elevdata$macroarea2<-as.factor(elevdata$macroarea2)

##Reduce data to a binary distinction between presence vs. absence of ejectives/uvulars
elevdata <- mutate(elevdata, NonMarginal01 = as.logical(Nonmarginal_Uvular), NonMarginal01 = as.numeric(NonMarginal01))
elevdata<- mutate(elevdata, NonMarginal02 = as.logical(Nonmarginal_Ejective), NonMarginal02 = as.numeric(NonMarginal02))


##plot maps
#uvulars
map <- openmap(c(80,-180), c(-70,180), type='nps', minNumTiles=100)
plot(map)

plotdatauvulars<- drop_na(elevdata %>% filter(NonMarginal01==T) %>% select(latitude, longitude))
coordinates(plotdatauvulars)<-~longitude+latitude
proj4string(plotdatauvulars)<-CRS("+init=epsg:4326")
points(spTransform(plotdatauvulars,osm()), pch=21, col="white", bg="black", cex=1.2)

plotdatanouvulars<-drop_na(elevdata %>% filter(NonMarginal01==F) %>% select(latitude, longitude))
coordinates(plotdatanouvulars)<-~longitude+latitude
proj4string(plotdatanouvulars)<-CRS("+init=epsg:4326")
points(spTransform(plotdatanouvulars,osm()), pch=21, col="black", bg="white", cex=1.2)

#ejectives
map <- openmap(c(80,-180), c(-70,180), type='nps', minNumTiles=100)
plot(map)

plotdataejectives<- drop_na(elevdata %>% filter(NonMarginal02==T) %>% select(latitude, longitude))
coordinates(plotdataejectives)<-~longitude+latitude
proj4string(plotdataejectives)<-CRS("+init=epsg:4326")
points(spTransform(plotdataejectives,osm()), pch=21, col="white", bg="black", cex=1.2)

plotdatanoejectives<- drop_na(elevdata %>% filter(NonMarginal02==F) %>% select(latitude, longitude))
coordinates(plotdatanoejectives)<-~longitude+latitude
proj4string(plotdatanoejectives)<-CRS("+init=epsg:4326")
points(spTransform(plotdatanoejectives,osm()), pch=21, col="black", bg="white", cex=1.2)

##Descriptive stats
#subset data
uvulars<-elevdata %>% filter(NonMarginal01==T)
nouvulars<-elevdata %>% filter(NonMarginal01==F)
ejectives<-elevdata %>% filter(NonMarginal02==T)
noejectives<-elevdata %>% filter(NonMarginal02==F)

#compute global means
mean(uvulars$elevation, na.rm=1)
mean(nouvulars$elevation, na.rm=1)
mean(ejectives$elevation, na.rm=1)
mean(noejectives$elevation, na.rm=1)

#compute means by area
aggregate(elevation~macroarea2, FUN='mean', data=uvulars)
aggregate(elevation~macroarea2, FUN='mean', data=nouvulars)
aggregate(elevation~macroarea2, FUN='mean', data=ejectives)
aggregate(elevation~macroarea2, FUN='mean', data=noejectives)

#compute number of observations by area
aggregate(NonMarginal01~macroarea2, FUN=length, data=uvulars)
aggregate(NonMarginal01~macroarea2, FUN=length, data=nouvulars)
aggregate(NonMarginal02~macroarea2, FUN=length, data=ejectives)
aggregate(NonMarginal02~macroarea2, FUN=length, data=noejectives)

#plot distribution of number of uvulars and ejectives depending on altitude
elevdata %>% filter(Nonmarginal_Uvular < 15) %>%
  ggplot(aes(group=Nonmarginal_Uvular, x=Nonmarginal_Uvular, y=elevation)) +
  geom_boxplot(outlier.alpha=0.1) +
  labs(x="Number of uvular consonants", y ="Elevation")

elevdata %>% filter(Nonmarginal_Ejective < 15) %>% 
  ggplot(aes(group=Nonmarginal_Ejective, x=Nonmarginal_Ejective, y=elevation)) +
  geom_boxplot(outlier.alpha=0.1) +
  labs(x="Number of ejective consonants", y ="Elevation")

elevdata %>% filter(Nonmarginal_Uvular < 15) %>%
  ggplot(aes(group=Nonmarginal_Uvular, x=Nonmarginal_Uvular, y=elevation)) +
  geom_boxplot(outlier.alpha=0.1, width=2.5, varwidth=T) +
  labs(x="Number of uvular consonants", y ="Elevation")

elevdata %>% filter(Nonmarginal_Ejective < 15) %>% 
  ggplot(aes(group=Nonmarginal_Ejective, x=Nonmarginal_Ejective, y=elevation)) +
  geom_boxplot(outlier.alpha=0.1, width=2.5,  varwidth=T) +
  labs(x="Number of ejective consonants", y ="Elevation")

## Modeling
#Tranform elevation to its log10 for modelling
elevdata <- mutate(elevdata, elevationlog10 = log10(elevation))

#Bayesian logistic mixed effects regressions
#set prior
priors <-set_prior("normal(0, 2)", class="b", coef="elevationlog10")

#Model for uvulars
elevmodeluvulars<-brm(NonMarginal01 ~ elevationlog10 + (1+ elevationlog10| macroarea2) +(1|family_id), family= 'bernoulli', data=elevdata, warmup=6000, iter=8000, chains=4, prior=priors, control = list(adapt_delta = 0.99))

#Model assessment and checks
#check Rhat and ESS values
summary(elevmodeluvulars)

#Inspect chains
plot(elevmodeluvulars)

#Inspect plots of observed data and posterior predictive samples
pp_check(elevmodeluvulars)
pp_check(elevmodeluvulars, type="error_binned")

#Assess predictive accuracy
modelled_elevdata<-elevdata %>% drop_na(NonMarginal01, elevationlog10, macroarea2, family_id)
elevmodeluvulars_pred <- predict(elevmodeluvulars, type = "response")[ , "Estimate"]
elevmodeluvulars_pred <- as.numeric(elevmodeluvulars_pred > mean(modelled_elevdata$NonMarginal01))
(classtab_elevmodeluvulars <- table(predicted = elevmodeluvulars_pred, observed = modelled_elevdata$NonMarginal01))
(acc_elevmodeluvulars <- sum(diag(classtab_elevmodeluvulars)) / sum(classtab_elevmodeluvulars))

#Assess posterior probability versus chance
elevmodeluvularssamples<-posterior_samples(elevmodeluvulars)
sum(elevmodeluvularssamples$b_elevationlog10 < 0) /nrow(elevmodeluvularssamples)

#model for ejectives 
elevmodelejectives<-brm(NonMarginal02 ~ elevationlog10 + (1+ elevationlog10| macroarea2) +(1|family_id), family= 'bernoulli', data=elevdata, warmup=6000, iter=8000, chains=4, prior=priors, control = list(adapt_delta = 0.99))

#Model assessment and checks
#check Rhat and ESS values
summary(elevmodelejectives)

#Inspect chains
plot(elevmodelejectives)

#Inspect plots of observed data and posterior predictive samples
pp_check(elevmodelejectives)
pp_check(elevmodelejectives, type="error_binned")

#Assess predictive accuracy
modelled_elevdata<-elevdata %>% drop_na(NonMarginal02, elevationlog10, macroarea2, family_id)
elevmodelejectives_pred <- predict(elevmodelejectives, type = "response")[ , "Estimate"]
elevmodelejectives_pred <- as.numeric(elevmodelejectives_pred > mean(modelled_elevdata$NonMarginal02))
(classtab_elevmodelejectives <- table(predicted = elevmodelejectives_pred, observed = modelled_elevdata$NonMarginal02))
(acc_elevmodelejectives <- sum(diag(classtab_elevmodelejectives)) / sum(classtab_elevmodelejectives))

#Assess posterior probability versus chance
elevmodelejectivessamples<-posterior_samples(elevmodelejectives)
sum(elevmodelejectivessamples$b_elevationlog10 < 0) /nrow(elevmodelejectivessamples)

##by-area and by-family analysis
#by-area
#compute mean elevations and proportions of uvulars and ejectives per by area
meanelevarea<-aggregate(elevation~macroarea2, FUN="mean", data=elevdata)
uvularproportionarea<-aggregate(NonMarginal01~macroarea2, FUN="mean", data=elevdata)
ejectiveproportionarea<-aggregate(NonMarginal02~macroarea2, FUN="mean", data=elevdata)

#least squares regression
summary(lm(uvularproportionarea$NonMarginal01~meanelevarea$elevation))
summary(lm(ejectiveproportionarea$NonMarginal02~meanelevarea$elevation))

#plot results
plot(meanelevarea$elevation, uvularproportionarea$NonMarginal01, bg="black", pch=21, xlab="Mean elevation per area", ylab="Proportion Uvulars")
lines(lowess(meanelevarea$elevation, uvularproportionarea$NonMarginal01, f=10, iter=10))
plot(meanelevarea$elevation, ejectiveproportionarea$NonMarginal02, bg="black", pch=21, xlab="Mean elevation per area", ylab="Proportion Ejectives")
lines(lowess(meanelevarea$elevation, ejectiveproportionarea$NonMarginal02, f=10, iter=10))

#by family
largefamilies<-filter(elevdata, family_id %in% c("afro1255", "araw1281", "atha1245", "atla1278", "aust1307", "cari1283", "gong1255", "mand1469", "maya1287", "mong1329", "nakh1245", "otom1299", "sali1255", "sino1245", "taik1256", "tupi1275", "turk1311", "ural1272"))

#compute mean elevations and proportions of uvulars and ejectives per by area
meanelevfamily<-aggregate(elevation~family_id, FUN="mean", data=largefamilies)
uvularproportionfamily<-aggregate(NonMarginal01~family_id, FUN="mean", data=largefamilies)
ejectiveproportionfamily<-aggregate(NonMarginal02~family_id, FUN="mean", data=largefamilies)

#least squares regression
summary(lm(uvularproportionfamily$NonMarginal01~meanelevfamily$elevation))
summary(lm(ejectiveproportionfamily$NonMarginal02~meanelevfamily$elevation))

#plot results
plot(meanelevfamily$elevation, uvularproportionfamily$NonMarginal01, bg="black", pch=21, xlab="Mean elevation per family", ylab="Proportion Uvulars")
lines(lowess(meanelevfamily$elevation, uvularproportionfamily$NonMarginal01, f=10, iter=10))
plot(meanelevfamily$elevation, ejectiveproportionfamily$NonMarginal02, bg="black", pch=21, xlab="Mean elevation per family", ylab="Proportion Ejectives")
lines(lowess(meanelevfamily$elevation, ejectiveproportionfamily$NonMarginal02, f=10, iter=10))

##Cooccurence of uvulars and ejectives within families
familycountuvulars<-aggregate(NonMarginal01~family_id, FUN='mean', data=elevdata)
familycountuvulars$NonMarginal01<-as.logical(familycountuvulars$NonMarginal01)
mean(familycountuvulars$NonMarginal01)
sum(familycountuvulars$NonMarginal01==T)
familycountejectives<-aggregate(NonMarginal02~family_id, FUN='mean', data=elevdata)
familycountejectives$NonMarginal02<-as.logical(familycountejectives$NonMarginal02)
mean(familycountuvulars$NonMarginal01)
sum(familycountuvulars$NonMarginal01==T)
jaccard.test(familycountuvulars$NonMarginal01, familycountejectives$NonMarginal02, , px=0.2753036, py=0.2631579, method="exact")