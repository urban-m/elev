Production of maps and plots for descriptive statistics
================
Matthias Urban
10 November, 2020

``` r
library(tidyverse)
require(OpenStreetMap)
require(rgdal)
require(sf)
require(ggplot2)
```

# Data preparation

Read in data

``` r
elevdata <- read.csv('../Data/uvulars_ejectives_pruned2.csv', header =T)
```

Change separator from comma to dot for coordinates and treat as numeric

``` r
elevdata$latitude<-as.numeric(gsub(',','.',elevdata$latitude))
elevdata$longitude<-as.numeric(gsub(',','.',elevdata$longitude))
```

Treat macroareas as factors

``` r
elevdata$macroarea2<-as.factor(elevdata$macroarea2)
```

Reduce the data to a binary distinction between presence vs. absence of
ejectives/uvulars

``` r
elevdata<-mutate(elevdata, NonMarginal01 = as.logical(Nonmarginal_Uvular), NonMarginal01 = as.numeric(NonMarginal01))
elevdata<-mutate(elevdata, NonMarginal02 = as.logical(Nonmarginal_Ejective), NonMarginal02 = as.numeric(NonMarginal02))
```

# Plot maps

## Uvulars

``` r
map <- openmap(c(85,-180), c(-80,180), type='nps', minNumTiles=100)
map<-openproj(map, projection="+init=epsg:3832")
plot(map)

plotdatanouvulars<-drop_na(elevdata %>% filter(NonMarginal01==F) %>% select(latitude, longitude))
plotdatanouvulars<-st_as_sf(plotdatanouvulars, coords = c("longitude", "latitude"), crs=4326)
plotdatanouvulars<-st_transform(plotdatanouvulars, crs=3832)
plot(plotdatanouvulars, pch=21, col="black", bg="white", cex=1.2, add=T)

plotdatauvulars<- drop_na(elevdata %>% filter(NonMarginal01==T) %>% select(latitude, longitude))
plotdatauvulars<-st_as_sf(plotdatauvulars, coords = c("longitude", "latitude"), crs=4326)
plotdatauvulars<-st_transform(plotdatauvulars, crs=3832)
plot(plotdatauvulars, pch=21, col="white", bg="black", cex=1.2, add=T)

```

![](Maps-and-descriptive-statistics_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

## Ejectives

``` r
map <- openmap(c(85,-180), c(-80,180), type='nps', minNumTiles=100)
map<-openproj(map, projection="+init=epsg:3832")
plot(map)


plotdatanoejectives<- drop_na(elevdata %>% filter(NonMarginal02==F) %>% select(latitude, longitude))
plotdatanoeejectives<-st_as_sf(plotdatanoeejectives, coords = c("longitude", "latitude"), crs=4326)
plotdatanoeejectives<-st_transform(plotdatanoeejectives, crs=3832)
plot(plotdatanoeejectives, pch=21, col="black", bg="white", cex=1.2, add=T)

plotdataejectives<- drop_na(elevdata %>% filter(NonMarginal02==T) %>% select(latitude, longitude))
plotdataejectives<-st_as_sf(plotdataejectives, coords = c("longitude", "latitude"), crs=4326)
plotdataejectives<-st_transform(plotdataejectives, crs=3832)
plot(plotdataejectives, pch=21, col="white", bg="black", cex=1.2, add=T)

```

![](Maps-and-descriptive-statistics_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

# Descriptive stats

Subset data

``` r
uvulars<-elevdata %>% filter(NonMarginal01==T)
nouvulars<-elevdata %>% filter(NonMarginal01==F)
ejectives<-elevdata %>% filter(NonMarginal02==T)
noejectives<-elevdata %>% filter(NonMarginal02==F)
```

Compute global means

``` r
mean(uvulars$elevation, na.rm=1)
```

    ## [1] 1136.301

``` r
mean(nouvulars$elevation, na.rm=1)
```

    ## [1] 590.1413

``` r
mean(ejectives$elevation, na.rm=1)
```

    ## [1] 1236.913

``` r
mean(noejectives$elevation, na.rm=1)
```

    ## [1] 606.0188

Compute means by area

``` r
aggregate(elevation~macroarea2, FUN='mean', data=uvulars)
```

    ##               macroarea2 elevation
    ## 1                 Africa  755.3889
    ## 2                 Europe  872.9118
    ## 3         Middle America 1034.6923
    ## 4             New Guinea 2184.0000
    ## 5          North America  627.1765
    ## 6       Northern Eurasia 1755.9636
    ## 7 South & Southeast Asia  722.8571
    ## 8          South America 1776.5263
    ## 9           Western Asia  875.2143

``` r
aggregate(elevation~macroarea2, FUN='mean', data=nouvulars)
```

    ##                macroarea2 elevation
    ## 1                  Africa  678.2531
    ## 2               Australia  190.6528
    ## 3                  Europe  342.4667
    ## 4          Middle America  995.1892
    ## 5              New Guinea  645.0137
    ## 6           North America  743.8364
    ## 7        Northern Eurasia 1868.6556
    ## 8                 Oceania  300.3889
    ## 9  South & Southeast Asia  476.2844
    ## 10          South America  441.9898
    ## 11           Western Asia 1340.6000

``` r
aggregate(elevation~macroarea2, FUN='mean', data=ejectives)
```

    ##         macroarea2 elevation
    ## 1           Africa 1474.8333
    ## 2           Europe 1388.0000
    ## 3   Middle America 1219.3333
    ## 4    North America  749.6739
    ## 5 Northern Eurasia 1303.7500
    ## 6    South America 1459.0833
    ## 7     Western Asia 1182.0000

``` r
aggregate(elevation~macroarea2, FUN='mean', data=noejectives)
```

    ##                macroarea2 elevation
    ## 1                  Africa  609.0000
    ## 2               Australia  190.6528
    ## 3                  Europe  310.1573
    ## 4          Middle America  913.8000
    ## 5              New Guinea  665.8108
    ## 6           North America  645.3488
    ## 7        Northern Eurasia 1840.7234
    ## 8                 Oceania  300.3889
    ## 9  South & Southeast Asia  491.6267
    ## 10          South America  527.6645
    ## 11           Western Asia  948.5333

Compute number of observations by area

``` r
aggregate(NonMarginal01~macroarea2, FUN=length, data=uvulars)
```

    ##               macroarea2 NonMarginal01
    ## 1                 Africa            54
    ## 2                 Europe            35
    ## 3         Middle America            13
    ## 4             New Guinea             1
    ## 5          North America            35
    ## 6       Northern Eurasia            59
    ## 7 South & Southeast Asia            15
    ## 8          South America            38
    ## 9           Western Asia            16

``` r
aggregate(NonMarginal01~macroarea2, FUN=length, data=nouvulars)
```

    ##                macroarea2 NonMarginal01
    ## 1                  Africa           653
    ## 2               Australia           312
    ## 3                  Europe            81
    ## 4          Middle America            37
    ## 5              New Guinea            75
    ## 6           North America            56
    ## 7        Northern Eurasia            92
    ## 8                 Oceania            38
    ## 9  South & Southeast Asia           219
    ## 10          South America           296
    ## 11           Western Asia             5

``` r
aggregate(NonMarginal02~macroarea2, FUN=length, data=ejectives)
```

    ##         macroarea2 NonMarginal02
    ## 1           Africa            61
    ## 2           Europe            21
    ## 3   Middle America            15
    ## 4    North America            46
    ## 5 Northern Eurasia             4
    ## 6    South America            24
    ## 7     Western Asia             4

``` r
aggregate(NonMarginal02~macroarea2, FUN=length, data=noejectives)
```

    ##                macroarea2 NonMarginal02
    ## 1                  Africa           646
    ## 2               Australia           312
    ## 3                  Europe            95
    ## 4          Middle America            35
    ## 5              New Guinea            76
    ## 6           North America            45
    ## 7        Northern Eurasia           147
    ## 8                 Oceania            38
    ## 9  South & Southeast Asia           234
    ## 10          South America           310
    ## 11           Western Asia            17

## Plot distribution of number of uvulars and ejectives depending on altitude

``` r
nobservations <- function(x){return(c(y = 6000, label = length(x)))}

elevdata %>% filter(Nonmarginal_Uvular < 15) %>%
  ggplot(aes(group=Nonmarginal_Uvular, x=Nonmarginal_Uvular, y=elevation)) +
  geom_boxplot(outlier.alpha=0.1) +
  stat_summary(fun.data = nobservations, geom = "text", fun = median) +
  labs(x="Number of uvular consonants", y ="Elevation")


```

    ## Warning: Removed 97 rows containing non-finite values (stat_boxplot).

![](Maps-and-descriptive-statistics_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

``` r
nobservations <- function(x){return(c(y = 6000, label = length(x)))}

elevdata %>% filter(Nonmarginal_Ejective < 15) %>% 
  ggplot(aes(group=Nonmarginal_Ejective, x=Nonmarginal_Ejective, y=elevation)) +
  geom_boxplot(outlier.alpha=0.1) +
  stat_summary(fun.data = nobservations, geom = "text", fun = median) +
  labs(x="Number of ejective consonants", y ="Elevation")
```

    ## Warning: Removed 97 rows containing non-finite values (stat_boxplot).

![](Maps-and-descriptive-statistics_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->