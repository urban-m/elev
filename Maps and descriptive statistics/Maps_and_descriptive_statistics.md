Production of maps and plots for descriptive statistics
================
Matthias Urban
23 Dezember, 2020

# Required packages

``` r
require(tidyverse)
require(OpenStreetMap)
require(rgdal)
require(sp)
require(sf)
require(ggplot2)
```

# Data preparation

Read in data

``` r
elevdata <- read.csv("../Data/uvulars_ejectives_pruned2_rhotics.csv", header = T)
```

Change separator from comma to dot for coordinates and treat as numeric

``` r
elevdata$latitude <- as.numeric(gsub(",", ".", elevdata$latitude))
elevdata$longitude <- as.numeric(gsub(",", ".", elevdata$longitude))
```

Treat macroareas as factors

``` r
elevdata$macroarea2 <- as.factor(elevdata$macroarea2)
```

Remove rows with empty cells for Elevation

``` r
elevdata <- drop_na(elevdata, elevation)
```

Reduce the data to a binary distinction between presence vs. absence of
ejectives/uvulars

``` r
elevdata <- mutate(elevdata, NonMarginal01 = as.logical(Nonmarginal_Uvular), NonMarginal01 = as.numeric(NonMarginal01))
elevdata <- mutate(elevdata, NonMarginal02 = as.logical(Nonmarginal_Ejective), NonMarginal02 = as.numeric(NonMarginal02))
```

# Plot maps

## Ejectives

``` r
tiff(file="fig1_pacific_centered.tiff", width=2013, height = 1656)
map <- openmap(c(78, -180), c(-62, 180), type = "nps", minNumTiles = 100)
map <- openproj(map, projection = "+init=epsg:3832")
plot(map)
plotdatanoejectives <- drop_na(elevdata %>% filter(NonMarginal02 == F) %>% select(latitude, longitude))
plotdatanoejectives <- st_as_sf(plotdatanoejectives, coords = c("longitude", "latitude"), crs = 4326)
plotdatanoejectives <- st_transform(plotdatanoejectives, crs = 3832)
plot(plotdatanoejectives, pch = 21, col = "black", bg = "white", cex = 2.3, add = T)

plotdataejectives <- drop_na(elevdata %>% filter(NonMarginal02 == T) %>% select(latitude, longitude))
plotdataejectives <- st_as_sf(plotdataejectives, coords = c("longitude", "latitude"), crs = 4326)
plotdataejectives <- st_transform(plotdataejectives, crs = 3832)
plot(plotdataejectives, pch = 21, col = "white", bg = "black", cex = 2.3, add = T)
dev.off()
```

## Uvulars

``` r
tiff(file="fig2_pacific_centered.tiff", width=2013, height = 1656)
map <- openmap(c(78, -180), c(-62, 180), type = "nps", minNumTiles = 100)
map <- openproj(map, projection = "+init=epsg:3832")
plot(map)
plotdatanouvulars <- drop_na(elevdata %>% filter(NonMarginal01 == F) %>% select(latitude, longitude))
plotdatanouvulars <- st_as_sf(plotdatanouvulars, coords = c("longitude", "latitude"), crs = 4326)
plotdatanouvulars <- st_transform(plotdatanouvulars, crs = 3832)
plot(plotdatanouvulars, pch = 21, col = "black", bg = "white", cex = 2.3, add = T)

plotdatauvulars <- drop_na(elevdata %>% filter(NonMarginal01 == T) %>% select(latitude, longitude))
plotdatauvulars <- st_as_sf(plotdatauvulars, coords = c("longitude", "latitude"), crs = 4326)
plotdatauvulars <- st_transform(plotdatauvulars, crs = 3832)
plot(plotdatauvulars, pch = 21, col = "white", bg = "black", cex = 2.3, add = T)
dev.off()
```

Legends were added manually post-hoc.

# Descriptive stats

Subset data

``` r
uvulars <- elevdata %>% filter(NonMarginal01 == T)
nouvulars <- elevdata %>% filter(NonMarginal01 == F)
ejectives <- elevdata %>% filter(NonMarginal02 == T)
noejectives <- elevdata %>% filter(NonMarginal02 == F)
```

Compute global means and median values

``` r
mean(uvulars$elevation, na.rm = 1)
```

    ## [1] 1136.301

``` r
median(uvulars$elevation, na.rm = 1)
```

    ## [1] 623

    ## [1] 1136.301

``` r
mean(nouvulars$elevation, na.rm = 1)
```

    ## [1] 590.1413

``` r
median(nouvulars$elevation, na.rm = 1)
```

    ## [1] 306

    ## [1] 590.1413

``` r
mean(ejectives$elevation, na.rm = 1)
```

    ## [1] 1236.913

``` r
median(ejectives$elevation, na.rm = 1)
```

    ## [1] 1136

    ## [1] 1236.913

``` r
mean(noejectives$elevation, na.rm = 1)
```

    ## [1] 606.0188

``` r
median(noejectives$elevation, na.rm = 1)
```

    ## [1] 304.5

    ## [1] 606.0188

Compute means and median values by area

``` r
aggregate(elevation ~ macroarea2, FUN = "mean", data = uvulars)
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
aggregate(elevation ~ macroarea2, FUN = "median", data = uvulars)
```

    ##               macroarea2 elevation
    ## 1                 Africa     513.0
    ## 2                 Europe     484.0
    ## 3         Middle America     434.0
    ## 4             New Guinea    2184.0
    ## 5          North America     457.0
    ## 6       Northern Eurasia    1305.0
    ## 7 South & Southeast Asia     438.5
    ## 8          South America    1036.0
    ## 9           Western Asia     736.5

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
aggregate(elevation ~ macroarea2, FUN = "mean", data = nouvulars)
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
aggregate(elevation ~ macroarea2, FUN = "median", data = nouvulars)
```

    ##                macroarea2 elevation
    ## 1                  Africa     500.0
    ## 2               Australia     134.0
    ## 3                  Europe     179.0
    ## 4          Middle America     849.0
    ## 5              New Guinea     305.0
    ## 6           North America     525.0
    ## 7        Northern Eurasia    1613.5
    ## 8                 Oceania     131.0
    ## 9  South & Southeast Asia     278.0
    ## 10          South America     204.0
    ## 11           Western Asia    1141.0

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
aggregate(elevation ~ macroarea2, FUN = "mean", data = ejectives)
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
aggregate(elevation ~ macroarea2, FUN = "median", data = ejectives)
```

    ##         macroarea2 elevation
    ## 1           Africa    1497.0
    ## 2           Europe    1445.0
    ## 3   Middle America    1375.0
    ## 4    North America     450.5
    ## 5 Northern Eurasia     267.5
    ## 6    South America     400.5
    ## 7     Western Asia    1033.0

    ##         macroarea2 elevation
    ## 1           Africa 1474.8333
    ## 2           Europe 1388.0000
    ## 3   Middle America 1219.3333
    ## 4    North America  749.6739
    ## 5 Northern Eurasia 1303.7500
    ## 6    South America 1459.0833
    ## 7     Western Asia 1182.0000

``` r
aggregate(elevation ~ macroarea2, FUN = "mean", data = noejectives)
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

``` r
aggregate(elevation ~ macroarea2, FUN = "median", data = noejectives)
```

    ##                macroarea2 elevation
    ## 1                  Africa       458
    ## 2               Australia       134
    ## 3                  Europe       173
    ## 4          Middle America       624
    ## 5              New Guinea       305
    ## 6           North America       553
    ## 7        Northern Eurasia      1607
    ## 8                 Oceania       131
    ## 9  South & Southeast Asia       286
    ## 10          South America       207
    ## 11           Western Asia       897

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
aggregate(NonMarginal01 ~ macroarea2, FUN = length, data = uvulars)
```

    ##               macroarea2 NonMarginal01
    ## 1                 Africa            54
    ## 2                 Europe            34
    ## 3         Middle America            13
    ## 4             New Guinea             1
    ## 5          North America            34
    ## 6       Northern Eurasia            55
    ## 7 South & Southeast Asia            14
    ## 8          South America            38
    ## 9           Western Asia            14

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
aggregate(NonMarginal01 ~ macroarea2, FUN = length, data = nouvulars)
```

    ##                macroarea2 NonMarginal01
    ## 1                  Africa           636
    ## 2               Australia           265
    ## 3                  Europe            75
    ## 4          Middle America            37
    ## 5              New Guinea            73
    ## 6           North America            55
    ## 7        Northern Eurasia            90
    ## 8                 Oceania            36
    ## 9  South & Southeast Asia           211
    ## 10          South America           293
    ## 11           Western Asia             5

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
aggregate(NonMarginal02 ~ macroarea2, FUN = length, data = ejectives)
```

    ##         macroarea2 NonMarginal02
    ## 1           Africa            60
    ## 2           Europe            20
    ## 3   Middle America            15
    ## 4    North America            46
    ## 5 Northern Eurasia             4
    ## 6    South America            24
    ## 7     Western Asia             4

    ##         macroarea2 NonMarginal02
    ## 1           Africa            61
    ## 2           Europe            21
    ## 3   Middle America            15
    ## 4    North America            46
    ## 5 Northern Eurasia             4
    ## 6    South America            24
    ## 7     Western Asia             4

``` r
aggregate(NonMarginal02 ~ macroarea2, FUN = length, data = noejectives)
```

    ##                macroarea2 NonMarginal02
    ## 1                  Africa           630
    ## 2               Australia           265
    ## 3                  Europe            89
    ## 4          Middle America            35
    ## 5              New Guinea            74
    ## 6           North America            43
    ## 7        Northern Eurasia           141
    ## 8                 Oceania            36
    ## 9  South & Southeast Asia           225
    ## 10          South America           307
    ## 11           Western Asia            15

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
nobservations <- function(x) {
  return(c(y = 6000, label = length(x)))
}
png(file="fig3b_updated.png", width=1342, height=1104)
elevdata %>%
  filter(Nonmarginal_Uvular < 15) %>%
  ggplot(aes(group = Nonmarginal_Uvular, x = Nonmarginal_Uvular, y = elevation)) +
  geom_boxplot(outlier.alpha = 0.1) +
  stat_summary(fun.data = nobservations, geom = "text", fun = median, size=10) +
  labs(x = "Number of uvular consonants", y = "Elevation") +
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=24,face="bold"))
dev.off()
```

``` r
nobservations <- function(x) {
  return(c(y = 6000, label = length(x)))
}
png(file="fig3a_updated.png", width=1342, height=1104)
elevdata %>%
  filter(Nonmarginal_Ejective < 15) %>%
  ggplot(aes(group = Nonmarginal_Ejective, x = Nonmarginal_Ejective, y = elevation)) +
  geom_boxplot(outlier.alpha = 0.1) +
  stat_summary(fun.data = nobservations, geom = "text", fun = median, size=10) +
  labs(x = "Number of ejective consonants", y = "Elevation")+
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=24,face="bold"))
dev.off()
```
