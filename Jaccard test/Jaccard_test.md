Testing the association between presence and absence of ejectives and
uvulars within families with the Jaccard Similarity Coefficient analyses
================
Matthias Urban
21 November, 2020

# Overview

This document describes how to obtain the number of families with
ejective or uvular consonants and how to to test the association between
presence and absence of ejectives and uvulars within families with the
Jaccard Similarity Coefficient

# Required packages

``` r
library(tidyverse)
library(jaccard)
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

Remove rows with empty cells for Latitude and Longitude

``` r
elevdata <- drop_na(elevdata, elevation)
```

Reduce the data to a binary distinction between presence vs. absence of
ejectives/uvulars

``` r
elevdata <- mutate(elevdata, NonMarginal01 = as.logical(Nonmarginal_Uvular), NonMarginal01 = as.numeric(NonMarginal01))
elevdata <- mutate(elevdata, NonMarginal02 = as.logical(Nonmarginal_Ejective), NonMarginal02 = as.numeric(NonMarginal02))
```

# Obtain the number of language families in the dataset that feature uvular consonants

``` r
familycountuvulars <- aggregate(NonMarginal01 ~ family_id, FUN = "mean", data = elevdata)
familycountuvulars$NonMarginal01 <- as.logical(familycountuvulars$NonMarginal01)
sum(familycountuvulars$NonMarginal01 == T)
```

    ## [1] 68

# Obtain the number of language families in the dataset that feature ejective consonants

``` r
familycountejectives <- aggregate(NonMarginal02 ~ family_id, FUN = "mean", data = elevdata)
familycountejectives$NonMarginal02 <- as.logical(familycountejectives$NonMarginal02)
sum(familycountejectives$NonMarginal02 == T)
```

    ## [1] 65

# Test for the association between uvular and ejective consonants within families

``` r
jaccard.test(familycountuvulars$NonMarginal01, familycountejectives$NonMarginal02, px = mean(familycountuvulars$NonMarginal01), py = mean(familycountejectives$NonMarginal02), method = "exact")
```

    ## $statistics
    ## [1] 0.2586988
    ## 
    ## $pvalue
    ## [1] 5.166421e-11
    ## 
    ## $expectation
    ## [1] 0.1561948
