Bayesian modelling of ejectives and uvulars depending on altitude and
ancillary analyses
================
Matthias Urban
23 Dezember, 2020

# Overview

This document describes the Bayesian modeling of ejectives and uvulars
(presence or absence) depending on altitude and ancillary analyses

# Required packages

``` r
library(tidyverse)
library(brms)
library(boot)
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
elevdata <- mutate(elevdata, NonMarginal03 = as.logical(Nonmarginal_Uvular_no_rhotics), NonMarginal03 = as.numeric(NonMarginal03))
```

# Modeling

## Bayesian logistic mixed effects regressions

Tranform elevation to its log10 for modelling

``` r
elevdata <- mutate(elevdata, elevationlog10 = log10(elevation))
```

Set prior

``` r
priors <- set_prior("normal(0, 2)", class = "b", coef = "elevationlog10")
```

### Model for uvulars

``` r
elevmodeluvulars <- brm(NonMarginal01 ~ elevationlog10 + (1 + elevationlog10 | macroarea2) + (1 | family_id), family = "bernoulli", data = elevdata, seed = 31011, warmup = 6000, iter = 8000, chains = 4, prior = priors, control = list(adapt_delta = 0.999, max_treedepth = 20))
```

    ## Compiling Stan program...

    ## Start sampling

Model assessment and checks

check Rhat and ESS values

``` r
summary(elevmodeluvulars)
```

    ##  Family: bernoulli 
    ##   Links: mu = logit 
    ## Formula: NonMarginal01 ~ elevationlog10 + (1 + elevationlog10 | macroarea2) + (1 | family_id) 
    ##    Data: elevdata (Number of observations: 2031) 
    ## Samples: 4 chains, each with iter = 8000; warmup = 6000; thin = 1;
    ##          total post-warmup samples = 8000
    ## 
    ## Group-Level Effects: 
    ## ~family_id (Number of levels: 246) 
    ##               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sd(Intercept)     2.55      0.41     1.85     3.47 1.00     2383     3709
    ## 
    ## ~macroarea2 (Number of levels: 11) 
    ##                               Estimate Est.Error l-95% CI u-95% CI Rhat
    ## sd(Intercept)                     3.26      1.24     1.46     6.19 1.00
    ## sd(elevationlog10)                0.49      0.34     0.02     1.30 1.00
    ## cor(Intercept,elevationlog10)    -0.37      0.53    -0.98     0.85 1.00
    ##                               Bulk_ESS Tail_ESS
    ## sd(Intercept)                     2996     4497
    ## sd(elevationlog10)                1927     2632
    ## cor(Intercept,elevationlog10)     4575     5015
    ## 
    ## Population-Level Effects: 
    ##                Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## Intercept         -3.09      1.22    -5.77    -0.85 1.00     2449     3615
    ## elevationlog10     0.16      0.28    -0.32     0.80 1.00     4579     4162
    ## 
    ## Samples were drawn using sampling(NUTS). For each parameter, Bulk_ESS
    ## and Tail_ESS are effective sample size measures, and Rhat is the potential
    ## scale reduction factor on split chains (at convergence, Rhat = 1).

Inspect chains

``` r
plot(elevmodeluvulars)
```

![](Bayesian_modelling_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->![](Bayesian_modelling_files/figure-gfm/unnamed-chunk-11-2.png)<!-- -->

Inspect plots of observed data and posterior predictive samples

``` r
pp_check(elevmodeluvulars)
```

    ## Using 10 posterior samples for ppc type 'dens_overlay' by default.

![](Bayesian_modelling_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

``` r
pp_check(elevmodeluvulars, type = "error_binned")
```

    ## Using 10 posterior samples for ppc type 'error_binned' by default.

![](Bayesian_modelling_files/figure-gfm/unnamed-chunk-12-2.png)<!-- -->

Assess predictive accuracy

``` r
modelled_elevdata <- elevdata %>% drop_na(NonMarginal01, elevationlog10, macroarea2, family_id)
elevmodeluvulars_pred <- predict(elevmodeluvulars, type = "response")[, "Estimate"]
elevmodeluvulars_pred <- as.numeric(elevmodeluvulars_pred > mean(modelled_elevdata$NonMarginal01))
(classtab_elevmodeluvulars <- table(predicted = elevmodeluvulars_pred, observed = modelled_elevdata$NonMarginal01))
```

    ##          observed
    ## predicted    0    1
    ##         0 1330  192
    ##         1  445   64

``` r
(acc_elevmodeluvulars <- sum(diag(classtab_elevmodeluvulars)) / sum(classtab_elevmodeluvulars))
```

    ## [1] 0.6863614

Assess posterior probability versus chance

``` r
elevmodeluvularssamples <- posterior_samples(elevmodeluvulars)
sum(elevmodeluvularssamples$b_elevationlog10 < 0) / nrow(elevmodeluvularssamples)
```

    ## [1] 0.2785

### Model for uvulars without rhotics

``` r
elevmodeluvularswithoutrhotics <- brm(NonMarginal03 ~ elevationlog10 + (1 + elevationlog10 | macroarea2) + (1 | family_id), family = "bernoulli", data = elevdata, seed = 31011, warmup = 6000, iter = 8000, chains = 4, prior = priors, control = list(adapt_delta = 0.999, max_treedepth = 20))
```

    ## Compiling Stan program...

    ## recompiling to avoid crashing R session

    ## Start sampling

Model assessment and checks

check Rhat and ESS values

``` r
summary(elevmodeluvularswithoutrhotics)
```

    ##  Family: bernoulli 
    ##   Links: mu = logit 
    ## Formula: NonMarginal03 ~ elevationlog10 + (1 + elevationlog10 | macroarea2) + (1 | family_id) 
    ##    Data: elevdata (Number of observations: 2031) 
    ## Samples: 4 chains, each with iter = 8000; warmup = 6000; thin = 1;
    ##          total post-warmup samples = 8000
    ## 
    ## Group-Level Effects: 
    ## ~family_id (Number of levels: 246) 
    ##               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sd(Intercept)     2.98      0.48     2.17     4.04 1.00     2446     3913
    ## 
    ## ~macroarea2 (Number of levels: 11) 
    ##                               Estimate Est.Error l-95% CI u-95% CI Rhat
    ## sd(Intercept)                     3.42      1.30     1.46     6.54 1.00
    ## sd(elevationlog10)                0.57      0.36     0.05     1.43 1.00
    ## cor(Intercept,elevationlog10)    -0.32      0.52    -0.97     0.82 1.00
    ##                               Bulk_ESS Tail_ESS
    ## sd(Intercept)                     3230     4513
    ## sd(elevationlog10)                2174     3257
    ## cor(Intercept,elevationlog10)     4441     4497
    ## 
    ## Population-Level Effects: 
    ##                Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## Intercept         -3.72      1.27    -6.37    -1.28 1.00     2609     4050
    ## elevationlog10     0.35      0.30    -0.21     1.02 1.00     4675     4133
    ## 
    ## Samples were drawn using sampling(NUTS). For each parameter, Bulk_ESS
    ## and Tail_ESS are effective sample size measures, and Rhat is the potential
    ## scale reduction factor on split chains (at convergence, Rhat = 1).

Inspect chains

``` r
plot(elevmodeluvularswithoutrhotics)
```

![](Bayesian_modelling_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->![](Bayesian_modelling_files/figure-gfm/unnamed-chunk-17-2.png)<!-- -->

Inspect plots of observed data and posterior predictive samples

``` r
pp_check(elevmodeluvularswithoutrhotics)
```

    ## Using 10 posterior samples for ppc type 'dens_overlay' by default.

![](Bayesian_modelling_files/figure-gfm/unnamed-chunk-18-1.png)<!-- -->

``` r
pp_check(elevmodeluvularswithoutrhotics, type = "error_binned")
```

    ## Using 10 posterior samples for ppc type 'error_binned' by default.

![](Bayesian_modelling_files/figure-gfm/unnamed-chunk-18-2.png)<!-- -->

Assess predictive accuracy

``` r
modelled_elevdata <- elevdata %>% drop_na(NonMarginal03, elevationlog10, macroarea2, family_id)
elevmodeluvularswithoutrhotics_pred <- predict(elevmodeluvularswithoutrhotics, type = "response")[, "Estimate"]
elevmodeluvularswithoutrhotics_pred <- as.numeric(elevmodeluvularswithoutrhotics_pred > mean(modelled_elevdata$NonMarginal03))
(classtab_elevmodeluvularswithoutrhotics <- table(predicted = elevmodeluvularswithoutrhotics_pred, observed = modelled_elevdata$NonMarginal03))
```

    ##          observed
    ## predicted    0    1
    ##         0 1385  180
    ##         1  408   58

``` r
(acc_elevmodeluvularswithoutrhotics <- sum(diag(classtab_elevmodeluvularswithoutrhotics)) / sum(classtab_elevmodeluvularswithoutrhotics))
```

    ## [1] 0.7104874

Assess posterior probability versus chance

``` r
elevmodeluvularswithoutrhoticssamples <- posterior_samples(elevmodeluvularswithoutrhotics)
sum(elevmodeluvularswithoutrhoticssamples$b_elevationlog10 < 0) / nrow(elevmodeluvularswithoutrhoticssamples)
```

    ## [1] 0.099625

### Model for ejectives

``` r
elevmodelejectives <- brm(NonMarginal02 ~ elevationlog10 + (1 + elevationlog10 | macroarea2) + (1 | family_id), family = "bernoulli", data = elevdata, seed = 31011, warmup = 6000, iter = 8000, chains = 4, prior = priors, control = list(adapt_delta = 0.999, max_treedepth = 20))
```

    ## Compiling Stan program...

    ## recompiling to avoid crashing R session

    ## Start sampling

Model assessment and checks

Check Rhat and ESS values

``` r
summary(elevmodelejectives)
```

    ##  Family: bernoulli 
    ##   Links: mu = logit 
    ## Formula: NonMarginal02 ~ elevationlog10 + (1 + elevationlog10 | macroarea2) + (1 | family_id) 
    ##    Data: elevdata (Number of observations: 2031) 
    ## Samples: 4 chains, each with iter = 8000; warmup = 6000; thin = 1;
    ##          total post-warmup samples = 8000
    ## 
    ## Group-Level Effects: 
    ## ~family_id (Number of levels: 246) 
    ##               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sd(Intercept)     4.09      0.68     2.96     5.56 1.00     2951     4070
    ## 
    ## ~macroarea2 (Number of levels: 11) 
    ##                               Estimate Est.Error l-95% CI u-95% CI Rhat
    ## sd(Intercept)                     5.14      1.96     2.36     9.87 1.00
    ## sd(elevationlog10)                1.55      0.70     0.52     3.30 1.00
    ## cor(Intercept,elevationlog10)    -0.36      0.39    -0.92     0.52 1.00
    ##                               Bulk_ESS Tail_ESS
    ## sd(Intercept)                     3836     4466
    ## sd(elevationlog10)                2707     2847
    ## cor(Intercept,elevationlog10)     4305     5373
    ## 
    ## Population-Level Effects: 
    ##                Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## Intercept         -8.88      2.22   -13.57    -4.73 1.00     3998     4972
    ## elevationlog10     1.57      0.71     0.13     2.94 1.00     3851     4572
    ## 
    ## Samples were drawn using sampling(NUTS). For each parameter, Bulk_ESS
    ## and Tail_ESS are effective sample size measures, and Rhat is the potential
    ## scale reduction factor on split chains (at convergence, Rhat = 1).

Inspect chains

``` r
plot(elevmodelejectives)
```

![](Bayesian_modelling_files/figure-gfm/unnamed-chunk-23-1.png)<!-- -->![](Bayesian_modelling_files/figure-gfm/unnamed-chunk-23-2.png)<!-- -->

Inspect plots of observed data and posterior predictive samples

``` r
pp_check(elevmodelejectives)
```

    ## Using 10 posterior samples for ppc type 'dens_overlay' by default.

![](Bayesian_modelling_files/figure-gfm/unnamed-chunk-24-1.png)<!-- -->

``` r
pp_check(elevmodelejectives, type = "error_binned")
```

    ## Using 10 posterior samples for ppc type 'error_binned' by default.

![](Bayesian_modelling_files/figure-gfm/unnamed-chunk-24-2.png)<!-- -->

Assess predictive accuracy

``` r
modelled_elevdata <- elevdata %>% drop_na(NonMarginal02, elevationlog10, macroarea2, family_id)
elevmodelejectives_pred <- predict(elevmodelejectives, type = "response")[, "Estimate"]
elevmodelejectives_pred <- as.numeric(elevmodelejectives_pred > mean(modelled_elevdata$NonMarginal02))
(classtab_elevmodelejectives <- table(predicted = elevmodelejectives_pred, observed = modelled_elevdata$NonMarginal02))
```

    ##          observed
    ## predicted    0    1
    ##         0 1561  148
    ##         1  297   25

``` r
(acc_elevmodelejectives <- sum(diag(classtab_elevmodelejectives)) / sum(classtab_elevmodelejectives))
```

    ## [1] 0.7808961

Assess posterior probability versus chance

``` r
elevmodelejectivessamples <- posterior_samples(elevmodelejectives)
sum(elevmodelejectivessamples$b_elevationlog10 < 0) / nrow(elevmodelejectivessamples)
```

    ## [1] 0.018

## By-area and by-family analysis

### By area

Compute median elevations and proportions of uvulars and ejectives by
area

``` r
medianelevarea <- aggregate(elevation ~ macroarea2, FUN = "median", data = elevdata)
uvularproportionarea <- aggregate(NonMarginal01 ~ macroarea2, FUN = "mean", data = elevdata)
uvularwithoutrhoticsproportionarea <- aggregate(NonMarginal03 ~ macroarea2, FUN = "mean", data = elevdata)
ejectiveproportionarea <- aggregate(NonMarginal02 ~ macroarea2, FUN = "mean", data = elevdata)
```

Least squares regression

``` r
summary(lm(uvularproportionarea$NonMarginal01 ~ medianelevarea$elevation))
```

    ## 
    ## Call:
    ## lm(formula = uvularproportionarea$NonMarginal01 ~ medianelevarea$elevation)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.18031 -0.11162 -0.07838  0.08687  0.38722 
    ## 
    ## Coefficients:
    ##                           Estimate Std. Error t value Pr(>|t|)  
    ## (Intercept)              0.0470556  0.0880766   0.534    0.606  
    ## medianelevarea$elevation 0.0003271  0.0001337   2.446    0.037 *
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.1869 on 9 degrees of freedom
    ## Multiple R-squared:  0.3994, Adjusted R-squared:  0.3327 
    ## F-statistic: 5.985 on 1 and 9 DF,  p-value: 0.03697

``` r
summary(lm(uvularwithoutrhoticsproportionarea$NonMarginal03 ~ medianelevarea$elevation))
```

    ## 
    ## Call:
    ## lm(formula = uvularwithoutrhoticsproportionarea$NonMarginal03 ~ 
    ##     medianelevarea$elevation)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.18041 -0.10009 -0.07586  0.07661  0.39344 
    ## 
    ## Coefficients:
    ##                           Estimate Std. Error t value Pr(>|t|)  
    ## (Intercept)              0.0317165  0.0857997   0.370   0.7202  
    ## medianelevarea$elevation 0.0003370  0.0001302   2.587   0.0294 *
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.1821 on 9 degrees of freedom
    ## Multiple R-squared:  0.4265, Adjusted R-squared:  0.3628 
    ## F-statistic: 6.693 on 1 and 9 DF,  p-value: 0.02936

``` r
summary(lm(ejectiveproportionarea$NonMarginal02 ~ medianelevarea$elevation))
```

    ## 
    ## Call:
    ## lm(formula = ejectiveproportionarea$NonMarginal02 ~ medianelevarea$elevation)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.16742 -0.10812 -0.03973  0.06648  0.39087 
    ## 
    ## Coefficients:
    ##                           Estimate Std. Error t value Pr(>|t|)
    ## (Intercept)              9.468e-02  8.055e-02   1.175    0.270
    ## medianelevarea$elevation 6.402e-05  1.223e-04   0.524    0.613
    ## 
    ## Residual standard error: 0.171 on 9 degrees of freedom
    ## Multiple R-squared:  0.02956,    Adjusted R-squared:  -0.07826 
    ## F-statistic: 0.2742 on 1 and 9 DF,  p-value: 0.6132

Plot results

``` r
tiff(file="fig4a_updated.tiff", width=972, height=800)
plot(medianelevarea$elevation, ejectiveproportionarea$NonMarginal02, bg = "black", pch = 21, xlab = "Median elevation per area", ylab = "", cex.lab=2.5)
title(ylab="Proportion Ejectives", line=2.2, cex.lab=2.5)
lines(lowess(medianelevarea$elevation, ejectiveproportionarea$NonMarginal02, f = 10, iter = 10))
dev.off()
tiff(file="fig4b_updated.tiff", width=972, height=800)
plot(medianelevarea$elevation, uvularproportionarea$NonMarginal01, bg = "black", pch = 21, xlab = "Median elevation per area", ylab = "", cex.lab=2.5)
title(ylab="Proportion Uvulars", line=2.2, cex.lab=2.5)
lines(lowess(medianelevarea$elevation, uvularproportionarea$NonMarginal01, f = 10, iter = 10))
dev.off()
tiff(file="fig4b_updated.tiff", width=972, height=800)
plot(medianelevarea$elevation, uvularwithoutrhoticsproportionarea$NonMarginal03, bg = "black", pch = 21, xlab = "Median elevation per area", ylab = "", cex.lab=2.5)
title(ylab="Proportion Uvulars without Rhotics", line=2.2, cex.lab=2.5)
lines(lowess(medianelevarea$elevation, uvularwithoutrhoticsproportionarea$NonMarginal03, f = 10, iter = 10))
dev.off()
```

### By family

``` r
largefamilies <- filter(elevdata, family_id %in% c("afro1255", "araw1281", "atha1245", "atla1278", "aust1307", "cari1283", "gong1255", "mand1469", "maya1287", "mong1329", "nakh1245", "otom1299", "sali1255", "sino1245", "taik1256", "tupi1275", "turk1311", "ural1272"))
```

Compute median elevations and proportions of uvulars and ejectives per
by family

``` r
medianelevfamily <- aggregate(elevation ~ family_id, FUN = "median", data = largefamilies)
uvularproportionfamily <- aggregate(NonMarginal01 ~ family_id, FUN = "mean", data = largefamilies)
uvularwithoutrhoticsproportionfamily <- aggregate(NonMarginal03 ~ family_id, FUN = "mean", data = largefamilies)
ejectiveproportionfamily <- aggregate(NonMarginal02 ~ family_id, FUN = "mean", data = largefamilies)
```

Least squares regression

``` r
summary(lm(uvularproportionfamily$NonMarginal01 ~ medianelevfamily$elevation))
```

    ## 
    ## Call:
    ## lm(formula = uvularproportionfamily$NonMarginal01 ~ medianelevfamily$elevation)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -0.4138 -0.2002 -0.1224  0.1514  0.7974 
    ## 
    ## Coefficients:
    ##                             Estimate Std. Error t value Pr(>|t|)
    ## (Intercept)                0.1891044  0.1202549   1.573    0.135
    ## medianelevfamily$elevation 0.0001286  0.0001175   1.095    0.290
    ## 
    ## Residual standard error: 0.333 on 16 degrees of freedom
    ## Multiple R-squared:  0.06968,    Adjusted R-squared:  0.01154 
    ## F-statistic: 1.198 on 1 and 16 DF,  p-value: 0.2898

``` r
summary(lm(uvularwithoutrhoticsproportionfamily$NonMarginal03 ~ medianelevfamily$elevation))
```

    ## 
    ## Call:
    ## lm(formula = uvularwithoutrhoticsproportionfamily$NonMarginal03 ~ 
    ##     medianelevfamily$elevation)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -0.4061 -0.2021 -0.1108  0.1618  0.8092 
    ## 
    ## Coefficients:
    ##                             Estimate Std. Error t value Pr(>|t|)
    ## (Intercept)                0.1770100  0.1229528   1.440    0.169
    ## medianelevfamily$elevation 0.0001312  0.0001201   1.092    0.291
    ## 
    ## Residual standard error: 0.3405 on 16 degrees of freedom
    ## Multiple R-squared:  0.06933,    Adjusted R-squared:  0.01116 
    ## F-statistic: 1.192 on 1 and 16 DF,  p-value: 0.2911

``` r
summary(lm(ejectiveproportionfamily$NonMarginal02 ~ medianelevfamily$elevation))
```

    ## 
    ## Call:
    ## lm(formula = ejectiveproportionfamily$NonMarginal02 ~ medianelevfamily$elevation)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -0.5448 -0.2070 -0.1604  0.3266  0.7874 
    ## 
    ## Coefficients:
    ##                             Estimate Std. Error t value Pr(>|t|)
    ## (Intercept)                0.1229161  0.1418376   0.867    0.399
    ## medianelevfamily$elevation 0.0002022  0.0001386   1.459    0.164
    ## 
    ## Residual standard error: 0.3928 on 16 degrees of freedom
    ## Multiple R-squared:  0.1175, Adjusted R-squared:  0.0623 
    ## F-statistic:  2.13 on 1 and 16 DF,  p-value: 0.1638

Plot results

``` r
tiff(file="fig5a_updated.tiff", width=972, height=800)
plot(medianelevfamily$elevation, ejectiveproportionfamily$NonMarginal02, bg = "black", pch = 21, xlab = "Median elevation per family", ylab = "", cex.lab=2.5)
title(ylab="Proportion Ejectives", line=2.2, cex.lab=2.5)
lines(lowess(medianelevfamily$elevation, ejectiveproportionfamily$NonMarginal02, f = 10, iter = 10))
dev.off()
tiff(file="fig5b_updated.tiff", width=972, height=800)
plot(medianelevfamily$elevation, uvularproportionfamily$NonMarginal01, bg = "black", pch = 21, xlab = "Median elevation per family", ylab = "", cex.lab=2.5)
lines(lowess(medianelevfamily$elevation, uvularproportionfamily$NonMarginal01, f = 10, iter = 10))
title(ylab="Proportion Uvulars", line=2.2, cex.lab=2.5)
dev.off()
tiff(file="fig5c_updated.tiff", width=972, height=800)
plot(medianelevfamily$elevation, uvularwithoutrhoticsproportionfamily$NonMarginal03, bg = "black", pch = 21, xlab = "Median elevation per family", ylab = "", cex.lab=2.5)
title(ylab="Proportion Uvulars without Rhotics", line=2.2, cex.lab=2.5)
lines(lowess(medianelevfamily$elevation, uvularwithoutrhoticsproportionfamily$NonMarginal03, f = 10, iter = 10))
dev.off()
```
