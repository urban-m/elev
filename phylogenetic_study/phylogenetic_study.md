Phylogenetic study of ejectives and uvulars (presense / absence) in IE
and ST
================
Steven Moran
21 September, 2020

Overview
========

For the traits, we use presence vs. absence of ejectives and uvulars.
Hence, we can use the “Nonmarginal Ejective” and “Nonmarginal Uvular”
columns in the datasheet. These are the number of ejectives/uvulars
minus number of marginal ejectives/uvulars. For the logistic regression,
these counts were used as the basis for the binary distinction. So if
there is one uvular but it is coded as marginal then it is going to be
noted as FALSE.

For the phylognetic traits, we use presence / absence of class, e.g. a
language does or does not have ejectives or uvulars. This is basically a
discrete variable model for phylogenetic reconstruction. Then we’ll have
the probability of the trait (variable) at the root.

The Indo-European and Sino-Tibetan phylogenies come from:

-   Chang W, Cathcart C, Hall D, & Garrett A. 2015. Ancestry-constrained
    phylogenetic analysis supports the Indo-European steppe hypothesis.
    Language, 91(1):194-244.

-   Zhang M, Yan S, Pan W, & Jin L. 2019. Phylogenetic evidence for
    Sino-Tibetan origin in northern China in the Late Neolithic. Nature,
    569, 112-115.

Data prep
=========

Data inspection and cleaning.

    df <- read.csv('../Data/uvulars_ejectives_pruned2.csv', stringsAsFactors = FALSE)

Which sources are <NA> for Glottocodes? This issue is discussed in
detail here:

-   <a href="https://github.com/phoible/dev/issues/270" class="uri">https://github.com/phoible/dev/issues/270</a>

Drop NAs.

    df %>% filter(is.na(Glottocode))

    ##   InventoryID Glottocode ISO6393 GlottologName    PhoibleName
    ## 1        2281       <NA>    <NA>          <NA> Modern Aramaic
    ## 2        2729       <NA>    <NA>          <NA>      Djindewal
    ##                 SpecificDialect Source family_id level status latitude
    ## 1 Modern Aramaic (Northeastern)     ea      <NA>  <NA>   <NA>     <NA>
    ## 2                          <NA>     er      <NA>  <NA>   <NA>     <NA>
    ##   longitude country_ids macroarea   macroarea2 elevation Uvulars
    ## 1      <NA>        <NA>   Eurasia Western Asia        NA       1
    ## 2      <NA>        <NA> Australia    Australia        NA       0
    ##   Marginal_Uvular Nonmarginal_Uvular Ejectives Marginal_Ejective
    ## 1               0                  1         0                 0
    ## 2               0                  0         0                 0
    ##   Nonmarginal_Ejective
    ## 1                    0
    ## 2                    0

    df <- df %>% filter(!is.na(Glottocode))
    expect_equal(nrow(df %>% filter(is.na(Glottocode))), 0)

Extract relevant data for this analysis and then investigate ejectives
and uvulars.

    ejectives <- df %>% select(InventoryID, Source, Glottocode, Nonmarginal_Ejective) %>% rename(has_ejectives = Nonmarginal_Ejective)
    uvulars <- df %>% select(InventoryID, Source, Glottocode, Nonmarginal_Uvular) %>% rename(has_uvulars = Nonmarginal_Uvular)

Ejectives
---------

Check for duplicate Glottocodes (we can have only one data point per
daughter node on the phylogeny).

    e.dups <- ejectives %>% group_by(Source, Glottocode) %>% filter(n()>1)
    e.dups %>% arrange(Glottocode) %>% kable()

| InventoryID | Source | Glottocode | has\_ejectives |
|------------:|:-------|:-----------|---------------:|

Drop some of these data points.

    ejectives <- ejectives %>% filter(!(InventoryID %in% c(2883, 2037, 2031, 2207, 2345,2348, 2618, 2988)))
    expect_equal(nrow(ejectives %>% group_by(Source, Glottocode) %>% filter(n()>1)), 0)

Uvulars
-------

Check for duplicate Glottocodes (we can have only one data point per
daughter node on the phylogeny).

    u.dups <- uvulars %>% group_by(Source, Glottocode) %>% filter(n()>1)
    u.dups %>% arrange(Glottocode) %>% kable()

| InventoryID | Source | Glottocode | has\_uvulars |
|------------:|:-------|:-----------|-------------:|

There’s disagreement about the presence of absence of uvulars between
inventories for:

    u.dups %>% filter(Glottocode %in% c('ajab1235', 'basq1248', 'mose1249', 'nort2980', 'port1283', 'stan1288', 'west2368')) %>% arrange(Glottocode) %>% kable() 

| InventoryID | Source | Glottocode | has\_uvulars |
|------------:|:-------|:-----------|-------------:|

One inventory for ajab1235 lists uvulars the second does not. This may
because these are different dialects with the same Glottocode. It is
clear from the first inventory, and a subsequent publication by Eric
Morey (“A Grammar of Ajagbe”) that it contains two uvulars. We take thus
the first inventory.

-   <a href="https://phoible.org/inventories/view/651" class="uri">https://phoible.org/inventories/view/651</a>
-   <a href="https://phoible.org/inventories/view/652" class="uri">https://phoible.org/inventories/view/652</a>

SPA inventory for basq1248 lists a uvular, the other does not. SPA lists
it as a loan, so we take the second inventory.

-   <a href="https://phoible.org/inventories/view/179" class="uri">https://phoible.org/inventories/view/179</a>
-   <a href="https://phoible.org/inventories/view/2161" class="uri">https://phoible.org/inventories/view/2161</a>

The inventories for Moseten are:

-   <a href="https://phoible.org/inventories/view/944" class="uri">https://phoible.org/inventories/view/944</a>
-   <a href="https://phoible.org/inventories/view/1986" class="uri">https://phoible.org/inventories/view/1986</a>

The first contains uvular and Sakel (2004:30) states:

    /h/:[X] and [h]

    The velar fricative [X] and the uvular fricative [h] form one phoneme /h/. [h] exclusively occurs at the beginning of a syllable.

The IPA chart (page 25) lists glottal fricative /h/ with a footnote (36:
Allophone: \[x\]). I think should be updated to be in line with the
SAPHON interpretation.

The inventories for Quechua include:

-   <a href="https://phoible.org/inventories/view/2030" class="uri">https://phoible.org/inventories/view/2030</a>
-   <a href="https://phoible.org/inventories/view/2037" class="uri">https://phoible.org/inventories/view/2037</a>
-   <a href="https://phoible.org/inventories/view/2031" class="uri">https://phoible.org/inventories/view/2031</a>

The first two indicate one uvular.

Portuguese is listed in PHOIBLE twice:

-   <a href="https://phoible.org/inventories/view/2206" class="uri">https://phoible.org/inventories/view/2206</a>
-   <a href="https://phoible.org/inventories/view/2207" class="uri">https://phoible.org/inventories/view/2207</a>

The European dialect is noted as having a uvular. As per Wikipedia:

-   <a href="https://en.wikipedia.org/wiki/Portuguese_phonology" class="uri">https://en.wikipedia.org/wiki/Portuguese_phonology</a>

<!-- -->

    The consonant hereafter denoted as /ʁ/ has a variety of realizations depending on dialect. In Europe, it is typically a uvular trill [ʀ]; however, a pronunciation as a voiced uvular fricative [ʁ] may be becoming dominant in urban areas. There is also a realization as a voiceless uvular fricative [χ], and the original pronunciation as an alveolar trill [r] also remains very common in various dialects.[15] A common realization of the word-initial /r/ in the Lisbon accent is a voiced uvular fricative trill [ʀ̝].[16] In Brazil, /ʁ/ can be velar, uvular, or glottal and may be voiceless unless between voiced sounds;[17] it is usually pronounced as a voiceless velar fricative [x], a voiceless glottal fricative [h] or voiceless uvular fricative [χ]. See also Guttural R in Portuguese.

Spanish also has two inventories:

-   <a href="https://phoible.org/inventories/view/2308" class="uri">https://phoible.org/inventories/view/2308</a>
-   <a href="https://phoible.org/inventories/view/2210" class="uri">https://phoible.org/inventories/view/2210</a>

One with uvular (the Castilian variety).

Lastly, different dialects of Western Balochi:

-   <a href="https://phoible.org/inventories/view/2622" class="uri">https://phoible.org/inventories/view/2622</a>
-   <a href="https://phoible.org/inventories/view/2345" class="uri">https://phoible.org/inventories/view/2345</a>
-   <a href="https://phoible.org/inventories/view/2348" class="uri">https://phoible.org/inventories/view/2348</a>
-   <a href="https://phoible.org/inventories/view/2618" class="uri">https://phoible.org/inventories/view/2618</a>

the first reports a uvular, the others do not.

For the time being, we drop the same inventories as we did with the
ejectives.

    uvulars <- uvulars %>% filter(!(InventoryID %in% c(2883, 2037, 2031, 2207, 2345, 2348, 2618, 2988)))
    expect_equal(nrow(uvulars %>% group_by(Source, Glottocode) %>% filter(n()>1)), 0)

Note that we still have duplicate Glottocodes across sources.

    ejectives %>% group_by(Glottocode) %>% filter(n()>1) %>% kable()

| InventoryID | Source | Glottocode | has\_ejectives |
|------------:|:-------|:-----------|---------------:|

    uvulars %>% group_by(Glottocode) %>% filter(n()>1) %>% kable()

| InventoryID | Source | Glottocode | has\_uvulars |
|------------:|:-------|:-----------|-------------:|

We remove the UPSID inventory for the time being given the trump
hierarchy.

    ejectives <- ejectives %>% filter(!(InventoryID %in% c(648)))
    uvulars <- uvulars %>% filter(!(InventoryID %in% c(648)))

Since we’ve removed the same data points in the ejectives and uvulars
data, make sure that they match.

    expect_true(all(ejectives$InventoryID %in% uvulars$InventoryID))
    expect_true(all(uvulars$InventoryID %in% ejectives$InventoryID))

Some clean up.

    rm(e.dups, u.dups)

Formulate the traits files
--------------------------

First select just the columns for Glottocodes and presence/absence of
the variable. Then reformulate as “Y” (present) vs “N” (absent). Lastly,
turn the dataframe into something that works in a discrete variable
phylogenetic analysis.

    ejectives.traits <- ejectives %>% select(Glottocode, has_ejectives)
    table(ejectives.traits$has_ejectives)

    ## 
    ##    0    1    2    3    4    5    6    7    8    9   10   12   13   14   15   18 
    ## 1947    4    6   14   29   44   24   10   13   13    6    3    2    3    1    1 
    ##   26   27 
    ##    1    1

    ejectives.traits <- mutate_at(ejectives.traits, vars(-Glottocode), function(x) ifelse(x==0, "N", "Y"))
    rownames(ejectives.traits) <- ejectives.traits[,1]
    colnames(ejectives.traits)[1] <- "taxa"
    head(ejectives.traits)

    ##              taxa has_ejectives
    ## abkh1244 abkh1244             Y
    ## adyg1241 adyg1241             Y
    ## kaba1278 kaba1278             Y
    ## afad1236 afad1236             Y
    ## afar1241 afar1241             N
    ## alab1254 alab1254             Y

    table(ejectives.traits$has_ejectives)

    ## 
    ##    N    Y 
    ## 1947  175

    uvulars.traits <- uvulars %>% select(Glottocode, has_uvulars)
    table(uvulars.traits$has_uvulars)

    ## 
    ##    0    1    2    3    4    5    6    7    8    9   10   11   12   20   22   23 
    ## 1856   88   63   52   30    2   16    1    5    2    1    2    1    1    1    1

    uvulars.traits <- mutate_at(uvulars.traits, vars(-Glottocode), function(x) ifelse(x==0, "N", "Y"))
    rownames(uvulars.traits) <- uvulars.traits[,1]
    colnames(uvulars.traits)[1] <- "taxa"
    head(uvulars.traits)

    ##              taxa has_uvulars
    ## abkh1244 abkh1244           Y
    ## adyg1241 adyg1241           Y
    ## kaba1278 kaba1278           Y
    ## afad1236 afad1236           N
    ## afar1241 afar1241           N
    ## alab1254 alab1254           N

    table(uvulars.traits$has_uvulars)

    ## 
    ##    N    Y 
    ## 1856  266

Combine the traits files so that the languages can be analyzed the
distribution of ejectives and uvulars.

    traits <- left_join(ejectives.traits, uvulars.traits)

    ## Joining, by = "taxa"

    rownames(traits) <- traits[,1]

Here is the cross-tabular distribution of ejectives vs uvulars.

    table(traits$has_ejectives, traits$has_uvulars) %>% kable()

|     |    N |   Y |
|:----|-----:|----:|
| N   | 1761 | 186 |
| Y   |   95 |  80 |

Save the traits dataframes.

    save(traits, ejectives.traits, uvulars.traits, file="trees/traits.Rdata")
