Remove uvular rhotics from counts
================
Steven Moran
(12 November, 2020)

    library(tidyverse)
    library(knitr)
    library(testthat)

# Overview

An anonymous reviewer notes that our decision to include uvular rhotics
requires explicit justification. They would like to see if there is any
impact on removing rhotics from the uvular counts on our results.

We show in our [extraction code](get_uvulars_ejectives.md) how many
rhotics are present in the data we extracted.

    uvulars <- read_csv("uvulars.csv")

Here is that data repeated. There are 22 occurrences in 1255
observations.

    temp <- uvulars %>%
      filter(grepl("ʀ", Phoneme)) %>%
      select(InventoryID, Glottocode, LanguageName, SpecificDialect, Phoneme, Allophones, Marginal)

    temp %>% kable()

| InventoryID | Glottocode | LanguageName     | SpecificDialect                                                                                              | Phoneme | Allophones | Marginal |
|------------:|:-----------|:-----------------|:-------------------------------------------------------------------------------------------------------------|:--------|:-----------|:---------|
|          33 | bata1289   | Batak            | NA                                                                                                           | ʀ       | ʀ          | NA       |
|         161 | stan1295   | German           | NA                                                                                                           | ʀ       | ʌ ʌ̯ ʀ r    | NA       |
|         162 | stan1290   | French           | NA                                                                                                           | ʀ       | ʀ ʁ̞ ʀ̥      | NA       |
|         241 | bata1289   | BATAK            | NA                                                                                                           | ʀ       | NA         | TRUE     |
|         304 | stan1295   | GERMAN           | NA                                                                                                           | ʀ       | NA         | FALSE    |
|         331 | stan1290   | FRENCH           | NA                                                                                                           | ʀ       | NA         | FALSE    |
|         459 | mogh1245   | MOGHOL           | NA                                                                                                           | ʀ       | NA         | FALSE    |
|        1049 | east2295   | Standard Yiddish | NA                                                                                                           | ʀ       | ʀ          | FALSE    |
|        1050 | dutc1256   | Dutch            | Belgian Standard                                                                                             | ʀ       | ʀ r        | FALSE    |
|        1610 | siwi1239   | Siwi             | NA                                                                                                           | ʀ       | ʀ          | FALSE    |
|        1837 | xiri1243   | Xiriâna          | NA                                                                                                           | ʀ       | NA         | NA       |
|        1874 | japr1238   | Japreria         | NA                                                                                                           | ʀ       | NA         | NA       |
|        2157 | stan1318   | Arabic           | Safad, Beirut, Damascus, Kuwait                                                                              | xʀ̥      | xʀ̥         | FALSE    |
|        2171 | dutc1256   | Dutch            | the dialect of Maastricht (south-eastern dialect group; Central Limburgian)                                  | ʀʁ      | ʀʁ ʀ̥ʁ̥      | FALSE    |
|        2172 | dutc1256   | Dutch            | the Dutch dialect of Weert (West-Limburgian), rural variety (not Stadsweerts)                                | ʀʁ      | ʀʁ ʀ̥ʁ̥      | FALSE    |
|        2173 | dutc1256   | Dutch            | the Belgian Limburg dialect of Hamont (belongs to the West Limburg dialects, subclassification: Dommellands) | ʀ       | ʀ χ        | FALSE    |
|        2200 | luxe1241   | Luxembourgish    | Standard central Luxembourgish                                                                               | ʀ       | ʀ ʁ ə ɐ χ  | FALSE    |
|        2272 | swis1247   | Zurich German    | NA                                                                                                           | ʀ       | NA         | FALSE    |
|        2337 | luxe1241   | Luxembourgish    | NA                                                                                                           | ʀ       | NA         | FALSE    |
|        2398 | stan1295   | German           | German (Standard)                                                                                            | ʀ       | NA         | FALSE    |
|        2399 | east2295   | Eastern Yiddish  | Eastern Yiddish (Central)                                                                                    | ʀ       | NA         | FALSE    |
|        2540 | kara1462   | Tofa             | NA                                                                                                           | ʀ       | NA         | FALSE    |

But note that these include multiple doculects for the same language,
e.g. multiple different analyses of German or Dutch and their dialects.
By Glottocode, we have only 13 observations.

    temp %>%
      group_by(Glottocode) %>%
      summarize(count = n()) %>%
      kable()

    ## `summarise()` ungrouping output (override with `.groups` argument)

| Glottocode | count |
|:-----------|------:|
| bata1289   |     2 |
| dutc1256   |     4 |
| east2295   |     2 |
| japr1238   |     1 |
| kara1462   |     1 |
| luxe1241   |     2 |
| mogh1245   |     1 |
| siwi1239   |     1 |
| stan1290   |     2 |
| stan1295   |     3 |
| stan1318   |     1 |
| swis1247   |     1 |
| xiri1243   |     1 |

Here is a glimpse at the uvulars data in general.

    uvulars %>%
      select(InventoryID, Glottocode, LanguageName, SpecificDialect, Phoneme, Allophones, Marginal) %>%
      head() %>%
      kable()

| InventoryID | Glottocode | LanguageName | SpecificDialect | Phoneme | Allophones     | Marginal |
|------------:|:-----------|:-------------|:----------------|:--------|:---------------|:---------|
|           2 | kett1243   | Ket          | NA              | q       | ɣ qχ ɢ ʁ q ʀ χ | NA       |
|           3 | lakk1252   | Lak          | NA              | qʰ      | qʰ             | NA       |
|           3 | lakk1252   | Lak          | NA              | qʷʰ     | qʷʰ            | NA       |
|           3 | lakk1252   | Lak          | NA              | qʷʼ     | qʷʼ            | NA       |
|           3 | lakk1252   | Lak          | NA              | qʼ      | qʼ             | NA       |
|           3 | lakk1252   | Lak          | NA              | q͈       | q͈              | NA       |

Let’s drop the rhotics and generate the counts. First get the uvulars
without the rhotics.

    uvulars_no_rhotics <- uvulars %>% filter(!grepl("ʀ", Phoneme))

Test that they’ve all been removed.

    expect_equal(nrow(uvulars_no_rhotics %>% filter(grepl("ʀ", Phoneme))), 0)
    expect_equal(nrow(uvulars %>% filter(!grepl("ʀ", Phoneme))) + nrow(uvulars %>% filter(grepl("ʀ", Phoneme))), nrow(uvulars))

    uvular_counts_minus_rhotics <- uvulars_no_rhotics %>%
      group_by(InventoryID) %>%
      summarize(Uvulars_no_rhotics = n())

    ## `summarise()` ungrouping output (override with `.groups` argument)

    uvular_marginals_minus_rhotics <- uvulars_no_rhotics %>%
      filter(Marginal) %>%
      group_by(InventoryID) %>%
      summarize(Marginal_Uvular_no_rhotics = n())

    ## `summarise()` ungrouping output (override with `.groups` argument)

Since we do some hand annotation, e.g. adding in Urban’s more
fine-grained macroareas, we need to load up the data that we use in our
analyses and then add in the new non-rhotic uvular counts.

    results <- read_csv("../Data/uvulars_ejectives_pruned2.csv")

    ## 
    ## ── Column specification ────────────────────────────────────────────────────────
    ## cols(
    ##   .default = col_character(),
    ##   InventoryID = col_double(),
    ##   elevation = col_double(),
    ##   Uvulars = col_double(),
    ##   Marginal_Uvular = col_double(),
    ##   Nonmarginal_Uvular = col_double(),
    ##   Ejectives = col_double(),
    ##   Marginal_Ejective = col_double(),
    ##   Nonmarginal_Ejective = col_double()
    ## )
    ## ℹ Use `spec()` for the full column specifications.

A quick look:

    results %>%
      head() %>%
      kable()

| InventoryID | Glottocode | ISO6393 | GlottologName  | PhoibleName | SpecificDialect    | Source | family\_id | level    | status     | latitude  | longitude | country\_ids   | macroarea | macroarea2 | elevation | Uvulars | Marginal\_Uvular | Nonmarginal\_Uvular | Ejectives | Marginal\_Ejective | Nonmarginal\_Ejective |
|------------:|:-----------|:--------|:---------------|:------------|:-------------------|:-------|:-----------|:---------|:-----------|:----------|:----------|:---------------|:----------|:-----------|----------:|--------:|-----------------:|--------------------:|----------:|-------------------:|----------------------:|
|        2552 | abkh1244   | abk     | Abkhazian      | Abkhaz      | Abkhaz (Bzyb)      | ea     | abkh1242   | language | vulnerable | 43,056218 | 41,159115 | GE RU TR       | Eurasia   | Europe     |       539 |      11 |                0 |                  11 |        13 |                  0 |                    13 |
|        2349 | adyg1241   | ady     | Adyghe         | Adyghe      | NA                 | ea     | abkh1242   | language | vulnerable | 44        | 39,33     | IL JO RU SY TR | Eurasia   | Europe     |       304 |       6 |                0 |                   6 |        14 |                  0 |                    14 |
|        2310 | kaba1278   | kbd     | Kabardian      | Kabardian   | Kabardian (Baksan) | ea     | abkh1242   | language | vulnerable | 43,5082   | 43,3918   | RU TR          | Eurasia   | Europe     |      1189 |       8 |                0 |                   8 |        10 |                  0 |                    10 |
|         198 | afad1236   | aal     | Afade          | KOTOKO      | NA                 | upsid  | afro1255   | language | vulnerable | 12,0551   | 14,6343   | CM NG          | Africa    | Africa     |       287 |       0 |                0 |                   0 |         4 |                  0 |                     4 |
|        1342 | afar1241   | aar     | Afar           | Afar        | NA                 | gm     | afro1255   | language | safe       | 12,2281   | 41,8083   | DJ ER ET       | Africa    | Africa     |       354 |       0 |                0 |                   0 |         0 |                  0 |                     0 |
|        1451 | alab1254   | alw     | Alaba-K’abeena | Alaaba      | NA                 | gm     | afro1255   | language | safe       | 7,39014   | 38,1732   | ET             | Africa    | Africa     |      1984 |       0 |                0 |                   0 |         8 |                  0 |                     8 |

Now let’s merge in the new counts.

    results_uvulars <- left_join(results, uvular_counts_minus_rhotics)

    ## Joining, by = "InventoryID"

    results_uvulars <- left_join(results_uvulars, uvular_marginals_minus_rhotics)

    ## Joining, by = "InventoryID"

Have a look.

    results_uvulars %>%
      head() %>%
      kable()

| InventoryID | Glottocode | ISO6393 | GlottologName  | PhoibleName | SpecificDialect    | Source | family\_id | level    | status     | latitude  | longitude | country\_ids   | macroarea | macroarea2 | elevation | Uvulars | Marginal\_Uvular | Nonmarginal\_Uvular | Ejectives | Marginal\_Ejective | Nonmarginal\_Ejective | Uvulars\_no\_rhotics | Marginal\_Uvular\_no\_rhotics |
|------------:|:-----------|:--------|:---------------|:------------|:-------------------|:-------|:-----------|:---------|:-----------|:----------|:----------|:---------------|:----------|:-----------|----------:|--------:|-----------------:|--------------------:|----------:|-------------------:|----------------------:|---------------------:|------------------------------:|
|        2552 | abkh1244   | abk     | Abkhazian      | Abkhaz      | Abkhaz (Bzyb)      | ea     | abkh1242   | language | vulnerable | 43,056218 | 41,159115 | GE RU TR       | Eurasia   | Europe     |       539 |      11 |                0 |                  11 |        13 |                  0 |                    13 |                   11 |                            NA |
|        2349 | adyg1241   | ady     | Adyghe         | Adyghe      | NA                 | ea     | abkh1242   | language | vulnerable | 44        | 39,33     | IL JO RU SY TR | Eurasia   | Europe     |       304 |       6 |                0 |                   6 |        14 |                  0 |                    14 |                    6 |                            NA |
|        2310 | kaba1278   | kbd     | Kabardian      | Kabardian   | Kabardian (Baksan) | ea     | abkh1242   | language | vulnerable | 43,5082   | 43,3918   | RU TR          | Eurasia   | Europe     |      1189 |       8 |                0 |                   8 |        10 |                  0 |                    10 |                    8 |                            NA |
|         198 | afad1236   | aal     | Afade          | KOTOKO      | NA                 | upsid  | afro1255   | language | vulnerable | 12,0551   | 14,6343   | CM NG          | Africa    | Africa     |       287 |       0 |                0 |                   0 |         4 |                  0 |                     4 |                   NA |                            NA |
|        1342 | afar1241   | aar     | Afar           | Afar        | NA                 | gm     | afro1255   | language | safe       | 12,2281   | 41,8083   | DJ ER ET       | Africa    | Africa     |       354 |       0 |                0 |                   0 |         0 |                  0 |                     0 |                   NA |                            NA |
|        1451 | alab1254   | alw     | Alaba-K’abeena | Alaaba      | NA                 | gm     | afro1255   | language | safe       | 7,39014   | 38,1732   | ET             | Africa    | Africa     |      1984 |       0 |                0 |                   0 |         8 |                  0 |                     8 |                   NA |                            NA |

Let’s replace the NA with zeros.

    results_uvulars <- results_uvulars %>% mutate(Uvulars_no_rhotics = replace_na(Uvulars_no_rhotics, 0))
    results_uvulars <- results_uvulars %>% mutate(Marginal_Uvular_no_rhotics = replace_na(Marginal_Uvular_no_rhotics, 0))

Let’s check where the counts differ and make sure we’re getting the same
languages that we noted above with rhotic uvulars.

    x <- results_uvulars %>% filter(Uvulars != Uvulars_no_rhotics)

Since we use a trump order for which inventories we include when there
are duplicates, let’s see which ones from our various doculects above
are not in the final results.

    temp[which(!temp$InventoryID %in% results_uvulars$InventoryID), ] %>% kable()

| InventoryID | Glottocode | LanguageName    | SpecificDialect                                                                                              | Phoneme | Allophones | Marginal |
|------------:|:-----------|:----------------|:-------------------------------------------------------------------------------------------------------------|:--------|:-----------|:---------|
|          33 | bata1289   | Batak           | NA                                                                                                           | ʀ       | ʀ          | NA       |
|         161 | stan1295   | German          | NA                                                                                                           | ʀ       | ʌ ʌ̯ ʀ r    | NA       |
|         162 | stan1290   | French          | NA                                                                                                           | ʀ       | ʀ ʁ̞ ʀ̥      | NA       |
|         241 | bata1289   | BATAK           | NA                                                                                                           | ʀ       | NA         | TRUE     |
|         304 | stan1295   | GERMAN          | NA                                                                                                           | ʀ       | NA         | FALSE    |
|         331 | stan1290   | FRENCH          | NA                                                                                                           | ʀ       | NA         | FALSE    |
|        2171 | dutc1256   | Dutch           | the dialect of Maastricht (south-eastern dialect group; Central Limburgian)                                  | ʀʁ      | ʀʁ ʀ̥ʁ̥      | FALSE    |
|        2172 | dutc1256   | Dutch           | the Dutch dialect of Weert (West-Limburgian), rural variety (not Stadsweerts)                                | ʀʁ      | ʀʁ ʀ̥ʁ̥      | FALSE    |
|        2173 | dutc1256   | Dutch           | the Belgian Limburg dialect of Hamont (belongs to the West Limburg dialects, subclassification: Dommellands) | ʀ       | ʀ χ        | FALSE    |
|        2272 | swis1247   | Zurich German   | NA                                                                                                           | ʀ       | NA         | FALSE    |
|        2337 | luxe1241   | Luxembourgish   | NA                                                                                                           | ʀ       | NA         | FALSE    |
|        2398 | stan1295   | German          | German (Standard)                                                                                            | ʀ       | NA         | FALSE    |
|        2399 | east2295   | Eastern Yiddish | Eastern Yiddish (Central)                                                                                    | ʀ       | NA         | FALSE    |

And which ones were included.

    temp[which(temp$InventoryID %in% results_uvulars$InventoryID), ] %>% kable()

| InventoryID | Glottocode | LanguageName     | SpecificDialect                 | Phoneme | Allophones | Marginal |
|------------:|:-----------|:-----------------|:--------------------------------|:--------|:-----------|:---------|
|         459 | mogh1245   | MOGHOL           | NA                              | ʀ       | NA         | FALSE    |
|        1049 | east2295   | Standard Yiddish | NA                              | ʀ       | ʀ          | FALSE    |
|        1050 | dutc1256   | Dutch            | Belgian Standard                | ʀ       | ʀ r        | FALSE    |
|        1610 | siwi1239   | Siwi             | NA                              | ʀ       | ʀ          | FALSE    |
|        1837 | xiri1243   | Xiriâna          | NA                              | ʀ       | NA         | NA       |
|        1874 | japr1238   | Japreria         | NA                              | ʀ       | NA         | NA       |
|        2157 | stan1318   | Arabic           | Safad, Beirut, Damascus, Kuwait | xʀ̥      | xʀ̥         | FALSE    |
|        2200 | luxe1241   | Luxembourgish    | Standard central Luxembourgish  | ʀ       | ʀ ʁ ə ɐ χ  | FALSE    |
|        2540 | kara1462   | Tofa             | NA                              | ʀ       | NA         | FALSE    |

Write the non-rhotic uvular counts to disk.

    write_csv(results_uvulars, "../Data/uvulars_ejectives_pruned2_rhotics.csv")
