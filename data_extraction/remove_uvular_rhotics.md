Remove uvular rhotics from counts
================
Steven Moran
(17 November, 2020)

    library(tidyverse)
    library(knitr)
    library(testthat)

# Overview

An anonymous reviewer notes that our decision to include uvular rhotics
requires explicit justification. They would like to see if there is any
impact on removing rhotics from the uvular counts on our results.

An important question in response is, what does the reviewer mean
explicitly by rhotic? Rhotics are cross-linguistically hard to define;
see for example the in-depth description of rhotics in (Ladefoged and
Maddieson 1996). In particular, what phonetic properties should be
considered for the “gluttal R” phenomenon?

-   <a href="https://en.wikipedia.org/wiki/Guttural_R" class="uri">https://en.wikipedia.org/wiki/Guttural_R</a>

Ladefoged & Maddieson mention that the prototypical rhotic members are
trills made with the tip or blade of the tongue, i.e., alveolar \[r\] or
uvular \[ʀ\]. However, other continuants made at the uvular place of
articulation can also be categorized as rhotics.

As they point out, from an expert phonetician’s point of view, it:

> > > “is whether the class membership is based only on synchronic and
> > > diachronic relationships between the members of the class, or
> > > whether there is indeed a phonetic similarity between all rhotics
> > > that has hitherto been missed”

Additionally, they note that similarity may be either acoustic or
auditory.

We show in our [extraction code](get_uvulars_ejectives.md) how many
general rhotics, i.e. according the [IPA
chart](https://www.internationalphoneticassociation.org/content/full-ipa-chart)
are present in the data we extracted.

“having thought a bit more, I’d include ʁ and ʀ but exclude χ and cite
Maddieson & Ladefoged in support.”

Let’s drop the rhotics, including \[ʁ\] and \[ʀ\], but excluding \[χ\]
(Ladefoged and Maddieson 1996) and re-generate the counts. First load
the uvular data.

    uvulars <- read_csv("uvulars.csv")

Next, get the uvulars without the rhotics.

    uvulars_no_rhotics <- uvulars %>% filter(!grepl("ʀ|ʁ", Phoneme))

Which rhotics did we remove?

    uvulars_with_rhotics <- uvulars %>% filter(grepl("ʀ|ʁ", Phoneme))

    uvulars_with_rhotics %>%
      select(InventoryID, Glottocode, LanguageName, Phoneme, Allophones) %>%
      head() %>%
      kable()

| InventoryID | Glottocode | LanguageName | Phoneme | Allophones |
|------------:|:-----------|:-------------|:--------|:-----------|
|           4 | kaba1278   | Kabardian    | ʁ       | ʁ          |
|           4 | kaba1278   | Kabardian    | ʁʷ      | ʁʷ         |
|           5 | nucl1302   | Georgian     | ʁ       | ʁ χ͉ ʁ      |
|           6 | buru1296   | Burushaski   | ʁ       | ʁ          |
|          33 | bata1289   | Batak        | ʀ       | ʀ          |
|          61 | aleu1260   | Aleut        | ʁ       | ʁ          |

    uvulars_with_rhotics %>%
      group_by(Phoneme) %>%
      summarize(count = n()) %>%
      arrange(desc(count)) %>%
      kable()

    ## `summarise()` ungrouping output (override with `.groups` argument)

| Phoneme | count |
|:--------|------:|
| ʁ       |   155 |
| ʁʷ      |    34 |
| ʀ       |    19 |
| ʁ̞       |     6 |
| ʁː      |     4 |
| ʁˤ      |     3 |
| ɢʁ      |     2 |
| ʀʁ      |     2 |
| ʁʲ      |     2 |
| ʁʷˤ     |     2 |
| ʁ̞ʷ      |     1 |
| ʁ̞̰ʷ      |     1 |
| ʁʷː     |     1 |
| xʀ̥      |     1 |

Test that they’ve all been removed.

    expect_equal(nrow(uvulars_no_rhotics %>% filter(grepl("ʀ|ʁ", Phoneme))), 0)
    expect_equal(nrow(uvulars %>% filter(!grepl("ʀ|ʁ", Phoneme))) + nrow(uvulars %>% filter(grepl("ʀ|ʁ", Phoneme))), nrow(uvulars))

Count the uvulars and marginal uvulars without rhotics.

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
|        2552 | abkh1244   | abk     | Abkhazian      | Abkhaz      | Abkhaz (Bzyb)      | ea     | abkh1242   | language | vulnerable | 43,056218 | 41,159115 | GE RU TR       | Eurasia   | Europe     |       539 |      11 |                0 |                  11 |        13 |                  0 |                    13 |                    8 |                            NA |
|        2349 | adyg1241   | ady     | Adyghe         | Adyghe      | NA                 | ea     | abkh1242   | language | vulnerable | 44        | 39,33     | IL JO RU SY TR | Eurasia   | Europe     |       304 |       6 |                0 |                   6 |        14 |                  0 |                    14 |                    4 |                            NA |
|        2310 | kaba1278   | kbd     | Kabardian      | Kabardian   | Kabardian (Baksan) | ea     | abkh1242   | language | vulnerable | 43,5082   | 43,3918   | RU TR          | Eurasia   | Europe     |      1189 |       8 |                0 |                   8 |        10 |                  0 |                    10 |                    6 |                            NA |
|         198 | afad1236   | aal     | Afade          | KOTOKO      | NA                 | upsid  | afro1255   | language | vulnerable | 12,0551   | 14,6343   | CM NG          | Africa    | Africa     |       287 |       0 |                0 |                   0 |         4 |                  0 |                     4 |                   NA |                            NA |
|        1342 | afar1241   | aar     | Afar           | Afar        | NA                 | gm     | afro1255   | language | safe       | 12,2281   | 41,8083   | DJ ER ET       | Africa    | Africa     |       354 |       0 |                0 |                   0 |         0 |                  0 |                     0 |                   NA |                            NA |
|        1451 | alab1254   | alw     | Alaba-K’abeena | Alaaba      | NA                 | gm     | afro1255   | language | safe       | 7,39014   | 38,1732   | ET             | Africa    | Africa     |      1984 |       0 |                0 |                   0 |         8 |                  0 |                     8 |                   NA |                            NA |

Let’s replace the NA with zeros.

    results_uvulars <- results_uvulars %>% mutate(Uvulars_no_rhotics = replace_na(Uvulars_no_rhotics, 0))
    results_uvulars <- results_uvulars %>% mutate(Marginal_Uvular_no_rhotics = replace_na(Marginal_Uvular_no_rhotics, 0))

Let’s check where the counts differ and make sure we’re getting the same
languages that we noted above with rhotic uvulars. There are 11
observations.

    results_uvulars %>%
      filter(Uvulars != Uvulars_no_rhotics) %>%
      select(InventoryID, Glottocode, GlottologName, SpecificDialect, Source, Uvulars, Uvulars_no_rhotics) %>%
      kable()

| InventoryID | Glottocode   | GlottologName           | SpecificDialect                                                       | Source | Uvulars | Uvulars\_no\_rhotics |
|------------:|:-------------|:------------------------|:----------------------------------------------------------------------|:-------|--------:|---------------------:|
|        2552 | abkh1244     | Abkhazian               | Abkhaz (Bzyb)                                                         | ea     |      11 |                    8 |
|        2349 | adyg1241     | Adyghe                  | NA                                                                    | ea     |       6 |                    4 |
|        2310 | kaba1278     | Kabardian               | Kabardian (Baksan)                                                    | ea     |       8 |                    6 |
|        1542 | awng1244     | Awngi                   | NA                                                                    | gm     |       4 |                    2 |
|        1248 | bana1305     | Bana                    | NA                                                                    | gm     |       4 |                    2 |
|         132 | egyp1253     | Egyptian Arabic         | NA                                                                    | spa    |       6 |                    4 |
|        1403 | hdii1240     | Hdi                     | NA                                                                    | gm     |       4 |                    2 |
|        1406 | lagw1237     | Lagwan                  | NA                                                                    | gm     |       2 |                    0 |
|        1098 | liby1240     | Libyan Arabic           | NA                                                                    | ph     |       3 |                    2 |
|         591 | lish1246     | Lishán Didán            | NA                                                                    | upsid  |       3 |                    2 |
|         133 | moro1292     | Moroccan Arabic         | NA                                                                    | spa    |       9 |                    6 |
|        2607 | najd1235     | Najdi Arabic            | NA                                                                    | ea     |       3 |                    2 |
|        2621 | sheh1240     | Jibbali                 | Jibbali (Central)                                                     | ea     |       2 |                    1 |
|        1610 | siwi1239     | Siwi                    | NA                                                                    | gm     |       1 |                    0 |
|        2157 | stan1318     | Standard Arabic         | Safad, Beirut, Damascus, Kuwait                                       | uz     |       2 |                    1 |
|         575 | taha1241     | Tahaggart Tamahaq       | NA                                                                    | upsid  |       3 |                    2 |
|        1837 | xiri1243     | Xiriâna                 | NA                                                                    | saphon |       1 |                    0 |
|         206 | ahte1237     | Ahtena                  | NA                                                                    | upsid  |       5 |                    4 |
|         651 | ajab1235     | Aja (Benin)             | adja (Bénin)                                                          | aa     |       2 |                    1 |
|        1282 | gbes1238     | Gbesi Gbe               | NA                                                                    | gm     |       4 |                    2 |
|        1281 | kota1272     | Kotafon Gbe             | NA                                                                    | gm     |       4 |                    2 |
|        1411 | ngom1272     | Ngomba                  | NA                                                                    | gm     |       2 |                    0 |
|        1280 | west2456     | Western Xwla Gbe        | NA                                                                    | gm     |       4 |                    2 |
|        1279 | xwel1235     | Xwela Gbe               | NA                                                                    | gm     |       4 |                    2 |
|        1669 | bela1260     | Lemeting                | Metting                                                               | gm     |       1 |                    0 |
|        2222 | coco1260     | Cocos Islands Malay     | Spoken in Kampung Cocos in the Lahad Datu district of Sabah, Malaysia | uz     |       1 |                    0 |
|        1214 | muna1247     | Muna                    | NA                                                                    | ph     |       1 |                    0 |
|        1874 | japr1238     | Japrería                | NA                                                                    | saphon |       1 |                    0 |
|         862 | barb1263     | Barbareño               | NA                                                                    | ph     |       3 |                    4 |
|        2474 | chen1255     | Chenchu                 | NA                                                                    | ea     |       1 |                    0 |
|          61 | aleu1260     | Aleut                   | NA                                                                    | spa    |       3 |                    2 |
|         319 | cent2128     | Central Siberian Yupik  | NA                                                                    | upsid  |       6 |                    4 |
|          62 | kala1399     | Kalaallisut             | NA                                                                    | spa    |       6 |                    5 |
|        2291 | nauk1242     | Naukan Yupik            | NA                                                                    | ea     |       3 |                    2 |
|        1077 | nort2943     | North Alaskan Inupiatun | NA                                                                    | ph     |       3 |                    2 |
|        1914 | abip1241     | Abipon                  | NA                                                                    | saphon |       2 |                    1 |
|        1118 | kadi1248     | Kadiwéu                 | NA                                                                    | ph     |       2 |                    1 |
|        1127 | moco1246     | Mocoví                  | NA                                                                    | ph     |       2 |                    1 |
|        2219 | bukh1238     | Bukharic                | Bukharan                                                              | uz     |       2 |                    1 |
|        2546 | camp1261     | Campidanese Sardinian   | Campidanese Sardinian (Sestu)                                         | ea     |       1 |                    0 |
|        2167 | dani1285     | Danish                  | NA                                                                    | uz     |       1 |                    0 |
|        1050 | dutc1256     | Dutch                   | Belgian Standard                                                      | ph     |       1 |                    0 |
|        1049 | east2295     | Eastern Yiddish         | NA                                                                    | ph     |       1 |                    0 |
|        1438 | fern1234     | Pichi                   | NA                                                                    | gm     |       1 |                    0 |
|        2537 | khal1271     | Khalkhal                | NA                                                                    | ea     |       2 |                    1 |
|        1570 | krio1253     | Krio                    | NA                                                                    | gm     |       1 |                    0 |
|        2221 | kumz1235     | Kumzari                 | Kumzari spoken in Khasab, Oman                                        | uz     |       3 |                    2 |
|        2200 | luxe1241     | Luxembourgish           | Standard central Luxembourgish                                        | uz     |       3 |                    1 |
|        2484 | nort2641     | Northern Kurdish        | Northern Kurdish (Kurmanji)                                           | ea     |       3 |                    2 |
|         171 | nucl1235     | Eastern Armenian        | NA                                                                    | spa    |       2 |                    1 |
|         510 | ormu1247     | Ormuri                  | NA                                                                    | upsid  |       2 |                    1 |
|         976 | osse1243     | Ossetian                | NA                                                                    | ph     |       3 |                    2 |
|        2206 | port1283\_01 | Portuguese              | Lisbon                                                                | uz     |       1 |                    0 |
|        1564 | reun1238     | Réunion Creole French   | NA                                                                    | gm     |       1 |                    0 |
|        2182 | stan1290     | French                  | French (Parisian speaker)                                             | uz     |       1 |                    0 |
|        2184 | stan1295     | German                  | NA                                                                    | uz     |       1 |                    0 |
|        2266 | taji1245     | Tajik                   | Tajik (Bukhara)                                                       | ea     |       3 |                    2 |
|        2232 | uppe1400     | Upper Saxon             | Chemnitz dialect                                                      | uz     |       2 |                    1 |
|        2514 | wakh1245     | Wakhi                   | NA                                                                    | ea     |       3 |                    2 |
|        2622 | west2368\_01 | Western Balochi         | Western Balochi (Sarāwānī)                                            | ea     |       2 |                    1 |
|        2418 | yagn1238     | Yagnobi                 | NA                                                                    | ea     |       4 |                    3 |
|        2561 | buru1296     | Burushaski              | Burushaski (Werchikwar)                                               | ea     |       3 |                    2 |
|        2370 | gily1242     | Nivkh                   | Nivkh (West Sakhalin)                                                 | ea     |       4 |                    3 |
|        2392 | svan1243     | Svan                    | Svan (Upper Bal)                                                      | ea     |       4 |                    3 |
|        2477 | dong1285     | Dongxiang               | NA                                                                    | ea     |       3 |                    2 |
|        2242 | east2337     | East Yugur              | NA                                                                    | ea     |       4 |                    3 |
|        2301 | kang1281     | Kangjia                 | NA                                                                    | ea     |       4 |                    3 |
|         459 | mogh1245     | Mogholi                 | NA                                                                    | upsid  |       2 |                    1 |
|        2465 | andi1255     | Andi                    | NA                                                                    | ea     |      12 |                   10 |
|         228 | arch1244     | Archi                   | NA                                                                    | upsid  |      22 |                   18 |
|        2467 | avar1256     | Avar                    | Avar (Chadakolob)                                                     | ea     |       8 |                    6 |
|        2390 | bats1242     | Bats                    | NA                                                                    | ea     |       4 |                    3 |
|        2329 | dido1241     | Tsez                    | Tsez (Asax)                                                           | ea     |       7 |                    5 |
|        2565 | ghod1238     | Godoberi                | NA                                                                    | ea     |       8 |                    6 |
|        2237 | hunz1247     | Hunzib                  | NA                                                                    | uz     |       4 |                    3 |
|        2335 | lakk1252     | Lak                     | Lak (Kumux)                                                           | ea     |      11 |                   10 |
|         531 | rutu1240     | Rutul                   | NA                                                                    | upsid  |      20 |                   16 |
|        1059 | zoog1238     | Zoogocho Zapotec        | zxon                                                                  | ph     |       1 |                    0 |
|         543 | shus1248     | Shuswap                 | NA                                                                    | upsid  |       9 |                    6 |
|        2614 | amdo1237     | Amdo Tibetan            | NA                                                                    | ea     |       2 |                    1 |
|        1208 | jiar1240     | Northern Gyalrong       | Chabao; Japhug                                                        | ph     |       5 |                    4 |
|        2429 | nort2722     | Northern Qiang          | Northern Qiang (Yadu)                                                 | ea     |       4 |                    3 |
|        2298 | puxi1242     | Puxi                    | NA                                                                    | ea     |       4 |                    3 |
|        2230 | shix1238     | Shixing                 | Lower Xumi                                                            | uz     |       4 |                    3 |
|        2420 | zhon1235     | Zhongu                  | NA                                                                    | ea     |       4 |                    3 |
|        1440 | dime1235     | Dime                    | NA                                                                    | gm     |       2 |                    1 |
|        2605 | suii1243     | Sui                     | NA                                                                    | ea     |       4 |                    3 |
|        2075 | avac1239     | Avá-Canoeiro            | NA                                                                    | saphon |       1 |                    0 |
|        2500 | bash1264     | Bashkir                 | Bashkir (Standard)                                                    | ea     |       2 |                    1 |
|        2540 | kara1462     | Taiga Sayan Turkic      | NA                                                                    | ea     |       3 |                    2 |
|        2318 | kara1467     | Kara-Kalpak             | NA                                                                    | ea     |       3 |                    2 |
|        2433 | khak1248     | Khakas                  | Khakas (Standard)                                                     | ea     |       2 |                    1 |
|        2355 | kumy1244     | Kumyk                   | NA                                                                    | ea     |       2 |                    1 |
|         603 | nort2690     | Northern Uzbek          | NA                                                                    | upsid  |       3 |                    2 |
|        2296 | nort2697     | North Azerbaijani       | North Azerbaijani (Shirvan)                                           | ea     |       2 |                    1 |
|        2609 | tata1255     | Tatar                   | Tatar (Standard)                                                      | ea     |       3 |                    2 |
|        2612 | uigh1240     | Uighur                  | Uyghur (Xinjiang)                                                     | ea     |       2 |                    1 |
|        2444 | yaku1245     | Sakha                   | Sakha (Standard)                                                      | ea     |       2 |                    1 |
|        2434 | vach1239     | Vach Khanty             | Eastern Khanty (Vakh)                                                 | ea     |       2 |                    1 |
|        1084 | utes1238     | Ute-Southern Paiute     | NA                                                                    | ph     |       3 |                    2 |
|        2286 | nort2745     | Northern Yukaghir       | NA                                                                    | ea     |       2 |                    1 |
|        1027 | sout2750     | Southern Yukaghir       | Southern                                                              | ph     |       2 |                    1 |

Write the non-rhotic uvular counts to disk.

    write_csv(results_uvulars, "../Data/uvulars_ejectives_pruned2_rhotics.csv")

# References

<div id="refs" class="references hanging-indent">

<div id="ref-LadefogedMaddieson1996">

Ladefoged, Peter, and Ian Maddieson. 1996. *The Sounds of the World’s
Languages*. Cambridge, UK: Blackwell.

</div>

</div>
