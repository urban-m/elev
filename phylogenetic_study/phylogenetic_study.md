Phylogenetic study of ejectives and uvulars (presense / absence) in IE
and ST
================
Steven Moran
22 June, 2020

For the traits, we use presence vs. absence of ejectives and uvulars.
Hence, we can use the “Nonmarginal Ejective” and “Nonmarginal Uvular”
columns in the datasheet. These are the number of ejectives/uvulars
minus number of marginal ejectives/uvulars. For the logistic regression,
these counts were used as the basis for the binary distinction. So if
you have one uvular but it’s coded as marginal it’s going to be noted as
FALSE.

For the phylognetic traits, we use presence / absence of class, e.g. a
language does or does not have ejectives or uvulars. This is basically a
discrete variable model for phylogenetic reconstruction. Then we’ll have
the probability of the trait (variable) at the root.

The Indo-European and Sino-Tibetan phylogenies come from:

  - Chang W, Cathcart C, Hall D, & Garrett A. 2015. Ancestry-constrained
    phylogenetic analysis supports the Indo-European steppe hypothesis.
    Language, 91(1):194-244.

  - Zhang M, Yan S, Pan W, & Jin L. 2019. Phylogenetic evidence for
    Sino-Tibetan origin in northern China in the Late Neolithic. Nature,
    569, 112-115.

## Data prep

Generate the traits data.

``` r
df <- read.csv('../uvulars_ejectives_pruned.csv', stringsAsFactors = FALSE)
```

Which sources are <NA> for Glottocodes? This issue is discussed in
detail here:

  - <https://github.com/phoible/dev/issues/270>

<!-- end list -->

``` r
df %>% filter(is.na(Glottocode))
```

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

Extract relevant data for this analysis and then investigate ejectives
and uvulars.

``` r
ejectives <- df %>% select(InventoryID, Glottocode, Nonmarginal_Ejective) %>% rename(has_ejectives = Nonmarginal_Ejective)
uvulars <- df %>% select(InventoryID, Glottocode, Nonmarginal_Uvular) %>% rename(has_uvulars = Nonmarginal_Uvular)
```

### Ejectives

Check for duplicate Glottocodes (we can have only one data point per
daughter node on the phylogeny).

``` r
e.dups <- ejectives %>% group_by(Glottocode) %>% filter(n()>1)
e.dups %>% arrange(Glottocode) %>% kable()
```

| InventoryID | Glottocode | has\_ejectives |
| ----------: | :--------- | -------------: |
|         651 | ajab1235   |              0 |
|         652 | ajab1235   |              0 |
|        1239 | barb1263   |              9 |
|         862 | barb1263   |              0 |
|         179 | basq1248   |              0 |
|        2161 | basq1248   |              0 |
|         296 | cayu1262   |              0 |
|        1926 | cayu1262   |              0 |
|        1085 | dupa1235   |              0 |
|        2253 | dupa1235   |              0 |
|          88 | karo1304   |              0 |
|         438 | karo1304   |              0 |
|        2882 | kawa1290   |              0 |
|        2883 | kawa1290   |              0 |
|         944 | mose1249   |              0 |
|        1986 | mose1249   |              0 |
|        2030 | nort2980   |              0 |
|        2037 | nort2980   |              0 |
|        2031 | nort2980   |              0 |
|        2206 | port1283   |              0 |
|        2207 | port1283   |              0 |
|         532 | sand1273   |              5 |
|        1652 | sand1273   |              6 |
|        2308 | stan1288   |              0 |
|        2210 | stan1288   |              0 |
|        1939 | trum1247   |              4 |
|         588 | trum1247   |              0 |
|        2622 | west2368   |              0 |
|        2345 | west2368   |              0 |
|        2348 | west2368   |              0 |
|        2618 | west2368   |              0 |
|        2782 | west2443   |              0 |
|        2988 | west2443   |              0 |
|         648 | zuni1245   |              4 |
|          91 | zuni1245   |             12 |
|        2281 | NA         |              0 |
|        2729 | NA         |              0 |

There’s disagreement about ejectives between inventories for:

  - barb1263: 0 vs 9 (862 vs 1239)
  - sand1273: 5 vs 6 (532 vs 1652)
  - trum1247: 0 vs 4 (588 vs 1939)
  - zuni1245: 4 vs 12 (648 vs 91)

Since we’re only interested in the presence vs absence of ejectives, we
investigate why barb1263 and trum1247 differ. For detailed report, see:

  - <https://github.com/bambooforest/phoible-scripts/blob/master/duplicates/duplicate-inventories.md>

In sum, the phonological inventory for barb1263:

  - <https://phoible.org/inventories/view/1239>

incorrectly had ejectives (due to a misintrepretation of symbols in the
source document). Refer to the issue here for more details:

  - <https://github.com/phoible/dev/issues/265>

The phonological inventories for trum1247 are:

  - <https://phoible.org/inventories/view/588>
  - <https://phoible.org/inventories/view/1939>

The first comes from UPSID via Monod-Becquelin (1975) and does not list
ejectives at all. The second is from the more recent work by Guiardello
(1999:1), who writes:

    Trumai has 23 consonants and 6 vowels. In Guiardello (1992), the number of Trumai phonemic consonants was smaller, given that the analysis for some phones was different from the current one: the lateral fricative /ɬ/ was analyzed as an allophone of /l/, but now it is analyzed as a independent phoneme; the affricate /ts/ and the ejectives /t̪ʼ/, /tʼ/,/kʼ/, and /tsʼ/ were previously classified as consonant clusters (i.e. /t+s/ and /t+s+ʔ/), but now we believe they are more adequately analyzed as single phonemes. Therefore, the consonant chart presented here has some differences in relation to the one presented in previous work. 

Therefore, we take the second inventory for analysis.

### Uvulars

Check for duplicate Glottocodes (we can have only one data point per
daughter node on the phylogeny).

``` r
u.dups <- uvulars %>% group_by(Glottocode) %>% filter(n()>1)
u.dups %>% arrange(Glottocode) %>% kable()
```

| InventoryID | Glottocode | has\_uvulars |
| ----------: | :--------- | -----------: |
|         651 | ajab1235   |            2 |
|         652 | ajab1235   |            0 |
|        1239 | barb1263   |            3 |
|         862 | barb1263   |            2 |
|         179 | basq1248   |            1 |
|        2161 | basq1248   |            0 |
|         296 | cayu1262   |            0 |
|        1926 | cayu1262   |            0 |
|        1085 | dupa1235   |            0 |
|        2253 | dupa1235   |            0 |
|          88 | karo1304   |            0 |
|         438 | karo1304   |            0 |
|        2882 | kawa1290   |            0 |
|        2883 | kawa1290   |            0 |
|         944 | mose1249   |            1 |
|        1986 | mose1249   |            0 |
|        2030 | nort2980   |            1 |
|        2037 | nort2980   |            1 |
|        2031 | nort2980   |            0 |
|        2206 | port1283   |            1 |
|        2207 | port1283   |            0 |
|         532 | sand1273   |            0 |
|        1652 | sand1273   |            0 |
|        2308 | stan1288   |            1 |
|        2210 | stan1288   |            0 |
|        1939 | trum1247   |            0 |
|         588 | trum1247   |            0 |
|        2622 | west2368   |            2 |
|        2345 | west2368   |            0 |
|        2348 | west2368   |            0 |
|        2618 | west2368   |            0 |
|        2782 | west2443   |            0 |
|        2988 | west2443   |            0 |
|         648 | zuni1245   |            0 |
|          91 | zuni1245   |            0 |
|        2281 | NA         |            1 |
|        2729 | NA         |            0 |

There’s disagreement about the presence of absence of ejectives between
inventories for:

``` r
u.dups %>% filter(Glottocode %in% c('ajab1235', 'basq1248', 'mose1249', 'nort2980', 'port1283', 'stan1288', 'west2368')) %>% arrange(Glottocode) %>% kable() 
```

| InventoryID | Glottocode | has\_uvulars |
| ----------: | :--------- | -----------: |
|         651 | ajab1235   |            2 |
|         652 | ajab1235   |            0 |
|         179 | basq1248   |            1 |
|        2161 | basq1248   |            0 |
|         944 | mose1249   |            1 |
|        1986 | mose1249   |            0 |
|        2030 | nort2980   |            1 |
|        2037 | nort2980   |            1 |
|        2031 | nort2980   |            0 |
|        2206 | port1283   |            1 |
|        2207 | port1283   |            0 |
|        2308 | stan1288   |            1 |
|        2210 | stan1288   |            0 |
|        2622 | west2368   |            2 |
|        2345 | west2368   |            0 |
|        2348 | west2368   |            0 |
|        2618 | west2368   |            0 |

One inventory for ajab1235 lists uvulars the second does not. This may
because these are different dialects with the same Glottocode. It is
clear from the first inventory, and a subsequent publication by Eric
Morey (“A Grammar of Ajagbe”) that it contains two uvulars. We take thus
the first inventory.

  - <https://phoible.org/inventories/view/651>
  - <https://phoible.org/inventories/view/652>

SPA inventory for basq1248 lists a uvular, the other does not. SPA lists
it as a loan, so we take the second inventory.

  - <https://phoible.org/inventories/view/179>
  - <https://phoible.org/inventories/view/2161>

The inventories for Moseten are:

  - <https://phoible.org/inventories/view/944>
  - <https://phoible.org/inventories/view/1986>

The first contains uvular and Sakel (2004:30) states:

    /h/:[X] and [h]
    
    The velar fricative [X] and the uvular fricative [h] form one phoneme /h/. [h] exclusively occurs at the beginning of a syllable.

The IPA chart (page 25) lists glottal fricative /h/ with a footnote (36:
Allophone: \[x\]). I think should be updated to be in line with the
SAPHON interpretation.

The inventories for Quechua include:

  - <https://phoible.org/inventories/view/2030>
  - <https://phoible.org/inventories/view/2037>
  - <https://phoible.org/inventories/view/2031>

The first two indicate one uvular. TODO: what to do?

Portuguese is listed in PHOIBLE twice:

  - <https://phoible.org/inventories/view/2206>
  - <https://phoible.org/inventories/view/2207>

The European dialect is noted as having a uvular. As per Wikipedia:

  - <https://en.wikipedia.org/wiki/Portuguese_phonology>

<!-- end list -->

    The consonant hereafter denoted as /ʁ/ has a variety of realizations depending on dialect. In Europe, it is typically a uvular trill [ʀ]; however, a pronunciation as a voiced uvular fricative [ʁ] may be becoming dominant in urban areas. There is also a realization as a voiceless uvular fricative [χ], and the original pronunciation as an alveolar trill [r] also remains very common in various dialects.[15] A common realization of the word-initial /r/ in the Lisbon accent is a voiced uvular fricative trill [ʀ̝].[16] In Brazil, /ʁ/ can be velar, uvular, or glottal and may be voiceless unless between voiced sounds;[17] it is usually pronounced as a voiceless velar fricative [x], a voiceless glottal fricative [h] or voiceless uvular fricative [χ]. See also Guttural R in Portuguese.

Spanish also has two inventories:

  - <https://phoible.org/inventories/view/2308>
  - <https://phoible.org/inventories/view/2210>

One with uvular (the Castilian variety).

Lastly, different dialects of Western Balochi:

  - <https://phoible.org/inventories/view/2622>
  - <https://phoible.org/inventories/view/2345>
  - <https://phoible.org/inventories/view/2348>
  - <https://phoible.org/inventories/view/2618>

the first reports a uvular, the others do not. TODO: what to do here?

## Formulate the traits file
