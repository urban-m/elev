Prune the IE and ST phylogenies with PHOIBLE data
================
Steven Moran

08 January, 2021

# Overview

We use [R](https://www.r-project.org/) (R Core Team 2020) and the
following [R
packages](https://cran.r-project.org/web/packages/available_packages_by_name.html)
(Wickham 2011; J. Zhang 2017) in this script:

``` r
library(testthat)
library(phylotools)
source('lib/functions.R')

# Choose which traits to use to prune:
# load("trees/traits.Rdata")
load("../Harald/traits-harald.Rdata")
```

We prune the language phylogeneies in our sample with the daughter
languages that we have in the traits data that we identified in PHOIBLE
(Moran and McCloy 2019). This only needs to be run once (per input
source, i.e., phoible traits and extended phoible traits via data mining
from Harald Hammarström; see main text in the paper for details).

## Indo-European

The Indo-European phylogeny was published by Chang et al. (2015) and is
available in [D-PLACE](https://github.com/D-PLACE/dplace-data) (Kirby et
al. 2016).

``` r
# Tree paths
tree <- 'trees/ie-c-tree.nex'
forest <- 'trees/ie-c-trees.nex'

# Prune the trees
pr_sum_tree <- PruneSummaryTree(tree, traits$taxa, 'Glottocode')
pr_trees <- PruneForest(forest, traits$taxa, 'Glottocode')

# Check that the summary tree tip labels are the same as (the first) posterior tree tip labels
expect_true(setequal(pr_sum_tree$tip.label, pr_trees$STATE_0$tip.label))

# Prune the traits data to match the tree tips for analysis
data <- PruneTraits(traits, pr_sum_tree$tip.label)

# Combine them into a list of R data objects for analysis with the BT3 wrapper
pr_sum_tree <- list(data=data, tree=pr_sum_tree)
pr_trees <- list(data=data, tree=pr_trees)
# save(pr_sum_tree, pr_trees, file='trees/ie-c-trees.Rdata')
save(pr_sum_tree, pr_trees, file='trees/harald-ie-c-trees.Rdata')
```

## Sino-Tibetan

The Sino-Tibetan phylogeny was published by M. Zhang et al. (2019) and
is available in [D-PLACE](https://github.com/D-PLACE/dplace-data) (Kirby
et al. 2016).

``` r
# Tree paths
tree <- 'trees/sinotibetan-z-tree.nex'
forest <- 'trees/sinotibetan-z-trees.nex'

# Prune the trees
pr_sum_tree <- PruneSummaryTree(tree, traits$taxa, 'Glottocode')
pr_trees <- PruneForest(forest, traits$taxa, 'Glottocode')

# Check that the summary tree tip labels are the same as (the first) posterior tree tip labels
expect_true(setequal(pr_sum_tree$tip.label, pr_trees$STATE_0$tip.label))

# Prune the traits data to match the tree tips for analysis
data <- PruneTraits(traits, pr_sum_tree$tip.label)

# Combine them into a list of R data objects for analysis with the BT3 wrapper
pr_sum_tree <- list(data=data, tree=pr_sum_tree)
pr_trees <- list(data=data, tree=pr_trees)
# save(pr_sum_tree, pr_trees, file='trees/sinotibetan-z-trees.Rdata')
save(pr_sum_tree, pr_trees, file='trees/harald-sinotibetan-z-trees.Rdata')
```

Clean up.

``` r
rm(pr_sum_tree, pr_trees, tree, forest)
```

# References

<div id="refs" class="references csl-bib-body hanging-indent">

<div id="ref-Changetal2015Ancestry-constrained" class="csl-entry">

Chang, Will, Chundra Cathcart, David Hall, and Andrew Garrett. 2015.
“Ancestry-Constrained Phylogenetic Analysis Supports Indo-European
Steppe Hypothesis.” *Language* 91: 194–244.
<https://doi.org/10.1353/lan.2015.0005>.

</div>

<div id="ref-kirby2016d" class="csl-entry">

Kirby, Kathryn R., Russell D. Gray, Simon J. Greenhill, Fiona M. Jordan,
Stephanie Gomes-Ng, Hans-Jörg Bibiko, Damián E. Blasi, et al. 2016.
“D-PLACE: A Global Database of Cultural, Linguistic and Environmental
Diversity.” *PLoS ONE* 11 (7): e0158391.

</div>

<div id="ref-MoranMcCloy2019" class="csl-entry">

Moran, Steven, and Daniel McCloy, eds. 2019. *PHOIBLE 2.0*. Jena: Max
Planck Institute for the Science of Human History.
<https://doi.org/10.5281/zenodo.2562766>.

</div>

<div id="ref-R" class="csl-entry">

R Core Team. 2020. *R: A Language and Environment for Statistical
Computing*. Vienna, Austria: R Foundation for Statistical Computing.
<https://www.R-project.org/>.

</div>

<div id="ref-testthat" class="csl-entry">

Wickham, Hadley. 2011. “Testthat: Get Started with Testing.” *The R
Journal* 3: 5–10.
<https://journal.r-project.org/archive/2011-1/RJournal_2011-1_Wickham.pdf>.

</div>

<div id="ref-phylotools" class="csl-entry">

Zhang, Jinlong. 2017. *Phylotools: Phylogenetic Tools for
Eco-Phylogenetics*. <https://CRAN.R-project.org/package=phylotools>.

</div>

<div id="ref-Zhang2019" class="csl-entry">

Zhang, Menghan, Shi Yan, Wuyun Pan, and Li Jin. 2019. “Phylogenetic
Evidence for Sino-Tibetan Origin in Northern China in the Late
Neolithic.” *Nature* 569 (7754): 112–15.
<https://doi.org/10.1038/s41586-019-1153-z>.

</div>

</div>
