Phylogenetic study of ejectives and uvulars (presense / absence) in IE
and ST
================
Steven Moran
29 June, 2020

# Prune the phylogenies

Prune the language phylogeneies with the daughter languages that we have
in the traits data. This only needs to be run once.

## Indo-European

``` r
# Tree paths
tree <- 'trees/indoeuropean-C-merged.nex'
forest <- 'trees/indoeuropean-C-20k.nex'

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

## Sinotibetan (Zhang)

``` r
# Tree paths
tree <- 'trees/sinotibetan-zhang-merged.nex'
forest <- 'trees/sinotibetan-zhang-10k.nex'

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

## Utoaztecan

TODO: something about this tree doesn’t work (not dated?).

``` r
# Tree paths
# tree <- 'trees/utoaztecan-merged.nex'
# forest <- 'trees/utoaztecan-800.nex'

# Prune the trees
# pr_sum_tree <- PruneSummaryTree(tree, traits$taxa, 'Glottocode')
# pr_trees <- PruneForest(forest, traits$taxa, 'Glottocode')

# Check that the summary tree tip labels are the same as (the first) posterior tree tip labels
# expect_true(setequal(pr_sum_tree$tip.label, pr_trees$STATE_0$tip.label))

# Prune the traits data to match the tree tips for analysis
# data <- PruneTraits(traits, pr_sum_tree$tip.label)

# Combine them into a list of R data objects for analysis with the BT3 wrapper
# pr_sum_tree <- list(data=data, tree=pr_sum_tree)
# pr_trees <- list(data=data, tree=pr_trees)
# save(pr_sum_tree, pr_trees, file=paste0(.PathToPrunedTrees, 'sinotibetan-z-trees.Rdata'))
```

Clean up.

``` r
rm(pr_sum_tree, pr_trees, tree, forest)
```