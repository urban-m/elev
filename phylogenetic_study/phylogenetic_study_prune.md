Phylogenetic study of ejectives and uvulars (presense / absence) in IE
and ST
================
Steven Moran
21 September, 2020

Prune the phylogenies
=====================

Prune the language phylogeneies with the daughter languages that we have
in the traits data. This only needs to be run once (per input source,
i.e. phoible traits and extended phoible traits via Harald Hammarström).

Indo-European
-------------

The phylogeny comes from:

-   Chang W, Cathcart C, Hall D, & Garrett A. 2015. Ancestry-constrained
    phylogenetic analysis supports the Indo-European steppe hypothesis.
    Language, 91(1):194-244.

<!-- -->

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

Sinotibetan (Zhang)
-------------------

Phylogeny comes from:

-   Zhang M, Yan S, Pan W, & Jin L. 2019. Phylogenetic evidence for
    Sino-Tibetan origin in northern China in the Late Neolithic. Nature,
    569, 112-115.

<!-- -->

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

Clean up.

    rm(pr_sum_tree, pr_trees, tree, forest)
