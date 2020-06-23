# Util functions for pruning phylogenies
# Steven Moran <steven.moran@uzh.ch>

library(phylotools)

PruneTraits <- function(traits, tip.labels) {
  traits.cut <- subset(traits, traits$taxa %in% tip.labels)
  return(traits.cut)
}

PruneSummaryTree <- function(nexus.file, codes, which=c('LanguageName', 'ISO', 'Glottocode')) {
  # Trees have tip labels like "Ache<ache1246|guq>" with language name and Glottolog codes. Take Glottolog code. Return tree.
  tree <- read.nexus(nexus.file)
  switch(which,
         Glottocode = {tree$tip.label <- gsub('(.*)(<)(.*)(\\|)(.*)(>)','\\3', tree$tip.label)},
         ISO = {tree$tip.label <- gsub('(.*)(<)(.*)(\\|)(.*)(>)','\\5', tree$tip.label)},
         LanguageName = {tree$tip.label <- gsub('(.*)(<)(.*)(\\|)(.*)(>)','\\1', tree$tip.label)}
         )
  # Drop tips missing in traits
  tree <- drop.tip(tree, setdiff(tree$tip.label, codes))
  # Remove any remaining duplicates.
  if (any(duplicated(tree$tip.label))) {
    index <- which(duplicated(tree$tip.label))
    tree$tip.label[index] <- "remove"
    tree <- drop.tip(tree, "remove")
  }
  return(tree)
}

PruneForest <- function(nexus.file, codes, which=c('LanguageName', 'ISO', 'Glottocode')) {
  # Load a multipho object and drop some tips.
  forest <- read.nexus(nexus.file)
  forest <- lapply(forest, function(t) {
    switch(which,
           Glottocode = {t$tip.label <- gsub('(.*)(<)(.*)(\\|)(.*)(>)','\\3', t$tip.label)},
           ISO = {t$tip.label <- gsub('(.*)(<)(.*)(\\|)(.*)(>)','\\5', t$tip.label)},
           LanguageName = {t$tip.label <- gsub('(.*)(<)(.*)(\\|)(.*)(>)','\\1', t$tip.label)}
    )
    t <- drop.tip(t, setdiff(t$tip.label, codes))
    if (any(duplicated(t$tip.label))) {
      index <- which(duplicated(t$tip.label))
      t$tip.label[index] <- "remove"
      t <- drop.tip(t, "remove")
    }
    return(t)
  })
  class(forest) <- 'multiPhylo'
  attributes(forest)$TipLabel <- forest[[1]]$tip.label
  return(forest)
}
