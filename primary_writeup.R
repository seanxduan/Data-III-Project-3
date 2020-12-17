## ----setup--------------------------------------------------------------------
# Seed for random number generation
set.seed(42)
knitr::opts_chunk$set(cache.extra = knitr::rand_seed)
knitr::knit_hooks$set(purl = knitr::hook_purl)

