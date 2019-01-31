library(proftools)
library(profmem)
library(soilDB)

# https://github.com/ncss-tech/soilDB/issues/55

## note: it is not clear if unexported functions like .formatLandformString() are named in the results



## notes:
# http://adv-r.had.co.nz/Profiling.html#measure-perf
# https://cran.r-project.org/web/packages/proftools/index.html

## system.time(x <- fetchNASIS(from='pedons'))
#
# ~ 4k pedons in local database SS => 80 seconds (slow simplifyColorData)
# ~ 4k pedons in local database SS => 44 seconds (fast simplifyColorData)
# ~ 4k pedons in local database SS => 32 seconds (lapply() .pickBestTaxHistory)
# ~ 4k pedons in local database SS => 30 seconds (stringsAsFactors=FALSE for taxhistory uncode())
# ~ 4k pedons in local database SS => 30 seconds (lapply() .formatLandformString)

## profile
pd <- profileExpr(x <- fetchNASIS(from='pedons'))

hotPaths(pd)

flatProfile(pd)

callSummary(pd)

par(mar=c(1,1,4,1))
flameGraph(pd, svgfile = 'fetchNASIS_pedons-flame-graph.svg', cex=0.4)
flameGraph(pd, svgfile = 'fetchNASIS_pedons-flame-graph-time.svg', order = 'time', cex=0.4)


## object size
# ~ 13Mb for stringsAsFactors = TRUE|FALSE

## RAM usage?
# https://cran.r-project.org/web/packages/profmem/vignettes/profmem.html

# hmm, how does this work
# p <- profmem(x <- fetchNASIS(from='pedons'), threshold = 2e5)


