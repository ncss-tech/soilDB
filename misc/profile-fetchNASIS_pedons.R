library(proftools)
library(soilDB)

## profile
## http://adv-r.had.co.nz/Profiling.html#measure-perf
## https://cran.r-project.org/web/packages/proftools/index.html
# profile
pd <- profileExpr(x <- fetchNASIS(from='pedons'))

hotPaths(pd)

flatProfile(pd)

callSummary(pd)

par(mar=c(1,1,4,1))
flameGraph(pd, svgfile = 'fetchNASIS_pedons-flame-graph.svg', cex=0.4)
flameGraph(pd, svgfile = 'fetchNASIS_pedons-flame-graph-time.svg', order = 'time', cex=0.4)

