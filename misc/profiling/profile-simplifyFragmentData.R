## profile
## http://adv-r.had.co.nz/Profiling.html#measure-perf
## https://cran.r-project.org/web/packages/proftools/index.html

# https://github.com/ncss-tech/soilDB/issues/55

library(proftools)
library(soilDB)


# base table is phorizon so that NULL data can be converted to 0s later
q.rf.data <- "SELECT p.phiid, fragvol, fragsize_l, fragsize_r, fragsize_h, fragshp, fraghard 
FROM ( 
SELECT DISTINCT phiid FROM phorizon_View_1 
) as p
LEFT OUTER JOIN phfrags_View_1 ON p.phiid = phfrags_View_1.phiidref;
"

channel <- RODBC::odbcDriverConnect(connection=getOption('soilDB.NASIS.credentials'))

d <- RODBC::sqlQuery(channel, q.rf.data, stringsAsFactors=FALSE)
d <- uncode(d, stringsAsFactors = FALSE)

RODBC::odbcClose(channel)


system.time(x <- simplifyFragmentData(d, id.var = 'phiid', nullFragsAreZero = TRUE))


# profile
pd <- profileExpr(x <- simplifyFragmentData(d, id.var = 'phiid', nullFragsAreZero = TRUE))

hotPaths(pd)

flatProfile(pd)

callSummary(pd)

par(mar=c(1,1,4,1))
flameGraph(pd, svgfile = 'simplifyFragmentData-flame-graph.svg', cex=0.4)
flameGraph(pd, svgfile = 'simplifyFragmentData-flame-graph-time.svg', order = 'time', cex=0.4)

