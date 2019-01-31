## profile
## http://adv-r.had.co.nz/Profiling.html#measure-perf
## https://cran.r-project.org/web/packages/proftools/index.html

# https://github.com/ncss-tech/soilDB/issues/88

library(proftools)
library(soilDB)


# unique-ness enforced via peiid (pedon-level) and phiid (horizon-level)
q <- "SELECT peiid, phiid, colormoistst, colorpct as pct, colorhue, colorvalue, colorchroma
  FROM
  pedon_View_1 
  INNER JOIN phorizon_View_1 ON pedon_View_1.peiid = phorizon_View_1.peiidref
  INNER JOIN phcolor_View_1 ON phorizon_View_1.phiid = phcolor_View_1.phiidref
  ORDER BY phiid, colormoistst;"

# setup connection local NASIS
channel <- RODBC::odbcDriverConnect(connection=getOption('soilDB.NASIS.credentials'))

# exec query
d <- RODBC::sqlQuery(channel, q, stringsAsFactors=FALSE)

# close connection
RODBC::odbcClose(channel)

# uncode domained columns
d <- uncode(d)

# convert back to characters / numeric
d$colormoistst <- as.character(d$colormoistst)
d$colorhue <- as.character(d$colorhue)
# careful!
# uncode creates factors, so we have to convert to character first
d$colorvalue <- as.numeric(as.character(d$colorvalue))
d$colorchroma <- as.numeric(as.character(d$colorchroma))



####
####
####


# plyr-based approach, rgb2munsell() inside groups: ~ 4k pedons => 45 seconds
# plyr-based approach, rgb2munsell() outside groups: ~ 4k pedons => 10 seconds
system.time(x <- simplifyColorData(d))


# profile
pd <- profileExpr(x <- simplifyColorData(d))

hotPaths(pd)

flatProfile(pd)

callSummary(pd)

par(mar=c(1,1,4,1))
flameGraph(pd, svgfile = 'simplifyColorData-plyr-flame-graph.svg', cex=0.4)
flameGraph(pd, svgfile = 'simplifyColorData-plyr-flame-graph-time.svg', order = 'time', cex=0.4)

