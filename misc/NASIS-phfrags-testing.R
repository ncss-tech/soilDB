
##
## 
##

library(soilDB)

e <- get_extended_data_from_NASIS_db()

## R-based approach, re-write of original SQL by Dylan and Jay
rf <- e$frag_summary

## SQL-based approach c/o Stephen
rf.sql <- e$frag_summary_v2


head(rf)
head(rf.sql)

setdiff(names(rf), names(rf.sql))

nrow(rf)
nrow(rf.sql)

g <- plyr::join(rf, rf.sql, by='phiid', type='inner')


plot(g[, 3], g[, 20])
plot(g[, 4], g[, 21])



x <- fetchNASIS()
horizons(x)


x <- fetchNASIS(nullFragsAreZero=FALSE)
horizons(x)





channel <- RODBC::odbcDriverConnect(connection=getOption('soilDB.NASIS.credentials'))
d <- RODBC::sqlQuery(channel, q.rf.data, stringsAsFactors=FALSE)
d    <- uncode(d, stringsAsFactors = stringsAsFactors)
# close connection
RODBC::odbcClose(channel)

dput(d)


simplifyFragmentData(d, id.var = 'phiid', nullFragsAreZero = TRUE)

simplifyFragmentData(d, id.var = 'phiid', nullFragsAreZero = FALSE)


