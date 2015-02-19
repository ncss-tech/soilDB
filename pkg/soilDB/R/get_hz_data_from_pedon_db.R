# ISSUES:
# 1. working with MS Access-SQL is a serious limitation
# 2. joining to tables with 1:many cardinality will result in multiple rows/horizon
# 3. this can be overcome using CTE queries to local NASIS, but not Access
# 4. cases where this happens:
#    - multiple textures defined for a single horizon-- currently texture is not returned, see NASIS version for 50% fix
#    - multiple lab sample numbers in phsample

get_hz_data_from_pedon_db <- function(dsn) {
  # must have RODBC installed
  if(!requireNamespace('RODBC'))
    stop('please install the `RODBC` package', call.=FALSE)
  
	q <- "SELECT pedon.peiid, phorizon.phiid, pedon.upedonid as pedon_id, phorizon.hzname, phorizon.hzdept, phorizon.hzdepb,
  phorizon.claytotest AS clay, IIF(IsNULL(phorizon.silttotest), (100 - (phorizon.sandtotest + phorizon.claytotest)), phorizon.silttotest) AS silt, phorizon.sandtotest AS sand, texture, phfield, phnaf, eff.choice AS effervescence, l.labsampnum, IIF(IsNULL(f.total_frags_pct), 0, f.total_frags_pct) AS total_frags_pct, fragvoltot
	FROM (
	(
	(
	pedon INNER JOIN phorizon ON pedon.peiid = phorizon.peiidref)

	LEFT OUTER JOIN (SELECT phiidref, SUM(fragvol) as total_frags_pct FROM phfrags GROUP BY phiidref) as f on phorizon.phiid = f.phiidref)

	LEFT OUTER JOIN (SELECT phiidref, labsampnum FROM phsample) as l ON phorizon.phiid = l.phiidref)
	
	LEFT OUTER JOIN (SELECT * FROM metadata_domain_detail WHERE metadata_domain_detail.domain_id = 1255) AS eff ON phorizon.effclass = eff.choice_id
	
	ORDER BY pedon.upedonid, phorizon.hzdept ASC;"
  
	# setup connection to our pedon database
	channel <- RODBC::odbcConnectAccess2007(dsn, readOnlyOptimize=TRUE)
	
	# exec query
	d <- RODBC::sqlQuery(channel, q, stringsAsFactors=FALSE)
	
	# test for duplicate horizons: due to bugs in our queries that lead to >1 row/hz
	hz.tab <- table(d$phiid)
	dupe.hz <- which(hz.tab > 1)
	dupe.hz.pedon.ids <- d$pedon_id[d$phiid %in% names(hz.tab[dupe.hz])]
	
	if(length(dupe.hz) > 0) {
		message(paste('notice: duplicate horizons in query results, matching pedons:\n', paste(unique(dupe.hz.pedon.ids), collapse=','), sep=''))
	}
	
	
	# next, get horizon textures and condence to 1:1 cardinality with results from above
	q.texture <- "SELECT phorizon.phiid, IIF(IsNULL(tx.choice), til.choice, tx.choice) as texture_class
FROM
(
(
(
phorizon
LEFT OUTER JOIN (SELECT phtiid, phiidref, texcl, lieutex FROM phtexture) as t ON phorizon.phiid = t.phiidref)
LEFT OUTER JOIN (SELECT * FROM metadata_domain_detail WHERE metadata_domain_detail.domain_id = 189) AS tx ON t.texcl = tx.choice_id)
LEFT OUTER JOIN (SELECT * FROM metadata_domain_detail WHERE metadata_domain_detail.domain_id = 192) AS til ON t.lieutex = til.choice_id);"
	
	# exec query
	d.texture <- RODBC::sqlQuery(channel, q.texture, stringsAsFactors=FALSE)
	
	# concat multiple textures/horizon into a single record
	d.texture <- aggregate(texture_class ~ phiid, data=d.texture, FUN=function(x) do.call('paste', as.list(x)))
	
	# join
	d <- join(d, d.texture, by='phiid', type='left')
	
	# close connection
	RODBC::odbcClose(channel)
	
	# done
	return(d)
}

