get_hz_data_from_pedon_db <- function(dsn) {
	# this can be optimized
	# RF calculation should be done in  a sub-query
	q <- "SELECT pedon.peiid, phorizon.phiid, pedon.upedonid as pedon_id, phorizon.hzname, phorizon.hzdept, phorizon.hzdepb,
  phorizon.claytotest as clay, phorizon.silttotest as silt, phorizon.sandtotest as sand, IIF(IsNULL(tx.choice), til.choice, tx.choice) as texture_class, phfield, eff.choice AS effervescence, l.labsampnum, IIF(IsNULL(f.total_frags_pct), 0, f.total_frags_pct) AS total_frags_pct
	FROM (
	(
	(
	(
	(
	(pedon INNER JOIN phorizon ON pedon.peiid = phorizon.peiidref) 
	LEFT OUTER JOIN (SELECT phiidref, SUM(fragvol) as total_frags_pct FROM phfrags GROUP BY phiidref) as f on phorizon.phiid = f.phiidref)
	LEFT OUTER JOIN (SELECT phiidref, labsampnum FROM phsample) as l ON phorizon.phiid = l.phiidref)
	LEFT OUTER JOIN (SELECT phtiid, phiidref, texcl, lieutex FROM phtexture) as t ON phorizon.phiid = t.phiidref)
	LEFT OUTER JOIN (SELECT * FROM metadata_domain_detail WHERE metadata_domain_detail.domain_id = 189) AS tx ON t.texcl = tx.choice_id)
	LEFT OUTER JOIN (SELECT * FROM metadata_domain_detail WHERE metadata_domain_detail.domain_id = 192) AS til ON t.lieutex = til.choice_id)
	LEFT OUTER JOIN (SELECT * FROM metadata_domain_detail WHERE metadata_domain_detail.domain_id = 1255) AS eff ON phorizon.effclass = eff.choice_id
	ORDER BY pedon.upedonid, phorizon.hzdept ASC;"
  
	# setup connection to our pedon database
	channel <- odbcConnectAccess(dsn, readOnlyOptimize=TRUE)
	
	# exec query
	d <- sqlQuery(channel, q, stringsAsFactors=FALSE)
	
	# test for duplicate horizons: due to bugs in our queries that lead to >1 row/hz
	hz.tab <- table(d$phiid)
	if(any(hz.tab > 1))
		cat('notice: duplicate horizons in query results\n')
	
	# close connection
	odbcClose(channel)
	
	# done
	return(d)
}

