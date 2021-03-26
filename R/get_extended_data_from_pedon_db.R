# TODO: does not have parity with extended data function pulling from NASIS
# missing queries for veg, ecosite, rf.data, surf.rf.summary, photolink, sitepm, structure



#' Extract accessory tables and summaries from a local pedonPC Database
#' 
#' Extract accessory tables and summaries from a local pedonPC Database.
#' 
#' This function currently works only on Windows.
#' 
#' @param dsn The path to a 'pedon.mdb' database.
#' @return A list with the results.
#' @author Jay M. Skovlin and Dylan E. Beaudette
#' @seealso \code{\link{get_hz_data_from_pedon_db}},
#' \code{\link{get_site_data_from_pedon_db}}
#' @keywords manip
#' @export get_extended_data_from_pedon_db
get_extended_data_from_pedon_db <- function(dsn) {
  # must have odbc installed
  if(!requireNamespace('odbc'))
    stop('please install the `odbc` package', call.=FALSE)
  
	# query diagnostic horizons, usually a 1:many relationship with pedons
	q.diagnostic <- "SELECT peiidref as peiid, featkind, featdept, featdepb
FROM pediagfeatures
	ORDER BY pediagfeatures.peiidref, pediagfeatures.featdept;"
	
	
	# this query is resistant to dupes
	# query rock-fragment summary by horizon
	q.rf.summary <- "SELECT DISTINCT phfrags.phiidref as phiid, 
IIF(IsNULL(f1_gr.gravel), 0.0, f1_gr.gravel) as gravel, 
IIF(IsNULL(f1_pgr.gravel), 0.0, f1_pgr.gravel) as paragravel, 
IIF(IsNULL(f2_cb.cobbles), 0.0, f2_cb.cobbles) as cobbles, 
IIF(IsNULL(f2_pcb.cobbles), 0.0, f2_pcb.cobbles) as paracobbles,
IIF(IsNULL(f3.stones), 0.0, f3.stones) as stones, 
IIF(IsNULL(f4.boulders), 0.0, f4.boulders) as boulders, 
IIF(IsNULL(f5.channers), 0.0, f5.channers) as channers, 
IIF(IsNULL(f6.flagstones), 0.0, f6.flagstones) as flagstones
FROM ((((((((
phfrags	
LEFT OUTER JOIN (
	SELECT phfrags.phiidref, Sum(phfrags.fragvol) AS gravel
	FROM phfrags
LEFT OUTER JOIN (
		SELECT * FROM metadata_domain_detail WHERE metadata_domain_detail.domain_id = 173) AS m ON phfrags.fraghard = m.choice_id
		WHERE ((phfrags.fragsize_h <= 76) OR (phfrags.fragsize_r <= 76 And phfrags.fragsize_r >= 2))
		AND (m.choice IN ('strongly', 'very strongly', 'indurated') OR m.choice IS NULL)
		GROUP BY phfrags.phiidref
	) as f1_gr ON phfrags.phiidref = f1_gr.phiidref)
LEFT OUTER JOIN (
		SELECT phfrags.phiidref, Sum(phfrags.fragvol) AS gravel
		FROM phfrags
		LEFT OUTER JOIN (SELECT * FROM metadata_domain_detail WHERE metadata_domain_detail.domain_id = 173) AS m ON phfrags.fraghard = m.choice_id
		WHERE (phfrags.fragsize_h <= 76 OR (phfrags.fragsize_r <= 76 And phfrags.fragsize_r >= 2))
		AND m.choice NOT IN ('strongly', 'very strongly', 'indurated')
		GROUP BY phfrags.phiidref
	) as f1_pgr ON phfrags.phiidref = f1_pgr.phiidref)
LEFT OUTER JOIN (
		SELECT phfrags.phiidref, Sum(phfrags.fragvol) AS cobbles
		FROM phfrags
		LEFT OUTER JOIN (SELECT * FROM metadata_domain_detail WHERE metadata_domain_detail.domain_id = 173) AS m ON phfrags.fraghard = m.choice_id
		WHERE ((phfrags.fragsize_l >= 75 AND phfrags.fragsize_h <= 250) OR (phfrags.fragsize_r >= 76 And phfrags.fragsize_r <= 250))
		AND (m.choice IN ('strongly', 'very strongly', 'indurated') OR m.choice IS NULL)
		GROUP BY phfrags.phiidref
	) as f2_cb ON phfrags.phiidref = f2_cb.phiidref)
LEFT OUTER JOIN (
		SELECT phfrags.phiidref, Sum(phfrags.fragvol) AS cobbles
		FROM phfrags
		LEFT OUTER JOIN (SELECT * FROM metadata_domain_detail WHERE metadata_domain_detail.domain_id = 173) AS m ON phfrags.fraghard = m.choice_id
		WHERE ((phfrags.fragsize_l >= 75 AND phfrags.fragsize_h <= 250) OR (phfrags.fragsize_r >= 76 And phfrags.fragsize_r <= 250))
		AND m.choice NOT IN ('strongly', 'very strongly', 'indurated')
		GROUP BY phfrags.phiidref
	) as f2_pcb ON phfrags.phiidref = f2_pcb.phiidref)
LEFT OUTER JOIN (
		SELECT phfrags.phiidref, Sum(phfrags.fragvol) AS stones
		FROM phfrags
		LEFT OUTER JOIN (SELECT * FROM metadata_domain_detail WHERE metadata_domain_detail.domain_id = 173) AS m ON phfrags.fraghard = m.choice_id
		WHERE (((phfrags.fragsize_l)>=250) OR ((phfrags.fragsize_r)>=250) OR (((phfrags.fragsize_l)>=380) AND (phfrags.fragshp)=1) OR   		(((phfrags.fragsize_r)>=380) AND ((phfrags.fragshp)=1)))
		GROUP BY phfrags.phiidref
	) as f3 ON phfrags.phiidref = f3.phiidref)
LEFT OUTER JOIN (
		SELECT phfrags.phiidref, Sum(phfrags.fragvol) AS boulders
		FROM phfrags
		LEFT OUTER JOIN (SELECT * FROM metadata_domain_detail WHERE metadata_domain_detail.domain_id = 173) AS m ON phfrags.fraghard = m.choice_id
		WHERE (((phfrags.fragsize_l)>=600) OR (((phfrags.fragsize_r)>=600)  AND ((phfrags.fragshp)=1)))
		GROUP BY phfrags.phiidref
	) as f4 ON phfrags.phiidref = f4.phiidref)
LEFT OUTER JOIN (
		SELECT phfrags.phiidref, Sum(phfrags.fragvol) AS channers
		FROM phfrags
		LEFT OUTER JOIN (SELECT * FROM metadata_domain_detail WHERE metadata_domain_detail.domain_id = 173) AS m ON phfrags.fraghard = m.choice_id
		WHERE (((phfrags.fragsize_l)>=2) AND ((phfrags.fragsize_h)<=150) AND ((phfrags.fragshp)=1)) OR (((phfrags.fragshp)=1) AND   ((phfrags.fragsize_r)>=2 And (phfrags.fragsize_r)<=150))
		GROUP BY phfrags.phiidref
	) as f5 ON phfrags.phiidref = f5.phiidref)
LEFT OUTER JOIN (
		SELECT phfrags.phiidref, Sum(phfrags.fragvol) AS flagstones
		FROM phfrags
		LEFT OUTER JOIN (SELECT * FROM metadata_domain_detail WHERE metadata_domain_detail.domain_id = 173) AS m ON phfrags.fraghard = m.choice_id
		WHERE (((phfrags.fragsize_l)>=150) AND ((phfrags.fragsize_h)<=380) AND ((phfrags.fragshp)=1)) OR (((phfrags.fragshp)=1) AND   ((phfrags.fragsize_r)>=150 And (phfrags.fragsize_r)<=380))
		GROUP BY phfrags.phiidref
	) as f6 ON phfrags.phiidref = f6.phiidref)	
	ORDER BY phfrags.phiidref;"

	
	# get horizon texture modifiers
	q.hz.texmod <- "SELECT phorizon.peiidref, phorizon.phiid, phtexture.phtiid, phtexturemod.seqnum, texmod 
  FROM 
	((phorizon INNER JOIN phtexture ON phorizon.phiid = phtexture.phiidref) 
	LEFT OUTER JOIN phtexturemod ON phtexture.phtiid = phtexturemod.phtiidref);"

	
	# get geomorphic features
	##changes made: added join to peiid through the siteobs table, change to key field linkages for geomftiidref - key field was dropped from sitegeomordesc table
	q.geomorph <- "SELECT pedon.peiid, sitegeomordesc.geomfmod, geomorfeat.geomfname, sitegeomordesc.geomfeatid, sitegeomordesc.existsonfeat, sitegeomordesc.geomfiidref, lcase(geomorfeattype.geomftname) as geomftname
FROM geomorfeattype RIGHT JOIN (geomorfeat RIGHT JOIN ((site INNER JOIN sitegeomordesc ON site.siteiid = sitegeomordesc.siteiidref) INNER JOIN (siteobs INNER JOIN pedon ON siteobs.siteobsiid = pedon.siteobsiidref) ON site.siteiid = siteobs.siteiidref) ON geomorfeat.geomfiid = sitegeomordesc.geomfiidref) ON geomorfeattype.geomftiid = geomorfeat.geomftiidref ORDER BY peiid, geomfeatid ASC"
	

	# get petaxhistory data 
	q.taxhistory <- "SELECT peiidref as peiid, classdate, classifier, classtype, taxonname, taxonkind, seriesstatus, taxpartsize, taxorder, taxsuborder, taxgrtgroup, taxsubgrp, soiltaxedition, osdtypelocflag
  	FROM petaxhistory 
	ORDER BY petaxhistory.peiidref;"	

	# setup connection to our pedon database
	channel <- DBI::dbConnect(odbc::odbc(), .connection_string = paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=", dsn))
	
	# exec queries
	d.diagnostic <- DBI::dbGetQuery(channel, q.diagnostic)
	d.rf.summary <- DBI::dbGetQuery(channel, q.rf.summary)
	d.hz.texmod <- DBI::dbGetQuery(channel, q.hz.texmod)
	d.geomorph <- DBI::dbGetQuery(channel, q.geomorph)
	d.taxhistory <- DBI::dbGetQuery(channel, q.taxhistory)

	# close connection
	DBI::dbDisconnect(channel)

	## uncode the one that need that here
	d.diagnostic <- uncode(d.diagnostic)
	d.hz.texmod <- uncode(d.hz.texmod)
	d.taxhistory <- uncode(d.taxhistory)
	# d.sitepm <- uncode(d.sitepm)
	# d.structure <- uncode(d.structure)
	
	# generate wide-formatted, diagnostic boolean summary
	d.diag.boolean <- .diagHzLongtoWide(d.diagnostic)
	
	# return a list of results
	return(list(diagnostic=d.diagnostic, diagHzBoolean=d.diag.boolean, frag_summary=d.rf.summary, texmodifier=d.hz.texmod, geomorph=d.geomorph, taxhistory=d.taxhistory))
}
