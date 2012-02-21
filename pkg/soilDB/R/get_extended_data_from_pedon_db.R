get_extended_data_from_pedon_db <- function(dsn) {
	# query diagnostic horizons, usually a 1:many relationship with pedons
	q.diagnostic <- "SELECT peiidref as peiid, dfk.choice as diag_kind, featdept, featdepb
FROM pediagfeatures
	LEFT OUTER JOIN (SELECT * FROM metadata_domain_detail WHERE domain_id = 147) AS dfk ON pediagfeatures.featkind = dfk.choice_id
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
	q.hz.texmod <- "SELECT phorizon.peiidref, phorizon.phiid, phtexture.phtiid, phtexturemod.seqnum, tmod.choice as texture_modifier 
  FROM (
	(phorizon INNER JOIN phtexture ON phorizon.phiid = phtexture.phiidref) 
	LEFT OUTER JOIN phtexturemod ON phtexture.phtiid = phtexturemod.phtiidref) 
	LEFT OUTER JOIN (SELECT * FROM metadata_domain_detail WHERE metadata_domain_detail.domain_id = 190) AS tmod ON phtexturemod.texmod =   tmod.choice_id;"
	
	## TODO: does this query need a DISTINCT keyword ?
	# get geomorphic features
	q.geomorph <- "SELECT pedon.peiid, sitegeomordesc.geomfeatid, sitegeomordesc.existsonfeat, 	sitegeomordesc.geomfmod, geomorfeat.geomfname, IIf(([sitegeomordesc].[geomftiidref]=1),'landform',IIf(([sitegeomordesc].[geomftiidref]=2),'landscape',IIf(([sitegeomordesc].[geomftiidref]=3),'microfeature','anthropogenic feature'))) AS feature_type
FROM (geomorfeat INNER JOIN (site INNER JOIN sitegeomordesc ON site.siteiid = sitegeomordesc.siteiidref) ON (geomorfeat.geomftdbsidref = sitegeomordesc.geomftdbsidref) AND (geomorfeat.geomftiidref = sitegeomordesc.geomftiidref) AND (geomorfeat.geomfiid = sitegeomordesc.geomfiidref)) INNER JOIN pedon ON site.siteiid = pedon.siteiidref 
ORDER BY peiid ASC;"
  
	
	# setup connection to our pedon database
	channel <- odbcConnectAccess(dsn, readOnlyOptimize=TRUE)
	
	# exec queries
	d.diagnostic <- sqlQuery(channel, q.diagnostic, stringsAsFactors=FALSE)
	d.rf.summary <- sqlQuery(channel, q.rf.summary, stringsAsFactors=FALSE)
	d.hz.texmod <- sqlQuery(channel, q.hz.texmod, stringsAsFactors=FALSE)
	d.geomorph <- sqlQuery(channel, q.geomorph, stringsAsFactors=FALSE)
	
	# close connection
	odbcClose(channel)
	
	# generate wide-formatted, diagnostic boolean summary
	d.diag.boolean <- diagHzLongtoWide(d.diagnostic)
	
	# return a list of results
	return(list(diagnostic=d.diagnostic, diagHzBoolean=d.diag.boolean, frag_summary=d.rf.summary, texmodifier=d.hz.texmod, geomorph=d.geomorph))
}
