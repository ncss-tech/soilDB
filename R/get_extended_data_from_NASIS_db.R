## TODO: need to be careful about how l,rv,h are used here...

get_extended_data_from_NASIS_db <- function(dsn) {
	# query diagnostic horizons, usually a 1:many relationship with pedons
	q.diagnostic <- "SELECT peiidref as peiid, dfk.ChoiceName as diag_kind, featdept, featdepb
FROM dbo.pediagfeatures 
	LEFT OUTER JOIN (SELECT * FROM dbo.MetadataDomainDetail WHERE DomainID = 147) AS dfk ON dbo.pediagfeatures.featkind = dfk.ChoiceValue
	ORDER BY dbo.pediagfeatures.peiidref, dbo.pediagfeatures.featdept;"
  
	
	# this query is resistant to dupes
	# query rock-fragment summary by horizon
	q.rf.summary <- "SELECT DISTINCT dbo.phfrags.phiidref as phiid, 

CASE WHEN f1_gr.gravel IS NULL THEN 0.0 ELSE f1_gr.gravel END as gravel, 
CASE WHEN f1_pgr.gravel IS NULL THEN 0.0 ELSE f1_pgr.gravel END as paragravel,
CASE WHEN f2_cb.cobbles IS NULL THEN 0.0 ELSE f2_cb.cobbles END as cobbles, 
CASE WHEN f2_pcb.cobbles IS NULL THEN 0.0 ELSE f2_pcb.cobbles END as paracobbles,
CASE WHEN f3.stones IS NULL THEN 0.0 ELSE f3.stones END as stones, 
CASE WHEN f4.boulders IS NULL THEN 0.0 ELSE f4.boulders END as boulders, 
CASE WHEN f5.channers IS NULL THEN 0.0 ELSE f5.channers END as channers, 
CASE WHEN f6.flagstones IS NULL THEN 0.0 ELSE f6.flagstones END as flagstones

FROM ((((((((
dbo.phfrags
LEFT OUTER JOIN (
		SELECT dbo.phfrags.phiidref, Sum(dbo.phfrags.fragvol) AS gravel
		FROM dbo.phfrags
		LEFT OUTER JOIN (SELECT * FROM dbo.MetadataDomainDetail WHERE dbo.MetadataDomainDetail.DomainID = 173) AS m ON phfrags.fraghard = m.ChoiceValue
		WHERE ((dbo.phfrags.fragsize_h <= 76) OR (dbo.phfrags.fragsize_r <= 76 And dbo.phfrags.fragsize_r >= 2))
		AND (m.ChoiceName IN ('strongly', 'very strongly', 'indurated') OR m.ChoiceName IS NULL)
		GROUP BY dbo.phfrags.phiidref
	) as f1_gr ON dbo.phfrags.phiidref = f1_gr.phiidref)

LEFT OUTER JOIN (
		SELECT dbo.phfrags.phiidref, Sum(dbo.phfrags.fragvol) AS gravel
		FROM dbo.phfrags
		LEFT OUTER JOIN (SELECT * FROM dbo.MetadataDomainDetail WHERE dbo.MetadataDomainDetail.DomainID = 173) AS m ON phfrags.fraghard = m.ChoiceValue
		WHERE (dbo.phfrags.fragsize_h <= 76 OR (dbo.phfrags.fragsize_r <= 76 And dbo.phfrags.fragsize_r >= 2))
		AND m.ChoiceName NOT IN ('strongly', 'very strongly', 'indurated')
		GROUP BY dbo.phfrags.phiidref
	) as f1_pgr ON dbo.phfrags.phiidref = f1_pgr.phiidref)

	LEFT OUTER JOIN (
		SELECT dbo.phfrags.phiidref, Sum(dbo.phfrags.fragvol) AS cobbles
		FROM dbo.phfrags
		LEFT OUTER JOIN (SELECT * FROM dbo.MetadataDomainDetail WHERE dbo.MetadataDomainDetail.DomainID = 173) AS m ON phfrags.fraghard = m.ChoiceValue
		WHERE ((dbo.phfrags.fragsize_l >= 75 AND dbo.phfrags.fragsize_h <= 250) OR (dbo.phfrags.fragsize_r >= 76 And dbo.phfrags.fragsize_r <= 250))
		AND (m.ChoiceName IN ('strongly', 'very strongly', 'indurated') OR m.ChoiceName IS NULL)
		GROUP BY dbo.phfrags.phiidref
	) as f2_cb ON dbo.phfrags.phiidref = f2_cb.phiidref)

	LEFT OUTER JOIN (
		SELECT dbo.phfrags.phiidref, Sum(dbo.phfrags.fragvol) AS cobbles
		FROM dbo.phfrags
		LEFT OUTER JOIN (SELECT * FROM dbo.MetadataDomainDetail WHERE dbo.MetadataDomainDetail.DomainID = 173) AS m ON phfrags.fraghard = m.ChoiceValue
		WHERE ((dbo.phfrags.fragsize_l >= 75 AND dbo.phfrags.fragsize_h <= 250) OR (dbo.phfrags.fragsize_r >= 76 And dbo.phfrags.fragsize_r <= 250))
		AND m.ChoiceName NOT IN ('strongly', 'very strongly', 'indurated')
		GROUP BY dbo.phfrags.phiidref
	) as f2_pcb ON dbo.phfrags.phiidref = f2_pcb.phiidref)

	LEFT OUTER JOIN (
		SELECT dbo.phfrags.phiidref, Sum(dbo.phfrags.fragvol) AS stones
		FROM dbo.phfrags
		LEFT OUTER JOIN (SELECT * FROM dbo.MetadataDomainDetail WHERE dbo.MetadataDomainDetail.DomainID = 173) AS m ON phfrags.fraghard = m.ChoiceValue
		WHERE (((dbo.phfrags.fragsize_l)>=250) OR ((dbo.phfrags.fragsize_r)>=250) OR (((dbo.phfrags.fragsize_l)>=380) AND (dbo.phfrags.fragshp)=1) OR   (((dbo.phfrags.fragsize_r)>=380) AND ((dbo.phfrags.fragshp)=1)))
		GROUP BY dbo.phfrags.phiidref
	) as f3 ON dbo.phfrags.phiidref = f3.phiidref)

	LEFT OUTER JOIN (
		SELECT dbo.phfrags.phiidref, Sum(dbo.phfrags.fragvol) AS boulders
		FROM dbo.phfrags
		LEFT OUTER JOIN (SELECT * FROM dbo.MetadataDomainDetail WHERE dbo.MetadataDomainDetail.DomainID = 173) AS m ON phfrags.fraghard = m.ChoiceValue
		WHERE (((dbo.phfrags.fragsize_l)>=600) OR (((dbo.phfrags.fragsize_r)>=600)  AND ((dbo.phfrags.fragshp)=1)))
		GROUP BY dbo.phfrags.phiidref
	) as f4 ON dbo.phfrags.phiidref = f4.phiidref)

	LEFT OUTER JOIN (
		SELECT dbo.phfrags.phiidref, Sum(dbo.phfrags.fragvol) AS channers
		FROM dbo.phfrags
		LEFT OUTER JOIN (SELECT * FROM dbo.MetadataDomainDetail WHERE dbo.MetadataDomainDetail.DomainID = 173) AS m ON phfrags.fraghard = m.ChoiceValue
		WHERE (((dbo.phfrags.fragsize_l)>=2) AND ((dbo.phfrags.fragsize_h)<=150) AND ((dbo.phfrags.fragshp)=1)) OR (((dbo.phfrags.fragshp)=1) AND   ((dbo.phfrags.fragsize_r)>=2 And (dbo.phfrags.fragsize_r)<=150))
		GROUP BY dbo.phfrags.phiidref
	) as f5 ON dbo.phfrags.phiidref = f5.phiidref)
	
	LEFT OUTER JOIN (
		SELECT dbo.phfrags.phiidref, Sum(dbo.phfrags.fragvol) AS flagstones
		FROM dbo.phfrags
		LEFT OUTER JOIN (SELECT * FROM dbo.MetadataDomainDetail WHERE dbo.MetadataDomainDetail.DomainID = 173) AS m ON phfrags.fraghard = m.ChoiceValue
		WHERE (((dbo.phfrags.fragsize_l)>=150) AND ((dbo.phfrags.fragsize_h)<=380) AND ((dbo.phfrags.fragshp)=1)) OR (((dbo.phfrags.fragshp)=1) AND   ((dbo.phfrags.fragsize_r)>=150 And (dbo.phfrags.fragsize_r)<=380))
		GROUP BY dbo.phfrags.phiidref
	) as f6 ON dbo.phfrags.phiidref = f6.phiidref)
	
	ORDER BY dbo.phfrags.phiidref;"

	# get horizon texture modifiers
	q.hz.texmod <- "SELECT dbo.phorizon.peiidref AS peiid, dbo.phorizon.phiid AS phiid, dbo.phtexture.phtiid AS phtiid, dbo.phtexturemod.seqnum, tmod.ChoiceName as texture_modifier 
  FROM (
	(dbo.phorizon INNER JOIN dbo.phtexture ON dbo.phorizon.phiid = dbo.phtexture.phiidref) 
	LEFT OUTER JOIN dbo.phtexturemod ON dbo.phtexture.phtiid = dbo.phtexturemod.phtiidref) 
	LEFT OUTER JOIN (SELECT * FROM dbo.MetadataDomainDetail WHERE dbo.MetadataDomainDetail.DomainID = 190) AS tmod ON dbo.phtexturemod.texmod =   tmod.ChoiceValue;"
	
	# get geomorphic features
	q.geomorph <- "SELECT pedon.peiid, sitegeomordesc.sitegeomdiid, sitegeomordesc.geomfeatid, sitegeomordesc.existsonfeat, sitegeomordesc.geomfmod, geomorfeat.geomfname, CASE WHEN dbo.sitegeomordesc.geomfeatid=1 THEN 'landform' WHEN dbo.sitegeomordesc.geomfeatid=2 THEN 'landscape' WHEN dbo.sitegeomordesc.geomfeatid=3 THEN 'microfeature' WHEN dbo.sitegeomordesc.geomfeatid=4 THEN 'anthropogenic feature' END AS feature_type
	FROM ((geomorfeat INNER JOIN (site INNER JOIN sitegeomordesc ON site.siteiid = sitegeomordesc.siteiidref) ON (geomorfeat.geomftiidref = sitegeomordesc.geomfeatid) AND (geomorfeat.geomfiid = sitegeomordesc.geomfiidref)) INNER JOIN siteobs ON site.siteiid = siteobs.siteiidref) INNER JOIN pedon ON (siteobs.siteobsiid = pedon.siteobsiidref) AND (siteobs.siteobsiid = pedon.siteobsiidref)
ORDER BY pedon.peiid, sitegeomordesc.sitegeomdiid;"
   
	
	# setup connection to our local NASIS database
	channel <- odbcConnect('nasis_local', uid='NasisSqlRO', pwd='Re@d0n1y') 
	
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

