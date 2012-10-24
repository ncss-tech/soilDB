## TODO: need to be careful about how {l, rv, h} are used here...

get_extended_data_from_NASIS_db <- function(dsn) {
	# query diagnostic horizons, usually a 1:many relationship with pedons
	q.diagnostic <- "SELECT peiidref as peiid, dfk.ChoiceName as diag_kind, featdept, featdepb
FROM dbo.pediagfeatures 
	LEFT OUTER JOIN (SELECT * FROM dbo.MetadataDomainDetail WHERE DomainID = 147) AS dfk ON dbo.pediagfeatures.featkind = dfk.ChoiceValue
	ORDER BY dbo.pediagfeatures.peiidref, dbo.pediagfeatures.featdept;"
  
	q.surf.rf.summary <- "SELECT dbo.pedon.peiid, 

CASE WHEN f1_gr.gravel IS NULL THEN 0.0 ELSE f1_gr.gravel END as surface_gravel, 
CASE WHEN f2_cb.cobbles IS NULL THEN 0.0 ELSE f2_cb.cobbles END as surface_cobbles, 
CASE WHEN f3.stones IS NULL THEN 0.0 ELSE f3.stones END as surface_stones, 
CASE WHEN f4.boulders IS NULL THEN 0.0 ELSE f4.boulders END as surface_boulders,
CASE WHEN f5.channers IS NULL THEN 0.0 ELSE f5.channers END as surface_channers, 
CASE WHEN f6.flagstones IS NULL THEN 0.0 ELSE f6.flagstones END as surface_flagstones,
CASE WHEN f1_pgr.gravel IS NULL THEN 0.0 ELSE f1_pgr.gravel END as surface_paragravel,
CASE WHEN f2_pcb.cobbles IS NULL THEN 0.0 ELSE f2_pcb.cobbles END as surface_paracobbles

FROM ((((((((((

dbo.pedon

INNER JOIN dbo.siteobs 
ON siteobsiid = pedon.siteobsiidref)

LEFT OUTER JOIN
(
SELECT DISTINCT siteobsiidref FROM dbo.sitesurffrags
) as p ON p.siteobsiidref = siteobs.siteobsiid)

LEFT OUTER JOIN (
		SELECT siteobsiidref, Sum(sfragcov) AS gravel
		FROM dbo.sitesurffrags
		LEFT OUTER JOIN (SELECT * FROM dbo.MetadataDomainDetail WHERE dbo.MetadataDomainDetail.DomainID = 173) AS m ON sfraghard = m.ChoiceValue
		WHERE (sfragsize_r <= 76 OR sfragsize_h <= 76) AND (sfragshp != 1 OR sfragshp IS NULL) 
		AND (m.ChoiceName IN ('strongly', 'very strongly', 'indurated') OR m.ChoiceName IS NULL)
		GROUP BY siteobsiidref
	) as f1_gr ON p.siteobsiidref = f1_gr.siteobsiidref)

LEFT OUTER JOIN (
		SELECT siteobsiidref, Sum(sfragcov) AS gravel
		FROM dbo.sitesurffrags
		LEFT OUTER JOIN (SELECT * FROM dbo.MetadataDomainDetail WHERE dbo.MetadataDomainDetail.DomainID = 173) AS m ON sfraghard = m.ChoiceValue
		WHERE (sfragsize_r <= 76 OR sfragsize_h <= 76) AND (sfragshp != 1 OR sfragshp IS NULL)
		AND m.ChoiceName NOT IN ('strongly', 'very strongly', 'indurated')
		GROUP BY siteobsiidref
	) as f1_pgr ON p.siteobsiidref = f1_pgr.siteobsiidref)

	LEFT OUTER JOIN (
		SELECT siteobsiidref, Sum(sfragcov) AS cobbles
		FROM dbo.sitesurffrags
		LEFT OUTER JOIN (SELECT * FROM dbo.MetadataDomainDetail WHERE dbo.MetadataDomainDetail.DomainID = 173) AS m ON sfraghard = m.ChoiceValue
		WHERE (sfragsize_r >= 76 OR sfragsize_l >= 76) AND (sfragsize_r <= 250 OR sfragsize_h <= 250) AND (sfragshp != 1 OR sfragshp IS NULL)
		AND (m.ChoiceName IN ('strongly', 'very strongly', 'indurated') OR m.ChoiceName IS NULL)
		GROUP BY siteobsiidref
	) as f2_cb ON p.siteobsiidref = f2_cb.siteobsiidref)

	LEFT OUTER JOIN (
		SELECT siteobsiidref, Sum(sfragcov) AS cobbles
		FROM dbo.sitesurffrags
		LEFT OUTER JOIN (SELECT * FROM dbo.MetadataDomainDetail WHERE dbo.MetadataDomainDetail.DomainID = 173) AS m ON sfraghard = m.ChoiceValue
		WHERE (sfragsize_r >= 76 OR sfragsize_l >= 76) AND (sfragsize_r <= 250 OR sfragsize_h <= 250) AND (sfragshp != 1 OR sfragshp IS NULL)
		AND m.ChoiceName NOT IN ('strongly', 'very strongly', 'indurated')
		GROUP BY siteobsiidref
	) as f2_pcb ON p.siteobsiidref = f2_pcb.siteobsiidref)

	LEFT OUTER JOIN (
		SELECT siteobsiidref, Sum(sfragcov) AS stones
		FROM dbo.sitesurffrags
		LEFT OUTER JOIN (SELECT * FROM dbo.MetadataDomainDetail WHERE dbo.MetadataDomainDetail.DomainID = 173) AS m ON sfraghard = m.ChoiceValue
		WHERE (sfragsize_r >= 250 OR sfragsize_l >= 250) AND (sfragsize_r <= 600 OR sfragsize_h <= 600) AND (sfragshp != 1 OR sfragshp IS NULL)
		GROUP BY siteobsiidref
	) as f3 ON p.siteobsiidref = f3.siteobsiidref)

	LEFT OUTER JOIN (
		SELECT siteobsiidref, Sum(sfragcov) AS boulders
		FROM dbo.sitesurffrags
		LEFT OUTER JOIN (SELECT * FROM dbo.MetadataDomainDetail WHERE dbo.MetadataDomainDetail.DomainID = 173) AS m ON sfraghard = m.ChoiceValue
		WHERE sfragsize_r >= 600 OR sfragsize_l >= 600
		GROUP BY siteobsiidref
	) as f4 ON p.siteobsiidref = f4.siteobsiidref)

	LEFT OUTER JOIN (
		SELECT siteobsiidref, Sum(sfragcov) AS channers
		FROM dbo.sitesurffrags
		LEFT OUTER JOIN (SELECT * FROM dbo.MetadataDomainDetail WHERE dbo.MetadataDomainDetail.DomainID = 173) AS m ON sfraghard = m.ChoiceValue
		WHERE (sfragsize_r <= 76 OR sfragsize_h <= 76) AND sfragshp = 1
		AND (m.ChoiceName IN ('strongly', 'very strongly', 'indurated') OR m.ChoiceName IS NULL)
		GROUP BY siteobsiidref
	) as f5 ON p.siteobsiidref = f5.siteobsiidref)
	
	LEFT OUTER JOIN (
		SELECT siteobsiidref, Sum(sfragcov) AS flagstones
		FROM dbo.sitesurffrags
		LEFT OUTER JOIN (SELECT * FROM dbo.MetadataDomainDetail WHERE dbo.MetadataDomainDetail.DomainID = 173) AS m ON sfraghard = m.ChoiceValue
		WHERE (sfragsize_r >= 150 OR sfragsize_l >= 150) AND (sfragsize_r <= 380 OR sfragsize_h <= 380) AND sfragshp = 1
		AND (m.ChoiceName IN ('strongly', 'very strongly', 'indurated') OR m.ChoiceName IS NULL)
		GROUP BY siteobsiidref
	) as f6 ON p.siteobsiidref = f6.siteobsiidref)
	
	ORDER BY dbo.pedon.peiid;"
	
	
	# query rock-fragment summary by horizon
	q.rf.summary <- "SELECT p.phiidref as phiid, 

CASE WHEN f1_gr.gravel IS NULL THEN 0.0 ELSE f1_gr.gravel END as gravel, 
CASE WHEN f2_cb.cobbles IS NULL THEN 0.0 ELSE f2_cb.cobbles END as cobbles,
CASE WHEN f3.stones IS NULL THEN 0.0 ELSE f3.stones END as stones, 
CASE WHEN f4.boulders IS NULL THEN 0.0 ELSE f4.boulders END as boulders,
CASE WHEN f1_pgr.gravel IS NULL THEN 0.0 ELSE f1_pgr.gravel END as paragravel,
CASE WHEN f2_pcb.cobbles IS NULL THEN 0.0 ELSE f2_pcb.cobbles END as paracobbles,
CASE WHEN f5.channers IS NULL THEN 0.0 ELSE f5.channers END as channers, 
CASE WHEN f6.flagstones IS NULL THEN 0.0 ELSE f6.flagstones END as flagstones

FROM ((((((((
(
SELECT DISTINCT phiidref FROM dbo.phfrags
) as p

LEFT OUTER JOIN (
		SELECT phiidref, Sum(fragvol) AS gravel
		FROM dbo.phfrags
		LEFT OUTER JOIN (SELECT * FROM dbo.MetadataDomainDetail WHERE dbo.MetadataDomainDetail.DomainID = 173) AS m ON fraghard = m.ChoiceValue
		WHERE (fragsize_r <= 76 OR fragsize_h <= 76) AND (fragshp != 1 OR fragshp IS NULL)
		AND (m.ChoiceName IN ('strongly', 'very strongly', 'indurated') OR m.ChoiceName IS NULL)
		GROUP BY phiidref
	) as f1_gr ON p.phiidref = f1_gr.phiidref)

LEFT OUTER JOIN (
		SELECT phiidref, Sum(fragvol) AS gravel
		FROM dbo.phfrags
		LEFT OUTER JOIN (SELECT * FROM dbo.MetadataDomainDetail WHERE dbo.MetadataDomainDetail.DomainID = 173) AS m ON fraghard = m.ChoiceValue
		WHERE (fragsize_r <= 76 OR fragsize_h <= 76) AND (fragshp != 1 OR fragshp IS NULL)
		AND m.ChoiceName NOT IN ('strongly', 'very strongly', 'indurated')
		GROUP BY phiidref
	) as f1_pgr ON p.phiidref = f1_pgr.phiidref)

	LEFT OUTER JOIN (
		SELECT phiidref, Sum(fragvol) AS cobbles
		FROM dbo.phfrags
		LEFT OUTER JOIN (SELECT * FROM dbo.MetadataDomainDetail WHERE dbo.MetadataDomainDetail.DomainID = 173) AS m ON fraghard = m.ChoiceValue
		WHERE (fragsize_r >= 76 OR fragsize_l >= 76) AND (fragsize_r <= 250 OR fragsize_h <= 250) AND (fragshp != 1 OR fragshp IS NULL)
		AND (m.ChoiceName IN ('strongly', 'very strongly', 'indurated') OR m.ChoiceName IS NULL)
		GROUP BY phiidref
	) as f2_cb ON p.phiidref = f2_cb.phiidref)

	LEFT OUTER JOIN (
		SELECT phiidref, Sum(fragvol) AS cobbles
		FROM dbo.phfrags
		LEFT OUTER JOIN (SELECT * FROM dbo.MetadataDomainDetail WHERE dbo.MetadataDomainDetail.DomainID = 173) AS m ON fraghard = m.ChoiceValue
		WHERE (fragsize_r >= 76 OR fragsize_l >= 76) AND (fragsize_r <= 250 OR fragsize_h <= 250) AND (fragshp != 1 OR fragshp IS NULL)
		AND m.ChoiceName NOT IN ('strongly', 'very strongly', 'indurated')
		GROUP BY phiidref
	) as f2_pcb ON p.phiidref = f2_pcb.phiidref)

	LEFT OUTER JOIN (
		SELECT phiidref, Sum(fragvol) AS stones
		FROM dbo.phfrags
		LEFT OUTER JOIN (SELECT * FROM dbo.MetadataDomainDetail WHERE dbo.MetadataDomainDetail.DomainID = 173) AS m ON fraghard = m.ChoiceValue
		WHERE (fragsize_r >= 250 OR fragsize_l >= 250) AND (fragsize_r <= 600 OR fragsize_h <= 600) AND (fragshp != 1 OR fragshp IS NULL)
		GROUP BY phiidref
	) as f3 ON p.phiidref = f3.phiidref)

	LEFT OUTER JOIN (
		SELECT phiidref, Sum(fragvol) AS boulders
		FROM dbo.phfrags
		LEFT OUTER JOIN (SELECT * FROM dbo.MetadataDomainDetail WHERE dbo.MetadataDomainDetail.DomainID = 173) AS m ON fraghard = m.ChoiceValue
		WHERE fragsize_r >= 600 OR fragsize_l >= 600
		GROUP BY phiidref
	) as f4 ON p.phiidref = f4.phiidref)

	LEFT OUTER JOIN (
		SELECT phiidref, Sum(fragvol) AS channers
		FROM dbo.phfrags
		LEFT OUTER JOIN (SELECT * FROM dbo.MetadataDomainDetail WHERE dbo.MetadataDomainDetail.DomainID = 173) AS m ON fraghard = m.ChoiceValue
		WHERE (fragsize_r <= 76 OR fragsize_h <= 76) AND fragshp = 1
		AND (m.ChoiceName IN ('strongly', 'very strongly', 'indurated') OR m.ChoiceName IS NULL)
		GROUP BY phiidref
	) as f5 ON p.phiidref = f5.phiidref)
	
	LEFT OUTER JOIN (
		SELECT phiidref, Sum(fragvol) AS flagstones
		FROM dbo.phfrags
		LEFT OUTER JOIN (SELECT * FROM dbo.MetadataDomainDetail WHERE dbo.MetadataDomainDetail.DomainID = 173) AS m ON fraghard = m.ChoiceValue
		WHERE (fragsize_r >= 150 OR fragsize_l >= 150) AND (fragsize_r <= 380 OR fragsize_h <= 380) AND fragshp = 1
		AND (m.ChoiceName IN ('strongly', 'very strongly', 'indurated') OR m.ChoiceName IS NULL)
		GROUP BY phiidref
	) as f6 ON p.phiidref = f6.phiidref)
	
	ORDER BY p.phiidref;"

	# get horizon texture modifiers
	q.hz.texmod <- "SELECT dbo.phorizon.peiidref AS peiid, dbo.phorizon.phiid AS phiid, dbo.phtexture.phtiid AS phtiid, dbo.phtexturemod.seqnum, tmod.ChoiceName as texture_modifier 
  FROM (
	(dbo.phorizon INNER JOIN dbo.phtexture ON dbo.phorizon.phiid = dbo.phtexture.phiidref) 
	LEFT OUTER JOIN dbo.phtexturemod ON dbo.phtexture.phtiid = dbo.phtexturemod.phtiidref) 
	LEFT OUTER JOIN (SELECT * FROM dbo.MetadataDomainDetail WHERE dbo.MetadataDomainDetail.DomainID = 190) AS tmod ON dbo.phtexturemod.texmod = tmod.ChoiceValue;"
	
	# get geomorphic features
	q.geomorph <- "SELECT pedon.peiid, sitegeomordesc.sitegeomdiid, sitegeomordesc.geomfeatid, sitegeomordesc.existsonfeat, sitegeomordesc.geomfmod, geomorfeat.geomfname, CASE WHEN dbo.sitegeomordesc.geomfeatid=1 THEN 'landform' WHEN dbo.sitegeomordesc.geomfeatid=2 THEN 'landscape' WHEN dbo.sitegeomordesc.geomfeatid=3 THEN 'microfeature' WHEN dbo.sitegeomordesc.geomfeatid=4 THEN 'anthropogenic feature' END AS feature_type
	FROM ((geomorfeat INNER JOIN (site INNER JOIN sitegeomordesc ON site.siteiid = sitegeomordesc.siteiidref) ON (geomorfeat.geomftiidref = sitegeomordesc.geomfeatid) AND (geomorfeat.geomfiid = sitegeomordesc.geomfiidref)) INNER JOIN siteobs ON site.siteiid = siteobs.siteiidref) INNER JOIN pedon ON (siteobs.siteobsiid = pedon.siteobsiidref) AND (siteobs.siteobsiid = pedon.siteobsiidref)
ORDER BY pedon.peiid, sitegeomordesc.sitegeomdiid;"
   
	# get petaxhistory data 
	q.taxhistory <- "SELECT peiidref as peiid, classdate, classifier, classtype, taxonname, tk.ChoiceName as taxon_kind, ss.ChoiceName as series_status, ps.ChoiceName as part_size_class, ts.ChoiceName as tax_subgroup, te.ChoiceName as tax_edition, osdtypelocflag
  	FROM (((((dbo.petaxhistory 
		LEFT OUTER JOIN (SELECT * FROM dbo.MetadataDomainDetail WHERE dbo.MetadataDomainDetail.DomainID = 127) AS ps ON petaxhistory.taxpartsize = ps.ChoiceValue)
  		LEFT OUTER JOIN (SELECT * FROM dbo.MetadataDomainDetail WHERE dbo.MetadataDomainDetail.DomainID = 187) AS ts ON petaxhistory.taxsubgrp = ts.ChoiceValue)
  		LEFT OUTER JOIN (SELECT * FROM dbo.MetadataDomainDetail WHERE dbo.MetadataDomainDetail.DomainID = 102) AS tk ON petaxhistory.taxonkind = tk.ChoiceValue)
		LEFT OUTER JOIN (SELECT * FROM dbo.MetadataDomainDetail WHERE dbo.MetadataDomainDetail.DomainID = 4956) AS ss ON petaxhistory.seriesstatus = ss.ChoiceValue)
		LEFT OUTER JOIN (SELECT * FROM dbo.MetadataDomainDetail WHERE dbo.MetadataDomainDetail.DomainID = 2030) AS te ON petaxhistory.seriesstatus = te.ChoiceValue)
	ORDER BY petaxhistory.peiidref;"
	
	
	# setup connection to our local NASIS database
	channel <- odbcConnect('nasis_local', uid='NasisSqlRO', pwd='nasisRe@d0n1y') 
	
	# exec queries
	d.diagnostic <- sqlQuery(channel, q.diagnostic, stringsAsFactors=FALSE)
	d.rf.summary <- sqlQuery(channel, q.rf.summary, stringsAsFactors=FALSE)
	d.surf.rf.summary <- sqlQuery(channel, q.surf.rf.summary, stringsAsFactors=FALSE)
	d.hz.texmod <- sqlQuery(channel, q.hz.texmod, stringsAsFactors=FALSE)
	d.geomorph <- sqlQuery(channel, q.geomorph, stringsAsFactors=FALSE)
	d.taxhistory <- sqlQuery(channel, q.taxhistory, stringsAsFactors=FALSE)
	
	# close connection
	odbcClose(channel)
	
	# generate wide-formatted, diagnostic boolean summary
	d.diag.boolean <- diagHzLongtoWide(d.diagnostic)
	
	# return a list of results
	return(list(diagnostic=d.diagnostic, diagHzBoolean=d.diag.boolean, frag_summary=d.rf.summary, surf_frag_summary=d.surf.rf.summary, texmodifier=d.hz.texmod, geomorph=d.geomorph, taxhistory=d.taxhistory))
}

