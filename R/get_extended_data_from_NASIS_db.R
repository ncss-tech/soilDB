## TODO: need to be careful about how {l, rv, h} are used here...

get_extended_data_from_NASIS_db <- function() {
	
  # photo links from PedonPC
  q.photolink <- "SELECT siteobs.siteiidref AS siteiid, siteobstext.recdate,siteobstext.textcat, siteobstext.textentry AS imagepath
  FROM (
  siteobs LEFT OUTER JOIN siteobstext ON siteobs.siteobsiid = siteobstext.siteobsiidref) 
  WHERE siteobstext.textcat LIKE 'Photo%' ORDER BY siteobstext.siteobstextkind;"
  
  
	## TODO: adapt this for SQL server syntax!
	# returns the first structure defined per horizon
	q.structure <- "SELECT s.phiid, FIRST(s.structure_grade) as structure_grade, FIRST(s.structure_size) as structure_size, FIRST(s.structure_type) as structure_type
	FROM
	(SELECT phstructure.phiidref as phiid, sg.ChoiceName as structure_grade, ss.ChoiceName as structure_size, st.ChoiceName as structure_type, structid, structpartsto
	FROM ((
	phstructure
	LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 1300) AS sg ON phstructure.structgrade = sg.ChoiceValue)
	LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 1302) AS ss ON phstructure.structsize = ss.ChoiceValue)
	LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 1303) AS st ON phstructure.structtype = st.ChoiceValue
	WHERE structtype IS NOT NULL
	ORDER BY phstructure.phiidref, structid ASC
	) as s
	GROUP BY s.phiid
	ORDER BY s.phiid;"
	
	# existing veg
	q.veg <- "SELECT siteiid, siteexistveg_View_1.seqnum, plantsym, plantsciname, plantnatvernm, vegetationstratalevel, orderofdominance
	FROM
	(((
	site_View_1 INNER JOIN siteobs_View_1 ON site_View_1.siteiid = siteobs_View_1.siteiidref) 
	INNER JOIN siteexistveg_View_1 ON siteexistveg_View_1.siteobsiidref = siteobs_View_1.siteobsiid)
	INNER JOIN localplant ON siteexistveg_View_1.lplantiidref = localplant.lplantiid)
	INNER JOIN plant ON localplant.plantiidref = plant.plantiid
	ORDER BY site_View_1.siteiid;"
	
	# query diagnostic horizons, usually a 1:many relationship with pedons
	q.diagnostic <- "SELECT peiidref as peiid, dfk.ChoiceName as diag_kind, featdept, featdepb
FROM pediagfeatures_View_1 
	LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 147) AS dfk ON pediagfeatures_View_1.featkind = dfk.ChoiceValue
	ORDER BY pediagfeatures_View_1.peiidref, pediagfeatures_View_1.featdept;"
  
	q.surf.rf.summary <- "SELECT pedon_View_1.peiid, 

CASE WHEN f1_gr.gravel IS NULL THEN 0.0 ELSE f1_gr.gravel END as surface_gravel, 
CASE WHEN f2_cb.cobbles IS NULL THEN 0.0 ELSE f2_cb.cobbles END as surface_cobbles, 
CASE WHEN f3.stones IS NULL THEN 0.0 ELSE f3.stones END as surface_stones, 
CASE WHEN f4.boulders IS NULL THEN 0.0 ELSE f4.boulders END as surface_boulders,
CASE WHEN f5.channers IS NULL THEN 0.0 ELSE f5.channers END as surface_channers, 
CASE WHEN f6.flagstones IS NULL THEN 0.0 ELSE f6.flagstones END as surface_flagstones,
CASE WHEN f1_pgr.gravel IS NULL THEN 0.0 ELSE f1_pgr.gravel END as surface_paragravel,
CASE WHEN f2_pcb.cobbles IS NULL THEN 0.0 ELSE f2_pcb.cobbles END as surface_paracobbles

FROM ((((((((((

pedon_View_1

INNER JOIN siteobs_View_1 
ON siteobsiid = pedon_View_1.siteobsiidref)

LEFT OUTER JOIN
(
SELECT DISTINCT siteobsiidref FROM sitesurffrags_View_1
) as p ON p.siteobsiidref = siteobs_View_1.siteobsiid)

LEFT OUTER JOIN (
		SELECT siteobsiidref, Sum(sfragcov) AS gravel
		FROM sitesurffrags_View_1
		LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 173) AS m ON sfraghard = m.ChoiceValue
		WHERE (sfragsize_r <= 76 OR sfragsize_h <= 76) AND (sfragshp != 1 OR sfragshp IS NULL) 
		AND (m.ChoiceName IN ('strongly', 'very strongly', 'indurated') OR m.ChoiceName IS NULL)
		GROUP BY siteobsiidref
	) as f1_gr ON p.siteobsiidref = f1_gr.siteobsiidref)

LEFT OUTER JOIN (
		SELECT siteobsiidref, Sum(sfragcov) AS gravel
		FROM sitesurffrags_View_1
		LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 173) AS m ON sfraghard = m.ChoiceValue
		WHERE (sfragsize_r <= 76 OR sfragsize_h <= 76) AND (sfragshp != 1 OR sfragshp IS NULL)
		AND m.ChoiceName NOT IN ('strongly', 'very strongly', 'indurated')
		GROUP BY siteobsiidref
	) as f1_pgr ON p.siteobsiidref = f1_pgr.siteobsiidref)

	LEFT OUTER JOIN (
		SELECT siteobsiidref, Sum(sfragcov) AS cobbles
		FROM sitesurffrags_View_1
		LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 173) AS m ON sfraghard = m.ChoiceValue
		WHERE (sfragsize_r >= 76 OR sfragsize_l >= 76) AND (sfragsize_r <= 250 OR sfragsize_h <= 250) AND (sfragshp != 1 OR sfragshp IS NULL)
		AND (m.ChoiceName IN ('strongly', 'very strongly', 'indurated') OR m.ChoiceName IS NULL)
		GROUP BY siteobsiidref
	) as f2_cb ON p.siteobsiidref = f2_cb.siteobsiidref)

	LEFT OUTER JOIN (
		SELECT siteobsiidref, Sum(sfragcov) AS cobbles
		FROM sitesurffrags_View_1
		LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 173) AS m ON sfraghard = m.ChoiceValue
		WHERE (sfragsize_r >= 76 OR sfragsize_l >= 76) AND (sfragsize_r <= 250 OR sfragsize_h <= 250) AND (sfragshp != 1 OR sfragshp IS NULL)
		AND m.ChoiceName NOT IN ('strongly', 'very strongly', 'indurated')
		GROUP BY siteobsiidref
	) as f2_pcb ON p.siteobsiidref = f2_pcb.siteobsiidref)

	LEFT OUTER JOIN (
		SELECT siteobsiidref, Sum(sfragcov) AS stones
		FROM sitesurffrags_View_1
		LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 173) AS m ON sfraghard = m.ChoiceValue
		WHERE (sfragsize_r >= 250 OR sfragsize_l >= 250) AND (sfragsize_r <= 600 OR sfragsize_h <= 600) AND (sfragshp != 1 OR sfragshp IS NULL)
		GROUP BY siteobsiidref
	) as f3 ON p.siteobsiidref = f3.siteobsiidref)

	LEFT OUTER JOIN (
		SELECT siteobsiidref, Sum(sfragcov) AS boulders
		FROM sitesurffrags_View_1
		LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 173) AS m ON sfraghard = m.ChoiceValue
		WHERE sfragsize_r >= 600 OR sfragsize_l >= 600
		GROUP BY siteobsiidref
	) as f4 ON p.siteobsiidref = f4.siteobsiidref)

	LEFT OUTER JOIN (
		SELECT siteobsiidref, Sum(sfragcov) AS channers
		FROM sitesurffrags_View_1
		LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 173) AS m ON sfraghard = m.ChoiceValue
		WHERE (sfragsize_r <= 76 OR sfragsize_h <= 76) AND sfragshp = 1
		AND (m.ChoiceName IN ('strongly', 'very strongly', 'indurated') OR m.ChoiceName IS NULL)
		GROUP BY siteobsiidref
	) as f5 ON p.siteobsiidref = f5.siteobsiidref)
	
	LEFT OUTER JOIN (
		SELECT siteobsiidref, Sum(sfragcov) AS flagstones
		FROM sitesurffrags_View_1
		LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 173) AS m ON sfraghard = m.ChoiceValue
		WHERE (sfragsize_r >= 150 OR sfragsize_l >= 150) AND (sfragsize_r <= 380 OR sfragsize_h <= 380) AND sfragshp = 1
		AND (m.ChoiceName IN ('strongly', 'very strongly', 'indurated') OR m.ChoiceName IS NULL)
		GROUP BY siteobsiidref
	) as f6 ON p.siteobsiidref = f6.siteobsiidref)
	
	ORDER BY pedon_View_1.peiid;"
	
	
	# query rock-fragment summary by horizon
	q.rf.summary <- "SELECT p.phiid, 

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
	SELECT DISTINCT phiid FROM phorizon_View_1
	) as p
	
	LEFT OUTER JOIN (
	SELECT phiidref, Sum(fragvol) AS gravel
	FROM phfrags_View_1
	LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 173) AS m ON fraghard = m.ChoiceValue
	WHERE (fragsize_r <= 76 OR fragsize_h <= 76) AND (fragshp != 1 OR fragshp IS NULL)
	AND (m.ChoiceName IN ('strongly', 'very strongly', 'indurated') OR m.ChoiceName IS NULL)
	GROUP BY phiidref
	) as f1_gr ON p.phiid = f1_gr.phiidref)
	
	LEFT OUTER JOIN (
	SELECT phiidref, Sum(fragvol) AS gravel
	FROM phfrags_View_1
	LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 173) AS m ON fraghard = m.ChoiceValue
	WHERE (fragsize_r <= 76 OR fragsize_h <= 76) AND (fragshp != 1 OR fragshp IS NULL)
	AND m.ChoiceName NOT IN ('strongly', 'very strongly', 'indurated')
	GROUP BY phiidref
	) as f1_pgr ON p.phiid = f1_pgr.phiidref)
	
	LEFT OUTER JOIN (
	SELECT phiidref, Sum(fragvol) AS cobbles
	FROM phfrags_View_1
	LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 173) AS m ON fraghard = m.ChoiceValue
	WHERE (fragsize_r >= 76 OR fragsize_l >= 76) AND (fragsize_r <= 250 OR fragsize_h <= 250) AND (fragshp != 1 OR fragshp IS NULL)
	AND (m.ChoiceName IN ('strongly', 'very strongly', 'indurated') OR m.ChoiceName IS NULL)
	GROUP BY phiidref
	) as f2_cb ON p.phiid = f2_cb.phiidref)
	
	LEFT OUTER JOIN (
	SELECT phiidref, Sum(fragvol) AS cobbles
	FROM phfrags_View_1
	LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 173) AS m ON fraghard = m.ChoiceValue
	WHERE (fragsize_r >= 76 OR fragsize_l >= 76) AND (fragsize_r <= 250 OR fragsize_h <= 250) AND (fragshp != 1 OR fragshp IS NULL)
	AND m.ChoiceName NOT IN ('strongly', 'very strongly', 'indurated')
	GROUP BY phiidref
	) as f2_pcb ON p.phiid = f2_pcb.phiidref)
	
	LEFT OUTER JOIN (
	SELECT phiidref, Sum(fragvol) AS stones
	FROM phfrags_View_1
	LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 173) AS m ON fraghard = m.ChoiceValue
	WHERE (fragsize_r >= 250 OR fragsize_l >= 250) AND (fragsize_r <= 600 OR fragsize_h <= 600) AND (fragshp != 1 OR fragshp IS NULL)
	GROUP BY phiidref
	) as f3 ON p.phiid = f3.phiidref)
	
	LEFT OUTER JOIN (
	SELECT phiidref, Sum(fragvol) AS boulders
	FROM phfrags_View_1
	LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 173) AS m ON fraghard = m.ChoiceValue
	WHERE fragsize_r >= 600 OR fragsize_l >= 600
	GROUP BY phiidref
	) as f4 ON p.phiid = f4.phiidref)
	
	LEFT OUTER JOIN (
	SELECT phiidref, Sum(fragvol) AS channers
	FROM phfrags_View_1
	LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 173) AS m ON fraghard = m.ChoiceValue
	WHERE (fragsize_r <= 76 OR fragsize_h <= 76) AND fragshp = 1
	AND (m.ChoiceName IN ('strongly', 'very strongly', 'indurated') OR m.ChoiceName IS NULL)
	GROUP BY phiidref
	) as f5 ON p.phiid = f5.phiidref)
	
	LEFT OUTER JOIN (
	SELECT phiidref, Sum(fragvol) AS flagstones
	FROM phfrags_View_1
	LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 173) AS m ON fraghard = m.ChoiceValue
	WHERE (fragsize_r >= 150 OR fragsize_l >= 150) AND (fragsize_r <= 380 OR fragsize_h <= 380) AND fragshp = 1
	AND (m.ChoiceName IN ('strongly', 'very strongly', 'indurated') OR m.ChoiceName IS NULL)
	GROUP BY phiidref
	) as f6 ON p.phiid = f6.phiidref)
	
	ORDER BY p.phiid;"
  

	# get horizon texture modifiers
	q.hz.texmod <- "SELECT phorizon_View_1.peiidref AS peiid, phorizon_View_1.phiid AS phiid, phtexture_View_1.phtiid AS phtiid, phtexturemod_View_1.seqnum, tmod.ChoiceName as texture_modifier 
  FROM (
	(phorizon_View_1 INNER JOIN phtexture_View_1 ON phorizon_View_1.phiid = phtexture_View_1.phiidref) 
	LEFT OUTER JOIN phtexturemod_View_1 ON phtexture_View_1.phtiid = phtexturemod_View_1.phtiidref) 
	LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 190) AS tmod ON phtexturemod_View_1.texmod = tmod.ChoiceValue;"
	
	# get geomorphic features
	q.geomorph <- "SELECT pedon_View_1.peiid, sitegeomordesc_View_1.geomfmod, geomorfeat.geomfname, sitegeomordesc_View_1.existsonfeat, sitegeomordesc_View_1.geomfiidref, lower(geomorfeattype.geomftname) as geomftname
FROM geomorfeattype 
  RIGHT JOIN (geomorfeat 
  RIGHT JOIN ((site_View_1 INNER JOIN sitegeomordesc_View_1 ON site_View_1.siteiid = sitegeomordesc_View_1.siteiidref) 
  INNER JOIN (siteobs_View_1 INNER JOIN pedon_View_1 ON siteobs_View_1.siteobsiid = pedon_View_1.siteobsiidref) 
  ON site_View_1.siteiid = siteobs_View_1.siteiidref) 
  ON geomorfeat.geomfiid = sitegeomordesc_View_1.geomfiidref) 
  ON geomorfeattype.geomftiid = geomorfeat.geomftiidref 
  ORDER BY peiid ASC;"
	
   
	# get petaxhistory data 
	q.taxhistory <- "SELECT peiidref as peiid, classdate, classifier, tk.ChoiceName as class_type, taxonname, tk.ChoiceName as taxon_kind, ss.ChoiceName as series_status, ps.ChoiceName as part_size_class, tord.ChoiceName as tax_order, tso.ChoiceName as tax_suborder, tgg.ChoiceName as tax_grtgroup, ts.ChoiceName as tax_subgroup, te.ChoiceName as tax_edition, osdtypelocflag
  	FROM (((((((((petaxhistory_View_1 
		LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 127) AS ps ON petaxhistory_View_1.taxpartsize = ps.ChoiceValue)
  		LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 187) AS ts ON petaxhistory_View_1.taxsubgrp = ts.ChoiceValue)
  		LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 102) AS tk ON petaxhistory_View_1.taxonkind = tk.ChoiceValue)
		LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 4956) AS ss ON petaxhistory_View_1.seriesstatus = ss.ChoiceValue)
		LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 2030) AS te ON petaxhistory_View_1.soiltaxedition = te.ChoiceValue)
		LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 4942) AS cl ON petaxhistory_View_1.classtype = cl.ChoiceValue)
		LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 132) AS tord ON petaxhistory_View_1.taxorder = tord.ChoiceValue)
		LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 134) AS tso ON petaxhistory_View_1.taxsuborder = tso.ChoiceValue)
		LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 130) AS tgg ON petaxhistory_View_1.taxgrtgroup = tgg.ChoiceValue)
	ORDER BY petaxhistory_View_1.peiidref;"
	
	
	# setup connection to our local NASIS database
	channel <- odbcConnect('nasis_local', uid='NasisSqlRO', pwd='nasisRe@d0n1y') 
	
	# exec queries
	d.veg <- sqlQuery(channel, q.veg, stringsAsFactors=FALSE)
	d.diagnostic <- sqlQuery(channel, q.diagnostic, stringsAsFactors=FALSE)
	d.rf.summary <- sqlQuery(channel, q.rf.summary, stringsAsFactors=FALSE)
	d.surf.rf.summary <- sqlQuery(channel, q.surf.rf.summary, stringsAsFactors=FALSE)
	d.hz.texmod <- sqlQuery(channel, q.hz.texmod, stringsAsFactors=FALSE)
	d.geomorph <- sqlQuery(channel, q.geomorph, stringsAsFactors=FALSE)
	d.taxhistory <- sqlQuery(channel, q.taxhistory, stringsAsFactors=FALSE)
  d.photolink <- sqlQuery(channel, q.photolink, stringsAsFactors=FALSE)
	
	# close connection
	odbcClose(channel)
	
	# generate wide-formatted, diagnostic boolean summary
	d.diag.boolean <- diagHzLongtoWide(d.diagnostic)
	
	# return a list of results
	return(list(veg=d.veg,
							diagnostic=d.diagnostic, 
							diagHzBoolean=d.diag.boolean, 
							frag_summary=d.rf.summary, 
							surf_frag_summary=d.surf.rf.summary, 
							texmodifier=d.hz.texmod, 
							geomorph=d.geomorph, 
							taxhistory=d.taxhistory,
              photo=d.photolink))
}

