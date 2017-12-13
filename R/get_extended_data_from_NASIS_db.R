## TODO: need to be careful about how {l, rv, h} are used here...

## TODO: multiple records / site in siteobs are possible and will result in duplicate data

## TODO_JS: incorporated the use of uncode() into all except the fragment queries, which I think are best left as they are.

get_extended_data_from_NASIS_db <- function(SS=TRUE, nullFragsAreZero=TRUE) {
  # must have RODBC installed
  if(!requireNamespace('RODBC'))
    stop('please install the `RODBC` package', call.=FALSE)
  
  # photo links from PedonPC stored as sitetext notes
  q.photolink <- "SELECT siteobs_View_1.siteiidref AS siteiid, siteobstext_View_1.recdate, siteobstext_View_1.textcat, siteobstext_View_1.textentry AS imagepath
  FROM
  siteobs_View_1 LEFT OUTER JOIN siteobstext_View_1 ON siteobs_View_1.siteobsiid = siteobstext_View_1.siteobsiidref
  WHERE siteobstext_View_1.textcat LIKE 'Photo%' ORDER BY siteobstext_View_1.siteobstextkind;"
  
  # get all structure records / horizon
  q.structure <- "SELECT phstructure.phiidref as phiid, structgrade, structsize, structtype, structid, structpartsto
	FROM phstructure_View_1
	WHERE structtype IS NOT NULL
	ORDER BY phstructure.phiidref, structid ASC;"
  
  # existing veg
  q.veg <- "SELECT siteiid, vegplotid, vegplotname, obsdate, primarydatacollector, datacollectionpurpose, assocuserpedonid, plotplantinventory.seqnum, plantsym, plantsciname, plantnatvernm, orderofdominance, speciescancovpct, speciescancovclass

  FROM site_View_1 AS s
  INNER JOIN siteobs ON siteobs.siteiidref=s.siteiid
  LEFT JOIN vegplot_View_1 AS v on v.siteobsiidref=siteobs.siteobsiid
  LEFT JOIN plotplantinventory ON plotplantinventory.vegplotiidref=v.vegplotiid
  INNER JOIN plant ON plant.plantiid=plotplantinventory.plantiidref;"
  
  # ecological site
  q.ecosite <- "SELECT siteiidref AS siteiid, ecositeid, ecositenm, ecositecorrdate, classifier As es_classifier
  FROM siteecositehistory_View_1 AS seh
  INNER JOIN ecologicalsite AS es ON es.ecositeiid=seh.ecositeiidref
  ORDER BY 'siteiid';"
  
  
  # query diagnostic horizons, usually a 1:many relationship with pedons
  q.diagnostic <- "SELECT peiidref as peiid, featkind, featdept, featdepb
FROM pediagfeatures_View_1 
	ORDER BY pediagfeatures_View_1.peiidref, pediagfeatures_View_1.featdept;"
  
  
  # TODO: convert this to simplifyFragmentData
  q.surf.rf.summary <- "SELECT pedon_View_1.peiid, 
f1_fgr.gravel as surface_fgravel, 
f1_gr.gravel as surface_gravel, 
f2_cb.cobbles as surface_cobbles, 
f3.stones as surface_stones, 
f4.boulders as surface_boulders,
f5.channers as surface_channers, 
f6.flagstones as surface_flagstones,
f1_pgr.gravel as surface_paragravel,
f2_pcb.cobbles as surface_paracobbles

FROM

pedon_View_1

INNER JOIN siteobs_View_1 
ON siteobsiid = pedon_View_1.siteobsiidref

LEFT OUTER JOIN
(
SELECT DISTINCT siteobsiidref FROM sitesurffrags_View_1
) as p ON p.siteobsiidref = siteobs_View_1.siteobsiid

LEFT OUTER JOIN (
  	SELECT siteobsiidref, Sum(sfragcov) AS gravel
		FROM sitesurffrags_View_1
		LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 173) AS m ON sfraghard = m.ChoiceValue
		WHERE (sfragsize_r <= 5 OR sfragsize_h <= 5) AND (sfragshp != 1 OR sfragshp IS NULL) 
		AND (m.ChoiceName IN ('strongly', 'very strongly', 'indurated') OR m.ChoiceName IS NULL)
		GROUP BY siteobsiidref
	) as f1_fgr ON p.siteobsiidref = f1_fgr.siteobsiidref

LEFT OUTER JOIN (
		SELECT siteobsiidref, Sum(sfragcov) AS gravel
		FROM sitesurffrags_View_1
		LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 173) AS m ON sfraghard = m.ChoiceValue
		WHERE (sfragsize_r <= 76 OR sfragsize_h <= 76) AND (sfragshp != 1 OR sfragshp IS NULL) 
		AND (m.ChoiceName IN ('strongly', 'very strongly', 'indurated') OR m.ChoiceName IS NULL)
		GROUP BY siteobsiidref
	) as f1_gr ON p.siteobsiidref = f1_gr.siteobsiidref

LEFT OUTER JOIN (
		SELECT siteobsiidref, Sum(sfragcov) AS gravel
		FROM sitesurffrags_View_1
		LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 173) AS m ON sfraghard = m.ChoiceValue
		WHERE (sfragsize_r <= 76 OR sfragsize_h <= 76) AND (sfragshp != 1 OR sfragshp IS NULL)
		AND m.ChoiceName NOT IN ('strongly', 'very strongly', 'indurated')
		GROUP BY siteobsiidref
	) as f1_pgr ON p.siteobsiidref = f1_pgr.siteobsiidref

	LEFT OUTER JOIN (
		SELECT siteobsiidref, Sum(sfragcov) AS cobbles
		FROM sitesurffrags_View_1
		LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 173) AS m ON sfraghard = m.ChoiceValue
		WHERE (sfragsize_r >= 76 OR sfragsize_l >= 76) AND (sfragsize_r <= 250 OR sfragsize_h <= 250) AND (sfragshp != 1 OR sfragshp IS NULL)
		AND (m.ChoiceName IN ('strongly', 'very strongly', 'indurated') OR m.ChoiceName IS NULL)
		GROUP BY siteobsiidref
	) as f2_cb ON p.siteobsiidref = f2_cb.siteobsiidref

	LEFT OUTER JOIN (
		SELECT siteobsiidref, Sum(sfragcov) AS cobbles
		FROM sitesurffrags_View_1
		LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 173) AS m ON sfraghard = m.ChoiceValue
		WHERE (sfragsize_r >= 76 OR sfragsize_l >= 76) AND (sfragsize_r <= 250 OR sfragsize_h <= 250) AND (sfragshp != 1 OR sfragshp IS NULL)
		AND m.ChoiceName NOT IN ('strongly', 'very strongly', 'indurated')
		GROUP BY siteobsiidref
	) as f2_pcb ON p.siteobsiidref = f2_pcb.siteobsiidref

	LEFT OUTER JOIN (
		SELECT siteobsiidref, Sum(sfragcov) AS stones
		FROM sitesurffrags_View_1
		LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 173) AS m ON sfraghard = m.ChoiceValue
		WHERE (sfragsize_r >= 250 OR sfragsize_l >= 250) AND (sfragsize_r <= 600 OR sfragsize_h <= 600) AND (sfragshp != 1 OR sfragshp IS NULL)
		GROUP BY siteobsiidref
	) as f3 ON p.siteobsiidref = f3.siteobsiidref

	LEFT OUTER JOIN (
		SELECT siteobsiidref, Sum(sfragcov) AS boulders
		FROM sitesurffrags_View_1
		LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 173) AS m ON sfraghard = m.ChoiceValue
		WHERE sfragsize_r >= 600 OR sfragsize_l >= 600
		GROUP BY siteobsiidref
	) as f4 ON p.siteobsiidref = f4.siteobsiidref

	LEFT OUTER JOIN (
		SELECT siteobsiidref, Sum(sfragcov) AS channers
		FROM sitesurffrags_View_1
		LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 173) AS m ON sfraghard = m.ChoiceValue
		WHERE (sfragsize_r <= 76 OR sfragsize_h <= 76) AND sfragshp = 1
		AND (m.ChoiceName IN ('strongly', 'very strongly', 'indurated') OR m.ChoiceName IS NULL)
		GROUP BY siteobsiidref
	) as f5 ON p.siteobsiidref = f5.siteobsiidref
	
	LEFT OUTER JOIN (
		SELECT siteobsiidref, Sum(sfragcov) AS flagstones
		FROM sitesurffrags_View_1
		LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 173) AS m ON sfraghard = m.ChoiceValue
		WHERE (sfragsize_r >= 150 OR sfragsize_l >= 150) AND (sfragsize_r <= 380 OR sfragsize_h <= 380) AND sfragshp = 1
		AND (m.ChoiceName IN ('strongly', 'very strongly', 'indurated') OR m.ChoiceName IS NULL)
		GROUP BY siteobsiidref
	) as f6 ON p.siteobsiidref = f6.siteobsiidref
	
	ORDER BY pedon_View_1.peiid;"


  # base table is phorizon so that NULL data can be converted to 0s later
  q.rf.data <- "SELECT p.phiid, fragvol, fragsize_l, fragsize_r, fragsize_h, fragshp, fraghard
  FROM 
  (
 	SELECT DISTINCT phiid FROM phorizon_View_1
 	) as p  
  LEFT OUTER JOIN phfrags_View_1 ON p.phiid = phfrags_View_1.phiidref;"

  # get horizon texture modifiers
  q.hz.texmod <- "SELECT phorizon_View_1.peiidref AS peiid, phorizon_View_1.phiid AS phiid, phtexture_View_1.phtiid AS phtiid, phtexturemod_View_1.seqnum, texmod 
  FROM
	phorizon_View_1 INNER JOIN phtexture_View_1 ON phorizon_View_1.phiid = phtexture_View_1.phiidref
	LEFT OUTER JOIN phtexturemod_View_1 ON phtexture_View_1.phtiid = phtexturemod_View_1.phtiidref;"

  # get geomorphic features
  q.geomorph <- "SELECT pedon_View_1.peiid, sitegeomordesc_View_1.geomfmod, geomorfeat.geomfname, sitegeomordesc_View_1.geomfeatid, sitegeomordesc_View_1.existsonfeat, sitegeomordesc_View_1.geomfiidref, lower(geomorfeattype.geomftname) as geomftname
FROM geomorfeattype 
  RIGHT JOIN geomorfeat 
  RIGHT JOIN site_View_1 INNER JOIN sitegeomordesc_View_1 ON site_View_1.siteiid = sitegeomordesc_View_1.siteiidref
  INNER JOIN siteobs_View_1 INNER JOIN pedon_View_1 ON siteobs_View_1.siteobsiid = pedon_View_1.siteobsiidref
  ON site_View_1.siteiid = siteobs_View_1.siteiidref
  ON geomorfeat.geomfiid = sitegeomordesc_View_1.geomfiidref
  ON geomorfeattype.geomftiid = geomorfeat.geomftiidref 
  ORDER BY peiid, geomfeatid ASC;"


  q.taxhistory <- "SELECT peiidref as peiid, classdate, classifier, classtype, taxonname, taxonkind, seriesstatus, taxpartsize, taxorder, taxsuborder, taxgrtgroup, taxsubgrp, soiltaxedition, osdtypelocflag, taxmoistcl, taxtempregime, taxfamother, psctopdepth, pscbotdepth
  	FROM
    petaxhistory_View_1 LEFT OUTER JOIN petaxhistmoistcl_View_1 ON petaxhistory_View_1.petaxhistoryiid = petaxhistmoistcl_View_1.pedtaxhistoryiidref
    LEFT OUTER JOIN petxhistfmother_View_1 ON petaxhistory_View_1.petaxhistoryiid = petxhistfmother_View_1.pedtaxhistoryiidref
ORDER BY petaxhistory_View_1.peiidref;"


  q.sitepm <- "SELECT siteiidref as siteiid, seqnum, pmorder, pmdept, pmdepb, pmmodifier, pmgenmod, pmkind, pmorigin, pmweathering 
FROM
sitepm_View_1 INNER JOIN site_View_1 on sitepm_View_1.siteiidref = site_View_1.siteiid;"


	
	# setup connection local NASIS
	channel <- RODBC::odbcDriverConnect(connection="DSN=nasis_local;UID=NasisSqlRO;PWD=nasisRe@d0n1y")
	
	# exec queries
	d.veg <- RODBC::sqlQuery(channel, q.veg, stringsAsFactors=FALSE)
	d.ecosite <- RODBC::sqlQuery(channel, q.ecosite, stringsAsFactors=FALSE)
	d.diagnostic <- RODBC::sqlQuery(channel, q.diagnostic, stringsAsFactors=FALSE)
	d.rf.data <- RODBC::sqlQuery(channel, q.rf.data, stringsAsFactors=FALSE)
	d.surf.rf.summary <- RODBC::sqlQuery(channel, q.surf.rf.summary, stringsAsFactors=FALSE)
	d.hz.texmod <- RODBC::sqlQuery(channel, q.hz.texmod, stringsAsFactors=FALSE)
	d.geomorph <- RODBC::sqlQuery(channel, q.geomorph, stringsAsFactors=FALSE)
	d.taxhistory <- RODBC::sqlQuery(channel, q.taxhistory, stringsAsFactors=FALSE)
	d.photolink <- RODBC::sqlQuery(channel, q.photolink, stringsAsFactors=FALSE)
	d.sitepm <- RODBC::sqlQuery(channel, q.sitepm, stringsAsFactors=FALSE)
	d.structure <- RODBC::sqlQuery(channel, q.structure, stringsAsFactors=FALSE)

	## uncode the one that need that here
	d.diagnostic <- uncode(d.diagnostic)
	d.rf.data <- uncode(d.rf.data)
	d.hz.texmod <- uncode(d.hz.texmod)
	d.taxhistory <- uncode(d.taxhistory)
	d.sitepm <- uncode(d.sitepm)
	d.structure <- uncode(d.structure)

	# close connection
	RODBC::odbcClose(channel)
	
	
	## the following steps will not work when data are missing from local DB or SS
	# return NULL in those cases
	
	if(nrow(d.diagnostic) > 0) {
	  # generate wide-formatted, diagnostic boolean summary
	  d.diag.boolean <- .diagHzLongtoWide(d.diagnostic)
	} else {
	  d.diag.boolean <- NULL
	}
	
	if(nrow(d.photolink) > 0) {
	  # parse imagename and imagepath for photo links
	  d.photolink$imagename <- basename(d.photolink$imagepath)
	}

	if(nrow(d.rf.data) > 0) {
	  # summarize rock fragment data
	  d.rf.summary <- simplfyFragmentData(d.rf.data, 'phiid', nullFragsAreZero = nullFragsAreZero)
	} else {
	  d.rf.summary <- NULL
	}
	
	
	# return a list of results
	return(list(veg=d.veg, ecositehistory=d.ecosite,
							diagnostic=d.diagnostic, 
							diagHzBoolean=d.diag.boolean, 
							frag_summary=d.rf.summary, 
							surf_frag_summary=d.surf.rf.summary, 
							texmodifier=d.hz.texmod, 
							geomorph=d.geomorph, 
							taxhistory=d.taxhistory,
						  photo=d.photolink,
							pm=d.sitepm,
              struct=d.structure))
}

