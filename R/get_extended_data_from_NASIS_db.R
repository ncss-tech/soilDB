## TODO: need to be careful about how {l, rv, h} are used here...

## TODO: multiple records / site in siteobs are possible and will result in duplicate data

## TODO_JS: incorporated the use of uncode() into all except the fragment queries, which I think are best left as they are.



#' Extract accessory tables and summaries from a local NASIS Database
#'
#' @param SS get data from the currently loaded Selected Set in NASIS or from
#' the entire local database (default: `TRUE`)
#'
#' @param nullFragsAreZero should fragment volumes of NULL be interpreted as 0?
#' (default: TRUE), see details
#'
#' @param stringsAsFactors logical: should character vectors be converted to
#' factors? This argument is passed to the `uncode()` function. It does not
#' convert those vectors that have been set outside of `uncode()` (i.e. hard
#' coded).
#'
#' @param dsn Optional: path to local SQLite database containing NASIS
#' table structure; default: `NULL`
#'
#' @return A list with the results.
#' @author Jay M. Skovlin and Dylan E. Beaudette
#' @seealso \code{\link{get_hz_data_from_NASIS_db}},
#' \code{\link{get_site_data_from_NASIS_db}}
#' @keywords manip
#' @examples
#'
#' \donttest{
#'
#' if(local_NASIS_defined()) {
#'  # query extended data
#'  e <- try(get_extended_data_from_NASIS_db())
#'
#'  # show contents of extended data
#'  str(e)
#' }
#'
#' }
#'
#' @export get_extended_data_from_NASIS_db
get_extended_data_from_NASIS_db <- function(SS = TRUE,
                                            nullFragsAreZero = TRUE,
                                            stringsAsFactors = default.stringsAsFactors(),
                                            dsn = NULL) {


  # photo links from PedonPC stored as sitetext notes
  q.photolink <- "SELECT so.siteiidref AS siteiid, sot.recdate, sot.textcat,  CAST(sot.textentry AS text) AS imagepath
  FROM
  siteobs_View_1 AS so
  LEFT OUTER JOIN siteobstext_View_1 AS sot ON so.siteobsiid = sot.siteobsiidref
  WHERE sot.textcat LIKE 'Photo%'
  ORDER BY sot.siteobstextkind;"

  # toggle selected set vs. local DB
  if (SS == FALSE) {
    q.photolink <- gsub(pattern = '_View_1', replacement = '', x = q.photolink, fixed = TRUE)
  }


  # get all structure records / horizon
  q.structure <- "SELECT phiidref as phiid, structgrade, structsize, structtype, structid, structpartsto
	FROM phstructure_View_1
	WHERE structtype IS NOT NULL
	ORDER BY phiidref, structid ASC;"

  # toggle selected set vs. local DB
  if (SS == FALSE) {
    q.structure <- gsub(pattern = '_View_1', replacement = '', x = q.structure, fixed = TRUE)
  }


  # ecological site
  q.ecosite <- "SELECT siteiidref AS siteiid, ecositeid, ecositenm, ecositecorrdate, classifier As es_classifier
  FROM siteecositehistory_View_1 AS seh
  -- note: ecologicalsite table not managed by SS
  LEFT OUTER JOIN ecologicalsite AS es ON es.ecositeiid=seh.ecositeiidref
  ORDER BY siteiid;"

  # toggle selected set vs. local DB
  if (SS == FALSE) {
    q.ecosite <- gsub(pattern = '_View_1', replacement = '', x = q.ecosite, fixed = TRUE)
  }


  ## TODO: include all peiid from pedon table so that there are no NA in the boolean summary
  ## https://github.com/ncss-tech/soilDB/issues/59
  # query diagnostic horizons, usually a 1:many relationship with pedons
  q.diagnostic <- "SELECT peiidref as peiid, featkind, featdept, featdepb
  FROM
  pediagfeatures_View_1 AS pdf
	ORDER BY pdf.peiidref, pdf.featdept;"

  # toggle selected set vs. local DB
  if (SS == FALSE) {
    q.diagnostic <- gsub(pattern = '_View_1', replacement = '', x = q.diagnostic, fixed = TRUE)
  }

  q.restriction <- "SELECT peiidref as peiid, resdept, resdepb, resthk_l, resthk_r, resthk_h, reskind, reshard
FROM perestrictions_View_1 As prf
	ORDER BY prf.peiidref, prf.resdept;"

  # toggle selected set vs. local DB
  if (SS == FALSE) {
    q.restriction <- gsub(pattern = '_View_1', replacement = '', x = q.restriction, fixed = TRUE)
  }

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


  # toggle selected set vs. local DB
  if (SS == FALSE) {
    q.surf.rf.summary <- gsub(pattern = '_View_1', replacement = '', x = q.surf.rf.summary, fixed = TRUE)
  }

  # base table is phorizon so that NULL data can be converted to 0s later
  q.rf.data <- "SELECT p.phiid, fragvol, fragsize_l, fragsize_r, fragsize_h, fragshp, fraghard
  FROM
  (
 	SELECT DISTINCT phiid FROM phorizon_View_1
 	) as p
  LEFT OUTER JOIN phfrags_View_1 ON p.phiid = phfrags_View_1.phiidref;"

  # toggle selected set vs. local DB
  if (SS == FALSE) {
    q.rf.data <- gsub(pattern = '_View_1', replacement = '', x = q.rf.data, fixed = TRUE)
  }

  q.art.data <- paste0("SELECT p.phiid, huartvol, huartsize_l, huartsize_r, huartsize_h,
              huartkind, huartco, huartshp, huartrnd, huartpen, huartsafety, huartper,
                       recwlupdated, recuseriidref, phhuartiid
                       FROM (
                       SELECT DISTINCT phiid FROM phorizon_View_1
                       ) as p LEFT OUTER JOIN phhuarts ", ifelse(SS, "_View_1","") ,
                       " ON p.phiid = phiidref;")
  if (SS == FALSE) {
    q.art.data <- gsub(pattern = '_View_1', replacement = '', x = q.art.data, fixed = TRUE)
  }

  # new phfrags summary SQL
  q.rf.data.v2 <- "
  SET NOCOUNT ON

  -- find fragsize_r
  CREATE TABLE #RF1 (peiid INT, phiid INT, phfragsiid INT, fragvol REAL,
  shape CHAR(7), para INT, nonpara INT, fragsize_r2 INT);

  INSERT INTO  #RF1 (peiid, phiid, phfragsiid, fragvol, shape, para, nonpara, fragsize_r2)
  SELECT             peiid, phiid, phfragsiid, fragvol,
  -- shape
  CASE WHEN fragshp = 2 OR fragshp IS NULL THEN 'nonflat' ELSE 'flat' END shape,
  -- hardness
  CASE WHEN fraghard IN (6, 7, 9, 5, 10, 3, 14)                     THEN 1 ELSE NULL END para,
  CASE WHEN fraghard IN (11, 4, 8, 12, 2, 13)   OR fraghard IS NULL THEN 1 ELSE NULL END nonpara,
  -- fragsize_r
  CASE WHEN fragsize_r IS NOT NULL THEN fragsize_r
       WHEN fragsize_r IS NULL     AND fragsize_h IS NOT NULL AND fragsize_l IS NOT NULL
       THEN (fragsize_h + fragsize_l) / 2
       WHEN fragsize_h IS NOT NULL THEN fragsize_h
       WHEN fragsize_l IS NOT NULL THEN fragsize_l
       ELSE NULL END
  fragsize_r2

  FROM
  pedon_View_1    pe                           LEFT OUTER JOIN
  phorizon_View_1 ph ON ph.peiidref = pe.peiid LEFT OUTER JOIN
  phfrags_View_1  pf ON pf.phiidref = ph.phiid

  ORDER BY pe.peiid, ph.phiid, pf.phfragsiid;


  -- compute logicals
  CREATE TABLE #RF2 (
  peiid INT, phiid INT, phfragsiid INT, fragvol REAL, para INT, nonpara INT,
  fine_gravel INT, gravel INT, cobbles INT, stones INT, boulders INT, channers INT, flagstones INT,
  unspecified INT
  );
  INSERT INTO  #RF2 (
  peiid, phiid, phfragsiid, fragvol, para, nonpara,
  fine_gravel, gravel, cobbles, stones, boulders, channers, flagstones,
  unspecified
  )
  SELECT
  peiid, phiid, phfragsiid, fragvol, para, nonpara,
  -- fragments
  CASE WHEN   fragsize_r2 >= 2  AND fragsize_r2 <= 5   AND shape = 'nonflat' THEN 1 ELSE NULL END fine_gravel,
  CASE WHEN   fragsize_r2 >= 2  AND fragsize_r2 <= 76  AND shape = 'nonflat' THEN 1 ELSE NULL END gravel,
  CASE WHEN   fragsize_r2 > 76  AND fragsize_r2 <= 250 AND shape = 'nonflat' THEN 1 ELSE NULL END cobbles,
  CASE WHEN ((fragsize_r2 > 250 AND fragsize_r2 <= 600 AND shape = 'nonflat') OR
  (fragsize_r2 >= 380 AND fragsize_r2 < 600 AND shape = 'flat'))
  THEN 1 ELSE NULL END stones,
  CASE WHEN   fragsize_r2 > 600 THEN 1 ELSE NULL END boulders,
  CASE WHEN   fragsize_r2 >= 2  AND fragsize_r2 <= 150 AND shape = 'flat' THEN 1 ELSE NULL END channers,
  CASE WHEN   fragsize_r2 > 150 AND fragsize_r2 <= 380 AND shape = 'flat' THEN 1 ELSE NULL END flagstones,
  CASE WHEN   fragsize_r2 IS NULL                                         THEN 1 ELSE NULL END unspecified

  FROM
  #RF1

  ORDER BY peiid, phiid, phfragsiid;


  -- summarize rock fragments
  SELECT
  phiid,
  -- nonpara rock fragments
  SUM(fragvol * fine_gravel * nonpara)  fine_gravel,
  SUM(fragvol * gravel      * nonpara)  gravel,
  SUM(fragvol * cobbles     * nonpara)  cobbles,
  SUM(fragvol * stones      * nonpara)  stones,
  SUM(fragvol * boulders    * nonpara)  boulders,
  SUM(fragvol * channers    * nonpara)  channers,
  SUM(fragvol * flagstones  * nonpara)  flagstones,
  -- para rock fragments
  SUM(fragvol * fine_gravel * para)     parafine_gravel,
  SUM(fragvol * gravel      * para)     paragravel,
  SUM(fragvol * cobbles     * para)     paracobbles,
  SUM(fragvol * stones      * para)     parastones,
  SUM(fragvol * boulders    * para)     paraboulders,
  SUM(fragvol * channers    * para)     parachanners,
  SUM(fragvol * flagstones  * para)     paraflagstones,
  -- unspecified
  SUM(fragvol * unspecified)            unspecified,
  -- total_frags_pct_para
  SUM(fragvol               * nonpara)  total_frags_pct_nopf,
  -- total_frags_pct
  SUM(fragvol)                          total_frags_pct

  FROM #RF2

  GROUP BY peiid, phiid

  ORDER BY phiid;


  -- cleanup
  DROP TABLE #RF1;
  DROP TABLE #RF2;
  "
  # toggle selected set vs. local DB
  if (SS == FALSE) {
    q.rf.data.v2 <- gsub(pattern = '_View_1', replacement = '', x = q.rf.data.v2, fixed = TRUE)
  }


  # get horizon texture modifiers
  q.hz.texmod <- "SELECT phz.peiidref AS peiid, phz.phiid AS phiid, pht.phtiid AS phtiid, phtm.seqnum, texmod
  FROM
	phorizon_View_1 AS phz
  INNER JOIN phtexture_View_1 AS pht ON phz.phiid = pht.phiidref
	LEFT OUTER JOIN phtexturemod_View_1 AS phtm ON pht.phtiid = phtm.phtiidref;"

  # toggle selected set vs. local DB
  if (SS == FALSE) {
    q.hz.texmod <- gsub(pattern = '_View_1', replacement = '', x = q.hz.texmod, fixed = TRUE)
  }

  # get geomorphic features (sqlite safe -- no RIGHT JOIN 2020/12/02)
  q.geomorph <- "SELECT pedon_View_1.peiid, sitegeomordesc_View_1.geomfmod,
                         geomorfeat.geomfname, sitegeomordesc_View_1.geomfeatid,
                         sitegeomordesc_View_1.existsonfeat, sitegeomordesc_View_1.geomfiidref,
                         lower(geomorfeattype.geomftname) as geomftname
                  FROM pedon_View_1
                    INNER JOIN siteobs_View_1 ON siteobs_View_1.siteobsiid = pedon_View_1.siteobsiidref
                    INNER JOIN site_View_1 ON site_View_1.siteiid = siteobs_View_1.siteiidref
                    INNER JOIN sitegeomordesc_View_1 ON site_View_1.siteiid = sitegeomordesc_View_1.siteiidref
                    INNER JOIN geomorfeat ON  geomorfeat.geomfiid = sitegeomordesc_View_1.geomfiidref
                    INNER JOIN geomorfeattype ON geomorfeattype.geomftiid = geomorfeat.geomftiidref
                  ORDER BY peiid, geomfeatid ASC;"


  # toggle selected set vs. local DB
  if (SS == FALSE) {
    q.geomorph <- gsub(pattern = '_View_1', replacement = '', x = q.geomorph, fixed = TRUE)
  }


  q.taxhistory <- "SELECT peiidref as peiid, classdate, classifier, classtype, taxonname, localphase, taxonkind, seriesstatus, taxclname, taxpartsize, taxorder, taxsuborder, taxgrtgroup, taxsubgrp, soiltaxedition, osdtypelocflag, taxmoistcl, taxtempregime, taxfamother, psctopdepth, pscbotdepth
  	FROM
    petaxhistory_View_1 AS pth
    LEFT OUTER JOIN petaxhistmoistcl_View_1 AS pthm ON pth.petaxhistoryiid = pthm.pedtaxhistoryiidref
    LEFT OUTER JOIN petxhistfmother_View_1 AS ptho ON pth.petaxhistoryiid = ptho.pedtaxhistoryiidref
    ORDER BY pth.peiidref;"

  # toggle selected set vs. local DB
  if (SS == FALSE) {
    q.taxhistory <- gsub(pattern = '_View_1', replacement = '', x = q.taxhistory, fixed = TRUE)
  }


  q.sitepm <- "SELECT siteiidref as siteiid, seqnum, pmorder, pmdept, pmdepb, pmmodifier, pmgenmod, pmkind, pmorigin, pmweathering
  FROM
  sitepm_View_1 AS spm
  INNER JOIN site_View_1 AS s ON spm.siteiidref = s.siteiid;"

  # toggle selected set vs. local DB
  if (SS == FALSE) {
    q.sitepm <- gsub(pattern = '_View_1', replacement = '', x = q.sitepm, fixed = TRUE)
  }

  q.hz.desgn <- "SELECT phiid, seqnum, hzname, hzdept, hzdepb, desgndisc, desgnmaster, desgnmasterprime, desgnvert
  FROM phorizon_View_1 AS ph
  ORDER BY phiid;"

  # toggle selected set vs. local DB
  if (SS == FALSE) {
    q.hz.desgn <- gsub(pattern = '_View_1', replacement = '', x = q.hz.desgn, fixed = TRUE)
  }

  q.hz.dessuf <- "SELECT phiidref AS phiid, phs.seqnum, desgnsuffix
  FROM phdesgnsuffix_View_1 AS phs
  ORDER BY phiidref, seqnum ASC;"

  # toggle selected set vs. local DB
  if (SS == FALSE) {
    q.hz.dessuf <- gsub(pattern = '_View_1', replacement = '', x = q.hz.dessuf, fixed = TRUE)
  }

  channel <- dbConnectNASIS(dsn)

  if (inherits(channel, 'try-error'))
    return(data.frame())

	# exec queries
  d.ecosite <- dbQueryNASIS(channel, q.ecosite, close = FALSE)
  d.diagnostic <- dbQueryNASIS(channel, q.diagnostic, close = FALSE)
  d.restriction <- dbQueryNASIS(channel, q.restriction, close = FALSE)

  d.rf.data <- dbQueryNASIS(channel, q.rf.data, close = FALSE)
  # d.rf.data.v2 <- dbQueryNASIS(channel, q.rf.data.v2, close = FALSE)
  d.art.data <- dbQueryNASIS(channel, q.art.data, close = FALSE)

  d.surf.rf.summary <- dbQueryNASIS(channel, q.surf.rf.summary, close = FALSE)
  d.hz.texmod <- dbQueryNASIS(channel, q.hz.texmod, close = FALSE)
  d.geomorph <- dbQueryNASIS(channel, q.geomorph, close = FALSE)
  d.taxhistory <- dbQueryNASIS(channel, q.taxhistory, close = FALSE)
  d.photolink <- dbQueryNASIS(channel, q.photolink, close = FALSE)
  d.sitepm <- dbQueryNASIS(channel, q.sitepm, close = FALSE)
  d.structure <- dbQueryNASIS(channel, q.structure, close = FALSE)
  d.hz.desgn <- dbQueryNASIS(channel, q.hz.desgn, close = FALSE)
  d.hz.dessuf <- dbQueryNASIS(channel, q.hz.dessuf)

	## uncode the ones that need that here
	d.diagnostic <- uncode(d.diagnostic, stringsAsFactors = stringsAsFactors, dsn = dsn)
	d.restriction <- uncode(d.restriction, stringsAsFactors = stringsAsFactors, dsn = dsn)
	d.rf.data    <- uncode(d.rf.data, stringsAsFactors = stringsAsFactors, dsn = dsn)
	d.art.data  <-  uncode(d.art.data, stringsAsFactors = stringsAsFactors, dsn = dsn)
	d.hz.texmod  <- uncode(d.hz.texmod, stringsAsFactors = stringsAsFactors, dsn = dsn)
	# https://github.com/ncss-tech/soilDB/issues/53
	d.taxhistory <- uncode(d.taxhistory, stringsAsFactors = FALSE, dsn = dsn)
	d.sitepm     <- uncode(d.sitepm, stringsAsFactors = stringsAsFactors, dsn = dsn)
	d.structure  <- uncode(d.structure, stringsAsFactors = stringsAsFactors, dsn = dsn)
	d.hz.desgn <- uncode(d.hz.desgn, stringsAsFactors = stringsAsFactors, dsn = dsn)
	d.hz.dessuf <- uncode(d.hz.dessuf, stringsAsFactors = stringsAsFactors, dsn = dsn)

	## the following steps will not work when data are missing from local DB or SS
	# return NULL in those cases

	if (nrow(d.diagnostic) > 0) {
	  # generate wide-formatted, diagnostic boolean summary
	  d.diag.boolean <- .diagHzLongtoWide(d.diagnostic)
	} else {
	  d.diag.boolean <- NULL
	}

	if (nrow(d.hz.dessuf) > 0) {
	  # generate wide-formatted, hz suffix boolean summary
	  d.hzdesgnsuf.boolean <- .hzSuffixLongtoWide(d.hz.dessuf)
	  d.hz.desgn <- merge(d.hz.desgn, d.hzdesgnsuf.boolean, by = 'phiid',
	                      sort = FALSE, all.x = TRUE, incomparables = NA)
	} else {
	  d.hz.desgn <- NULL
	}

	if (nrow(d.photolink) > 0) {
	  # parse imagename and imagepath for photo links
	  ncharpath <- nchar(d.photolink$imagepath)

	  # windows max path length is 260 (unless overridden in registry?)
	  if (any(ncharpath > 260))
	    warning("some image paths are too long (>260 characters) for basename()")

	  bad.idx <- which(ncharpath > 260)
	  d.photolink$imagepath[bad.idx] <- substr(d.photolink$imagepath[bad.idx],
	                                           start = ncharpath[bad.idx] - 260,
	                                           stop = ncharpath[bad.idx])
	  d.photolink$imagename <- try(basename(d.photolink$imagepath))
	}

	d.rf.summary <- simplifyFragmentData(d.rf.data, id.var='phiid', nullFragsAreZero = nullFragsAreZero)

	# summarize rock fragment data
	if (nrow(d.rf.data) > 0) {
	  # keep track of UNIQUE original phiids so that we can optionally fill NA with 0 in a second pass
	  all.ids <- unique(d.rf.data[, 'phiid', drop=FALSE])

	  # left join
	  d.rf.summary <- merge(all.ids, d.rf.summary, by = 'phiid',
	                       sort = FALSE, all.x = TRUE, incomparables = NA)
	  ## basic checks for problematic data

	  # recent NSSH changes to gravel/cobble threshold 76mm -> 75mm
	  qc.idx <- which(d.rf.data$fragsize_h == 76)
	  if(length(qc.idx) > 0) {
	    msg <- sprintf('-> QC: some fragsize_h values == 76mm, may be mis-classified as cobbles [%i / %i records]', length(qc.idx), nrow(d.rf.data))
	    message(msg)
	  }

	}

	if (nullFragsAreZero) {
	    # iterate over every column except for the ID
	    nm <- names(d.rf.summary)
	    nm <- nm[grep('phiid', nm, fixed = TRUE, invert = TRUE)]

	    # a for-loop seems fine
	    for (v in nm) {
	      d.rf.summary[[v]] <- ifelse(is.na(d.rf.summary[[v]]), 0, d.rf.summary[[v]])
	    }
	}

	# artifact summary
	d.art.summary <- simplifyArtifactData(d.art.data, id.var='phiid', nullFragsAreZero = nullFragsAreZero)

	if (nrow(d.art.data) > 0) {

	  art.all.ids <- unique(d.art.data[, 'phiid', drop=FALSE])
	  d.art.summary <- merge(art.all.ids, d.art.summary, by = 'phiid',
	                        sort = FALSE, all.x = TRUE, incomparables = NA)
	  # recent NSSH changes to gravel/cobble threshold 76mm -> 75mm
	  qc.idx <- which(d.art.data$huartsize_h == 76)
	  if (length(qc.idx) > 0) {
	    msg <- sprintf('-> QC: some huartsize_h values == 76mm, may be mis-classified as cobbles [%i / %i records]', length(qc.idx), nrow(d.art.data))
	    message(msg)
	  }
  }

	if (nullFragsAreZero) {
	  nm <- names(d.art.summary)
	  nm <- nm[grep("phiid", nm, fixed = TRUE, invert = TRUE)]

	  # a for-loop seems fine
	  for (v in nm) {
	    d.art.summary[[v]] <- ifelse(is.na(d.art.summary[[v]]), 0, d.art.summary[[v]])
	  }
	}

	# r.rf.data.v2 nullFragsAreZero = TRUE
	# idx <- !names(d.rf.data.v2) %in% "phiid"
	# if (nullFragsAreZero == TRUE) {
	#   d.rf.data.v2[idx] <- lapply(d.rf.data.v2[idx], function(x) ifelse(is.na(x), 0, x))
	# }


	# return a list of results
	return(list(ecositehistory = d.ecosite,
							diagnostic = d.diagnostic,
							diagHzBoolean = d.diag.boolean,
							restriction = d.restriction,
							frag_summary = d.rf.summary,
							# frag_summary_v2 = d.rf.data.v2,
							art_summary = d.art.summary,
							surf_frag_summary = d.surf.rf.summary,
							texmodifier = d.hz.texmod,
							geomorph = d.geomorph,
							taxhistory = d.taxhistory,
						  photo = d.photolink,
							pm = d.sitepm,
              struct = d.structure,
							hzdesgn = d.hz.desgn))
}

