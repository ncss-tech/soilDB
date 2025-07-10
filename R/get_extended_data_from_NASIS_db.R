#' Get accessory tables and summaries from a local NASIS Database
#'
#' @param SS get data from the currently loaded Selected Set in NASIS or from
#' the entire local database (default: `TRUE`)
#' @param nullFragsAreZero should fragment volumes of NULL be interpreted as 0?
#' (default: TRUE), see details
#' @param stringsAsFactors deprecated
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
                                            stringsAsFactors = NULL,
                                            dsn = NULL) {

  if (!missing(stringsAsFactors) && is.logical(stringsAsFactors)) {
    .Deprecated(msg = sprintf("stringsAsFactors argument is deprecated.\nSetting package option with `NASISDomainsAsFactor(%s)`", stringsAsFactors))
    NASISDomainsAsFactor(stringsAsFactors)
  }
  
  # photo links from PedonPC stored as sitetext notes
  q.photolink <- "SELECT so.siteiidref AS siteiid, sot.recdate, sot.textcat,  CAST(sot.textentry AS text) AS imagepath
  FROM
  siteobs_View_1 AS so
  LEFT OUTER JOIN siteobstext_View_1 AS sot ON so.siteobsiid = sot.siteobsiidref
  WHERE sot.textcat LIKE 'Photo%'
  ORDER BY sot.siteobstextkind;"

  # toggle selected set vs. local DB
  if (isFALSE(SS)) {
    q.photolink <- gsub(pattern = '_View_1', replacement = '', x = q.photolink, fixed = TRUE)
  }


  # get all structure records / horizon
  q.structure <- "SELECT phiidref as phiid, structgrade, structsize, structtype, structid, structpartsto
	FROM phstructure_View_1
	WHERE structtype IS NOT NULL
	ORDER BY phiidref, structid ASC;"

  # toggle selected set vs. local DB
  if (isFALSE(SS)) {
    q.structure <- gsub(pattern = '_View_1', replacement = '', x = q.structure, fixed = TRUE)
  }

  # query diagnostic horizons, usually a 1:many relationship with pedons
  q.diagnostic <- "SELECT peiidref as peiid, featkind, featdept, featdepb
  FROM
  pediagfeatures_View_1 AS pdf
	ORDER BY pdf.peiidref, pdf.featdept;"

  # toggle selected set vs. local DB
  if (isFALSE(SS)) {
    q.diagnostic <- gsub(pattern = '_View_1', replacement = '', x = q.diagnostic, fixed = TRUE)
  }

  q.restriction <- "SELECT peiidref as peiid, resdept, resdepb, resthk_l, resthk_r, resthk_h, reskind, reshard
FROM perestrictions_View_1 As prf
	ORDER BY prf.peiidref, prf.resdept;"

  # toggle selected set vs. local DB
  if (isFALSE(SS)) {
    q.restriction <- gsub(pattern = '_View_1', replacement = '', x = q.restriction, fixed = TRUE)
  }

  # base table is phorizon so that NULL data can be converted to 0s later
  q.rf.data <- "SELECT p.phiid, fragvol, fragsize_l, fragsize_r, fragsize_h, fragshp, fraghard
  FROM
  (
 	SELECT DISTINCT phiid FROM phorizon_View_1
 	) as p
  LEFT OUTER JOIN phfrags_View_1 ON p.phiid = phfrags_View_1.phiidref;"

  # toggle selected set vs. local DB
  if (isFALSE(SS)) {
    q.rf.data <- gsub(pattern = '_View_1', replacement = '', x = q.rf.data, fixed = TRUE)
  }

  q.art.data <- paste0("SELECT p.phiid, huartvol, huartsize_l, huartsize_r, huartsize_h,
              huartkind, huartco, huartshp, huartrnd, huartpen, huartsafety, huartper,
                       recwlupdated, recuseriidref, phhuartiid
                       FROM (
                       SELECT DISTINCT phiid FROM phorizon_View_1
                       ) as p LEFT OUTER JOIN phhuarts_View_1 ON p.phiid = phiidref;")
  if (isFALSE(SS)) {
    q.art.data <- gsub(pattern = '_View_1', replacement = '', x = q.art.data, fixed = TRUE)
  }

  # get horizon texture modifiers
  q.hz.texmod <- "SELECT phz.peiidref AS peiid, phz.phiid AS phiid, pht.phtiid AS phtiid, phtm.seqnum, texmod
  FROM
	phorizon_View_1 AS phz
  INNER JOIN phtexture_View_1 AS pht ON phz.phiid = pht.phiidref
	LEFT OUTER JOIN phtexturemod_View_1 AS phtm ON pht.phtiid = phtm.phtiidref;"

  # toggle selected set vs. local DB
  if (isFALSE(SS)) {
    q.hz.texmod <- gsub(pattern = '_View_1', replacement = '', x = q.hz.texmod, fixed = TRUE)
  }

  # get geomorphic features (sqlite safe -- no RIGHT JOIN 2020/12/02)
  q.geomorph <- "SELECT pedon_View_1.peiid, 
                        siteobs_View_1.geomicrorelief, siteobs_View_1.geommicelev,
                        sitegeomordesc_View_1.geomfmod,
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
  if (isFALSE(SS)) {
    q.geomorph <- gsub(pattern = '_View_1', replacement = '', x = q.geomorph, fixed = TRUE)
  }


  q.taxhistory <- "SELECT peiidref as peiid, classdate, classifier, classtype, taxonname, localphase, taxonkind, seriesstatus, taxclname, taxpartsize, taxorder, taxsuborder, taxgrtgroup, taxsubgrp, soiltaxedition, osdtypelocflag, taxmoistcl, taxtempregime, taxfamother, taxreaction, taxfamhahatmatcl, psctopdepth, pscbotdepth
  	FROM
    petaxhistory_View_1 AS pth
    LEFT OUTER JOIN petaxhistmoistcl_View_1 AS pthm ON pth.petaxhistoryiid = pthm.pedtaxhistoryiidref
    LEFT OUTER JOIN petxhistfmother_View_1 AS ptho ON pth.petaxhistoryiid = ptho.pedtaxhistoryiidref
    ORDER BY pth.peiidref;"

  # toggle selected set vs. local DB
  if (isFALSE(SS)) {
    q.taxhistory <- gsub(pattern = '_View_1', replacement = '', x = q.taxhistory, fixed = TRUE)
  }


  q.sitepm <- "SELECT siteiidref as siteiid, seqnum, pmorder, pmdept, pmdepb, pmmodifier, pmgenmod, pmkind, pmorigin 
  FROM
  sitepm_View_1 AS spm
  INNER JOIN site_View_1 AS s ON spm.siteiidref = s.siteiid
  ORDER BY spm.siteiidref, pmorder;"

  # toggle selected set vs. local DB
  if (isFALSE(SS)) {
    q.sitepm <- gsub(pattern = '_View_1', replacement = '', x = q.sitepm, fixed = TRUE)
  }

  q.hz.desgn <- "SELECT phiid, seqnum, hzname, hzdept, hzdepb, desgndisc, desgnmaster, desgnmasterprime, desgnvert
  FROM phorizon_View_1 AS ph
  ORDER BY phiid;"

  # toggle selected set vs. local DB
  if (isFALSE(SS)) {
    q.hz.desgn <- gsub(pattern = '_View_1', replacement = '', x = q.hz.desgn, fixed = TRUE)
  }

  q.hz.dessuf <- "SELECT phiidref AS phiid, phs.seqnum, desgnsuffix
  FROM phdesgnsuffix_View_1 AS phs
  ORDER BY phiidref, seqnum ASC;"

  # toggle selected set vs. local DB
  if (isFALSE(SS)) {
    q.hz.dessuf <- gsub(pattern = '_View_1', replacement = '', x = q.hz.dessuf, fixed = TRUE)
  }
  
  # get siteaoverlap data
  q.siteaoverlap <- "SELECT siteaoverlap_View_1.siteiidref as siteiid, 
                        siteaoverlap_View_1.seqnum,                         
			area.areasymbol, area.areaname, areatype.areatypename
                  FROM siteaoverlap_View_1
                    INNER JOIN area ON area.areaiid = siteaoverlap_View_1.areaiidref
		                INNER JOIN areatype ON areatype.areatypeiid = area.areatypeiidref
                  ORDER BY siteiidref ASC;"
  
  # toggle selected set vs. local DB
  if (isFALSE(SS)) {
    q.siteaoverlap <- gsub(pattern = '_View_1', replacement = '', x = q.siteaoverlap, fixed = TRUE)
  }  

  channel <- dbConnectNASIS(dsn)

  if (inherits(channel, 'try-error'))
    return(data.frame())
  
	# exec queries
  d.ecosite <- get_ecosite_history_from_NASIS_db(SS = SS, dsn = channel) # this query has been abstracted out
  d.diagnostic <- dbQueryNASIS(channel, q.diagnostic, close = FALSE)
  d.restriction <- dbQueryNASIS(channel, q.restriction, close = FALSE)

  d.rf.data <- dbQueryNASIS(channel, q.rf.data, close = FALSE)
  d.art.data <- dbQueryNASIS(channel, q.art.data, close = FALSE)
  
  # 2021-11-05: leaving this in the extended data result for now, but no longer used in fetchNASIS('pedons')
  # 2024-09-24: surface fragments have been handled by simplifyFragmentData for some time 
  #             removing query because it relies on domain information internally 
  #             decoding domains should all be handled through uncode()/standard NASIS metadata options
  # d.surf.rf.summary <- dbQueryNASIS(channel, q.surf.rf.summary, close = FALSE)

  d.hz.texmod <- dbQueryNASIS(channel, q.hz.texmod, close = FALSE)
  d.geomorph <- dbQueryNASIS(channel, q.geomorph, close = FALSE)
  d.taxhistory <- dbQueryNASIS(channel, q.taxhistory, close = FALSE)
  d.photolink <- dbQueryNASIS(channel, q.photolink, close = FALSE)
  d.sitepm <- dbQueryNASIS(channel, q.sitepm, close = FALSE)
  d.structure <- dbQueryNASIS(channel, q.structure, close = FALSE)
  d.hz.desgn <- dbQueryNASIS(channel, q.hz.desgn, close = FALSE)
  d.hz.dessuf <- dbQueryNASIS(channel, q.hz.dessuf, close = FALSE)
  d.siteaoverlap <- dbQueryNASIS(channel, q.siteaoverlap)

	## uncode the ones that need that here
	d.diagnostic <- uncode(d.diagnostic, dsn = dsn)
	d.restriction <- uncode(d.restriction, dsn = dsn)
	d.rf.data    <- uncode(d.rf.data, dsn = dsn)
	d.art.data  <-  uncode(d.art.data, dsn = dsn)
	d.hz.texmod  <- uncode(d.hz.texmod, dsn = dsn)
	d.geomorph  <- uncode(d.geomorph, dsn = dsn)
	# ensure that taxhistory is always character
	d.taxhistory[] <- lapply(uncode(d.taxhistory, dsn = dsn), 
	                         function(x) if (is.factor(x)) as.character(x) else x)
	d.sitepm     <- uncode(d.sitepm, dsn = dsn)
	d.structure  <- uncode(d.structure, dsn = dsn)
	d.hz.desgn <- uncode(d.hz.desgn, dsn = dsn)
	d.hz.dessuf <- uncode(d.hz.dessuf, dsn = dsn)

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
	  if (any(ncharpath > 260, na.rm = TRUE)) {
	    warning("some image paths are too long (>260 characters) for basename()")

  	  bad.idx <- which(ncharpath > 260)
  	  d.photolink$imagepath[bad.idx] <- substr(d.photolink$imagepath[bad.idx],
  	                                           start = ncharpath[bad.idx] - 260,
  	                                           stop = ncharpath[bad.idx])
  	  d.photolink$imagename <- try(basename(d.photolink$imagepath))
	  }
	}

	d.rf.summary <- simplifyFragmentData(d.rf.data, id.var='phiid', nullFragsAreZero = nullFragsAreZero)

	# summarize rock fragment data
	if (nrow(d.rf.data) > 0) {
	  # keep track of UNIQUE original phiids so that we can optionally fill NA with 0 in a second pass
	  all.ids <- unique(d.rf.data[, 'phiid', drop = FALSE])

	  # left join
	  d.rf.summary <- merge(all.ids, d.rf.summary, by = 'phiid',
	                       sort = FALSE, all.x = TRUE, incomparables = NA)
	  ## basic checks for problematic data

	  # 2022/05/18: removed this QC warning, the "change" to 75mm was a typographical error
	  
	  # recent NSSH changes to gravel/cobble threshold 76mm -> 75mm
	  # qc.idx <- which(d.rf.data$fragsize_h == 76)
	  # if (length(qc.idx) > 0) {
	  #   msg <- sprintf('-> QC: some fragsize_h values == 75mm, may be mis-classified as cobbles [%i / %i records]', length(qc.idx), nrow(d.rf.data))
	  #   message(msg)
	  # }

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

	  art.all.ids <- unique(d.art.data[, 'phiid', drop = FALSE])
	  d.art.summary <- merge(art.all.ids, d.art.summary, by = 'phiid',
	                        sort = FALSE, all.x = TRUE, incomparables = NA)
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
	            siteaoverlap = d.siteaoverlap,
							diagnostic = d.diagnostic,
							diagHzBoolean = d.diag.boolean,
							restriction = d.restriction,
							frag_summary = d.rf.summary,
							# frag_summary_v2 = d.rf.data.v2,
							art_summary = d.art.summary,
							# surf_frag_summary = d.surf.rf.summary,
							texmodifier = d.hz.texmod,
							geomorph = d.geomorph,
							taxhistory = d.taxhistory,
						  photo = d.photolink,
							pm = d.sitepm,
              struct = d.structure,
							hzdesgn = d.hz.desgn))
}

