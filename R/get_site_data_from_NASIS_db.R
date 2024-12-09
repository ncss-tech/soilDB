#' Get Site Data from a local NASIS Database
#'
#' Get site-level data from a local NASIS database.
#' 
#' @details
#' 
#' It is possible to have multiple pedon records per site observation and multiple site observations
#' per site, which will result in multiple records per site. See argument `include_pedon=FALSE` to 
#' omit joins to pedon and transect tables.
#' 
#' The following aggregations of child table information are performed by this function: 
#' 
#'  - Site Area Overlap for State, County and MLRA are returned for related area records, as specified in the site table, as the following column names: `site_state`, `site_county`, and `site_mlra`.
#' 
#'  - Site Observation Surface Fragment data are simplified (converted to wide format) using `simplifyFragmentData()`.
#' 
#'  - The best Ecological Site History record is selected using `get_ecosite_history_from_NASIS_db(best = TRUE)`.
#' 
#'  - Site Other Vegetation Class information is aggregated by class name, using `" & "` as the 
#' separator when multiple classes are assigned.
#' 
#'  - When multiple Site Bedrock entries are present, only the shallowest is returned by this 
#' function. In lieu of bedrock depth the first record in the table is returned.
#'
#' @param SS fetch data from Selected Set in NASIS or from the entire local
#' database (default: `TRUE`)
#' @param include_pedon Include pedon and transect data joined to site? (default: `TRUE`)
#' @param nullFragsAreZero should surface fragment cover percentages of `NULL` be interpreted as `0`? (default: `TRUE`)
#' @param stringsAsFactors deprecated
#' @param dsn Optional: path to local SQLite database containing NASIS table structure; default: `NULL`
#'
#' @return A data.frame
#'
#' @author Jay M. Skovlin and Dylan E. Beaudette
#' @seealso [get_hz_data_from_NASIS_db()], [fetchNASIS()], [fetchVegdata()]
#' @keywords manip
#'
#' @export get_site_data_from_NASIS_db
get_site_data_from_NASIS_db <- function(SS = TRUE,
                                        include_pedon = TRUE,
                                        nullFragsAreZero = TRUE,
                                        stringsAsFactors = NULL,
                                        dsn = NULL) {
  .SD <- NULL
  
  if (!missing(stringsAsFactors) && is.logical(stringsAsFactors)) {
    .Deprecated(msg = sprintf("stringsAsFactors argument is deprecated.\nSetting package option with `NASISDomainsAsFactor(%s)`", stringsAsFactors))
    NASISDomainsAsFactor(stringsAsFactors)
  }
  
	q <- paste0("SELECT siteiid, siteobsiid, CAST(usiteid AS varchar(60)) as site_id, 
  	", ifelse(include_pedon, "peiid, CAST(upedonid AS varchar(60)) as pedon_id, ", ""), "
  	obsdate as obs_date, utmzone, utmeasting, utmnorthing, horizdatnm, 
    longstddecimaldegrees as x_std, latstddecimaldegrees as y_std, longstddecimaldegrees, latstddecimaldegrees, gpspositionalerror, 
    ", ifelse(include_pedon, "descname as describer, pedonpurpose, pedontype, pedlabsampnum, labdatadescflag, tsectstopnum, tsectinterval, utransectid, tsectkind, tsectselmeth, erocl,", ""), "
    elev as elev_field, slope as slope_field, aspect as aspect_field, 
    ecostatename, ecostateid, commphasename, commphaseid, plantassocnm, 
    siteobs_View_1.earthcovkind1, siteobs_View_1.earthcovkind2, 
    bedrckdepth, bedrckkind, bedrckhardness, pmgroupname, 
    hillslopeprof, geomslopeseg, shapeacross, shapedown, slopecomplex, drainagecl,
    geomposhill, geomposmntn, geompostrce, geomposflats, swaterdepth,
    flodfreqcl, floddurcl, flodmonthbeg, pondfreqcl, ponddurcl, pondmonthbeg, 
    climstaid, climstanm, climstatype, ffd, map, reannualprecip, airtempa, soiltempa, airtemps, soiltemps, airtempw, soiltempw

  FROM
  
  site_View_1 INNER JOIN siteobs_View_1 ON site_View_1.siteiid = siteobs_View_1.siteiidref
  ", ifelse(include_pedon, "LEFT OUTER JOIN pedon_View_1 ON siteobs_View_1.siteobsiid = pedon_View_1.siteobsiidref
  LEFT OUTER JOIN transect_View_1 ON pedon_View_1.tsectiidref = transect_View_1.tsectiid", ""),"
  LEFT OUTER JOIN
  (
        SELECT siteiidref, bedrckdepth, bedrckkind, bedrckhardness, ROW_NUMBER() OVER(PARTITION BY siteiidref ORDER BY bedrckdepth ASC) as rn
        FROM sitebedrock_View_1
  ) as sb ON site_View_1.siteiid = sb.siteiidref

WHERE sb.rn IS NULL OR sb.rn = 1

ORDER BY siteobs_View_1.siteobsiid;")
      
  q2 <- paste0("SELECT siteiid, siteobsiid, 
         ", ifelse(include_pedon, "peiid, ", ""), "
         sitesurffrags_View_1.* FROM sitesurffrags_View_1 
         INNER JOIN siteobs_View_1 ON sitesurffrags_View_1.siteobsiidref = siteobs_View_1.siteobsiid
         INNER JOIN site_View_1 ON siteobs_View_1.siteiidref = site_View_1.siteiid
         ", ifelse(include_pedon, "LEFT OUTER JOIN pedon_View_1 ON siteobs_View_1.siteobsiid = pedon_View_1.siteobsiidref", ""), "
         ORDER BY siteobs_View_1.siteobsiid;")

  q3 <- "SELECT site_View_1.siteiid, ovegclid, ovegclname, ovegcldesc, ovegcliid 
        FROM site_View_1
        INNER JOIN siteothvegclass_View_1 ON siteothvegclass_View_1.siteiidref = site_View_1.siteiid
        INNER JOIN othvegclass ON siteothvegclass_View_1.ovegcliidref = othvegclass.ovegcliid"

  q4 <- "SELECT siteiid, area.areasymbol AS site_state FROM area 
         LEFT JOIN site_View_1 ON area.areaiid = site_View_1.stateareaiidref"
  q5 <- "SELECT siteiid, area.areasymbol AS site_county FROM area 
         LEFT JOIN site_View_1 ON area.areaiid = site_View_1.countyareaiidref"
  q6 <- "SELECT siteiid, area.areasymbol AS site_mlra FROM area 
         LEFT JOIN site_View_1 ON area.areaiid = site_View_1.mlraareaiidref"
  
  channel <- dbConnectNASIS(dsn)

  if (inherits(channel, 'try-error'))
    return(data.frame())

	# toggle selected set vs. local DB
	if (SS == FALSE) {
	  q <- gsub(pattern = '_View_1', replacement = '', x = q, fixed = TRUE)
	  q2 <- gsub(pattern = '_View_1', replacement = '', x = q2, fixed = TRUE)
	  q3 <- gsub(pattern = '_View_1', replacement = '', x = q3, fixed = TRUE)
	  q4 <- gsub(pattern = '_View_1', replacement = '', x = q4, fixed = TRUE)
	  q5 <- gsub(pattern = '_View_1', replacement = '', x = q5, fixed = TRUE)
	  q6 <- gsub(pattern = '_View_1', replacement = '', x = q6, fixed = TRUE)
	}

	# exec query
	d <- dbQueryNASIS(channel, q, close = FALSE)

	# ## this shouldn't happen, retain for debugging
	# # test for an error
	if (inherits(d, 'try-error'))
	  stop('error in SQL')

	# uncode domain columns
	d <- uncode(d, dsn = dsn)
	
	# surface fragments
	sfr <- dbQueryNASIS(channel, q2, close = FALSE)
	
	multi.siteobs <- unique(sfr[, c("siteiid","siteobsiid")])
	multisite <- table(multi.siteobs$siteiid) 
	if (any(multisite > 1)) {
	  assign("multisiteobs.surface", value = multi.siteobs[multi.siteobs$siteiid %in% names(multisite[multisite > 1]),], envir = get_soilDB_env())
	}
	
	phs <- simplifyFragmentData(
	  uncode(sfr, dsn = dsn),
	  id.var = "siteobsiid",
	  vol.var = "sfragcov",
	  prefix = "sfrag",
	  msg = "surface fragment cover"
	)
	
	colnames(phs) <- paste0("surface_", colnames(phs))
	colnames(phs)[1] <- "siteobsiid"
	
	if (nrow(d) > 0) {
  	ldx <- !d$siteobsiid %in% phs$siteobsiid
  	if (!any(ldx)) {
  	  phs <- phs[1:nrow(d),]
  	  phs$siteobsiid <- d$siteobsiid
  	} else {
  	  phs_null <- phs[0,][1:sum(ldx),]
  	  phs_null$siteobsiid <- d$siteobsiid[ldx]
  	  phs <- rbind(phs, phs_null)
  	}
  	
  	# handle NA for totals
  	if (nullFragsAreZero) {
  	  phs[is.na(phs)] <- 0
  	} 
  	d2 <- merge(d, phs, by = "siteobsiid", all.x = TRUE, sort = FALSE)
	} else {
	  d2 <- cbind(d, phs[0,])
	}
	
	# short-circuit: 0 rows means nothing in the selected set and thus we stop here
	if (nrow(d2) == 0) {
	  return(d2)
	}

	# join in "best" ecological site
	bes <- get_ecosite_history_from_NASIS_db(SS = SS, best = TRUE, dsn = dsn)
	if (length(bes) > 0) {
	  d2 <- merge(d2, bes, by = "siteiid", all.x = TRUE, sort = FALSE)
	}
	
	# join-in othervegclass string
	sov <- try(dbQueryNASIS(channel, q3, close = FALSE))
	
	if (!inherits(sov, 'try-error') && nrow(sov) > 0) {
  	ov <- data.table::data.table(sov)[, .formatOtherVegString(.SD, id.name = "siteiid", name.sep = ' & '), 
  	                                         by = "siteiid", .SDcols = colnames(sov)] 
  	ov$siteiid <- NULL
  	if (nrow(ov) > 0) {
  	  d2 <- merge(d2, ov, by = "siteiid", all.x = TRUE, sort = FALSE)
  	}
	}
	
	# join-in state, county, mlra string
	state <- try(dbQueryNASIS(channel, q4, close = FALSE))
	county <- try(dbQueryNASIS(channel, q5, close = FALSE))
	mlra <- try(dbQueryNASIS(channel, q6))
	
	if (!inherits(state, 'try-error') && nrow(state) > 0) {
	  d2 <- merge(d2, state, by = "siteiid", all.x = TRUE, sort = FALSE)
	}
	
	if (!inherits(county, 'try-error') && nrow(state) > 0) {
	  d2 <- merge(d2, county, by = "siteiid", all.x = TRUE, sort = FALSE)
	}
	
	if (!inherits(mlra, 'try-error') && nrow(state) > 0) {
	  d2 <- merge(d2, mlra, by = "siteiid", all.x = TRUE, sort = FALSE)
	}
	
  # https://github.com/ncss-tech/soilDB/issues/41
	# warn if mixed datums
	# if (length(na.omit(unique(d2$horizdatnm))) > 1)
	# 	message('multiple horizontal datums present, consider using WGS84 coordinates (x_std, y_std)')

	# are there any duplicate pedon IDs?
	t.pedon_id <- table(d2$pedon_id)
	not.unique.pedon_id <- t.pedon_id > 1
	if (any(not.unique.pedon_id))
	  assign('dup.pedon.ids', value = names(t.pedon_id[which(not.unique.pedon_id)]), envir = get_soilDB_env())

	# warn about sites without a matching pedon (records missing peiid)
	missing.pedon <- which(is.na(d2$peiid))
	if (length(missing.pedon) > 0)
	  assign('sites.missing.pedons', value = unique(d2$site_id[missing.pedon]), envir = get_soilDB_env())

  ## set factor levels, when it makes sense
	# most of these are done via uncode()

  # surface shape
	d2$shapeacross <- factor(d2$shapeacross, levels = c('concave', 'linear', 'convex', 'undulating', 'complex'))
	d2$shapedown <- factor(d2$shapedown, levels = c('concave', 'linear', 'convex', 'undulating', 'complex'))

  # create 3D surface shape
  d2$slope_shape <- paste0(d2$shapeacross, ' / ', d2$shapedown)

  # make reasonable levels for 3D slope shape
  ss.grid <- expand.grid(na.omit(unique(d2$shapeacross)), na.omit(unique(d2$shapedown)))
  ss.levels <- apply(ss.grid, 1, function(i) { paste(rev(i), collapse = ' / ')})
  d2$slope_shape <- factor(d2$slope_shape, levels = ss.levels)

	# done
	return(d2)
}

