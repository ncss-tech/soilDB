## TODO: temporary hack to deal with the possibility of multiple site-bedrock entries:
# row number is computed for the site-bedrock table... we can't depend on the verticle order column to contain useful information
# we then filter on sb.rn at the end of the query.
# this same syntax will not work in Access

## further ideas:
# http://stackoverflow.com/questions/3800551/select-first-row-in-each-group-by-group
# ... this would be a lot cleaner if we used WITH ... to define all sub-tables

## TODO: spatial data will likely be referenced to multiple datums...
# the STD coordiants in NASIS are WGS84, but have to be manually "calculated"
# see: Import of Standard WGS84 Georeference
# for now, 'longstddecimaldegrees' and 'latstddecimaldegrees' are read-in as new site-level attributes
# ... this needs to be synced to PedonPC functions

## TODO: multiple records / site in siteobs are possible and will result in duplicate data

#' Get Site Data from a local NASIS Database
#'
#' Get site-level data from a local NASIS database.
#'
#' When multiple "site bedrock" entries are present, only the shallowest is
#' returned by this function.
#'
#' @param SS fetch data from Selected Set in NASIS or from the entire local
#' database (default: `TRUE`)
#' 
#' @param nullFragsAreZero should surface fragment cover percentages of NULL be interpreted as 0? (default: TRUE)
#'
#' @param stringsAsFactors logical: should character vectors be converted to
#' factors? This argument is passed to the `uncode()` function. It does not
#' convert those vectors that have been set outside of `uncode()` (i.e. hard
#' coded).
#'
#' @param dsn Optional: path to local SQLite database containing NASIS
#' table structure; default: `NULL`
#'
#' @return A data.frame
#'
#' @author Jay M. Skovlin and Dylan E. Beaudette
#' @seealso \code{\link{get_hz_data_from_NASIS_db}}
#' @keywords manip
#'
#' @export get_site_data_from_NASIS_db
get_site_data_from_NASIS_db <- function(SS = TRUE,
                                        nullFragsAreZero = TRUE,
                                        stringsAsFactors = default.stringsAsFactors(),
                                        dsn = NULL) {

	q <- "SELECT siteiid as siteiid, peiid, CAST(usiteid AS varchar(60)) as site_id, CAST(upedonid AS varchar(60)) as pedon_id, obsdate as obs_date,
utmzone, utmeasting, utmnorthing, -(longdegrees + CASE WHEN longminutes IS NULL THEN 0.0 ELSE longminutes / 60.0 END + CASE WHEN longseconds IS NULL THEN 0.0 ELSE longseconds / 60.0 / 60.0 END) as x, latdegrees + CASE WHEN latminutes IS NULL THEN 0.0 ELSE latminutes / 60.0 END + CASE WHEN latseconds IS NULL THEN 0.0 ELSE latseconds / 60.0 / 60.0 END as y, horizdatnm, longstddecimaldegrees as x_std, latstddecimaldegrees as y_std,
gpspositionalerror, descname as describer, pedonpurpose, pedontype, pedlabsampnum, labdatadescflag,
tsectstopnum, tsectinterval, utransectid, tsectkind, tsectselmeth,
elev as elev_field, slope as slope_field, aspect as aspect_field, plantassocnm, siteobs_View_1.earthcovkind1, siteobs_View_1.earthcovkind2, erocl, bedrckdepth, bedrckkind, bedrckhardness, hillslopeprof, geomslopeseg, shapeacross, shapedown, slopecomplex, drainagecl,
flodfreqcl, floddurcl, flodmonthbeg, pondfreqcl, ponddurcl, pondmonthbeg, geomposhill, geomposmntn, geompostrce, geomposflats, swaterdepth

FROM

site_View_1 INNER JOIN siteobs_View_1 ON site_View_1.siteiid = siteobs_View_1.siteiidref
LEFT OUTER JOIN pedon_View_1 ON siteobs_View_1.siteobsiid = pedon_View_1.siteobsiidref
LEFT OUTER JOIN transect_View_1 ON pedon_View_1.tsectiidref = transect_View_1.tsectiid
LEFT OUTER JOIN
(
      SELECT siteiidref, bedrckdepth, bedrckkind, bedrckhardness, ROW_NUMBER() OVER(PARTITION BY siteiidref ORDER BY bedrckdepth ASC) as rn
      FROM sitebedrock_View_1
) as sb ON site_View_1.siteiid = sb.siteiidref

WHERE sb.rn IS NULL OR sb.rn = 1

ORDER BY pedon_View_1.peiid ;"
      
  q2 <- "SELECT siteiid, peiid, siteobsiid, sitesurffrags_View_1.* FROM sitesurffrags_View_1 
         INNER JOIN siteobs_View_1 ON sitesurffrags_View_1.siteobsiidref = siteobs_View_1.siteobsiid
         INNER JOIN site_View_1 ON siteobs_View_1.siteiidref = site_View_1.siteiid
         LEFT OUTER JOIN pedon_View_1 ON siteobs_View_1.siteobsiid = pedon_View_1.siteobsiidref
         ORDER BY pedon_View_1.peiid ;"

  channel <- dbConnectNASIS(dsn)

  if (inherits(channel, 'try-error'))
    return(data.frame())

	# toggle selected set vs. local DB
	if (SS == FALSE) {
	  q <- gsub(pattern = '_View_1', replacement = '', x = q, fixed = TRUE)
	  q2 <- gsub(pattern = '_View_1', replacement = '', x = q2, fixed = TRUE)
	}

	# exec query
	d <- dbQueryNASIS(channel, q, close = FALSE)

	# ## this shouldn't happen, retain for debugging
	# # test for an error
	if (inherits(d, 'try-error'))
	  stop('error in SQL')

	# uncode domain columns
	d <- uncode(d, stringsAsFactors = stringsAsFactors, dsn = dsn)
	
	# surface fragments
	sfr <- dbQueryNASIS(channel, q2, close = FALSE)
	
	multi.siteobs <- unique(sfr[, c("siteiid","siteobsiid")])$siteiid
	if (any(table(multi.siteobs) > 1)) {
	  message("-> QC: surface fragment records from multiple site observations.\n\tUse `get('multisiteobs.surface', envir=soilDB.env) for site (siteiid) and site observation (siteobsiid)`")
	  assign("multisiteobs.surface", value = multi.siteobs[table(multi.siteobs) > 1, ], envir = soilDB.env)
	}
	
	phs <- simplifyFragmentData(
	  uncode(sfr, dsn = dsn),
	  id.var = "peiid",
	  vol.var = "sfragcov",
	  prefix = "sfrag",
	  msg = "surface fragment cover"
	)
	
	ldx <- !d$peiid %in% phs$peiid
	if (!any(ldx)) {
	  phs <- phs[1:nrow(d),]
	  phs$peiid <- d$peiid
	} else {
	  phs_null <- phs[0,][1:sum(ldx),]
	  phs_null$peiid <- d$peiid[ldx]
	  phs <- rbind(phs, phs_null)
	}
	
	# handle NA for totals
	if (nullFragsAreZero) {
	  phs[is.na(phs)] <- 0
	} 
	colnames(phs) <- paste0("surface_", colnames(phs))
	colnames(phs)[1] <- "peiid"
	d2 <- merge(d, phs, by = "peiid", all.x = TRUE, sort = FALSE)
	
	# short-circuit: 0 rows means nothing in the selected set and thus we stop here
	if (nrow(d2) == 0) {
	  return(d2)
	}

  # https://github.com/ncss-tech/soilDB/issues/41
	# warn if mixed datums
	if (length(na.omit(unique(d2$horizdatnm))) > 1)
		message('multiple horizontal datums present, consider using WGS84 coordinates (x_std, y_std)')

	# are there any duplicate pedon IDs?
	t.pedon_id <- table(d2$pedon_id)
	not.unique.pedon_id <- t.pedon_id > 1
	if (any(not.unique.pedon_id))
		assign('dup.pedon.ids', value=names(t.pedon_id[which(not.unique.pedon_id)]), envir=soilDB.env)

	# warn about sites without a matching pedon (records missing peiid)
	missing.pedon <- which(is.na(d2$peiid))
	if (length(missing.pedon) > 0)
		assign('sites.missing.pedons', value=unique(d2$site_id[missing.pedon]), envir=soilDB.env)

  ## set factor levels, when it makes sense
	# most of these are done via uncode()

  # surface shape
  d2$shapeacross <- factor(d2$shapeacross, levels=c('concave', 'linear', 'convex', 'undulating', 'complex'))
  d2$shapedown <- factor(d2$shapedown, levels=c('concave', 'linear', 'convex', 'undulating', 'complex'))

  # create 3D surface shape
  d2$slope_shape <- paste0(d2$shapeacross, ' / ', d2$shapedown)

  # make reasonable levels for 3D slope shape
  ss.grid <- expand.grid(na.omit(unique(d2$shapeacross)), na.omit(unique(d2$shapedown)))
  ss.levels <- apply(ss.grid, 1, function(i) { paste(rev(i), collapse = ' / ')})
  d2$slope_shape <- factor(d2$slope_shape, levels = ss.levels)

  # convert factors to strings
  # 2021-11-05: this code was unreachable; "df" not defined
  
  # idx <- unlist(lapply(d2, is.factor))
  # if (stringsAsFactors == FALSE & any(idx)) {
  #   d2[idx] <- lapply(d2[idx], as.character)
  # }

	# done
	return(d2)
}

