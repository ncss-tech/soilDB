## 2013-01-08: now much faster since we only mix/clean data with > 1 color / horizon

# results can be referenced via phiid (horizon-level ID)


#' Get Soil Color Data from a local NASIS Database
#'
#' Get, format, mix, and return color data from a NASIS database.
#'
#' @param SS fetch data from Selected Set in NASIS or from the entire local
#' database (default: `TRUE`)
#' @param method Aggregation method to handle multiple colors per horizon and moisture state. Default `"dominant"` for dominant condition (or first record) within moisture state. Other options include `"mixed"` to calculate mixture using `simplifyColorData()` and `"none"` to do no aggregation (returns a long format representation that may have multiple values per horizon and moisture state)
#' @param mixColors Deprecated. See `method`. Should mixed colors be calculated where multiple colors are populated for the same moisture state in a horizon? Default `FALSE` takes the dominant color based on `colorpct` or first record based on horizon ID (`phiid`) sorting for "moist" and "dry" state. Pedon Horizon Color records without a moisture state populated are ignored.
#' @param dsn Optional: path to local SQLite database containing NASIS
#' table structure; default: `NULL`
#' @return A data.frame with the results.
#' @author Jay M. Skovlin and Dylan E. Beaudette
#' @seealso \code{\link{simplifyColorData}},
#' \code{\link{get_hz_data_from_NASIS_db}},
#' \code{\link{get_site_data_from_NASIS_db}}
#' @keywords manip
#' @export get_colors_from_NASIS_db
get_colors_from_NASIS_db <- function(SS = TRUE, method = "dominant", mixColors = FALSE, dsn = NULL) {

  if (!missing(mixColors)) {
    .Deprecated(msg = "`mixColors` argument is deprecated, see `method` argument for additional aggregation options")
    if (isTRUE(mixColors)) {
      method <- "mixed"
    }
  }
  
  method <- match.arg(method, c("dominant", "mixed", "none"))
  
	# unique-ness enforced via peiid (pedon-level) and phiid (horizon-level)
	# TODO: is alias of colorpct necessary?
  q <- "SELECT peiid, phiid, colormoistst, colorpct as pct, colorhue, colorvalue, colorchroma
  FROM
  pedon_View_1
  INNER JOIN phorizon_View_1 ON pedon_View_1.peiid = phorizon_View_1.peiidref
  INNER JOIN phcolor_View_1 ON phorizon_View_1.phiid = phcolor_View_1.phiidref
  ORDER BY phiid, colormoistst;"

  channel <- dbConnectNASIS(dsn)

  if (inherits(channel, 'try-error'))
    return(data.frame())

  # toggle selected set vs. local DB
  if (SS == FALSE) {
    q <- gsub(pattern = '_View_1', replacement = '', x = q, fixed = TRUE)
  }

  # exec query
  d <- dbQueryNASIS(channel, q)

	# uncode domained columns
	d <- uncode(d, dsn = dsn)

	# convert factors if present back to characters / numeric
	d$colormoistst <- as.character(d$colormoistst)
	d$colorhue <- as.character(d$colorhue)
	d$colorvalue <- as.numeric(as.character(d$colorvalue))
	d$colorchroma <- as.numeric(as.character(d$colorchroma))

  # sanity check, only attempt to simplify colors if there are > 1 rows
  if (nrow(d) > 1 && (method == "mixed")) {
    # mix colors as-needed, mixing done in CIE LAB space
    d.final <- simplifyColorData(d, id.var = 'phiid', wt = 'pct')
  } else if (method == "dominant") {
    d.final <- .dominantColors(d)
  } else {
    d.final <- d
  }

	# done
	return(d.final)
}

.dominantColors <- function(d, id.var = 'phiid', moist_state = 'colormoistst', wt = 'pct',
                            hue = 'colorhue', value = 'colorvalue', chroma = 'colorchroma') {
  
  if (!requireNamespace("aqp")) {
    stop("package 'aqp' is required", call. = FALSE)
  }
  
  # use data.table
  .I <- NULL; .SD <- NULL
  d <- data.table::as.data.table(d)
  
  # calculate table of IDs
  did <- unique(d[, .SD, .SDcols = id.var])
  
  # allow for alternate capitalization of dry/moist in moist_state
  d[[moist_state]] <- tolower(d[[moist_state]])
  
  # filter to only target moist_states with at least hue+value
  d <- d[which(d[[moist_state]] %in% c('dry', 'moist') &
                 !is.na(d[[hue]]) &
                 !is.na(d[[value]])),]
  
  # sort on ID, moisture state, and weight (% color); NA wt sorted to end
  d <- d[order(d[[id.var]], d[[moist_state]], d[[wt]], decreasing = TRUE),]
  
  # take index of first record in each horizon ID*moist_state combination
  dom <- d[, list(first_idx = .I[1]), by = c(id.var, moist_state)]
  d$peiid <- NULL; d[[moist_state]] <- NULL; d[[wt]] <- NULL
  
  # process dry values into d_ H/V/C, hex color and RGB triplet columns
  dry <- d[dom[which(dom$colormoistst == "dry"), ]$first_idx, ]
  if (nrow(dry) > 0) {
    dry$dry_soil_color <- aqp::munsell2rgb(dry[[hue]], dry[[value]], dry[[chroma]])
    dry <- cbind(dry, t(col2rgb(dry$dry_soil_color) / 255))
  } else {
    dry$dry_soil_color <- character()
    dry$r <- numeric()
    dry$g <- numeric()
    dry$b <- numeric()
  }
  colnames(dry) <- c(id.var, 'd_hue', 'd_value', 'd_chroma', 'dry_soil_color', 'd_r', 'd_g', 'd_b')
  
  # sigma is NA for single color
  dry$d_sigma <- NA_real_
  
  # process moist values into m_ H/V/C, hex color and RGB triplet columns
  moist <- d[dom[which(dom$colormoistst == "moist"), ]$first_idx, ]
  if (nrow(moist) > 0) {
    moist$moist_soil_color <- aqp::munsell2rgb(moist[[hue]], moist[[value]], moist[[chroma]])
    moist <- cbind(moist, t(col2rgb(moist$moist_soil_color) / 255))
  } else {
    moist$moist_soil_color <- character()
    moist$r <- numeric()
    moist$g <- numeric()
    moist$b <- numeric()
  }
  colnames(moist) <- c(id.var, 'm_hue', 'm_value', 'm_chroma', 'moist_soil_color', 'm_r', 'm_g', 'm_b')
  moist$m_sigma <- NA_real_
  
  # dry and moist full outer join on horizon ID
  res <- merge(dry, moist, all = TRUE)
  
  # return data.frame in original order (of source NASIS query) 
  as.data.frame(res)
}
