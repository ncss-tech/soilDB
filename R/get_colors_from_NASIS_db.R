## 2013-01-08: now much faster since we only mix/clean data with > 1 color / horizon

# results can be referenced via phiid (horizon-level ID)


#' Get Soil Color Data from a local NASIS Database
#'
#' Get, format, mix, and return color data from a NASIS database.
#'
#' @param SS fetch data from Selected Set in NASIS or from the entire local
#' database (default: `TRUE`)
#' @param mixColors should mixed colors be calculated (Default: `TRUE`) where multiple colors are populated for the same moisture state in a horizon? `FALSE` takes the dominant color for each horizon moist/dry state.
#' @param dsn Optional: path to local SQLite database containing NASIS
#' table structure; default: `NULL`
#' @return A data.frame with the results.
#' @author Jay M. Skovlin and Dylan E. Beaudette
#' @seealso \code{\link{simplifyColorData}},
#' \code{\link{get_hz_data_from_NASIS_db}},
#' \code{\link{get_site_data_from_NASIS_db}}
#' @keywords manip
#' @export get_colors_from_NASIS_db
get_colors_from_NASIS_db <- function(SS = TRUE, mixColors = TRUE, dsn = NULL) {

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
  if (nrow(d) > 1 && mixColors) {
    # mix colors as-needed, mixing done in CIE LAB space
    d.final <- simplifyColorData(d, id.var = 'phiid', wt = 'pct')
  } else {
    d.final <- .dominantColors(d)
  }

	# done
	return(d.final)
}

.dominantColors <- function(d, id.var = 'phiid', moist_state = 'colormoistst', wt = 'pct',
                            hue = 'colorhue', value = 'colorvalue', chroma = 'colorchroma') {
  .I <- NULL
  d[[moist_state]] <- tolower(d[[moist_state]])
  d <- d[d[[moist_state]] %in% c('dry', 'moist'),]
  d <- data.table::as.data.table(d)[order(d[[id.var]], d[[moist_state]], d[[wt]], decreasing = TRUE),]
  dom <- d[, .I[1], by = c(id.var, moist_state)]
  d$peiid <- NULL; d[[moist_state]] <- NULL; d[[wt]] <- NULL
  
  dry <- d[dom[which(dom$colormoistst == "dry"), ]$V1, ]
  dry$dry_soil_color <- aqp::munsell2rgb(dry[[hue]], dry[[value]], dry[[chroma]])
  dry <- cbind(dry, t(col2rgb(dry$dry_soil_color) / 255))
  colnames(dry) <- c(id.var, 'd_hue', 'd_value', 'd_chroma', 'dry_soil_color', 'd_r', 'd_g', 'd_b')
  dry$d_sigma <- 0
  
  moist <- d[dom[which(dom$colormoistst == "moist"), ]$V1, ]
  moist$moist_soil_color <- aqp::munsell2rgb(moist[[hue]], moist[[value]], moist[[chroma]])
  moist <- cbind(moist, t(col2rgb(moist$moist_soil_color) / 255))
  colnames(moist) <- c(id.var, 'm_hue', 'm_value', 'm_chroma', 'moist_soil_color', 'm_r', 'm_g', 'm_b')
  moist$m_sigma <- 0
  
  as.data.frame(dry[moist, on = id.var])
}
