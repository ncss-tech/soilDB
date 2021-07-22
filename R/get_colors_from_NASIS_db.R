## 2013-01-08: now much faster since we only mix/clean data with > 1 color / horizon

# results can be referenced via phiid (horizon-level ID)


#' Get Soil Color Data from a local NASIS Database
#'
#' Get, format, mix, and return color data from a NASIS database.
#'
#' @param SS fetch data from Selected Set in NASIS or from the entire local
#' database (default: `TRUE`)
#' @param dsn Optional: path to local SQLite database containing NASIS
#' table structure; default: `NULL`
#' @return A data.frame with the results.
#' @author Jay M. Skovlin and Dylan E. Beaudette
#' @seealso \code{\link{simplifyColorData}},
#' \code{\link{get_hz_data_from_NASIS_db}},
#' \code{\link{get_site_data_from_NASIS_db}}
#' @keywords manip
#' @export get_colors_from_NASIS_db
get_colors_from_NASIS_db <- function(SS = TRUE, dsn = NULL) {

	# unique-ness enforced via peiid (pedon-level) and phiid (horizon-level)
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
	d <- uncode(d, stringsAsFactors = FALSE, dsn = dsn)

	# convert back to characters / numeric
	d$colormoistst <- as.character(d$colormoistst)
	d$colorhue <- as.character(d$colorhue)
	# careful!
	# uncode creates factors, so we have to convert to character first
	d$colorvalue <- as.numeric(as.character(d$colorvalue))
	d$colorchroma <- as.numeric(as.character(d$colorchroma))

  # sanity check, only attempt to simplify colors if there are > 1 rows
  if (nrow(d) > 1) {
    # mix colors as-needed, mixing done in CIE LAB space
    d.final <- simplifyColorData(d, id.var = 'phiid', wt = 'pct')
  } else {
    # TODO: this could lead to problems due to assumed column presence
    # do nothing
    d.final <- d
  }

	# done
	return(d.final)
}

