#' Get text note data from a local NASIS Database
#'
#' @param SS get data from the currently loaded Selected Set in NASIS or from
#' the entire local database (default: `TRUE`)
#'
#' @param fixLineEndings convert line endings from `\r\n` to `\n`
#'
#' @param dsn Optional: path to local SQLite database containing NASIS
#' table structure; default: `NULL`
#'
#' @return A `list` with the results.
#' @author Dylan E. Beaudette and Jay M. Skovlin
#' @seealso \code{\link{get_hz_data_from_pedon_db}},
#' \code{\link{get_site_data_from_pedon_db}}
#' @keywords manip
#' @examples
#'
#' \donttest{
#' if(local_NASIS_defined()) {
#'  # query text note data
#'  t <- try(get_text_notes_from_NASIS_db())
#'
#'  # show contents text note data, includes: siteobs, site, pedon, horizon level text notes data.
#'  str(t)
#'
#'  # view text categories for site text notes
#'  if(!inherits(t, 'try-error')) {
#'   table(t$site_text$textcat)
#'  }
#' }
#' }
#'
#' @export get_text_notes_from_NASIS_db
get_text_notes_from_NASIS_db <- function(SS=TRUE, fixLineEndings=TRUE, dsn = NULL) {

	# petext
	q.petext <- "SELECT recdate, recauthor, pedontextkind, textcat, textsubcat, peiidref AS peiid, petextiid, CAST(textentry as text) AS textentry FROM petext_View_1;"

	# sitetext
	q.sitetext <- "SELECT recdate, recauthor, sitetextkind, textcat, textsubcat, siteiidref AS siteiid, sitetextiid, CAST(textentry as text) AS textentry FROM sitetext_View_1;"

	# siteobstext
	q.siteobstext <- "SELECT recdate, recauthor, siteobstextkind, textcat, textsubcat, siteiidref AS site_id, siteobstextiid, CAST(textentry as text) AS textentry FROM (
siteobs_View_1 LEFT OUTER JOIN
siteobstext_View_1 ON siteobs_View_1.siteobsiid = siteobstext_View_1.siteobsiidref);"

	# phtext
	q.phtext <- "SELECT recdate, recauthor, phorizontextkind, textcat, textsubcat, phiidref AS phiid, phtextiid, CAST(textentry as text) AS textentry FROM phtext_View_1;"

	# photo links
	q.photos <- "SELECT recdate, recauthor, siteobstextkind, textcat, textsubcat, siteiidref AS site_id, siteobstextiid, CAST(textentry as text) AS textentry FROM (siteobs_View_1 LEFT OUTER JOIN siteobstext_View_1 ON siteobs_View_1.siteobsiid = siteobstext_View_1.siteobsiidref) WHERE siteobstext_View_1.textcat LIKE 'Photo%' ORDER BY siteobstext_View_1.siteobstextkind;"

	# toggle selected set vs. local DB
	if (SS == FALSE) {
	  q.petext <- gsub(pattern = '_View_1', replacement = '', x = q.petext, fixed = TRUE)
	  q.sitetext <- gsub(pattern = '_View_1', replacement = '', x = q.sitetext, fixed = TRUE)
	  q.siteobstext <- gsub(pattern = '_View_1', replacement = '', x = q.siteobstext, fixed = TRUE)
	  q.phtext <- gsub(pattern = '_View_1', replacement = '', x = q.phtext, fixed = TRUE)
	  q.photos <- gsub(pattern = '_View_1', replacement = '', x = q.photos, fixed = TRUE)
	}

	# check for datasource, NASIS credential options, and successful connection
	channel <- dbConnectNASIS(dsn)

	if (inherits(channel, 'try-error'))
	  return(data.frame())

	# run queries
	d.petext <- dbQueryNASIS(channel, q.petext, close = FALSE)
	d.sitetext <- dbQueryNASIS(channel, q.sitetext, close = FALSE)
	d.siteobstext <- dbQueryNASIS(channel, q.siteobstext, close = FALSE)
	d.phtext <- dbQueryNASIS(channel, q.phtext, close = FALSE)
	d.photos <- dbQueryNASIS(channel, q.photos)

	# uncode domained columns
	d.petext <- uncode(d.petext, dsn = dsn)
	d.sitetext <- uncode(d.sitetext, dsn = dsn)
	d.siteobstext <- uncode(d.siteobstext, dsn = dsn)
	d.phtext <- uncode(d.phtext, dsn = dsn)
	d.photos <- uncode(d.photos, dsn = dsn)

	# optionally convert \r\n -> \n
 	if (fixLineEndings) {
   	 d.petext$textentry <- gsub(d.petext$textentry, pattern = '\r\n', replacement = '\n', fixed = TRUE)
   	 d.sitetext$textentry <- gsub(d.sitetext$textentry, pattern = '\r\n', replacement = '\n', fixed = TRUE)
   	 d.siteobstext$textentry <- gsub(d.siteobstext$textentry, pattern = '\r\n', replacement = '\n', fixed = TRUE)
   	 d.phtext$textentry <- gsub(d.phtext$textentry, pattern = '\r\n', replacement = '\n', fixed = TRUE)
  }

	# return a list of results
	return(list(pedon_text = d.petext,
							site_text = d.sitetext,
							siteobs_text = d.siteobstext,
							horizon_text = d.phtext,
							photo_links = d.photos))

}
