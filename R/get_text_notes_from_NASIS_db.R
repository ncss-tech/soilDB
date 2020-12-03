get_text_notes_from_NASIS_db <- function(SS=TRUE, fixLineEndings=TRUE, sqlite_path = NULL) {

	# petext
	q.petext <- "SELECT recdate, recauthor, pedontextkind, textcat, textsubcat, peiidref AS peiid, petextiid, CAST(textentry AS ntext) AS textentry FROM petext_View_1;"

	# sitetext
	q.sitetext <- "SELECT recdate, recauthor, sitetextkind, textcat, textsubcat, siteiidref AS siteiid, sitetextiid, CAST(textentry AS ntext) AS textentry FROM sitetext_View_1;"

	# siteobstext
	q.siteobstext <- "SELECT recdate, recauthor, siteobstextkind, textcat, textsubcat, siteiidref AS site_id, siteobstextiid, CAST(textentry AS ntext) AS textentry FROM (
siteobs_View_1 LEFT OUTER JOIN
siteobstext_View_1 ON siteobs_View_1.siteobsiid = siteobstext_View_1.siteobsiidref);"

	# phtext
	q.phtext <- "SELECT recdate, recauthor, phorizontextkind, textcat, textsubcat, phiidref AS phiid, phtextiid, CAST(textentry AS ntext) AS textentry FROM phtext_View_1;"

	# photo links
	q.photos <- "SELECT recdate, recauthor, siteobstextkind, textcat, textsubcat, siteiidref AS site_id, siteobstextiid, CAST(textentry AS ntext) AS textentry FROM (siteobs_View_1 LEFT OUTER JOIN siteobstext_View_1 ON siteobs_View_1.siteobsiid = siteobstext_View_1.siteobsiidref) WHERE siteobstext_View_1.textcat LIKE 'Photo%' ORDER BY siteobstext_View_1.siteobstextkind;"

	# check for RODBC, NASIS credential options, and successful connection
	channel <- dbConnectNASIS(sqlite_path)
	
	if (inherits(channel, 'try-error'))
	  return(data.frame())

	# run queries
	d.petext <- dbQueryNASIS(channel, q.petext, close = FALSE)
	d.sitetext <- dbQueryNASIS(channel, q.sitetext, close = FALSE)
	d.siteobstext <- dbQueryNASIS(channel, q.siteobstext, close = FALSE)
	d.phtext <- dbQueryNASIS(channel, q.phtext, close = FALSE)
	d.photos <- dbQueryNASIS(channel, q.photos)

	# uncode domained columns
	d.petext <- uncode(d.petext)
	d.sitetext <- uncode(d.sitetext)
	d.siteobstext <- uncode(d.siteobstext)
	d.phtext <- uncode(d.phtext)
	d.photos <- uncode(d.photos)

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
