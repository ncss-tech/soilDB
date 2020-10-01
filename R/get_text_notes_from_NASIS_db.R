get_text_notes_from_NASIS_db <- function(SS=TRUE, fixLineEndings=TRUE) {
  # must have RODBC installed
  if(!requireNamespace('RODBC'))
    stop('please install the `RODBC` package', call.=FALSE)

	# petext
	q.petext <- "SELECT recdate, recauthor, pedontextkind, textcat, textsubcat, CAST(textentry AS ntext) AS textentry, peiidref AS peiid, petextiid FROM petext_View_1;"

	# sitetext
	q.sitetext <- "SELECT recdate, recauthor, sitetextkind, textcat, textsubcat, CAST(textentry AS ntext) AS textentry, siteiidref AS siteiid, sitetextiid FROM sitetext_View_1;"

	# siteobstext
	q.siteobstext <- "SELECT recdate, recauthor, siteobstextkind, textcat, textsubcat, CAST(textentry AS ntext) AS textentry, siteiidref AS site_id, siteobstextiid FROM (
siteobs_View_1 LEFT OUTER JOIN
siteobstext_View_1 ON siteobs_View_1.siteobsiid = siteobstext_View_1.siteobsiidref);"

	# phtext
	q.phtext <- "SELECT recdate, recauthor, phorizontextkind, textcat, textsubcat, CAST(textentry AS ntext) AS textentry, phiidref AS phiid, phtextiid FROM phtext_View_1;"

	# photo links
	q.photos <- "SELECT recdate, recauthor, siteobstextkind, textcat, textsubcat, CAST(textentry AS ntext) AS textentry, siteiidref AS site_id, siteobstextiid FROM (siteobs_View_1 LEFT OUTER JOIN siteobstext_View_1 ON siteobs_View_1.siteobsiid = siteobstext_View_1.siteobsiidref) WHERE siteobstext_View_1.textcat LIKE 'Photo%' ORDER BY siteobstext_View_1.siteobstextkind;"

	# check for RODBC, NASIS credential options, and successful connection
	channel <- .openNASISchannel()
	if (channel == -1)
	  return(data.frame())

	# run queries
	d.petext <- RODBC::sqlQuery(channel, q.petext, stringsAsFactors=FALSE)
	d.sitetext <- RODBC::sqlQuery(channel, q.sitetext, stringsAsFactors=FALSE)
	d.siteobstext <- RODBC::sqlQuery(channel, q.siteobstext, stringsAsFactors=FALSE)
	d.phtext <- RODBC::sqlQuery(channel, q.phtext, stringsAsFactors=FALSE)
	d.photos <- RODBC::sqlQuery(channel, q.photos, stringsAsFactors=FALSE)

	# close connection
	RODBC::odbcClose(channel)

	# uncode domained columns
	d.petext <- uncode(d.petext)
	d.sitetext <- uncode(d.sitetext)
	d.siteobstext <- uncode(d.siteobstext)
	d.phtext <- uncode(d.phtext)
	d.photos <- uncode(d.photos)

	# optionally convert \r\n -> \n
 	 if(fixLineEndings){
   	 d.petext$textentry <- gsub(d.petext$textentry, pattern = '\r\n', replacement = '\n', fixed = TRUE)
   	 d.sitetext$textentry <- gsub(d.sitetext$textentry, pattern = '\r\n', replacement = '\n', fixed = TRUE)
   	 d.siteobstext$textentry <- gsub(d.siteobstext$textentry, pattern = '\r\n', replacement = '\n', fixed = TRUE)
   	 d.phtext$textentry <- gsub(d.phtext$textentry, pattern = '\r\n', replacement = '\n', fixed = TRUE)
  	}

	# return a list of results
	return(list(pedon_text=d.petext,
							site_text=d.sitetext,
							siteobs_text=d.siteobstext,
							horizon_text=d.phtext,
							photo_links=d.photos))

}
