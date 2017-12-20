get_text_notes_from_NASIS_db <- function(SS=TRUE) {
  # must have RODBC installed
  if(!requireNamespace('RODBC'))
    stop('please install the `RODBC` package', call.=FALSE)
  
	# petext
	q.petext <- "SELECT recdate, recauthor, pedontextkind, textcat, textsubcat, textentry, peiidref AS peiid, petextiid FROM petext_View_1;"
	
	# sitetext
	q.sitetext <- "SELECT recdate, recauthor, sitetextkind, textcat, textsubcat, textentry, siteiidref AS siteiid, sitetextiid FROM sitetext_View_1;"
	
	# siteobstext
	q.siteobstext <- "SELECT recdate, recauthor, siteobstextkind, textcat, textsubcat, textentry, siteiidref AS site_id, siteobstextiid FROM (
siteobs_View_1 LEFT OUTER JOIN 
siteobstext_View_1 ON siteobs_View_1.siteobsiid = siteobstext_View_1.siteobsiidref);"
	
	# phtext
	q.phtext <- "SELECT recdate, recauthor, phorizontextkind, textcat, textsubcat, textentry, phiidref AS phiid, phtextiid FROM phtext_View_1;"
	
	# photo links
	q.photos <- "SELECT recdate, recauthor, siteobstextkind, textcat, textsubcat, textentry, siteiidref AS site_id, siteobstextiid FROM (siteobs_View_1 LEFT OUTER JOIN siteobstext_View_1 ON siteobs_View_1.siteobsiid = siteobstext_View_1.siteobsiidref) WHERE siteobstext_View_1.textcat LIKE 'Photo%' ORDER BY siteobstext_View_1.siteobstextkind;"
	
	# setup connection local NASIS
	channel <- RODBC::odbcDriverConnect(connection="DSN=nasis_local;UID=NasisSqlRO;PWD=nasisRe@d0n1y")
	
	# run queries
	d.petext <- RODBC::sqlQuery(channel, q.petext, stringsAsFactors=FALSE)
	d.sitetext <- RODBC::sqlQuery(channel, q.sitetext, stringsAsFactors=FALSE)
	d.siteobstext <- RODBC::sqlQuery(channel, q.siteobstext, stringsAsFactors=FALSE)
	d.phtext <- RODBC::sqlQuery(channel, q.phtext, stringsAsFactors=FALSE)
	d.photos <- RODBC::sqlQuery(channel, q.photos, stringsAsFactors=FALSE)

	# uncode domained columns
	d.petext <- uncode(d.petext)
	d.sitetext <- uncode(d.sitetext)
	d.siteobstext <- uncode(d.siteobstext)
	d.phtext <- uncode(d.phtext)
	d.photos <- uncode(d.photos)
	
	# close connection
	RODBC::odbcClose(channel)
		
	# return a list of results
	return(list(pedon_text=d.petext,
							site_text=d.sitetext,
							siteobs_text=d.siteobstext,
							horizon_text=d.phtext,
							photo_links=d.photos))
	
}