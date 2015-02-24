get_text_notes_from_NASIS_db <- function() {
  # must have RODBC installed
  if(!requireNamespace('RODBC'))
    stop('please install the `RODBC` package', call.=FALSE)
  
	# petext
	q.petext <- "SELECT recdate, recauthor, tk.ChoiceName AS textkind, textcat, textsubcat, textentry, peiidref AS peiid, petextiid FROM (petext_View_1 LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 1311) AS tk ON petext_View_1.pedontextkind = tk.ChoiceValue);"
	
	# sitetext
	q.sitetext <- "SELECT recdate, recauthor, tk.ChoiceName AS textkind, textcat, textsubcat, textentry, siteiidref AS siteiid, sitetextiid FROM (sitetext_View_1 LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 1313) AS tk ON sitetext_View_1.sitetextkind = tk.ChoiceValue);"
	
	# siteobstext
	q.siteobstext <- "SELECT recdate, recauthor, tk.ChoiceName AS textkind, textcat, textsubcat, textentry, siteiidref AS site_id, siteobstextiid FROM ((
siteobs_View_1 LEFT OUTER JOIN 
siteobstext_View_1 ON siteobs_View_1.siteobsiid = siteobstext_View_1.siteobsiidref) 
LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 1314) AS tk ON siteobstext_View_1.siteobstextkind = tk.ChoiceValue)
WHERE textentry IS NOT NULL
	ORDER BY siteobstext_View_1.siteobstextkind;"
	
	# phtext
	q.phtext <- "SELECT recdate, recauthor, tk.ChoiceName AS textkind, textcat, textsubcat, textentry, phiidref AS phiid, phtextiid FROM (phtext_View_1 LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 1312) AS tk ON phtext_View_1.phorizontextkind = tk.ChoiceValue);"
	
	# photo links
	q.photos <- "SELECT recdate, recauthor, tk.ChoiceName AS textkind, textcat, textsubcat, textentry, siteiidref AS site_id, siteobstextiid FROM ((siteobs_View_1 LEFT OUTER JOIN siteobstext_View_1 ON siteobs_View_1.siteobsiid = siteobstext_View_1.siteobsiidref) LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 1314) AS tk ON siteobstext_View_1.siteobstextkind = tk.ChoiceValue) WHERE siteobstext_View_1.textcat LIKE 'Photo%' ORDER BY siteobstext_View_1.siteobstextkind;"
	
	# setup connection local NASIS
	channel <- RODBC::odbcDriverConnect(connection="DSN=nasis_local;UID=NasisSqlRO;PWD=nasisRe@d0n1y")
	
	# run queries
	d.petext <- RODBC::sqlQuery(channel, q.petext, stringsAsFactors=FALSE)
	d.sitetext <- RODBC::sqlQuery(channel, q.sitetext, stringsAsFactors=FALSE)
	d.siteobstext <- RODBC::sqlQuery(channel, q.siteobstext, stringsAsFactors=FALSE)
	d.phtext <- RODBC::sqlQuery(channel, q.phtext, stringsAsFactors=FALSE)
	d.photos <- RODBC::sqlQuery(channel, q.photos, stringsAsFactors=FALSE)
	
	# close connection
	RODBC::odbcClose(channel)
		
	# return a list of results
	return(list(pedon_text=d.petext,
							site_text=d.sitetext,
							siteobs_text=d.siteobstext,
							horizon_text=d.phtext,
							photo_links=d.photos))
	
}