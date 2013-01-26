get_text_notes_from_NASIS_db <- function() {

	# petext
	q.petext <- "SELECT recdate, recauthor, tk.ChoiceName AS textkind, textcat, textsubcat, textentry, peiidref AS peiid, petextiid FROM (petext LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE MetadataDomainDetail.DomainID = 1311) AS tk ON petext.pedontextkind = tk.ChoiceValue);"
	
	# sitetext
	q.sitetext <- "SELECT recdate, recauthor, tk.ChoiceName AS textkind, textcat, textsubcat, textentry, siteiidref AS siteiid, sitetextiid FROM (sitetext LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE MetadataDomainDetail.DomainID = 1313) AS tk ON sitetext.sitetextkind = tk.ChoiceValue);"
	
	# siteobstext
	q.siteobstext <- "SELECT siteobstext.recdate, siteobstext.recauthor, tk.ChoiceName AS textkind, siteobstext.textcat, siteobstext.textsubcat, siteobstext.textentry, siteobs.siteiidref AS site_id, siteobstext.siteobstextiid FROM ((
siteobs LEFT OUTER JOIN 
siteobstext ON siteobs.siteobsiid = siteobstext.siteobsiidref) 
LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE MetadataDomainDetail.DomainID = 1314) AS tk ON siteobstext.siteobstextkind = tk.ChoiceValue)
WHERE textentry IS NOT NULL
	ORDER BY siteobstext.siteobstextkind;"
	
	# phtext
	q.phtext <- "SELECT recdate, recauthor, tk.ChoiceName AS textkind, textcat, textsubcat, textentry, phiidref AS phiid, phtextiid FROM (phtext LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE MetadataDomainDetail.DomainID = 1312) AS tk ON phtext.phorizontextkind = tk.ChoiceValue);"
	
	# photo links
	q.photos <- "SELECT siteobstext.recdate, siteobstext.recauthor, tk.ChoiceName AS textkind, siteobstext.textcat, siteobstext.textsubcat, siteobstext.textentry, siteobs.siteiidref AS site_id, siteobstext.siteobstextiid FROM ((siteobs LEFT OUTER JOIN siteobstext ON siteobs.siteobsiid = siteobstext.siteobsiidref) LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE MetadataDomainDetail.DomainID = 1314) AS tk ON siteobstext.siteobstextkind = tk.ChoiceValue) WHERE siteobstext.textcat LIKE 'Photo%' ORDER BY siteobstext.siteobstextkind;"
	
	# setup connection to our local NASIS database
	channel <- odbcConnect('nasis_local', uid='NasisSqlRO', pwd='nasisRe@d0n1y') 
	
	# run queries
	d.petext <- sqlQuery(channel, q.petext, stringsAsFactors=FALSE)
	d.sitetext <- sqlQuery(channel, q.sitetext, stringsAsFactors=FALSE)
	d.siteobstext <- sqlQuery(channel, q.siteobstext, stringsAsFactors=FALSE)
	d.phtext <- sqlQuery(channel, q.phtext, stringsAsFactors=FALSE)
	d.photos <- sqlQuery(channel, q.photos, stringsAsFactors=FALSE)
	
	# close connection
	odbcClose(channel)
		
	# return a list of results
	return(list(pedon_text=d.petext,
							site_text=d.sitetext,
							siteobs_text=d.siteobstext,
							horizon_text=d.phtext,
							photo_links=d.photos))
	
}