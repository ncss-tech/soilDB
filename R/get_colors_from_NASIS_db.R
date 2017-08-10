## 2013-01-08: now much faster since we only mix/clean data with > 1 color / horizon

# results can be referenced via phiid (horizon-level ID)
get_colors_from_NASIS_db <- function() {
  # must have RODBC installed
  if(!requireNamespace('RODBC'))
    stop('please install the `RODBC` package', call.=FALSE)
  
  ## 2016-04-18 BUG FIX: value and chroma have always been coded, but we were not decoding
	# unique-ness enforced via peiid (pedon-level) and phiid (horizon-level)
  q <- "SELECT peiid, phiid, ms.ChoiceLabel AS colormoistst, colorpct as pct, mh.ChoiceName AS colorhue, mv.ChoiceName AS colorvalue, mc.ChoiceName AS colorchroma
FROM
  pedon_View_1 INNER JOIN phorizon_View_1 ON pedon_View_1.peiid = phorizon_View_1.peiidref
  INNER JOIN phcolor_View_1 ON phorizon_View_1.phiid = phcolor_View_1.phiidref
  LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 1242) AS mh ON phcolor_View_1.colorhue = mh.ChoiceValue
  LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 1244) AS mv ON phcolor_View_1.colorvalue = mv.ChoiceValue
  LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 1241) AS mc ON phcolor_View_1.colorchroma = mc.ChoiceValue
  LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 1243) AS ms ON phcolor_View_1.colormoistst = ms.ChoiceValue
  ORDER BY phiid, colormoistst;"
  
	# setup connection local NASIS
	channel <- RODBC::odbcDriverConnect(connection="DSN=nasis_local;UID=NasisSqlRO;PWD=nasisRe@d0n1y")
	
	# exec query
	d <- RODBC::sqlQuery(channel, q, stringsAsFactors=FALSE)
	
	# close connection
  RODBC::odbcClose(channel)
	
  # sanity check, only attempt to simplify colors if there are > 1 rows
  if(nrow(d) > 1) {
    # mix colors as-needed, mixing done in CIE LAB space
    d.final <- simplifyColorData(d)
  } else {
    # TODO: this could lead to problems due to assumed column presence
    # do nothing
    d.final <- d
  }
  
	# done
	return(d.final)
}

