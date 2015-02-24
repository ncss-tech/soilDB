get_lablayer_data_from_NASIS_db <- function() {
  # must have RODBC installed
  if(!requireNamespace('RODBC'))
    stop('please install the `RODBC` package', call.=FALSE)
  
q.ncsslablayer <- "SELECT ncsspedonlabdataiidref AS labpeiid, labsampnum, layerseqnum, hzdept, hzdepb, layertype, hzname, hznameoriginal, stratextsflag, mps.ChoiceName AS moist_prepstate, txcl.ChoiceName AS lab_texcl, sandvcmeasured, sandcomeasured, sandmedmeasured, sandfinemeasured, sandvfmeasured, sandtotmeasured, siltcomeasured, siltfinemeasured, silttotmeasured, claycarbmeasured, clayfinemeasured, claytotmeasured, carbonorganicpctmeasured, carbontotalpctmeasured, ompctest, fiberrubbedpct, fiberunrubbedpct, fragwt25, fragwt520, fragwt2075, fragwt275, wtpct0175, wtpctgt2ws, ph1to1h2o, ph01mcacl2, phnaf, phoxidized, resistivity, ecmeasured, esp, sar, cecsumcations, cec7, ecec, sumbases, basesatsumcations, basesatnh4oac, caco3equivmeasured, caco3lt20measured, gypsumequivmeasured, feoxalatemeasured, feextractable, fetotal, sioxalatemeasured, extracid, extral, aloxalatemeasured, altotal, pmehlich3, ph2osolublemeasured, poxalatemeasured, polsenmeasured, ptotalmeasured, nzpretention, dbthirdbar, dbovendry, aggstabpct, wtenthbarclod, wtenthbarsieve, wthirdbarclod, wthirdbarsieve, wfifteenbarmeasured, wretentiondiffws, wfifteenbartoclay, adod, lep, cole, liquidlimitmeasured, pi, recwlupdated, ncsslayerlabdataiid 
FROM ((
ncsslayerlabdata_View_1
LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 5103) AS mps ON ncsslayerlabdata_View_1.moistprepstate = mps.ChoiceValue)
LEFT OUTER JOIN (SELECT * FROM MetadataDomainDetail WHERE DomainID = 189) AS txcl ON ncsslayerlabdata_View_1.texcl = txcl.ChoiceValue)
ORDER BY labpeiid, hzdept ASC;"

	# setup connection local NASIS
	channel <- RODBC::odbcDriverConnect(connection="DSN=nasis_local;UID=NasisSqlRO;PWD=nasisRe@d0n1y")
	
	# exec queries
	d.lablayer <- RODBC::sqlQuery(channel, q.ncsslablayer, stringsAsFactors=FALSE)
		
	# close connection
	RODBC::odbcClose(channel)
	
	# return a list of results
	return(d.lablayer)
}