get_lablayer_data_from_NASIS_db <- function(SS = TRUE) {

  # hacks to make R CMD check --as-cran happy:
  cec7 <- NULL
  claytot <- NULL
  claycarb <- NULL
  carbonorganicpct <- NULL

  # must have RODBC installed
  if(!requireNamespace('RODBC')) {
    stop('please install the `RODBC` package', call.=FALSE)
    }
  q.ncsslablayer <- paste0("SELECT ncsspedonlabdataiidref AS labpeiid, ncsslayerlabdataiid AS labphiid, labsampnum, layerseqnum, hzdept, hzdepb, layertype, hzname, hznameoriginal, stratextsflag, moistprepstate, texcl, sandvcmeasured, sandcomeasured, sandmedmeasured, sandfinemeasured, sandvfmeasured, sandtotmeasured, siltcomeasured, siltfinemeasured, silttotmeasured, claycarbmeasured, clayfinemeasured, claytotmeasured, carbonorganicpctmeasured, carbontotalpctmeasured, ompctest, fiberrubbedpct, fiberunrubbedpct, fragwt25, fragwt520, fragwt2075, fragwt275, wtpct0175, wtpctgt2ws, ph1to1h2o, ph01mcacl2, phnaf, phoxidized, resistivity, ecmeasured, esp, sar, cecsumcations, cec7, ecec, sumbases, basesatsumcations, basesatnh4oac, caco3equivmeasured, caco3lt20measured, gypsumequivmeasured, feoxalatemeasured, feextractable, fetotal, sioxalatemeasured, extracid, extral, aloxalatemeasured, altotal, pmehlich3, ph2osolublemeasured, poxalatemeasured, polsenmeasured, ptotalmeasured, nzpretention, dbthirdbar, dbovendry, aggstabpct, wtenthbarclod, wtenthbarsieve, wthirdbarclod, wthirdbarsieve, wfifteenbarmeasured, wretentiondiffws, wfifteenbartoclay, adod, lep, cole, liquidlimitmeasured, pi, recwlupdated, ncsslayerlabdataiid

FROM ncsslayerlabdata_View_1

ORDER BY labpeiid, hzdept ASC;")


  channel <- .openNASISchannel()
  if (channel == -1)
    return(data.frame())

	# handle Views/selected set argument
  if(!SS)
    q.ncsslablayer <- gsub(q.ncsslablayer, pattern = "_View_1", replacement = "")

	# exec queries
	d.lablayer <- RODBC::sqlQuery(channel, q.ncsslablayer, stringsAsFactors=FALSE)


	# recode metadata domains
	d.lablayer <- uncode(d.lablayer)


	# trim names
	names(d.lablayer) <- gsub("measured", "", names(d.lablayer))


	# transform variables
	d.lablayer <- within(d.lablayer, {
	  cec7clay = round(cec7 / (claytot - claycarb), 2)
	  organicmatpct = round(carbonorganicpct * 1.724, 2)
	  })

	# close connection
	RODBC::odbcClose(channel)

	# return a list of results
	return(d.lablayer)
}
