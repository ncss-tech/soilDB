#' Get lab pedon layer data from a local NASIS Database
#'
#' Get lab pedon layer-level (horizon-level) data from a local NASIS database.
#'
#' @param SS fetch data from the currently loaded selected set in NASIS or from
#' the entire local database (default: `TRUE`)
#'
#' @param dsn Optional: path to local SQLite database containing NASIS
#' table structure; default: `NULL`
#'
#' @return A data.frame.
#' @note This function queries KSSL laboratory site/horizon data from a local
#' NASIS database from the lab layer data table.
#'
#' @author Jay M. Skovlin and Dylan E. Beaudette
#' @seealso \code{\link{get_labpedon_data_from_NASIS_db}}
#' @keywords manip
#' @export get_lablayer_data_from_NASIS_db
get_lablayer_data_from_NASIS_db <- function(SS = TRUE, dsn = NULL) {

  # hacks to make R CMD check --as-cran happy:
  cec7 <- NULL
  claytot <- NULL
  claycarb <- NULL
  carbonorganicpct <- NULL

  q.ncsslablayer <- paste0("SELECT ncsspedonlabdataiidref AS labpeiid, ncsslayerlabdataiid AS labphiid, labsampnum, layerseqnum, hzdept, hzdepb, layertype, hzname, hznameoriginal, stratextsflag, moistprepstate, texcl, sandvcmeasured, sandcomeasured, sandmedmeasured, sandfinemeasured, sandvfmeasured, sandtotmeasured, siltcomeasured, siltfinemeasured, silttotmeasured, claycarbmeasured, clayfinemeasured, claytotmeasured, carbonorganicpctmeasured, carbontotalpctmeasured, ompctest, fiberrubbedpct, fiberunrubbedpct, fragwt25, fragwt520, fragwt2075, fragwt275, wtpct0175, wtpctgt2ws, ph1to1h2o, ph01mcacl2, phnaf, phoxidized, resistivity, ecmeasured, esp, sar, cecsumcations, cec7, ecec, sumbases, basesatsumcations, basesatnh4oac, caco3equivmeasured, caco3lt20measured, gypsumequivmeasured, feoxalatemeasured, feextractable, fetotal, sioxalatemeasured, extracid, extral, aloxalatemeasured, altotal, pmehlich3, ph2osolublemeasured, poxalatemeasured, polsenmeasured, ptotalmeasured, nzpretention, dbthirdbar, dbovendry, aggstabpct, wtenthbarclod, wtenthbarsieve, wthirdbarclod, wthirdbarsieve, wfifteenbarmeasured, wretentiondiffws, wfifteenbartoclay, adod, lep, cole, liquidlimitmeasured, pi, recwlupdated, ncsslayerlabdataiid

FROM ncsslayerlabdata_View_1

ORDER BY labpeiid, hzdept ASC;")

  channel <- dbConnectNASIS(dsn)

  if (inherits(channel, 'try-error'))
    return(data.frame())

	# handle Views/selected set argument
  if(!SS)
    q.ncsslablayer <- gsub(q.ncsslablayer, pattern = "_View_1", replacement = "")

	# exec query
  d.lablayer <- dbQueryNASIS(channel, q.ncsslablayer)

	# recode metadata domains
	d.lablayer <- uncode(d.lablayer, dsn = dsn)


	# trim names
	names(d.lablayer) <- gsub("measured", "", names(d.lablayer))


	# transform variables
	d.lablayer <- within(d.lablayer, {
	  cec7clay = round(cec7 / (claytot - claycarb), 2)
	  organicmatpct = round(carbonorganicpct * 1.724, 2)
	  })

	# return a list of results
	return(d.lablayer)
}
