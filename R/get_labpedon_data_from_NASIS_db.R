get_labpedon_data_from_NASIS_db <- function(SS = TRUE) {
  # must have RODBC installed
  if(!requireNamespace('RODBC'))
    stop('please install the `RODBC` package', call.=FALSE)

  q.ncsslabpedon <- "SELECT peiidref AS peiid, upedonid, descname, taxonname, taxclname, ncsspedonlabdata_View_1.pedlabsampnum, psctopdepth, pscbotdepth, noncarbclaywtavg, claytotwtavg, le0to100, wf0175wtavgpsc, volfractgt2wtavg, cec7clayratiowtavg, labdatasheeturl, ncsspedonlabdataiid AS labpeiid
  FROM (ncsspedonlabdata_View_1 LEFT OUTER JOIN pedon_View_1 ON ncsspedonlabdata_View_1.peiidref = pedon_View_1.peiid);"

  channel <- .openNASISchannel()
  if (channel == -1)
    return(data.frame())

  # handle Views/selected set argument
  if(!SS)
    q.ncsslabpedon <- gsub(q.ncsslabpedon, pattern = "_View_1", replacement = "")

	# exec queries
	d.labpedon <- RODBC::sqlQuery(channel, q.ncsslabpedon, stringsAsFactors=FALSE)

	# close connection
	RODBC::odbcClose(channel)

	# return a list of results
	return(d.labpedon)
}
