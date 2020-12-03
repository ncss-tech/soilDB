get_labpedon_data_from_NASIS_db <- function(SS = TRUE, sqlite_path = NULL) {

  q.ncsslabpedon <- "SELECT peiidref AS peiid, upedonid, descname, taxonname, taxclname, ncsspedonlabdata_View_1.pedlabsampnum, psctopdepth, pscbotdepth, noncarbclaywtavg, claytotwtavg, le0to100, wf0175wtavgpsc, volfractgt2wtavg, cec7clayratiowtavg, labdatasheeturl, ncsspedonlabdataiid AS labpeiid
  FROM (ncsspedonlabdata_View_1 LEFT OUTER JOIN pedon_View_1 ON ncsspedonlabdata_View_1.peiidref = pedon_View_1.peiid);"

  channel <- dbConnectNASIS(sqlite_path)
  
  if (inherits(channel, 'try-error'))
    return(data.frame())

  # handle Views/selected set argument
  if (!SS)
    q.ncsslabpedon <- gsub(q.ncsslabpedon, pattern = "_View_1", replacement = "")

  d.labpedon <- dbQueryNASIS(channel, q.ncsslabpedon)

	# return a list of results
	return(d.labpedon)
}
