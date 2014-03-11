get_labpedon_data_from_NASIS_db <- function() {

q.ncsslabpedon <- "SELECT peiidref AS peiid, upedonid, ncsspedonlabdata_View_1.pedlabsampnum, psctopdepth, pscbotdepth, noncarbclaywtavg, claytotwtavg, le0to100, wf0175wtavgpsc, volfractgt2wtavg, cec7clayratiowtavg, labdatasheeturl, ncsspedonlabdataiid AS labpeiid 
  FROM (ncsspedonlabdata_View_1 LEFT OUTER JOIN pedon_View_1 ON ncsspedonlabdata_View_1.peiidref = pedon_View_1.peiid);"

  # setup connection to our local NASIS database
	channel <- odbcConnect('nasis_local', uid='NasisSqlRO', pwd='nasisRe@d0n1y') 
	
	# exec queries
	d.labpedon <- sqlQuery(channel, q.ncsslabpedon, stringsAsFactors=FALSE)
		
	# close connection
	odbcClose(channel)
	
	# return a list of results
	return(d.labpedon)
}