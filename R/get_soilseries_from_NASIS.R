get_soilseries_from_NASIS <- function(stringsAsFactors = default.stringsAsFactors()) {
  # must have RODBC installed
  if (!requireNamespace('RODBC')) stop('please install the `RODBC` package', call.=FALSE)
  
  q.soilseries <- "
  SELECT soilseriesname, soilseriesstatus, benchmarksoilflag, statsgoflag, mlraoffice, areasymbol, areatypename, taxclname, taxorder, taxsuborder, taxgrtgroup, taxsubgrp, taxpartsize, taxpartsizemod, taxceactcl, taxreaction, taxtempcl, originyear, establishedyear, soiltaxclasslastupdated, soilseriesiid
  
  FROM 
  soilseries ss 

  INNER JOIN
      area       a  ON a.areaiid      = ss.typelocstareaiidref
  INNER JOIN
      areatype   at ON at.areatypeiid = ss.typelocstareatypeiidref

  ORDER BY soilseriesname
  ;"
  
  # LEFT OUTER JOIN
  #     soilseriestaxmineralogy sstm ON sstm.soilseriesiidref = ss.soilseriesiid
  
  # setup connection local NASIS
  channel <- RODBC::odbcDriverConnect(connection=getOption('soilDB.NASIS.credentials'))
  
  # exec query
  d.soilseries <- RODBC::sqlQuery(channel, q.soilseries, stringsAsFactors = FALSE)
  
  # close connection
  RODBC::odbcClose(channel)
  
  # recode metadata domains
  d.soilseries <- uncode(d.soilseries, stringsAsFactors = stringsAsFactors)
  
  # prep
  d.soilseries$soiltaxclasslastupdated <- format(d.soilseries$soiltaxclasslastupdated, "%Y")
  
  # done
  return(d.soilseries)
}



get_soilseries_from_NASISWebReport <- function(soils, stringsAsFactors = default.stringsAsFactors()) {
  
  url <-"https://nasis.sc.egov.usda.gov/NasisReportsWebSite/limsreport.aspx?report_name=get_soilseries_from_NASISWebReport"
  
  d.ss <- lapply(soils, function(x) {
    args = list(p_soilseriesname = x)
    d    = parseWebReport(url, args)
  })
  d.ss <- do.call("rbind", d.ss)
  
  # set factor levels according to metadata domains
  d.ss[! names(d.ss) %in% c("mlraoffice", "taxminalogy")] <- uncode(d.ss[! names(d.ss) %in% c("mlraoffice", "taxminalogy")], db = "SDA", stringsAsFactors = stringsAsFactors)
  d.ss[names(d.ss) %in% c("mlraoffice")] <- uncode(d.ss[names(d.ss) %in% c("mlraoffice")], db = "LIMS", stringsAsFactors = stringsAsFactors)
  
  # return data.frame
  return(d.ss)
}
