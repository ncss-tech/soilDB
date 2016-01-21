## TODO: merge with other vegplot functions

get_vegplot_from_NASIS_db2 <- function() {
  # must have RODBC installed
  if(!requireNamespace('RODBC'))
    stop('please install the `RODBC` package', call.=FALSE)
  
  # existing veg
q.inventory <- "SELECT siteiid, vegplotid, vegplotname, obsdate, primarydatacollector, datacollectionpurpose, assocuserpedonid, plotplantinventory.seqnum, plantsym, plantsciname, plantnatvernm, orderofdominance, speciescancovpct, speciescancovclass

  FROM site_View_1 AS s
  INNER JOIN siteobs ON siteobs.siteiidref=s.siteiid
  LEFT JOIN vegplot_View_1 AS v on v.siteobsiidref=siteobs.siteobsiid
  LEFT JOIN plotplantinventory ON plotplantinventory.vegplotiidref=v.vegplotiid
  INNER JOIN plant ON plant.plantiid=plotplantinventory.plantiidref
  ORDER BY s.siteiid, plotplantinventory.seqnum;"

# existing veg
q.transect <- "SELECT siteiid, vegplotid, vegplotname, obsdate, primarydatacollector, datacollectionpurpose, assocuserpedonid, vegtransectid, vegtransplantsummiid vtpsiid, transectlength, plantsym, plantsciname, plantnatvernm

  FROM site_View_1 AS s
  INNER JOIN siteobs ON siteobs.siteiidref=s.siteiid
  INNER JOIN vegplot_View_1 AS v on v.siteobsiidref=siteobs.siteobsiid
  LEFT JOIN vegtransect AS vt ON vt.vegplotiidref=v.vegplotiid
  LEFT JOIN vegtransectplantsummary AS vtps ON vtps.vegtransectiidref=vt.vegtransectiid
  INNER JOIN plant ON plant.plantiid=vtps.plantiidref
  ORDER BY s.siteiid;"

q.plant <- "SELECT plantiid, plantsym

  FROM plant_View_1;"

# setup connection local NASIS
channel <- RODBC::odbcDriverConnect(connection="DSN=nasis_local;UID=NasisSqlRO;PWD=nasisRe@d0n1y")

# exec queries
d.inventory <- RODBC::sqlQuery(channel, q.inventory, stringsAsFactors=FALSE)
d.transect <- RODBC::sqlQuery(channel, q.transect, stringsAsFactors=FALSE)

# close connection
RODBC::odbcClose(channel)


# return a list of results
return(list(inventory=d.inventory, 
            transect=d.transect))
}