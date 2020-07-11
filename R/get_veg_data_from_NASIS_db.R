## TODO: merge with other vegplot functions

get_veg_data_from_NASIS_db <- function(SS=TRUE) {
  # must have RODBC installed
  if(!requireNamespace('RODBC'))
    stop('please install the `RODBC` package', call.=FALSE)

# warning to use NASIS query to load related vegplot data for this to work
warning("In order to query this data you'll need to load all related vegplots to your sites and pedons in NASIS.", call. = FALSE)

  ## does this require tables that standard site/pedons queries don't hit?
  # https://github.com/ncss-tech/soilDB/issues/49
  # existing veg
  q.veg <- "SELECT siteiid, vegplotid, vegplotname, obsdate, primarydatacollector, datacollectionpurpose, assocuserpedonid, ppi.seqnum, plantsym, plantsciname, plantnatvernm, orderofdominance, speciescancovpct, speciescancovclass

  FROM site_View_1 AS s
  INNER JOIN siteobs_View_1 AS so ON so.siteiidref = s.siteiid
  LEFT JOIN vegplot_View_1 AS v on v.siteobsiidref = so.siteobsiid
  LEFT JOIN plotplantinventory_View_1 AS ppi ON ppi.vegplotiidref = v.vegplotiid
  -- note: plant table not managed by SS
  LEFT OUTER JOIN plant ON plant.plantiid = ppi.plantiidref;"

  # toggle selected set vs. local DB
  if(SS == FALSE) {
    q.veg <- gsub(pattern = '_View_1', replacement = '', x = q.veg, fixed = TRUE)
  }

  # existing veg
#q.inventory <- "SELECT siteiid, vegplotid, vegplotname, obsdate, primarydatacollector, datacollectionpurpose, assocuserpedonid, #plotplantinventory.seqnum, plantsym, plantsciname, plantnatvernm, orderofdominance, speciescancovpct, speciescancovclass
#
#  FROM site_View_1 AS s
#  INNER JOIN siteobs ON siteobs.siteiidref=s.siteiid
#  LEFT JOIN vegplot_View_1 AS v on v.siteobsiidref=siteobs.siteobsiid
#  LEFT JOIN plotplantinventory ON plotplantinventory.vegplotiidref=v.vegplotiid
#  INNER JOIN plant ON plant.plantiid=plotplantinventory.plantiidref
#  ORDER BY s.siteiid, plotplantinventory.seqnum;"

# existing veg
q.vegtransect <- "SELECT siteiid, vegplotid, vegplotname, obsdate, primarydatacollector, datacollectionpurpose, assocuserpedonid, vegtransectid, vegtransplantsummiid vtpsiid, transectlength, plantsym, plantsciname, plantnatvernm

  FROM site_View_1 AS s
  INNER JOIN siteobs ON siteobs.siteiidref=s.siteiid
  INNER JOIN vegplot_View_1 AS v on v.siteobsiidref=siteobs.siteobsiid
  LEFT JOIN vegtransect AS vt ON vt.vegplotiidref=v.vegplotiid
  LEFT JOIN vegtransectplantsummary AS vtps ON vtps.vegtransectiidref=vt.vegtransectiid
  INNER JOIN plant ON plant.plantiid=vtps.plantiidref
  ORDER BY s.siteiid;"

# toggle selected set vs. local DB
  if(SS == FALSE) {
    q.veg <- gsub(pattern = '_View_1', replacement = '', x = q.veg, fixed = TRUE)
  }

#q.plant <- "SELECT plantiid, plantsym
#  FROM plant_View_1;"

channel <- .openNASISchannel()
if (channel == -1)
  return(data.frame())

# exec queries
d.veg <- RODBC::sqlQuery(channel, q.veg, stringsAsFactors=FALSE)
d.vegtransect <- RODBC::sqlQuery(channel, q.vegtransect, stringsAsFactors=FALSE)

# close connection
RODBC::odbcClose(channel)


# return a list of results
return(list(veg=d.veg,
            vegtransect=d.vegtransect))
}
