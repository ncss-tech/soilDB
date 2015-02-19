get_veg_other_from_MT_veg_db <- function(dsn) {
  
  # must have RODBC installed
  if(!requireNamespace('RODBC'))
    stop('please install the `RODBC` package', call.=FALSE)
	
	#pull site and plot data and total production value
	q <- "SELECT tblESD_DK.PlotKey, tblESD_DK.DKKey, tblSites.SiteID AS site_id, tblESD_DK.SpeciesSymbol, tblESD_DK.LPIPercentBasal, tblESD_DK.CanopyCoverPercent
FROM tblSites LEFT JOIN ((tblPlots LEFT JOIN tblESD_DK ON tblPlots.PlotKey = tblESD_DK.PlotKey) LEFT JOIN (SELECT * FROM tblSpecies)  AS Spec ON tblESD_DK.SpeciesSymbol = Spec.SpeciesCode) ON tblSites.SiteKey = tblPlots.SiteKey
	WHERE (((tblESD_DK.PlotKey) Is Not Null) AND ((tblESD_DK.SpeciesSymbol) Like 'shrub canopy' Or (tblESD_DK.SpeciesSymbol) Like 'tree canopy' Or (tblESD_DK.SpeciesSymbol) Like 'bare ground' Or (tblESD_DK.SpeciesSymbol) Like 'rock or logs' Or (tblESD_DK.SpeciesSymbol) Like 'clubmoss' Or (tblESD_DK.SpeciesSymbol) Like 'basal vegetation') AND ((tblESD_DK.DKClass) Is Null))
	ORDER BY tblESD_DK.PlotKey, tblESD_DK.SpeciesSymbol;"

	# setup connection to our pedon database
	channel <- RODBC::odbcConnectAccess(dsn, readOnlyOptimize=TRUE)
	
	# exec query
	d <- RODBC::sqlQuery(channel, q, stringsAsFactors=FALSE)
	
	# close connection
	RODBC::odbcClose(channel)
	
	# done
	return(d)
}







