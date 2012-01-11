get_veg_from_MT_veg_db <- function(dsn) {
	
	#pull site and plot data and total production value
	q <- "SELECT tblSites.SiteKey, tblPlots.PlotKey, SiteID, EScalled.EcolSite as Ecol_Site_called, EScalled.SiteName as Ecol_Sitename_called, ESkeyed.EcolSite as Ecol_Site_keyed, ESkeyed.SiteName as Ecol_Sitename_keyed, tblPlots.ESD_MLRA as MLRA, tblPLots.ESD_LRU as LRU, HT.abbreviation as Habitat_Type, TP.total_production as total_production  
  FROM (
	(
	(
	(tblSites INNER JOIN tblPlots ON tblSites.SiteKey = tblPlots.SiteKey)
	LEFT OUTER JOIN (SELECT tblESD_DK.PlotKey, SUM(tblESD_DK.Production) as total_production FROM tblESD_DK GROUP BY tblESD_DK.PlotKey) AS TP ON TP.PlotKey =   tblPlots.PlotKey)
	LEFT OUTER JOIN (SELECT * FROM tblEcolSites) as EScalled ON tblPlots.EcolSite = EScalled.EcolSite)
	LEFT OUTER JOIN (SELECT * FROM tblEcolSites) as ESkeyed ON tblPlots.EcolSite = ESkeyed.EcolSite)
	LEFT OUTER JOIN (SELECT * FROM HabitatTypes) as HT ON HT.htcode = tblPlots.EcolSite_Text2
	ORDER BY tblSites.SiteID;"
  
	# setup connection to our pedon database
	channel <- odbcConnectAccess(dsn, readOnlyOptimize=TRUE)
	
	# exec query
	d <- sqlQuery(channel, q, stringsAsFactors=FALSE)
	
	# close connection
	odbcClose(channel)
	
	# done
	return(d)
}

