# Script for extracting vegetation species and cover data from an NPS PLOTS database
# add as get_veg_from_NPS_PLOTS_db() to soilDB package
# Jay Skovlin, 12/4/2013
# dsn <- "H:/GNP_vegetation_data_MR/GlacierNP_vegdata/PLOTS_v32_BE.accdb"
get_veg_from_NPS_PLOTS_db <- function(dsn) {
  # must have RODBC installed
  if(!requireNamespace('RODBC'))
    stop('please install the `RODBC` package', call.=FALSE)
  
  q.vegdata <- "SELECT tPlots.Plot_Code AS site_id, tPlotEventSpecies.Stratum, tPlotEventSpecies.Spp_Code, xPLANTS_lu.Sci_Name, xPLANTS_lu.ComName, tPlotEventSpecies.Cover_Code, tPlotEventSpecies.Real_Cover, tPlotEventSpecies.Within_Plot
FROM xPLANTS_lu RIGHT JOIN (tSpecies RIGHT JOIN (tPlots INNER JOIN (tPlotEvents INNER JOIN tPlotEventSpecies ON tPlotEvents.Plot_Event = tPlotEventSpecies.Plot_Event) ON tPlots.Plot_Code = tPlotEvents.Plot_Code) ON tSpecies.Spp_Code = tPlotEventSpecies.Spp_Code) ON xPLANTS_lu.Plants_Symbol = tSpecies.PLANTS_Symbol
ORDER BY tPlots.Plot_Code, tPlotEventSpecies.Stratum DESC , tPlotEventSpecies.Real_Cover DESC"
  
  # setup connection to our pedon database 
  channel <- RODBC::odbcConnectAccess2007(dsn, readOnlyOptimize=TRUE)
  
  # exec query
  d <- RODBC::sqlQuery(channel, q.vegdata, stringsAsFactors=FALSE)
  
  # close connection
  RODBC::odbcClose(channel)
}
