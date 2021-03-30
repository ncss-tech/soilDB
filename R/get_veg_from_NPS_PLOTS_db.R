# Script for extracting vegetation species and cover data from an NPS PLOTS database
# add as get_veg_from_NPS_PLOTS_db() to soilDB package
# Jay Skovlin, 12/4/2013
# dsn <- "H:/GNP_vegetation_data_MR/GlacierNP_vegdata/PLOTS_v32_BE.accdb"


#' Retrieve Vegetation Data from an NPS PLOTS Database
#' 
#' Used to extract species, stratum, and cover vegetation data from a backend
#' NPS PLOTS Database.  Currently works for any Microsoft Access database with
#' an .mdb file format.
#' 
#' 
#' @param dsn file path to the NPS PLOTS access database on your system.
#' @return A data.frame with vegetation data in a long format with linkage to
#' NRCS soil pedon data via the site_id key field.
#' @note This function currently only works on Windows.
#' @author Jay M. Skovlin
#' @keywords manip
#' @export get_veg_from_NPS_PLOTS_db
get_veg_from_NPS_PLOTS_db <- function(dsn) {
  # must have odbc installed
  if(!requireNamespace('odbc'))
    stop('please install the `odbc` package', call.=FALSE)
  
  q <- "SELECT tPlots.Plot_Code AS site_id, tPlotEventSpecies.Stratum, tPlotEventSpecies.Spp_Code, xPLANTS_lu.Sci_Name, xPLANTS_lu.ComName, tPlotEventSpecies.Cover_Code, tPlotEventSpecies.Real_Cover, tPlotEventSpecies.Within_Plot
FROM xPLANTS_lu RIGHT JOIN (tSpecies RIGHT JOIN (tPlots INNER JOIN (tPlotEvents INNER JOIN tPlotEventSpecies ON tPlotEvents.Plot_Event = tPlotEventSpecies.Plot_Event) ON tPlots.Plot_Code = tPlotEvents.Plot_Code) ON tSpecies.Spp_Code = tPlotEventSpecies.Spp_Code) ON xPLANTS_lu.Plants_Symbol = tSpecies.PLANTS_Symbol
ORDER BY tPlots.Plot_Code, tPlotEventSpecies.Stratum DESC , tPlotEventSpecies.Real_Cover DESC"
  
  # setup connection to our pedon database
  channel <- DBI::dbConnect(odbc::odbc(), .connection_string = paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=", dsn))
  
  # exec query
  d <- DBI::dbGetQuery(channel, q)
  
  # close connection
  DBI::dbDisconnect(channel)
  return(d)
}
