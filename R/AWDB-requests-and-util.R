##
##
##

## 
.AWDB_elementSets <- list(
  'SCAN' = c(
    "STO", "SMS", "RDC", "SAL", "PREC", "TOBS", "SRADV", 
    "WDIRV", "WSPDV", "WSPDX", "DPTP", "PRCP", 
    "PVPV", "RHUM", "RHUMN", "RHUMX", "SVPV", "SNWD", 
    "NTRDV", "WDIR", "WTEQ"
  ), 
  'SNTL' = c(
    "STO", "SMS", "TOBS", "RDC", "SAL", 
    "PREC", "WTEQ", "SNWD", "PTEMP", 
    "WDIRV", "WSPDX", "WSPDV", "SRADV", "RHUMV", "SRADX", "RHUMX", 
    "RHUM", "SRAD", "SRADN", "RHUMN", "PRES", "SWINV", "SWOTV", "WDIRZ", 
    "LWINV", "LWOTV", "PVPV"
  ), 
  'SNOW' = c("SNDN", "SNWD", "WTEQ"), 
  'SNTLT' = c(
    "TOBS", "SNWD", "STO", "SMS", "WSPDV", 
    "WDIRV", "WSPDX", "PREC", "RDC", "SAL", "WDIRZ", 
    "SMV", "SRADV", "STV", "AWDC", "RHUM", 
    "RHUMN", "RHUMV", "RHUMX", "SRAD", "SRADN", "SRADX", "WDIR"
  )
)



#' @title Get AWDB Element (Sensor) Metadata
#' @description This function retrieves all element (sensor) metadata from the AWDB webservice.
#' 
#' @param ... Additional arguments to `.soilDB_curl_get_JSON()`, such as `timeout = 120`
#' @return A `data.frame` unless network or server error, then `NULL`
#' 
get_AWDB_elements <- function(...) {
  
  .u <- 'https://wcc.sc.egov.usda.gov/awdbRestApi/services/v1/reference-data?referenceLists=elements&visibility=all'
  .res <- .soilDB_curl_get_JSON(.u, gzip = FALSE, quiet = TRUE, ...)
  
  # errors result in NULL
  if(inherits(.res, 'data.frame')) {
    .res <- .res$elements
  }
  
  return(.res)
}





