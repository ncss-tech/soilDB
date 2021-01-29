

#' Example \code{SoilProfilecollection} Objects Returned by \code{fetchNASIS}.
#' 
#' Several examples of soil profile collections returned by
#' \code{fetchNASIS(from='pedons')} as \code{SoilProfileCollection} objects.
#' 
#' 
#' @name loafercreek
#' @aliases loafercreek gopheridge mineralKing
#' @docType data
#' @keywords datasets
#' @examples
#' 
#' \donttest{
#' if(require("aqp")) {
#' # load example dataset
#'   data("gopheridge")
#'   
#'   # what kind of object is this?
#'   class(gopheridge)
#'   
#'   # how many profiles?
#'   length(gopheridge)
#'   
#'   # there are 60 profiles, this calls for a split plot
#'   par(mar=c(0,0,0,0), mfrow=c(2,1))
#'   
#'   # plot soil colors
#'   plot(gopheridge[1:30, ], name='hzname', color='soil_color')
#'   plot(gopheridge[31:60, ], name='hzname', color='soil_color')
#'   
#'   # need a larger top margin for legend
#'   par(mar=c(0,0,4,0), mfrow=c(2,1))
#'   # generate colors based on clay content
#'   plot(gopheridge[1:30, ], name='hzname', color='clay')
#'   plot(gopheridge[31:60, ], name='hzname', color='clay')
#'   
#'   # single row and no labels
#'   par(mar=c(0,0,0,0), mfrow=c(1,1))
#'   # plot soils sorted by depth to contact
#'   plot(gopheridge, name='', print.id=FALSE, plot.order=order(gopheridge$bedrckdepth))
#'   
#'   # plot first 10 profiles
#'   plot(gopheridge[1:10, ], name='hzname', color='soil_color', label='pedon_id', id.style='side')
#'   
#'   # add rock fragment data to plot:
#'   addVolumeFraction(gopheridge[1:10, ], colname='total_frags_pct')
#'   
#'   # add diagnostic horizons
#'   addDiagnosticBracket(gopheridge[1:10, ], kind='argillic horizon', col='red', offset=-0.4)
#'   
#'   ## loafercreek
#'   data("loafercreek")
#'   # plot first 10 profiles
#'   plot(loafercreek[1:10, ], name='hzname', color='soil_color', label='pedon_id', id.style='side')
#'   
#'   # add rock fragment data to plot:
#'   addVolumeFraction(loafercreek[1:10, ], colname='total_frags_pct')
#'   
#'   # add diagnostic horizons
#'   addDiagnosticBracket(loafercreek[1:10, ], kind='argillic horizon', col='red', offset=-0.4)
#' }
#' }
#' 
NULL





#' SCAN and SNOTEL Station Metadata
#' 
#' SCAN and SNOTEL station metadata, a work in progress.
#' 
#' These data have been compiled from several sources and represent a
#' progressive effort to organize SCAN/SNOTEL station metadata. Therefore, some
#' records may be missing or incorrect. Details on this effort can be found at
#' the associated GitHub issue page:
#' \url{https://github.com/ncss-tech/soilDB/issues/61}.
#' 
#' @name SCAN_SNOTEL_metadata
#' @aliases SCAN_SNOTEL_metadata state_FIPS_codes
#' @docType data
#' @format A data frame with 1092 observations on the following 12 variables.
#' \describe{ \item{list("Name")}{station name} \item{list("Site")}{station ID}
#' \item{list("State")}{state} \item{list("Network")}{sensor network: SCAN /
#' SNOTEL} \item{list("County")}{county} \item{list("Elevation_ft")}{station
#' elevation in feet} \item{list("Latitude")}{latitude of station}
#' \item{list("Longitude")}{longitude of station} \item{list("HUC")}{associated
#' watershed} \item{list("climstanm")}{climate station name (TODO: remove this
#' column)} \item{list("upedonid")}{associated user pedon ID}
#' \item{list("pedlabsampnum")}{associated lab sample ID} }
#' @keywords datasets
NULL





#' Soil Database Interface
#' 
#' This package provides methods for extracting soils information from local
#' PedonPC and AK Site databases (MS Access format), local NASIS databases (MS
#' SQL Server), and the SDA webservice. Currently USDA-NCSS data sources are
#' supported, however, there are plans to develop interfaces to outside systems
#' such as the Global Soil Mapping project.
#' 
#' It can be difficult to locate all of the dependencies required for
#' sending/processing SOAP requests, especially on UNIX-like operating systems.
#' Windows binary packages for the dependencies can be found
#' \href{http://www.stats.ox.ac.uk/pub/RWin/bin/windows/contrib/2.15/here}. See
#' \code{\link{fetchPedonPC}} for a simple wrapper function that should suffice
#' for typical site/pedon/hz queries. An introduction to the soilDB package can
#' be found
#' \href{https://r-forge.r-project.org/scm/viewvc.php/*checkout*/docs/soilDB/soilDB-Intro.html?root=aqphere}.
#' 
#' @name soilDB-package
#' @aliases soilDB.env soilDB-package soilDB
#' @docType package
#' @author J.M. Skovlin and D.E. Beaudette
#' @seealso \code{\link{fetchPedonPC}, \link{fetchNASIS}, \link{SDA_query},
#' \link{loafercreek}}
#' @keywords package
NULL





#' Timeline of US Published Soil Surveys
#' 
#' This dataset contains the years of each US Soil Survey was published.
#' 
#' This data was web scraped from the NRCS Soils Website. The scraping
#' procedure and a example plot are included in the examples section below.
#' 
#' @name us_ss_timeline
#' @docType data
#' @format A data frame with 5209 observations on the following 5 variables.
#' \describe{ \item{list("ssa")}{Soil Survey name, a character vector}
#' \item{list("year")}{year of publication, a numeric vector}
#' \item{list("pdf")}{does a pdf exists, a logical vector}
#' \item{list("state")}{State abbreviation, a character vector} }
#' @source
#' https://www.nrcs.usda.gov/wps/portal/nrcs/soilsurvey/soils/survey/state/
#' @keywords datasets
NULL



