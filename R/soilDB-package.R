#' Soil Database Interface
#'
#' A collection of functions for reading soil data from U.S. Department of Agriculture Natural Resources Conservation Service (USDA-NRCS) and National Cooperative Soil Survey (NCSS) databases
#'
#' This package provides methods for extracting soils information from local NASIS databases (MS
#' SQL Server), local PedonPC and AKSite databases (MS Access format), Soil Data Access, and 
#' other soil-related web services.
#' 
#' @name soilDB-package
#' @aliases soilDB-package soilDB
#' @author J.M. Skovlin, D.E. Beaudette, S.M Roecker, A.G. Brown
#' @seealso \code{\link{fetchNASIS}, \link{SDA_query}, \link{loafercreek}}
#' @importFrom grDevices rgb convertColor col2rgb
#' @importFrom stats aggregate complete.cases na.omit as.formula splinefun weighted.mean
#' @importFrom utils URLencode object.size read.csv read.table read.delim setTxtProgressBar txtProgressBar write.table type.convert unzip
#' @importFrom data.table rbindlist data.table as.data.table merge.data.table
#' @importFrom DBI dbGetQuery dbConnect dbSendQuery dbFetch
#' @importFrom methods slot slot<- as is
#' @importFrom aqp plotSPC checkHzDepthLogic pbindlist SoilTextureLevels parseMunsell
#' @importClassesFrom aqp SoilProfileCollection
#' @importMethodsFrom aqp plot length nrow site depths<- site<- horizons horizons<- hzidname<- idname hzidname horizonDepths diagnostic_hz diagnostic_hz<- restrictions restrictions<- metadata metadata<- hzdesgnname hzdesgnname<- hztexclname hztexclname<- profile_id profile_id<- hzID hzID<- aqp_df_class aqp_df_class<- siteNames horizonNames
"_PACKAGE"

#' Example \code{SoilProfilecollection} Objects Returned by \code{fetchNASIS}.
#'
#' Several examples of soil profile collections returned by
#' \code{fetchNASIS(from='pedons')} as \code{SoilProfileCollection} objects.
#'
#' @name loafercreek
#' @aliases loafercreek gopheridge mineralKing
#' @docType data
#' @keywords datasets
#' @examplesIf require("aqp")
#' @examples
#'
#' \donttest{
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
#'   plot(gopheridge[1:10, ], name='hzname', color='soil_color', label='upedonid', id.style='side')
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
#'   plot(loafercreek[1:10, ], name='hzname', color='soil_color', label='upedonid', id.style='side')
#'
#'   # add rock fragment data to plot:
#'   addVolumeFraction(loafercreek[1:10, ], colname='total_frags_pct')
#'
#'   # add diagnostic horizons
#'   addDiagnosticBracket(loafercreek[1:10, ], kind='argillic horizon', col='red', offset=-0.4)
#' }
#'
NULL

#' USDA-NRCS Station Metadata for SCAN, CSCAN, SNOTEL, SNOWLITE Networks
#'
#' These metadata are a work in progress.
#'
#' @name SCAN_SNOTEL_metadata
#' @aliases SCAN_SNOTEL_metadata state_FIPS_codes
#' @docType data
#' @format A `data.frame` with 1186 SCAN, CSCAN, SNOTEL, and SNOWLITE station metadata records
#' @keywords datasets
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
#'
#' @format A data.frame with 5209 observations on the following 5 variables.
#' - `"ssa"`: Soil Survey name, a character vector
#' - `"year"`: Year of publication, a numeric vector
#' - `"pdf"`: Does a manuscript PDF document exist? a logical vector
#' - `"state"`: State abbreviation, a character vector
#'
#' @source https://www.nrcs.usda.gov/wps/portal/nrcs/soilsurvey/soils/survey/state/
#' @keywords datasets
NULL

#' NASIS 7 Metadata
#'
#' NASIS 7 Metadata from MetadataDomainDetail, MetadataDomainMaster, and MetadataTableColumn tables
#'
#' @format A `data.frame` with the following columns:
#'  - `DomainID` - Integer. ID that uniquely identifies a domain in a data model, not just within a database.
#'  - `DomainName` - Character. Domain Name.
#'  - `DomainRanked` - Integer. Is domain ranked? `0` = No; `1` = Yes
#'  - `DisplayLabel` - Character. Domain Display Label.
#'  - `ChoiceSequence` - Integer. Order or sequence of Choices.
#'  - `ChoiceValue` - Integer. Value of choice level.
#'  - `ChoiceName` - Character. Name of choice level.
#'  - `ChoiceLabel` - Character. Label of choice level.
#'  - `ChoiceObsolete` - Integer. Is choice level obsolete? `0` = No; `1` = Yes
#'  - `ColumnPhysicalName` - Character. Physical column name.
#'  - `ColumnLogicalName` - Character. Logical column name.
#'
#' @name metadata
#' @docType data
#'
#' @keywords datasets
NULL

#' NASIS 7 Tables, Columns and Foreign Keys
#'
#' This dataset contains NASIS 7 Tables, Columns and Foreign Keys
#'
#' @name NASIS_table_column_keys
#' @docType data
#'
#' @keywords datasets
NULL
