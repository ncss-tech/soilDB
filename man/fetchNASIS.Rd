\name{fetchNASIS}
\alias{fetchNASIS}
\alias{getHzErrorsNASIS}
\alias{fetchNASIS_pedons}
\alias{get_phorizon_from_NASIS_db}
\alias{fetchNASIS_components}
\alias{get_component_copm_data_from_NASIS_db}
\alias{get_component_horizon_data_from_NASIS_db}
\alias{get_component_correlation_data_from_NASIS_db}
\alias{get_component_copm_data_from_NASIS_db}
\alias{get_component_cogeomorph_data_from_NASIS_db}
\alias{get_component_esd_data_from_NASIS_db}
\alias{get_component_otherveg_data_from_NASIS_db}
\alias{get_copedon_from_NASIS_db}

\alias{get_legend_from_NASIS}
\alias{get_lmuaoverlap_from_NASIS}
\alias{get_mapunit_from_NASIS}
\alias{get_projectmapunit_from_NASIS}
\alias{get_projectstaff_from_NASIS}
\alias{get_component_diaghz_from_NASIS_db}
\alias{get_mutext_from_NASIS_db}

\alias{fetchVegdata}
\alias{get_vegplot_from_NASIS_db}
\alias{get_vegplot_location_from_NASIS_db}
\alias{get_vegplot_species_from_NASIS_db}
\alias{get_vegplot_textnote_from_NASIS_db}
\alias{get_vegplot_transect_from_NASIS_db}
\alias{get_vegplot_transpecies_from_NASIS_db}
\alias{get_vegplot_tree_si_details_from_NASIS_db}
\alias{get_vegplot_tree_si_summary_from_NASIS_db}
\alias{get_vegplot_trhi_from_NASIS_db}



\title{Fetch commonly used site/pedon/horizon or component data from a local NASIS database.}
\description{Fetch commonly used site/pedon/horizon data or component from a local NASIS database, return as a SoilProfileCollection object.}

\usage{
fetchNASIS(from = 'pedons', ...)

fetchNASIS_pedons(SS=TRUE, rmHzErrors=TRUE, nullFragsAreZero=TRUE, 
                  soilColorState='moist', lab=FALSE, 
                  stringsAsFactors = default.stringsAsFactors()
                  )
fetchNASIS_components(SS=TRUE, rmHzErrors=TRUE, fill = FALSE, 
                      stringsAsFactors = default.stringsAsFactors()
                      )
getHzErrorsNASIS(strict=TRUE)
}

\arguments{
  \item{from}{determines what objects should fetched? ('pedons' | 'components')}
  \item{\dots}{arguments passed to \code{fetchNASIS_pedons()} or \code{fetchNASIS_components()}}
  \item{SS}{fetch data from the currently loaded selected set in NASIS or from the entire local database (default: TRUE)}
  \item{stringsAsFactors}{logical: should character vectors be converted to factors? This argument is passed to the uncode() function. It does not convert those vectors that have been set outside of uncode() (i.e. hard coded). The 'factory-fresh' default is TRUE, but this can be changed by setting options(stringsAsFactors = FALSE)}
  \item{rmHzErrors}{should pedons with horizonation errors be removed from the results? (default: TRUE)}
  \item{nullFragsAreZero}{should fragment volumes of NULL be interpreted as 0? (default: TRUE), see details}
  \item{soilColorState}{which colors should be used to generate the convenience field 'soil_color'? ('moist' | 'dry')}
  \item{lab}{should the phlabresults child table be fetched with site/pedon/horizon data (default: FALSE)}  
  \item{fill}{(fetchNASIS_components only: include components without horizon data? (default: FALSE)}
  \item{strict}{how strict should horizon boundaries be checked for consistency: TRUE=more | FALSE=less}
}

\value{a SoilProfileCollection class object}
\author{D. E. Beaudette and J. M. Skovlin}

\details{The value of \code{nullFragsAreZero} will have a significant impact on the rock fragment fractions returned by \code{fetchNASIS}. Set \code{nullFragsAreZero = FALSE} in those cases where there are many data-gaps and NULL rock fragment values should be interpretated as NULLs. Set \code{nullFragsAreZero = TRUE} in those cases where NULL rock fragment values should be interpreted as 0.

This function attempts to do most of the boilerplate work when extracting site/pedon/horizon or component data from a local NASIS database. Pedons that are missing horizon data, or have errors in their horizonation are excluded from the returned object, however, their IDs are printed on the console. Pedons with combination horizons (e.g. B/C) are erroneously marked as errors due to the way in which they are stored in NASIS as two overlapping horizon records.

See \code{\link{getHzErrorsNASIS}} for a simple approach to identifying pedons with problematic horizonation.

See the \href{http://ncss-tech.github.io/AQP/soilDB/NASIS-component-data.html}{NASIS component tutorial}, and \href{http://ncss-tech.github.io/AQP/soilDB/fetchNASIS-mini-tutorial.html}{NASIS pedon tutorial} for more information.}

\note{This function currently works only on Windows, and requires a 'nasis_local' ODBC connection.}

\examples{
\donttest{
# query depends on some pedon data, queried against the national database
# note that you must setup this connection ahead of time
f <- fetchNASIS(from = 'pedons')

# plot only those profiles with densic contact
plot(f[which(f$densic.contact), ], name='hzname')

# get basic component data from local NASIS, after performing a 
# DMU-* query against the national database
fc <- fetchNASIS(from = 'components')
}
}

\keyword{manip}

