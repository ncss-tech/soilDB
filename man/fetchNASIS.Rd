\name{fetchNASIS}
\alias{fetchNASIS}
\alias{get_extended_data_from_NASIS_db}
\alias{getHzErrorsNASIS}
\alias{simplfyFragmentData}
\alias{fetchNASIS_pedons}
\alias{get_phorizon_from_NASIS_db}
\alias{fetchNASIS_component_data}
\alias{get_component_copm_data_from_NASIS_db}
\alias{get_component_data_from_NASIS_db}
\alias{get_component_horizon_data_from_NASIS_db}
\alias{get_component_correlation_data_from_NASIS_db}
\alias{get_component_copm_data_from_NASIS_db}
\alias{get_component_cogeomorph_data_from_NASIS_db}
\alias{get_component_esd_data_from_NASIS_db}
\alias{get_component_otherveg_data_from_NASIS_db}
\alias{get_copedon_from_NASIS_db}
\alias{get_comonth_from_NASIS_db}
\alias{get_vegplot_from_NASIS_db}
\alias{get_vegplot_from_NASIS_db2}
\alias{get_projectmapunit_from_NASIS}
\alias{get_text_notes_from_NASIS_db}

\title{Fetch commonly used site/pedon/horizon data from a PedonPC database.}
\description{Fetch commonly used site/pedon/horizon data from a PedonPC or local NASIS database, return as a SoilProfileCollection object.}

\usage{
fetchNASIS(rmHzErrors = TRUE, nullFragsAreZero=TRUE, soilColorState='moist', lab='FALSE')
fetchNASIS_component_data(rmHzErrors=TRUE, fill = FALSE)
getHzErrorsNASIS(strict=TRUE)
get_comonth_from_NASIS_db(fill=FALSE)
}

\arguments{
  \item{rmHzErrors}{should pedons with horizonation errors be removed from the results? (default: TRUE)}
  \item{nullFragsAreZero}{should fragment volumes of NULL be interpreted as 0? (default: TRUE), see details}
  \item{soilColorState}{which colors should be used to generate the convenience field 'soil_color'? ('moist' | 'dry')}
  \item{lab}{should the phlabresults child table be fetched (default: FALSE)}  
  \item{strict}{should horizonation by strictly enforced? (TRUE)}
  \item{fill}{should missing "month" rows in the comonth table be filled with NA (FALSE)}
}

\value{a SoilProfileCollection class object}
\author{D. E. Beaudette and J. M. Skovlin}

\details{The value of \code{nullFragsAreZero} will have a significant impact on the rock fragment fractions returned by \code{fetchNASIS}. Set \code{nullFragsAreZero = FALSE} in those cases where there are many data-gaps and NULL rock fragment values should be interpretated as NULLs. Set \code{nullFragsAreZero = TRUE} in those cases where NULL rock fragment values should be interpreted as 0.

This function attempts to do most of the boilerplate work when extracting site/pedon/horizon data from a local NASIS database. Pedons that are missing horizon data, or have errors in their horizonation are excluded from the returned object, however, their IDs are printed on the console. Pedons with combination horizons (e.g. B/C) are erroneously marked as errors due to the way in which they are stored in NASIS as two overlapping horizon records.

See \code{\link{getHzErrorsNASIS}} for a simple approach to identifying pedons with problematic horizonation.

See the \href{http://ncss-tech.github.io/AQP/soilDB/NASIS-component-data.html}{NASIS component tutorial}, and \href{http://ncss-tech.github.io/AQP/soilDB/fetchNASIS-mini-tutorial.html}{NASIS pedon tutorial} for more information.}

\note{This function currently works only on Windows, and requires a 'nasis_local' ODBC connection.}

\examples{
\dontrun{
# query depends on some pedon data, queried against the national database
# note that you must setup this connection ahead of time
f <- fetchNASIS()

# plot only those profiles with densic contact
plot(f[which(f$densic.contact), ], name='hzname')

# get basic component data from local NASIS, after performing a 
# DMU-* query against the national database
fc <- fetchNASIS_component_data()
}
}

\keyword{manip}

