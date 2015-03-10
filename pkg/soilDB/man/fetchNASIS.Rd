\name{fetchNASIS}
\alias{get_text_notes_from_NASIS_db}
\alias{fetchNASIS}
\alias{fetchNASIS_component_data}
\alias{get_component_data_from_NASIS_db}
\alias{get_copedon_from_NASIS_db}
\alias{get_component_horizon_data_from_NASIS_db}
\alias{getHzErrorsNASIS}

\title{Fetch commonly used site/horizon data from a PedonPC database.}
\description{Fetch commonly used site/horizon data from a PedonPC or local NASIS database, return as a SoilProfileCollection object.}

\usage{
fetchNASIS(rmHzErrors = TRUE, nullFragsAreZero=TRUE)
fetchNASIS_component_data()
getHzErrorsNASIS(strict=TRUE)
}

\arguments{
  \item{rmHzErrors}{should pedons with horizonation errors be removed from the results? (default: TRUE)}
  \item{nullFragsAreZero}{should fragment volumes of NULL be interpreted as 0? (default: TRUE), see details}
  \item{strict}{should horizonation by strictly enforced? (TRUE)}
}

\value{a SoilProfileCollection class object}
\author{D. E. Beaudette and J. M. Skovlin}

\details{The value of \code{nullFragsAreZero} will have a significant impact on the rock fragment fractions returned by \code{fetchNASIS}. Set \code{nullFragsAreZero = FALSE} in those cases where there are many data-gaps and NULL rock fragment values should be interpretated as NULLs. Set \code{nullFragsAreZero = TRUE} in those cases where NULL rock fragment values should be interpreted as 0.

This function attempts to do most of the boilerplate work when extracting site/horizon data from a local NASIS database. Pedons that have errors in their horizonation are excluded from the returned object, however, their IDs are printed on the console. See \code{\link{getHzErrorsNASIS}} for a simple approach to identifying pedons with problematic horizonation.}

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

