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
fetchNASIS(rmHzErrors = TRUE)
fetchNASIS_component_data()
getHzErrorsNASIS(strict=TRUE)
}

\arguments{
  \item{rmHzErrors}{should pedons with horizonation errors be removed from the results? (TRUE)}
  \item{strict}{should horizonation by strictly enforced? (TRUE)}
}

\details{This function currently works only on Windows, and requires a 'nasis_local' ODBC connection.}
\value{a SoilProfileCollection class object}
\author{D. E. Beaudette and J. M. Skovlin}
\note{This fuction attempts to do most of the boilerplate work when extracting site/horizon data from a local NASIS database. Pedons that have errors in their horizonation are excluded from the returned object, however, their IDs are printed on the console. See \code{\link{getHzErrorsNASIS}} for a simple approach to identifying pedons with problematic horizonation.}

\seealso{\code{\link{get_hz_data_from_pedon_db}}}
\examples{
\dontrun{
# query depends on some pedon data, queried against the national database
# note that you must setup this connection ahead of time
# see inst/doc/setup_ODBC_local_NASIS.pdf
f <- fetchNASIS()

# plot only those profiles with densic contact
plot(f[which(f$densic.contact), ], name='hzname')

# get basic component data from local NASIS, after performing a 
# DMU-* query against the national database
fc <- fetchNASIS_component_data()
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}

