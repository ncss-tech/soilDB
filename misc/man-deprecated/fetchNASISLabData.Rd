\name{fetchNASISLabData}
\alias{fetchNASISLabData}


\title{Fetch lab data used site/horizon data from a PedonPC database.}
\description{Fetch KSSL laboratory pedon/horizon layer data from a local NASIS database, return as a SoilProfileCollection object.}

\usage{fetchNASISLabData(SS = TRUE)}
\arguments{
  \item{SS}{fetch data from the currently loaded selected set in NASIS or from the entire local database (default: TRUE)}
}
\value{a SoilProfileCollection class object}
\details{This function currently works only on Windows, and requires a 'nasis_local' ODBC connection.}
\author{J.M. Skovlin and D.E. Beaudette}
\note{This function attempts to do most of the boilerplate work when extracting KSSL laboratory site/horizon data from a local NASIS database. Lab pedons that have errors in their horizonation are excluded from the returned object, however, their IDs are printed on the console. See \code{\link{getHzErrorsNASIS}} for a simple approach to identifying pedons with problematic horizonation.}

\seealso{\code{\link{get_labpedon_data_from_NASIS_db}}}

\keyword{manip}

