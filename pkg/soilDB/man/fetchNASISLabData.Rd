\name{fetchNASISLabData}
\alias{fetchNASISLabData}


\title{Fetch lab data used site/horizon data from a PedonPC database.}
\description{Fetch KSSL laboratory pedon/horizon layer data from a local NASIS database, return as a SoilProfileCollection object.}

\usage{fetchNASISLabData()}


\details{This function currently works only on Windows, and requires a 'nasis_local' ODBC connection.}
\value{a SoilProfileCollection class object}
\author{J.M. Skovlin and D.E. Beaudette}
\note{This fuction attempts to do most of the boilerplate work when extracting KSSL laboratory site/horizon data from a local NASIS database. Lab pedons that have errors in their horizonation are excluded from the returned object, however, their IDs are printed on the console. See \code{\link{getHzErrorsNASIS}} for a simple approach to identifying pedons with problematic horizonation.}

\seealso{\code{\link{get_labpedon_data_from_NASIS_db}}}
\examples{
\dontrun{
# query depends on some lab data, queried against the national database
# note that you must setup this connection ahead of time
# see inst/doc/setup_ODBC_local_NASIS.pdf
f <- fetchNASISLabData()

# plot only those profiles with densic contact
#plot(f[which(f$densic.contact), ], name='hzname')

}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}

