\name{fetchPedonPC}
\alias{fetchPedonPC}
\alias{fetchNASIS}
\alias{getHzErrorsPedonPC}
\title{Fetch commonly used site/horizon data from a PedonPC database.}
\description{Fetch commonly used site/horizon data from a PedonPC or local NASIS database, return as a SoilProfileCollection object.}
\usage{fetchPedonPC(dsn)}
\arguments{
  \item{dsn}{The path to a 'pedon.mdb' or local database.}
}
\details{This function currently works only on Windows.}
\value{a SoilProfileCollection class object}
\references{}
\author{Dylan E. Beaudette and Jay M. Skovlin}
\note{This fuction attempts to do most of the boilerplate work when extracting site/horizon data from a PedonPC or local NASIS database. Pedons that have errors in their horizonation are excluded from the returned object, however, their IDs are printed on the console. See \code{getHzErrorsPedonPC} for a simple approach to identifying pedons with problematic horizonation.}

\seealso{\code{\link{get_hz_data_from_pedon_db}}}
\examples{
\dontrun{
# path to local PedonPC back-end DB
dsn <- "S:/Service_Center/NRCS/pedon/pedon.mdb"

# get routinely used soil data SoilProfileCollection object
f <- fetchPedonPC(dsn)

# alternatively, get these data from a local NASIS database
# query depends on some pedon data, queried against the national database
# note that you must setup this connection ahead of time
# see inst/doc/setup_ODBC_local_NASIS.pdf
f <- fetchNASIS()

# plot only those profiles with densic contact
plot(f[which(f$densic.contact), ], name='hzname')
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}

