\name{fetchPedonPC}
\alias{fetchPedonPC}
\alias{getHzErrorsPedonPC}

\title{Fetch commonly used site/horizon data from a PedonPC v.5 database.}
\description{Fetch commonly used site/horizon data from a version 5.x PedonPC database, return as a SoilProfileCollection object.}

\usage{
fetchPedonPC(dsn)
getHzErrorsPedonPC(dsn, strict=TRUE)
}

\arguments{
  \item{dsn}{The path to a PedonPC version 5.x database}
  \item{strict}{should horizonation by strictly enforced? (TRUE)}
}

\details{This function currently works only on Windows.}
\value{a SoilProfileCollection class object}
\author{D. E. Beaudette and J. M. Skovlin}
\note{This fuction attempts to do most of the boilerplate work when extracting site/horizon data from a PedonPC or local NASIS database. Pedons that have errors in their horizonation are excluded from the returned object, however, their IDs are printed on the console. See \code{\link{getHzErrorsPedonPC}} for a simple approach to identifying pedons with problematic horizonation. Records from the 'taxhistory' table are selected based on 1) most recent record, or 2) record with the least amount of missing data.}

\seealso{\code{\link{get_hz_data_from_pedon_db}}}
\examples{
\dontrun{
# path to local PedonPC back-end DB
dsn <- "S:/Service_Center/NRCS/pedon/pedon.accdb"

# get routinely used soil data SoilProfileCollection object
f <- fetchPedonPC(dsn)

# plot only those profiles with densic contact
plot(f[which(f$densic.contact), ], name='hzname')
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}

