\name{fetchPedonPC}
\alias{fetchPedonPC}
\alias{getHzErrorsPedonPC}
\title{Fetch commonly used site/horizon data from a PedonPC database.}
\description{Fetch commonly used site/horizon data from a PedonPC database, return as a SoilProfileCollection object.}
\usage{fetchPedonPC(dsn)}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dsn}{The path to a 'pedon.mdb' database.}
}
\details{This function currently works only on Windows.}
\value{a SoilProfileCollection class object}
\references{}
\author{Dylan E. Beaudette and Jay M. Skovlin}
\note{This fuction attempts to do most of the boilerplate work when extracting site/horizon data from a PedonPC database. Pedons that have errors in their horizonation are excluded from the returned object, however, their IDs are printed on the console. See \code{getHzErrorsPedonPC} for a simple approach to identifying pedons with problematic horizonation.}

\seealso{\code{\link{get_hz_data_from_pedon_db}}}
\examples{}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}

