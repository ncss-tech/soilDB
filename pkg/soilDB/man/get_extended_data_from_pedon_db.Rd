\name{get_extended_data_from_pedon_db}
\alias{get_extended_data_from_pedon_db}
\alias{diagHzLongtoWide}

\title{Extract accessory tables and summaries from a local pedonPC Database}
\description{Extract accessory tables and summaries from a local pedonPC Database.}
\usage{
get_extended_data_from_pedon_db(dsn)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dsn}{The path to a 'pedon.mdb' database.}
}
\details{This function currently works only on Windows.}
\value{A list with the results.}
\references{}
\author{Jay M. Skovlin and Dylan E. Beaudette}
\note{}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{get_hz_data_from_pedon_db}}, \code{\link{get_site_data_from_pedon_db}}
}
\examples{
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}
