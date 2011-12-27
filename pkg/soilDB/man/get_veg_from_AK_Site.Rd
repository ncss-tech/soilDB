\name{get_veg_from_AK_Site}
\alias{get_veg_from_AK_Site}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Retrieve Vegetation Data from an AK Site Database}
\description{Retrieve Vegetation Data from an AK Site Database}
\usage{
get_veg_from_AK_Site(dsn)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dsn}{file path the the AK Site access database}
}
\details{}
\value{A dataframe with vegetation data in long format, linked to site ID.}
\references{}
\author{Dylan E. Beaudette}
\note{This function currently works only on Windows.}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{get_hz_data_from_pedon_db}}, \code{\link{get_site_data_from_pedon_db}}
}
\examples{
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}
