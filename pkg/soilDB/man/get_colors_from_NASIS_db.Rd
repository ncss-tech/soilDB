\name{get_colors_from_NASIS_db}
\alias{get_colors_from_NASIS_db}

\title{Extract Soil Color Data from a local NASIS Database}
\description{Get, format, mix, and return color data from a NASIS database.}
\usage{
get_colors_from_NASIS_db(dsn)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dsn}{The name of the local NASIS database connection (see details).}
}
\details{This function currently works only on Windows.}
\value{A dataframe with the results.}
\references{}
\author{Jay M. Skovlin and Dylan E. Beaudette}
\note{}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{get_hz_data_from_NASIS_db}}, \code{\link{get_site_data_from_NASIS_db}}
}
\examples{
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}
