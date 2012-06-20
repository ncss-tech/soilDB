\name{get_hz_data_from_NASIS_db}
\alias{get_hz_data_from_NASIS_db}

\title{Extract Horizon Data from a local NASIS Database}
\description{Get horizon-level data from a local NASIS database.}
\usage{
get_hz_data_from_NASIS_db()
}
\details{This function currently works only on Windows.}
\value{A dataframe.}

\author{Jay M. Skovlin and Dylan E. Beaudette}
\note{NULL total rock fragment values are assumed to represent an _absense_ of rock fragments, and set to 0.}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{get_hz_data_from_NASIS_db}}, \code{\link{get_site_data_from_NASIS_db}}
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}
