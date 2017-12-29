\name{get_hz_data_from_NASIS_db}
\alias{get_hz_data_from_NASIS_db}

\title{Extract Horizon Data from a local NASIS Database}
\description{Get horizon-level data from a local NASIS database.}
\usage{
get_hz_data_from_NASIS_db(SS = TRUE, stringsAsFactors = default.stringsAsFactors())
}
\arguments{
  \item{SS}{fetch data from Selected Set in NASIS or from the entire local database (default: TRUE)}
  \item{stringsAsFactors}{logical: should character vectors be converted to factors? This argument is passed to the uncode() function. It does not convert those vectors that have been set outside of uncode() (i.e. hard coded). The 'factory-fresh' default is TRUE, but this can be changed by setting options(stringsAsFactors = FALSE)}
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
