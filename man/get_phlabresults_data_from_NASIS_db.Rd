\name{get_phlabresults_data_from_NASIS_db}
\alias{get_phlabresults_data_from_NASIS_db}

\title{Extract phlabresults table from a local NASIS Database}
\description{Get, format, aggregate, and return phlabresults table from a NASIS database.}
\usage{
get_phlabresults_data_from_NASIS_db(SS=TRUE)
}

\arguments{
  \item{SS}{fetch data from the currently loaded selected set in NASIS or from the entire local database (default: TRUE)}	
}

\details{This function currently works only on Windows.}
\value{A dataframe with the results.}
\author{Stephen Roecker}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{get_hz_data_from_NASIS_db}}, \code{\link{get_site_data_from_NASIS_db}}
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}
