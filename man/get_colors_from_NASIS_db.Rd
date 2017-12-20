\name{get_colors_from_NASIS_db}
\alias{get_colors_from_NASIS_db}

\title{Extract Soil Color Data from a local NASIS Database}
\description{Get, format, mix, and return color data from a NASIS database.}
\usage{
get_colors_from_NASIS_db(SS = TRUE)
}
\arguments{
  \item{SS}{fetch data from Selected Set in NASIS or from the entire local database (default: TRUE)}	
}
\details{This function currently works only on Windows.}
\value{A dataframe with the results.}
\author{Jay M. Skovlin and Dylan E. Beaudette}



\seealso{
\code{\link{simplifyColorData}}, \code{\link{get_hz_data_from_NASIS_db}}, \code{\link{get_site_data_from_NASIS_db}}
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}
