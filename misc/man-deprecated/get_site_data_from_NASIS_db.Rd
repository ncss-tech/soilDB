\name{get_site_data_from_NASIS_db}
\alias{get_site_data_from_NASIS_db}

\title{Extract Site Data from a local NASIS Database}
\description{Get site-level data from a local NASIS database.}
\usage{get_site_data_from_NASIS_db(SS = TRUE, stringsAsFactors = default.stringsAsFactors())}
\arguments{
  \item{SS}{fetch data from Selected Set in NASIS or from the entire local database (default: TRUE)}
  \item{stringsAsFactors}{logical: should character vectors be converted to factors? This argument is passed to the uncode() function. It does not convert those vectors that have been set outside of uncode() (i.e. hard coded). The 'factory-fresh' default is TRUE, but this can be changed by setting options(stringsAsFactors = FALSE)}
}
\value{A data.frame.}
\author{Jay M. Skovlin and Dylan E. Beaudette}
\details{When multiple "site bedrock" entries are present, only the shallowest is returned by this function.}
\note{This function currently works only on Windows.}

\seealso{
\code{\link{get_hz_data_from_NASIS_db}}, 
}



\keyword{manip}

