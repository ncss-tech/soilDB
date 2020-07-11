\name{get_veg_data_from_NASIS_db}
\alias{get_veg_data_from_NASIS_db}

\title{Extract veg data from a local NASIS Database}
\description{Extract veg data from a local NASIS Database.}
\usage{
get_veg_data_from_NASIS_db(SS = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{SS}{get data from the currently loaded Selected Set in NASIS or from the entire local database (default: TRUE)}
}
\details{This function currently works only on Windows.}
\value{A list with the results.}
\author{Jay M. Skovlin and Dylan E. Beaudette}


\examples{
\donttest{
if(local_NASIS_defined()) {
 # query text note data
 v <- try(get_veg_from_NASIS_db())

 # show contents veg data returned
 str(v)
}
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}
