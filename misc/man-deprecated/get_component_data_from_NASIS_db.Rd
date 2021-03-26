\name{get_component_data_from_NASIS_db}
\alias{get_component_data_from_NASIS_db}
\alias{get_component_restrictions_from_NASIS_db}

\title{Extract component data from a local NASIS Database}
\description{Extract component data from a local NASIS Database.}

\usage{
get_component_data_from_NASIS_db(SS = TRUE, stringsAsFactors = default.stringsAsFactors())
get_component_restrictions_from_NASIS_db(SS = TRUE)
}


\arguments{
  \item{SS}{get data from the currently loaded Selected Set in NASIS or from the entire local database (default: TRUE)}
  \item{stringsAsFactors}{logical: should character vectors be converted to factors? This argument is passed to the uncode() function. It does not convert those vectors that have set outside of uncode() (i.e. hard coded). The 'factory-fresh' default is TRUE, but this can be changed by setting options(stringsAsFactors = FALSE)}
}

\details{This function currently works only on Windows.}
\value{A list with the results.}
\author{Dylan E. Beaudette, Stephen Roecker, and Jay M. Skovlin}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{fetchNASIS}}
}

\examples{
\donttest{
if(local_NASIS_defined()) {
 # query text note data
 fc <- try(get_component_data_from_NASIS_db())

 # show structure of component data returned
 str(fc)
}
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}
