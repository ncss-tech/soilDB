\name{get_extended_data_from_NASIS_db}
\alias{get_extended_data_from_NASIS_db}

\title{Extract accessory tables and summaries from a local NASIS Database}
\description{Extract accessory tables and summaries from a local NASIS Database.}
\usage{
get_extended_data_from_NASIS_db(SS = TRUE, nullFragsAreZero = TRUE,
                                stringsAsFactors = default.stringsAsFactors()
                                )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{SS}{get data from the currently loaded Selected Set in NASIS or from the entire local database (default: TRUE)}
  \item{nullFragsAreZero}{should fragment volumes of NULL be interpreted as 0? (default: TRUE), see details}
  \item{stringsAsFactors}{logical: should character vectors be converted to factors? This argument is passed to the uncode() function. It does not convert those vectors that have been set outside of uncode() (i.e. hard coded). The 'factory-fresh' default is TRUE, but this can be changed by setting options(stringsAsFactors = FALSE)}
}
\details{This function currently works only on Windows.}
\value{A list with the results.}
\author{Jay M. Skovlin and Dylan E. Beaudette}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{get_hz_data_from_NASIS_db}}, \code{\link{get_site_data_from_NASIS_db}}
}

\examples{
\donttest{

if(local_NASIS_defined()) {
 # query extended data
 e <- try(get_extended_data_from_NASIS_db())

 # show contents of extended data
 str(e)
}

}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}
