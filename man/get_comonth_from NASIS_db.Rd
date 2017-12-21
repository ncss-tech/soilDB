\name{get_comonth_from_NASIS_db}
\alias{get_comonth_from_NASIS_db}

\title{Extract component month data from a local NASIS Database}
\description{Extract component month data from a local NASIS Database.}
\usage{
get_comonth_from_NASIS_db(SS = TRUE, fill = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{SS}{get data from the currently loaded Selected Set in NASIS or from the entire local database (default: TRUE)}
  \item{fill}{should missing "month" rows in the comonth table be filled with NA (FALSE)}
}	
}
\details{This function currently works only on Windows.}
\value{A list with the results.}
\author{Stephen Roecker}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{fetchNASIS()}}, \code{\link{fetchNASIS_components()}}
}

\examples{
\dontrun{
# query text note data
cm <- get_comonth_from_NASIS_db()

# show structure of component month data
str(cm)

}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}
