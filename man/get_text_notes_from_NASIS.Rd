\name{get_text_notes_from_NASIS_db}
\alias{get_text_notes_from_NASIS_db}

\title{Extract accessory tables and summaries from a local NASIS Database}
\description{Extract accessory tables and summaries from a local NASIS Database.}
\usage{
get_text_notes_from_NASIS_db()
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{SS}{get data from the currently loaded Selected Set in NASIS or from the entire local database (default: TRUE)}	
}
\details{This function currently works only on Windows.}
\value{A list with the results.}
\author{Dylan E. Beaudette and Jay M. Skovlin}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{get_hz_data_from_pedon_db}}, \code{\link{get_site_data_from_pedon_db}}
}

\examples{
\dontrun{
# query text note data
t <- get_text_notes_from_NASIS_db()

# show contents of various text note data
str(t)

# get URL for photos linked to sites


}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}
