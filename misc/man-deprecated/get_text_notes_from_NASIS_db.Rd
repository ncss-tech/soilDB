\name{get_text_notes_from_NASIS_db}
\alias{get_text_notes_from_NASIS_db}

\title{Extract text note data from a local NASIS Database}
\description{Extract text note data from a local NASIS Database.}
\usage{
get_text_notes_from_NASIS_db(SS = TRUE, fixLineEndings = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{SS}{get data from the currently loaded Selected Set in NASIS or from the entire local database (default: TRUE)}
  \item{fixLineEndings}{convert line endings from "\\r\\n" to "\\n"}
}
\details{This function currently works only on Windows.}
\value{A list with the results.}
\author{Dylan E. Beaudette and Jay M. Skovlin}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{get_hz_data_from_pedon_db}}, \code{\link{get_site_data_from_pedon_db}}
}

\examples{
\donttest{
if(local_NASIS_defined()) {
 # query text note data
 t <- try(get_text_notes_from_NASIS_db())

 # show contents text note data, includes: siteobs, site, pedon, horizon level text notes data.
 str(t)

 # view text categories for site text notes
 if(!inherits(t, 'try-error')) {
  table(t$site_text$textcat)
 }
}
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}
