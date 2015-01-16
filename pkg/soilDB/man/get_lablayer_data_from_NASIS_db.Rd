\name{get_lablayer_data_from_NASIS_db}
\alias{get_lablayer_data_from_NASIS_db}

\title{Extract lab pedon layer data from a local NASIS Database}
\description{Get lab pedon layer-level(horizon-level) data from a local NASIS database.}
\usage{get_lablayer_data_from_NASIS_db()}

\value{A dataframe.}
\author{Jay M. Skovlin and Dylan E. Beaudette}
\details{This function currently works only on Windows, and requires a 'nasis_local' ODBC connection.}
\note{This fuction queries KSSL laboratory site/horizon data from a local NASIS database from the lab layer data table.}

\seealso{
\code{\link{get_labpedon_data_from_NASIS_db}} 
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.

\keyword{manip}

