\name{get_hz_data_from_pedon_db}
\alias{get_hz_data_from_pedon_db}

\title{Extract Horizon Data from a PedonPC Database}
\description{Get horizon-level data from a PedonPC database.}
\usage{
get_hz_data_from_pedon_db(dsn)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dsn}{The path to a 'pedon.mdb' database.}
}
\details{This function currently works only on Windows.}
\value{A dataframe.}

\author{Dylan E. Beaudette and Jay M. Skovlin}
\note{NULL total rock fragment values are assumed to represent an _absense_ of rock fragments, and set to 0.}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{get_colors_from_pedon_db}}, \code{\link{get_site_data_from_pedon_db}}
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}
