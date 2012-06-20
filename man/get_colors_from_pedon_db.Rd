\name{get_colors_from_pedon_db}
\alias{get_colors_from_pedon_db}
\alias{mix_and_clean_colors}

\title{Extract Soil Color Data from a PedonPC Database}
\description{Get, format, mix, and return color data from a PedonPC database.}
\usage{
get_colors_from_pedon_db(dsn)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dsn}{The path to a 'pedon.mdb' database.}
}
\details{This function currently works only on Windows.}
\value{A dataframe with the results.}
\author{Dylan E. Beaudette and Jay M. Skovlin}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{get_hz_data_from_pedon_db}}, \code{\link{get_site_data_from_pedon_db}}
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}
