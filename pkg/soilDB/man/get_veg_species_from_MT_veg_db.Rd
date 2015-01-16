\name{get_veg_species_from_MT_veg_db}
\alias{get_veg_species_from_MT_veg_db}

\title{Extract species-level Data from a Montana RangeDB database}
\description{Get species-level data from a Montana RangeDB database.}
\usage{
get_veg_species_from_MT_veg_db(dsn) 
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dsn}{The name of the Montana RangeDB front-end database connection (see details).}
}
\details{This function currently works only on Windows.}
\value{A dataframe.}
\author{Jay M. Skovlin}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{get_veg_from_MT_veg_db}}, \code{\link{get_veg_other_from_MT_veg_db}}
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}
