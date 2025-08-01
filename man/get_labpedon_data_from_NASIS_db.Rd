% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_labpedon_data_from_NASIS_db.R
\name{get_labpedon_data_from_NASIS_db}
\alias{get_labpedon_data_from_NASIS_db}
\title{Get lab pedon data from a local NASIS Database}
\usage{
get_labpedon_data_from_NASIS_db(SS = TRUE, dsn = NULL)
}
\arguments{
\item{SS}{fetch data from the currently loaded selected set in NASIS or from
the entire local database (default: TRUE)}

\item{dsn}{Optional: path to local SQLite database containing NASIS
table structure; default: \code{NULL}}
}
\value{
A data.frame.
}
\description{
Get lab pedon-level data from a local NASIS database.
}
\note{
This function queries KSSL laboratory site/horizon data from a local
NASIS database from the lab pedon data table.
}
\seealso{
\code{\link{get_lablayer_data_from_NASIS_db}}
}
\author{
Jay M. Skovlin and Dylan E. Beaudette
}
\keyword{manip}
