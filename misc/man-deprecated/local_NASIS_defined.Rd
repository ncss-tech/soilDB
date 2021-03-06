% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/openNASISchannel.R
\name{local_NASIS_defined}
\alias{local_NASIS_defined}
\title{Check for presence of \code{nasis_local} ODBC data source}
\usage{
local_NASIS_defined(static_path = NULL)
}
\arguments{
\item{static_path}{Optional: path to local SQLite database containing NASIS table structure; default: NULL}
}
\value{
logical
}
\description{
Check for presence of \code{nasis_local} ODBC data source
}
\examples{

if(local_NASIS_defined()) {
  # use fetchNASIS or some other lower-level fetch function
} else {
  message('could not find `nasis_local` ODBC data source')
}
}
