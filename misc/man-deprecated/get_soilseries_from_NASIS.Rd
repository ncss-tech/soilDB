\name{get_soilseries_from_NASIS}
\alias{get_soilseries_from_NASIS}
\alias{get_soilseries_from_NASISWebReport}

\title{Get records from the Soil Classification (SC) database}
\description{These functions return records from the Soil Classification database, either from the local NASIS database (all series) or via web report (named series only).}

\usage{

get_soilseries_from_NASIS(stringsAsFactors = default.stringsAsFactors())
get_soilseries_from_NASISWebReport(soils, 
stringsAsFactors = default.stringsAsFactors())
}

\arguments{
  \item{soils}{character vector of soil series names}
  \item{stringsAsFactors}{logical: should character vectors be converted to factors? This argument is passed to the uncode() function. It does not convert those vectors that have set outside of uncode() (i.e. hard coded). The 'factory-fresh' default is TRUE, but this can be changed by setting options(stringsAsFactors = FALSE)}
  
}


\value{A \code{data.frame}.}

\author{Stephen Roecker}

\keyword{manip}
