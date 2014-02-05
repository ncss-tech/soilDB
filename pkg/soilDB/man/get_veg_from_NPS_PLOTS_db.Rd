\name{get_veg_from_NPS_PLOTS_db}
\alias{get_veg_from_NPS_PLOTS_db}

\title{Retrieve Vegetation Data from an NPS PLOTS Database}

\description{Used to extract species, stratum, and cover vegetation data from a backend NPS PLOTS Database.  Currently works for any Microsoft Access database with an .mdb file format.}

\usage{get_veg_from_NPS_PLOTS_db(dsn)}

\arguments{
  \item{dsn}{file path to the NPS PLOTS access database on your system.}
}

\value{A dataframe with vegetation data in a long format with linkage to NRCS soil pedon data via the site_id key field.}

\author{Jay M. Skovlin}

\note{This function currently only works on Windows.}

\keyword{manip}
