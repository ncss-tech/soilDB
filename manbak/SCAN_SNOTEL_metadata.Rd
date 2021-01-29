\name{SCAN_SNOTEL_metadata}
\alias{SCAN_SNOTEL_metadata}
\alias{state_FIPS_codes}

\docType{data}

\title{SCAN and SNOTEL Station Metadata}

\description{SCAN and SNOTEL station metadata, a work in progress.}

\usage{data("SCAN_SNOTEL_metadata")}

\format{
  A data frame with 1092 observations on the following 12 variables.
  \describe{
    \item{\code{Name}}{station name}
    \item{\code{Site}}{station ID}
    \item{\code{State}}{state}
    \item{\code{Network}}{sensor network: SCAN / SNOTEL}
    \item{\code{County}}{county}
    \item{\code{Elevation_ft}}{station elevation in feet}
    \item{\code{Latitude}}{latitude of station}
    \item{\code{Longitude}}{longitude of station}
    \item{\code{HUC}}{associated watershed}
    \item{\code{climstanm}}{climate station name (TODO: remove this column)}
    \item{\code{upedonid}}{associated user pedon ID}
    \item{\code{pedlabsampnum}}{associated lab sample ID}
  }
}

\details{These data have been compiled from several sources and represent a progressive effort to organize SCAN/SNOTEL station metadata. Therefore, some records may be missing or incorrect. Details on this effort can be found at the associated GitHub issue page: \url{https://github.com/ncss-tech/soilDB/issues/61}.}


\keyword{datasets}
