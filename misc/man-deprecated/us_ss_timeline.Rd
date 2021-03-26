\name{us_ss_timeline}
\alias{us_ss_timeline}
\docType{data}

\title{
Timeline of US Published Soil Surveys
}

\description{
This dataset contains the years of each US Soil Survey was published.
}

\usage{data("us_ss_timeline")}

\format{
  A data frame with 5209 observations on the following 5 variables.
  \describe{
    \item{\code{ssa}}{Soil Survey name, a character vector}
    \item{\code{year}}{year of publication, a numeric vector}
    \item{\code{pdf}}{does a pdf exists, a logical vector}
    \item{\code{state}}{State abbreviation, a character vector}
  }
}

\details{
This data was web scraped from the NRCS Soils Website. The scraping procedure and a example plot are included in the examples section below.
}

\source{
https://www.nrcs.usda.gov/wps/portal/nrcs/soilsurvey/soils/survey/state/
}

\keyword{datasets}
