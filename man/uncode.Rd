\name{uncode}
\alias{metadata}
\alias{uncode}
\alias{code}

\title{Convert coded values returned from NASIS and SDA queries to factors}
\description{These functions convert the coded values returned from NASIS or SDA to factors (e.g. 1 = Alfisols) using the metadata tables from NASIS. For SDA the metadata is pulled from a static snapshot in the soilDB package (/data/metadata.rda).}
\usage{
uncode(df, invert = FALSE, db = "NASIS", stringsAsFactors = default.stringsAsFactors())
code(df, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df}{data.frame}
  \item{invert}{converts the code labels back to their coded values (FALSE)}
  \item{db}{label specifying the soil database the data is coming from, which indicates whether or not to query metadata from local NASIS database ("NASIS") or use soilDB-local snapshot ("LIMS" or "SDA")}
  \item{stringsAsFactors}{logical: should character vectors be converted to factors? The 'factory-fresh' default is TRUE, but this can be changed by setting options(stringsAsFactors = FALSE)}
  \item{\dots}{arguments passed on to \code{uncode}}
  }
  
\details{These functions convert the coded values returned from NASIS into their plaintext representation. The converted values from NASIS, or sourced from SDA, are upgraded to specifically-leveled factors using the metadata tables from NASIS. For SDA the metadata is pulled from a static snapshot in the soilDB package.
}

\value{A dataframe with the results.}
\author{Stephen Roecker}
\examples{
\dontrun{
# query component by nationalmusym
comp = fetchSDA_component(WHERE = "nationalmusym = '2vzcp'")
s = site(comp$spc)
s = uncode(s, NASIS = FALSE)
levels(s$taxorder)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}% use one of  RShowDoc("KEYWORDS")
