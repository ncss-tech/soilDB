\name{uncode}
\alias{uncode}
\alias{code}
\alias{nasis_metadata}
\title{Convert coded values returned from NASIS and SDA queries to factors}
\description{These functions convert the coded values returned from NASIS or SDA to factors (e.g. 1 = Alfisols) using the metadata tables from NASIS. For SDA the metadata is pulled from a static snapshot in the soilDB package (i.e. data(nasis_metadata)).}
\usage{
uncode(data.frame, invert = FALSE, NASIS = TRUE)
code(data.frame, invert = TRUE, NASIS = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{invert}{converts the code labels back to their coded values()}
  \item{NASIS}{a logical argument indicating whether or not to query the metadata from NASIS or use the static snapshot data(nasis_metadata) (default: TRUE)}
}
\details{These functions convert the coded values returned from NASIS or SDA to factors using the metadata tables from NASIS. For SDA the metadata is pulled from a static snapshot in the soilDB package (i.e. data(nasis_metadata)).}
\value{A dataframe with the results.}
\author{Stephen Roecker}
\examples{
\dontrun{
# query component by nationalmusym
comp = fetchSDA_components(nationalmusym = "2vzcp")
s = site(comp$spc)
s = uncode(s, NASIS = FALSE)
levels(s$taxorder)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{~metadata}% use one of  RShowDoc("KEYWORDS")
