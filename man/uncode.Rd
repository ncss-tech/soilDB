\name{uncode}
\alias{metadata}
\alias{uncode}
\alias{code}

\title{Convert coded values returned from NASIS and SDA queries to factors}
\description{These functions convert the coded values returned from NASIS or SDA to factors (e.g. 1 = Alfisols) using the metadata tables from NASIS. For SDA the metadata is pulled from a static snapshot in the soilDB package (/data/metadata.rda).}
\usage{
uncode(df, invert = FALSE, db = "NASIS", 
       droplevels = FALSE,
       stringsAsFactors = default.stringsAsFactors()
       )
code(df, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df}{data.frame}
  \item{invert}{converts the code labels back to their coded values (FALSE)}
  \item{db}{label specifying the soil database the data is coming from, which indicates whether or not to query metadata from local NASIS database ("NASIS") or use soilDB-local snapshot ("LIMS" or "SDA")}
  \item{droplevels}{logical: indicating whether to drop unused levels in classifying factors. This is useful when a class has large number of unused classes, which can waste space in tables and figures.}
  \item{stringsAsFactors}{logical: should character vectors be converted to factors? The 'factory-fresh' default is TRUE, but this can be changed by setting options(stringsAsFactors = FALSE)}
  \item{\dots}{arguments passed on to \code{uncode}}
  }
  
\details{These functions convert the coded values returned from NASIS into their plain text representation. It duplicates the functionality of the CODELABEL function found in NASIS. This function is primarily intended to be used internally by other soilDB R functions, in order to minimizes the need to manually convert values. 

The function works by iterating through the column names in a data frame and looking up whether they match any of the ColumnPhysicalNames found in the metadata domain tables. If matches are found then the columns coded values are converted to their corresponding factor levels. Therefore it is not advisable to reuse column names from NASIS unless the contents match the range of values and format found in NASIS. Otherwise uncode() will convert their values to NA.

When data is being imported from NASIS, the metadata tables are sourced directly from NASIS. When data is being imported from SDA or the NASIS Web Reports, the metadata is pulled from a static snapshot in the soilDB package.

Beware the default is to return the values as factors rather than strings. While strings are generally preferable, factors make plotting more convenient. Generally the factor level ordering returned by uncode() follows the naturally ordering of categories that would be expected (e.g. sand, silt, clay).
}

\value{A data frame with the results.}
\author{Stephen Roecker}
\examples{
\donttest{
if(requireNamespace("curl") &
    curl::has_internet() &
    require(aqp)) {
  # query component by nationalmusym
  comp <- fetchSDA(WHERE = "nationalmusym = '2vzcp'")
  s <- site(comp)
  
  # use SDA uncoding domain via db argument
  s <- uncode(s,  db="SDA")
  levels(s$taxorder)
}
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}% use one of  RShowDoc("KEYWORDS")
