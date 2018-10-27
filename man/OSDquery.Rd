\name{OSDquery}
\alias{OSDquery}

\title{Full text searching of the USDA-NRCS Official Series Descriptions}

\description{This is a rough example of how chunks of text parsed from OSD records can be made search-able with the \href{PostgreSQL fulltext indexing}{https://www.postgresql.org/docs/9.5/static/textsearch.html} and query system (\href{syntax details}{https://www.postgresql.org/docs/9.5/static/datatype-textsearch.html}). Each search field (except for the "brief narrative" and MLRA) corresponds with a section header in an OSD. The results may not include every OSD due to formatting errors and typos. Results are scored based on the number of times search terms match words in associated sections. This is a programatic interface to \href{this webpage}{http://soilmap2-1.lawr.ucdavis.edu/osd-search/index.php}.}

\usage{
  OSDquery(mlra='', taxonomic_class='', typical_pedon='', 
  brief_narrative='', ric='', use_and_veg='', 
  competing_series='', geog_location='', geog_assoc_soils='')
}

\arguments{
  
  \item{mlra}{a comma-delimeted list of MLRA to search}
  \item{taxonomic_class}{search family level classification}
  \item{typical_pedon}{search typical pedon section}
  \item{brief_narrative}{search brief narrative}
  \item{ric}{search range in characteristics section}
  \item{use_and_veg}{search use and vegetation section}
  \item{competing_series}{search competing section}
  \item{geog_location}{search geographic setting section}
  \item{geog_assoc_soils}{search geographicaly associated soils section}
  
}

\details{
  See \href{this webpage}{http://soilmap2-1.lawr.ucdavis.edu/osd-search/index.php} for more information.
  
  \describe{
    \item{}{family level taxa are derived from SC database, not parsed OSD records}
    \item{}{MLRA are derived via spatial intersection (SSURGO x MLRA polygons)}
    \item{}{MLRA-filtering is only possible for series used in the current SSURGO snapshot (component name)}
    \item{}{logical AND: &}
    \item{}{logical OR: |}
    \item{}{wildcard, e.g. rhy-something rhy:*}
    \item{}{search terms with spaces need doubled single quotes: ''san joaquin''}
    \item{}{combine search terms into a single expression: (grano:* | granite)}
  } 
  
}

\value{a \code{data.frame} object containing soil series names that match patterns supplied as arguments.}

\references{
  
  \url{http://www.nrcs.usda.gov/wps/portal/nrcs/detailfull/soils/home/?cid=nrcs142p2_053587}
}

\author{D.E. Beaudette}

\note{SoilWeb maintains a snapshot of the Official Series Description data.}

\seealso{
\code{\link{fetchOSD}}
}

\examples{
# find all series that list Pardee as a geographically associated soil.
s <- OSDquery(geog_assoc_soils = 'pardee')
# get data for these series
x <- fetchOSD(s$series, extended = TRUE, colorState = 'dry')
# simple figure
par(mar=c(0,0,1,1))
plot(x$SPC)
}

\keyword{manip}
