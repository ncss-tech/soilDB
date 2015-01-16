\name{SoilProfileCollection-class}
\Rdversion{1.1}
\docType{class}
\alias{SoilProfileCollection-class}
\alias{SoilProfileCollection}
\alias{$,SoilProfileCollection-method}
\alias{$<-,SoilProfileCollection-method}
\alias{[,SoilProfileCollection-method}
\alias{proj4string,SoilProfileCollection-method}
\alias{proj4string<-,SoilProfileCollection,ANY-method}
\alias{coordinates,SoilProfileCollection-method}
\alias{coordinates<-,SoilProfileCollection-method}
\alias{names,SoilProfileCollection-method}
\alias{length,SoilProfileCollection-method}
\alias{max,SoilProfileCollection-method}
\alias{min,SoilProfileCollection-method}
\alias{show,SoilProfileCollection-method}
\alias{nrow,SoilProfileCollection-method}
\alias{unique,SoilProfileCollection-method}
\alias{unique,SoilProfileCollection-method}
\alias{rbind.SoilProfileCollection}

\title{SoilProfileCollection Class}
\description{Basic class for storing soil profile collections, associated site data, and metadata.}

\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("SoilProfileCollection", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{idcol}:}{Object of class \code{"character"} the name of the column used to uniquely identify profiles }
    \item{\code{depthcols}:}{Object of class \code{"character"} with the names of columns containing the horizon top and bottom boundaries}
    \item{\code{metadata}:}{Object of class \code{"data.frame"} with collection-level metadata, having a single row, and user-defined columns}
    \item{\code{horizons}:}{Object of class \code{"data.frame"} with 1 or more rows per profile}
    \item{\code{site}:}{Object of class \code{"data.frame"} with 1 row per profile }
    \item{\code{sp}:}{Object of class \code{"SpatialPoints"} with 1 row per profile }
    \item{\code{diagnostic}:}{Object of class \code{"data.frame"} with 0 or more rows per profile}
  }
}
\section{Methods}{
  \describe{
    \item{$}{\code{signature(x = "SoilProfileCollection")}: ... }
    \item{$<-}{\code{signature(x = "SoilProfileCollection")}: ... }
    \item{[}{\code{signature(x = "SoilProfileCollection", i = "ANY", j = "missing")}: ... }
    \item{[[}{\code{signature(x = "SoilProfileCollection", i = "ANY", j = "missing")}: ... }
    \item{[[<-}{\code{signature(x = "SoilProfileCollection", i = "ANY", j = "missing")}: ... }
    \item{coordinates<-}{\code{signature(object = "SoilProfileCollection")}: ... }
    \item{horizonDepths}{\code{signature(object = "SoilProfileCollection")}: ... }
    \item{horizons}{\code{signature(object = "SoilProfileCollection")}: ... }
    \item{horizons<-}{\code{signature(object = "SoilProfileCollection")}: ... }
    \item{idname}{\code{signature(object = "SoilProfileCollection")}: ... }
    \item{names}{\code{signature(x = "SoilProfileCollection")}: ... }
    \item{length}{\code{signature(x = "SoilProfileCollection")}: ... }
    \item{max}{\code{signature(x = "SoilProfileCollection")}: ... }
    \item{metadata}{\code{signature(object = "SoilProfileCollection")}: ... }
    \item{metadata<-}{\code{signature(object = "SoilProfileCollection")}: ... }
    \item{min}{\code{signature(x = "SoilProfileCollection")}: ... }
    \item{profile_id}{\code{signature(object = "SoilProfileCollection")}: ... }
    \item{profile_plot}{\code{signature(object = "SoilProfileCollection")}: ... }
    \item{show}{\code{signature(object = "SoilProfileCollection")}: ... }
    \item{site}{\code{signature(object = "SoilProfileCollection")}: ... }
    \item{site<-}{\code{signature(object = "SoilProfileCollection")}: ... }
    \item{slab}{\code{signature(data = "SoilProfileCollection")}: ... }
    \item{units}{\code{signature(object = "SoilProfileCollection")}: ... }
    \item{units<-}{\code{signature(object = "SoilProfileCollection")}: ... }
    \item{unique}{\code{signature(object = "SoilProfileCollection"), vars = "character"}: ... }
	 }
}

\author{Pierre Roudier and Dylan E. Beaudette}

\examples{
## use the digest library to detect duplicate data
data(sp1)

# make a copy, make new IDs, and stack
s.1 <- sp1 
s.2 <- sp1
s.2$id <- paste(s.2$id, '-copy', sep='')
s <- rbind(s.1, s.2)
depths(s) <- id ~ top + bottom

# digests are computed from horizon-level data only
# horizon boundaries and 'prop'
# result is an index of unqique profiles
if(require(digest)) {
	u <- unique(s, vars=c('top', 'bottom', 'prop'))
}

# compare with and without dupes:
# note subsetting of SoilProfileCollection
cbind(dupes=length(s), no.dupes=length(s[u, ]))

## 3. concatenate SoilProfileCollection objects
require(plyr)
d <- ldply(1:10, random_profile)

# promote to SoilProfileCollection and plot
depths(d) <- id ~ top + bottom
plot(d)

# split into new SoilProfileCollection objects by index
d.1 <- d[1, ]
d.2 <- d[2, ]
d.345 <- d[3:5, ]

# recombine, note that profiles are sorted according to ID
d.new <- rbind(d.345, d.1, d.2)
plot(d.new)


\dontrun{
# these next examples should throw an error
# insert a missing horizon boundary
data(sp1)
sp1$top[1] <- NA
depths(sp1) <- id ~ top + bottom
}
}
\keyword{classes}
