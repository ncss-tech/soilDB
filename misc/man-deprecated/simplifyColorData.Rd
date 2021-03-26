\name{simplifyColorData}
\alias{simplifyColorData}
\alias{mix_and_clean_colors}

\title{Simplify Color Data by ID}
\description{Simplify multiple Munsell color observations associated with each horizon.}
\usage{
simplifyColorData(d, id.var = "phiid", wt = "colorpct", bt = FALSE)
mix_and_clean_colors(x, wt='pct', backTransform = FALSE)
}

\arguments{
  \item{d}{a \code{data.frame} object, typically returned from NASIS, see details}
  \item{id.var}{character vector with the name of the column containing an ID that is unique among all horizons in \code{d}}
  \item{x}{a \code{data.frame} object containing sRGB coordinates associated with a group of colors to mix}
  \item{wt}{a character vector with the name of the column containing color weights for mixing}
  \item{bt}{logical, should the mixed sRGB representation of soil color be transformed to closest Munsell chips? This is performed by \code{aqp::rgb2Munsell}}  
  \item{backTransform}{logical, should the mixed sRGB representation of soil color be transformed to closest Munsell chips? This is performed by \code{aqp::rgb2Munsell}}
}

\details{
This function is mainly intended for the processing of NASIS pedon/horizon data which may or may not contain multiple colors per horizon/moisture status combination. \code{simplifyColorData} will "mix" multiple colors associated with horizons in \code{d}, according to IDs specified by \code{id.var}, using "weights" (area percentages) specified by the \code{wt} argument to \code{mix_and_clean_colors}.

Note that this function doesn't actually simulate the mixture of pigments on a surface, rather, "mixing" is approximated via weighted average in the CIELAB colorspace.

The \code{simplifyColorData} function can be applied to data sources other than NASIS by careful use of the \code{id.var} and \code{wt} arguments. However, \code{d} must contain Munsell colors split into columns named "colorhue", "colorvalue", and "colorchroma". In addition, the moisture state ("Dry" or "Moist") must be specified in a column named "colormoistst".

The \code{mix_and_clean_colors} function can be applied to arbitrary data sources as long as \code{x} contains sRGB coordinates in columns named "r", "g", and "b". This function should be applied to chunks of rows within which color mixtures make sense.

There are examples in \href{http://ncss-tech.github.io/AQP/soilDB/KSSL-demo.html}{the KSSL data tutorial} and \href{http://ncss-tech.github.io/AQP/soilDB/mixing-soil-color-data.html}{the soil color mixing tutorial}.
}


\author{D.E. Beaudette}


\keyword{manip}

