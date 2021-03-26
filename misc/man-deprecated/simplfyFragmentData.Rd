\name{simplifyFragmentData}
\alias{simplifyFragmentData}
\alias{simplfyFragmentData}
\alias{simplifyArtifactData}

\title{Simplify Coarse Fraction Data}
\description{Simplify multiple coarse fraction (>2mm) records by horizon.}
\usage{
simplifyFragmentData(rf, id.var, nullFragsAreZero = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{rf}{a \code{data.frame} object, typically returned from NASIS, see details}
  \item{id.var}{character vector with the name of the column containing an ID that is unique among all horizons in \code{rf}}
  \item{nullFragsAreZero}{should fragment volumes of NULL be interpreted as 0? (default: TRUE), see details}
}
\details{
This function is mainly intended for the processing of NASIS pedon/horizon data which contains multiple coarse fragment descriptions per horizon. \code{simplifyFragmentData} will "sieve out" coarse fragments into the USDA classes, split into hard and para- fragments.

The \code{simplifyFragmentData} function can be applied to data sources other than NASIS by careful use of the \code{id.var} argument. However, \code{rf} must contain coarse fragment volumes in the column "fragvol", fragment size (mm) in columns "fragsize_l", "fragsize_r", "fragsize_h", and fragment cementation class in "fraghard".

There are examples in \href{http://ncss-tech.github.io/AQP/soilDB/KSSL-demo.html}{the KSSL data tutorial}.
}


\author{D.E. Beaudette}

\keyword{manip}
