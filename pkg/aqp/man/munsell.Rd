\name{munsell}
\Rdversion{1.1}
\alias{munsell}
\docType{data}
\title{Munsell-RGB Lookup Table for Common Soil Colors}
\description{A lookup table of interpolated Munsell color chips for common soil colors.}
\usage{data(munsell)}
\format{
  A data frame with 8825 observations on the following 6 variables.
  \describe{
    \item{\code{hue}}{Munsell Hue, upper case}
    \item{\code{value}}{Munsell Value}
    \item{\code{chroma}}{Munsell Chroma}
    \item{\code{r}}{red value (0-1)}
    \item{\code{g}}{green value (0-1)}
    \item{\code{b}}{blue value (0-1)}
  }
}
\details{
See \code{munsell2rgb} for conversion examples. Note that this table does not currently have entires for values of 2.5-- common in most soil color books. These chips should be added in the next major release of aqp.
}
\source{
Color chip XYZ values: http://www.cis.rit.edu/mcsl/online/munsell.php
}
\references{
	\describe{
		\item{http://www.brucelindbloom.com/index.html?ColorCalcHelp.html}{Color conversion equations}
		\item{http://casoilresource.lawr.ucdavis.edu/drupal/node/201}{Methods used to generate this table}
	}
}
\examples{
data(munsell)
}
\keyword{datasets}
