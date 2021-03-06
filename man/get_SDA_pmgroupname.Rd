% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SDA_pmgroupname.R
\name{get_SDA_pmgroupname}
\alias{get_SDA_pmgroupname}
\title{Get map unit parent material group information from Soil Data Access}
\usage{
get_SDA_pmgroupname(areasymbols = NULL, mukeys = NULL, simplify = TRUE)
}
\arguments{
\item{areasymbols}{vector of soil survey area symbols}

\item{mukeys}{vector of map unit keys}

\item{simplify}{logical; group into generalized parent material groups? Default \code{TRUE}}
}
\value{
a data.frame
}
\description{
Uses "Dominant Component" aggregation method.
}
\author{
Jason Nemecek, Chad Ferguson, Andrew Brown
}
