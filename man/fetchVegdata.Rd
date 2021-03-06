% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fetchVegdata.R
\name{fetchVegdata}
\alias{fetchVegdata}
\alias{get_vegplot_from_NASIS_db}
\alias{get_vegplot_location_from_NASIS_db}
\alias{get_vegplot_species_from_NASIS_db}
\alias{get_vegplot_textnote_from_NASIS_db}
\alias{get_vegplot_transect_from_NASIS_db}
\alias{get_vegplot_transpecies_from_NASIS_db}
\alias{get_vegplot_tree_si_details_from_NASIS_db}
\alias{get_vegplot_tree_si_summary_from_NASIS_db}
\alias{get_vegplot_trhi_from_NASIS_db}
\alias{get_legend_from_NASIS}
\alias{get_lmuaoverlap_from_NASIS}
\title{Load most common vegplot data from local NASIS database}
\usage{
fetchVegdata(
  SS = TRUE,
  stringsAsFactors = default.stringsAsFactors(),
  dsn = NULL
)
}
\arguments{
\item{SS}{fetch data from the currently loaded selected set in NASIS or from the entire local database (default: \code{TRUE})}

\item{stringsAsFactors}{logical: should character vectors be converted to
factors? This argument is passed to the \code{uncode()} function. It does not
convert those vectors that have been set outside of \code{uncode()} (i.e. hard
coded).}

\item{dsn}{Optional: path to local SQLite database containing NASIS
table structure; default: \code{NULL}}
}
\value{
A named list containing: "vegplot", "vegplotlocation", "vegplotrhi", "vegplotspecies", "vegtransect", "vegtransplantsum", 'vegsiteindexsum', "vegsiteindexdet", and  "vegplottext" tables
}
\description{
Load most common vegplot data from local NASIS database
}
