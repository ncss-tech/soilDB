\name{diagnosticPropertyPlot}
\alias{diagnosticPropertyPlot}
\alias{diagnosticPropertyPlot2}

\title{Diagnostic Property Plot}
\description{Generate a graphical description of the presence/absence of soil diagnostic properties.}
\usage{
diagnosticPropertyPlot(f, v, k, id = "pedon_id")
diagnosticPropertyPlot2(f, v, k, id = "pedon_id")
}

\arguments{
  \item{f}{a \code{SoilProfileCollection} object}
  \item{v}{a character vector of site-level attributes that are boolean (e.g. TRUE/FALSE) data}
  \item{k}{an integer, number of groups to highlight}
  \item{id}{the name of the site-level attribute containing an ID to include next next to leave of the diagnostic property dendrogram}
}

\details{This function attempts to display several pieces of information within a single figure. First, soil profiles are sorted according to the presence/absence of diagnostic features named in \code{v}. Second, these diagnostic features are sorted according to their distribution among soil profiles. Third, a binary grid is established with row-ordering of profiles based on step 1 and column-ordering based on step 2. Blue cell represent the presence of a diagnostic feature. Soils with similar diagnostic features should 'clump' together. See examples below.}

\value{a list is silently returned by this function, containing:
\describe{
  \item{\code{rd}}{a \code{data.frame} containing IDs and grouping code}
  \item{\code{profile.order}}{a vector containing the order of soil profiles (row-order in figure), according to diagnostic property values}
  \item{\code{var.order}}{a vector containing the order of variables (column-order in figure), according to their distribution among profiles}
}
}

\author{D.E. Beaudette and J.M. Skovlin}
\note{\code{diagnosticPropertyPlot2} is a lattice-based version, and requires a unique value passed to \code{id}.}


\examples{
\dontrun{
# sample data, an SPC
data(gopheridge, package='soilDB')

# get depth class
sdc <- getSoilDepthClass(gopheridge)
site(gopheridge) <- sdc

# diagnostic properties to consider, no need to convert to factors
v <- c('lithic.contact', 'paralithic.contact', 'argillic.horizon', 
'cambic.horizon', 'ochric.epipedon', 'mollic.epipedon', 'very.shallow',
'shallow', 'mod.deep', 'deep', 'very.deep')

# base graphics
x <- diagnosticPropertyPlot(gopheridge, v, k=5)

# lattice graphics
x <- diagnosticPropertyPlot2(gopheridge, v, k=3)

# check output
str(x)
}
}

\keyword{hplots}

