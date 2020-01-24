\name{fetchNASISLabData}
\alias{fetchNASISLabData}


\title{Fetch lab data used site/horizon data from a PedonPC database.}
\description{Fetch KSSL laboratory pedon/horizon layer data from a local NASIS database, return as a SoilProfileCollection object.}

\usage{fetchNASISLabData(SS = TRUE)}
\arguments{
  \item{SS}{fetch data from the currently loaded selected set in NASIS or from the entire local database (default: TRUE)}
}
\value{a SoilProfileCollection class object}
\details{This function currently works only on Windows, and requires a 'nasis_local' ODBC connection.}
\author{J.M. Skovlin and D.E. Beaudette}
\note{This fuction attempts to do most of the boilerplate work when extracting KSSL laboratory site/horizon data from a local NASIS database. Lab pedons that have errors in their horizonation are excluded from the returned object, however, their IDs are printed on the console. See \code{\link{getHzErrorsNASIS}} for a simple approach to identifying pedons with problematic horizonation.}

\seealso{\code{\link{get_labpedon_data_from_NASIS_db}}}
\examples{
\donttest{
  # check required packages
  if(require(aqp) & requireNamespace("RODBC")) {

    # test that NASIS db connection is set up
    # note that you must setup this connection ahead of time
    # see inst/doc/setup_ODBC_local_NASIS.pdf
    if(any(grepl(names(RODBC::odbcDataSources()), pattern="nasis_local"))) {
      
      # query depends on some lab data, queried against the national database
      f <- try(fetchNASISLabData())
      # note: wrap in try in case no lab data in selected set
    
      # plot only those profiles with densic contact
      if(!inherits(f,'try-error')) {
      
        # which pedons have densic.contact==TRUE
        idx <- which(f$densic.contact)
        
        # if there are any pedons with densic contacts, plot them
        if(length(idx))
          plot(f[idx, ], name='hzname')
    
      } else { message(f[1]) }
    }
  }
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}

