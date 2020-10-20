\name{get_cosoilmoist_from_NASIS}
\alias{get_cosoilmoist_from_NASIS}

\title{Read and Flatten the Component Soil Moisture Tables}
\description{Read and flatten the component soil moisture month tables from a local NASIS Database.}
\usage{
get_cosoilmoist_from_NASIS(impute = TRUE, stringsAsFactors = default.stringsAsFactors())
}
\arguments{
  \item{impute}{replace missing (i.e. NULL) values with "Not_Populated" for categorical data, or the "RV" for numeric data or 201 cm if the "RV" is also NULL (default: TRUE)}
  \item{stringsAsFactors}{logical: should character vectors be converted to factors? This argument is passed to the uncode() function. It does not convert those vectors that have set outside of uncode() (i.e. hard coded). The 'factory-fresh' default is TRUE, but this can be changed by setting options(stringsAsFactors = FALSE)}
}
\value{A data.frame.}
\author{S.M. Roecker}
\details{The component soil moisture tables within NASIS house monthly data on flooding, ponding, and soil moisture status. The soil moisture status is used to specify the water table depth for components (e.g. \code{status == "Moist"}).
}
\note{This function currently works only on Windows.}

\seealso{
\link{fetchNASIS}, \link{get_cosoilmoist_from_NASISWebReport}, \link{get_cosoilmoist_from_SDA}, \code{get_comonth_from_SDA}
}


\examples{
\donttest{
if(local_NASIS_defined()) {
 # load cosoilmoist (e.g. water table data)
 test <- try(get_cosoilmoist_from_NASIS())

 # inspect
 if(!inherits(test, 'try-error')) {
   head(test)
 }
}
}}
\keyword{manip}

