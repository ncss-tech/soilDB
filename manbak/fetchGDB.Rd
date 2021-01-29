\name{fetchGDB}
\alias{fetchGDB}
\alias{get_legend_from_GDB}
\alias{get_mapunit_from_GDB}
\alias{get_component_from_GDB}

\title{Load and Flatten Data from SSURGO file geodatabases}
\description{Functions to load and flatten commonly used tables and from SSURGO file geodatabases, and create soil profile collection objects (SPC).}
\usage{
fetchGDB(dsn = "gNATSGO_CONUS.gdb",
         WHERE = NULL,
         childs = TRUE,
         droplevels = TRUE,
         stringsAsFactors = TRUE
         )


get_legend_from_GDB(dsn = "gNATSGO_CONUS.gdb",
                    WHERE = NULL,
                    droplevels = TRUE,
                    stringsAsFactors = TRUE,
                    stats = FALSE
                    )

get_mapunit_from_GDB(dsn = "gNATSGO_CONUS.gdb",
                     WHERE = NULL,
                     droplevels = TRUE,
                     stringsAsFactors = TRUE,
                     stats = FALSE
                     )

get_component_from_GDB(dsn = "gNATSGO_CONUS.gdb",
                       WHERE = NULL,
                       childs = FALSE,
                       droplevels = TRUE,
                       stringsAsFactors = TRUE
                       )

}


\arguments{
  \item{dsn}{data source name (interpretation varies by driver - for some drivers, dsn is a file name, but may also be a folder, or contain the name and access credentials of a database); in case of GeoJSON, dsn may be the character string holding the geojson data. It can also be an open database connection.}
  \item{WHERE}{text string formatted as an SQL WHERE clause (default: FALSE)}
  \item{childs}{logical; if FALSE parent material and geomorphic child tables are not flattened and appended}
  \item{droplevels}{logical: indicating whether to drop unused levels in classifying factors. This is useful when a class has large number of unused classes, which can waste space in tables and figures.}
  \item{stringsAsFactors}{logical: should character vectors be converted to factors? This argument is passed to the uncode() function. It does not convert those vectors that have set outside of uncode() (i.e. hard coded). The 'factory-fresh' default is TRUE, but this can be changed by setting options(stringsAsFactors = FALSE)}
  \item{stats}{Return extended summary statistics (for legend or mapunit only)}
  }


\details{These functions return data from SSURGO file geodatabases with the use of a simple text string that formatted as an SQL WHERE clause (e.g. \code{WHERE = "areasymbol = 'IN001'"}. Any columns within the target table can be specified (except for fetchGDB() currently, which only targets the legend with the WHERE clause).
}
\value{A \code{data.frame} or \code{SoilProfileCollection} object.}
\author{Stephen Roecker}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
\donttest{

## replace `dsn` with path to your own geodatabase (SSURGO OR gNATSGO)
##
##
##  download CONUS gNATSGO from here:
##    https://www.nrcs.usda.gov/wps/portal/nrcs/detail/soils/survey/geo/?cid=nrcseprd1464625
##
##
# dsn <- "D:/geodata/soils/gNATSGO_CONUS.gdb"

# le <- get_legend_from_GDB(dsn = dsn, WHERE = "areasymbol LIKE '\%'")

# mu <- get_mapunit_from_GDB(dsn = dsn, WHERE = "muname LIKE 'Miami\%'")

# co <- get_component_from_GDB(dsn, WHERE = "compname = 'Miami'
#                              AND majcompflag = 'Yes'", childs = FALSE)

# f_in_GDB <- fetchGDB(WHERE = "areasymbol LIKE 'IN\%'")

}
}
\keyword{manip}
