\name{fetchSDA_components}
\alias{fetchSDA_components}
\alias{get_mapunit_from_SDA}
\alias{get_component_from_SDA}
\alias{get_chorizon_from_SDA}
\alias{get_cosoilmoist_from_SDA_db}
\alias{get_cosoilmoist_from_NASIS_db}

\title{Extract component tables from a local NASIS Database or Soil Data Access}
\description{Get, format, impute, and return component tables.}
\usage{
fetchSDA_componets(WHERE = NULL, duplicates = FALSE)
get_mapunits_from_SDA(WHERE = NULL)
get_components_from_SDA(WHERE = NULL, duplicates = FALSE)
get_chorizons_from_SDA(WHERE = NULL, duplicates = FALSE)
get_cosoilmoist_from_SDA_db(mukey, impute = TRUE)
get_cosoilmoist_from_NASIS_db(impute = TRUE)
}


\arguments{
  \item{WHERE}{text string formated as an SQL WHERE clause (default: FALSE)}
  \item{duplicates}{logical; if TRUE duplicate nationalmusym are returned}
  \item{mukey}{mukey used to query Soil Data Access via SDA_query()}
  \item{impute}{replace missing (i.e. NULL) values with "Not_Populated" for categorical data, or the "RV" for numeric data or 201 cm if the "RV" is also NULL (default: TRUE)}
}


\details{The function SDA get and fetch functions require an internet connection and a WHERE clause.

If the duplicates argument is set to TRUE, duplicate components are returned. This is not necessary with data returned from NASIS, which has one unique national map unit. SDA has duplicate map national map units, one for each legend it exists in.

The function get_cosoilmoist_from_NASIS_db() only works only on Windows , and requires a 'nasis_local' ODBC connection. See the \href{http://ncss-tech.github.io/AQP/soilDB/setup_local_nasis.html}{NASIS ODBC Setup tutorial} for instructions. }
\value{A dataframe or list with the results.}
\author{Stephen Roecker}


%% ~Make other sections like Warning with \section{Warning }{....} ~


\examples{
\dontrun{
# query component by nationalmusym
test = fetchSDA_components(WHERE = "nationalmusym = '2vzcp'")
plot(test$spc)


# query cosoilmoist by mukey
library(soilDB)

test <- get_cosoilmoist_from_SDA_db(c("406339"))

# remove depths with NA
test <- subset(test, !is.na(dept_r))

# plot annual water table
library(ggplot2)

ggplot(test) +
  geom_rect(aes(xmin = as.integer(month), xmax = as.integer(month) + 1,
                ymin = 0, ymax = max(test$depb_r),
                fill = flodfreqcl)) +
  geom_line(aes(x = as.integer(month), y = dept_r, 
                lty = status), cex = 1) +
  geom_pointrange(aes(x = as.integer(month), 
                     ymin = dept_l, y = dept_r, ymax = dept_h)) +
  ylim(max(test$depb_r), 0) +
  xlab("month") + ylab("depth (cm)") +
  facet_wrap(~ paste(compname, comppct_r, "pct", mukey, sep = "-")) +
  ggtitle("Water Table Levels from Component Soil Moisture Month Data")
  }
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}
