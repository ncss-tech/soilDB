\name{get_cosoilmoist_from_NASIS_db}
\alias{get_cosoilmoist_from_NASIS_db}
\alias{get_cosoilmoist_from_SDA_db}

\title{Extract cosoilmoist table from a local NASIS Database or Soil Data Access}
\description{Get, format, impute, and return cosoilmoist table.}
\usage{
get_cosoilmoist_from_NASIS_db(impute = TRUE)
get_cosoilmoist_from_SDA_db(mukey, impute = TRUE)
}


\arguments{
  \item{mukey}{mukey used to query Soil Data Access via SDA_query()}
  \item{impute}{replace missing (i.e. NULL) values with "Not_Populated" for categorical data or the "RV" for numeric data (default: TRUE)}
}


\details{The function get_cosoilmoist_from_NASIS_db() only works only on Windows , and requires a 'nasis_local' ODBC connection. See the \href{http://ncss-tech.github.io/AQP/soilDB/setup_local_nasis.html}{NASIS ODBC Setup tutorial} for instructions. The function get_cosoilmoist_from_SDA_db() requires an internet connection and vector of mukeys. The mukey for any given map unit can be found on SoilWeb by clicking on a polygon in SoilWeb or QGIS.}
\value{A dataframe with the results.}
\author{Stephen Roecker}


%% ~Make other sections like Warning with \section{Warning }{....} ~


\examples{
\dontrun{
# query cosoilmoist by mukey
library(soilDB)

test <- get_cosoimoist_from_SDA_db(c("406339"))

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
  facet_wrap(~ paste(compname, comppct_r, "%", mukey, sep = "-")) +
  ggtitle("Water Table Levels from Component Soil Moisture Month Data")
  }
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}
