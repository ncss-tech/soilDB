\name{fetchSDA_component}
\alias{fetchSDA_component}
\alias{fetchLIMS_component}
\alias{get_mapunit_from_SDA}
\alias{get_mapunit_from_LIMS}
\alias{get_component_from_SDA}
\alias{get_component_from_LIMS}
\alias{get_chorizon_from_SDA}
\alias{get_chorizon_from_LIMS}
\alias{get_cosoilmoist_from_SDA}
\alias{get_cosoilmoist_from_LIMS}
\alias{get_cosoilmoist_from_NASIS}

\title{Extract component tables from a local NASIS Database or Soil Data Access}
\description{Get, format, impute, and return component tables.}
\usage{
fetchSDA_component(WHERE = NULL, duplicates = FALSE, rmHzErrors = FALSE)
fetchLIMS_component(projectname, rmHzErrors = FALSE, fill = FALSE)
get_mapunit_from_SDA(WHERE = NULL)
get_mapunit_from_LIMS(projectname)
get_component_from_SDA(WHERE = NULL, duplicates = FALSE)
get_component_from_LIMS(projectname)
get_chorizon_from_SDA(WHERE = NULL, duplicates = FALSE)
get_chorizon_from_LIMS(projectname, fill = FALSE)
get_cosoilmoist_from_SDA(WHERE = NULL, duplicates = FALSE, impute = TRUE)
get_cosoilmoist_from_LIMS(projectname, impute = TRUE)
get_cosoilmoist_from_NASIS(impute = TRUE)
}


\arguments{
  \item{WHERE}{text string formated as an SQL WHERE clause (default: FALSE)}
  \item{projectname}{text string value identifying the project name inserted into a SQL WHERE clause (default: NA)}
  \item{duplicates}{logical; if TRUE duplicate nationalmusym are returned}
  \item{impute}{replace missing (i.e. NULL) values with "Not_Populated" for categorical data, or the "RV" for numeric data or 201 cm if the "RV" is also NULL (default: TRUE)}
  \item{rmHzErrors}{should pedons with horizonation errors be removed from the results? (default: FALSE)}
  \item{fill}{should rows with missing component ids be removed NA (FALSE)}
}


\details{The SDA and LIMS functions can get and fetch data with an internet connection and a WHERE clause.

If the duplicates argument is set to TRUE, duplicate components are returned. This is not necessary with data returned from NASIS, which has one unique national map unit. SDA has duplicate map national map units, one for each legend it exists in.

The function get_cosoilmoist_from_NASIS_db() only works only on Windows , and requires a 'nasis_local' ODBC connection. See the \href{http://ncss-tech.github.io/AQP/soilDB/setup_local_nasis.html}{NASIS ODBC Setup tutorial} for instructions. }
\value{A dataframe or list with the results.}
\author{Stephen Roecker}


%% ~Make other sections like Warning with \section{Warning }{....} ~


\examples{
\dontrun{
library(soilDB)
library(ggplot2)
library(gridExtra)

# query soil components by areasymbol and musym
test = fetchSDA_component(WHERE = "areasymbol = 'IN005' AND musym = 'MnpB2'")
test = test$spc

# profile plot
plot(test)

# convert the data for depth plot
clay_slice = horizons(slice(test, 0:200 ~ claytotal_l + claytotal_r + claytotal_h))
names(clay_slice) <- gsub("claytotal_", "", names(clay_slice))

om_slice = horizons(slice(test, 0:200 ~ om_l + om_r + om_h))
names(om_slice) = gsub("om_", "", names(om_slice))

test2 = rbind(data.frame(clay_slice, var = "clay"),
              data.frame(om_slice, var = "om")
              )

h = merge(test2, site(test)[c("nationalmusym", "cokey", "compname", "comppct_r")],
          by = "cokey", 
          all.x = TRUE
          )

# depth plot of clay content by soil component
gg_comp <- function(x) {
  ggplot(x) +
  geom_line(aes(y = r, x = hzdept_r)) +
  geom_line(aes(y = r, x = hzdept_r)) +
  geom_ribbon(aes(ymin = l, ymax = h, x = hzdept_r), alpha = 0.2) +
  xlim(200, 0) +
  xlab("depth (cm)") +
  facet_grid(var ~ nationalmusym + paste(compname, comppct_r)) +
  coord_flip()
  }
g1 <- gg_comp(subset(h, var == "clay"))
g2 <- gg_comp(subset(h, var == "om"))

grid.arrange(g1, g2)


# query cosoilmoist (e.g. water table data) by mukey
# NA depths are interpreted as (???) with impute=TRUE argument
x <- get_cosoilmoist_from_SDA(WHERE = "mukey = '1395352'", impute = TRUE)

ggplot(x, aes(x = as.integer(month), y = dept_r, lty = status)) +
  geom_rect(aes(xmin = as.integer(month), xmax = as.integer(month) + 1,
                ymin = 0, ymax = max(x$depb_r),
                fill = flodfreqcl)) +
  geom_line(cex = 1) +
  geom_point() +
  geom_ribbon(aes(ymin = dept_l, ymax = dept_h), alpha = 0.2) +
  ylim(max(x$depb_r), 0) +
  xlab("month") + ylab("depth (cm)") +
  scale_x_continuous(breaks = 1:12, labels = month.abb, name="Month") +
  facet_wrap(~ paste0(compname, ' (', comppct_r , ')')) +
  ggtitle(paste0(x$nationalmusym[1], 
  ': Water Table Levels from Component Soil Moisture Month Data'))

  }
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}
