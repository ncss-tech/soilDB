\name{fetchSDA_component}
\alias{fetchSDA_component}
\alias{get_legend_from_SDA}
\alias{get_lmuaoverlap_from_SDA}
\alias{get_mapunit_from_SDA}
\alias{get_component_from_SDA}
\alias{get_chorizon_from_SDA}
\alias{get_cosoilmoist_from_SDA}
\alias{get_cosoilmoist_from_NASIS}
\alias{get_cointerp_from_SDA}
\alias{get_diagnostics_from_SDA}

\title{Extract component tables from Soil Data Access}
\description{Get, format, impute, and return component tables.}
\usage{
fetchSDA_component(WHERE = NULL, duplicates = FALSE, childs = TRUE, 
                   nullFragsAreZero = TRUE, rmHzErrors = FALSE,
                   drop.unused.levels = TRUE,
                   stringsAsFactors = default.stringsAsFactors()
                   )
                   
get_mapunit_from_SDA(WHERE = NULL, 
                     drop.unused.levels = TRUE,
                     stringsAsFactors = default.stringsAsFactors()
                     )

get_component_from_SDA(WHERE = NULL, duplicates = FALSE, childs = TRUE,
                       drop.unused.levels = TRUE,
                       stringsAsFactors = default.stringsAsFactors()
                       )
                       
get_chorizon_from_SDA(WHERE = NULL, duplicates = FALSE, childs = TRUE,
                      nullFragsAreZero = TRUE,
                      drop.unused.levels = TRUE,
                      stringsAsFactors = default.stringsAsFactors()
                      )
                      
get_cosoilmoist_from_SDA(WHERE = NULL, duplicates = FALSE, impute = TRUE, 
                         stringsAsFactors = default.stringsAsFactors()
                         )
                         
get_cosoilmoist_from_NASIS(impute = TRUE, stringsAsFactors = default.stringsAsFactors())
}


\arguments{
  \item{WHERE}{text string formated as an SQL WHERE clause (default: FALSE)}
  \item{duplicates}{logical; if TRUE duplicate nationalmusym are returned}
  \item{childs}{logical; if FALSE parent material and geomorphic child tables are not flattened and appended}
  \item{impute}{replace missing (i.e. NULL) values with "Not_Populated" for categorical data, or the "RV" for numeric data or 201 cm if the "RV" is also NULL (default: TRUE)}
  \item{nullFragsAreZero}{should fragment volumes of NULL be interpreted as 0? (default: TRUE), see details}
  \item{rmHzErrors}{should pedons with horizonation errors be removed from the results? (default: FALSE)}
  UE)	
  \item{drop.unused.levels}{logical: indicating whether to drop unused levels in classifying factors. This is useful when a class has large number of unused classes, which can waste space in tables and figures.}
  \item{stringsAsFactors}{logical: should character vectors be converted to factors? This argument is passed to the uncode() function. It does not convert those vectors that have set outside of uncode() (i.e. hard coded). The 'factory-fresh' default is TRUE, but this can be changed by setting options(stringsAsFactors = FALSE)}
  }


\details{The SDA functions can get and fetch data with an internet connection and a WHERE clause.

If the duplicates argument is set to TRUE, duplicate components are returned. This is not necessary with data returned from NASIS, which has one unique national map unit. SDA has duplicate map national map units, one for each legend it exists in.

The function get_cosoilmoist_from_NASIS_db() only works only on Windows , and requires a 'nasis_local' ODBC connection. See the \href{http://ncss-tech.github.io/AQP/soilDB/setup_local_nasis.html}{NASIS ODBC Setup tutorial} for instructions. 

The value of \code{nullFragsAreZero} will have a significant impact on the rock fragment fractions returned by \code{fetchSDA_component}. Set \code{nullFragsAreZero = FALSE} in those cases where there are many data-gaps and NULL rock fragment values should be interpretated as NULLs. Set \code{nullFragsAreZero = TRUE} in those cases where NULL rock fragment values should be interpreted as 0.
}
\value{A dataframe or list with the results.}
\author{Stephen Roecker}


%% ~Make other sections like Warning with \section{Warning }{....} ~


\examples{
\donttest{


if (
  requireNamespace("soilDB", quietly = TRUE) & 
  requireNamespace("ggplot2", quietly = TRUE) & 
  requireNamespace("gridExtra", quietly = TRUE) & 
  requireNamespace("viridis", quietly = TRUE)
) {
  # query soil components by areasymbol and musym
  test = fetchSDA_component(WHERE = "areasymbol = 'IN005' AND musym = 'MnpB2'")
  
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
  
  
  
  # query all Miami major components
  s <- get_component_from_SDA(WHERE = "compname = 'Miami' AND majcompflag = 'Yes'")
  
  # landform vs 3-D morphometry
  test <- {
    subset(s, ! is.na(landform) | ! is.na(geompos)) ->.;
    split(., .$drainagecl, drop = TRUE) ->.;
    lapply(., function(x) {
      test = data.frame()
      test = as.data.frame(table(x$landform, x$geompos))
      test$compname   = x$compname[1]
      test$drainagecl = x$drainagecl[1]
      names(test)[1:2] <- c("landform", "geompos")
      return(test)
    }) ->.;
    do.call("rbind", .) ->.;
    .[.$Freq > 0, ] ->.;
    within(., {
      landform = reorder(factor(landform), Freq, max)
      geompos  = reorder(factor(geompos),  Freq, max)
      geompos  = factor(geompos, levels = rev(levels(geompos)))
    }) ->.;
  }
  test$Freq2 <- cut(test$Freq, 
                    breaks = c(0, 5, 10, 25, 50, 100, 150),
                    labels = c("<5", "5-10", "10-25", "25-50", "50-100", "100-150")
  )
  ggplot(test, aes(x = geompos, y = landform, fill = Freq2)) + 
    geom_tile(alpha = 0.5) + facet_wrap(~ paste0(compname, "\n", drainagecl)) +
    scale_fill_viridis(discrete = T) +
    theme(aspect.ratio = 1, axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
    ggtitle("Landform vs 3-D Morphometry for Miami Major Components on SDA")
  
  
}



}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}
