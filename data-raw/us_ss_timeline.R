## code to prepare `us_ss_timeline` dataset goes here
## author: stephen roecker
## 

library(rvest)

data(state)
st <- paste0(c(state.abb, "PR", "DC", "VI", "PB"))

us_ss_timeline <- {
  .<- lapply(st, function(x) {
    cat("getting", x, "\n")
    urlbase <- "https://www.nrcs.usda.gov/wps/portal/nrcs/surveylist/soils/survey/state/?stateId="
    df  <- rvest::html_table(rvest::html_nodes(rvest::read_html(paste0(urlbase, x)), "table")[[21]])
    df$state <- x
    return(df)
  });
  .<- do.call("rbind", .);
  names(.) <- c("ssa", "year", "pdf", "wss", "state")
  .<- .[.$year != "current", ];
}

us_ss_timeline <- within(us_ss_timeline, {
  ssa <- stringi::stri_enc_toascii(sapply(ssa, function(x) strsplit(x, "\\r")[[1]][1]))
  year <- as.numeric(year)
  pdf <- pdf == "Yes"
  wss <- NULL
})
usethis::use_data(us_ss_timeline, overwrite = TRUE)
