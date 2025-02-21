#' Load SoilProfileCollection Datasets
#' 
#' Loads specified data sets, or list the available data sets.
#' 
#' @param ... Literal character strings or names.
#' @param list character. A vector of object names to load.
#' @param verbose logical. If `TRUE`, additional diagnostics are printed.
#' @param envir environment. The environment where the data should be loaded. Default `.GlobalEnv`.
#' @param path character. Path (within package `/inst/` folder) to data sets. Default: `"extdata"`
#' @param package character. Package name to load data from. Default: `"soilDB"`
#' @param overwrite logical. Should existing objects of the same name in `envir` be replaced? Default: `TRUE`
#' @noRd
#' @return Called for side-effect of loading the specified dataset into global environment.
#' @examplesIf requireNamespace("aqp", quietly = TRUE)
#' 
#' aqp_data(loafercreek)
#' 
aqp_data <- function(...,
                     list = character(),
                     verbose = getOption("verbose"),
                     envir = .GlobalEnv,
                     path = "extdata",
                     package = "soilDB",
                     overwrite = TRUE) {
  
  # This is a draft function that would support moving SoilProfileCollection objects to inst/extdata
  # In theory this would eliminate the need for soilDB to explicitly import the SPC class and the aqp package
  
  if (!requireNamespace("aqp")) {
    stop("package 'aqp' is required", call. = FALSE)
  }
  
  stopifnot(is.character(list))
  nn <- c(as.character(substitute(list(...))[-1L]), list)
  
  for (n in nn) {
    fn <- system.file(path, paste0(n, ".rda"), package = package)
    if (file.exists(fn) && (overwrite || !exists(n, envir = envir))) {
      load(fn, envir = envir, verbose = verbose)
    } else {
      if (isFALSE(overwrite) && exists(n, envir = envir)) {
        warning("Object '", n, "' already exists in target environment", call. = FALSE)
      } else {
        warning("Data set '", n, "' not found", call. = FALSE)
      }
    }
  }
}
