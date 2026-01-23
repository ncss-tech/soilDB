if (requireNamespace("aqp", quietly = TRUE)) {
  fn <- system.file("extdata", "mineralKing.rda", package = "soilDB")
  if (nzchar(fn)) {
    load(fn)
  } else {
    fn <- system.file("inst", "extdata", "mineralKing.rda", package = "soilDB")
    load(fn)
  }
  rm(fn)
} else {
  if (interactive()) {
    message("Install the 'aqp' package to use sample SoilProfileCollection object datasets")
  }
  assign("mineralKing", NULL, envir = environment())
}