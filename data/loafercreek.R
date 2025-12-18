if (requireNamespace("aqp", quietly = TRUE)) {
  fn <- system.file("extdata", "loafercreek.rda", package = "soilDB")
  if (nzchar(fn)) {
    load(fn)
  } else {
    fn <- system.file("inst", "extdata", "loafercreek.rda", package = "soilDB")
    load(fn)
  }
} else {
  if (interactive()) {
    message("Install the 'aqp' package to use sample SoilProfileCollection object datasets")
  }
  assign("loafercreek", NULL, envir = environment())
}