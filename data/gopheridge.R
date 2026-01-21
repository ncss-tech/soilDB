if (requireNamespace("aqp", quietly = TRUE)) {
  fn <- system.file("extdata", "gopheridge.rda", package = "soilDB")
  if (nzchar(fn)) {
    load(fn)
  } else {
    fn <- system.file("inst", "extdata", "gopheridge.rda", package = "soilDB")
    load(fn)
  }
  rm(fn)
} else {
  if (interactive()) {
    message("Install the 'aqp' package to use sample SoilProfileCollection object datasets")
  }
  assign("gopheridge", NULL, envir = environment())
}