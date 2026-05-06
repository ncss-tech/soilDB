test_that("fetchGDB works", {
  
  skip_on_cran()
  
  skip_if_not_installed("aqp")
  
  skip_if_not_installed("soilDBdata")
  
  f <- system.file("extdata", "manual", "gSSURGO_MH_FY26.zip", package = "soilDBdata")
  
  skip_if_not(nzchar(f))
  
  dsn <- paste0("/vsizip/{", f, "}/gSSURGO_MH.gdb")
  
  # default behavior
  res <- suppressWarnings(fetchGDB(dsn))
  expect_true(inherits(res, 'SoilProfileCollection'))
  
  # include child tables
  res <- suppressWarnings(fetchGDB(dsn, childs = TRUE))
  expect_true(inherits(res, 'SoilProfileCollection'))
  
  # trigger legend-level iteration
  res <- suppressWarnings(fetchGDB(dsn, WHERE = "legend.areasymbol = legend.areasymbol"))
  expect_true(inherits(res, 'SoilProfileCollection'))
})
