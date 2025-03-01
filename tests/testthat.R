if (as.integer(R.Version()$major) >= 4) {
  library(testthat)
  library(soilDB)

  test_check("soilDB")
}
