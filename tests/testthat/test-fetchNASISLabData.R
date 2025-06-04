context("fetchNASISLabData() -- requires local NASIS and ODBC connection")

## helper functions used to skip tests that rely on special conditions
# http://r-pkgs.had.co.nz/tests.html
#
# * NASIS on the local machine
# * pedons / component missing from local database



check_local_NASIS_labdata_available <- function() {

  # attempt to load pedons
  res <- try(suppressWarnings(fetchNASISLabData(SS = FALSE)), silent = TRUE)
  if (inherits(res, 'try-error')) {
    skip("no NCSS Lab Data in local NASIS database")
  }
}


## tests

test_that("fetchNASISLabData returns reasonable data", {
  
  
  # test for conditions permitting this test to run
  if(!local_NASIS_defined()) {
    skip("local NASIS database not available")
  }

  skip_if_not_installed("aqp")
  
  # test for lab data to check
  check_local_NASIS_labdata_available()

  # get data
  # ignore warnings for now
  x <- suppressWarnings(fetchNASISLabData(SS = FALSE))

  # expected outcomes
  expect_true(inherits(x, 'SoilProfileCollection'))
  expect_gt(nrow(aqp::site(x)), 0)
  expect_gt(nrow(aqp::horizons(x)), 0)
  expect_identical(aqp::idname(x), 'ncsspedonlabdataiid')
  expect_identical(aqp::hzidname(x), 'ncsslayerlabdataiid')
  expect_identical(aqp::horizonDepths(x), c("hzdept", "hzdepb"))
})
