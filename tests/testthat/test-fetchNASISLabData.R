context("fetchNASISLabData() -- requires local NASIS and ODBC connection")

## helper functions used to skip tests that rely on special conditions
# http://r-pkgs.had.co.nz/tests.html
#
# * NASIS on the local machine
# * pedons / component missing from local database



check_local_NASIS_labdata_available <- function() {

  # attempt to load pedons
  res <- try(suppressWarnings(fetchNASISLabData(SS = FALSE)), silent = TRUE)
  if(class(res) == 'try-error'){
    skip("no NCSS Lab Data in local NASIS database")
  }
}


## tests

test_that("fetchNASISLabData returns reasonable data", {

  # test for conditions permitting this test to run
  if(!local_NASIS_defined()) {
    skip("local NASIS database not available")
  }

  # test for lab data to check
  check_local_NASIS_labdata_available()

  # get data
  # ignore warnings for now
  x <- suppressWarnings(fetchNASISLabData(SS = FALSE))

  # expected outcomes
  expect_true(inherits(x, 'SoilProfileCollection'))
  expect_equal(nrow(site(x)) > 0, TRUE)
  expect_equal(nrow(horizons(x)) > 0, TRUE)
  expect_equal(idname(x), 'labpeiid')
  expect_equal(hzidname(x), 'labphiid')
  expect_equal(horizonDepths(x), c("hzdept", "hzdepb"))
})
