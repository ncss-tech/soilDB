context("fetchNASIS() -- requires local NASIS and ODBC connection")

## helper functions used to skip tests that rely on special conditions
# http://r-pkgs.had.co.nz/tests.html
#
# * NASIS on the local machine
# * pedons / component missing from local database


check_local_NASIS_pedons_available <- function() {

  # attempt to load pedons
  # these functions will return empty data.frame objects when there are no data in the SS
  res1 <- try(suppressWarnings(get_site_data_from_NASIS_db()), silent = TRUE)
  res2 <- try(suppressWarnings(get_hz_data_from_NASIS_db()), silent = TRUE)

  if(nrow(res1) == 0) {
    skip("no Site/Pedon records in local NASIS database")
  }
  if(nrow(res2) == 0) {
    skip("no Pedon Horizon records in local NASIS database")
  }
}

check_local_NASIS_components_available <- function() {

  # attempt to load components
  # these functions will return empty data.frame objects when there are no data in the SS
  res1 <- try(suppressWarnings(get_component_data_from_NASIS_db()), silent = TRUE)
  res2 <- try(suppressWarnings(get_component_horizon_data_from_NASIS_db()), silent = TRUE)

  # res <- try(suppressWarnings(fetchNASIS(from='pedons')), silent = TRUE)
  # note: this was too broad of a test -- any error in fetchNASIS will result in skipping the test!
  #if(class(res) == 'try-error'){
  if(nrow(res1) == 0) {
    skip("no Component records in local NASIS database")
  }

  if(nrow(res2) == 0) {
    skip("no Component Horizon records in local NASIS database")
  }
}


## tests

test_that("fetchNASIS(from='pedons') returns reasonable data", {

  # test for conditions permitting this test to run
  if(!local_NASIS_defined()) {
    skip("local NASIS database not available")
  }

  # pedons must be present for tests
  check_local_NASIS_pedons_available()

  # get data
  # ignore warnings for now
  x <- suppressWarnings(fetchNASIS(from='pedons'))

  # expected outcomes
  expect_true(inherits(x, 'SoilProfileCollection'))
  expect_equal(nrow(site(x)) > 0, TRUE)
  expect_equal(nrow(horizons(x)) > 0, TRUE)
  expect_equal(idname(x), 'peiid')
  expect_equal(horizonDepths(x), c("hzdept", "hzdepb"))

  # no NA in total fragments using default arguments
  expect_equal(any(is.na(x$total_frags_pct)), FALSE)
  expect_equal(any(is.na(x$total_frags_pct_nopf)), FALSE)
  expect_equal(any(is.na(x$fragvoltot)), FALSE)

})

test_that("fetchNASIS(from='pedons') nullFragsAreZero works as expected", {

  # test for conditions permitting this test to run
  if(!local_NASIS_defined()) {
    skip("local NASIS database not available")
  }

  # components must be present for tests
  check_local_NASIS_pedons_available()

  # get data
  # ignore warnings for now
  x <- suppressWarnings(fetchNASIS(from='pedons'))
  y <- suppressWarnings(fetchNASIS(from='pedons', nullFragsAreZero=FALSE))

  # no NA in total fragments using default arguments
  expect_true(all(horizons(x)[is.na(y$total_frags_pct),'total_frags_pct'] ==0))
  expect_true(all(horizons(x)[is.na(y$total_art_pct),'total_art_pct'] ==0))
})

test_that("fetchNASIS(from='components') returns reasonable data", {

  # hack for in-house testing only
  # WWW services aren't always available and will cause CRAN to drop our package if tests fail
  if(!local_NASIS_defined()) {
    skip("in-house testing only")
  }

  # must have components to complete test
  check_local_NASIS_components_available()

  # get data
  # ignore warnings for now
  x <- suppressWarnings(fetchNASIS(from='components'))

  # expected outcomes
  expect_true(inherits(x, 'SoilProfileCollection'))
  expect_equal(nrow(site(x)) > 0, TRUE)
  expect_equal(nrow(horizons(x)) > 0, TRUE)
  expect_equal(idname(x), 'coiid')
  expect_equal(horizonDepths(x), c("hzdept_r", "hzdepb_r"))

})


