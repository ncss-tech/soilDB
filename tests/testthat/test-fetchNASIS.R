context("fetchNASIS() -- requires local NASIS and ODBC connection")

# TODO: develop minimal test set for NASIS data, stored as static SQLite DB
dsn <- NULL

## helper functions used to skip tests that rely on special conditions
# http://r-pkgs.had.co.nz/tests.html
#
# * NASIS on the local machine
# * pedons / component missing from local database


check_local_NASIS_pedons_available <- function(dsn = NULL) {

  # attempt to load pedons
  # these functions will return empty data.frame objects when there are no data in the SS
  res1 <- try(suppressWarnings(get_site_data_from_NASIS_db(dsn = dsn)), silent = TRUE)
  res2 <- try(suppressWarnings(get_hz_data_from_NASIS_db(dsn = dsn)), silent = TRUE)

  if (nrow(res1) == 0) {
    skip("no Site/Pedon records in local NASIS database")
  }
  if (nrow(res2) == 0) {
    skip("no Pedon Horizon records in local NASIS database")
  }
}

check_local_NASIS_components_available <- function(dsn = NULL) {

  # attempt to load components
  # these functions will return empty data.frame objects when there are no data in the SS
  res1 <- try(suppressWarnings(get_component_data_from_NASIS_db(dsn = dsn)), silent = TRUE)
  res2 <- try(suppressWarnings(get_component_horizon_data_from_NASIS_db(dsn = dsn)), silent = TRUE)

  # res <- try(suppressWarnings(fetchNASIS(from='pedons')), silent = TRUE)
  # note: this was too broad of a test -- any error in fetchNASIS will result in skipping the test!
  #if(class(res) == 'try-error'){
  if (nrow(res1) == 0) {
    skip("no Component records in local NASIS database")
  }

  if (nrow(res2) == 0) {
    skip("no Component Horizon records in local NASIS database")
  }
}


## tests

test_that("fetchNASIS(from='pedons') returns reasonable data", {

  # test for conditions permitting this test to run
  if (!local_NASIS_defined(dsn = dsn)) {
    skip("local NASIS database not available")
  }

  # pedons must be present for tests
  check_local_NASIS_pedons_available(dsn = dsn)

  # get data
  # ignore warnings for now
  x <- suppressWarnings(fetchNASIS(from = 'pedons'))

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
  
  # make sure fill and rmHzErrors work without error
  y <- suppressWarnings(fetchNASIS(from = 'pedons', fill = TRUE))
  expect_true(inherits(y, 'SoilProfileCollection'))
  
  z <- suppressWarnings(fetchNASIS(from = 'pedons', fill = TRUE, rmHzErrors = FALSE))
  expect_true(inherits(z, 'SoilProfileCollection'))
})

test_that("fetchNASIS(from='pedons') nullFragsAreZero works as expected", {

  # test for conditions permitting this test to run
  if (!local_NASIS_defined(dsn = dsn)) {
    skip("local NASIS database not available")
  }

  # components must be present for tests
  check_local_NASIS_pedons_available(dsn = dsn)

  # get data
  # ignore warnings for now
  x <- suppressWarnings(fetchNASIS(from = 'pedons'))
  y <- suppressWarnings(fetchNASIS(from = 'pedons', nullFragsAreZero = FALSE))

  # no NA in total fragments using default arguments
  expect_true(all(horizons(x)[is.na(y$total_frags_pct),'total_frags_pct'] == 0))
  expect_true(all(horizons(x)[is.na(y$total_art_pct),'total_art_pct'] == 0))
})

test_that("fetchNASIS(from='components') returns reasonable data", {

  skip_on_cran()

  if(!local_NASIS_defined()) {
    skip("local NASIS database not available")
  }

  # must have components to complete test
  check_local_NASIS_components_available(dsn = dsn)

  # get data
  # ignore warnings for now
  x <- suppressWarnings(fetchNASIS(from = 'components'))

  # expected outcomes
  expect_true(inherits(x, 'SoilProfileCollection'))
  expect_equal(nrow(site(x)) > 0, TRUE)
  expect_equal(nrow(horizons(x)) > 0, TRUE)
  expect_equal(idname(x), 'coiid')
  expect_equal(horizonDepths(x), c("hzdept_r", "hzdepb_r"))

})

test_that("get_text_notes_from_NASIS_db works", {
  if (!local_NASIS_defined(dsn = dsn)) {
    skip("local NASIS database not available")
  }
  expect_silent({get_text_notes_from_NASIS_db()})
})

test_that("getHzErrorsNASIS works", {
  if (!local_NASIS_defined(dsn = dsn)) {
    skip("local NASIS database not available")
  }
  expect_silent({suppressMessages(getHzErrorsNASIS(dsn = dsn))})
})

test_that("get_soilseries_from_NASIS works", {
  if (!local_NASIS_defined(dsn = dsn)) {
    skip("local NASIS database not available")
  }
  expect_silent({suppressMessages(res <- get_soilseries_from_NASIS(dsn = dsn))})

  # all calculated combined taxminalogy classes exist in corresponding taxclname
  over.idx <- grep(" over ", res$taxminalogy)
  expect_true(all(sapply(seq_len(length(over.idx)), function(i)
    grepl(res$taxminalogy[over.idx[i]], tolower(res$taxclname[over.idx[i]])))))

})




