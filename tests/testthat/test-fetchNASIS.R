context("fetchNASIS() -- requires local NASIS and ODBC connection")

## helper functions used to skip tests that rely on special conditions
# http://r-pkgs.had.co.nz/tests.html
# 
# * NASIS on the local machine
# * pedons / component missing from local database


check_local_NASIS <- function() {
  # check for connection
  if(! 'nasis_local' %in% names(RODBC::odbcDataSources())) {
    skip("local NASIS database not available")
  }
}


check_local_NASIS_pedons_available <- function() {
  
  # attempt to load pedons
  res <- try(suppressWarnings(fetchNASIS(from='pedons')), silent = TRUE)
  if(class(res) == 'try-error'){
    skip("no pedons in local NASIS database")
  }
}

check_local_NASIS_components_available <- function() {
  
  # attempt to load pedons
  res <- try(suppressWarnings(fetchNASIS(from='components')), silent = TRUE)
  if(class(res) == 'try-error'){
    skip("no components in local NASIS database")
  }
}


## tests

test_that("fetchNASIS(from='pedons') returns reasonable data", {
  
  # test for conditions permitting this test to run
  check_local_NASIS()
  check_local_NASIS_pedons_available()
  
  # get data
  # ignore warnings for now
  x <- suppressWarnings(fetchNASIS(from='pedons'))
  
  # expected outcomes
  expect_match(class(x), 'SoilProfileCollection')
  expect_equal(nrow(site(x)) > 0, TRUE)
  expect_equal(nrow(horizons(x)) > 0, TRUE)
  expect_equal(idname(x), 'peiid')
  expect_equal(horizonDepths(x), c("hzdept", "hzdepb"))
  
  # no NA in total fragments using default arguments 
  expect_equal(any(is.na(x$total_frags_pct)), FALSE)
  expect_equal(any(is.na(x$total_frags_pct_nopf)), FALSE)
  expect_equal(any(is.na(x$fragvoltot)), FALSE)
  
})

test_that("fetchNASIS(from='components') returns reasonable data", {
  
  # test for conditions permitting this test to run
  check_local_NASIS()
  check_local_NASIS_components_available()
  
  # get data
  # ignore warnings for now
  x <- suppressWarnings(fetchNASIS(from='components'))
  
  # expected outcomes
  expect_match(class(x), 'SoilProfileCollection')
  expect_equal(nrow(site(x)) > 0, TRUE)
  expect_equal(nrow(horizons(x)) > 0, TRUE)
  expect_equal(idname(x), 'coiid')
  expect_equal(horizonDepths(x), c("hzdept_r", "hzdepb_r"))
  
})


