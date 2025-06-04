context("fetchSDA() -- requires internet connection")

test_that("fetchSDA() works", {
  
  skip_if_not_installed("aqp")
  
  skip_if_not_installed("httr")
  
  skip_if_offline()

  skip_on_cran()

  # single component
  x <<- suppressMessages(fetchSDA(WHERE="nationalmusym = 'kzc4'"))

  # basic test
  expect_true(inherits(x, 'SoilProfileCollection'))
})

## tests

test_that("fetchSDA() returns an SPC", {
  
  skip_if_not_installed("aqp")
  
  skip_if_not_installed("httr")
  
  skip_if_offline()

  skip_on_cran()

  # SPC integrity and expected IDs / hz depths
  expect_identical(aqp::idname(x), 'cokey')
  expect_identical(aqp::horizonDepths(x), c('hzdept_r', 'hzdepb_r'))

})

test_that("fetchSDA() returns expected results", {
  
  skip_if_not_installed("aqp")
  
  skip_if_not_installed("httr")
  
  skip_if_offline()

  skip_on_cran()

  library(aqp)
  
  # there should be 2 components and 10 horizons
  expect_identical(nrow(aqp::site(x)), 2)
  expect_identical(aqp::nrow(x), 10)

  # should match original WHERE clause
  expect_identical(x$nationalmusym, c('kzc4', 'kzc4'))
  expect_identical(aqp::site(x)$nationalmusym, c('kzc4', 'kzc4'))

  # test that both components have no NRCS forest/range site assigned
  expect_contains(aqp::site(x)$ecoclassid, c("F114XB104IN", "F114XA101IN"))
})

test_that("fetchSDA(duplicates=TRUE) works as expected", {
  
  skip_if_not_installed("aqp")
  
  skip_if_not_installed("httr")
  
  skip_if_offline()

  skip_on_cran()

  x2 <<- suppressWarnings(suppressMessages(fetchSDA(WHERE="nationalmusym = '22787'")))
  x3 <<- suppressWarnings(suppressMessages(fetchSDA(WHERE="nationalmusym = '22787'", duplicates = TRUE)))

  # we get a "duplicate" for each unique mukey within a nationalmusym -- 1 for each legend
  expect_identical(length(x2) * length(unique(x3$mukey)), length(x3))
})
