context("fetchSDA() -- requires internet connection")

## sample data: these should run in < 3 seconds


test_that("fetchSDA() works", {

  skip_if_offline()

  skip_on_cran()

  # single component
  x <<- suppressMessages(fetchSDA(WHERE="nationalmusym = 'kzc4'"))

  # basic test
  expect_true(inherits(x, 'SoilProfileCollection'))
})

## tests

test_that("fetchSDA() returns an SPC", {

  skip_if_offline()

  skip_on_cran()

  # SPC integrity and expected IDs / hz depths
  expect_equal(idname(x), 'cokey')
  expect_equal(horizonDepths(x), c('hzdept_r', 'hzdepb_r'))

})

test_that("fetchSDA() returns expected results", {

  skip_if_offline()

  skip_on_cran()

  # there should be 2 components and 10 horizons
  expect_equal(length(x), 2)
  expect_equal(nrow(x), 10)

  # should match original WHERE clause
  expect_equal(x$nationalmusym, c('kzc4', 'kzc4'))
  expect_equal(site(x)$nationalmusym, c('kzc4', 'kzc4'))

  # test that both components have no NRCS forest/range site assigned
  expect_equal(site(x)$ecoclassid, c(NA_character_, NA_character_))
})
