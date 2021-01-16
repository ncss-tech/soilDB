context("fetchSCAN() -- requires internet connection")

test_that("fetchSCAN() works", {

  skip_if_offline()

  skip_on_cran()

  ## sample data
  x <<- fetchSCAN(site.code=2001, year=c(2014))

  # standard request
  expect_true(inherits(x, 'list'))

})

test_that("fetchSCAN() returns the right kind of data", {

  skip_if_offline()

  skip_on_cran()

  # metadata + some sensor data
  expect_true(inherits(x, 'list'))
  expect_true(inherits(x$metadata, 'data.frame'))
  expect_true(inherits(x$STO, 'data.frame'))
  expect_true(ncol(x$STO) == 7)

})
