context("fetchSCAN() -- requires internet connection")

x <- NULL

test_that("fetchSCAN() works", {

  skip_if_offline()

  skip_on_cran()

  ## sample data
  x <<- fetchSCAN(site.code = 2001, year = c(2014))
  
  # skip on error
  skip_if(inherits(x, 'try-error') || is.null(x))
  
  # standard request
  expect_true(inherits(x, 'list'))
  
  # completely empty request for valid site
  y <<- fetchSCAN(site.code = 2072, year = 1800)
  
})

test_that("fetchSCAN() returns the right kind of data", {

  skip_if_offline()

  skip_on_cran()
  
  # skip on error
  # skip on error
  skip_if(inherits(x, 'try-error') || is.null(x))
  
  # metadata + some sensor data
  expect_true(inherits(x, 'list'))
  expect_true(inherits(x$metadata, 'data.frame'))
  expect_true(inherits(x$STO, 'data.frame'))
  expect_true(ncol(x$STO) == 9)
  
  expect_true(inherits(x$SMS, 'data.frame'))
  expect_true(ncol(x$SMS) == 9)
  
  expect_true(inherits(y, 'list'))
  expect_equal(nrow(y$metadata), 1)
})
