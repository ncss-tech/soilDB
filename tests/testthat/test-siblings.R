context("siblings() -- requires internet connection")


test_that("siblings() returns reasonable data", {
  
  skip_if_offline()
  
  # hack for in-house testing only
  # WWW services aren't always available and will cause CRAN to drop our package if tests fail
  if( ! getOption('.soilDB_testNetworkFunctions') ) {
    skip("in-house testing only")
  }
  
  x <- siblings('amador', component.data = TRUE)
  
  # standard request
  expect_true(inherits(x, 'list'))
  expect_equal(length(x), 2)
  
  expect_true(inherits(x$sib, 'data.frame'))
  expect_true(inherits(x$sib.data, 'data.frame'))
  
  expect_equal(names(x$sib), c('series', 'sibling', 'majcompflag', 'n'))
})



test_that("siblings() returns skeleton with bogus query", {
  
  skip_if_offline()
  
  # hack for in-house testing only
  # WWW services aren't always available and will cause CRAN to drop our package if tests fail
  if( ! getOption('.soilDB_testNetworkFunctions') ) {
    skip("in-house testing only")
  }
  
  # a skeleton list should be returned
  res <- siblings('XXX')
  expect_true(inherits(res, 'list'))
  
  # TODO: elements should be NULL vs. FALSE
  
})

# TODO: test cousins
