context("siblings() -- requires internet connection")


test_that("siblings() returns reasonable data", {

  skip_if_offline()

  skip_on_cran()

  x <- siblings('amador', component.data = TRUE)
  
  # if load balancer or server is down, skip tests
  skip_if(is.null(x))
  
  # standard request
  expect_true(inherits(x, 'list'))
  expect_length(x, 2)

  expect_true(inherits(x$sib, 'data.frame'))
  expect_true(inherits(x$sib.data, 'data.frame'))

  expect_named(x$sib, c('series', 'sibling', 'majcompflag', 'n'))
})



test_that("siblings() returns skeleton with bogus query", {

  skip_if_offline()

  skip_on_cran()

  # a skeleton list should be returned
  res <- siblings('XXX')
  
  expect_null(res)

})

# test cousins, takes more time
test_that("siblings(..., cousins = TRUE)", {
  
  skip_if_offline()
  
  skip_on_cran()
  
  x <- siblings('amador', only.major = TRUE, cousins = TRUE)
    
  # if load balancer or server is down, skip tests
  skip_if(is.null(x))
    
  expect_true(inherits(x, 'list'))
  expect_length(x, 2)
  
  expect_true(inherits(x$sib, 'data.frame'))
  expect_true(inherits(x$cousins, 'data.frame'))
  
  expect_named(x$sib, c('series', 'sibling', 'majcompflag', 'n'))
  expect_named(x$cousins, c('series', 'sibling', 'majcompflag', 'n'))
})


