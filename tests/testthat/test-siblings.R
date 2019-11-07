context("siblings() -- requires internet connection")


test_that("siblings() returns reasonable data", {
  
  skip_if_offline()
  
  x <- siblings('amador', component.data = TRUE)
  
  # standard request
  expect_equal(class(x), 'list')
  expect_equal(length(x), 2)
  
  expect_equal(class(x$sib), 'data.frame')
  expect_equal(class(x$sib.data), 'data.frame')
  
  expect_equal(names(x$sib), c('series', 'sibling', 'majcompflag', 'n'))
})



test_that("siblings() returns skeleton with bogus query", {
  
  skip_if_offline()
  
  # a skeleton list should be returned
  res <- siblings('XXX')
  expect_equal(class(res), 'list')
  
  # TODO: elements should be NULL vs. FALSE
  
})

# TODO: test cousins
