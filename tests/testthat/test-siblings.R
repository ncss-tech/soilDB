context("siblings() -- requires internet connection")

## sample data
x <- siblings('amador', component.data = TRUE)

test_that("siblings() returns skeleton with bogus query", {
  
  # a skeleton list should be returned
  res <- siblings('XXX')
  expect_equal(class(res), 'list')
  
  # TODO: elements should be NULL vs. FALSE
  
})


test_that("siblings() returns reasonable data", {
  
  # standard request
  expect_equal(class(x), 'list')
  expect_equal(length(x), 2)
  
  expect_equal(class(x$sib), 'data.frame')
  expect_equal(class(x$sib.data), 'data.frame')
  
  expect_equal(names(x$sib), c('series', 'sibling', 'n'))
})

# TODO: test cousins
