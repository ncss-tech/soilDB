context("water year/day")


test_that("works as expected", {
  
  # first day of water year 2019
  x <- as.Date("2018-10-01")
  res <- waterDayYear(x)
  
  expect_equal(res$wy, 2019)
  expect_equal(res$wd, 1)
  
  # last day of water year 2018
  x <- as.Date("2018-09-30")
  res <- waterDayYear(x)
  
  expect_equal(res$wy, 2018)
  expect_equal(res$wd, 365)
  
  # important dates
  x <- as.Date(c("1982-11-19", "1981-02-17"))
  res <- waterDayYear(x)
  
  # TODO: double-check these
  expect_equal(res$wy, c(1983, 1981))
  expect_equal(res$wd, c(50,  140))
  
  # structural integrity
  expect_true(class(res) == 'data.frame')
  expect_true(length(x) == nrow(res))
  
})

