context("water year/day")


test_that("works as expected", {
  
  # 2008 is a leap year
  res.lr <- waterDayYear(as.Date("2008-03-01"))
  res <- waterDayYear(as.Date("2009-03-01"))

  # check years
  expect_equal(res.lr$wy, 2008L)
  expect_equal(res$wy, 2009L)
  
  # days are offset by 1
  expect_equal(res.lr$wd, 153L)
  expect_equal(res$wd, 152L)
  
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
  expect_true(inherits(res, 'data.frame'))
  expect_true(length(x) == nrow(res))
  
  # # text interface, with YYYY-MM-DD hh:mm:ss
  # res <- waterDayYear("2000-12-05 00:00:00")
  # expect_equal(res$wy, 2001L)
  # 
  # # NOTE: 00:00:00 is not robust to time zone differences e.g. winbuilder 
  # #       times are interpreted differently depending on locale
  # #       resulting in either WD 66 or 65
  # expect_equal(res$wd, 66L)
  
  # text interface, with YYYY-MM-DD hh:mm:ss
  res <- waterDayYear("2000-12-05 12:00:00")
  expect_equal(res$wy, 2001L)
  
  expect_equal(res$wd, 66L)
  
  
  ## what about time zones... shouldn't matter?
  # waterDayYear(as.POSIXct("2000-12-05 00:00:00", tz = 'GMT'))
  
  
})

