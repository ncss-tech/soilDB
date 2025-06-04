context("water year/day")


test_that("works as expected", {

  skip_on_cran()
  
  # NOTE: waterDayYear requires `tz` argument for consistent conversion across locales

  # 2008 is a leap year
  res.lr <- waterDayYear(as.Date("2008-03-01"))
  res <- waterDayYear(as.Date("2009-03-01"))

  # check years
  expect_identical(res.lr$wy, 2008L)
  expect_identical(res$wy, 2009L)

  # days are offset by 1
  expect_identical(res.lr$wd, 153L)
  expect_identical(res$wd, 152L)

  # first day of water year 2019
  x <- as.Date("2018-10-01")
  res <- waterDayYear(x)
  
  expect_identical(res$wy, 2019)
  expect_identical(res$wd, 1)

  # last day of water year 2018
  x <- as.Date("2018-09-30")
  res <- waterDayYear(x)

  expect_identical(res$wy, 2018)
  expect_identical(res$wd, 365)

  # important dates
  x <- as.Date(c("1982-11-19", "1981-02-17"))
  res <- waterDayYear(x)

  # TODO: double-check these
  expect_identical(res$wy, c(1983, 1981))
  expect_identical(res$wd, c(50,  140))

  # structural integrity
  expect_true(inherits(res, 'data.frame'))
  expect_true(length(x) == nrow(res))

  # text interface, with YYYY-MM-DD hh:mm:ss
  # NOTE: waterDayYear requires `format` argument for subdaily accuracy
  res <- waterDayYear("2000-12-05 12:00:00", format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  expect_identical(res$wy, 2001L)

  expect_identical(res$wd, 66L)

})

test_that("non-standard `end` of water year", {
  
  skip_on_cran()
  
  x <- as.Date("2018-09-30")

  # water year "2019" starts hypothetically on March 11th, 2018
  res <- waterDayYear(x, end = '03-10')

  expect_identical(res$wy, 2019L)
  expect_identical(res$wd, as.integer(round(difftime("2018-09-30", "2018-03-10"))))
})

.waterDayYearTZTest <- function(x, tz, test_tz = "UTC", ...) {
  tzorig <- Sys.getenv("TZ")
  Sys.setenv(TZ = tz)
  x <- waterDayYear(x, tz = test_tz, ...)
  Sys.setenv(TZ = tzorig)
  x$wd
}

test_that("waterDayYear gives consistent result within a timezone", {
  
  skip_on_cran()
  
  # test waterDayYear in other timezones
  res <- sapply(OlsonNames(),
              function(x)
                .waterDayYearTZTest("1992-09-11", tz = x, test_tz = x))
  expect_true(all(res == 347))

  res <- sapply(OlsonNames(),
                function(x)
                  .waterDayYearTZTest("2000-12-05 12:00:00", tz = x, test_tz = x, format = "%Y-%m-%d %H:%M:%S"))
  expect_true(all(res == 66))
})

test_that("waterDayYear tz='UTC' behaves the same across timezones" , {
  
  skip_on_cran()
  
  # test waterDayYear behavior with constant (UTC) timezone in simulated other timezones
  res <- sapply(OlsonNames(),
                function(x)
                  .waterDayYearTZTest("1992-09-11", tz = x, test_tz = "UTC"))
  expect_true(all(res == 347))

  res <- sapply(OlsonNames(),
              function(x)
                .waterDayYearTZTest("2000-12-05 12:00:00", tz = x, test_tz = "UTC", format = "%Y-%m-%d %H:%M:%S"))
  expect_true(all(res == 66))
})
