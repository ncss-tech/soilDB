context("fetchSCAN() -- requires internet connection")

x <- NULL
y <- NULL
z <- NULL

test_that("fetchSCAN sensor formatter handles deterministic data", {
  data <- data.frame(
    Site = "2001",
    Date = as.Date(c("2015-01-01", "2015-07-01")),
    Time = c("00:00", "00:00"),
    SMS_2 = c(10, 20),
    check.names = FALSE
  )
  metadata <- SCAN_site_metadata(2001)
  
  result <- .formatSCAN_soil_sensor_suites(
    data,
    code = "SMS",
    meta = metadata,
    hourlyFlag = TRUE,
    tz = "US/Central"
  )
  
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 2)
  expect_equal(result$depth, c(5, 5))
  expect_equal(format(result$datetime, "%z"), c("-0600", "-0500"))
})

test_that("fetchSCAN parser handles current and prior response layouts", {
  responses <- list(
    current = paste(
      "",
      "",
      "SCAN station metadata",
      "",
      "Site,Date,Time,SMS_2,",
      "2001,2015-01-01,00:00,10,",
      sep = "\n"
    ),
    prior = paste(
      "SCAN station metadata",
      "",
      "Site,Date,Time,SMS_2,",
      "2001,2015-01-01,00:00,10,",
      sep = "\n"
    )
  )
  
  for (response in responses) {
    result <- .get_SCAN_data(
      list(sitenum = 2001, y = 2015),
      .response_content = response
    )
    
    expect_identical(names(result), c("Site", "Date", "Time", "SMS_2"))
    expect_equal(result$Site, 2001)
    expect_equal(result$Date, as.Date("2015-01-01"))
    expect_equal(result$SMS_2, 10)
  }
})

test_that("fetchSCAN sensor formatter returns null for missing sensors", {
  data <- data.frame(
    Site = "2001",
    Date = as.Date("2015-01-01"),
    Time = "00:00",
    TEMP_2 = 10,
    check.names = FALSE
  )
  metadata <- SCAN_site_metadata(2001)
  
  expect_null(.formatSCAN_soil_sensor_suites(
    data,
    code = "SMS",
    meta = metadata,
    hourlyFlag = TRUE,
    tz = "UTC"
  ))
})

test_that("fetchSCAN formatter handles empty sensor data", {
  data <- data.frame(
    Site = "2001",
    Date = as.Date("2015-01-01"),
    Time = "00:00",
    SMS_2 = NA_real_,
    check.names = FALSE
  )
  metadata <- SCAN_site_metadata(2001)
  
  result <- .formatSCAN_soil_sensor_suites(
    data,
    code = "SMS",
    meta = metadata,
    hourlyFlag = TRUE,
    tz = "UTC"
  )
  
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
  expect_true(all(c("datetime", "water_year", "water_day") %in% names(result)))
})

test_that("fetchSCAN formatter applies the requested timezone", {
  data <- data.frame(
    Site = "2001",
    Date = as.Date("2015-07-01"),
    Time = "00:00",
    SMS_2 = 10,
    check.names = FALSE
  )
  metadata <- SCAN_site_metadata(2001)
  
  result <- .formatSCAN_soil_sensor_suites(
    data,
    code = "SMS",
    meta = metadata,
    hourlyFlag = TRUE,
    tz = "UTC"
  )
  
  expect_equal(format(result$datetime, "%z"), "+0000")
  expect_equal(format(result$datetime, "%Z"), "UTC")
})

test_that("fetchSCAN() works", {
  
  skip_if_not_installed("httr")
  
  skip_if_offline()

  skip_on_cran()

  ## sample data
  x <<- try(fetchSCAN(site.code = 2001, year = c(2014)), silent = TRUE)
  
  # skip on error
  skip_if(inherits(x, 'try-error') || is.null(x),
          "SCAN API unavailable or returned an invalid response")
  
  # standard request
  expect_true(inherits(x, 'list'))
  
  # completely empty request for valid site (bogus year)
  y <<- try(fetchSCAN(site.code = 2072, year = 1800), silent = TRUE)
  skip_if(inherits(y, 'try-error') || is.null(y),
          "SCAN API unavailable or returned an invalid empty response")

  # multiple sites / years / time zones (GMT-8, GMT-5)
  z <<- try(fetchSCAN(site.code = c(2218, 2005), year = c(2015, 2016)),
            silent = TRUE)
  
})

test_that("fetchSCAN() returns the right kind of data", {
  
  skip_if_not_installed("httr")
  
  skip_if_offline()

  skip_on_cran()
  
  # skip on error
  # skip on error
  skip_if(inherits(x, 'try-error') || is.null(x),
          "SCAN API unavailable or returned an invalid response")
  
  # metadata + some sensor data
  expect_true(inherits(x, 'list'))
  expect_true(inherits(x$metadata, 'data.frame'))
  expect_true(inherits(x$STO, 'data.frame'))
  expect_true(ncol(x$STO) == 9)
  
  expect_true(inherits(x$SMS, 'data.frame'))
  expect_true(ncol(x$SMS) == 9)
  
  # empty results should have the same data type and dimensions
  skip_if(inherits(y, 'try-error') || is.null(y),
          "SCAN API unavailable or returned an invalid empty response")

  expect_true(inherits(y, 'list'))
  expect_equivalent(nrow(y$metadata), 1)
  
  expect_true(inherits(y$STO, 'data.frame'))
  expect_true(ncol(y$STO) == 9)
  expect_true(inherits(y$SMS, 'data.frame'))
  expect_true(ncol(y$SMS) == 9)
})

test_that("timezone check", {
  
  skip_if_not_installed("httr")
  
  skip_if_offline()
  
  skip_on_cran()
  
  # skip on error
  skip_if(inherits(z, 'try-error') || is.null(z),
          "SCAN API unavailable or returned an invalid response")
  
  # default target timezone is US/Central, including CDT (-0500) and CST (-0600)
  .tz <- table(format(z$SMS$datetime, format = '%z'))
  
  skip_if(length(.tz) == 0)
  
  expect_true(all(names(.tz) %in% c("+0000", "-0500", "-0600")))
})
