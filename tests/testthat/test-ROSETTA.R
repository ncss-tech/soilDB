context("ROSETTA() -- requires internet connection")

# example data
x <<- structure(
  list(
    musym = c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L,
              1L),
    cokey = c(20392868L, 20392868L, 20392870L, 20392870L, 20392870L,
              20392870L, 20393517L, 20394343L, 20394343L, 20394343L),
    compname = c("Aluf",
                 "Aluf", "Hitilo", "Hitilo", "Hitilo", "Hitilo", "Aransas", "Muskogee",
                 "Muskogee", "Muskogee"),
    comppct_r = c(47L, 47L, 22L, 22L, 22L,
                  22L, 100L, 20L, 20L, 20L),
    hzname = c("H1", "H2", "H1", "H2",
               "H3", "H4", "H1", "H1", "H2", "H3"),
    hzdept_r = c(0L, 127L, 0L,
                 117L, 137L, 157L, 0L, 0L, 38L, 64L),
    hzdepb_r = c(127L, 203L,
                 117L, 137L, 157L, 203L, 152L, 38L, 64L, 203L),
    sandtotal_r = c(94.4,
                    91, 94.1, 55.4, 59.3, 62.8, 23.3, 11.7, 6.8, 23.3),
    silttotal_r = c(0.6,
                    1, 1.4, 17.6, 13.7, 19.2, 29.2, 69.8, 63.2, 29.2),
    claytotal_r = c(5,
                    8, 4.5, 27, 27, 18, 47.5, 18.5, 30, 47.5),
    dbthirdbar_r = c(1.6,
                     1.55, 1.55, 1.58, 1.58, 1.5, 1.45, 1.38, 1.35, 1.33),
    wthirdbar_decimal = c(0.117,
                          0.129, 0.106, 0.252, 0.261, 0.211, 0.369, 0.286, 0.308, 0.399
    ),
    wfifteenbar_decimal = c(0.049, 0.062, 0.039, 0.181, 0.156,
                            0.122, 0.285, 0.134, 0.165, 0.255)
  ),
  row.names = c(NA, -10L),
  class = "data.frame",
  SDA_id = "Table"
)



test_that("ROSETTA() works", {

  skip_if_offline()

  skip_on_cran()

  # attempting to use all possible soil properties
  vars <- c('sandtotal_r', 'silttotal_r', 'claytotal_r', 'dbthirdbar_r', 'wthirdbar_decimal', 'wfifteenbar_decimal')

  # submit request
  r <- ROSETTA(x, vars = vars)

  # correct object
  expect_true(inherits(r, 'data.frame'))

  # input / output are conformal
  expect_true(nrow(r) == nrow(x))

  # output contains new columns
  expect_true(all(c('theta_r', 'theta_s', 'alpha', 'npar', 'ksat') %in% names(r)))

})

test_that("bootstrap standard deviation", {
  
  skip_if_offline()
  
  skip_on_cran()
  
  # attempting to use all possible soil properties
  vars <- c('sandtotal_r', 'silttotal_r', 'claytotal_r', 'dbthirdbar_r', 'wthirdbar_decimal', 'wfifteenbar_decimal')
  
  # submit request
  r <- ROSETTA(x, vars = vars, include.sd = TRUE)
  
  # correct object
  expect_true(inherits(r, 'data.frame'))
  
  # input / output are conformal
  expect_true(nrow(r) == nrow(x))
  
  # output contains new columns
  expect_true(all(c('sd_theta_r', 'sd_theta_s', 'sd_alpha', 'sd_npar', 'sd_ksat') %in% names(r)))
  
})

test_that("correct model selection in the presence of NA", {

  skip_if_offline()

  skip_on_cran()

  # sprinkle NA
  x$dbthirdbar_r[1] <- NA
  x$wthirdbar_decimal[2] <- NA
  x$wfifteenbar_decimal[3] <- NA
  x$sandtotal_r[9] <- NA
  x[10, ] <- NA

  # attempting to use all possible soil properties
  vars <- c('sandtotal_r', 'silttotal_r', 'claytotal_r', 'dbthirdbar_r', 'wthirdbar_decimal', 'wfifteenbar_decimal')

  # submit request
  r <- ROSETTA(x, vars = vars)

  # missing 1/3 bar Db -> model 2
  expect_true(r$.rosetta.model[1] == 2)

  # missing 1/3 bar WT -> model 3
  expect_true(r$.rosetta.model[2] == 3)

  # missing 15 bar WT -> model 4
  expect_true(r$.rosetta.model[3] == 4)

  # no records missing -> model 5
  expect_true(all(r$.rosetta.model[4:8] == 5))

  # missing sand -> NA
  expect_true(is.na(r$theta_r[9]) & (r$.rosetta.model[9] == -1))

  # all NA
  expect_true(all(is.na(r[10, c('theta_r', 'theta_s', 'alpha', 'npar', 'ksat')])))


})
