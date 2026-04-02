test_that("get_SDA_hydric works", {
  
  skip_if_not_installed("httr")
  
  skip_if_offline()

  skip_on_cran()

  # by areasymbol
  x <- get_SDA_hydric(areasymbols = c("CA077", "CA630"))
  skip_if(inherits(x, 'try-error'))
  expect_length(unique(x$mukey), nrow(x))

  # check classification of mapunits
  x.nonhydric <- subset(x, x$HYDRIC_RATING == "Nonhydric")
  expect_equivalent(nrow(x.nonhydric), 176)
  expect_true(all(x.nonhydric$hydric_majors == 0 & x.nonhydric$hydric_inclusions == 0))

  # by mukey
  x <- get_SDA_hydric(mukeys = c(461994, 461995))
  expect_equivalent(nrow(x), 2)
  
  x <- get_SDA_hydric(mukeys = c(461994, 461995), method = "none")
  expect_equivalent(nrow(x), 11)
  
  x <- get_SDA_hydric(mukeys = c(461994, 461995), method = "dominant component")
  expect_equivalent(nrow(x), 2)
  
  x <- get_SDA_hydric(mukeys = c(461994, 461995), method = "dominant condition")
  expect_equivalent(nrow(x), 2)
})
