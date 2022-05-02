test_that("get_SDA_hydric works", {

  skip_if_offline()

  skip_on_cran()

  # by areasymbol
  x <- get_SDA_hydric(areasymbols = c("CA077", "CA630"))
  expect_equal(nrow(x), length(unique(x$mukey)))

  # check classification of mapunits
  x.nonhydric <- subset(x, x$HYDRIC_RATING == "Nonhydric")
  expect_equal(nrow(x.nonhydric), 175)
  expect_true(all(x.nonhydric$hydric_majors == 0 & x.nonhydric$hydric_inclusions == 0))

  # by mukey
  expect_equal(nrow(get_SDA_hydric(mukeys = c(461994, 461995))), 2)
  expect_equal(nrow(get_SDA_hydric(mukeys = c(461994, 461995), method = "none")), 11)
  expect_equal(nrow(get_SDA_hydric(mukeys = c(461994, 461995), method = "dominant component")), 2)
  expect_equal(nrow(get_SDA_hydric(mukeys = c(461994, 461995), method = "dominant condition")), 2)
})
