test_that("get_SDA_hydric works", {

  skip_if_offline()

  skip_on_cran()

  # by areasymbol
  expect_equal(nrow(get_SDA_hydric(areasymbols = c("CA077", "CA630"))), 313)

  # by mukey
  expect_equal(nrow(get_SDA_hydric(mukeys = c(461994, 461995))), 2)
})
