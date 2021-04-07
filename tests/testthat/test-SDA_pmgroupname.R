test_that("get_SDA_pmgroupname works", {
  skip_if_offline()

  skip_on_cran()

  expect_equal(nrow(get_SDA_pmgroupname(areasymbols = c("CA077", "CA630"))), 292)
  expect_equal(nrow(get_SDA_pmgroupname(mukeys = c(461994, 461995), simplify = FALSE)), 2)
})
