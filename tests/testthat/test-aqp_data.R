test_that("aqp_data works", {
  skip_on_cran()
  skip_if_not_installed("aqp")
  
  aqp_data(loafercreek)
  aqp_data("gopheridge", "mineralKing")
  
  expect_true(all(sapply(
    list(loafercreek, gopheridge, mineralKing),
    inherits,
    'SoilProfileCollection'
  )))
  
  expect_silent(aqp_data(list = c("loafercreek", "gopheridge", "mineralKing")))
  
  expect_warning(aqp_data(loafercreek, overwrite = FALSE))
})
