context("estimation of soil temperature regime")


## sample data
expected.str.levels <- c("gelic", "cryic", "frigid", "isofrigid", "mesic", "isomesic", 
                         "thermic", "isothermic", "hyperthermic", "isohyperthermic")
## tests

test_that("estimateSTR() basic functionality", {
  
  # vector of factors
  x <- estimateSTR(mast=17, mean.summer = 22, mean.winter = 12)
  expect_true(inherits(x, 'factor'))
  expect_equal(levels(x), expected.str.levels)
  
})


test_that("estimateSTR(): specific examples", {
  
  # thermic
  x <- estimateSTR(mast=17, mean.summer = 22, mean.winter = 12)
  expect_match(as.character(x), 'thermic')
  
  # thermic / hyperthermic
  # vectorized input
  x <- estimateSTR(mast=c(17,25), mean.summer = c(22, 26), mean.winter = c(12, 14))
  expect_equal(as.character(x), c('thermic', 'hyperthermic'))
  
  # mesic
  x <- estimateSTR(mast = 12, mean.summer = 15, mean.winter = 8)
  expect_match(as.character(x), 'mesic')
  
  # frigid
  x <- estimateSTR(mast = 4, mean.summer = 8, mean.winter = 1, O.hz = TRUE, saturated = FALSE, permafrost = FALSE)
  expect_match(as.character(x), 'frigid')
  
  # cryic
  x <- estimateSTR(mast = 3, mean.summer = 4, mean.winter = 1, O.hz = TRUE, saturated = FALSE, permafrost = FALSE)
  expect_match(as.character(x), 'cryic')
  
})

