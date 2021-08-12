test_that("get_SDA_coecoclass works", {
  skip_if_offline()
  
  skip_on_cran()
  
  res1 <- get_SDA_coecoclass(method = "dominant condition", 
                             areasymbols = c("CA077", "CA630"))
  expect_equal(length(unique(res1$mukey)), nrow(res1))
  
  res2 <- get_SDA_coecoclass(method = "dominant component", 
                             areasymbols = c("CA077", "CA630"))
  expect_equal(length(unique(res2$mukey)), nrow(res2))
  
  res3 <- get_SDA_coecoclass(method = "dominant condition", 
                             areasymbols = c("CA077", "CA630"), 
                             miscellaneous_areas = FALSE)
  expect_equal(length(unique(res3$mukey)), nrow(res3))
  
  res4 <- get_SDA_coecoclass(method = "dominant component", 
                             areasymbols = c("CA077", "CA630"), 
                             miscellaneous_areas = FALSE)
  expect_equal(length(unique(res4$mukey)), nrow(res4))
  
  res5 <- get_SDA_coecoclass(areasymbols = c("CA077", "CA630"))
  expect_equal(nrow(unique(res5[,c("cokey","coecoclasskey")])), nrow(res5))
  
  res6 <- get_SDA_coecoclass(method = "dominant component",
                             mukeys = c(461994, 461995))
  expect_equal(length(unique(res6$mukey)), 2)
  
  res7 <- get_SDA_coecoclass(mukeys = c(461994, 461995))
  expect_equal(length(unique(res7$cokey)), nrow(res7))
  
  res8 <- get_SDA_coecoclass(method = "dominant component", 
                             areasymbols = c("CA077", "CA630"),
                             ecoclassref = "Ecological Site Description Database")
  expect_equal(length(unique(res8$mukey)), nrow(res8))
  
})
