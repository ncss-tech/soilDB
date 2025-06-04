test_that("get_SDA_coecoclass works", {
  
  skip_if_not_installed("httr")
  
  skip_if_offline()
  
  skip_on_cran()
  
  res0 <- get_SDA_coecoclass(method = "dominant condition", 
                             areasymbols = "foo")
  expect_null(res0)
  
  res1 <- get_SDA_coecoclass(method = "dominant condition", 
                             areasymbols = c("CA077", "CA630"))
  skip_if(is.null(res1))
  expect_length(unique(res1$mukey), nrow(res1))
  
  res2 <- get_SDA_coecoclass(method = "dominant component", 
                             areasymbols = c("CA077", "CA630"))
  skip_if(is.null(res2))
  expect_length(unique(res2$mukey), nrow(res2))
  
  res3 <- get_SDA_coecoclass(method = "dominant condition", 
                             areasymbols = c("CA077", "CA630"), 
                             miscellaneous_areas = FALSE)
  skip_if(is.null(res3))
  expect_length(unique(res3$mukey), nrow(res3))
  
  res4 <- get_SDA_coecoclass(method = "dominant component", 
                             areasymbols = c("CA077", "CA630"), 
                             miscellaneous_areas = FALSE)
  skip_if(is.null(res4))
  expect_length(unique(res4$mukey), nrow(res4))
  
  res5 <- get_SDA_coecoclass(areasymbols = c("CA077", "CA630"))
  skip_if(is.null(res5))
  expect_identical(nrow(unique(res5[,c("cokey","coecoclasskey")])), nrow(res5))
  
  res6 <- get_SDA_coecoclass(method = "dominant component",
                             mukeys = c(461994, 461995))
  skip_if(is.null(res6))
  expect_length(unique(res6$mukey), 2)
  
  res7 <- get_SDA_coecoclass(mukeys = c(461994, 461995), include_minors = FALSE)
  skip_if(is.null(res7))
  expect_length(unique(res7$cokey), nrow(res7))
  
  res8 <- get_SDA_coecoclass(method = "dominant component", 
                             areasymbols = c("CA077", "CA630"),
                             ecoclassref = "Ecological Site Description Database")
  skip_if(is.null(res8))
  expect_length(unique(res8$mukey), nrow(res8))
  
  res9 <- get_SDA_coecoclass(mukeys = c(461994, 461995), 
                             ecoclasstypename = "Forage Suitability Group", 
                             method = "none", 
                             include_minors = FALSE)
  skip_if(is.null(res9))
  expect_identical(nrow(res9), nrow(res7))
  
})
