test_that("get_SDV_legend_elements works", {
  skip_if_offline()
  
  skip_on_cran()
  
  res <- get_SDV_legend_elements("attributekey = 427")
  
  expect_true(inherits(res, 'data.frame'))
  
  res <- get_SDV_legend_elements(c("attributekey = 427", 
                                   "nasisrulename = 'DHS - Potential for Radioactive Bioaccumulation'"))
  
  expect_true(inherits(res, 'list'))
})
