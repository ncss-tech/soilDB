test_that("get_SDV_legend_elements works", {
  skip_if_offline()
  
  skip_on_cran()
  
  # simplify=TRUE, one where clause returning one legend
  res <- get_SDV_legend_elements("attributekey = 427")
  
  expect_true(inherits(res, 'data.frame'))
  
  # simplify=TRUE, two where clauses returning list of length 3 = 2 + 1 legends
  res <- get_SDV_legend_elements(c("attributekey IN (427, 428)", 
                                   "nasisrulename = 'DHS - Potential for Radioactive Bioaccumulation'"))
  
  # one list, each element is a different legend
  expect_equal(length(res), 3)
  
  # simplify=FALSE, two where clauses returning list of length two
  res <- get_SDV_legend_elements(c("attributekey IN (427, 428)", 
                                   "nasisrulename = 'DHS - Potential for Radioactive Bioaccumulation'"), 
                                 simplify = FALSE)
  
  # one list for each WHERE clause
  expect_equal(length(res), 2)
  
})
