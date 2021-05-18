test_that("fetchLDM works", {
  
  skip_if_offline()
  
  skip_on_cran()
  
  res <- fetchLDM("Typic Argialbolls",
                  what = "corr_taxsubgrp",
                  tables = c("lab_physical_properties",
                             "lab_chemical_properties"))
  
  expect_true(inherits(res, 'SoilProfileCollection'))
})
