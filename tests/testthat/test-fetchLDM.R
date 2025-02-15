test_that("fetchLDM works", {
  
  skip_if_offline()

  skip_on_cran()
  
  skip_if_not_installed("aqp")
  
  skip_if_not_installed("httr")

  library(aqp)
  
  # physical and chemical properties tables are 1:1 with lab_layer
  res <- expect_warning(
    fetchLDM(
      "Typic Argialbolls",
      what = "corr_taxsubgrp",
      tables = c("lab_physical_properties", 
                 "lab_chemical_properties")
    ),
    "horizon bottom depths are shallower than top depth"
  )
  skip_if(inherits(res, 'try-error') || is.null(res))
  
  expect_true(inherits(res, 'SoilProfileCollection'))
  
  # xray/thermal table allows for different prep_code and analyzed_size_frac
  #  w/ default prep_code = "S" and analyzed_size_frac = "<2 mm"
  res2 <- expect_warning(expect_message(
    fetchLDM(
      "Typic Argialbolls",
      what = "corr_taxsubgrp",
      tables = c(
        "lab_physical_properties",
        "lab_chemical_properties",
        "lab_xray_and_thermal"
      )
    )
  ),
  "horizon bottom depths are shallower than top depth")
  
  skip_if(inherits(res2, 'try-error') || is.null(res2))
  
  #   parameters an all-NA table of XRD data is joined
  expect_true(inherits(res2, 'SoilProfileCollection') &&
                all(is.na(res2$analyzed_size_frac)))
  
  # specifying a size fraction will join in just that subset of data
  # so number of profiles and horizons matches that of the query without xray data
  res3 <- expect_warning(
    fetchLDM(
      "Typic Argialbolls",
      what = "corr_taxsubgrp",
      tables = c(
        "lab_physical_properties",
        "lab_chemical_properties",
        "lab_xray_and_thermal"
      ),
      analyzed_size_frac = "0.02-0.05 mm"
    ),
    "horizon bottom depths are shallower than top depth"
  )
  
  skip_if(inherits(res3, 'try-error') || is.null(res3))
  
  expect_true(
    inherits(res3, 'SoilProfileCollection') &&
      nrow(aqp::site(res)) == nrow(aqp::site(res3)) &&
      nrow(aqp::horizons(res)) == nrow(aqp::horizons(res3))
  )
  
})
