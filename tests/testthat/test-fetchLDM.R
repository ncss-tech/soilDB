test_that("fetchLDM works", {

  skip_if_offline()

  skip_on_cran()

  # physical and chemical properties tables are 1:1 with lab_layer
  res <- fetchLDM("Typic Argialbolls",
                  what = "corr_taxsubgrp",
                  tables = c("lab_physical_properties",
                             "lab_chemical_properties"))
  expect_true(inherits(res, 'SoilProfileCollection'))

  # xray/thermal table allows for different prep_code and analyzed_size_frac
  #  w/ default prep_code = "S" and analyzed_size_frac = "<2 mm"
  res2 <- expect_message(fetchLDM("Typic Argialbolls",
                   what = "corr_taxsubgrp",
                   tables = c("lab_physical_properties",
                              "lab_chemical_properties",
                              "lab_xray_and_thermal")))
  #   parameters an all-NA table of XRD data is joined
  expect_true(inherits(res2, 'SoilProfileCollection') &&
                all(is.na(res2$analyzed_size_frac)))

  # specifying a size fraction will join in just that subset of data
  # so number of profiles and horizons matches that of the query without xray data
  res3 <- fetchLDM("Typic Argialbolls",
                   what = "corr_taxsubgrp",
                  tables = c("lab_physical_properties",
                             "lab_chemical_properties",
                             "lab_xray_and_thermal"),
                  analyzed_size_frac = "0.02-0.05 mm")
  expect_true(inherits(res3, 'SoilProfileCollection') &&
                length(res) == length(res3) &&
                nrow(res) == nrow(res3))

})
