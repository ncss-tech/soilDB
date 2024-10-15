test_that("fetchSOLUS works", {
  
  skip_if_offline()
  
  skip_on_cran()
  
  skip_if(as.logical(Sys.getenv("R_SOILDB_SKIP_LONG_EXAMPLES", unset = TRUE)))
  
  skip_if_not_installed("sf")
  
  skip_if_not_installed("terra")
  
  b <- c(-119.747629, -119.67935, 36.912019, 36.944987)
  
  bbox.sp <- sf::st_as_sf(wk::rct(
    xmin = b[1], xmax = b[2], ymin = b[3], ymax = b[4],
    crs = sf::st_crs(4326)
  ))
  
  ssurgo.geom <- soilDB::SDA_spatialQuery(
    bbox.sp,
    what = 'mupolygon',
    db = 'SSURGO',
    geomIntersection = TRUE
  )
  
  # SoilProfileCollection output, using linear interpolation for 1cm slices
  # site-level variables (e.g. resdept) added to site data.frame of SPC
  res <- fetchSOLUS(
    ssurgo.geom,
    depth_slices = c("0", "5", "15", "30", "60", "100", "150"),
    variables = c("sandtotal", "silttotal", "claytotal", "cec7", "resdept"),
    output_type = "prediction",
    method = "linear",
    grid = NULL
    #, samples = 1709
  )
  
  expect_length(res, 2)
  expect_equal(terra::ncell(res$grid), 3417)
  expect_equal(length(res$spc), 3417)
  
})
