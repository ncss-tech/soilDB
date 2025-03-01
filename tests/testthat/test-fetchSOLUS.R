test_that("fetchSOLUS works", {
  
  skip_if_offline()
  
  skip_on_cran()
  
  skip_if(as.logical(Sys.getenv("R_SOILDB_SKIP_LONG_EXAMPLES", unset = TRUE)))
  
  skip_if_not_installed("sf")
  
  skip_if_not_installed("terra")
  
  skip_if_not_installed("rvest")
  
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
  
  # raster and SoilProfileCollection output, using linear interpolation for 1cm slices
  # site-level variables (e.g. resdept) added to site data.frame of SPC
  res <- fetchSOLUS(
    ssurgo.geom,
    # depth_slices = c("0", "5", "15", "30", "60", "100", "150"),
    variables = c("sandtotal", "silttotal", "claytotal", "cec7", "resdept"),
    output_type = "prediction",
    method = "linear",
    # samples = 1709,
    grid = NULL
  )
  
  expect_length(res, 2)
  expect_equal(terra::ncell(res$grid), 3417)
  expect_equal(length(res$spc), 3417)
  
})


test_that("virtual and out-of-bounds requests", {
  
  skip_if_offline()
  
  skip_on_cran()
  
  skip_if(as.logical(Sys.getenv("R_SOILDB_SKIP_LONG_EXAMPLES", unset = TRUE)))
  
  skip_if_not_installed("sf")
  
  skip_if_not_installed("terra")
  
  skip_if_not_installed("rvest")
  
  # virtual raster for one variable*depth
  tmp <- fetchSOLUS(variables = "claytotal",
                    output_type = "prediction",
                    depth_slices = "0")
  
  expect_true(inherits(tmp, "SpatRaster"))
  expect_true(grepl("claytotal_0_cm_p", terra::sources(tmp)))
  
  # virtual raster covers large area
  pe <- terra::as.polygons(tmp, ext = TRUE)
  expect_true(terra::expanse(pe, unit = "km") > 1e7)
  
  # extract corner point and buffer 500km
  te1 <- terra::buffer(terra::as.points(terra::simplifyGeom(pe))[1], 5e5)
  
  # only bottom-right corner returns data (expected 5000x5000 cells at 100m resolution)
  expect_equal(terra::ncell(
    fetchSOLUS(
      x = te1,
      variables = "claytotal",
      output_type = "prediction",
      depth_slices = "0"
    )
  ), 2.5e7)
  
  # choose an area that does not overlap the source raster at all
  te2 <- terra::as.polygons(terra::ext(-1, 1, -1, 1))
  terra::crs(te2) <- "OGC:CRS84"
  
  expect_error(
    fetchSOLUS(
      x = te2,
      variables = "claytotal",
      output_type = "prediction",
      depth_slices = "0"
    ),
    regexp = "outside the boundaries of the source data extent"
  )
  
})
