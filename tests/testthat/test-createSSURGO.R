test_that(".write_table_with_log creates and appends correctly with chunking", {

  skip_on_cran()

  skip_if_not_installed("RSQLite")
  skip_if_not_installed("DBI")

  tmp <- tempfile(fileext = ".sqlite")
  con <- DBI::dbConnect(RSQLite::SQLite(), tmp)
  on.exit({
    try(DBI::dbDisconnect(con), silent = TRUE)
    try(unlink(tmp), silent = TRUE)
  })

  df_full <- data.frame(x = 1:50, y = letters[1:50], stringsAsFactors = FALSE)
  df_part1 <- df_full[1:10, , drop = FALSE]
  df_part2 <- df_full[11:50, , drop = FALSE]

  rec1 <- .write_table_with_log(conn = con, name = "t_test", value = df_part1, overwrite = TRUE, append = FALSE, file = NULL, quiet = TRUE)
  expect_true(is.list(rec1))
  expect_equal(rec1$table, "t_test")
  expect_equal(rec1$rows, nrow(df_part1))

  oldenv <- Sys.getenv("R_SOILDB_SSURGO_ROW_CHUNK_SIZE")
  Sys.setenv(R_SOILDB_SSURGO_ROW_CHUNK_SIZE = "15")
  on.exit(Sys.setenv(R_SOILDB_SSURGO_ROW_CHUNK_SIZE = oldenv))

  rec2 <- .write_table_with_log(conn = con, name = "t_test", value = df_part2, overwrite = FALSE, append = TRUE, file = NULL, quiet = TRUE)
  expect_true(is.list(rec2))
  expect_equal(rec2$table, "t_test")
  expect_equal(rec2$rows, nrow(df_part2))

  db_df <- DBI::dbReadTable(con, "t_test")

  expect_equal(db_df, df_full)
})

test_that("areasymbol filtering avoids oversized regular expressions", {
  files <- c(
    file.path("exports", "CA067", "tabular", "mapunit.txt"),
    file.path("exports", "spatial", "soilmu_a_ca077.shp"),
    file.path("exports", "spatial", "soilmu_a_ca0670.shp"),
    file.path("exports", "spatial", "soilmu_a_ca9990.txt")
  )
  requested <- c(
    sprintf("CA%03d", 1:999),
    "CA067",
    "CA077",
    "CA999"
  )

  matches <- .match_ssurgo_areasymbols(files, requested)

  expect_true(matches$match[1])
  expect_true(matches$match[2])
  expect_false(matches$match[3])
  expect_false(matches$match[4])
  expect_setequal(matches$found, c("CA067", "CA077"))
})

test_that("areasymbol matching discovers symbols in non-data paths", {
  files <- c(
    file.path("exports", "CA067", "tabular", "mapunit.txt"),
    file.path("exports", "spatial", "soilmu_a_ca077.shp"),
    file.path("exports", "CA067", "wss_SSA_CA067_[2026-01-01].zip"),
    file.path("exports", "README.txt.bak"),
    file.path("exports", "notes.csv")
  )

  result <- .match_ssurgo_areasymbols(files, c("CA067", "CA077"))
  expect_true(result$match[3])
  expect_setequal(result$found, c("CA067", "CA077"))
})

test_that("createSSURGO exdir inventory messages and errors", {
  exdir <- tempfile("empty-ssurgo-")
  dir.create(exdir)
  on.exit(unlink(exdir, recursive = TRUE), add = TRUE)

  expect_message(
    expect_error(
      createSSURGO(filename = tempfile(fileext = ".sqlite"),
                   exdir = exdir, quiet = FALSE),
      "appears to be empty"
    ),
    "Scanning SSURGO extraction directory"
  )
})

test_that("createSSURGO does not append after failed table initialization", {

  skip_on_cran()
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("DBI")

  exdir <- tempfile("ssurgo-init-fail-")
  dir.create(file.path(exdir, "tabular"), recursive = TRUE)
  on.exit(unlink(exdir, recursive = TRUE, force = TRUE), add = TRUE)

  f1 <- file.path(exdir, "tabular", "mapunit_part1.txt")
  f2 <- file.path(exdir, "tabular", "mapunit_part2.txt")
  writeLines("1", f1)
  writeLines("2", f2)

  mstabcol_file <- file.path(exdir, "tabular", "mstabcol.txt")
  writeLines("mapunit|1|mukey|Map Unit Key||String||10", mstabcol_file)

  msidxdet_file <- file.path(exdir, "tabular", "msidxdet.txt")
  writeLines("mapunit|IX_dummy|junk|junk", msidxdet_file)

  out_db <- tempfile(fileext = ".sqlite")
  on.exit(unlink(out_db, force = TRUE), add = TRUE)

  append_calls <- logical(0)
  write_call_n <- 0L

  testthat::local_mocked_bindings(
    .inventory_ssurgo_files = function(...) {
      list(
        f.shp = character(0),
        shp.grp = NULL,
        f.shp.sc = character(0),
        include_spatial = FALSE,
        f.txt.grp = list(mapunit = c(f1, f2)),
        txt.first = character(0),
        include_tabular = TRUE,
        mstabn = character(0),
        mstabcn = mstabcol_file,
        msidxdn = msidxdet_file,
        mstab = NULL,
        mstab_lut = c(mapunit = "mapunit")
      )
    },
    .read_delim_tabular = function(...) {
      data.frame(mukey = "123", stringsAsFactors = FALSE)
    },
    .write_table_with_log = function(conn, name, value, overwrite = FALSE,
                                     append = FALSE, file = NULL, quiet = TRUE) {
      append_calls <<- c(append_calls, append)
      write_call_n <<- write_call_n + 1L

      if (write_call_n == 1L) {
        DBI::dbWriteTable(conn, name, value[0, , drop = FALSE], append = FALSE, overwrite = FALSE)
        stop("mock init failure", call. = FALSE)
      }

      list(table = name, ssa = NA_character_, rows = nrow(value), elapsed = 0)
    },
    .package = "soilDB"
  )

  expect_no_error(
    createSSURGO(
      filename = out_db,
      exdir = exdir,
      include_spatial = FALSE,
      include_tabular = TRUE,
      quiet = FALSE
    )
  )

  expect_identical(write_call_n, 2L)
  expect_identical(append_calls, c(FALSE, FALSE))
  expect_false(append_calls[2])
})

# createSSURGO accepts raster-related parameters
test_that("createSSURGO accepts raster-related parameters", {
  skip_on_cran()
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("DBI")
  skip_if_not_installed("terra")

  exdir <- tempfile("soilDB-test-parameter-exdir-")
  dir.create(exdir)
  on.exit(unlink(exdir, recursive = TRUE, force = TRUE), add = TRUE)

  expect_error(
    createSSURGO(
      filename = tempfile(fileext = ".sqlite"),
      exdir = exdir,
      include_raster = TRUE,
      fiscal_year = "FY2024",
      LAPPLY.FUN = lapply,
      quiet = TRUE
    ),
    "appears to be empty"
  )
})

# .write_raster_with_cache uses fiscal_year in path
test_that(".write_raster_with_cache constructs correct cache path with fiscal_year", {
  skip_on_cran()
  skip_if_not_installed("terra")

  # Create a minimal raster for testing
  r <- terra::rast(nrows = 10, ncols = 10, xmin = 0, xmax = 10, ymin = 0, ymax = 10)
  terra::values(r) <- 1:100

  temp_cache_base <- tempfile("soilDB-test-cache-")
  on.exit({
    try(unlink(temp_cache_base, recursive = TRUE), silent = TRUE)
  }, add = TRUE)

  # Test with fiscal_year specified
  result <- .write_raster_with_cache(
    raster_obj = r,
    areasymbol = "CA067",
    fiscal_year = "FY2024",
    cache_root = NULL,
    quiet = TRUE
  )

  # Result should contain the fiscal year in path
  expect_true(grepl("FY2024", result, ignore.case = TRUE))
  # Result should contain "raster" subdirectory
  expect_true(grepl("raster", result, ignore.case = TRUE))
  # Result should contain the area symbol in filename
  expect_true(grepl("CA067", result, ignore.case = TRUE))
})

# .write_raster_with_cache creates nested directory structure
test_that(".write_raster_with_cache creates nested WSS cache structure", {
  skip_on_cran()
  skip_if_not_installed("terra")

  # Create a minimal raster for testing
  r <- terra::rast(nrows = 10, ncols = 10, xmin = 0, xmax = 10, ymin = 0, ymax = 10)
  terra::values(r) <- 1:100

  temp_cache <- tempfile("soilDB-test-wss-cache-")
  on.exit({
    try(unlink(temp_cache, recursive = TRUE), silent = TRUE)
  }, add = TRUE)

  # Write with custom cache_root and fiscal_year
  raster_path <- .write_raster_with_cache(
    raster_obj = r,
    areasymbol = "CA067",
    fiscal_year = "FY2024",
    cache_root = file.path(temp_cache, "WSS", "FY2024", "raster"),
    quiet = TRUE
  )

  # Verify file was created
  expect_true(file.exists(raster_path))
  # Verify path contains the expected structure
  expect_true(grepl("FY2024.*raster", raster_path))
})

# .resolve_ssa_crs resolves standard CRS correctly for various SSA regions
test_that(".resolve_ssa_crs resolves CRS correctly by region prefix and grid_alignment", {
  # CONUS
  expect_equal(.resolve_ssa_crs("CA067"), "EPSG:5070")
  expect_equal(.resolve_ssa_crs("TX001"), "EPSG:5070")

  # Alaska
  expect_equal(.resolve_ssa_crs("AK650"), "EPSG:3338")

  # Hawaii
  expect_equal(.resolve_ssa_crs("HI701"), "EPSG:6628")

  # Puerto Rico / Virgin Islands
  expect_equal(.resolve_ssa_crs("PR687"), "EPSG:32161")
  expect_equal(.resolve_ssa_crs("VI010"), "EPSG:32161")

  # Pacific Islands
  expect_equal(.resolve_ssa_crs("GU600"), "EPSG:4326")
  expect_equal(.resolve_ssa_crs("AS010"), "EPSG:4326")
  expect_equal(.resolve_ssa_crs("MP001"), "EPSG:4326")
  expect_equal(.resolve_ssa_crs("PW001"), "EPSG:4326")
  expect_equal(.resolve_ssa_crs("FM001"), "EPSG:4326")
  expect_equal(.resolve_ssa_crs("MH001"), "EPSG:4326")

  # Fallbacks / Default
  expect_equal(.resolve_ssa_crs("US"), "EPSG:5070")
  expect_equal(.resolve_ssa_crs(NULL), "EPSG:5070")
  expect_equal(.resolve_ssa_crs(character(0)), "EPSG:5070")

  # NLCD alignment canonical PROJ string
  nlcd_crs <- .resolve_ssa_crs("CA067", grid_alignment = "nlcd")
  expect_equal(nlcd_crs, "+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")
})

# .create_ssa_raster_template with template = NULL and default legacy_gssurgo alignment
test_that(".create_ssa_raster_template with default legacy_gssurgo aligns to Peaslee 10k+5 grid and expands outward", {
  skip_on_cran()
  skip_if_not_installed("sf")
  skip_if_not_installed("terra")

  # Create synthetic sf polygon in EPSG:4269 (NAD83)
  poly <- sf::st_polygon(list(matrix(
    c(-121.55, 38.55,
      -121.45, 38.55,
      -121.45, 38.65,
      -121.55, 38.65,
      -121.55, 38.55),
    ncol = 2, byrow = TRUE
  )))
  poly_sf <- sf::st_sf(geometry = sf::st_sfc(poly, crs = 4269))

  tmpl_legacy <- .create_ssa_raster_template(
    mu_shp = poly_sf,
    template = NULL,
    areasymbol = "CA067",
    resolution_m = 10,
    buffer_m = 10,
    grid_alignment = "legacy_gssurgo"
  )

  expect_s4_class(tmpl_legacy, "SpatRaster")
  expect_equal(terra::crs(tmpl_legacy, describe = TRUE)$code, "5070")
  expect_equal(terra::res(tmpl_legacy), c(10, 10))

  ext_legacy <- terra::ext(tmpl_legacy)
  # Legacy gSSURGO Peaslee grid coordinates satisfy (coord - 5) %% 10 == 0
  expect_equal(unname((ext_legacy$xmin - 5) %% 10), 0)
  expect_equal(unname((ext_legacy$xmax - 5) %% 10), 0)
  expect_equal(unname((ext_legacy$ymin - 5) %% 10), 0)
  expect_equal(unname((ext_legacy$ymax - 5) %% 10), 0)

  # Outward expansion check against original transformed polygon bounding box
  poly_5070 <- sf::st_transform(poly_sf, "EPSG:5070")
  raw_bbox <- sf::st_bbox(poly_5070)
  expect_lte(ext_legacy$xmin, raw_bbox[["xmin"]] - 10)
  expect_gte(ext_legacy$xmax, raw_bbox[["xmax"]] + 10)
  expect_lte(ext_legacy$ymin, raw_bbox[["ymin"]] - 10)
  expect_gte(ext_legacy$ymax, raw_bbox[["ymax"]] + 10)
})

# Test 11b: .create_ssa_raster_template with target_aligned alignment
test_that(".create_ssa_raster_template with target_aligned aligns to 10k grid", {
  skip_on_cran()
  skip_if_not_installed("sf")
  skip_if_not_installed("terra")

  poly <- sf::st_polygon(list(matrix(
    c(-121.55, 38.55,
      -121.45, 38.55,
      -121.45, 38.65,
      -121.55, 38.65,
      -121.55, 38.55),
    ncol = 2, byrow = TRUE
  )))
  poly_sf <- sf::st_sf(geometry = sf::st_sfc(poly, crs = 4269))

  tmpl_target <- .create_ssa_raster_template(
    mu_shp = poly_sf,
    template = NULL,
    areasymbol = "CA067",
    resolution_m = 10,
    buffer_m = 10,
    grid_alignment = "target_aligned"
  )

  expect_s4_class(tmpl_target, "SpatRaster")
  expect_equal(terra::crs(tmpl_target, describe = TRUE)$code, "5070")
  expect_equal(terra::res(tmpl_target), c(10, 10))

  ext_target <- terra::ext(tmpl_target)
  # Target aligned grid coordinates satisfy coord %% 10 == 0
  expect_equal(unname(ext_target$xmin %% 10), 0)
  expect_equal(unname(ext_target$xmax %% 10), 0)
  expect_equal(unname(ext_target$ymin %% 10), 0)
  expect_equal(unname(ext_target$ymax %% 10), 0)

  poly_5070 <- sf::st_transform(poly_sf, "EPSG:5070")
  raw_bbox <- sf::st_bbox(poly_5070)
  expect_lte(ext_target$xmin, raw_bbox[["xmin"]] - 10)
  expect_gte(ext_target$xmax, raw_bbox[["xmax"]] + 10)
  expect_lte(ext_target$ymin, raw_bbox[["ymin"]] - 10)
  expect_gte(ext_target$ymax, raw_bbox[["ymax"]] + 10)
})

# Test 11c: .create_ssa_raster_template with nlcd alignment
test_that(".create_ssa_raster_template with nlcd aligns to NLCD CONUS origin (-2415585, 3314805)", {
  skip_on_cran()
  skip_if_not_installed("sf")
  skip_if_not_installed("terra")

  poly <- sf::st_polygon(list(matrix(
    c(-121.55, 38.55,
      -121.45, 38.55,
      -121.45, 38.65,
      -121.55, 38.65,
      -121.55, 38.55),
    ncol = 2, byrow = TRUE
  )))
  poly_sf <- sf::st_sf(geometry = sf::st_sfc(poly, crs = 4269))

  tmpl_nlcd <- .create_ssa_raster_template(
    mu_shp = poly_sf,
    template = NULL,
    areasymbol = "CA067",
    resolution_m = 30,
    buffer_m = 30,
    grid_alignment = "nlcd"
  )

  expect_s4_class(tmpl_nlcd, "SpatRaster")
  expect_equal(terra::res(tmpl_nlcd), c(30, 30))

  ext_nlcd <- terra::ext(tmpl_nlcd)
  # Snapped coordinates locked to (-2415585, 3314805)
  expect_equal(unname((ext_nlcd$xmin - (-2415585)) %% 30), 0)
  expect_equal(unname((ext_nlcd$xmax - (-2415585)) %% 30), 0)
  expect_equal(unname((ext_nlcd$ymin - 3314805) %% 30), 0)
  expect_equal(unname((ext_nlcd$ymax - 3314805) %% 30), 0)

  # Check outward expansion in NLCD target CRS
  nlcd_crs <- "+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
  poly_nlcd <- sf::st_transform(poly_sf, nlcd_crs)
  raw_bbox <- sf::st_bbox(poly_nlcd)
  expect_lte(ext_nlcd$xmin, raw_bbox[["xmin"]] - 30)
  expect_gte(ext_nlcd$xmax, raw_bbox[["xmax"]] + 30)
  expect_lte(ext_nlcd$ymin, raw_bbox[["ymin"]] - 30)
  expect_gte(ext_nlcd$ymax, raw_bbox[["ymax"]] + 30)
})

# Grid phase matching between adjacent SSAs
test_that("grid phase matching produces aligned templates between adjacent SSAs", {
  skip_on_cran()
  skip_if_not_installed("sf")
  skip_if_not_installed("terra")

  # Adjacent polygon 1
  poly1 <- sf::st_polygon(list(matrix(
    c(-121.50, 38.50,
      -121.40, 38.50,
      -121.40, 38.60,
      -121.50, 38.60,
      -121.50, 38.50),
    ncol = 2, byrow = TRUE
  )))
  sf1 <- sf::st_sf(geometry = sf::st_sfc(poly1, crs = 4269))

  # Adjacent polygon 2 (shares east edge with polygon 1)
  poly2 <- sf::st_polygon(list(matrix(
    c(-121.40, 38.50,
      -121.30, 38.50,
      -121.30, 38.60,
      -121.40, 38.60,
      -121.40, 38.50),
    ncol = 2, byrow = TRUE
  )))
  sf2 <- sf::st_sf(geometry = sf::st_sfc(poly2, crs = 4269))

  # Test default legacy_gssurgo
  tmpl1_leg <- .create_ssa_raster_template(sf1, template = NULL, areasymbol = "CA067", resolution_m = 10, grid_alignment = "legacy_gssurgo")
  tmpl2_leg <- .create_ssa_raster_template(sf2, template = NULL, areasymbol = "CA067", resolution_m = 10, grid_alignment = "legacy_gssurgo")

  orig1_leg <- terra::origin(tmpl1_leg)
  orig2_leg <- terra::origin(tmpl2_leg)
  expect_equal(orig1_leg, orig2_leg)
  expect_equal(terra::res(tmpl1_leg), terra::res(tmpl2_leg))

  # Test target_aligned
  tmpl1_tgt <- .create_ssa_raster_template(sf1, template = NULL, areasymbol = "CA067", resolution_m = 10, grid_alignment = "target_aligned")
  tmpl2_tgt <- .create_ssa_raster_template(sf2, template = NULL, areasymbol = "CA067", resolution_m = 10, grid_alignment = "target_aligned")

  orig1_tgt <- terra::origin(tmpl1_tgt)
  orig2_tgt <- terra::origin(tmpl2_tgt)
  expect_equal(orig1_tgt, orig2_tgt)
  expect_equal(terra::res(tmpl1_tgt), terra::res(tmpl2_tgt))

  # Test nlcd
  tmpl1_nlcd <- .create_ssa_raster_template(sf1, template = NULL, areasymbol = "CA067", resolution_m = 30, grid_alignment = "nlcd")
  tmpl2_nlcd <- .create_ssa_raster_template(sf2, template = NULL, areasymbol = "CA067", resolution_m = 30, grid_alignment = "nlcd")

  orig1_nlcd <- terra::origin(tmpl1_nlcd)
  orig2_nlcd <- terra::origin(tmpl2_nlcd)
  expect_equal(orig1_nlcd, orig2_nlcd)
  expect_equal(terra::res(tmpl1_nlcd), terra::res(tmpl2_nlcd))
})

# .create_ssa_raster_template with custom template preserves resolution, CRS, origin
test_that(".create_ssa_raster_template with custom template preserves resolution, CRS, and origin", {
  skip_on_cran()
  skip_if_not_installed("sf")
  skip_if_not_installed("terra")

  # Custom template with 30m resolution and offset origin
  custom_tmpl <- terra::rast(
    xmin = -2000005,
    xmax = -1900005,
    ymin = 2000005,
    ymax = 2100005,
    resolution = c(30, 30),
    crs = "EPSG:5070"
  )

  poly <- sf::st_polygon(list(matrix(
    c(-1950000, 2050000,
      -1940000, 2050000,
      -1940000, 2060000,
      -1950000, 2060000,
      -1950000, 2050000),
    ncol = 2, byrow = TRUE
  )))
  poly_sf <- sf::st_sf(geometry = sf::st_sfc(poly, crs = 5070))

  res_tmpl <- .create_ssa_raster_template(
    mu_shp = poly_sf,
    template = custom_tmpl,
    areasymbol = "CA067",
    resolution_m = 10
  )

  expect_equal(terra::res(res_tmpl), c(30, 30))
  expect_equal(terra::crs(res_tmpl), terra::crs(custom_tmpl))
  expect_equal(terra::origin(res_tmpl), terra::origin(custom_tmpl))
})

# .write_raster_with_cache COG format and datatype
test_that(".write_raster_with_cache writes INT4U datatype and COG format", {
  skip_on_cran()
  skip_if_not_installed("terra")

  r <- terra::rast(nrows = 10, ncols = 10, xmin = 0, xmax = 100, ymin = 0, ymax = 100, crs = "EPSG:5070")
  terra::values(r) <- c(rep(1001:1050, each = 1), rep(NA, 50))

  temp_dir <- tempfile("soilDB-test-cog-")
  on.exit(try(unlink(temp_dir, recursive = TRUE), silent = TRUE), add = TRUE)

  file_path <- .write_raster_with_cache(
    raster_obj = r,
    areasymbol = "CA067",
    cache_root = temp_dir,
    quiet = TRUE
  )

  expect_true(file.exists(file_path))

  read_r <- terra::rast(file_path)
  expect_equal(terra::datatype(read_r), "INT4U")

  desc <- terra::describe(file_path)
  expect_true(any(grepl("LAYOUT=COG", desc)))
  expect_true(any(grepl("COMPRESSION=DEFLATE", desc)))
  expect_true(any(grepl("Block=", desc)))
  expect_true(any(grepl("NoData Value=0", desc)))
})

# .rasterize_ssurgo_ssa with mock shapefile (touches = FALSE and TRUE)
test_that(".rasterize_ssurgo_ssa rasterizes mock shapefile with touches = FALSE and TRUE", {
  skip_on_cran()
  skip_if_not_installed("sf")
  skip_if_not_installed("terra")

  temp_ssa <- tempfile("soilDB-test-ssa-")
  dir.create(temp_ssa, recursive = TRUE)
  on.exit(try(unlink(temp_ssa, recursive = TRUE), silent = TRUE), add = TRUE)

  # Create mock polygons with mukey
  poly1 <- sf::st_polygon(list(matrix(
    c(-121.50, 38.50,
      -121.45, 38.50,
      -121.45, 38.55,
      -121.50, 38.55,
      -121.50, 38.50),
    ncol = 2, byrow = TRUE
  )))
  poly2 <- sf::st_polygon(list(matrix(
    c(-121.45, 38.50,
      -121.40, 38.50,
      -121.40, 38.55,
      -121.45, 38.55,
      -121.45, 38.50),
    ncol = 2, byrow = TRUE
  )))

  mock_sf <- sf::st_sf(
    mukey = c("12345", "67890"),
    geometry = sf::st_sfc(poly1, poly2, crs = 4269)
  )

  sf::st_write(mock_sf, file.path(temp_ssa, "soilmu_a_test.shp"), quiet = TRUE)

  # Rasterize with touches = FALSE
  r_untouched <- .rasterize_ssurgo_ssa(temp_ssa, areasymbol = "CA067", touches = FALSE, quiet = TRUE)
  expect_s4_class(r_untouched, "SpatRaster")
  vals_untouched <- stats::na.omit(terra::values(r_untouched))
  expect_true(all(vals_untouched %in% c(12345, 67890)))

  # Rasterize with touches = TRUE
  r_touched <- .rasterize_ssurgo_ssa(temp_ssa, areasymbol = "CA067", touches = TRUE, quiet = TRUE)
  expect_s4_class(r_touched, "SpatRaster")
  vals_touched <- stats::na.omit(terra::values(r_touched))
  expect_true(all(vals_touched %in% c(12345, 67890)))
  expect_gte(length(vals_touched), length(vals_untouched))
})

# createSSURGO works with include_spatial = FALSE and include_raster = TRUE
test_that("createSSURGO handles include_spatial = FALSE and include_raster = TRUE without error", {
  skip_on_cran()
  skip_if_not_installed("sf")
  skip_if_not_installed("terra")
  skip_if_not_installed("RSQLite")

  temp_root <- tempfile("soilDB-test-spatial-false-")
  ssa_dir <- file.path(temp_root, "spatial")
  dir.create(ssa_dir, recursive = TRUE)
  on.exit(try(unlink(temp_root, recursive = TRUE), silent = TRUE), add = TRUE)

  # Create mock polygons
  poly1 <- sf::st_polygon(list(matrix(
    c(-121.50, 38.50,
      -121.45, 38.50,
      -121.45, 38.55,
      -121.50, 38.55,
      -121.50, 38.50),
    ncol = 2, byrow = TRUE
  )))
  mock_sf <- sf::st_sf(
    mukey = c("12345"),
    geometry = sf::st_sfc(poly1, crs = 4269)
  )
  sf::st_write(mock_sf, file.path(ssa_dir, "soilmu_a_ca067.shp"), quiet = TRUE)

  db_file <- file.path(temp_root, "test.sqlite")
  res <- expect_no_error(
    createSSURGO(
      filename = db_file,
      exdir = temp_root,
      include_spatial = FALSE,
      include_tabular = FALSE,
      include_raster = TRUE,
      fiscal_year = "FY2026",
      quiet = TRUE
    )
  )

  # Check final output raster was created next to test.sqlite
  expected_tif <- file.path(temp_root, "test.tif")
  expect_true(file.exists(expected_tif))
  rp <- attr(res, "raster_paths")
  expect_true(!is.null(rp))
  expect_equal(attr(rp, "output_raster"), normalizePath(expected_tif, winslash = "/"))
})

# createSSURGO crops a multi-SSA raster to the snapped vector union
test_that("createSSURGO uses the snapped union extent for multiple SSAs", {
  skip_on_cran()
  skip_if_not_installed("sf")
  skip_if_not_installed("terra")
  skip_if_not_installed("RSQLite")

  for (alignment in c("legacy_gssurgo", "target_aligned", "nlcd")) {
    temp_root <- tempfile(paste0("soilDB-test-multi-ssa-", alignment, "-"))
    dir.create(temp_root, recursive = TRUE)
    on.exit(try(unlink(temp_root, recursive = TRUE), silent = TRUE), add = TRUE)

    for (spec in list(
      list(dir = "ssa1", file = "soilmu_a_ca067.shp", x = -121.50),
      list(dir = "ssa2", file = "soilmu_a_ca077.shp", x = -121.40)
    )) {
      ssa_dir <- file.path(temp_root, spec$dir)
      dir.create(ssa_dir, recursive = TRUE)
      poly <- sf::st_polygon(list(matrix(
        c(spec$x, 38.50, spec$x + 0.02, 38.50,
          spec$x + 0.02, 38.52, spec$x, 38.52, spec$x, 38.50),
        ncol = 2, byrow = TRUE
      )))
      sf::st_write(
        sf::st_sf(mukey = 12345, geometry = sf::st_sfc(poly, crs = 4269)),
        file.path(ssa_dir, spec$file), quiet = TRUE
      )
    }

    db_file <- file.path(temp_root, "multi.sqlite")
    result <- createSSURGO(
      filename = db_file,
      exdir = temp_root,
      include_spatial = FALSE,
      include_tabular = FALSE,
      include_raster = TRUE,
      fiscal_year = "FY2026",
      grid_alignment = alignment,
      quiet = TRUE
    )

    output <- attr(attr(result, "raster_paths"), "output_raster")
    expect_true(file.exists(output))
    actual <- terra::ext(terra::rast(output))
    expected <- .compute_ssa_vector_bbox(
      mu_files = c(
        file.path(temp_root, "ssa1", "soilmu_a_ca067.shp"),
        file.path(temp_root, "ssa2", "soilmu_a_ca077.shp")
      ),
      ssa_list = list(list(areasymbol = "ca067"), list(areasymbol = "ca077")),
      grid_alignment = alignment,
      resolution_m = 10
    )
    expect_equal(unname(as.vector(actual)), unname(expected[c("xmin", "xmax", "ymin", "ymax")]))
  }
})
# .create_final_raster_output crops to snapped target_bbox
test_that(".create_final_raster_output crops output raster to snapped target_bbox without padding", {
  skip_on_cran()
  skip_if_not_installed("terra")

  temp_root <- tempfile("soilDB-test-final-crop-")
  dir.create(temp_root, recursive = TRUE)
  on.exit(try(unlink(temp_root, recursive = TRUE), silent = TRUE), add = TRUE)

  # Create a mock raster covering [0, 100] x [0, 100] in EPSG:5070 with 5m offset alignment
  mock_r <- terra::rast(
    xmin = -5, xmax = 105, ymin = -5, ymax = 105,
    resolution = 10,
    crs = "EPSG:5070"
  )
  terra::values(mock_r) <- 1001
  mock_tif <- file.path(temp_root, "mock_ssa.tif")
  terra::writeRaster(mock_r, mock_tif, overwrite = TRUE)

  raster_paths <- list(list(areasymbol = "CA067", path = mock_tif))
  target_file <- file.path(temp_root, "final.tif")

  # Target bbox [12, 12, 48, 48] in legacy_gssurgo snapping should snap to [5, 5, 55, 55]
  target_bbox <- c(xmin = 12, ymin = 12, xmax = 48, ymax = 48)

  res_path <- .create_final_raster_output(
    raster_paths = raster_paths,
    filename = target_file,
    target_bbox = target_bbox,
    grid_alignment = "legacy_gssurgo",
    resolution_m = 10,
    overwrite = TRUE,
    quiet = TRUE
  )

  expect_true(!is.null(res_path))
  expect_true(file.exists(res_path))

  cropped_r <- terra::rast(res_path)
  ext <- terra::ext(cropped_r)
  expect_equal(unname(ext$xmin), 5)
  expect_equal(unname(ext$ymin), 5)
  expect_equal(unname(ext$xmax), 55)
  expect_equal(unname(ext$ymax), 55)
  expect_equal(terra::datatype(cropped_r), "INT4U")
  desc <- terra::describe(res_path)
  expect_true(any(grepl("LAYOUT=COG", desc)))
  expect_true(any(grepl("COMPRESSION=DEFLATE", desc)))
  expect_true(any(grepl("NoData Value=0", desc)))
})
